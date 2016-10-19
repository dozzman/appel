open Absyn
open Types
open ErrorMsg
open Env
open Printf

module S = Symbol

exception Undefined_type
exception Unimplemented
exception Type_mismatch

type expty = Translate.exp * ty

let rec actual_ty t =
  match t with
  | NAME (symbol, tyref) ->
    begin match !tyref with
    | Some ty ->
      actual_ty ty
    | None -> t
    end
  | _ -> t

let check_type_errors env (symbol, ty_exp) =
  let rec assert_type_defined ty =
    match actual_ty ty with
    | NAME (sym, _) ->
        let error_msg =
          sprintf "Type '%s' has not been fully defined" (S.name sym)
        in
          error_at_pos error_msg ty_exp.ty_pos;
          false
    | ARRAY (elem_ty, _) ->
      assert_type_defined elem_ty
    | RECORD (param_list, _) ->
      List.fold_left
        (fun acc (sym, ty') ->
          match ty' with
          (* recursive record has already been defined *)
          | NAME (s, _) when s = symbol ->
            true
          | _ ->
            if not (assert_type_defined ty') then false
            else acc)
        true
        param_list
    | _ -> true
  in
    match S.look env.tenv symbol with
    | Some ty ->
      if assert_type_defined ty then 0
      else 1
    | None ->
      let error_msg =
        sprintf "Undeclared type '%s'" (S.name symbol)
      in
        error_at_pos error_msg ty_exp.ty_pos;
        1

let rec type_closure_contains env symbol check_symbol =
  let check_descend inner_ty =
    match inner_ty with
    | NAME (next_symbol, ty_ref) ->
      type_closure_contains env next_symbol check_symbol
    | _ -> true (* inner types must be names or primitives *)
  in
    if symbol = check_symbol then true
    else match S.look env.tenv symbol with
    | None -> false
    | Some ty ->
      (match ty with
      | NAME (_, ty_ref) ->
        (match !ty_ref with
        | Some ty' ->
          check_descend ty'
        | None -> false)
      | ARRAY (elem_ty, _) ->
        check_descend elem_ty
      | RECORD (param_list, _) ->
        List.fold_left
          (fun acc (sym, ty') ->
            if not (check_descend ty') then false
            else acc)
          true
          param_list
      | _ -> true)

let forward_declare_type env symbol =
  match S.look env.tenv symbol with
  | None ->
    let forward_type = NAME (symbol, ref None) in
    let env' =
      {env with tenv = S.enter env.tenv symbol forward_type}  in
      `Declared (env', forward_type)
  | Some ty ->
    (match actual_ty ty with
    | (NAME _) as actualty ->
      `Declared (env, actualty)
    | existing_ty ->
      `Defined existing_ty)

let trans_tydec env (symbol, ty_exp) =
  match forward_declare_type env symbol with
  | `Defined ty ->
    let error_msg = sprintf "type %s already fully defined" (S.name symbol) in
      error_at_pos error_msg ty_exp.ty_pos;
      env
  | `Declared (env', (NAME (symbol, type_ref))) ->
    (match ty_exp.ty_desc with
    | NameTy name ->
      (match forward_declare_type env' name with
      | `Defined ty ->
        (* Dont need to call 'actual_ty' here *)
        type_ref := Some (actual_ty ty);
        env'
      | `Declared (env'', ty) ->
        (match type_closure_contains env'' name symbol with
        | true ->
          let error_msg = "type declaration loop detected" in
            error_at_pos error_msg ty_exp.ty_pos;
            env
        | false ->
          type_ref := Some ty;
          env''))
    | ArrayTy name ->
      (match forward_declare_type env' name with
      | `Defined ty ->
        type_ref := Some (ARRAY ((actual_ty ty), ref ()));
        env'
      | `Declared (env'', ty) ->
        (match type_closure_contains env'' name symbol with
        | true ->
          let error_msg = "type declaration loop detected" in
            error_at_pos error_msg ty_exp.ty_pos;
            env
        | false ->
          type_ref := Some (ARRAY (actual_ty ty, ref ()));
          env''))
    | RecordTy params ->
      let env'', record_params =
        List.fold_left
          (fun (env, record_params) param ->
            (match forward_declare_type env param.param_typename with
            | `Defined ty ->
              (env, (param.param_name, ty)::record_params)
            | `Declared (env', ty) ->
              (env', (param.param_name, ty)::record_params)))
        (env', [])
        params
      in
        type_ref := Some (RECORD (record_params, ref ()));
        env'')
  | _ ->
    impossible
      "forward_declare_type should always return a NAME type"
      ty_exp.ty_pos;
    env

let check_int (_, ty) pos =
  if ty != INT then
    error_at_pos "integer required" pos

let check_string(_, ty) pos =
  if ty != STRING then
    error_at_pos "string required" pos

let rec transExp env main_exp =
  let rec trexp e = match e.exp_desc with
    | VarExp vexp ->
      trvar vexp
    | NilExp -> ((), NIL)
    | IntExp x -> ((), INT)
    | AssignExp (var, exp) ->
      let (_, varty) as var1 = trvar var in
      let (_, expty) as exp1 = trexp exp in
      begin match varty with
      | INT -> check_int exp1 exp.exp_pos; ((), UNIT)
      | STRING -> check_string var1 var.var_pos; ((), UNIT)
      | RECORD (_, unique) ->
        begin match expty with
        | RECORD (_, unique1) ->
          if unique != unique1 then begin
            error_at_pos "attempt to assign a record of a different type" e.exp_pos
          end;
          ((), UNIT)
        | _ ->
          error_at_pos "attempt to assign record to non-record value" e.exp_pos;
          ((), UNIT)
        end
      | ARRAY (_, unique) ->
        begin match expty with
        | ARRAY (_, unique1) ->
          if unique != unique1 then begin
            error_at_pos "attempt to assign an array of a different type" e.exp_pos
          end;
          ((), UNIT)
        | _ ->
          error_at_pos "attempt to assign array to non-array value" e.exp_pos;
          ((), UNIT)
        end
      | NIL ->
        impossible "variable should never have type nil" e.exp_pos;
        ((), UNIT)
      | UNIT ->
        ((), UNIT)
      | NAME _ ->
        impossible "variable type should never be unencountered!" e.exp_pos;
        ((), UNIT)
      end
    | StringExp _ -> ((), STRING)
    | SeqExp expseq ->
        List.fold_left
          (fun (_, _) exp -> transExp env exp)
          ((), UNIT)
          expseq
    | OpExp (lhs, PlusOp, rhs)
    | OpExp (lhs, MinusOp, rhs)
    | OpExp (lhs, TimesOp, rhs)
    | OpExp (lhs, DivideOp, rhs) ->
      check_int (trexp lhs) e.exp_pos;
      check_int (trexp rhs) e.exp_pos;
      ( (), INT )
    | LetExp (decls, exp) ->
      let new_env = List.fold_left
        (fun env dec -> transDec env dec)
        env
        decls
      in
        transExp new_env exp
    | RecordExp (params, type_symbol) ->
      (match S.look env.tenv type_symbol with
      | Some name_type ->
        let annotated_type = actual_ty name_type in
        (match annotated_type with
          | RECORD (annotated_params, unique) ->
            if List.length annotated_params <> List.length params then
              let error_msg =
                sprintf "Declared record type does not match record signature"
              in
                error_at_pos error_msg e.exp_pos;
                ((), annotated_type)
            else
              let sorted_params =
                List.sort
                  (fun (p1,_) (p2,_) ->
                    compare
                      (S.name p1.reclabel_name)
                      (S.name p2.reclabel_name))
                  params
              in
              let sorted_a_params =
                List.sort
                  (fun (p1,_) (p2,_) -> compare (S.name p1) (S.name p2))
                  annotated_params
              in
                List.iter2
                  (fun (p_lbl, p_exp) (a_lbl, a_ty) ->
                    let (_, p_ty) = trexp p_exp in
                      if not (S.compare p_lbl.reclabel_name a_lbl) then
                        let error_msg =
                          sprintf "Record labels do not match. \
                                   found '%s', expecting '%s'"
                            (S.name p_lbl.reclabel_name) (S.name a_lbl)
                        in
                          error_at_pos error_msg p_lbl.reclabel_pos
                      else if p_ty <> a_ty then
                        let error_msg =
                          sprintf "Unexpected type for parameter '%s'. \
                                   Found '%s', expected '%s'"
                            (S.name p_lbl.reclabel_name)
                            (Types.string_of_type p_ty)
                            (Types.string_of_type a_ty)
                        in
                          error_at_pos error_msg p_lbl.reclabel_pos
                  )
                  sorted_params
                  sorted_a_params;
            ((), annotated_type)
          | _ ->
            let error_msg =
              sprintf
                "This record is defined as having type '%s' \
                 which is not a record!"
              (S.name type_symbol)
            in
              error_at_pos error_msg e.exp_pos;
              ((), annotated_type)
        )
      | None ->
        let error_msg =
          sprintf "Undeclared record type '%s'" (S.name type_symbol)
        in
          error_at_pos error_msg e.exp_pos;
          ((), UNIT)
      )
    | _ ->
      error_at_pos "unimplemented expression encountered" Lexing.dummy_pos;
      ( (), INT )

  (* Translate variable *)
  and trvar v = match v.var_desc with
    | SimpleVar symbol ->
      begin match S.look env.venv symbol with
      | Some (VarEntry t) -> ((), actual_ty t)
      | Some (FunEntry _) -> error_at_pos "cannot reference a function" v.var_pos;  ((), UNIT)
      | None -> error_at_pos "undefined variable" v.var_pos; ((), UNIT)
      end
    | FieldVar (var, symbol) ->
      begin match trvar var with
      | (_, RECORD (symboltys, unique)) ->
        let field_type =
        List.fold_left
          ( fun result (symbol1, ty1) ->
            if (S.compare symbol symbol1) then Some ty1 else result )
          None
          symboltys in
        begin match field_type with
        | None -> error_at_pos "unknown record field" v.var_pos; ((), UNIT)
        | Some ty1 -> ((), actual_ty ty1)
        end
      | _ -> error_at_pos "attempting to reference member of non-record type"
      v.var_pos; ((), UNIT)
      end
    | SubscriptVar (var, exp) ->
        check_int (transExp env exp) v.var_pos;
        begin match trvar var with
        | (_, ARRAY (ty, unique)) -> ((), ty)
        | _ -> error_at_pos "attempting to access array position of non-array
        type" v.var_pos; ((), UNIT)
        end
  in
    trexp main_exp

  (* translate declarations *)
and transDec env =
  let trdec d = match d.dec_desc with
  | TyDec tydec_list ->
    let env' = List.fold_left trans_tydec env tydec_list in
      (* check for undefined types *)
      let failures = List.fold_left
        (fun acc tydec -> acc + (check_type_errors env' tydec))
        0
        tydec_list
      in
        (match failures with
        | 0 -> env'
        | _ -> env)
  | VarDec (var_symbol, escapes, optional_type_annotation_symbol, exp) ->
    let (exp_translation, exp_type) = transExp env exp in
      (match optional_type_annotation_symbol with
      | Some (type_annotation_symbol, type_annotation_pos) ->
        let optional_type_annotation = S.look env.tenv type_annotation_symbol in
          begin match optional_type_annotation with
          | Some type_annotation ->
            if exp_type <> type_annotation then begin
              error_at_pos ("Variable " ^ (S.name var_symbol) ^
                            " declared as type " ^ string_of_type type_annotation ^
                            " but is assigned with value of type " ^ string_of_type exp_type)
                            d.dec_pos;
              raise Type_mismatch
            end
          | None ->
            error_at_pos ("Undefined type " ^ (S.name type_annotation_symbol)) type_annotation_pos;
            raise Undefined_type
          end
      | None -> ()
      );
      {env with venv = S.enter env.venv var_symbol (VarEntry exp_type)}

  | _ ->
    error_at_pos "unimplemented declaration encountered" Lexing.dummy_pos;
    env
  in
    function main_dec -> trdec main_dec

let transProg env exp = ignore (transExp env exp)
