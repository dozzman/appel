open Absyn
open Types
open ErrorMsg
open Env

module S = Symbol

exception Undefined_type
exception Unimplemented
exception Type_mismatch

type expty = Translate.exp * ty

let rec actual_ty t =
  match t with
  | NAME (symbol, tyref) ->
    begin match !tyref with
    | Some ty -> actual_ty ty
    | None -> t
    end
  | _ -> t

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
  | TyDec (dec_name, dec_type_exp) ->
    (match dec_type_exp.ty_desc with
    | RecordTy formal_param_list ->
      let record_types = List.fold_left
        (fun rec_type_list formal_param ->
          match S.look env.tenv formal_param.param_typename with
          | Some param_type ->
            (formal_param.param_name, param_type)::rec_type_list
          | None ->
            error_at_pos
              "undefined type for record parameter"
              formal_param.param_pos;
              rec_type_list)
        []
        formal_param_list
      in
        {env with
          tenv = S.enter env.tenv dec_name (RECORD (record_types, (ref ())))}
    | _ ->
      error_at_pos "Unimplemented type declaration encountered" Lexing.dummy_pos;
      raise Unimplemented
    )
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
