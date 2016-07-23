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

let check_int (exp, ty) pos =
  if ty != INT then
    error_at_pos "integer required" pos

let check_string(exp, ty) pos =
  if ty != STRING then
    error_at_pos "string required" pos

let rec transExp venv tenv main_exp = 
  let rec trexp =
    function
    | VarExp (SimpleVar (symbol, pos)) ->
      begin match S.look venv symbol with
      | Some (VarEntry t) -> ((), t)
      | Some (FunEntry _) -> error_at_pos "expected variable, found function" pos; ((), INT)
      | None -> error_at_pos "undefined variable" pos; ((), INT)
      end
    | NilExp -> ( (), NIL)
    | IntExp x -> ( (), INT)
    | AssignExp (var, exp, pos) ->
      let (_, varty) as var1 = trvar var in
      let (_, expty) as exp1 = trexp exp in
      begin match varty with
      | INT -> check_int exp1 pos; ((), UNIT)
      | STRING -> check_string exp1 pos; ((), UNIT)
      | RECORD (_, unique) ->
        begin match expty with
        | RECORD (_, unique1) ->
          if unique != unique1 then begin
            error_at_pos "attempt to assign a record of a different type" pos
          end;
          ((), UNIT)
        | _ ->
          error_at_pos "attempt to assign record to non-record value" pos;
          ((), UNIT)
        end
      | ARRAY (_, unique) ->
        begin match expty with
        | ARRAY (_, unique1) ->
          if unique != unique1 then begin
            error_at_pos "attempt to assign an array of a different type" pos
          end;
          ((), UNIT)
        | _ ->
          error_at_pos "attempt to assign array to non-array value" pos;
          ((), UNIT)
        end
      | NIL ->
        impossible "variable should never have type nil" pos;
        ((), UNIT)
      | UNIT ->
        ((), UNIT)
      | NAME _ ->
        impossible "variable type should never be unencountered!" pos;
        ((), UNIT)
      end
    | StringExp (s, pos) -> ((), STRING)
(*    | SeqExp expseq -> List.iter (function x -> tre*)
    | OpExp (lhs, PlusOp, rhs, pos)
    | OpExp (lhs, MinusOp, rhs, pos)
    | OpExp (lhs, TimesOp, rhs, pos)
    | OpExp (lhs, DivideOp, rhs, pos) ->
      check_int (trexp lhs) pos;
      check_int (trexp rhs) pos; 
      ( (), INT )
    | LetExp (decls, exps, pos) ->
      let rec translate_decs envs = function
      | dec::decs ->
          let new_env = transDec envs.venv envs.tenv dec in
            translate_decs new_env decs
      | [] -> envs
      in
        let new_envs = translate_decs {venv = venv; tenv = tenv} decls in
        let translated_exps = List.map (fun (exp, _) -> transExp new_envs.venv new_envs.tenv exp) exps in
          translated_exps |> List.rev |> List.hd
    | _ ->
      error_at_pos "unimplemented expression encountered" Lexing.dummy_pos;
      ( (), INT )

  (* Translate variable *)
  and trvar = function
    | SimpleVar (symbol, pos) ->
      begin match S.look venv symbol with
      | Some (VarEntry t) -> ((), actual_ty t)
      | Some (FunEntry _) -> error_at_pos "cannot reference a function" pos;  ((), UNIT)
      | None -> error_at_pos "undefined variable" pos; ((), UNIT)
      end
    | FieldVar (var, symbol, pos) ->
      begin match trvar var with
      | (_, RECORD (symboltys, unique)) ->
        let field_type =
        List.fold_left
          ( fun result (symbol1, ty1) ->
            if (S.compare symbol symbol1) then Some ty1 else result )
          None
          symboltys in
        begin match field_type with
        | None -> error_at_pos "unknown record field" pos; ((), UNIT)
        | Some ty1 -> ((), actual_ty ty1)
        end
      | _ -> error_at_pos "attempting to reference member of non-record type" pos; ((), UNIT)
      end
    | SubscriptVar (var, exp, pos) ->
        check_int (transExp venv tenv exp) pos;
        begin match trvar var with
        | (_, ARRAY (ty, unique)) -> ((), ty)
        | _ -> error_at_pos "attempting to access array position of non-array type" pos; ((), UNIT)
        end
  in
    trexp main_exp

  (* translate declarations *)
and transDec venv tenv =
  let trdec = function
  | TyDec (dec_name, dec_type, dec_pos) ->
    begin match dec_type with
    | RecordTy record_entries ->
      let rec parse_record_types = function
      | (name, escapes, entry_type_symbol, pos)::rest ->
        begin match S.look tenv entry_type_symbol with
        | Some entry_type -> (name, entry_type)::(parse_record_types rest)
        | None ->
          error_at_pos ("Undefined type for record entry" ^ (S.name name)) pos;
          raise Undefined_type
        end
      | [] -> []
      in
        {venv = venv; tenv = S.enter tenv dec_name (RECORD (parse_record_types record_entries, ref ()))}
    | _ ->
      error_at_pos "Unimplemented type declaration encountered" Lexing.dummy_pos;
      raise Unimplemented
    end
  | VarDec (var_symbol, escapes, optional_type_annotation_symbol, exp, pos) ->
    let (exp_translation, exp_type) = transExp venv tenv exp in
      begin match optional_type_annotation_symbol with
      | Some (type_annotation_symbol, type_annotation_pos) ->
        let optional_type_annotation = S.look tenv type_annotation_symbol in
          begin match optional_type_annotation with
          | Some type_annotation ->
            if exp_type <> type_annotation then begin
              error_at_pos ("Variable " ^ (S.name var_symbol) ^
                            " declared as type " ^ string_of_type type_annotation ^
                            " but is assigned with value of type " ^ string_of_type exp_type)
                            pos;
              raise Type_mismatch
            end
          | None ->
            error_at_pos ("Undefined type " ^ (S.name type_annotation_symbol)) type_annotation_pos;
            raise Undefined_type
          end
      | None -> ()
      end;
      {venv = S.enter venv var_symbol (VarEntry exp_type); tenv = tenv}

  | _ ->
    error_at_pos "unimplemented declaration encountered" Lexing.dummy_pos;
    {venv = venv; tenv = tenv}
  in
    function main_dec -> trdec main_dec

let transProg venv tenv exp = ignore (transExp venv tenv exp)
