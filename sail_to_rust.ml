(** Converts a Sail AST to a Rust AST **)

open Libsail
open Ast
open Ast_util
open Ast_defs
open Rust_gen
open Call_set

module SSet = Call_set.SSet

let print_id id =
    match id with
        | Id_aux (Id (x), _) -> print_string "Id "; print_endline x;
        | Id_aux (Operator (x), _) -> print_string "Op "; print_endline x

let process_register reg : rs_program =
    print_string "Register ";
    let (typ, id, exp) = match reg with | DEC_reg (typ, id, exp) -> (typ, id, exp) in
    print_id id;
    RsProg []

let process_scattered scattered : rs_program =
    print_string "Scattered ";
    (match scattered with
        | SD_function (rec_aux, tannot, id) -> print_string "function"; print_id id
        | SD_funcl (funcl) -> print_string "funcl"
        | SD_variant (id, typquant) -> print_string "variant"; print_id id
        | SD_unioncl (id, union_type) -> print_string "union"; print_id id
        | SD_mapping (id, tannot_opt) -> print_string "mapping"; print_id id
        | _ -> ());
    RsProg []

let process_type (Typ_aux (typ, annot)) : rs_type =
    match typ with
        | Typ_id id -> RsTypId (string_of_id id)
        | _ -> RsTypTodo

let process_vector_pat (items: 'a pat list) : rs_lit =
    let is_only_bits acc pat = match pat with
        | P_aux ((P_lit (L_aux (lit, _))), _) -> (match lit with
            | L_zero -> acc
            | L_one -> acc
            | _ -> false)
        | _ -> false
    in
    let string_of_bit (P_aux (pat, _)) = match pat with
        | P_lit (L_aux (lit, _)) -> (match lit with
            | L_zero -> "0"
            | L_one -> "1"
            | _ -> "x")
        | _ -> "X"
    in
    if (List.fold_left is_only_bits true items) then
        RsLitBin (Printf.sprintf "0b%s" (String.concat "" (List.map string_of_bit items)))
    else RsLitTodo

let process_lit (L_aux (lit, _)) : rs_lit =
    match lit with
        | L_unit -> RsLitUnit
        | L_zero -> RsLitNum Int64.zero
        | L_one -> RsLitNum Int64.one
        | L_true -> RsLitTrue
        | L_false -> RsLitFalse
        | L_num n -> RsLitNum (Big_int.to_int64 n)
        | L_hex s -> RsLitHex s
        | L_bin s -> RsLitBin s
        | L_string s -> RsLitStr s
        | L_undef -> RsLitTodo
        | L_real s -> RsLitTodo

let rec process_pat (P_aux (pat, annot)) : rs_pat =
    match pat with
        | P_lit lit -> RsPatLit (process_lit lit)
        | P_id id -> RsPatId (string_of_id id)
        | P_typ (typ, pat) -> RsPatType ((process_type typ), (process_pat pat))
        | P_wild -> RsPatWildcard
        | P_tuple pats -> RsPatTuple (List.map process_pat pats)
        | P_vector pats -> RsPatLit (process_vector_pat pats)
        | _ -> RsPatTodo

let process_vector (items: 'a exp list) : rs_lit =
    let is_only_bits acc exp = match exp with
        | E_aux (E_lit(L_aux(lit, _)), _) -> (match lit with
            | L_zero -> acc
            | L_one -> acc
            | _ -> false)
        | _ -> false
    in
    let string_of_bit (E_aux (exp, _)) = match exp with
        | E_lit (L_aux (lit, _)) -> (match lit with
            | L_zero -> "0"
            | L_one -> "1"
            | _ -> "x")
        | _ -> "X"
    in
    if (List.fold_left is_only_bits true items) then
        RsLitBin (Printf.sprintf "0b%s" (String.concat "" (List.map string_of_bit items)))
    else RsLitTodo

let rec process_exp (E_aux (exp, aux)) : rs_exp = 
    (* print_string "Exp "; *)
    (* print_endline (string_of_exp exp); *)
    match exp with
        | E_block exp_list -> RsBlock (List.map process_exp exp_list)
        | E_id id -> RsId (string_of_id id)
        | E_lit lit -> RsLit (process_lit lit)
        | E_typ (typ, exp) -> RsTodo (*print_string (string_of_typ typ);*)
        | E_app (id, exp_list) -> RsApp ((string_of_id id), (List.map process_exp exp_list))
        | E_app_infix (exp1, id, exp2) -> RsTodo
        | E_tuple (exp_list) -> RsTuple (List.map process_exp exp_list)
        | E_if (exp1, exp2, exp3) -> RsIf ((process_exp exp1), (process_exp exp2), (process_exp exp3)) 
        | E_loop (loop, measure, exp1, exp2) -> RsTodo
        | E_for (id, exp1, exp2, exp3, order, exp4) -> RsTodo
        | E_vector (exp_list) -> RsLit (process_vector exp_list)
        | E_vector_access (exp1, exp2) -> RsTodo
        | E_vector_subrange (exp1, exp2, exp3) -> RsTodo
        | E_vector_update (exp1, exp2, exp3) -> RsTodo
        | E_vector_update_subrange (exp1 ,exp2, exp3, exp4) -> RsTodo
        | E_vector_append (exp1 ,exp2) -> RsTodo
        | E_list (exp_list) -> RsTodo
        | E_cons (exp1, exp2) -> RsTodo
        | E_struct (fexp_list) -> RsTodo
        | E_struct_update (exp, fexp_list) -> RsTodo
        | E_field (exp, id) -> RsTodo
        | E_match (exp, pexp_list)
            -> (RsMatch (
                (process_exp exp),
                (List.map process_pexp pexp_list)
            ))
        | E_let (LB_aux (LB_val (let_var, let_exp), _), exp)
            -> (RsLet (
                (process_pat let_var),
                (process_exp let_exp),
                (process_exp exp)
            ))
        | E_assign (lexp, exp) -> 
            (RsAssign (
                (process_lexp lexp),
                (process_exp exp)
            ))
        | E_sizeof nexp -> RsTodo
        | E_return exp -> RsTodo
        | E_exit exp -> RsTodo
        | E_ref id -> RsTodo
        | E_throw exp -> RsTodo
        | E_try (exp, pexp_list) -> RsTodo
        | E_assert (exp1, exp2) -> RsTodo
        | E_var (lexp, exp1, exp2) -> RsTodo
        | E_internal_plet (pat, exp1, exp2) -> RsTodo
        | E_internal_return exp -> RsTodo
        | E_internal_value value -> RsTodo
        | E_internal_assume (n_constraint, exp) -> RsTodo
        | E_constraint n_constraint -> RsTodo
and process_lexp (LE_aux (lexp, annot)) : rs_lexp =
    match lexp with
        | LE_id id -> RsLexpId (string_of_id id)
        | LE_vector (lexp, idx) ->
            (RsLexpIndex (
                (process_lexp lexp),
                (process_exp idx)))
        | _ -> RsLexpTodo
and process_pexp (Pat_aux (pexp, annot)) : rs_pexp =
    match pexp with
        | Pat_exp (pat, exp) ->
            (RsPexp (
                (process_pat pat),
                (process_exp exp)
            ))
        | Pat_when (pat, exp1, exp2) ->
            (RsPexpWhen (
                (process_pat pat),
                (process_exp exp1),
                (process_exp exp2)
            ))



(* Return the ID of an application pattern as a string, or "" otherwise. *)
let pat_app_name (P_aux (pat_aux, _)) =
    match pat_aux with
        | P_app (id, _) -> string_of_id id
        | _ -> ""

let process_pat (P_aux (pat_aux, annot)) = 
    match pat_aux with
        | P_app (id, pat_list) -> print_id id
        | _ -> ()

let process_func (FCL_aux (func, annot)) (s: SSet.t) : rs_program =
    let (id, pexp) = match func with | FCL_funcl (id, pexp) -> (id, pexp) in
    let (pexp, annot) = match pexp with | Pat_aux (pexp, annot) -> (pexp, annot) in
    let name = (string_of_id id) in
    if SSet.mem name s then match pexp with
        | Pat_exp (pat, exp) ->
            let rs_exp = process_exp exp in
            RsProg [(name, rs_exp)]
        | Pat_when (pat1, exp, pat2) -> RsProg []
    else match pexp with
        | Pat_exp (pat, exp) ->
            let name = pat_app_name pat in
            if SSet.mem name s then
                let rs_exp = process_exp exp in
                RsProg [(name, rs_exp)]
            else RsProg []
        | _ -> RsProg []

let rec process_funcl (funcl: 'a funcl list) (s: SSet.t) : rs_program =
    match funcl with
        | h :: t -> merge_rs_prog (process_func h s) (process_funcl t s)
        | [] -> RsProg []

let process_fundef (FD_function (rec_opt, tannot_opt, funcl)) (s: SSet.t) : rs_program =
    process_funcl funcl s

let process_node (DEF_aux (def, annot)) (s: SSet.t) : rs_program =
    match def with
        | DEF_register (DEC_aux (dec_spec, annot)) -> process_register dec_spec
        | DEF_scattered (SD_aux (scattered, annot)) -> process_scattered scattered
        | DEF_fundef (FD_aux (fundef, annot)) -> process_fundef fundef s
        | DEF_impl funcl -> process_func funcl s
        | _ -> RsProg []

let rec process_defs defs (s: SSet.t): rs_program =
    match defs with
        | h :: t -> merge_rs_prog (process_node h s) (process_defs t s)
        | [] -> RsProg []

let sail_to_rust (ast: 'a ast) (s: SSet.t) : rs_program =
    process_defs ast.defs s
