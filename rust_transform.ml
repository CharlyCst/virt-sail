(** Rust Transformations **)
(** This module transforms raw Tust code generated from Sail into a valid Rust module. **)

open Rust_gen

(* ——————————————————————————— Custom transforms ———————————————————————————— *)

type custom_transform = {
    exp : rs_exp -> rs_exp;
    typ : rs_type -> rs_type;
}

let lexp_to_exp (lexp: rs_lexp) : rs_exp =
    match lexp with
        | RsLexpId id -> RsId id
        | _ -> RsId "LexpToExpTodo" 

let is_num_lit (pexp: rs_pexp) : bool =
    match pexp with
        | RsPexp (RsPatLit RsLitNum _, _) -> true
        | RsPexp (RsPatLit RsLitHex _, _) -> true
        | RsPexp (RsPatLit RsLitBin _, _) -> true
        | _ -> false


let bitvec_transfrom_exp (exp: rs_exp) : rs_exp =
    match exp with
        | RsApp (RsId "subrange_bits", [RsField (bitvec, "bits"); RsLit RsLitNum r_end; RsLit RsLitNum r_start]) ->
            let r_end = Int64.add r_end Int64.one in
            let subrange_call =
                Printf.sprintf "subrange::<%Lu, %Lu, %Lu>"
                    r_start
                    r_end
                    (Int64.sub r_end r_start)
            in
            RsApp (RsField (bitvec, subrange_call), [])
        | RsAssign (RsLexpIndexRange (RsLexpField (lexp, "bits"), RsLit RsLitNum r_end, RsLit RsLitNum r_start), exp) ->
            let r_end = Int64.add r_end Int64.one in
            let set_subrange =
                Printf.sprintf "set_subrange::<%Lu, %Lu, %Lu>"
                    r_start
                    r_end
                    (Int64.sub r_end r_start)
            in
            RsAssign (lexp, RsMethodApp (lexp_to_exp lexp, set_subrange, [exp]))
        | RsLit (RsLitNum n) ->
            let bitvec = Printf.sprintf "BitVector::new(%Lu)" n in
            RsId bitvec
        | RsLit (RsLitBin n) ->
            let bitvec = Printf.sprintf "BitVector::new(%s)" n in
            RsId bitvec
        | RsLit (RsLitHex n) ->
            let bitvec = Printf.sprintf "BitVector::new(%s)" n in
            RsId bitvec
        | RsMatch (exp, pat::pats) when is_num_lit pat ->
            RsMatch (RsMethodApp (exp, "bits", []), pat::pats)
        | _ -> exp

and uint_to_bitvector (n: int) : rs_type =
    if n <= 8 then
        RsTypId "u8"
    else if n <= 16 then
        RsTypId "u16"
    else if n <= 32 then
        RsTypId "u32"
    else if n <= 64 then
        RsTypId "u64"
    else
        RsTypId "InvalidBitVectorSize"

let bitvec_transfrom_type (typ: rs_type) : rs_type =
    match typ with
        | RsTypGenericParam ("bitvector", [RsTypParamNum n]) -> RsTypGenericParam ("BitVector", [RsTypParamNum n])
        | RsTypGenericParam ("bits", [RsTypParamNum n]) -> RsTypGenericParam ("BitVector", [RsTypParamNum n])

        (* TODO: once we resolve type aliasing we can remove those manual conversions *)
        | RsTypId "regbits" -> RsTypGenericParam ("BitVector", [RsTypParamNum 5])

        (* Otherwise keep as is *)
        | _ -> typ

let bitvec_transform = {
    exp = bitvec_transfrom_exp;
    typ = bitvec_transfrom_type;
}

(* ————————————————————————— Transform Expressions —————————————————————————— *)

let rec transform_pat (ct: custom_transform) (pat: rs_pat): rs_pat =
    pat

and transform_lexp (ct: custom_transform) (lexp: rs_lexp): rs_lexp =
    match lexp with
        | RsLexpId id -> RsLexpId id
        | RsLexpField (lexp, id) ->
            (RsLexpField (
                (transform_lexp ct lexp),
                id))
        | RsLexpIndex (lexp, exp) ->
            (RsLexpIndex (
                (transform_lexp ct lexp),
                (transform_exp ct exp)))
        | RsLexpIndexRange (lexp, range_start, range_end) ->
            (RsLexpIndexRange (
                (transform_lexp ct lexp),
                (transform_exp ct range_start),
                (transform_exp ct range_end)))
        | RsLexpTodo -> RsLexpTodo

and transform_exp (ct: custom_transform) (exp: rs_exp) : rs_exp =
    let exp = ct.exp exp in
    match exp with
        | RsLet (pat, exp, next) ->
            (RsLet (
                (transform_pat ct pat),
                (transform_exp ct exp),
                (transform_exp ct next)))
        | RsApp (app, args) -> transform_app ct app args
        | RsMethodApp (exp, id, args) ->
            (RsMethodApp (
                (transform_exp ct exp),
                id,
                (List.map (transform_exp ct) args)))
        | RsId id -> RsId id
        | RsLit lit -> RsLit lit
        | RsField (exp, field) -> RsField ((transform_exp ct exp), field)
        | RsBlock exps -> RsBlock (List.map (transform_exp ct) exps)
        | RsIf (cond, exp_true, exp_false) ->
            (RsIf (
                (transform_exp ct cond),
                (transform_exp ct exp_true),
                (transform_exp ct exp_false)))
        | RsMatch (exp, pexps) ->
            (RsMatch (
                (transform_exp ct exp),
                (List.map (transform_pexp ct) pexps)))
        | RsTuple exps ->
            (RsTuple
                (List.map (transform_exp ct) exps))
        | RsAssign (lexp, exp) ->
            (RsAssign (
                (transform_lexp ct lexp),
                (transform_exp ct exp)))
        | RsIndex (exp1, exp2) ->
            (RsIndex (
                (transform_exp ct exp1),
                (transform_exp ct exp2)))
        | RsBinop (exp1, binop, exp2) ->
            (RsBinop (
                (transform_exp ct exp1),
                binop,
                (transform_exp ct exp2)))
        | RsAs (exp, typ) ->
            (RsAs (
                (transform_exp ct exp),
                (transform_type ct typ)))
        | RsTodo -> RsTodo

and transform_app (ct: custom_transform) (fn: rs_exp) (args: rs_exp list) : rs_exp =
    let args = List.map (transform_exp ct) args in
    match (fn, args) with
        (* Built-in elementary operations *)
        | (RsId "plain_vector_access", [vector; item]) -> (RsIndex (vector, item))
        | (RsId "neq_int", [left; right]) -> (RsBinop (left, RsBinopNeq, right))
        | (RsId "neq_bits", [left; right]) -> (RsBinop (left, RsBinopNeq, right))
        | (RsId "eq_int", [left; right]) -> (RsBinop (left, RsBinopEq, right))
        | (RsId "eq_bool", [left; right]) -> (RsBinop (left, RsBinopEq, right))
        | (RsId "eq_bits", [left; right]) -> (RsBinop (left, RsBinopEq, right))
        | (RsId "eq_anything", [left; right]) -> (RsBinop (left, RsBinopEq, right))
        | (RsId "neq_anything", [left; right]) -> (RsBinop (left, RsBinopNeq, right))
        | (RsId "or_vec", [left; right]) -> (RsBinop (left, RsBinopOr, right))
        | (RsId "and_vec", [left; right]) -> (RsBinop (left, RsBinopAnd, right))
        | (RsId "xor_vec", [left; right]) -> (RsBinop (left, RsBinopXor, right))
        | (RsId "add_bits", [left; right]) -> (RsMethodApp (left, "wrapped_add", [right]))
        | (RsId "and_bool", [left; right]) -> (RsBinop (left, RsBinopLAnd, right))
        | (RsId "or_bool", [left; right]) -> (RsBinop (left, RsBinopLOr, right))

        (* Custom RISC-V bit extension functions *)
        | (RsId "EXTZ", (RsLit (RsLitNum n))::value::[]) ->
            (match n with
                | 8L -> RsAs (value, RsTypId "u8")
                | 16L -> RsAs (value, RsTypId "u16")
                | 32L -> RsAs (value, RsTypId "u32")
                | 64L -> RsAs (value, RsTypId "u64")
                | _ -> RsAs (value, RsTypId "InvalidUSigned")
            )
        (* Unsigned is used for array indexing *)
        | (RsId "unsigned", value::[]) -> RsAs (value, RsTypId "usize")

        (* Otherwise keep as is *)
        | _ -> (RsApp (fn, args))

and transform_pexp (ct: custom_transform) (pexp: rs_pexp) : rs_pexp =
    match pexp with
        | RsPexp (pat, exp) ->
            (RsPexp (
                (transform_pat ct pat),
                (transform_exp ct exp)))
        | RsPexpWhen (pat, exp1, exp2) ->
            (RsPexpWhen (
                (transform_pat ct pat),
                (transform_exp ct exp1),
                (transform_exp ct exp2)))
 
(* ———————————————————————————— Transform Types ————————————————————————————— *)
 
and transform_type_param (ct: custom_transform) (param: rs_type_param) : rs_type_param =
    match param with
        | RsTypParamTyp typ -> RsTypParamTyp (transform_type ct typ)
        | RsTypParamNum n -> RsTypParamNum n

and transform_type (ct: custom_transform) (typ: rs_type) : rs_type =
    let typ = ct.typ typ in
    match typ with
        | RsTypId "unit" -> RsTypUnit
        | RsTypId id -> RsTypId id
        | RsTypUnit -> RsTypUnit
        | RsTypTodo -> RsTypTodo
        | RsTypTuple types -> RsTypTuple (List.map (transform_type ct) types)
        | RsTypGeneric typ -> RsTypGeneric typ
        | RsTypGenericParam (typ, params) -> RsTypGenericParam (typ, (List.map (transform_type_param ct) params))

(* ———————————————————————— Transform Rust Programs ————————————————————————— *)

let transform_fn (ct: custom_transform) (fn: rs_fn) : rs_fn =
    let (args, ret) = fn.signature in
    let args = List.map (transform_type ct) args in
    let ret = transform_type ct ret in
    {
        name = fn.name;
        args = fn.args;
        signature = (args, ret);
        body = transform_exp ct fn.body;
    }

let rust_transform (ct: custom_transform) (RsProg fns) : rs_program =
    RsProg (List.map (transform_fn ct) fns)
