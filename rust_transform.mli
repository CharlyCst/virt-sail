open Libsail
open Ast
open Ast_defs
open Rust_gen
open Call_set

type expr_type_transform = {
    exp : rs_exp -> rs_exp;
    lexp: rs_lexp -> rs_lexp;
    pexp: rs_pexp -> rs_pexp;
    typ : rs_type -> rs_type;
}

type func_transform = {
    func : rs_fn -> rs_fn
}

val bitvec_transform: expr_type_transform
val nested_block_remover: expr_type_transform
val native_func_transform: expr_type_transform

val rust_transform_expr: expr_type_transform -> rs_program -> rs_program
val rust_transform_func: func_transform -> rs_program -> rs_program

val operator_rewriter: func_transform
val expr_type_operator_rewriter: expr_type_transform
val expr_type_hoister: expr_type_transform

val rust_remove_type_bits: rs_program -> rs_program
val rust_prelude_func_filter: rs_program -> rs_program
val insert_annotation_imports: rs_program -> rs_program
