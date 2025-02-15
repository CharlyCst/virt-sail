open Libsail
open Ast
open Ast_defs
open Rust_gen
open Call_set

module StringSet : sig
    type t
    val of_list : string list -> t
  end

type expr_type_transform = {
    exp : rs_exp -> rs_exp;
    lexp: rs_lexp -> rs_lexp;
    pexp: rs_pexp -> rs_pexp;
    typ : rs_type -> rs_type;
    pat : rs_pat -> rs_pat;
    obj : rs_obj -> rs_obj;
}

type func_transform = {
    func : rs_fn -> rs_fn
}

val rust_transform_expr: expr_type_transform -> rs_program -> rs_program
val rust_transform_func: func_transform -> rs_program -> rs_program

(* Stage 2 *)
val bitvec_transform: expr_type_transform

(* Stage 3 *)
val nested_block_remover: expr_type_transform
val native_func_transform: expr_type_transform
val expr_type_hoister: expr_type_transform
val virt_context_transform: func_transform
val enum_binder_generator: (string * string) list -> expr_type_transform
val operator_rewriter: func_transform
val expr_type_operator_rewriter: expr_type_transform
val rust_remove_type_bits: rs_program -> rs_program
val rust_prelude_func_filter: rs_program -> rs_program
val insert_annotation_imports: rs_program -> rs_program
val parametric_rewriter: func_transform
val bitfield_sanitizer: func_transform
val transform_basic_types: expr_type_transform
val add_wildcard_match: expr_type_transform

(* Stage 4 *)
val sail_context_binder_generator : StringSet.t -> expr_type_transform
val sail_context_arg_inserter: expr_type_transform

(* Optimizer *)
val dead_code_remover : expr_type_transform
