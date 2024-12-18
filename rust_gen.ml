(** Rust generation module **)


type rs_type =
    | RsTypId of string
    | RsTypTuple of rs_type list
    | RsTypUnit
    | RsTypGeneric of string
    | RsTypGenericParam of string * rs_type_param list
    | RsTypArray of rs_type_param * rs_type_param
    | RsTypOption of rs_type_param
    | RsTypTodo of string
and  rs_type_param =
    | RsTypParamTyp of rs_type
    | RsTypParamNum of int

type rs_fn_type = rs_type list * rs_type

type rs_lit =
    | RsLitUnit
    | RsLitTrue
    | RsLitFalse
    | RsLitNum of int64
    | RsLitBin of string
    | RsLitHex of string
    | RsLitStr of string
    | RsLitTodo

type rs_pat =
    | RsPatLit of rs_lit
    | RsPatId of string
    | RsPatType of rs_type * rs_pat
    | RsPatWildcard
    | RsPatTuple of rs_pat list
    | RsPatApp of rs_pat * rs_pat list
    | RsPatTodo of string
    | RsPatSome of rs_pat
    | RsPatNone

type rs_binop =
    | RsBinopEq
    | RsBinopNeq
    | RsBinopGt
    | RsBinopGe
    | RsBinopLt
    | RsBinopLe
    | RsBinopAnd
    | RsBinopOr
    | RsBinopXor
    | RsBinopLAnd
    | RsBinopLOr
    | RsBinopAdd 
    | RsBinopSub 
    | RsBinopMult 
    | RsBinopShiftLeft
    | RsBinopShiftRight
    | RsBinopMod

type rs_unop = 
    | RsUnopNeg

type rs_exp =
    | RsLet of rs_pat * rs_exp * rs_exp
    | RsApp of rs_exp * rs_exp list
    | RsMethodApp of rs_exp * string * rs_exp list
    | RsStaticApp of rs_type * string * rs_exp list
    | RsId of string
    | RsLit of rs_lit
    | RsField of rs_exp * string
    | RsBlock of rs_exp list
    | RsInstrList of rs_exp list
    | RsIf of rs_exp * rs_exp * rs_exp
    | RsMatch of rs_exp * rs_pexp list
    | RsTuple of rs_exp list
    | RsAssign of rs_lexp * rs_exp
    | RsIndex of rs_exp * rs_exp
    | RsBinop of rs_exp * rs_binop * rs_exp
    | RsUnop of rs_unop * rs_exp
    | RsAs of rs_exp * rs_type
    | RsSome of rs_exp
    | RsNone
    | RsPathSeparator of rs_type * rs_type
    | RsFor of rs_type * rs_lit * rs_lit * rs_exp
    | RsStruct of rs_type * (string * rs_exp) list
    | RsStructAssign of rs_exp * string * rs_exp
    | RsReturn of rs_exp
    | RsTodo of string
and rs_lexp =
    | RsLexpId of string
    | RsLexpField of rs_lexp * string
    | RsLexpIndex of rs_lexp * rs_exp
    | RsLexpIndexRange of rs_lexp * rs_exp * rs_exp
    | RsLexpTodo
and rs_pexp =
    | RsPexp of rs_pat * rs_exp
    | RsPexpWhen of rs_pat * rs_exp * rs_exp

type rs_block = rs_exp list

type rs_fn = {
    name: string;
    signature: rs_fn_type;
    args: rs_exp list;
    body: rs_exp;
}

type rs_enum = {
    name: string;
    fields: string list;
}

type rs_typed_enum = {
    name: string;
    fields: (string * rs_type) list;
}

type rs_struct = {
    name: string;
    fields: (string * rs_type) list;
}

type rs_alias = {
    new_typ: string;
    old_type: rs_type;
}

type rs_const = {
    name: string;
    value: string;
}

type rs_obj = 
    | RsFn of rs_fn
    | RsEnum of rs_enum
    | RsTypedEnum of rs_typed_enum
    | RsStruct of rs_struct
    | RsAlias of rs_alias
    | RsConst of rs_const
    | RsImport of string 
    | RsAttribute of string

type rs_program =
    | RsProg of rs_obj list

let merge_rs_prog (prog1: rs_program) (prog2: rs_program) : rs_program =
    let RsProg (fn1) = prog1 in
    let RsProg (fn2) = prog2 in
    RsProg (fn1 @ fn2)

let rec merge_rs_prog_list (programs: rs_program list): rs_program =
    match programs with
        | h :: t -> merge_rs_prog h (merge_rs_prog_list t)
        | _ -> RsProg []

let rec string_of_rs_type (typ: rs_type) : string =
    match typ with
        | RsTypId s -> s
        | RsTypTuple types ->
            Printf.sprintf "(%s)"
                (String.concat ", " (List.map string_of_rs_type types))
        | RsTypUnit -> "()"
        | RsTypGeneric t -> t
        | RsTypGenericParam (id, params) ->
            Printf.sprintf "%s<%s>"
                id
                (String.concat ", " (List.map string_of_rs_type_param params))
        | RsTypArray (typ, size) -> Printf.sprintf "[%s;%s]" (string_of_rs_type_param typ) (string_of_rs_type_param size)
        | RsTypOption param -> Printf.sprintf "Option<%s>" (string_of_rs_type_param param)
        | RsTypTodo e -> e
and string_of_rs_type_param (typ: rs_type_param) : string =
    match typ with
        | RsTypParamTyp typ -> string_of_rs_type typ
        | RsTypParamNum n -> Printf.sprintf "%d" n

let string_of_rs_lit (lit: rs_lit) : string =
    match lit  with
        | RsLitUnit -> "()"
        | RsLitTrue -> "true"
        | RsLitFalse -> "false"
        | RsLitNum n -> Printf.sprintf "%Li" n
        | RsLitBin n -> n
        | RsLitHex n -> n
        | RsLitStr s -> Printf.sprintf "String::from(\"%s\")" s
        | RsLitTodo -> "LIT_TODO"

let rec string_of_rs_pat (pat: rs_pat) : string =
    match pat with
        | RsPatLit lit -> string_of_rs_lit lit
        | RsPatId id-> id
        | RsPatType (typ, RsPatWildcard) -> "_"
        | RsPatType (typ, pat) -> Printf.sprintf "%s: %s" (string_of_rs_pat pat) (string_of_rs_type typ)
        | RsPatWildcard -> "_"
        | RsPatTuple pats ->
            Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_rs_pat pats))
        | RsPatApp (name, args) -> 
            Printf.sprintf "%s(%s)" (string_of_rs_pat name)  
            (String.concat ", " (List.map string_of_rs_pat args))
        | RsPatSome pat -> Printf.sprintf "Some(%s)" (string_of_rs_pat pat)
        | RsPatNone -> "None"
        | RsPatTodo text -> Printf.sprintf "%s" text

let string_of_rs_binop (binop: rs_binop) : string =
    match binop with
        | RsBinopEq -> "=="
        | RsBinopNeq -> "!="
        | RsBinopGt -> ">"
        | RsBinopGe -> ">="
        | RsBinopLt -> "<"
        | RsBinopLe -> "<="
        | RsBinopAnd -> "&"
        | RsBinopOr -> "|"
        | RsBinopXor -> "^"
        | RsBinopLAnd -> "&&"
        | RsBinopLOr -> "||"
        | RsBinopAdd -> "+"
        | RsBinopSub -> "-"
        | RsBinopMult -> "*"
        | RsBinopShiftLeft -> "<<"
        | RsBinopShiftRight -> ">>"
        | RsBinopMod -> "%"

let string_of_rs_unop (unop: rs_unop) : string =
    match unop with
        | RsUnopNeg -> "!"

let indent (n: int) : string =
    String.make (n * 4) ' '

let rec string_of_rs_exp (n: int) (exp: rs_exp) : string =
    match exp with
        (* The block indentation if not nedded after a  let, remove it to pretify*)
        | RsLet (pat, exp, RsBlock exps) ->
            Printf.sprintf "let %s = %s;\n%s%s"
                (string_of_rs_pat pat)
                (string_of_rs_exp n exp)
                (indent n)
                (String.concat
                    (Printf.sprintf ";\n%s" (indent n))
                    (List.map (string_of_rs_exp n) exps))
        | RsLet (pat, exp, next) ->
            Printf.sprintf "let %s = %s;\n%s%s"
                (string_of_rs_pat pat)
                (string_of_rs_exp n exp)
                (indent n)
                (string_of_rs_exp n next)
        | RsApp (fn, args)->
            Printf.sprintf "%s(%s)"
                (string_of_rs_exp n fn)
                (String.concat ", " (List.map (string_of_rs_exp n) args))
        | RsStaticApp(typ, func, args) ->
            Printf.sprintf "%s::%s(%s)" 
                (string_of_rs_type typ)
                func
                (String.concat ", " (List.map (string_of_rs_exp n) args))
        | RsMethodApp (exp, id, args)->
            Printf.sprintf "%s.%s(%s)"
                (string_of_rs_exp n exp)
                id 
                (String.concat ", " (List.map (string_of_rs_exp n) args))
        | RsId id -> id
        | RsLit lit  -> string_of_rs_lit lit
        | RsField (exp, field) ->
            Printf.sprintf "%s.%s"
                (string_of_rs_exp n exp)
                field
        | RsBlock exps ->
            Printf.sprintf "{\n%s%s\n%s}"
                (indent (n + 1))
                (String.concat
                    (Printf.sprintf ";\n%s" (indent (n + 1)))
                    (List.map (string_of_rs_exp (n + 1)) exps))
                (indent n)
        | RsInstrList exps ->
            Printf.sprintf "%s"
                (String.concat
                    (Printf.sprintf ";\n%s" (indent (n)))
                    (List.map (string_of_rs_exp (n)) exps))
        | RsIf (cond, then_exp, else_exp) ->
            Printf.sprintf "if {%s} {\n%s%s\n%s} else %s"
                (string_of_rs_exp n cond)
                (indent (n + 1))
                (string_of_rs_exp (n + 1) then_exp)
                (indent n)
                (match else_exp with
                    | RsIf (_, _, _) -> (string_of_rs_exp n else_exp)
                    | _ -> (Printf.sprintf "{\n%s%s\n%s}"
                        (indent (n + 1))
                        (string_of_rs_exp (n+1) else_exp))
                        (indent n))
        | RsMatch (exp, pexps) ->
            Printf.sprintf "match %s {\n%s%s%s}"
                (string_of_rs_exp n exp)
                (indent (n + 1))
                (String.concat
                    (indent (n + 1))
                    (List.map (string_of_rs_pexp (n+1)) pexps))
                (indent n)
        | RsTuple exps ->
            Printf.sprintf "(%s)" (String.concat ", " (List.map (string_of_rs_exp n) exps))
        | RsAssign (exp1, exp2) ->
            Printf.sprintf "%s = %s"
                (string_of_rs_lexp n exp1)
                (string_of_rs_exp n exp2)
        | RsIndex (exp1, exp2) ->
            Printf.sprintf "%s[%s]"
                (string_of_rs_exp n exp1)
                (string_of_rs_exp n exp2)
        | RsBinop (exp1, binop, exp2) ->
            Printf.sprintf "(%s %s %s)"
                (string_of_rs_exp n exp1)
                (string_of_rs_binop binop)
                (string_of_rs_exp n exp2)
        | RsUnop (unop, exp) ->
            Printf.sprintf "%s(%s)"
                (string_of_rs_unop unop)
                (string_of_rs_exp n exp)
        | RsAs (exp, typ) ->
            Printf.sprintf "(%s as %s)"
                (string_of_rs_exp (n + 1) exp)
                (string_of_rs_type typ)
        | RsSome (exp) -> 
            Printf.sprintf "Some(%s)" (string_of_rs_exp n exp)
        | RsNone -> "None"
        | RsPathSeparator (t1, t2) -> Printf.sprintf "%s::%s" (string_of_rs_type t1) (string_of_rs_type t2)
        | RsFor (var, start, until, body) -> Printf.sprintf "for %s in %s..=%s {\n%s%s\n%s}"
            (string_of_rs_type var)
            (string_of_rs_lit start)
            (string_of_rs_lit until)
            (indent (n + 1))
            (string_of_rs_exp (n + 1) body)
            (indent n)
        | RsStruct (name, entries) -> 
            Printf.sprintf "%s {\n%s%s\n%s}"
            (string_of_rs_type name)
            (indent (n + 1))
            (String.concat
                (Printf.sprintf ",\n%s" (indent (n + 1)))
                (List.map (fun (name, typ) -> (Printf.sprintf "%s: %s" name (string_of_rs_exp (n+1) typ))) entries))
            (indent n)
        | RsStructAssign (exp, field, value) -> 
            Printf.sprintf "%s.%s = %s; %s" (string_of_rs_exp n exp) field (string_of_rs_exp n value) (string_of_rs_exp n exp)
        | RsReturn exp -> Printf.sprintf "return %s;" (string_of_rs_exp n exp)
        | RsTodo text -> Printf.sprintf "todo!(\"%s\")" text
and string_of_rs_lexp (n: int) (lexp: rs_lexp) : string =
    match lexp with
        | RsLexpId id -> id
        | RsLexpField (lexp, id) ->
            Printf.sprintf "%s.%s"
                (string_of_rs_lexp n lexp)
                id
        | RsLexpIndex (lexp, idx) ->
            Printf.sprintf "%s[%s]"
                (string_of_rs_lexp n lexp)
                (string_of_rs_exp n idx)
        | RsLexpIndexRange (lexp, range_start, range_end) ->
            (* Implement support for this case if the assertion fails *)
            assert ((string_of_rs_exp n range_start) = "(64 - 1)"); 
            assert ((string_of_rs_exp n range_end) = "0"); 
            string_of_rs_lexp n lexp
        | RsLexpTodo ->  "LEXP_TODO"
and string_of_rs_pexp (n: int) (pexp: rs_pexp) : string =
    match pexp with
        | RsPexp (pat, exp) ->
            Printf.sprintf "%s => {%s}\n"
                (string_of_rs_pat pat)
                (string_of_rs_exp n exp)
        | RsPexpWhen (pat, cond_exp, exp) ->
            Printf.sprintf "%s if {%s} => {%s}\n"
                (string_of_rs_pat pat)
                (string_of_rs_exp n cond_exp)
                (string_of_rs_exp n exp)

let string_of_rs_fn_args (fn: rs_fn) : string =
    let string_of_arg_and_type (arg: rs_exp) (typ: rs_type) : string =
        match typ with
            | RsTypUnit -> "unit_arg: ()"
            | _ -> Printf.sprintf "%s: %s"
                (string_of_rs_exp 0 arg)
                (string_of_rs_type typ)
    in
    let (arg_types, _) = fn.signature in
    String.concat ", " (List.map2 string_of_arg_and_type fn.args arg_types)

let string_of_rs_fn (fn: rs_fn) : string =
    let (args, ret) = fn.signature in 
    let args = string_of_rs_fn_args fn in
    let (_, ret_type) = fn.signature in
    let ret_type = match ret_type with
        | RsTypUnit -> ""
        | _ -> Printf.sprintf " -> %s" (string_of_rs_type ret_type)
    in
    let signature = Printf.sprintf "pub fn %s(%s)%s {\n%s" fn.name args ret_type (indent 1) in
    let stmts = (match fn.body with
        | RsBlock exps
            -> String.concat
                (Printf.sprintf ";\n%s" (indent 1))
                (List.map (string_of_rs_exp 1) exps)
        | _ ->string_of_rs_exp 1 fn.body) in
    Printf.sprintf "%s%s\n}" signature stmts

let remove_last_char (s: string) : string =
    if String.length s = 0 then
        s
    else
        String.sub s 0 (String.length s - 1)

let parse_enum_fields (entries: string list) : string = 
    let prefixed_entries = List.map (fun s -> "    " ^ s ^ ",\n") entries in
    let merged_fields = String.concat "" prefixed_entries in
    remove_last_char merged_fields (* Removes last '\n'*)
 
let get_enum_guards () : string = "#[derive(Eq, PartialEq, Clone, Copy, Debug)]"

let string_of_rs_enum (enum: rs_enum) : string = 
    Printf.sprintf "%s\npub enum %s {\n%s\n}" (get_enum_guards()) enum.name (parse_enum_fields enum.fields)

let parse_typed_enum_fields (entries: (string * rs_type) list) : string = 
    let prefixed_entries = List.map (fun s -> "    " ^ (fst s) ^ "(" ^ (string_of_rs_type (snd s)) ^ "),\n") entries in
    let merged_fields = String.concat "" prefixed_entries in
    remove_last_char merged_fields (* Removes last '\n'*)

let string_of_rs_typed_enum (enum: rs_typed_enum) : string = 
    Printf.sprintf "%s\npub enum %s {\n%s\n}" (get_enum_guards()) enum.name (parse_typed_enum_fields enum.fields)

let parse_struct_fields (entries: (string * rs_type)  list) : string = 
    let prefixed_entries = List.map (fun s -> "    pub " ^ (fst s) ^ ": " ^ string_of_rs_type (snd s) ^ ",\n") entries in
    let merged_fields = String.concat "" prefixed_entries in
    remove_last_char merged_fields (* Removes last '\n'*)

let string_of_rs_struct (struc: rs_struct) : string = 
    Printf.sprintf "#[derive(Eq, PartialEq, Clone, Copy, Debug)]\npub struct %s {\n%s\n}" struc.name (parse_struct_fields struc.fields)

let string_of_rs_obj (obj: rs_obj) : string =
    match obj with
        | RsFn fn -> string_of_rs_fn fn
        | RsEnum enum -> string_of_rs_enum enum 
        | RsTypedEnum typed_enum -> string_of_rs_typed_enum typed_enum
        | RsStruct struc -> string_of_rs_struct struc
        | RsAlias alias -> Printf.sprintf "pub type %s = %s;" alias.new_typ (string_of_rs_type alias.old_type)
        | RsConst const -> Printf.sprintf "pub const %s: usize = %s;" const.name const.value
        | RsAttribute value -> Printf.sprintf "#![%s]" value
        | RsImport value -> Printf.sprintf "use %s;" value

let string_of_rs_prog (prog: rs_program) : string =
    let RsProg (funs) = prog in
    String.concat "\n\n" (List.map string_of_rs_obj funs)
