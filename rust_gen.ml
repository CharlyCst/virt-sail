(** Rust generation module **)

type rs_type =
    | RsTypId of string
    | RsTypTodo

type rs_pat =
    | RsPatLit
    | RsPatId of string
    | RsPatType of rs_type * rs_pat
    | RsPatTodo

type rs_lit =
    | RsLitUnit
    | RsLitTrue
    | RsLitFalse
    | RsLitNum of int64
    | RsLitBin of string
    | RsLitHex of string
    | RsLitStr of string
    | RsLitTodo

type rs_exp =
    | RsLet of rs_pat * rs_exp * rs_exp
    | RsApp of string * rs_exp list
    | RsId of string
    | RsLit of rs_lit
    | RsBlock of rs_exp list
    | RsIf of rs_exp * rs_exp * rs_exp
    | RsTodo

type rs_block = rs_exp list

type rs_fn = string * rs_exp

let string_of_rs_type (typ: rs_type) : string =
    match typ with
        | RsTypId s -> s
        | RsTypTodo -> "TYPE_TODO"

let rec string_of_rs_pat (pat: rs_pat) : string =
    match pat with
        | RsPatLit -> "pat_lit"
        | RsPatId id-> id
        | RsPatType (typ, pat) -> Printf.sprintf "%s: %s" (string_of_rs_pat pat) (string_of_rs_type typ)
        | RsPatTodo -> "PAT_TODO"

let string_of_rs_lit (lit: rs_lit) : string =
    match lit  with
        | RsLitUnit -> "()"
        | RsLitTrue -> "true"
        | RsLitFalse -> "false"
        | RsLitNum n -> Printf.sprintf "%Li" n
        | RsLitBin n -> n
        | RsLitHex n -> n
        | RsLitStr s -> Printf.sprintf "\"%s\"" s
        | RsLitTodo -> "LIT_TODO"

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
                (String.concat ";\n" (List.map (string_of_rs_exp n) exps))
        | RsLet (pat, exp, next) ->
            Printf.sprintf "let %s = %s;\n%s%s"
                (string_of_rs_pat pat)
                (string_of_rs_exp n exp)
                (indent n)
                (string_of_rs_exp n next)
        | RsApp (id, args)->
            Printf.sprintf "%s(%s)"
                id 
                (String.concat ", " (List.map (string_of_rs_exp n) args))
        | RsId id -> id
        | RsLit lit  -> string_of_rs_lit lit
        | RsBlock exps ->
            Printf.sprintf "{\n%s%s%s\n}"
                (indent n)
                (String.concat ";\n" (List.map (string_of_rs_exp (n + 1)) exps))
                (indent n)
        | RsIf (cond, then_exp, else_exp) ->
            Printf.sprintf "if %s {\n%s%s\n%s} else %s"
                (string_of_rs_exp n cond)
                (indent (n + 1))
                (string_of_rs_exp (n + 1) then_exp)
                (indent n)
                (match else_exp with
                    | RsIf (_, _, _) -> (string_of_rs_exp n else_exp)
                    | _ -> (Printf.sprintf "{\n%s%s\n%s}"
                        (indent (n + 1))
                        (string_of_rs_exp n else_exp))
                        (indent n))
        | RsTodo -> "todo!()"

let string_of_rs_fn (fn: rs_fn) : string =
    let (name, exp) = fn in
    let signature = Printf.sprintf "fn %s() {\n%s" name (indent 1) in
    let stmts = (match exp with
        | RsBlock exps -> String.concat (indent 1) (List.map (string_of_rs_exp 1) exps)
        | _ ->string_of_rs_exp 1 exp) in
    Printf.sprintf "%s%s\n}" signature stmts
