open Angstrom

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alpha = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_name_char = function
  | '_' | '-' | '.' -> true
  | c -> (is_alpha c) || (is_digit c)

let is_param_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | _ -> false

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

module Ast = struct
  type literal = [ `Numeric of string | `String of string ] [@@ deriving show, eq]

  type computation = {
    identifier: string;
    parameters: literal list;
  } [@@ deriving show, eq]

  type statement =
    | Anonymous of expression
    | Label of string * expression
    | Assignment of string * expression
  [@@ deriving show, eq]

  and expression =
    | Computation of computation
    | Block of statement list
    | Override of expression * expression
    | Repeat of expression * int
  [@@ deriving show, eq]
end

module Parsers = struct
  let escaped =
    (char '\\') *> (satisfy (function | '"' | '\\' -> true | _ -> false))

  let regular_string_char =
    satisfy (function | '"' | '\\' -> false | _ -> true)

  let quoted_char =
    escaped <|> regular_string_char

  let quoted_string =
    (char '"') *> (many quoted_char) <* (char '"')
    >>| (fun chars -> `String (string_of_chars chars))

  let comment =
    char '#' <*
    skip_while (function | '\x0a' | '\x0d' -> false | _ -> true) <*
    advance 1
    >>| ignore

  let is_ws = function
    | '\x20' | '\x09' -> true
    | _ -> false

  (* zero or more whitespace *)
  let ws0 = skip_while is_ws

  (* one or more whitespace *)
  let ws1 = take_while1 (function
    | '\x20' | '\x09' -> true
    | _ -> false
  ) >>| ignore

  (* exactly one newline *)
  let nl = skip (function
    | '\x0a' | '\x0d' -> true
    | _ -> false
  )

  (* zero or more whitespace or newline characters *)
  let wsnl0 = skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false
  )

  (* zero or more whitespace, comments or newlines *)
  let wsc0 =
    wsnl0 <* sep_by wsnl0 comment <* wsnl0

  (* require a line ending *)
  let eol =
    ws0 <* (comment <|> nl <|> end_of_input) <* wsc0

  let name =
    peek_char >>= function
      | None -> fail "EOF, expected name"
      | Some c ->
        if is_alpha c
        then take_while1 is_name_char
        else fail (Format.sprintf "Invalid start of name: '%C'" c)

  let label =
    name <* ws0 <* (char ':')

  let optional_label =
    let some x = Some x in
    option None (label >>| some)

  let value =
    lift2 (fun sign v -> `Numeric (sign ^ v))
      (option "" (char '-' >>| (fun _ -> "-")))
      (take_while1 is_param_char)
    >>= fun v ->
    match v with
    | `Numeric "with" -> fail "Unexpected keyword 'with'"
    | _ as n -> return n

  let param =
    value <|> quoted_string

  let single_parameter =
    param >>| (fun x -> [x])

  let multi_parameter =
    (char '[') *> wsc0 *> sep_by wsc0 param  <* wsc0 <* (char ']')

  let parameters1 =
    multi_parameter <|> single_parameter

  let multiplier =
    (char '*') *> ws0 *> (take_while1 is_digit) >>| int_of_string

  let optional_multiplier =
    option 1 multiplier

  let const_name =
    string "const" *> ws0 *> name <* ws0 <* char '='

  let end_of_input =
    peek_char >>= function
    | Some c -> fail (Format.sprintf "Unexpected character: %C" c)
    | None -> return ()

  let debug msg =
    (return () >>| fun _ -> Format.printf msg)

  let computation =
    lift2
    (
      fun identifier parameters ->
      Ast.Computation {identifier; parameters}
    )
    (name)
    ((ws0 *> parameters1) <|> return [])

  let start_block = (char '{')

  let end_block = (char '}')

  let statement =
    fix (fun stmt ->
      let block =
        (start_block *> wsc0 *> sep_by eol stmt <* wsc0 <* end_block) >>| fun exprs -> Ast.Block exprs
      in
      let term = choice [ computation; block ] in
      let override =
        lift2
        (fun source override -> Ast.Override (source, override))
        (term <* ws1 <* string "with" <* ws1)
        block
      in
      let repeat =
        lift2
        (fun expr mult -> Ast.Repeat (expr, mult))
        (choice [ override; term ])
        (ws0 *> multiplier)
      in
      let expression = choice [ repeat; override; term ] in
      let assignment_stmt =
        lift2
        (fun name expr -> Ast.Assignment (name, expr))
        (const_name <* ws0)
        expression
      in
      let label_stmt =
        lift2
        (fun label expr -> Ast.Label (label, expr))
        (label  <* ws0)
        expression
      in
      let anon_stmt =
        expression >>| fun expr -> Ast.Anonymous expr
      in
      choice [ assignment_stmt; label_stmt; anon_stmt ]
    )

  let program =
    wsc0 *> sep_by eol statement <* wsc0 <* end_of_input >>| fun exprs -> Ast.Block exprs
end

module Eval = struct
  let int_of_hex_res s: (int, string) result =
    match int_of_string_opt ("0x" ^ s) with
    | None -> Error (Format.sprintf "Invalid hex number: \"%s\"" s)
    | Some n -> Ok n

  let get_value v =
    match v with
    | `Numeric s -> Ok s
    | `String s -> Error (Format.sprintf "Invalid string \"%s\", expected numeric" s)

  let get_string v =
    match v with
    | `Numeric s -> Error (Format.sprintf "Invalid numeric \"%s\", expected string" s)
    | `String s -> Ok s

  let int_of_string_res s =
    match int_of_string_opt s with
    | None -> Error (Format.sprintf "Invalid decimal number: %s" s)
    | Some n -> Ok n


  (* The builtin Int32.of_string needs 0u prepended to string to parse unsigned integers bigger than 0x7fffffff *)
  let int32_of_string_res (s:string) =
    let s2 = match String.get s 0 with
    | '-' -> s
    | _ -> "0u" ^ s
    in
    match Int32.of_string_opt s2 with
      | None -> Error (Format.sprintf "Invalid 32 bit decimal number: \"%s\"" s)
      | Some n -> Ok n

  let (>>=) = CCResult.(>>=)
  let (>>|) = CCResult.(>|=)

  let string_of_chars chars =
    let buf = Buffer.create (List.length chars) in
    List.iter (Buffer.add_char buf ) chars;
    Ok (Buffer.contents buf)

  let d8 v =
    get_value v >>= int_of_string_res >>= fun n ->
    if n > 255 || n < -254
    then Error (Format.sprintf "d8: %d is out of range" n)
    else Ok (CCString.of_char @@ char_of_int (n land 0xff))

  let d16 v =
    get_value v >>= int_of_string_res >>= fun n ->
    if n > 0xffff || n <= -0xffff
    then Error (Format.sprintf "d16: %d is out of range" n)
    else Ok (
      CCString.of_list [
        (char_of_int ((n lsr 8) land 0xff));
        (char_of_int (n land 0xff))
      ]
    )

  let chars_of_int32 n =
    let (lsr) = Int32.shift_right_logical in
    let (land) = Int32.logand in
    let to_char i = char_of_int (Int32.to_int i) in
    CCString.of_list [
      to_char ((n lsr 24) land 0xffl);
      to_char ((n lsr 16) land 0xffl);
      to_char ((n lsr 8) land 0xffl);
      to_char (n land 0xffl)
    ]

  let d32 v =
    get_value v >>= int32_of_string_res >>= fun n ->
    Ok (chars_of_int32 n)

  let h8 v =
    get_value v >>= int_of_hex_res >>= fun n ->
    if n > 0xff || n < 0
    then Error (Format.sprintf "h8: %x is out of range" n)
    else Ok (CCString.of_char @@ char_of_int n)

  let asc v =
    get_string v

  let rec result_map (f: 'a -> ('b, string) result) (xs: 'a list): ('b list, string) result =
    match xs with
    | [] -> Ok []
    | x :: xs ->
      f x >>= fun head ->
      result_map f xs >>= fun tail ->
      Ok (head :: tail)


  module StringMap = CCMap.Make(CCString)

  module Output = struct
    type statement =
      | Empty
      | Anonymous of expression
      | Label of (string * expression)
    and expression =
      | Plain of string
      | Block of statement list
      | Repeat of expression * int

    let override (orig:expression) (over:expression): expression =
      let rec map_of_expr_out ns map (expr_out:expression) =
        match expr_out with
        | Plain _ -> map
        | Repeat (expr, _) -> map_of_expr_out ns map expr
        | Block stmt_outs ->
            List.fold_left
              (fun map1 stmt_out -> map_of_stmt_out ns map1 stmt_out)
              map
              stmt_outs
      and map_of_stmt_out ns map (stmt_out:statement) =
        match stmt_out with
        | Label (lbl, expr_out) ->
            let expr_map = map_of_expr_out (ns ^ lbl ^ ".") map expr_out in
            let merged = StringMap.union (fun _key _v1 v2 -> Some v2) map expr_map in
            StringMap.add (ns ^ lbl) expr_out merged
        | Empty
        | Anonymous _ -> map
      in
      let out_map = map_of_expr_out "" StringMap.empty over in

      let rec override_expr ns orig =
        match orig with
        | Plain _ as p -> p
        | Repeat (expr, n) -> Repeat ((override_expr ns expr), n)
        | Block stmts -> Block (CCList.map (override_stmt ns) stmts)
      and override_stmt ns orig: statement =
        match orig with
        | Empty -> Empty
        | Anonymous _ as a -> a
        | Label (lbl, expr) -> (
          match StringMap.find_opt (ns ^ lbl) out_map with
          | Some out -> Label (lbl, out)
          | None -> Label (lbl, override_expr (ns ^ lbl ^ ".") expr)
        )
      in
      override_expr "" orig

    let plain (s:string): expression =
      Plain s

    let repeat (n:int) (output:expression): expression =
      Repeat (output, n)

    let rec to_string (output:expression): string =
      match output with
      | Plain out -> out
      | Block stmt_outs ->
        stmt_outs |> CCList.map (fun o -> stmt_to_string o)|> CCString.concat ""
      | Repeat (out, n) -> CCString.repeat (to_string out) n
    and stmt_to_string (output:statement): string =
      match output with
      | Empty -> ""
      | Anonymous expr -> to_string expr
      | Label (_, expr) -> to_string expr
  end

  module Env = struct
    type t = Output.expression StringMap.t

    let add = StringMap.add

    let empty = StringMap.empty

    let get = StringMap.find_opt

    let add_wrapped (namespace:string) (wrap:t) (env:t): t =
        StringMap.fold
        (fun key value env ->
          StringMap.add (namespace ^ "." ^ key) value env)
        wrap
        env

  end

  type expr_return = (Env.t * Output.expression, string) result
  type stmt_return = (Env.t * Output.statement, string) result

  let eval_load (name:string) (env:Env.t): expr_return =
    match Env.get name env with
    | Some output -> Ok (Env.empty, output)
    | None -> Error (Format.sprintf "Unknown identifier '%s'" name)

  (* TODO: split computation and load *)
  let eval_computation (comp:Ast.computation) (env:Env.t): expr_return =
    let evaluate f xs =
      result_map f xs >>|
      CCString.concat "" >>|
      Output.plain >>| fun output ->
      (Env.empty, output)
    in
    match comp with
    | Ast.{ identifier; parameters=[]; _ } ->
        eval_load identifier env
    | Ast.{ identifier; parameters; _ } ->
        match identifier with
        | "d8"  -> evaluate d8 parameters
        | "h8"  -> evaluate h8 parameters
        | "d16" -> evaluate d16 parameters
        | "d32" -> evaluate d32 parameters
        | "asc" -> evaluate asc parameters
        | id -> Error (Format.sprintf "Unknown identifier '%s'" id)

  let rec eval_statement (stmt:Ast.statement) (env:Env.t): stmt_return =
    match stmt with
    | Anonymous expr ->
        eval_expression expr env >>| fun (_inner_env, output) ->
        (env, Output.Anonymous output)
    | Label (label, expr) ->
        eval_expression expr env >>| fun (_inner_env, output) ->
        (env, Output.Label (label, output))
    | Assignment (key, expr) -> (
        eval_expression expr env >>| fun (inner_env, output) ->
        let new_env: Env.t = Env.add_wrapped key inner_env env |> Env.add key output in
        (new_env, Output.Empty)
    )
  and eval_expression (expr:Ast.expression) (env:Env.t): expr_return =
      match expr with
      | Ast.Computation c -> eval_computation c env
      | Block stmts -> eval_block stmts env
      | Override (original, override) ->
          eval_expression override env >>= fun (_env, out_override) ->
          eval_expression original env >>| fun (inner_env, out_orig) ->
          (inner_env, Output.override out_orig out_override)
      | Repeat (expr, n) ->
          eval_expression expr env >>| fun (inner_env, out) ->
          (inner_env, Output.Repeat (out, n))
  and eval_block (stmts:Ast.statement list) (env:Env.t): expr_return =
    CCList.fold_left
    (
      fun acc stmt ->
      acc >>= fun (env, sum_output) ->
      eval_statement stmt env >>| fun (new_env, output) ->
      (new_env, CCList.append sum_output [output])
    )
    (Ok (env, []))
    stmts
    >>| fun (env, outputs) -> (env, Output.Block outputs)

  let eval (prog:Ast.expression): (string, string) result =
    eval_expression prog Env.empty >>| fun (_, output) -> Output.to_string output
end
