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
    | Assignment of string * expression
    | Section of string option * expression
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

  let parameters =
    multi_parameter <|> single_parameter <|> (return [])

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
    (ws0 *> parameters) (* TODO: dont eat whitespace if empty list *)

  let start_block = (char '{')

  let end_block = (char '}')

  let statement =
    fix (fun stmt ->
      let block =
        (start_block *> wsc0 *> (sep_by eol stmt) <* wsc0 <* end_block)
        >>| fun exprs -> Ast.Block exprs
      in
      let comp_block =
        choice [ computation; block ]
      in
      let override =
        lift2
        (fun source override ->
          (match source with
          | Ast.Computation _ ->
              Format.eprintf "Warning: you used a simple computation on the left side of your with statement, you probably want to wrap it in { }";
          | _ -> ());
          Ast.Override (source, override)
        )
        (comp_block <* ws0 <* string "with" <* ws1)
        block
      in
      let comp_block_override =
        choice [ comp_block; override]
      in
      let repeat =
        lift2
        (fun expr mult ->
          Ast.Repeat (expr, mult))
        comp_block_override
        (ws0 *> multiplier)
      in
      let expression =
        choice [ repeat; comp_block_override ]
      in
      let section =
        lift2
        (fun label rhs -> Ast.Section (label, rhs))
        (optional_label  <* ws0)
        expression
      in
      let assignment =
        lift2
        (fun name rhs -> Ast.Assignment (name, rhs))
        (const_name <* ws0)
        expression
      in
      choice [ assignment; section ]
    )

  let program =
    wsc0 *>
    sep_by eol statement
    <* wsc0 <* end_of_input
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

    let override (orig:expression) (_over:expression): expression =
      orig (* TODO *)

    let plain (s:string): expression =
      Plain s

    let repeat (n:int) (output:expression): expression =
      Repeat (output, n)

    let rec to_string (output:expression): string =
      let s_to_string (output:statement): string =
        match output with
        | Empty -> ""
        | Anonymous expr -> to_string expr
        | Label (_, expr) -> to_string expr
      in
      match output with
      | Plain out -> out
      | Block stmt_outs ->
        stmt_outs |> CCList.map (fun o -> s_to_string o)|> CCString.concat ""
      | Repeat (out, n) -> CCString.repeat (to_string out) n

      (*
    let no_label (expr_out:expression): statement =
      {
        label=None;
        expr_out;
      }
      *)

      (*
    let label (label:string) (value:value) =
      {
        label = Some label;
        value;
      }

    let rec to_string (output:value): string =
      let s, m = match output with
      | Plain (out, m) -> out, m
      | Block (outs, m) ->
        (outs |> CCList.map (fun o -> to_string o.value)|> CCString.concat ""), m
      in
      match s with
      | "" -> ""
      | s2 -> CCString.repeat s2 m

    let of_string ?label ?(multiplier=1) (out:string): t =
      {
        label;
        value = Plain (out, multiplier);
      }

    let mk_block ?label ?(multiplier=1) (outputs:t list): t =
      {
        label;
        value = Block (outputs, multiplier)
      }

    let empty: expression =
      of_string ""
      *)
  end

  module Env = struct
    type t = Output.expression StringMap.t

    let add = StringMap.add

    let empty = StringMap.empty

    let get = StringMap.find_opt

    let add_wrapped (namespace:string) (wrap:t) (env:t): t =
        StringMap.fold
        (fun key value env -> StringMap.add (namespace ^ "." ^ key) value env)
        env
        wrap

    (*let of_output (output:Output.value): t =*)
      (*let rec of_output1 ns map (output:Output.t): t =*)
        (*match output.label, output.value with*)
        (*| Some lbl, (Output.Plain _ as p) ->*)
            (*Format.printf "add %s \n" (ns ^ lbl);*)
            (*add (ns ^ lbl) p map*)
        (*| Some lbl, (Output.Block (outs, _) as b) ->*)
            (*List.fold_left*)
              (*(fun map1 output -> of_output1 (ns ^ lbl ^ ".") map1 output)*)
              (*map*)
              (*outs*)
            (*|>*)
            (*add (ns ^ lbl) b*)
        (*| None, Plain _ -> map*)
        (*| None, Block (outs, _) ->*)
            (*(* Keep the same namespace if no label *)*)
            (*List.fold_left*)
              (*(fun map1 output -> of_output1 ns map1 output)*)
              (*map*)
              (*outs*)
      (*in*)
      (*match output with*)
      (*| Plain _ -> empty*)
      (*| Block (outs, _n) ->*)
          (*CCList.fold_left*)
          (*(fun map output -> of_output1 "" map output)*)
          (*empty*)
          (*outs*)
  end

  (* Each evaluator returns the new variables set by it and it's output *)
  (*
  type eval_return = (Env.t * Output.t, string) result

  type evaluator = Env.t -> eval_return
  type evaluator_rhs = Env.t -> (Env.t * Output.value, string) result
  *)
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
    let rec eval_expression (expr:Ast.expression) (env:Env.t): expr_return =
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
    in
    match stmt with
    | Section (None, expr) ->
        eval_expression expr env >>| fun (_inner_env, output) ->
        (env, Output.Anonymous output)
    | Section (Some label, expr) ->
        eval_expression expr env >>| fun (_inner_env, output) ->
        (env, Output.Label (label, output))
    | Assignment (key, expr) -> (
        eval_expression expr env >>| fun (inner_env, output) ->
        let new_env: Env.t = Env.add_wrapped key inner_env env |> Env.add key output in
        (new_env, Output.Empty)
    )
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

  let eval (prog:Ast.statement list): (string, string) result =
    eval_block prog Env.empty >>| fun (_, output) -> Output.to_string output
end
