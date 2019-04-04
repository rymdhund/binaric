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
    multiplier: int;
  } [@@ deriving show, eq]

  type expression =
    | Assignment of string * rhs
    | Section of string option * rhs
  [@@ deriving show, eq]

  and rhs =
    | Computation of computation
    | Block of expression list
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
    char '#' <* skip_while (function | '\x0a' | '\x0d' -> false | _ -> true) <* advance 1 >>| ignore

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

  let rhs_computation =
    lift3
    (
      fun identifier parameters multiplier ->
      Ast.Computation {identifier; parameters; multiplier}
    )
    (name <* ws0)
    (parameters <* ws0)
    optional_multiplier

  let start_block = (char '{')

  let end_block = (char '}')

  let expression =
    fix (fun expr ->
      let rhs_block =
        (start_block *> wsc0 *> (sep_by eol expr) <* wsc0 <* end_block)
        >>| fun exprs -> Ast.Block exprs
      in
      let rhs =
        choice [ rhs_computation; rhs_block ]
      in
      let section =
        lift2
        (fun label rhs -> Ast.Section (label, rhs))
        (optional_label  <* ws0)
        rhs
      in
      let assignment =
        lift2
        (fun name rhs -> Ast.Assignment (name, rhs))
        (const_name <* ws0)
        rhs
      in
      choice [ assignment; section ]
    )

  let program =
    wsc0 *>
    sep_by eol expression
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

  module Env = struct
    type t = string StringMap.t

    let with_const (name:string) (value:string) t =
      StringMap.add name value t

    let empty = StringMap.empty

    let nest_names namespace t =
      Format.printf "nest len %d\n" (StringMap.cardinal t);
      StringMap.fold (fun key value env ->
        Format.printf "Nesting name %s %S\n" (namespace ^ "." ^ key) value;
        StringMap.add (namespace ^ "." ^ key) value env) t empty

    let merge t1 t2 =
      StringMap.union (fun _key _v1 v2 -> Some v2) t1 t2

    (* TODO: remove *)
    let print t =
      let sprinter = fun fmt -> Format.fprintf fmt "%S" in
      let pp = StringMap.pp sprinter sprinter in
      Format.fprintf Format.err_formatter "{\n";
      pp Format.err_formatter t;
      Format.fprintf Format.err_formatter "\n}\n";
  end

  (* Each evaluator returns the new variables set by it and it's output *)
  type eval_return = (Env.t * string, string) result

  type evaluator = Env.t -> eval_return

  let echo (data:string): evaluator =
    fun _env -> Ok (Env.empty, data)

  let fail (msg:string): evaluator =
    fun _env -> Error msg

  let output_const ?(multiplier=1) (key:string): evaluator =
    fun env ->
      match StringMap.find_opt key env with
      | Some value ->
          Ok (Env.empty, CCString.repeat value multiplier)
      | None -> Error (Format.sprintf "Unknown identifier '%s'" key)

  let only_echo (evaluator:evaluator): evaluator =
    fun env ->
      evaluator env >>| fun (_env, output) -> (Env.empty, output)

  let set_and_echo key (evaluator:evaluator): evaluator =
    fun env ->
      evaluator env >>| fun (env1, output) ->
      let e = Env.nest_names key env1 |> Env.with_const key output in
      (e, output)

  let set_and_swallow key (evaluator:evaluator): evaluator =
    fun env ->
      evaluator env >>| fun (env1, output) ->
      let e = Env.nest_names key env1 |> Env.with_const key output in
      (e, "")

  let chain (es:evaluator list): evaluator =
    fun env ->
      CCList.fold_left
      (
        fun acc evaluator ->
        acc >>= fun (env, sum_output) ->
        evaluator env >>| fun (new_env, output) ->
        (Env.merge env new_env, sum_output ^ output)
      ) (Ok (env, "")) es

  let eval_computation (comp:Ast.computation): evaluator =
    let mk_evaluator f xs =
      match result_map f xs >>| CCString.concat "" with
      | Ok output -> echo (CCString.repeat output comp.multiplier)
      | Error e -> fail e
    in
    match comp with
    | Ast.{ identifier; parameters=[]; _ } ->
        output_const ~multiplier:comp.multiplier identifier
    | Ast.{ identifier; parameters; _ } ->
        match identifier with
        | "d8"  -> mk_evaluator d8 parameters
        | "h8"  -> mk_evaluator h8 parameters
        | "d16" -> mk_evaluator d16 parameters
        | "d32" -> mk_evaluator d32 parameters
        | "asc" -> mk_evaluator asc parameters
        | id -> fail (Format.sprintf "Unknown identifier '%s'" id)

  let rec eval_expression (expr:Ast.expression): evaluator =
    let rhs_evaluator = function
    | Ast.Computation c -> eval_computation c
    | Block exprs -> eval_expressions exprs
    in
    match expr with
    | Section (_label, rhs) ->
        only_echo (rhs_evaluator rhs)
    | Assignment (key, rhs) ->
        set_and_swallow key (rhs_evaluator rhs)
  and eval_expressions (exprs:Ast.expression list): evaluator =
    chain (CCList.map eval_expression exprs)

  let eval (prog:Ast.expression list): (string, string) result =
    eval_expressions prog Env.empty >>| fun (_, output) -> output
end
