open Angstrom

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alpha = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_name_char = function
  | '_' | '-' -> true
  | c -> (is_alpha c) || (is_digit c)

let is_param_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | _ -> false

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

module Ast = struct
  type value = [ `Numeric of string | `String of string ] [@@ deriving show, eq]

  module Computation = struct
    type t = {
      identifier: string;
      parameters: value list;
      multiplier: int option;
    } [@@ deriving show, eq]
  end

  module Expression = struct
    type t = {
       label: string option;
       expr: unlabeled
    } [@@ deriving show, eq]
    and unlabeled =
      | Computation of Computation.t
      | Block of t list
    [@@ deriving show, eq]
  end
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

  (* zero or more whitespace *)
  let ws0 = skip_while (function
    | '\x20' | '\x09' -> true
    | _ -> false
  )

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
    let some x = Some x in
    option None (multiplier >>| some)

  let end_of_input =
    peek_char >>= function
    | Some c -> fail (Format.sprintf "Unexpected character: %C" c)
    | None -> return ()

  let computation =
    lift3
    (
      fun identifier parameters multiplier ->
      Ast.Computation.{identifier; parameters; multiplier}
    )
    (name <* ws0)
    (parameters <* ws0)
    optional_multiplier

  let rec somefilter xs = match xs with
    | [] -> []
    | Some x :: tail -> x :: somefilter tail
    | None :: tail -> somefilter tail

  let comp_expr =
    lift2
      (fun label comp -> Ast.Expression.{ label; expr = Computation comp})
      (optional_label <* ws0)
      computation

  let start_block = (char '{')

  let end_block = (char '}')

  let expression =
    fix (fun expr ->
      let unlabeled_block =
        start_block *> wsc0 *> (sep_by eol expr) <* wsc0 <* end_block
      in
      let block =
        lift2
        (fun label exprs -> Ast.Expression.{ label; expr = Block exprs })
        (optional_label  <* ws0)
        unlabeled_block
      in
      choice [ comp_expr; block ]
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

  let eval_computation (comp:Ast.Computation.t) =
    let result_map_to_string f xs =
      result_map f xs >>| CCString.concat ""
    in
    match comp with
    | Ast.Computation.{ identifier; parameters=[]; _ } -> Error (Format.sprintf "Unknown identifier \"%s\"" identifier)
    | Ast.Computation.{ identifier; parameters; _ } ->
        match identifier with
        | "d8" -> result_map_to_string d8 parameters
        | "h8" -> result_map_to_string h8 parameters
        | "d16" -> result_map_to_string d16 parameters
        | "d32" -> result_map_to_string d32 parameters
        | "asc" -> result_map_to_string asc parameters
        | id -> Error (Format.sprintf "Unknown identifier '%s'" id)

  let rec eval_expression (expr:Ast.Expression.t) =
    match expr with
    | { expr=Computation c; _} -> eval_computation c
    | { expr=Block exprs; _} ->
        result_map eval_expression exprs >>| CCString.concat ""

  let eval prog: (string, string) result =
    result_map eval_expression prog >>| CCString.concat ""
end
