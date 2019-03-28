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
  type value = [ `Value of string | `String of string ] [@@ deriving show]

  module Segment = struct
    type t = {
      label: string option;
      identifier: string;
      parameters: value list;
      multiplier: int option;
    } [@@ deriving show]
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

  let ws = skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false
  )

  let optional_comment =
    ws <* option () comment

  let ws' =
    optional_comment <* ws

  let name =
    ws *>
    peek_char >>= function
      | None -> fail "EOF, expected name"
      | Some c ->
        if is_alpha c
        then take_while1 is_name_char
        else fail (Format.sprintf "Invalid start of name: '%c'" c)

  let label =
    ws *> name <* ws <* (char ':') <* ws

  let optional_label =
    let some x = Some x in
    option None (label >>| some)

  let value =
    lift2 (fun sign v -> `Value (sign ^ v))
      (option "" (char '-' >>| (fun _ -> "-")))
      (take_while1 is_param_char)

  let param =
    value <|> quoted_string

  let single_parameter =
    ws *> (char '=') *> ws *> param >>| (fun x -> [x])

  let multi_parameter =
    ws *> (char '[') *> (many (ws' *> param)) <* ws' <* (char ']')

  let parameters =
    single_parameter <|> multi_parameter <|> (return [])

  let multiplier =
    ws *> (char '*') *> ws *> (take_while1 is_digit) >>| int_of_string

  let optional_multiplier =
    let some x = Some x in
    option None (multiplier >>| some)

  let end_of_input =
    peek_char >>= function
    | Some c -> fail (Format.sprintf "Unexpected character: '%c'" c)
    | None -> return ()

  let segment =
    lift4
    (
      fun label identifier parameters multiplier ->
      Ast.Segment.{label; identifier; parameters; multiplier}
    )
    optional_label
    name
    parameters
    optional_multiplier
    <* optional_comment

  let rec somefilter xs = match xs with
    | [] -> []
    | Some x :: tail -> x :: somefilter tail
    | None :: tail -> somefilter tail

  let program =
    let some x = Some x in
    let none _ = None in
    many (
      (segment >>| some)
      <|>
      (comment >>| none)
    ) <* end_of_input >>| somefilter
end

module Eval = struct

  let int_of_hex s =
    try
      Scanf.sscanf s "%x%!" (fun x -> Some x)
    with Scanf.Scan_failure _ ->
      None

  let d8 v =
    match v with
    | `Value s -> (
        match int_of_string_opt s with
        | None -> Error (Format.sprintf "Invalid decimal number: %s" s)
        | Some n when n > 255 || n < -254 -> Error (Format.sprintf "d8: %d is out of range" n)
        | Some n -> Ok [char_of_int (n land 0xff)]
    )
    | `String s -> Error (Format.sprintf "d8 can not handle strings. It is being applied to \"%s\"" s)

  let d16 v =
    match v with
    | `Value s -> (
        match int_of_string_opt s with
        | None -> Error (Format.sprintf "Invalid decimal number: %s" s)
        | Some n when n > 0xffff || n <= -0xffff -> Error (Format.sprintf "d16: %d is out of range" n)
        | Some n -> Ok [(char_of_int ((n lsr 8) land 0xff)); (char_of_int (n land 0xff))]
    )
    | `String s -> Error (Format.sprintf "d8 can not handle strings. It is being applied to \"%s\"" s)

  let h8 v =
    match v with
    | `Value s -> (
        match int_of_hex s with
        | None -> Error (Format.sprintf "Invalid hex number: %s" s)
        | Some n when n > 0xff || n < 0 -> Error (Format.sprintf "h8: %s is out of range" s)
        | Some n -> Ok [char_of_int n]
    )
    | `String s -> Error (Format.sprintf "h8 can not handle strings. It is being applied to \"%s\"" s)

  let rec result_map f xs =
    match xs with
    | [] -> Ok []
    | x :: xs -> (
      match f x, result_map f xs with
      | Error s, _ -> Error s
      | Ok _, Error s -> Error s
      | Ok cs, Ok res -> Ok (List.append cs res)
    )

  let eval_seg seg =
    match seg with
    | Ast.Segment.{ identifier; parameters=[]; _ } -> Error (Format.sprintf "Unknown identifier \"%s\"" identifier)
    | Ast.Segment.{ identifier; parameters; _ } ->
        match identifier with
        | "d8" -> result_map d8 parameters
        | "h8" -> result_map h8 parameters
        | "d16" -> result_map d16 parameters
        | id -> Error (Format.sprintf "Unknown identifier '%s'" id)

  let eval prog =
    match prog with
    | [] -> Ok []
    | seg :: _ -> eval_seg seg
end
