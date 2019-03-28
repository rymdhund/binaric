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
  type value = [ `Numeric of string | `String of string ] [@@ deriving show]

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
    lift2 (fun sign v -> `Numeric (sign ^ v))
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
  let int_of_hex_res s: (int, string) result =
    match int_of_string_opt ("0x" ^ s) with
    | None -> Error (Format.sprintf "Invalid hex number: \"%s\"" s)
    | Some n -> Ok n

  let get_value v =
    match v with
    | `Numeric s -> Ok s
    | `String s -> Error (Format.sprintf "Invalid string \"%s\", expected numeric" s)

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

  let result_bind res f =
    match res with
    | Ok x -> f x
    | Error _ as e -> e

  let (>>=) = result_bind


  let d8 v =
    get_value v >>= int_of_string_res >>= fun n ->
    if n > 255 || n < -254
    then Error (Format.sprintf "d8: %d is out of range" n)
    else Ok [char_of_int (n land 0xff)]

  let d16 v =
    get_value v >>= int_of_string_res >>= fun n ->
    if n > 0xffff || n <= -0xffff
    then Error (Format.sprintf "d16: %d is out of range" n)
    else Ok [(char_of_int ((n lsr 8) land 0xff)); (char_of_int (n land 0xff))]

  let chars_of_int32 n =
    let (lsr) = Int32.shift_right_logical in
    let (land) = Int32.logand in
    let to_char i = char_of_int (Int32.to_int i) in
    [
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
    else Ok [char_of_int n]

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
        | "d32" -> result_map d32 parameters
        | id -> Error (Format.sprintf "Unknown identifier '%s'" id)

  let eval prog =
    match prog with
    | [] -> Ok []
    | seg :: _ -> eval_seg seg
end
