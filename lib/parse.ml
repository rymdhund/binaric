open Angstrom

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false


let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false


let is_name_char = function
  | '_' | '-' | '.' -> true
  | c -> is_alpha c || is_digit c


let is_param_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false


let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars ;
  Buffer.contents buf


let is_special_string_char = function
  | '"' | '\\' -> true
  | _ -> false


let is_regular_string_char c = not (is_special_string_char c)

let escaped = char '\\' *> satisfy is_special_string_char

let regular_string_char = satisfy is_regular_string_char

let quoted_char = escaped <|> regular_string_char

let quoted_string =
  char '"' *> many quoted_char
  <* char '"'
  >>| fun chars -> `String (string_of_chars chars)


let comment =
  char '#'
  <* skip_while (function
         | '\x0a' | '\x0d' -> false
         | _ -> true )
  <* advance 1
  >>| ignore


let is_ws = function
  | '\x20' | '\x09' -> true
  | _ -> false


(* zero or more whitespace *)
let ws0 = skip_while is_ws

(* one or more whitespace *)
let ws1 =
  take_while1 (function
      | '\x20' | '\x09' -> true
      | _ -> false )
  >>| ignore


(* exactly one newline *)
let nl =
  skip (function
      | '\x0a' | '\x0d' -> true
      | _ -> false )


(* zero or more whitespace or newline characters *)
let wsnl0 =
  skip_while (function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false )


(* zero or more whitespace, comments or newlines *)
let wsc0 = wsnl0 <* sep_by wsnl0 comment <* wsnl0

(* require a line ending *)
let eol = ws0 <* (comment <|> nl <|> end_of_input) <* wsc0

let name =
  peek_char
  >>= function
  | None -> fail "EOF, expected name"
  | Some c ->
      if is_alpha c
      then take_while1 is_name_char
      else fail (Format.sprintf "Invalid start of name: '%C'" c)


let label = name <* ws0 <* char ':'

let value =
  lift2
    (fun sign v -> `Numeric (sign ^ v))
    (option "" (char '-' >>| fun _ -> "-"))
    (take_while1 is_param_char)
  >>= fun v ->
  match v with
  | `Numeric "with" -> fail "Unexpected keyword 'with'"
  | _ as n -> return n


let param = value <|> quoted_string

let single_parameter = param >>| fun x -> [ x ]

let multi_parameter = char '[' *> wsc0 *> sep_by wsc0 param <* wsc0 <* char ']'

let parameters1 = multi_parameter <|> single_parameter

let natural_number = take_while1 is_digit >>| int_of_string

let multiplier = string "**" *> ws0 *> natural_number

let var_name = string "let" *> ws0 *> name <* ws0 <* char '='

let end_of_input =
  peek_char
  >>= function
  | Some c -> fail (Format.sprintf "Unexpected character: %C" c)
  | None -> return ()


let debug msg = return () >>| fun _ -> Format.printf msg

let computation =
  lift2
    (fun identifier parameters -> Ast.Computation { identifier; parameters })
    name
    (ws0 *> parameters1 <|> return [])


let import =
  string "import" *> ws1 *> quoted_string
  >>| fun (`String file : [> `String of string ]) -> Ast.Import file


(** parse something like: [1..2] *)
let range =
  let some x = Some x in
  let opt_num = option None (natural_number >>| some) in
  lift2
    (fun x y -> (x, y))
    (string "[" *> ws0 *> opt_num)
    (ws0 *> string ".." *> ws0 *> opt_num <* ws0 <* string "]")


let import_raw =
  let opt_range = option (None, None) range in
  lift2
    (fun (`String file : [> `String of string ]) (start, end_) ->
      Ast.ImportRaw (file, start, end_) )
    (string "import.raw" *> ws1 *> quoted_string)
    (ws0 *> opt_range)


let start_block = char '{'

let end_block = char '}'

let statement =
  fix (fun stmt ->
      let block =
        start_block *> wsc0 *> sep_by eol stmt
        <* wsc0
        <* end_block
        >>| fun exprs -> Ast.Block exprs
      in
      let term = choice [ import; import_raw; computation; block ] in
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
          (var_name <* ws0)
          expression
      in
      let label_stmt =
        lift2
          (fun label expr -> Ast.Label (label, expr))
          (label <* ws0)
          expression
      in
      let anon_stmt = expression >>| fun expr -> Ast.Anonymous expr in
      choice [ assignment_stmt; label_stmt; anon_stmt ] )


let program =
  wsc0 *> sep_by eol statement
  <* wsc0
  <* end_of_input
  >>| fun exprs -> Ast.Block exprs
