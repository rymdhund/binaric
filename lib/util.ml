let ( >>= ) = CCResult.( >>= )
let ( >>| ) = CCResult.( >|= )

let int_min x y = if x < y then x else y

let int_of_hex_res s : (int, string) result =
  match int_of_string_opt ("0x" ^ s) with
  | None -> Error "Invalid hex number"
  | Some n -> Ok n


let int_of_string_res s =
  match int_of_string_opt s with
  | None -> Error "Invalid decimal number"
  | Some n -> Ok n


(* The builtin Int32.of_string needs 0u prepended to string to parse unsigned integers bigger than 0x7fffffff *)
let int32_of_string_res (s : string) =
  let s2 =
    match s.[0] with
    | '-' -> s
    | _ -> "0u" ^ s
  in
  match Int32.of_string_opt s2 with
  | None -> Error "Invalid 32 bit decimal number"
  | Some n -> Ok n


let int64_of_string_res (s : string) =
  let s2 =
    match s.[0] with
    | '-' -> s
    | _ -> "0u" ^ s
  in
  match Int64.of_string_opt s2 with
  | None -> Error "Invalid 64 bit decimal number"
  | Some n -> Ok n


let int32_of_hex_res (s : string) =
  let s2 =
    match s.[0] with
    | '-' -> s
    | _ -> "0x" ^ s
  in
  print_endline s2 ;
  match Int32.of_string_opt s2 with
  | None -> Error "Invalid 32 bit hexadecimal number"
  | Some n -> Ok n


let int64_of_hex_res (s : string) =
  let s2 =
    match s.[0] with
    | '-' -> s
    | _ -> "0x" ^ s
  in
  match Int64.of_string_opt s2 with
  | None -> Error "Invalid 64 bit hexadecimal number"
  | Some n -> Ok n


let string_of_chars chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars ;
  Ok (Buffer.contents buf)


let chars_of_int32 n =
  let ( lsr ) = Int32.shift_right_logical in
  let ( land ) = Int32.logand in
  let to_char i = char_of_int (Int32.to_int i) in
  CCString.of_list
    [ to_char ((n lsr 24) land 0xffl);
      to_char ((n lsr 16) land 0xffl);
      to_char ((n lsr 8) land 0xffl);
      to_char (n land 0xffl)
    ]


let chars_of_int64 n =
  let ( lsr ) = Int64.shift_right_logical in
  let ( land ) = Int64.logand in
  let to_char i = char_of_int (Int64.to_int i) in
  CCString.of_list
    [ to_char ((n lsr 56) land 0xffL);
      to_char ((n lsr 48) land 0xffL);
      to_char ((n lsr 40) land 0xffL);
      to_char ((n lsr 32) land 0xffL);
      to_char ((n lsr 24) land 0xffL);
      to_char ((n lsr 16) land 0xffL);
      to_char ((n lsr 8) land 0xffL);
      to_char (n land 0xffL)
    ]


let rec result_map (f : 'a -> ('b, string) result) (xs : 'a list) :
    ('b list, string) result =
  match xs with
  | [] -> Ok []
  | x :: xs ->
      f x >>= fun head -> result_map f xs >>= fun tail -> Ok (head :: tail)


let range_seq (n : int) : 'a Seq.t =
  let rec range_seq' cur () =
    if cur = n then Seq.Nil else Seq.Cons (cur, range_seq' (cur + 1))
  in
  range_seq' 0
