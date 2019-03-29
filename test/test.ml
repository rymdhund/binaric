module Ast = Binaric.Ast
module Parsers = Binaric.Parsers
module Eval = Binaric.Eval

let segment_eq (a:Ast.Computation.t) (b:Ast.Computation.t) =
  a.identifier = b.identifier &&
  a.parameters = b.parameters &&
  a.multiplier = b.multiplier

let segment = Alcotest.testable (fun ppf seg -> Format.fprintf ppf "%s" (Ast.Computation.show seg)) segment_eq
let element = Alcotest.testable (fun ppf seg -> Format.fprintf ppf "%s" (Ast.Expression.show seg)) (Ast.Expression.equal)
let binary_string = Alcotest.testable (fun fmt -> Format.fprintf fmt "%S") (=)


let check_parse_element exp s () =
  match Angstrom.parse_string Parsers.element s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.check element "same segment" exp seg

let check_parse exp s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.(check (list element)) "same program" exp seg

let mk_segment multiplier identifier parameters =
  Ast.Computation.{ multiplier; identifier; parameters }

let mk_element ?label ?multiplier identifier parameters =
  Ast.Expression.{ label; expr = Computation (mk_segment multiplier identifier parameters) }

let segment_set = List.map
  (fun (exp, s) -> ("parse segment", `Quick, check_parse_element exp s))
  [
    (
      mk_element "abc" [],
      "abc"
    );
    (
      mk_element ~label:"foo" "abc" [],
      "foo: abc"
    );
    (
      mk_element ~label:"f-o_o" "a-b_c" [],
      "f-o_o: a-b_c"
    );
    (
      mk_element ~label:"a" "b" [`Numeric "c"],
      "a: b = c"
    );
    (
      mk_element "b" [],
      "b [ ]"
    );
    (
      mk_element ~label:"a" "b" [`Numeric "c"],
      "a: b [ c ]"
    );
    (
      mk_element ~label:"a" "b" [`Numeric "c"; `Numeric "d"],
      "a: b [ c d ]"
    );
    (
      mk_element ~multiplier:10 "b" [],
      "b * 10"
    );
    (
      mk_element ~label:"a" ~multiplier:2 "b" [`Numeric "c"],
      "a: b = c * 2"
    );
  ]

let program_set = List.map
  (fun (exp, s) -> ("parse program", `Quick, check_parse exp s))
  [
    (
      [
        mk_element "a" [];
        mk_element "b" [];
      ],
      "a\nb"
    );
    (
      [
        mk_element "a" [];
        mk_element "b" [];
      ],
      "a # comment\nb"
    );
    (
      [
        mk_element "a" [];
      ],
      "# comment\na"
    );
    (
      [
        mk_element "a" [];
      ],
      "\na"
    );
    (
      [
        mk_element "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b c ]"
    );
    (
      [
        mk_element "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b #comment\n c ]"
    );
    (
      [
        mk_element "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b #comment\n c ] \n "
    );
    (
      [
        mk_element "a" [ `String "abc" ];
      ],
      "a = \"abc\" "
    );
    (
      [
        mk_element "a" [ `String "\"" ];
      ],
      "a = \"\\\"\" "
    );
  ]

let check_eval exp s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match Eval.eval prog with
    | Error msg -> Alcotest.fail msg
    | Ok res -> Alcotest.check binary_string "same evaluation" exp res

let eval_oneliners_set = List.map
  (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
  [
    ("\x00", "d8 = 0 ");
    ("\xff", "d8 = 255 ");
    ("\xff", "d8 = -1 ");
    ("\x00\xff", "d8 [ 0 255 ] ");
    ("\x00\xff", "h8 [ 0 ff ] ");
    ("\x00\x01", "d16 [ 1 ] ");
    ("\x00\x01", "d16 [ 1 ] ");
    ("\x00\x01\x00\xff", "d16 [ 1 255 ] ");
    ("\x00\x00\x00\x01", "d32 = 1 ");
    ("\xff\xff\xff\xff", "d32 = 4294967295 ");
    ("\xff\xff\xff\xff", "d32 = -1 ");
    ("\x00", "{ d8 = 0 }");
  ]


let eval_multiliners_set = List.map
  (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
  [
    (
      "\x00\xff",
      "d8 = 0\n" ^
      "d8 = 255"
    );
    (
      "\x00\xff\xf0",
      "h8 = 00 #comment\n" ^
      "h8 = ff #comment2\n" ^
      "h8 = f0 #comment3"
    );
    (
      "\015\002\254\x00\xff\x00\xff",
      "size:   d8 = 15\n" ^
      "width:  d8 = 2\n" ^
      "height: d8 = -2\n" ^
      "data:   h8 [ 00 ff 00 ff ]"
    );
    (
      "\015\002\254\x00\xff\x00\xff",
      "size:   d8 = 15\n" ^
      "width:  d8 = 2\n" ^
      "height: d8 = -2\n" ^
      "data:   h8 [ 00 ff 00 ff ]"
    );
  ]

let check_eval_fail expected s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.(check string) "same error" expected msg
  | Ok prog ->
    match Eval.eval prog with
    | Error msg -> Alcotest.(check string) "same error" expected msg
    | Ok _ -> Alcotest.fail "Expected to fail"

let eval_fail_set = List.map
  (fun (exp, s) -> ("eval fail program", `Quick, check_eval_fail exp s))
  [
    ("d8: 256 is out of range", "d8 = 256 ");
    ("d8: -255 is out of range", "d8 = -255 ");
    (": Unexpected character: '-'", "d8 = 1-1 ");
    ("Invalid decimal number: 0f", "d8 = 0f ");
    ("h8: 100 is out of range", "h8 = 100 ");
    ("Invalid hex number: \"-1\"", "h8 = -1 ");
    ("Invalid 32 bit decimal number: \"4294967296\"", "d32 = 4294967296");
    ("Invalid 32 bit decimal number: \"-4294967295\"", "d32 = -4294967295");
  ]

let () =
  Alcotest.run "Binaric Tests" [
    "segment_set", segment_set;
    "program_set", program_set;
    "eval_oneliners_set", eval_oneliners_set;
    "eval_multiliners_set", eval_multiliners_set;
    "eval_fail_set", eval_fail_set;
  ]
