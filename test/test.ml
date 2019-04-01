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


let check_parse_expr expected s () =
  match Angstrom.parse_string Parsers.expression s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.check element "same segment" expected seg

let check_parse exp s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.(check (list element)) "same program" exp seg

let mk_computation multiplier identifier parameters =
  Ast.Computation.{ multiplier; identifier; parameters }

let mk_expression ?(is_const=false) ?label ?(multiplier=1) identifier parameters =
  Ast.Expression.{
    is_const;
    label;
    expr = Computation (mk_computation multiplier identifier parameters)
  }

let expression_set = List.map
  (fun (exp, s) -> ("parse expression", `Quick, check_parse_expr exp s))
  [
    (
      mk_expression "abc" [],
      "abc"
    );
    (
      mk_expression ~label:"foo" "abc" [],
      "foo: abc"
    );
    (
      mk_expression ~label:"f-o_o" "a-b_c" [],
      "f-o_o: a-b_c"
    );
    (
      mk_expression ~label:"a" "b" [`Numeric "c"],
      "a: b c"
    );
    (
      mk_expression "b" [],
      "b [ ]"
    );
    (
      mk_expression ~label:"a" "b" [`Numeric "c"],
      "a: b [ c ]"
    );
    (
      mk_expression ~label:"a" "b" [`Numeric "c"; `Numeric "d"],
      "a: b [ c d ]"
    );
    (
      mk_expression ~multiplier:10 "b" [],
      "b * 10"
    );
    (
      mk_expression ~label:"a" ~multiplier:2 "b" [`Numeric "c"],
      "a: b c * 2"
    );
  ]

let program_set = List.map
  (fun (exp, s) -> ("parse program", `Quick, check_parse exp s))
  [
    (
      [
        mk_expression "a" [];
        mk_expression "b" [];
      ],
      "a\nb"
    );
    (
      [
        mk_expression "a" [];
        mk_expression "b" [];
      ],
      "a # comment\nb"
    );
    (
      [
        mk_expression "a" [];
      ],
      "# comment\na"
    );
    (
      [
        mk_expression "a" [];
      ],
      "\na"
    );
    (
      [
        mk_expression "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b c ]"
    );
    (
      [
        mk_expression "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b #comment\n c ]"
    );
    (
      [
        mk_expression "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b #comment\n c ] \n "
    );
    (
      [
        mk_expression "a" [ `String "abc" ];
      ],
      "a \"abc\" "
    );
    (
      [
        mk_expression "a" [ `String "\"" ];
      ],
      "a  \"\\\"\" "
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
    ("\x00", "d8  0 ");
    ("\xff", "d8  255 ");
    ("\xff", "d8  -1 ");
    ("\x00\xff", "d8 [ 0 255 ] ");
    ("\x00\xff", "h8 [ 0 ff ] ");
    ("\x00\x01", "d16 [ 1 ] ");
    ("\x00\x01", "d16 [ 1 ] ");
    ("\x00\x01\x00\xff", "d16 [ 1 255 ] ");
    ("\x00\x00\x00\x01", "d32  1 ");
    ("\xff\xff\xff\xff", "d32  4294967295 ");
    ("\xff\xff\xff\xff", "d32  -1 ");
    ("\x00", "{d8  0}");
    ("\x00", "{ d8  0 }");
    ("\x00", "{\nd8  0\n}");
    ("\x00", "{ \n d8  0 \n }");
    ("\x00", "a:d8 0");
  ]


let eval_multiliners_set = List.map
  (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
  [
    (
      "\x00\xff", {|
      d8 0
      d8 255|}
    );
    (
      "\x00\xff\xff", {|
      h8 [
          00
          ff
         ]
      h8  ff
      |}
    );
    (
      "\x00\xff\xf0", {|
      h8  00 #comment1
      h8  ff #comment2
      h8  f0 #comment3"
      |}
    );
    (
      "\015\002\254\x00\xff\x00\xff", {|
      size:   d8  15
      width:  d8  2
      height: d8  -2
      data:   {
              h8 [ 00 ff 00 ff ]
      }
      |}
    );
    (
      "\015\002\254\x00\xff\x00\xff", {|
      size:   d8   15
      width:  d8   2
      height: d8   -2
      data:   h8 [ 00 ff 00 ff ]
      |}
    );
    (
      "\xff", {|
      a: {
        h8   ff
      }
      |}
    );
    (
      "\xff\xff", {|
      a: {
        h8   ff
        h8   ff
      }
      |}
    );
    (
      "", {|
      a:{
      b:{
      }
      }
      |}
    );
    (
      "\001\002\003", {|
      a: { #comment
        one:     d8   1
        b: {
          two:   d8   2
          three: d8   3
        }
      }
      |}
    );
    (
      "\001\002\003", {|
      one:   d8  1
      # comment
      # comment
      two:   d8  2
      three: d8  3
      |}
    );
    (
      "abcdef", {|
      asc [
       "abc"
       "def"
      ]
      |}
    );
  ]

let eval_repetition_set = List.map
  (fun (exp, s) -> ("eval repetition", `Quick, check_eval exp s))
  [
    (
      "\xff\xff\xff", {|
      h8 ff * 3
      |}
    );
    (
      "\xff\xff\xff", {|
      a: h8 ff * 3
      |}
    );
    (
      "\xff\xff", {|
      const a: h8 ff * 2
      a
      |}
    );
    (
      "\xff\xff", {|
      const a: h8 ff
      a * 2
      |}
    );
  ]

let eval_const_set = List.map
  (fun (exp, s) -> ("eval const", `Quick, check_eval exp s))
  [
    (
      "\xff\xff", {|
      const foo: h8  ff
      foo
      foo
      |}
    );
    (
      "\xff\xff", {|
      bar: h8  ff
           bar
      |}
    );
    (
      "\xff\xff", {|
      const bar: {
        h8  ff
        h8  ff
      }
      bar
      |}
    );
    (
      "\xff\x00\xff", {|
      const x: {
        h8  ff
        foo: {
          h8 00
        }
        h8  ff
      }
      x
      |}
    );
    (
      "\xff\x00", {|
      const x: h8 00
      {
        const x: h8 ff
        x
      }
      x
      |}
    );
    (
      "\xff", {|
      const x: {
        const y: h8 ff
      }
      x.y
      |}
    );
    (
      "\xff\x00", {|
      const y: h8 00
      const x: {
        const y: h8 ff
      }
      x.y
      y
      |}
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
    ("d8: 256 is out of range", "d8  256 ");
    ("d8: -255 is out of range", "d8  -255 ");
    (": Unexpected character: '-'", "d8  1-1 ");
    ("Invalid decimal number: 0f", "d8  0f ");
    ("h8: 100 is out of range", "h8  100 ");
    ("Invalid hex number: \"-1\"", "h8  -1 ");
    ("Invalid 32 bit decimal number: \"4294967296\"", "d32  4294967296");
    ("Invalid 32 bit decimal number: \"-4294967295\"", "d32  -4294967295");
  ]

let () =
  Alcotest.run "Binaric Tests" [
    "expression set", expression_set;
    "program_set", program_set;
    "eval_oneliners_set", eval_oneliners_set;
    "eval_multiliners_set", eval_multiliners_set;
    "eval_repetition_set", eval_repetition_set;
    "eval_const_set", eval_const_set;
    "eval_fail_set", eval_fail_set;
  ]
