module Ast = Binaric.Ast
module Parsers = Binaric.Parsers
module Eval = Binaric.Eval

let statement =
  Alcotest.testable
    (fun ppf seg -> Format.fprintf ppf "%s" (Ast.show_statement seg))
    (Ast.equal_statement)

let binary_string = Alcotest.testable (fun fmt -> Format.fprintf fmt "%S") (=)

let check_parse_expr expected s () =
  match Angstrom.parse_string Parsers.statement s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.check statement "same statement" expected seg

let check_parse exp s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.(check (list statement)) "same program" exp seg

let mk_statement ?label identifier parameters =
  let mk_computation identifier parameters =
    Ast.{ identifier; parameters }
  in
  Ast.Section (label, Computation (mk_computation identifier parameters))

let statement_tests = List.map
  (fun (exp, s) -> ("parse statement", `Quick, check_parse_expr exp s))
  [
    (
      mk_statement "abc" [],
      "abc"
    );
    (
      mk_statement ~label:"foo" "abc" [],
      "foo: abc"
    );
    (
      mk_statement ~label:"f-o_o" "a-b_c" [],
      "f-o_o: a-b_c"
    );
    (
      mk_statement ~label:"a" "b" [`Numeric "c"],
      "a: b c"
    );
    (
      mk_statement "b" [],
      "b [ ]"
    );
    (
      mk_statement ~label:"a" "b" [`Numeric "c"],
      "a: b [ c ]"
    );
    (
      mk_statement ~label:"a" "b" [`Numeric "c"; `Numeric "d"],
      "a: b [ c d ]"
    );
  ]

let program_tests = List.map
  (fun (exp, s) -> ("parse program", `Quick, check_parse exp s))
  [
    (
      [
        mk_statement "a" [];
        mk_statement "b" [];
      ],
      "a\nb"
    );
    (
      [
        mk_statement "a" [];
        mk_statement "b" [];
      ],
      "a # comment\nb"
    );
    (
      [
        mk_statement "a" [];
      ],
      "# comment\na"
    );
    (
      [
        mk_statement "a" [];
      ],
      "\na"
    );
    (
      [
        mk_statement "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b c ]"
    );
    (
      [
        mk_statement "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b #comment\n c ]"
    );
    (
      [
        mk_statement "a" [ `Numeric "b"; `Numeric "c" ];
      ],
      "a [ b #comment\n c ] \n "
    );
    (
      [
        mk_statement "a" [ `String "abc" ];
      ],
      "a \"abc\" "
    );
    (
      [
        mk_statement "a" [ `String "\"" ];
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

let eval_oneliners_tests = List.map
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


let eval_multiliners_tests = List.map
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
    ("", "{}");
    ("\xff\xff", {|
    h8 ff
    {}
    {}
    h8 ff
    {}
    |});
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

let eval_repetition_tests = List.map
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
      const a = h8 ff * 2
      a
      |}
    );
    (
      "\xff\xff", {|
      const a = h8 ff
      a * 2
      |}
    );
  ]

let eval_const_tests = List.map
  (fun (exp, s) -> ("eval const", `Quick, check_eval exp s))
  [
    ("", "const abc=h8 ff");
    (
      "\xff\xff", {|
      const foo = h8  ff
      foo
      foo
      |}
    );
    (
      "\xff\xff", {|
      const bar = {
        h8  ff
        h8  ff
      }
      bar
      |}
    );
    (
      "\xff\x00\xff", {|
      const x = {
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
      const x = h8 00
      {
        const x = h8 ff
        x
      }
      x
      |}
    );
    (
      "\xff", {|
      const x = {
        const y = h8 ff
      }
      x.y
      |}
    );
    (
      "\xff\x00", {|
      const y = h8 00
      const x = {
        const y = h8 ff
      }
      x.y
      y
      |}
    );
  ]

let eval_override_tests = List.map
  (fun (exp, s) -> ("eval const", `Quick, check_eval exp s))
  [
    ("", "{} with {}");
    ("\xff", {|
    h8 ff with { }
    |});
    ("\xff", {|
    h8 ff with {
      h8 00
    }
    |});
    (* with is only evaluated on the rhs *)
    ("\xff", {|
    a: h8 ff with {
      a: h8 00
    }
    |});
    ("\x00\xff", {|
    {
      a: h8 ff
      b: h8 aa
    } with {
      a: h8 00
      b: h8 ff
    }
    |});
    ("\xff\xbb", {|
    const a = {
      a: h8 ff
      b: h8 aa
    }
    a with {
      b: h8 bb
    }
    |});

    (* Anonymous blocks are impossible to override *)
    ("\xff\xaa", {|
    {
      a: h8 ff
      {
        b: h8 aa
      }
    } with {
      b: h8 bb
    }
    |});
  ]

let check_eval_fail expected s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.(check string) "same error" expected msg
  | Ok prog ->
    match Eval.eval prog with
    | Error msg -> Alcotest.(check string) "same error" expected msg
    | Ok _ -> Alcotest.fail "Expected to fail"

let eval_fail_tests = List.map
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
    (
      "Unknown identifier 'bar'", {|
      bar: h8  ff
           bar
      |}
    );
  ]

let () =
  Alcotest.run "Binaric Tests" [
    "statement tests", statement_tests;
    "program_tests", program_tests;
    "eval_oneliners_tests", eval_oneliners_tests;
    "eval_multiliners_tests", eval_multiliners_tests;
    "eval_repetition_tests", eval_repetition_tests;
    "eval_const_tests", eval_const_tests;
    "eval_override_tests", eval_override_tests;
    "eval_fail_tests", eval_fail_tests;
  ]
