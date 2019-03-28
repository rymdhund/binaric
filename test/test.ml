module Ast = Binaric.Ast
module Parsers = Binaric.Parsers
module Eval = Binaric.Eval

let segment_eq (a:Ast.Segment.t) (b:Ast.Segment.t) =
  a.label = b.label &&
  a.identifier = b.identifier &&
  a.parameters = b.parameters &&
  a.multiplier = b.multiplier

let segment =
  let module M =
    struct
      type t = Ast.Segment.t
      let pp = fun ppf seg ->
        Format.fprintf ppf "%s" (Ast.Segment.show seg)
      let equal = segment_eq
    end
  in
  (module M: Alcotest.TESTABLE with type t = M.t)

let check_parse_segment exp s () =
  match Angstrom.parse_string Parsers.segment s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.check segment "same segment" exp seg

let check_parse exp s () =
  match Angstrom.parse_string Parsers.program s with
  | Error msg -> Alcotest.fail msg
  | Ok seg ->
    Alcotest.(check (list segment)) "same program" exp seg

let mk_segment ?label ?multiplier identifier parameters =
  Ast.Segment.{ label; multiplier; identifier; parameters }

let segment_set = List.map
  (fun (exp, s) -> ("parse segment", `Quick, check_parse_segment exp s))
  [
    (
      mk_segment "abc" [],
      "abc"
    );
    (
      mk_segment ~label:"foo" "abc" [],
      "foo: abc"
    );
    (
      mk_segment ~label:"f-o_o" "a-b_c" [],
      "f-o_o: a-b_c"
    );
    (
      mk_segment ~label:"a" "b" [`Value "c"],
      "a: b = c"
    );
    (
      mk_segment "b" [],
      "b [ ]"
    );
    (
      mk_segment ~label:"a" "b" [`Value "c"],
      "a: b [ c ]"
    );
    (
      mk_segment ~label:"a" "b" [`Value "c"; `Value "d"],
      "a: b [ c d ]"
    );
    (
      mk_segment ~multiplier:10 "b" [],
      "b * 10"
    );
    (
      mk_segment ~label:"a" ~multiplier:2 "b" [`Value "c"],
      "a: b = c * 2"
    );
  ]

let program_set = List.map
  (fun (exp, s) -> ("parse program", `Quick, check_parse exp s))
  [
    (
      [
        mk_segment "a" [];
        mk_segment "b" [];
      ],
      "a\nb"
    );
    (
      [
        mk_segment "a" [];
        mk_segment "b" [];
      ],
      "a # comment\nb"
    );
    (
      [
        mk_segment "a" [];
      ],
      "# comment\na"
    );
    (
      [
        mk_segment "a" [];
      ],
      "\na"
    );
    (
      [
        mk_segment "a" [ `Value "b"; `Value "c" ];
      ],
      "a [ b c ]"
    );
    (
      [
        mk_segment "a" [ `Value "b"; `Value "c" ];
      ],
      "a [ b #comment\n c ]"
    );
    (
      [
        mk_segment "a" [ `Value "b"; `Value "c" ];
      ],
      "a [ b #comment\n c ] \n "
    );
    (
      [
        mk_segment "a" [ `String "abc" ];
      ],
      "a = \"abc\" "
    );
    (
      [
        mk_segment "a" [ `String "\"" ];
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
    | Ok res -> Alcotest.(check (list char)) "same program" exp res

let eval_set = List.map
  (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
  [
    ([ '\x00' ], "d8 = 0 ");
    ([ '\xff' ], "d8 = 255 ");
    ([ '\xff' ], "d8 = -1 ");
    ([ '\x00'; '\xff' ], "d8 [ 0 255 ] ");
    ([ '\x00'; '\xff' ], "h8 [ 0 ff ] ");
    ([ '\x00'; '\x01' ], "d16 [ 1 ] ");
    ([ '\x00'; '\x01' ], "d16 [ 1 ] ");
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
    ("Invalid hex number: -1", "h8 = -1 ");
  ]

let () =
  Alcotest.run "Binaric Tests" [
    "segment_set", segment_set;
    "program_set", program_set;
    "eval_set", eval_set;
    "eval_fail_set", eval_fail_set;
  ]
