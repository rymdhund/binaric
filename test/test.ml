let binary_string =
  Alcotest.testable (fun fmt -> Format.fprintf fmt "%S") ( = )


let check_eval exp (prog : string) () =
  match Lib.run prog with
  | Error msg -> Alcotest.fail msg
  | Ok res -> Alcotest.check binary_string "same evaluation" exp res


let label_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("", "abc: []"); ("", "f-o_o: []"); ("", "let a-b_c = []\na-b_c") ]


let comment_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00\x00", "h8 0\nh8 0");
      ("\x00\x00", "h8 0 # comment\nh8 0");
      ("\x00", "# comment\nh8 0");
      ("\x00", "\nh8 0");
      ("\x00\xff", "h8 [ 0 #comment\n ff ]");
      ("\x00\xff", "h8 [ 0 #comment\n ff ] \n ")
    ]


let string_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("abc", {|asc "abc"|}); ("\\", {|asc "\\"|}); ("\"", {|asc "\""|}) ]


let oneliners_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00", "d8  0 ");
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
      ("\x00", "a:d8 0")
    ]


let multiliners_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00\xff", {|
      d8 0
      d8 255|});
      ("\x00\xff\xff", {|
h8 [
    00
    ff
   ]
h8  ff
      |});
      ( "\x00\xff\xf0",
        {|
h8  00 #comment1
h8  ff #comment2
h8  f0 #comment3"
      |} );
      ( "\015\002\254\x00\xff\x00\xff",
        {|
size:   d8  15
width:  d8  2
height: d8  -2
data:   {
        h8 [ 00 ff 00 ff ]
}
      |}
      );
      ( "\015\002\254\x00\xff\x00\xff",
        {|
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
      ("\xff", {|
a: {
  h8   ff
}
|});
      ("\xff\xff", {|
a: {
  h8   ff
  h8   ff
}
|});
      ("", {|
a:{
b:{
}
}
|});
      ( "\001\002\003",
        {|
a: { #comment
  one:     d8   1
  b: {
    two:   d8   2
    three: d8   3
  }
}
|}
      );
      ( "\001\002\003",
        {|
one:   d8  1
# comment
# comment
two:   d8  2
three: d8  3
|} );
      ("abcdef", {|
asc [
 "abc"
 "def"
]
|})
    ]


let repetition_tests =
  List.map
    (fun (exp, s) -> ("eval repetition", `Quick, check_eval exp s))
    [ ("\xff\xff\xff", {|
h8 ff ** 3
|});
      ("\xff\xff\xff", {|
a: h8 ff ** 3
|});
      ("\xff\xff", {|
let a = h8 ff ** 2
a
|});
      ("\xff\xff", {|
let a = h8 ff
a ** 2
|})
    ]


let let_tests =
  List.map
    (fun (exp, s) -> ("eval let", `Quick, check_eval exp s))
    [ ("", "let abc=h8 ff");
      ("\xff\xff", {|
let foo = h8  ff
foo
foo
|});
      ("\xff\xff", {|
let bar = {
  h8  ff
  h8  ff
}
bar
|});
      ( "\xff\x00\xff",
        {|
let x = {
  h8  ff
  foo: {
    h8 00
  }
  h8  ff
}
x
|} );
      ("\xff\x00", {|
let x = h8 00
{
  let x = h8 ff
  x
}
x
|});
      ("\xff", {|
let x = {
  let y = h8 ff
}
x.y
|});
      ("\xff\x00", {|
let y = h8 00
let x = {
  let y = h8 ff
}
x.y
y
|})
    ]


let override_tests =
  List.map
    (fun (exp, s) -> ("eval let", `Quick, check_eval exp s))
    [ ("", "{} with {}");
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
      ( "\x00\xff",
        {|
{
  a: h8 ff
  b: h8 aa
} with {
  a: h8 00
  b: h8 ff
}
|} );
      ( "\xff\xbb",
        {|
let a = {
  a: h8 ff
  b: h8 aa
}
a with {
  b: h8 bb
}
|} );
      (* Anonymous blocks are impossible to override *)
      ( "\xff\xaa",
        {|
{
  a: h8 ff
  {
    b: h8 aa
  }
} with {
  b: h8 bb
}
|} );
      ("\x01", {|
{
  a: {
    b: h8 00
  }
} with {
  a: h8 01
}
|});
      ("\x01", {|
{
  a: {
    b: h8 00
  }
} with {
  a.b: h8 01
}
|});
      ( "\x01",
        {|
let a = {
  b: h8 00
}
let c = {
  d: a
}
c with {
  d.b: h8 01
}
|}
      )
    ]


let import_tests =
  List.map
    (fun (exp, s) -> ("import", `Quick, check_eval exp s))
    [ ("\xff", {|
    import "ff.bn"
    |});
      (* Imports are always relative to the importing file *)
      ("\x0a", {|
    import "dir/inc_test.bn"
    |});
      ("\xff\x00\xff", {|
    import.raw "ff00ff.raw"
    |});
      ("\x00\xff", {|
    import.raw "ff00ff.raw" [1..]
    |});
      ("\xff\x00", {|
    import.raw "ff00ff.raw" [..2]
    |});
      ("\xff\x00", {|
    import.raw "ff00ff.raw" [0..2]
    |});
      ("\x00", {|
    import.raw "ff00ff.raw" [1..2]
    |});
      ("", {|
    import.raw "ff00ff.raw" [10..]
    |});
      ("", {|
    import.raw "ff00ff.raw" [..0]
    |})
    ]


let check_fail expected (prog : string) () =
  match Lib.run prog with
  | Error msg -> Alcotest.(check string) "same error" expected msg
  | Ok _ -> Alcotest.fail "Expected to fail"


let fail_tests =
  List.map
    (fun (exp, s) -> ("eval fail program", `Quick, check_fail exp s))
    [ ("Unknown identifier 'abc'", "abc");
      ("d8: 256 is out of range", "d8  256 ");
      ("d8: -255 is out of range", "d8  -255 ");
      (": Unexpected character: '-'", "d8  1-1 ");
      ("Invalid decimal number: 0f", "d8  0f ");
      ("h8: 100 is out of range", "h8  100 ");
      ("Invalid hex number: \"-1\"", "h8  -1 ");
      ("Invalid 32 bit decimal number: \"4294967296\"", "d32  4294967296");
      ("Invalid 32 bit decimal number: \"-4294967295\"", "d32  -4294967295");
      ("Unknown identifier 'bar'", {|
      bar: h8  ff
           bar
      |})
    ]


let () =
  Alcotest.run
    "Binaric Tests"
    [ ("comment_tests", comment_tests);
      ("string_tests", string_tests);
      ("oneliners_tests", oneliners_tests);
      ("multiliners_tests", multiliners_tests);
      ("repetition_tests", repetition_tests);
      ("let_tests", let_tests);
      ("override_tests", override_tests);
      ("import_tests", import_tests);
      ("fail_tests", fail_tests)
    ]
