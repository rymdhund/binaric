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
    [ ("\x00\x00", "i8.hex 0\ni8.hex 0");
      ("\x00\x00", "i8.hex 0 # comment\ni8.hex 0");
      ("\x00", "# comment\ni8.hex 0");
      ("\x00", "\ni8.hex 0");
      ("\x00\xff", "i8.hex [ 0 #comment\n ff ]");
      ("\x00\xff", "i8.hex [ 0 #comment\n ff ] \n ")
    ]


let string_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("abc", {|asc "abc"|}); ("\\", {|asc "\\"|}); ("\"", {|asc "\""|}) ]


let literal_tests =
  List.map
    (fun (exp, s) -> ("eval literal", `Quick, check_eval exp s))
    [ ("\x00", "i8.dec  0");
      ("\x01", "i8.dec  1");
      ("\xff", "i8.dec  255");
      ("\xff", "i8.dec  -1");
      ("\x00\x00", "i16.dec  0");
      ("\x00\x01", "i16.dec  1");
      ("\xff\xff", "i16.dec  65535");
      ("\xff\xff", "i16.dec  -1");
      ("\x00\x00\x00\x00", "i32.dec  0 ");
      ("\x00\x00\x00\x01", "i32.dec  1 ");
      ("\xff\xff\xff\xff", "i32.dec  4294967295 ");
      ("\xff\xff\xff\xff", "i32.dec  -1 ");
      ("\x00", "i8.hex 0");
      ("\xff", "i8.hex ff")
    ]


let oneliners_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00", "i8.dec  0 ");
      ("\xff", "i8.dec  255 ");
      ("\xff", "i8.dec  -1 ");
      ("\x00\xff", "i8.dec [ 0 255 ] ");
      ("\x00\xff", "i8.hex [ 0 ff ] ");
      ("\x00\x01", "i16.dec [ 1 ] ");
      ("\x00\x01", "i16.dec [ 1 ] ");
      ("\x00\x01\x00\xff", "i16.dec [ 1 255 ] ");
      ("\x00\x00\x00\x01", "i32.dec  1 ");
      ("\xff\xff\xff\xff", "i32.dec  4294967295 ");
      ("\xff\xff\xff\xff", "i32.dec  -1 ");
      ("\x00", "{i8.dec  0}");
      ("\x00", "{ i8.dec  0 }");
      ("\x00", "{\ni8.dec  0\n}");
      ("\x00", "{ \n i8.dec  0 \n }");
      ("\x00", "a:i8.dec 0")
    ]


let multiliners_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00\xff", {|
      i8.dec 0
      i8.dec 255|});
      ("\x00\xff\xff", {|
i8.hex [
    00
    ff
   ]
i8.hex  ff
      |});
      ( "\x00\xff\xf0",
        {|
i8.hex  00 #comment1
i8.hex  ff #comment2
i8.hex  f0 #comment3"
      |}
      );
      ( "\015\002\254\x00\xff\x00\xff",
        {|
size:   i8.dec  15
width:  i8.dec  2
height: i8.dec  -2
data:   {
        i8.hex [ 00 ff 00 ff ]
}
      |}
      );
      ( "\015\002\254\x00\xff\x00\xff",
        {|
size:   i8.dec   15
width:  i8.dec   2
height: i8.dec   -2
data:   i8.hex [ 00 ff 00 ff ]
|}
      );
      ("", "{}");
      ("\xff\xff", {|
i8.hex ff
{}
{}
i8.hex ff
{}
|});
      ("\xff", {|
a: {
  i8.hex   ff
}
|});
      ("\xff\xff", {|
a: {
  i8.hex   ff
  i8.hex   ff
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
  one:     i8.dec   1
  b: {
    two:   i8.dec   2
    three: i8.dec   3
  }
}
|}
      );
      ( "\001\002\003",
        {|
one:   i8.dec  1
# comment
# comment
two:   i8.dec  2
three: i8.dec  3
|}
      );
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
i8.hex ff ** 3
|});
      ("\xff\xff\xff", {|
a: i8.hex ff ** 3
|});
      ("\xff\xff", {|
let a = i8.hex ff ** 2
a
|});
      ("\xff\xff", {|
let a = i8.hex ff
a ** 2
|})
    ]


let let_tests =
  List.map
    (fun (exp, s) -> ("eval let", `Quick, check_eval exp s))
    [ ("", "let abc=i8.hex ff");
      ("\xff\xff", {|
let foo = i8.hex  ff
foo
foo
|});
      ("\xff\xff", {|
let bar = {
  i8.hex  ff
  i8.hex  ff
}
bar
|});
      ( "\xff\x00\xff",
        {|
let x = {
  i8.hex  ff
  foo: {
    i8.hex 00
  }
  i8.hex  ff
}
x
|}
      );
      ("\xff\x00", {|
let x = i8.hex 00
{
  let x = i8.hex ff
  x
}
x
|});
      ("\xff", {|
let x = {
  let y = i8.hex ff
}
x.y
|});
      ( "\xff\x00",
        {|
let y = i8.hex 00
let x = {
  let y = i8.hex ff
}
x.y
y
|} )
    ]


let override_tests =
  List.map
    (fun (exp, s) -> ("eval let", `Quick, check_eval exp s))
    [ ("", "{} with {}");
      ("\xff", {|
i8.hex ff with { }
|});
      ("\xff", {|
i8.hex ff with {
  i8.hex 00
}
|});
      (* with is only evaluated on the rhs *)
      ("\xff", {|
a: i8.hex ff with {
  a: i8.hex 00
}
|});
      ( "\x00\xff",
        {|
{
  a: i8.hex ff
  b: i8.hex aa
} with {
  a: i8.hex 00
  b: i8.hex ff
}
|}
      );
      ( "\xff\xbb",
        {|
let a = {
  a: i8.hex ff
  b: i8.hex aa
}
a with {
  b: i8.hex bb
}
|}
      );
      (* Anonymous blocks are impossible to override *)
      ( "\xff\xaa",
        {|
{
  a: i8.hex ff
  {
    b: i8.hex aa
  }
} with {
  b: i8.hex bb
}
|}
      );
      ("\x01", {|
{
  a: {
    b: i8.hex 00
  }
} with {
  a: i8.hex 01
}
|});
      ("\x01", {|
{
  a: {
    b: i8.hex 00
  }
} with {
  a.b: i8.hex 01
}
|});
      ( "\x01",
        {|
let a = {
  b: i8.hex 00
}
let c = {
  d: a
}
c with {
  d.b: i8.hex 01
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
      ("i8.dec: 256 is out of range", "i8.dec  256 ");
      ("i8.dec: -255 is out of range", "i8.dec  -255 ");
      (": Unexpected character: '-'", "i8.dec  1-1 ");
      ("Invalid decimal number: 0f", "i8.dec  0f ");
      ("i8.hex: 100 is out of range", "i8.hex  100 ");
      ("Invalid hex number: \"-1\"", "i8.hex  -1 ");
      ("Invalid 32 bit decimal number: \"4294967296\"", "i32.dec  4294967296");
      ("Invalid 32 bit decimal number: \"-4294967295\"", "i32.dec  -4294967295");
      ( "Unknown identifier 'bar'",
        {|
      bar: i8.hex  ff
           bar
      |} )
    ]


let () =
  Alcotest.run
    "Binaric Tests"
    [ ("literal_tests", literal_tests);
      ("comment_tests", comment_tests);
      ("string_tests", string_tests);
      ("oneliners_tests", oneliners_tests);
      ("multiliners_tests", multiliners_tests);
      ("repetition_tests", repetition_tests);
      ("let_tests", let_tests);
      ("override_tests", override_tests);
      ("import_tests", import_tests);
      ("fail_tests", fail_tests)
    ]
