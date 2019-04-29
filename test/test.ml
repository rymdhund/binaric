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
    [ ("\x00\x00", "i8 0x0\ni8 0x0");
      ("\x00\x00", "i8 0x0 # comment\ni8 0x0");
      ("\x00", "# comment\ni8 0x0");
      ("\x00", "\ni8 0x0");
      ("\x00\xff", "i8 0x[ 0 #comment\n ff ]");
      ("\x00\xff", "i8 0x[ 0 #comment\n ff ] \n ")
    ]


let string_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("abc", {|utf8 "abc"|}); ("\\", {|utf8 "\\"|}); ("\"", {|utf8 "\""|}) ]


let literal_tests =
  List.map
    (fun (exp, s) -> ("eval literal", `Quick, check_eval exp s))
    [ ("\x00", "i8  0");
      ("\x01", "i8  1");
      ("\xff", "i8  255");
      ("\xff", "i8  -1");
      ("\x00\x00", "i16  0");
      ("\x00\x01", "i16  1");
      ("\xff\xff", "i16  65535");
      ("\xff\xff", "i16  -1");
      ("\x01\x00", "i16.le  1");
      ("\x00\x00\x00\x00", "i32  0");
      ("\x00\x00\x00\x01", "i32  1");
      ("\xff\xff\xff\xff", "i32  4294967295");
      ("\xff\xff\xff\xff", "i32  -1");
      ("\x01\x00\x00\x00", "i32.le  1");
      ("\x00\x00\x00\x00\x00\x00\x00\x00", "i64  0");
      ("\x00\x00\x00\x00\x00\x00\x00\x01", "i64  1");
      ("\xff\xff\xff\xff\xff\xff\xff\xff", "i64  18446744073709551615");
      ("\xff\xff\xff\xff\xff\xff\xff\xff", "i64  -1");
      ("\x01\x00\x00\x00\x00\x00\x00\x00", "i64.le  1");
      ("\x00", "i8 0x0");
      ("\x01", "i8 0x1");
      ("\xff", "i8 0xff");
      ("\x00\x00", "i16 0x0");
      ("\x00\x01", "i16 0x1");
      ("\xff\xff", "i16 0xffff");
      ("\x01\x00", "i16.le 0x1");
      ("\x00\x00\x00", "i24 0x0");
      ("\x00\x00\x01", "i24 0x1");
      ("\xff\xff\xff", "i24 0xffffff");
      ("\x01\x00\x00", "i24.le 0x1");
      ("\x00\x00\x00\x00", "i32 0x0");
      ("\x00\x00\x00\x01", "i32 0x1");
      ("\xff\xff\xff\xff", "i32 0xffffffff");
      ("\x01\x00\x00\x00", "i32.le 0x1");
      ("\x00\x00\x00\x00\x00\x00\x00\x00", "i64 0x0");
      ("\x00\x00\x00\x00\x00\x00\x00\x01", "i64 0x1");
      ("\xff\xff\xff\xff\xff\xff\xff\xff", "i64 0xffffffffffffffff");
      ("\x01\x00\x00\x00\x00\x00\x00\x00", "i64.le 0x1");
      ("\x00", "i8 0b0");
      ("\xff", "i8 0b11111111");
      ("\xff\xff\x00\xff", "i32 0b11111111_11111111_00000000_11111111");
      ("\x00\x00\x00\x00\x00\x00\x00\x01", "i64 0b1");
      ("", {|utf8 ""|});
      ("abc", {|utf8 "abc"|});
      ("\xc2\xa2", "utf8 \"\xc2\xa2\"");
      ("\x00\xa2", "utf16 \"\xc2\xa2\"");
      ("\xa2\x00", "utf16.le \"\xc2\xa2\"");
      ("\xf0\x11", "i16  0xf_0_1_1");
      ("\x00\x0f\x42\x40", "i32  1_000_000")
    ]


let oneliners_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00", "i8  0 ");
      ("\xff", "i8  255 ");
      ("\xff", "i8  -1 ");
      ("\x00\xff", "i8 [ 0 255 ] ");
      ("\x00\xff", "i8 0x[ 0 ff ] ");
      ("\xf0\x00", "i8 0b[ 11110000 0 ] ");
      ("\x00\x01", "i16 [ 1 ] ");
      ("\x00\x01", "i16 [ 1 ] ");
      ("\x00\x01\x00\xff", "i16 [ 1 255 ] ");
      ("\x00\x00\x00\x01", "i32  1 ");
      ("\xff\xff\xff\xff", "i32  4294967295 ");
      ("\xff\xff\xff\xff", "i32  -1 ");
      ("\x00", "{i8  0}");
      ("\x00", "{ i8  0 }");
      ("\x00", "{\ni8  0\n}");
      ("\x00", "{ \n i8  0 \n }");
      ("\x00", "a:i8 0")
    ]


let multiliners_tests =
  List.map
    (fun (exp, s) -> ("eval program", `Quick, check_eval exp s))
    [ ("\x00\xff", {|
      i8 0
      i8 255|});
      ("\x00\xff\xff", {|
i8 0x[
    00
    ff
   ]
i8 0xff
      |});
      ( "\x00\xff\xf0",
        {|
i8 0x00 #comment1
i8 0xff #comment2
i8 0xf0 #comment3"
      |} );
      ( "\015\002\254\x00\xff\x00\xff",
        {|
size:   i8  15
width:  i8  2
height: i8  -2
data:   {
        i8 0x[ 00 ff 00 ff ]
}
      |}
      );
      ( "\015\002\254\x00\xff\x00\xff",
        {|
size:   i8   15
width:  i8   2
height: i8   -2
data:   i8 0x[ 00 ff 00 ff ]
|}
      );
      ("", "{}");
      ("\xff\xff", {|
i8 0xff
{}
{}
i8 0xff
{}
|});
      ("\xff", {|
a: {
  i8 0xff
}
|});
      ("\xff\xff", {|
a: {
  i8 0xff
  i8 0xff
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
  one:     i8   1
  b: {
    two:   i8   2
    three: i8   3
  }
}
|}
      );
      ( "\001\002\003",
        {|
one:   i8  1
# comment
# comment
two:   i8  2
three: i8  3
|} );
      ("abcdef", {|
utf8 [
 "abc"
 "def"
]
|})
    ]


let repetition_tests =
  List.map
    (fun (exp, s) -> ("eval repetition", `Quick, check_eval exp s))
    [ ("\xff\xff\xff", {|
i8 0xff ** 3
|});
      ("\xff\xff\xff", {|
a: i8 0xff ** 3
|});
      ("\xff\xff", {|
let a = i8 0xff ** 2
a
|});
      ("\xff\xff", {|
let a = i8 0xff
a ** 2
  |});
      ("\x01\xff\x01\xff", {|
{
  i8 0x01
  i8 0xff
} ** 2
|})
    ]


let let_tests =
  List.map
    (fun (exp, s) -> ("eval let", `Quick, check_eval exp s))
    [ ("", "let abc=i8 0xff");
      ("\xff\xff", {|
let foo = i8 0xff
foo
foo
|});
      ("\xff\xff", {|
let bar = {
  i8 0xff
  i8 0xff
}
bar
|});
      ( "\xff\x00\xff",
        {|
let x = {
  i8 0xff
  foo: {
    i8 0x00
  }
  i8 0xff
}
x
|} );
      ("\xff\x00", {|
let x = i8 0x00
{
  let x = i8 0xff
  x
}
x
|});
      ("\xff", {|
let x = {
  let y = i8 0xff
}
x.y
|});
      ("\xff\x00", {|
let y = i8 0x00
let x = {
  let y = i8 0xff
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
i8 0xff with { }
|});
      ("\xff", {|
i8 0xff with {
  i8 0x00
}
|});
      (* with is only evaluated on the rhs *)
      ("\xff", {|
a: i8 0xff with {
  a: i8 0x00
}
|});
      ( "\x00\xff",
        {|
{
  a: i8 0xff
  b: i8 0xaa
} with {
  a: i8 0x00
  b: i8 0xff
}
|}
      );
      ( "\xff\xbb",
        {|
let a = {
  a: i8 0xff
  b: i8 0xaa
}
a with {
  b: i8 0xbb
}
|} );
      (* Anonymous blocks are impossible to override *)
      ( "\xff\xaa",
        {|
{
  a: i8 0xff
  {
    b: i8 0xaa
  }
} with {
  b: i8 0xbb
}
|} );
      ("\x01", {|
{
  a: {
    b: i8 0x00
  }
} with {
  a: i8 0x01
}
|});
      ("\x01", {|
{
  a: {
    b: i8 0x00
  }
} with {
  a.b: i8 0x01
}
|});
      ( "\x01",
        {|
let a = {
  b: i8 0x00
}
let c = {
  d: a
}
c with {
  d.b: i8 0x01
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
      ("i8: '256' out of range", "i8  256 ");
      ("i8: '-255' out of range", "i8  -255 ");
      (": Unexpected character: '-'", "i8  1-1 ");
      ("i8: '0f' Invalid decimal number", "i8  0f ");
      ("i8: '0x100' out of range", "i8 0x100 ");
      (": Unexpected character: '-'", "i8 0x-1 ");
      ("i32: '4294967296' Invalid 32 bit decimal number", "i32  4294967296");
      ("i32: '-4294967295' Invalid 32 bit decimal number", "i32  -4294967295");
      ("i8: '-1' Invalid hex number", "i8 0x[ -1 ] ");
      ( "Unknown identifier 'bar'",
        {|
      bar: i8 0xff
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
