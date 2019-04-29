# Binaric

A friendly dsl for constructing binary files.

## Basic usage

```
$ binaric -o rgb.bmp examples/rgb.bmp.bn
```

## Types

The basic numeric types are `i8` up to `i64`.

```
i8   128     # one byte given by a decimal number
i32  1024    # four byte given by decimal number
i32  -1      # four bytes given by a negative decimal number
i8   0xff    # one byte given by a hex number
```

Endianess is specified by `.le` or `.be`. Big endian is the default.

```
i32 1     # 00 00 00 01
i32.be 1  # 00 00 00 01
i32.le 1  # 01 00 00 00
```

Strings can be encoded as `utf8` (which includes ascii), or `utf16`. Source files are always treated as utf8.

```
utf8      "abc"
utf8      "¢"
utf16     "¢"
utf16.le  "¢"
```

You can specify a number of values at a time using `[ ... ]`. Values in `0x[ ... ]` will be interpreted as hexadecimal.

```
i8  0x[ ff 00 ff 00 ]    # Will give ff00ff00
i16 [ 0 1 2 ]            # Will give 0000 0001 0002
utf8 [
  "This is "
  "three appended "
  "strings"
]
```

## Labels

Labels can be used to document your values.

```
first:  i8  0x01
second: i8  0x02
third:  i8  0x03

# Will give the bytes 0x01, 0x02, 0x03
```

## Blocks

Blocks can be used to express nested data.

```
header: {
  width:  i8 2
  height: i8 2
}
body: {
  data: i8 0x[ ff fa 00 aa ]
}
```

## Variables

You can bind variables to be used later.

```
let three = i8 3
let four  = i8 [ 0 4 ]

three  # 03
three  # 03
four   # 00 04
```

Variables are scoped within the block.

```
let foo = i8 0x05

{
  let foo = i8 0xff
  foo                   # Gives 0xff
}
foo                     # Gives 0x05
```

You can assign a block to a variable and you can get to the nested variables.

```
let foo = {
  let bar = i8 0xff
}
foo.bar   # Gives 0xff
```

## Repetition

Expressions can be repeated using the `**` operator.

```
i8  0x0f ** 4  #  0f 0f 0f 0f

{
  i8 0xff
  i8 0x00
} ** 2  # ff 00 ff 00

i8 0x00 ** 1000000000   # 1GB of zeroes
```

## Templating

You can override labels using the `with` construction:

```
{
  x:  i8 1
  y:  i8 2
} with {
  x:  i8 3
}
# gives 03 02
```

It also works on blocks assigned to a variable:

```
let point = {
  x:  i8 1
  y:  i8 2
}

point with {
  x:  i8 3
}
# gives 03 02
```

And nested templating:

```
let points = {
  p1:   point
  p2:   point
}

points with {
  p1.x: i8 3
}
# gives 03 02 01 02
```

## Importing other binaric files

You can import other binaric files.

```
import "png.bn"       # will run the "png.bn" file

import "png.bn" with {
  size: i8 12
}

let png = import "png.bn"

png with {
  size: i8 10
}

```

Imports are always relative to the importing file.

```
# This is "src/file.bn"

import "utils/foo.bn"   # will import src/utils/foo.bn
```

## Importing binary files

To import raw binary data from a file, use the `import.raw` expression.

```
import.raw "abc.png"
```

You can also import parts of a binary file. This is useful when you want to replace a part of a file. Here is how to replace the first 3 bytes of "abc.png":

```
header: {
  i8 0x[ ff ff ff ]
}
import.raw "abc.png" [3..]
```

And here we change just the 5th byte of a file:

```
import.raw "file.bn" [..5]

i8 0x00

import.raw "file.bn" [6..]
```
