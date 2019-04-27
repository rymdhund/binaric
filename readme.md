# Binaric

A friendly dsl for constructing binary files.

The basic supported operations are:

```
i8.dec   128     # one byte represented as a decimal number
i32.dec  1024    # four bytes represented as a decimal number, output in big endian
i32.dec  -1      # four bytes represented as a decimal number, output in big endian
i8.hex   ff      # one byte represented as a hex number
asc  "abc"   # three ascii letters
```

You can specify many outputs at a time

```
i8.hex  [ ff 00 ff 00 ]    # Will output ff00ff00
i16.dec [ 0 1 2 ]          # Will output 0000 0001 0002
asc [
  "here is "
  "one long "
  "string"
]
```

# Labels

Labels are good for documentation:

```
first:  i8.hex  01
second: i8.hex  02
third:  i8.hex  03

# Will give the bytes 0x01, 0x02, 0x03
```

# Blocks

Blocks can be used to express nested data.

```
header: {
  width:  i8.dec 2
  height: i8.dec 2
}
body: {
  data: i8.hex [ ff fa 00 aa ]
}
```

# Variables

You can bind variables to be used later.

```
let three = i8.dec   3
let four  = i8.dec [ 0 4  ]

three  # 03
three  # 03
four   # 00 04
```

Constants are scoped within the block.

```
let a = i8.hex 05

x: {
  let a = {
    i8.hex ff
  }
  a     # Gives 0xff
}
a       # Gives 0x05
```

You can assign a block to a variable, and you can get to the nested variables.

```
let a = {
  let b = {
    i8.hex ff
  }
}
a.b   # Gives 0xff
```

# Repetition

Expressions can be repeated using the `**` operator.

```
i8.hex  0f ** 4  #  0f 0f 0f 0f

{
  i8.hex ff
  i8.dec 0
} ** 2  # ff 00 ff 00
```

# Templating

You cat override labels using the `with` construction:

```
{
  x:  i8.dec 1
  y:  i8.dec 2
} with {
  x:  i8.dec 3
}
# gives 03 02
```

It also works on blocks assigned to a variable:

```
let point = {
  x:  i8.dec 1
  y:  i8.dec 2
}

point with {
  x:  i8.dec 3
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
  p1.x: i8.dec 3
}
# gives 03 02 01 02
```

# Importing other binaric files

You can import other binaric files.

```
import "png.bn"  # Will output the result of png.bn

import "png.bn" with {
  size: i8.dec 12
}

let png = import "png.bn"

png with {
  size: i8.dec 10
}

```

Imports are always relative to the importing file.

```
# this is "src/file.bn"

import "utils/foo.bn"   # will import utils/src/foo.bn
```

# Importing binary files

To import raw binary data from a file, use the `import.raw` expression.

```
import.raw "abc.png"
```

You can also import parts of a binary file. This is useful when you want to replace a part of a file. Here we overwrite the first 3 bytes of "abc.png":

```
header: {
  i8.hex [ ff ff ff ]
}
import.raw "abc.png" [3..]
```

And here we change just the 5th byte of a file:

```
import.raw "file.bn" [..5]

i8.hex 00

import.raw "file.bn" [6..]
```
