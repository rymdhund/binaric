# Binaric

A friendly dsl for constructing binary files.

The basic supported operations are:

```
d8   128     # one byte represented as a decimal number
d32  1024    # four bytes represented as a decimal number, output in big endian
d32  -1      # four bytes represented as a decimal number, output in big endian
h8   ff      # one byte represented as a hex number
asc  "abc"   # three ascii letters
```

You can specify many outputs at a time

```
h8  [ ff 00 ff 00 ]    # Will output ff00ff00
d16 [ 0 1 2 ]          # Will output 0000 0001 0002
asc [
  "here is "
  "one long "
  "string"
]
```

# Repetition

```
h8  0f * 4  #  0f 0f 0f 0f
```

# Labels

Labels are good for documentation:

```
first:  h8  01
second: h8  02
third:  h8  03

# Will give the bytes 0x01, 0x02, 0x03
```

# Sections

```
header: {
  width:  d8 2
  height: d8 2
}
body: {
  data: h8 [ ff fa 00 aa ]
}
```

# Constants

You can define constants to be used later.

```
const three = d8   3
const four  = d8 [ 0 4  ]

three  # 03
three  # 03
four   # 00 04
```


# Scopes

Constants are scoped within the block.

```
const a = h8 05

x: {
  const a = {
    h8 ff
  }
  a     # Gives 0xff
}
a     # Gives 0x05
```

You can assign a block to a constant, and you can get to the inner constants.

```
const a = {
  const b = {
    h8 ff
  }
}
a.b   # Gives 0xff
```

# Templating

You cat override labels using the `with` construction:

```
{
  x:  d8 1
  y:  d8 2
} with {
  x:  d8 3
}
# gives 03 02
```

It also works on blocks assigned to a constant:

```
const point = {
  x:  d8 1
  y:  d8 2
}

point with {
  x:  d8 3
}
# gives 03 02
```

And nested templating:

```
const points = {
  p1:   point
  p2:   point
}

points with {
  p1.x: d8 3
}
# gives 03 02 01 02
```

# Importing other binaric files

You can import other binaric files.

```
import "png.bn"  # Will output the result of png.bn

import "png.bn" with {
  size: d8 12
}

const png = import "png.bn"

png with {
  size: d8 10
}

```

Imports are always relative to the importing file.

```
# this is "src/file.bn"

import "utils/foo.bn"   # will import utils/src/foo.bn
```
