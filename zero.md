ᕦ(ツ)ᕤ
# zero

`zero` (always lowercase) is a feature-modular programming language designed for real-time distributed applications.

## design goals

- approachable: small, readable syntax
- fast: compiled, strongly typed, fast edit/build/test cycle
- flexible: modular backends targeting multiple languages/frameworks
- real-time: time is a first-class construct
- distributed: targets heterogenous clusters by default

## organisation

The specification of zero is organised into *layers*: layer 1 is the 'outermost' layer of the language, containing the basic concepts you need to learn in order to write programs in zero. Additional details and features are introduced at 'deeper' layers. The goal is to get you up and running quickly, adding complexity in stages.

## literate code

Programs in zero are written in a human-first markdown format. There are no comments; instead, all 'meta-information', such as specification, explanation, tutorials and tests, is defined using markdown text around the code. In the following document, all code examples are editable and runnable, and are presented like this:

    > (some code)
    => (expected result)

# layer 1

## types

### installed types

*Abstract numeric types* are arbitrary-precision numbers used when we don't care about the bit-depth or precision of the values we're programming with (we let the compiler choose the appropriate bit-depths):

    bool    : true or false
    int     : arbitrary-precision signed integer
    float   : arbitrary-precision floating-point number
    number  : either int or float

zero also defines the following *utility* types:

    char    : unicode character
    string  : unicode string
    time    : high-resolution time

### user-definable types

*enums* are defined using the `enum` keyword, as follows:

    enum evil = yes, no, maybe

*structure types* are defined using the `type` keyword, thus:

    type vec =
        number x, y, z = 0

zero automatically defines a constructor that can be called in many ways:

    vec v               : sets v.x, v.y, v.z to default (0)
    vec v = (1, 2, 3)   : v.x => 1, v.y => 2, v.z => 3
    vec v = (y: 1)      : v.y => 1, v.x and v.z => default (0)

## functions

Functions in zero are defined using a *named result* pattern, like this:

    on (vec r) = add (vec a, b)
        r = (a.x + b.x, a.y + b.y, a.z + b.z)

or like this:

    on (vec r) = (vec a) + (vec b)
        r = (a.x + b.x, a.y + b.y, a.z + b.z)

Function signatures can be any sequence of words, operators, or bracketed parameter groups. This *open syntax* allows concise definition of infix, prefix and post-fix operators, as well as more descriptive and readable code.

All functions are *pure* : they can only read their parameters, and write their results.

### if-then-else

zero uses a functional form of if-then-else, as follows:

    on (string s) = is (vec v) zero:
        b = if (v.x + v.y + v.z ==0) then true else false

## streams

A `stream` is a sequence of zero or more values.

    > int i$ << [0 to 10]
    => [0, 1, 2, ... 9]

The `$` suffix (read as 'string') indicates that a variable is a stream rather than a singular value; 

the `<<` operator indicates that we are 'pushing' or 'concatenating' the right-hand-side of the statement onto the stream; and 

the `[]` notation specifies a list of numbers.

### slice

Given any stream, we can extract some sub-range of the stream using the slice notation. For instance:

    > int j$ << i$[3 .. 5]
    => [3, 4]

If we try to read a slice that's outside the range of the stream, the slice is truncated:

    > int k$ << i$[8..15]
    => [8, 9]

Specifying a single number inside the `[]` does not return a singular value; instead, it returns a single-element slice:

    > int val = i$[8]
    => !! indexing not supported

    > int v$ << i$[8]
    => [8]

### map

Functions are defined on singular values; however, we can pass a stream to a function, which maps the function to each singular value in the stream. So, for instance:

    > int i$ << [1 to 10 incl]
    > int j$ << i$ * 2
    => [2, 4, 6, 8, ... 20]

We can also operate on two streams :

    > int k$ = i$ + j$
    => [3, 6, 9, 12, ... 30]

### reduce

We can reduce any stream through any function, using the following syntax:

    > int sum = i$ + _
    => 45

### filter

We can filter a stream using the following syntax:

    > int even$ = i$ : (_ & 1 == 0)

### sort

Finally, we can sort a stream using syntax like this:

    > int d$ = i$ > _

# features

A `feature` is the basic unit of modularity in zero. A feature defines a set of types,variables, and functions.

# contexts

A `context` is a group of features enabled at runtime. 