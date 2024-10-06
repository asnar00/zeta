ᕦ(ツ)ᕤ
# zero

A place for notes and scribbles about the language, rather than the zeta project.

Current definition of the language is:

## comments

*experimental*

comments are at the start of the line, followed by a `:`:

    comment:    code

## types

zero is strongly typed.

concrete numerical types:

    u8, u16, u32, u64
    i8, i16, i32, i64
    f32, f64

abstract numerical types:

    type int = i8 | i16 | i32 | i64
    type uint = u8 | u16 | u32 | u64
    type fp | float = f32 | f64
    type num | number = int | float

enums:

    enum evil = no | yes | dunno

convenience:

    boolean : bool | boolean
    string  : str | string
    time    : time

structure types:

long/short names for conciseness or verbosity

    type col | colour = 
        r | red, g | green, b | blue : number = 0

type narrowers:

    type vec | vector = 
        x, y, z : number = 0
    
    position in space: type pos | position < vector

    length in meters: type len | length < number

    direction vector always has length 0: type dir | direction < vector

variable declarations:

automatically created constructor

can use c-style:

    vec v = (x: 1, y: 2, y: 3)

or ts-style:

    v: vec = (1, 2, 3)

## functions



## arrays

    int a[] = [ 0 .. 10 ]               : [ 0, 1, ..., 9 ]
    int b[] = [ 1 .. =10 ]              : [ 1, 2, ..., 10 ]
    int c[] = a[] + b[]                 : map '+'
    int d[] = a[] + 1                   : map '+' with singular value
    int sum = a[] + _                   : reduce using '+'
    int e[] = a[] : _ & 1 == 0          : filter
    int s[] = a[>]                      : sort (descending)

    int f[] = a[] << b[]                : concatenate
    int g[] = a[] << 1                  : concatenate singular value
    int h[] = a[ 0 .. 5 ]               : slice (0-5 non-incl)
    int j[] = a[ 0 .= 5 ]               : slice (0-5 inclusive)
    int k[] = a[ 0 ]                    : slice (0-0 inclusive) *returns array*
    int l[] = a[ _ + 5 ]                : shift array left by 5 places
    int m[] = a[ _ * 2 ]                : scale the array

Generalisation of this syntax to n-dimensional arrays is made by noticing that (eg.) a 2D array is just a 1-D array, with a function mapping a 2D integer coordinate to a 1D integer index.

There's a general need for a type that is an "index into an array" -> this is the equivalent of a pointer. And I think that type just ends up being a slice, i.e. an array anyway. Arrays handle zero-or-more or zero-or-one semantics trivially, and the map syntax makes it just as easy to use them as singular values.

## streams

A stream is a 1-D array that exists in time; i.e. each value in the array has a specific time. Under the hood, it's a sequence of arrays, but each array has a set of meta-data that scale it to a time range (basically scale and offset). Streams are a bit like texture samplers, and there's a 1-1 correspondence with samplers in gpu-land.

So we can do a countdown:

    num a$ << [ 10 .. =1 ] at (1 hz)

or we can synthesize audio:

    audio out$ << sinewave(40 hz) at (48 khz) forever

`at` and `forever` and `until` are stream modifiers - they decide how fast it runs, and when it stops.


## features

## contexts

-----------------------
scribbles
------------------------

# some other nice ideas

- parse/print defined at the same time in the string format.

    so for instance:

    "hello \(name) how's it going"

    can both print and parse a string - it's bidirectional.

    So actually make the parser "accessible" as part of the language.

- brain-breaker: what if the compiler (=parser, grammar, lexer, etc) is feature modular? And you can create different compilers / languages just by selecting different contexts?

So you can morph the syntax of the language to whatever you want by adding features to the language. That's pretty cool, I reckon.

## demo targets:

1- the zerp repl, written in zero, with the editor
2- wasm backend: audio output (hummingbird-stems)
3- wgpu backend: polygon output (frankie the robot)

The thing about choosing these is that I know how they work, intimately! And they form a direct line to d3.





