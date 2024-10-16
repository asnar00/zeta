ᕦ(ツ)ᕤ
# zero

`zero` (always lowercase) is a feature-modular programming language designed for real-time distributed applications.

## design goals

- small: simple definition, readable syntax, featherweight tools
- fast: compiled, strongly typed, fast edit/build/test cycle
- flexible: modular backends targeting multiple languages/frameworks
- real-time: predictable throughput and latency
- distributed: targets heterogenous clusters

## organisation

The specification of zero is organised into *layers*: layer 1 is the 'outermost' layer of the language, containing the basic concepts you need to learn in order to write programs in zero. Additional refinements are introduced at 'deeper' layers. The goal is to get you up and running quickly, adding complexity in stages.

## literate code

Programs in zero are written as human-first markdown documents, like this one, containing text and code snippets. All 'meta-information', such as specification, explanation, tutorials and tests, is defined using markdown text, next to the code it describes. 

In the following document, all code examples are presented like this:

    > (some code)
    => (expected result)

# ______________________________________________
# layer 1

## types

### installed types

*Abstract numeric types* don't specify bit-depth; instead we let the compiler choose:

    int     : signed integer
    float   : floating-point number
    number  : int or float

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

zero automatically defines a constructor that can be called in three ways:

    > vec v                         : default constructor
    => v: vec(x: 0, y: 0, z: 0)
    
    > vec v = (1, 2, 3)             : anonymous constructor
    => v: vec(x: 1, y: 2, z: 3)
    
    > vec v = (y: 1)                : named constructor
    => v: vec(x: 0, y: 1, z: 0)

## functions

Functions in zero are defined using a *named result* pattern, like this:

    on (number r) = min (number a, number b)
        r = if (a < b) then a else b

or like this:

    on (vec r) = (vec a) + (vec b)
        r = (a.x + b.x, a.y + b.y, a.z + b.z)

Functions have *open syntax* : the signature can be any sequence of words, operators and bracketed parameter groups (there must be at least one parameter group). This allows us to concisely define and call conversion functions, operators (infix, prefix, postfix), and generally and write more readable, natural-sounding code.

Functions are *pure*, which means they can only read their parameters, and can only write their outputs (multiple outputs are allowed). Internally, functions can create and assign local variables, but those variables are thereafter immutable.

## streams

A *stream* is a sequence of values.

Stream variables are denoted by suffixing the variable name with `$` (read as 'stream' or 'string'):

    > int i$                    : declares a stream of integers
    => i$: []

The `<<` operator ('push') pushes a singular value onto the stream:

    > int i$ << 1 << 2 << 3     : push individual values to stream
    => i$: [1, 2, 3]

The `[]` notation specifies a range of values more concisely:

    > int i$ << [1, 2, 3]       : push multiple values
    => i$: [1, 2, 3]

The `[]` notation can also specify a range of values:
    
    > int i$ << [0 to 5]
    => i$: [0, 1, 2, 3, 4]

We can also use loops, like this:

    > int i$ << 1 << (i$ * 2) until (i$ == 16)
    => i$: [1, 2, 4, 8, 16]

or this:

    > int i$ << 1 << (i$ * 2) while (i$ < 32)
    => i$: [1, 2, 4, 8, 16]

### slices, pushing

The `[]` notation can also be used to take a slice of an array, for instance:

    > int i$ << [1, 2, 4, 8, 16]
    > j$ = i$[0 to 3]
    => j$: [1, 2, 4]

The `<<` operator with a stream as a right-hand-side argument empties that stream out into the receiver on the left:

    > int i$ << [1, 2, 3]
    > int j$ << [4, 5, 6]
    > i$ << j$
    => i$: [1, 2, 3, 4, 5, 6]
       j$: []

The `<<` operator combined with a slice operation can be used to transfer part of the right-hand stream into the receiver on the left:

    > int i$ << [1, 2, 3]
    > int j$ << [4, 5, 6]
    > i$ << j$[0 to 1]
    => i$: [1, 2, 3, 4]
       j$: [5, 6]

### map, reduce, filter, sort

Functions are defined to take singular values as parameters. This restriction lets zero give us a very concise syntax for mapping a function to a stream:

    > int i$ << [1 to 5 inclusive]
    => i$: [1, 2, 3, 4, 5]
    > int j$ = i$ + 1                   : maps '+ 1' to each element of i$
    => j$: [2, 3, 4, 5, 6]

We can also map a function to multiple streams, for example:

    > int k$ = i$ + j$                  : element-by-element addition
    => k$: [3, 4, 5, 6, 7]

To reduce an array to a singular value, we use the following syntax:

    > int sum = j$ + _                  : reduce j$ using '+'
    => sum: 20

The `_` here means something like "itself".

To filter, we use the `where` reserved keyword:

    > int e$ = j$ where (_ & 1 == 0)
    => e$: [2, 4, 6]

And finally, to sort, we use the `sort` keyword:

    > int d$ = sort j$ > _              : descending order
    => d$: [6, 5, 4, 3, 2]

### time

The `at` keyword specifies a 'sample rate' that adds a time-stamp to each value of a stream, for example:

    > int i$ << [10 to 0] at (1 hz)
    => i$: [10 at 0s, 9 at 1s, 7 at 2s, ... 1 at 9s]

This counts down from 10 to 1 at the rate of 1 step per second. The postfix function `hz` (hertz, or cycles per second) takes a number and returns a frequency.

We could also express the same thing using the `every` keyword:

    > int i$ << [10 to 0] every (1 sec)
    => i$: [10 at 0s, 9 at 1s, 7 at 2s, ... 1 at 9s]

Here, the postfix function `sec` takes a number and returns a duration.

The reserved keyword `t` refers to the "current time", and is available as a global read-only variable in every expression. So, for example, we can do things like:

    > float f$ << sin(t) at (48 khz) forever
    => f$: [0 at 0s, ...]

Here `forever` is a keyword that just means `while true` or `until false`.

## features

A `feature` is the fundamental unit of modularity in zero. A feature can define new types, variables, and functions; and it can modify existing types, variables, and functions.

The canonical "hello world" looks like this:

    feature Hello extends Run
        string out$                 : feature-scope variable
        on hello()                  : new or modified function
            out$ << "hello world"
        replace run()               : replace existing function
            hello()
    
    > run()
    => out$: ["hello world"]

We can then extend "hello world" to, for example, count down from 10 to 1 before printing hello:

    feature Countdown extends Hello
        on countdown()                          : new function
            out$ << [10 to 0] every (1 sec)
        before hello()                          : modify 'hello'
            countdown()
    
    > run()
    => out$: ["10" at 0s, "9" at 1s, ... "0" at 10s, 
              "hello world" at 10s]

Or, for example, we could say "goodbye" before the program ends:

    feature Goodbye extends Hello
        on bye()
            out$ << "kthxbye!"
        after hello()
            bye()
    
    > run()
    => out$: ["10" at 0s, "9" at 1s, ... "0" at 10s, 
              "hello world" at 10s, "kthxbye!" at 10s]

## contexts

A `context` is a group of enabled features. For instance:

    context SimpleHello = [Hello]
    
    context FullFatHello = [Hello, Countdown, Goodbye]
    
    context QuickPoliteHello = [Hello, Goodbye]
