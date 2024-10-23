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

The specification of zero is organised into *layers*: layer 1 is the 'outermost' layer of the language, containing the basic concepts you need to learn in order to write programs in zero. Additional refinements are introduced at 'deeper' layers. "Controversial", "experimental", "poorly-defined" or "needs work" language features are marked using the 👀 emoji.

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

## streams

A *stream* is a sequence of values.

Stream variables are denoted by suffixing the variable name with `$` (read as 'stream' or 'string'):

    > int i$                    : declares a stream of integers
    => i$: []

The `<<` operator ('push') pushes a singular value onto the stream:

    > int i$ << 1 << 2 << 3     : push individual ints to stream
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

### slices, pushing 👀 

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

The `<<` operator combined with a slice operation can be used to transfer part of the right-hand stream into the receiver on the left:  **controversial : there should actually be a better notation for this; a slice should be a read-only window onto a stream, and using things on the RHS shouldn't modify them**

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

The reserved keyword `t` 👀 refers to the "current time", and is available as a global read-only variable in every expression. So, for example, we can do things like: 

    > float f$ << sin(t) at (48 khz) forever
    => f$: [0 at 0s, ...]

Here `forever` is a keyword that just means `while true` or `until false`.

## features

A `feature` is the fundamental unit of modularity in zero. A feature can define new types, variables, and functions; and it can modify existing types, variables, and functions.

The canonical "hello world" looks like this:

    feature Hello extends Run
        string out$                 : feature-scope variable 👀 
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

## contexts 👀 

A `context` is a group of enabled features. For instance:

    context SimpleHello = [Hello]
    
    context FullFatHello = [Hello, Countdown, Goodbye]
    
    context QuickPoliteHello = [Hello, Goodbye]

We can `select`  a context, pulling all its functions into the global scope:

```
select FullFatHello
```

Which will automatically deactivate any other active overlapping contexts.

Alternatively, we can use the `.` notation to selectively call functions defined in a context:

```
SimpleHello.run()
FullFatHello.run()
```

# layer 2

## aliases

All names (variable names, type names, and functions) can be defined using a short *name* and a longer, more descriptive *alias*, using the `|` operator (read as "or"). For instance:

```
type col | colour =
    number r | red, g | green, b | blue = 0
```

So we can write a function using either the short names:

```
on (col r) = (col a) + (col b)
    r = col(a.r + b.r, a.g + b.g, a.b + b.b)
```

or the longer aliases:

```
on (colour r) = (colour a) + (colour b)
    r = colour(a.red + b.red, a.green + b.green, a.blue + b.blue)
```

This allows code editors to present unfamiliar code using the more descriptive aliases, and then switch to the more efficient short form for more familiar code.

## types

### concrete numeric types

We use abstract numeric types (`int`, `float`, and `number`) when we "don't care" about bit-depth or precision. During compilation, these types are replaced by some combination concrete numeric types with fixed bit-depth, precision, and layout:

```
u8, u16, u32, u64			: fixed-size unsigned integers
i8, i16, i32, i64			: fixed-size signed integers
f32, f64						  : fixed-precision IEEE floating-point
```

We can also define arbitrary-sized unsigned integers using `u[n]` where `n` is any positive integer, for example:

```
u1										: 1-bit unsigned integer
u3										: 3-bit unsigned integer
etc.
```

### type relations

We can define a type to be a *supertype* of one or more types; for example, `int` might be defined thus:

```
type int > i8, i16, i32, i64
```

This asserts, for each type `T` on the right-hand-side of the statement, that every `T` is an `int`, but not every `int` is a `T`.  Any function or structure defined using `int` can be rewritten by the compiler to use any of `i8`...`i64`, but not every function or structure defined using (say) `i32` can be rewritten to use `int`. 

**based on this definition, every u8 is an int but not vice versa, so does int > u8?**

We can also define a new type to be a *subtype* of existing types. For instance, we might wish to define a new floating-point format `f16` to be a 16-bit floating-point number:

```
type f16 =
	u1 sign = 0
	u5 exponent = 0
	u10 fraction = 0
```

We'd like every existing function and structure defined using `float` to be rewritable to use our new `f16` type, and we can achieve this using the subtyping declaration thus:

```
type f16 < float
```

### narrowed types

We can use subtype relations to achieve better code correctness, by creating *narrowed* versions of existing types. Recall our previous example structure type

```
type vec =
    number x, y, z = 0
```

We can create three subtypes of `vec` to represent absolute positions in space, offsets between absolute positions, and normalised directions (which always have a length of 1) thus:

```
type position < vec					: positions in space
type offset < vec						: offset from one position to another
type direction < vec				: normalised direction vectors
```

Here, the statement "every X is a Y but not every Y is an X" applies for X = {`pos`, `offset`, `dir`} and Y= `vec`. We can now go on to define functions like this:

```
on (offset r) = (position a) - (position b) ...
on (position r) = (position a) + (offset b) ...
on (direction r) = normalise (offset o) ...
```

Under this scheme, we can still call addition and subtraction on `vec` instances, but when we specifically use `position`, `offset`or `direction`, we gain the benefit of stricter type checking.

### structure extension





