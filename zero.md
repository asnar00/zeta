ᕦ(ツ)ᕤ
# zero
A concise introduction

## design goals

- approachable: small, readable, organised
- efficient: small/fast code, fast build cycle
- heterogenous target: multiple backends
- distributed, concurrent, real-time
- feature-modular

## organisation

zero is organised like an onion, with three layers: surface, middle, and core.

Broadly:

- In the surface layer, we write code using abstract numeric types (eg. `number`, `int`, `float`);
- In the middle layer, we write code using concrete numeric types (eg. `i32`, `f32`)
- In the core, we define the concrete numeric types using a dependent type system

The intention is that you can write useful programs purely using the surface layer, and you'll never have to dig into the depths.

# surface layer

## types

- `number` : any number
- `int` : any negative or positive integer
- `uint` : any positive integer
- `string` : strings
- `time` : time

## structures

    type vec =
        number x, y, z  = 0

    vec v
    vec v(1, 2, 3)
    vec v = vec(1, 2, 3)
    vec v = vec(z=3, y=2, x=1) 

## functions

Functions are declared using a *named-result* convention, with a free syntax for function signatures:

    on (number r) = min(number a, number b)
        r = a if (a < b) else b

    on (vec r) = (vec a) + (vec b)
        r = vec(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number r) = (vec a) dot (vec b):
        r = a.x * b.x + a.y * b.y + a.z * b.z

## streams

A *stream* is a value that evolves over time; it's like an array in other languages , except that we access it with a `number` index instead of an `int`. 

    int i$                                          # empty integer stream
    int i$ << 0 << 1 << 2 << 3                      # push items one at a time

after this:

        i[0] => 0
        i[1] => 1
        i[2] => 2
        i[3] => 3

We can also do the same thing in other ways:

    int i$ << [0, 1, 2, 3]                          # array
    int i$ << [0 .. 4]                              # range (exclusive)
    int i$ << [0 ..= 3]                             # range (inclusive)
    int i$ << 0 << (i$ + 1) until (i$ == 3)         # until loop
    int i$ << 0 << (i$ + 1) while (i$ < 3)          # while loop

zero has simple syntax for mapping, filtering, reducing and sorting streams:

    int j$ = i$ * 2                                 # map operation with singular value
    int k$ = i$ + j$                                # map operation with array
    int sum = i$ + _                                # reduce using '+'
    int even$ = i$ where (i$ % 2) == 0              # filter
    int descending$ = i$[_ > _]                     # sort

we can also perform linear transformations of the time dimension of the array:

    int delayed$ = i$[_ + 1]                        # add 1 to all timestamps
    int anticipated$ = i$[_ - 1]                    # subtract 1 from all timestamps
    int expanded$ = i$[_ * 2.0]                     # multiply all timestamps by 2
    int squashed$ = i$[_ * 0.5]                     # multiply all timestamps by 2

## tasks

Tasks are like functions, except they generate and operate on streams.

    on (int i$) << count down from (int start)      # named task
        i$ << start << (i$ - 1) until i$ == 0

We can launch tasks that execute at specific update rates:

    i$ << count down from 10                        # [10, 9, 8, ... 0]
    i$ << count down from 10, at 1 hz               # i$[0.0] = 10, i$[1.0] = 9, ...
    i$ << count down from 10, at 10 hz              # i$[0.0] = 10, i$[0.1] = 9, ...
    i$ << count down from 10, at 48 khz             # i$[0.0] = 10; i$[0.00002083] = 9; ...
    i$ << count down from 10, at 1 per hour         # i$[0] = 10; i$[3600] = 9; ...

## features

zero isn't object oriented (structure types can't have methods). Instead, code is organised into *feature clauses*:

    feature Hello
        on hello()
            out$ << "hello world!"
        on main()
            hello()

    feature Intro extends Hello
        before hello()
            countdown()
        on countdown()
            out$ << count down from (10), at 1 hz

    feature Success extends Hello
        after hello()
            success()
        on success()
            out$ << "ᕦ(ツ)ᕤ"

we can then generate different programs by compiling different sets of features:

    context hello_minimal = Hello
    context hello_with_intro = Hello, Intro
    context hello_success = Hello, Success
    context hello_all = Intro, Hello, Success

and then create programs by compiling specific contexts:

    minimal_program = compile(hello_minimal)

# middle layer

The middle layer defines a set of *concrete numeric types* that include implementation details such as bit-depth and memory layout. The compiler chooses which concrete types to substitute for abstract ones.

## types

Concrete numeric types:

We have the usual numeric types we'd expect, in the usual precisions:

- `u8`, `u16`, `u32`, `u64` : unsigned integers
- `i8`, `i16`, `i32`, `i64` : signed integers 
- `f16`, `f32`, `f64` : floating-point numbers

There's also a set of fixed-point number types suited to machine-learning workloads:

- `ufx.8`, `ufx1.7`, `ufx8.8`, ... : unsigned fixed-point numbers
- `fx.8`, `fx1.7`, `fx8.8`,  ... : signed fixed-point numbers

`string` is defined as an alias of `char$`, so you can pass it to functions but also use stream operations on it.

## system

The middle layer lets us define system devices as output streams or input streams:

    output char uart$

    on uart$ << (char c)                            # serial output
        write(c, 0x10000000)

    input char keyboard$                            # keyboard input

    [... more here]

# core layer

The core layer defines the concrete types using dependent types:

    type bit = 0, 1                                 # single bit (0 or 1)

    type uint(N) =                                  # unsigned int
        bit value[N]

    type u32 = uint(32)                             # concrete type as constant

    type int(N) =                                   # signed int
        bit value[N]

    type i32 = int(32)                              # concrete type

    type float(E, F)                                # floating point
        bit sign
        uint(E) exponent
        uint(F) fraction

    type f32 = float(8, 23)                         # concrete types
    type f64 = float(11, 52)

    type fixed(W, F)                                # signed fixed point
        int(W) whole
        uint(F) fraction

    type fx1.3 = fixed(1, 3)                        # and so on
    type fx8.8 = fixed(8, 8)







