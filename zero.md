# zero
*a programming language*

`zero` (always lowercase) is a feature-modular programming language. Programs are organised as trees of *features*, which can be composed in different combinations to yield different *products*.

## feature.md

A zero feature is written in a `.md` file with the following layout:

- a 1-3 word `# title`
- a one-line `*summary*`
- a natural-language `## specification`, describing why the feature exists and what it does, in 500 words or less
- an `## interface` describing the externally callable functions that the feature defines, using examples of when and how to call them, in a *literate style* : each example is a natural language explanation line or paragraph followed by a code block
- a `## definition`, defining types, feature-scope variables, and functions, also in a literate style (each code block is preceded by a natural-language line or paragraph introducing it)
- optionally, a `## tests` section defining tests

### interface

The interface consists of a list of publically visible functions that the feature defines. Each one is introduced by a paragraph describing what it does, followed by literate examples of input/output, separated using the symbol `=>`, as follows:

The `add` function adds two numbers together:

    add(4, 3) => 7

### definition

The definition is literate zero (text paragraphs introducing types, feature-scope variables, and functions). The convention is that each function lives in a separate code-block with its own introduction.

### tests

The tests section specifies more exhaustive tests, using the same literate format as the interface. The idea is that the interface provides a minimal number of examples, and the tests section rounds them out.

# language

The following section introduces the zero language.

## agent instructions

The `...` keyword followed a natural-language phrase acts as a *compile-time agent instruction* - a coding agent can generate code at its discretion, using the phrase and the surrounding context as a guide.

## types

zero provides a `string` type that works how you'd expect.

*concrete numeric types* specify numeric types with defined formats and sizes, for example:

    type int32 = ... 32-bit signed integer
    type uint8 = ... 8-bit signed integer
    type float32 = ... 32-bit IEEE floating-point number

In general, you can use any combination eg. `int8`, `float16`, as appropriate.

*abstract numeric types* are used when we don't care about the size or format, allowing the compiler to make the choice:

    type int = ... any-size signed integer
    type uint = ... any-size unsigned integer
    type float = ... any-size IEEE floating-point number
    type number = int | float
    type bool = ... true or false

*enumerations* are types which take one of a range of values, each specified by a single word:

    type tri-state = no | yes | maybe

Finally, *structure types* are bundles of values, each of which may be either a concrete or an abstract type, and allow a default value to be declared:

    type vector = 
        number x, y, z = 0

## type composition

A type can be defined as the composition of an existing type plus additional fields using `+`:

    type animal

    type dog = animal +
        string breed

    type cat = animal +
        int lives = 9

Here, `dog` and `cat` are both subtypes of `animal`. A type with no fields (like `animal`) is an abstract base type.

Multiple types can be composed together:

    type pet = animal + named +
        string owner

When a function has multiple definitions for different subtypes, the most specific version is called:

    on (string s) = describe (animal a)
        s = "an animal"

    on (string s) = describe (dog d)
        s = "a dog, breed: " + d.breed

Calling `describe` with a `dog` uses the specific version; calling it with a `cat` or any other `animal` subtype uses the fallback.

## variables

zero is strongly typed; variables are declared using C-style `type name` pairs followed by an assignment, for example:

    int32 i = 10
    float f = 1.02

Structure types are initialised using C-style constructors, but these don't have to be declared separately; they also support named members out of order.

    vector v = vector()                 # x=0, y=0, z=0
    vector v = vector(1, 2, 3)          # x=1, y=2, z=3
    vector v = vector(z=2, x=1)         # x=1, y=0, z=2

## functions

Functions have *open syntax* - a function prototype can be any sequence of words or symbols, with typed parameters in brackets (at least one set, even if there are no parameters) anywhere in the sequence.

Also, functions use a *named result* pattern - instead of a `return` keyword, we specify the result as a named variable, and return a value by assigning it.

The following are legal zero function definitions:

    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)

Functions must be *pure* - they cannot write back to their inputs, and *single-assignment* - a variable can only be initialised with values, never re-assigned.

## conditional blocks

In functions, inline ternaries work for simple cases:

    n = (a) if (a < b) else (b)

Conditions can be combined using `and` and `or`:

    bool valid = (x > 0 and x < 100)
    bool found = (name == "alice" or name == "bob")

For multi-branch logic, use `if`, `else if`, and `else` blocks. In pure functions, each branch must assign the result:

    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else if (n < 0)
            s = "negative"
        else
            s = "zero"

## arrays

Arrays are declared by suffixing the variable name with `$` - this makes it easy to look at a piece of code and understand which variables are singular, and which are arrays.

There are many ways of declaring array variables:

    int i$                      # i$ = []
    int i$[4]                   # i$ = [0, 0, 0, 0]
    int i$[4] = 1               # i$ = [1, 1, 1, 1]
    int i$ = [1, 2, 3, 4]       # i$ = [1, 2, 3, 4]
    int i$ = [1 through 4]      # i$ = [1, 2, 3, 4]
    int i$ = [0 to 4]           # i$ = [0, 1, 2, 3]

A string can be viewed as a character array by adding the `$` suffix:

    string name = "hello"
    char c$ = name$                 # c$ = ['h', 'e', 'l', 'l', 'o']

This works anywhere an array is expected — in function mapping, streaming, and array operations. `name` is the string, `name$` is the same data viewed as `char$`.

## applying functions to arrays

Functions cannot be declared with array parameters; however, passing an array to a function maps the function to each element of the array.

For example:

    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2             # j$ = [2, 4, 6, 8]

When mapping to multiple arrays, the arrays should be the same length; if the arrays are of different length, a default value of 0 (or the equivalent for the type) is used, and the resulting array has the same length as the longer of the two:

    int i$ = [1, 2, 3, 4, 5]
    int j$ = [1, 4, 7]
    int k$ = i$ + j$            # k$ = [2, 6, 10, 4, 5]

## reducing arrays using functions

The `_` symbol refers to the *current element* when operating on arrays. When used with an array and a function, it reduces the array to a single value:

    int i$ = [1, 2, 3, 4]
    int sum = i$ + _                    # sum = 1 + 2 + 3 + 4 = 10
    int min = smaller of (i$) and (_)   # min = 1

When `_` is used as an array index, it refers to the *current element's position*, allowing relative references:

    int i$ = [10, 20, 30, 40]
    int prev$ = i$[_ - 1]              # prev$ = [0, 10, 20, 30]

Out-of-bounds index references return the default value for the type (0 for numbers, "" for strings, false for bools).

## filtering arrays

The `where` keyword filters an array by a condition on its elements:

    type person =
        string name
        int age = 0

    person people$ = [...]
    person adults$ = [people$] where (_.age >= 18)
    person bob$ = [people$] where (_.name == "bob")
    person first bob = first of [people$] where (_.name == "bob")
    bool has minors = any of [people$] where (_.age < 18)
    int adult count = count of [people$] where (_.age >= 18)

Inside the `where` clause, `_` refers to the current element being tested.

## sorting arrays

The `sort` array function sorts an array. For simple arrays, no key is needed:

    int sorted$ = sort [i$]

For structs, use `by` with an expression using `_` for the current element:

    person sorted$ = sort [people$] by (_.age)
    person sorted$ = sort [people$] by (_.age) descending

The sort key can be any expression, including function calls:

    vector nearest$ = sort [vectors$] by (distance from (_) to (origin))

## array slicing

The *slice operator* `[i to j]` selects a range of elements from an array and returns it as an array. The slice operator is safe, because it always returns an array, even if the indices exceed the array length.

    int i$ = [1, 2, 3, 4]
    int j$ = i$[1 to 3]             # j$ = [2, 3]
    int k$ = i$[1 through 3]        # k$ = [2, 3, 4]
    int l$ = i$[0 to 10]            # l$ = [1, 2, 3, 4]

## array functions

*array functions* are a special type of function that operate on arrays; they are invoked by specifying the parameters in square brackets `[]` instead of normal brackets `()`. This means you can always tell them apart by looking at the caller code.

Examples of calling array functions:

    int i$ = [1, 2, 3, 4]
    int len = length of [i$]            # len = 4
    bool b = [i$] starts with [1]       # b = true
    int j$$ = split [i$] at [3]         # j$$ = [[1, 2], [4]]
    int k$ = join [j$$] with [7]        # k$ = [1, 2, 7, 4]

Array functions are defined using square brackets in the parameter list. Parameters without a type are generic — the type is inferred from the call site:

    on (bool result) = [items$] starts with [prefix$]
        result = ((items$[0 to length of [prefix$]]) == prefix$) & ..

## streaming

The *streaming operator* `<-` adds an element onto the end of an array:

    int i$ <- 1 <- 2 <- 3 <- 4          # i$ = [1, 2, 3, 4]

Using the target variable on the right-hand-side of a streaming operation, the most recent value of the stream is used:

    int i$ <- 1 <- (i$ + 1)             # i$ = [1, 2]

The keywords `while` and `until` repeat a streaming operation until some termination condition is achieved:

    int i$ <- 1 <- (i$ + 1) until (i$==4)   # i$ = [1, 2, 3, 4]

    int i$ <- 0 <- (i$ + 1) while (i$ < 4)  # i$ = [0, 1, 2, 3]

## tasks

A *task* is like a function, except it is defined using the `<-` operator, and operates on streams. From the outside, a task is pure — it takes inputs and produces a stream, and can be used anywhere a stream is expected. Inside, a task can use imperative constructs: mutable variables, conditional blocks, and sequential logic.

Tasks can produce streams from scalar inputs:

    on (int i$) <- count down from (int n)
        i$ <- n <- (i$ - 1) while (i$ > 1)

    int i$ <- count down from (4)           # i$ = [4, 3, 2, 1]

Tasks can iterate over input streams using `for each`. Variables declared above the loop persist across iterations (mutable state). The loop body runs once per element, and can emit zero or more values to the output stream:

    on (int depth$) <- bracket depth of (char c$) matching (string pair)
        int d = 0
        for each (c) in (c$)
            if (c == pair[0])
                d = d + 1
            else if (c == pair[1])
                d = d - 1
            depth$ <- d

    on (string label$) <- classify (int numbers$)
        for each (n) in (numbers$)
            if (n > 0)
                label$ <- "positive"
            else if (n < 0)
                label$ <- "negative"
            else
                label$ <- "zero"

The distinction: `for each (x) in (stream$)` iterates over a stream. Variables above the loop are initialised once; variables inside are per-iteration.

## concurrency

In zero, we can specify actions that take place concurrently using the `concurrently` and `and` keywords:

    concurrently
        print("hello")
    and
        beep()

Any number of `and` blocks can be put together into one construct.

## feature extension

A feature can define itself to be an extension of any existing feature using the form `feature <X> extends <Y>`, for example:

    feature bike-routes extends map-routes

A feature can extend the type definition of any existing type using `+=`:

    type colour +=
        number alpha = 1

A feature can also extend the definition of any function using the keywords `in`, `replace`, `before` and `after`:

To show how this works, we'll use the time-honoured "hello world" example, introducing a language feature called "define-and-call" to simultaneously define and call a function called `hello`:

    feature Hello
    on run()
        hello()

    on hello()
        print "hello world"

This would yield the following behaviour:

    >run()
    hello world

Then we extend this by adding a `countdown` before the `hello` message:

    feature Countdown extends Hello
    in run(), before hello()
        count down()
    on count down()
        print [10 through 1], once per second

yielding the following behaviour:

    >run()
    10 9 8 7 6 5 4 3 2 1
    hello world

Then we extend this by printing "goodbye" after everything is finished:

    feature Bye extends Hello
    on goodbye

    in run(), after hello()
        goodbye()

    on goodbye()
        print "bye!"

yielding the following behaviour:

    >run()
    10 9 8 7 6 5 4 3 2 1
    hello world
    bye!

Finally, we add an audio beep at the same time that we print "hello world":

    feature Beep extends Hello
    in run(), on hello()
        beep()

    on beep()
        play audio ("beep.wav")

yielding the following behaviour:

    >run()
    10 9 8 7 6 5 4 3 2 1
    hello world <beep>
    bye!

This code gives us four features: `Hello`, and three extensions `Countdown`, `Goodbye` and `Beep`, which can be enabled or disabled separately.

