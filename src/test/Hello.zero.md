ᕦ(ツ)ᕤ
# Hello

This is a *feature-normal-form* "hello world" program written in *zero*, a (very) experimental programming language. 

Feature-normal form (`fnf`) is a self-documenting, human-readable format for reading, writing and running code. 

This file explains `fnf` and introduces the syntax of zero, while also providing a test file for the `zeta.py` compiler.

## feature modularity

In zero, the fundamental unit of code modularity is the `feature`. A program is a tree of features, applied in the order they were written. Each feature changes the behaviour of its parent feature.

All code is bound to features; zero doesn't support "objects" in the classic sense, just structures (a bit like C) that are lightweight objects containing no code.

A longer discussion of the motivation for, and potential benefits of, feature modularity can be found [here](../../features.md).

## our first feature clause

The first thing to do is to declare the feature itself, extending the built-in `Main` feature:

    feature Hello extends Main

## specification / test / howto

Our new feature is going to implement a function called `hello` that returns "hello world" when called, like this:

        > hello() 
        => "hello world"

This code snippet defines a *test* : the `>` symbol at the start (mimicking a BASIC prompt from the old microcomputer days) will generate test code on the target platform, and run it. 

The `=>` operator specifies the result that we expect to see; if the result doesn't match, zeta will report a test failure tagged with the source filename and line number.

We can also just call the function without specifying an expected result; this will just output the result to the console, tagged with the file and line number of the code.

        > hello()

## implementation

Now, we'll implement the `hello` function in zero:

        on (out$: string) << hello()
            out$ << "hello world"

Let's unpack this to understand each of the keywords:

`on` defines a new function, but can also extend an existing function (which we'll look at later).

`(out$ : string)` defines the result of the function `hello`. `zero` uses an unusual convention called *named results* - there's no `return` keyword; rather, we supply a name and type for the result variable. (We do this because it simplifies a whole bunch of stuff, as we'll see later on).

The `$` suffix on the `out$` variable indicates that `out` is a *stream* of values (of type `string`) rather than a single value. (We require the `$` suffix in the variable name because that makes it easy to tell, just by looking at a variable, whether it's a stream or a singular value.)

The `<<` operator (called "push", loosely inspired by C++'s stream operator) pushes the result of the function into the stream.

We'll look at streams in more detail in future examples. For now, just think of a stream as an array of items. In this case, a stream of type `string` is just the stand-in for a console.

## layout agnostic

You'll notice that the layout of this code uses significant whitespace like python, but without the traditional `:` at the end of the declaration line. 

The zeta parser is *layout-agnostic* - it accepts code written according to the conventions of multiple languages. (the goal is to make zero programming feel natural for people with lots of muscle-memory of those languages).

For instance, the following are all acceptable alternative forms of `hello`:

C/C++ layout: (`type` then `name` in declarations, braces for indents)

    on (string out$) << hello() {
        out$ << "hello world";
    }

Typescript layout: (`name` : `type` in declarations, braces for indents)

    on (out$ : string) << hello() {
        out$ << "hello world";
    }

Python layout: (`name` : `type` in declarations, `:` and significant whitespace)

    on (out$ : string) << hello():
        out$ << "hello world"

Blended C/C++/Python layout (`type` then `name` in declarations, significant whitespace, no ":")

    on (string out$) << hello()
        out$ << "hello world"

Note that types are mandatory in all of these examples: zero is strongly typed.