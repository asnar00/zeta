ᕦ(ツ)ᕤ
# Lexer

This is just messing around at the moment; but it's an attempt to write a lexer in literate zero, and see what comes out of instinct.

The lexer breaks a string of characters into a sequence of *lexemes* - each corresponding to a group of characters in the source code.

Each lexeme is shown by as a pair, `type`:`value`.

    feature lexer
        enum token_type = 
            num | number, 
            id | identifier, 
            op | operator, 
            brace, 
            punc | punctuation, 
            eol,
            eof

        type lex | lexeme =
            type : token_type = newline
            val | value : string = ""

To give us useful location feedback, a lexeme also has an index into the code it came from:

    feature source_location extends lexer
        type lex += 
            source[] : char

Notice how there's two ways we could have implemented this:

- as an index only (an integer)
- as a pointer only (direct into the source string)

In zero, however, this is implemented as an array reference (to whatever the original code string was) *and* an index range in that array. The result is effectively a pointer: it's `null` if the index range has zero length, and any `map` operations on it will do nothing. If the range has length one, then it's effectively an index; and if the range is longer, then it's an array with a certain number of elements. The key is that the syntax to do things to it, is always the same. And I think this is elegant.

There's something to be done here with tracking what points to what, and managing lifetimes accordingly. Since everything is a stream in time, it should actually be fairly easy to decide when to retire something.

And to allow fast forward jumps while lexing, we also allow open-brace lexemes to store the lex index of their partner close-brace lexeme:

    feature jump_brace extends lexer
        type lex +=
            partner[] : lex

This again is stored as an array slice, so it's kind of like a pointer, but it's also easy to store nothing, or store the next one in the sequence.

So if we have a code string that looks like this:

    > code : str = "hello (123 \"string\" ++)"

then once we lex it like this:

    > (lex$ : lex) << lexer(code)

we'll see this:

    => [str:"hello", brace:"(", num:"123",  str:"string", op:"++", brace:")"]

and if we print out all the lex jumps as well as the values, we'll see that the open bracket stores a 4-lex jump forward to get to its partnering close-brace:

    > lex$.partner
    => [[], [brace:")"], [], [], [], [], [brace:"("]]

Notice the niceness of the `lex$.partner` formulation! That just returns an array with all the lex jump values.

This is a really nice exercise, because it shows you how literate zero code feels different, in a deep way, to the python code it was based on. Keep going!

## implementation

    feature lexer
        on (lex$ : lex) << lexer (code$ : char)
            lex$ << if (code$ is alphanumeric) then Lex(type: identifier, read (identifer) from (code$))
                    else if ((code$) is (operator)) then lex(type: operator, read (operator) from (code$))
                    else if ((code$) is ("\"")) then (type: string, read string from code$)
                    else if (code$ is ...) then ...
            
We also add a couple of functions to classify characters:

        on (b: bool) = is (c: char) an operator
            b = is c in "+-*/=%^&"

        on (b: bool) = is (c: char) a digit
            b = is c in "0123456789"

And a generic `read` function that steps `code$` forward:

        on (val$ : char) << read (type: token_type) from (code$: char)
            ... something that steps forward code$

And the "..." causes the LLM to kick in and write that code for you? ;-)



