# string
*string operations*

## specification

Built-in string functions for zero programs.

## interface

Remove leading and trailing whitespace from a string:

    on (string result) = trim (string s)

    trim ("  hello  ") => "hello"
    trim ("already") => "already"

Get the character at position i in a string:

    on (char c) = char (int i) of (string s)

    char (0) of ("hello") => "h"
    char (4) of ("hello") => "o"

Check if a string starts with a prefix:

    on (bool result) = (string s) starts with (string prefix)

    ("hello world") starts with ("hello") => true
    ("hello world") starts with ("world") => false

Split a string by a delimiter:

    on (string result$) = split (string s) by (string delim)

    split ("a/b/c") by ("/") => ["a", "b", "c"]
    split ("hello") by ("/") => ["hello"]

Get the length of a string:

    on (int n) = length of (string s)

    length of ("hello") => 5
    length of ("") => 0

Get a substring starting from a position:

    on (string sub) = substring of (string s) from (int start)

    substring of ("hello world") from (6) => "world"
    substring of ("abc") from (0) => "abc"

Split a string at the given positions, removing a 2-character separator at each position (the marked character and the one before it):

    on (string result$) = split [string s] at [int positions$]
