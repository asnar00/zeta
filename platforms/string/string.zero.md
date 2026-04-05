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

Replace all occurrences of a needle in a string:

    on (string result) = replace (string needle) in (string s) with (string replacement)

    replace ("world") in ("hello world") with ("zero") => "hello zero"

    substring of ("hello world") from (6) => "world"
    substring of ("abc") from (0) => "abc"

Split a string at the given positions, removing a 2-character separator at each position (the marked character and the one before it):

    on (string result$) = split [string s] at [int positions$]

## tests

Trim handles various whitespace patterns:

    trim ("") => ""
    trim ("  ") => ""
    trim ("no spaces") => "no spaces"
    trim ("  leading") => "leading"
    trim ("trailing  ") => "trailing"

Character access at boundaries:

    char (0) of ("a") => "a"
    char (2) of ("abcde") => "c"

Starts-with edge cases:

    ("") starts with ("") => true
    ("hello") starts with ("") => true
    ("") starts with ("x") => false
    ("abc") starts with ("abc") => true
    ("abc") starts with ("abcd") => false

Split by various delimiters:

    split ("one") by (",") => ["one"]
    split ("a,b") by (",") => ["a", "b"]
    split ("a,,b") by (",") => ["a", "", "b"]

Length of various strings:

    length of ("") => 0
    length of ("a") => 1
    length of ("hello world") => 11

Substring from various positions:

    substring of ("hello") from (0) => "hello"
    substring of ("hello") from (3) => "lo"
    substring of ("hello") from (5) => ""

Replace with various patterns:

    replace ("a") in ("aaa") with ("b") => "bbb"
    replace ("xy") in ("no match") with ("z") => "no match"
    replace ("") in ("hello") with ("x") => "xhxexlxlxox"
