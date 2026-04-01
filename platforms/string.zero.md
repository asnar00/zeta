# string
*string operations*

## specification

Built-in string functions.

## interface

Remove leading and trailing whitespace from a string:

    on (string result) = trim (string s)

Get the character at position i in a string:

    on (char c) = char (int i) of (string s)

Split a string at the given positions, removing a 2-character separator at each position (the marked character and the one before it):

    on (string result$) = split [string s] at [int positions$]

Check if a string starts with a prefix:

    on (bool result) = (string s) starts with (string prefix)

Split a string by a delimiter:

    on (string result$) = split (string s) by (string delim)

Get the length of a string:

    on (int n) = length of (string s)
