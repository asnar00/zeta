# parser
*zero language processor — translated from parser.py*

## specification

Parser functions for the zero translator, starting with bracket matching.

## interface

The `matching` function finds the position of a closing bracket in a string:

    matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3
    matching ("[]") in ("a[b[c]d]e") after (1) => 7

The `split stream parts` task splits a stream expression at top-level `<-` tokens:

    split stream parts ("1 <- 2 <- 3") => ["1", "2", "3"]
    split stream parts ("1 <- (a <- b) <- 3") => ["1", "(a <- b)", "3"]
    split stream parts ("hello") => ["hello"]

## definition

    feature parser extends zeta

### bracket matching

A task that tracks bracket depth as it scans characters:

    on (int depth$) <- bracket depth of (string s) matching (string pair)
        int d = 0
        for each (c) in (s.char$)
            if (c == char (0) of (pair))
                d = d + 1
            else if (c == char (1) of (pair))
                d = d - 1
            depth$ <- d

Find the position of the matching bracket:

    on (int pos) = matching (string pair) in (string s) after (int start)
        string sub = s[start onwards]
        int depth$ <- bracket depth of (sub) matching (pair)
        pos = start + index of first in [depth$] where (_ == 0)

### split stream parts

Splits a stream expression at top-level `<-` tokens, respecting parenthesis depth. Decomposed into three parallel streams, then sliced at separator positions:

    on (string part$) = split stream parts (string s)
        string padded = s + "<-"
        int depth$ <- bracket depth of (padded) matching ("()")
        bool lt$ <- (char (_) of (padded) == "<")
        bool is_sep$ <- (lt$[_ - 1] and char (_) of (padded) == "-" and depth$ == 0)
        int pos$ = indices of [is_sep$] where (_)
        string part$ = trim (split [padded] at [pos$])