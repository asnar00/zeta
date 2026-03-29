# parser
*zero language processor — translated from parser.py*

## bracket matching

A task that tracks bracket depth as it scans characters:

    on (int depth$) <- bracket depth of (char c$) matching (string pair)
        int d = 0
        char c <- c$
        if (c == pair[0])
            d = d + 1
        else if (c == pair[1])
            d = d - 1
        depth$ <- d

Find the position of the matching bracket:

    on (int pos) = matching (string pair) in (string s) after (int start)
        string sub = s[start onwards]
        int depth$ <- bracket depth of (sub) matching (pair)
        pos = start + index of first in [depth$] where (_ == 0)

## tests

Test bracket matching on the zeta logo:

    on (string result$) <- test brackets ()
        string logo = "ᕦ(ツ)ᕤ"
        int pos = matching ("()") in (logo) after (1)
        if (pos == 3)
            result$ <- "PASS brackets: logo paren at 3"
        else
            result$ <- "FAIL brackets: expected 3"

    on (string out$) <- main (string args$)
        string r <- test brackets ()
        out$ <- r
