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
        string sub = s[start:]
        int depth$ <- bracket depth of (sub) matching (pair)
        pos = start + index of first in [depth$] where (_ == 0)
