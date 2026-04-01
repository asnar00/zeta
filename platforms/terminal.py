# Platform implementation: terminal (Python)
# Implements the streams declared in terminal.zero.md

import sys


# @zero string out$
# Subscription: each value pushed to out$ prints to stdout
def _push_terminal_out(value: str):
    print(value)


# @zero string in$
# Yields lines from stdin
def terminal_in():
    for line in sys.stdin:
        yield line.rstrip('\n')
