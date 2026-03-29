# @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/parser.zero.md:22
def task_bracket_depth_of_matching__char__string(c_arr: str, pair: str):
    d = 0
    for c in c_arr:
        if c == pair[0]:
            d = d + 1
        elif c == pair[1]:
            d = d - 1
        yield d

# @zero on (string result$) <- test brackets; ziz/parser.zero.md:36
def task_test_brackets():
    logo = "ᕦ(ツ)ᕤ"
    pos = fn_matching__string_in__string_after__int("()", logo, 1)
    if pos == 3:
        yield "PASS brackets: logo paren at 3"
    else:
        yield "FAIL brackets: expected 3"

# @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/parser.zero.md:31
def fn_matching__string_in__string_after__int(pair: str, s: str, start: int) -> int:
    sub = s[start:]
    depth_arr = list(task_bracket_depth_of_matching__char__string(sub, pair))
    pos = start + next(i for i, x in enumerate(depth_arr) if x == 0)
    return pos
