from _runtime import register_tests

def test_parser_0():
    '''matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3'''
    _result = fn_matching__string_in__string_after__int("()", "ᕦ(ツ)ᕤ", 1)
    _expected = 3
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_parser_1():
    '''matching ("[]") in ("a[b[c]d]e") after (1) => 7'''
    _result = fn_matching__string_in__string_after__int("[]", "a[b[c]d]e", 1)
    _expected = 7
    assert _result == _expected, f"expected {_expected}, got {_result}"

register_tests('parser', [(test_parser_0, 'matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3'), (test_parser_1, 'matching ("[]") in ("a[b[c]d]e") after (1) => 7')])

# @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/parser.zero.md:20
def task_bracket_depth_of_matching__char__string(c_arr: str, pair: str):
    d = 0
    for c in c_arr:
        if c == pair[0]:
            d = d + 1
        elif c == pair[1]:
            d = d - 1
        yield d

# @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/parser.zero.md:29
def fn_matching__string_in__string_after__int(pair: str, s: str, start: int) -> int:
    sub = s[start:]
    depth_arr = list(task_bracket_depth_of_matching__char__string(sub, pair))
    pos = start + next(i for i, x in enumerate(depth_arr) if x == 0)
    return pos
