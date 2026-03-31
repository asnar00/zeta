from _runtime import register_tests

# Platform implementation: io (Python)
# Implements the functions declared in io.zero.md


# @zero on (string content) = read file (string path)
def fn_read_file__string(path: str) -> str:
    with open(path, 'r') as f:
        return f.read()


# @zero on write file (string path) (string content)
def fn_write_file__string__string(path: str, content: str):
    with open(path, 'w') as f:
        f.write(content)


# @zero on print (string message)
def fn_print__string(message: str):
    print(message)


# Platform implementation: string (Python)
# Implements the functions declared in string.zero.md


# @zero on (string result) = trim (string s)
def fn_trim__string(s: str) -> str:
    return s.strip()


# @zero on (char c) = char (int i) of (string s)
def fn_char__int_of__string(i: int, s: str) -> str:
    return s[i]


# @zero on (string result$) = split [string s] at [int positions$]
def fn_split_at(s: str, positions: list[int]) -> list[str]:
    parts = []
    start = 0
    for pos in positions:
        parts.append(s[start:pos - 1])
        start = pos + 1
    remainder = s[start:]
    if remainder:
        parts.append(remainder)
    return parts


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

def test_parser_2():
    '''split stream parts ("1 <- 2 <- 3") => ["1", "2", "3"]'''
    _result = fn_split_stream_parts__string("1 <- 2 <- 3")
    _expected = ["1", "2", "3"]
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_parser_3():
    '''split stream parts ("1 <- (a <- b) <- 3") => ["1", "(a <- b)", "3"]'''
    _result = fn_split_stream_parts__string("1 <- (a <- b) <- 3")
    _expected = ["1", "(a <- b)", "3"]
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_parser_4():
    '''split stream parts ("hello") => ["hello"]'''
    _result = fn_split_stream_parts__string("hello")
    _expected = ["hello"]
    assert _result == _expected, f"expected {_expected}, got {_result}"

register_tests('parser', [(test_parser_0, 'matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3'), (test_parser_1, 'matching ("[]") in ("a[b[c]d]e") after (1) => 7'), (test_parser_2, 'split stream parts ("1 <- 2 <- 3") => ["1", "2", "3"]'), (test_parser_3, 'split stream parts ("1 <- (a <- b) <- 3") => ["1", "(a <- b)", "3"]'), (test_parser_4, 'split stream parts ("hello") => ["hello"]')])

# @zero on (int depth$) <- bracket depth of matching (string s) (string pair); ziz/parser.zero.md:33
def task_bracket_depth_of_matching__string__string(s: str, pair: str):
    d = 0
    for c in list(s):
        if c == fn_char__int_of__string(0, pair):
            d = d + 1
        elif c == fn_char__int_of__string(1, pair):
            d = d - 1
        yield d

# @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/parser.zero.md:42
def fn_matching__string_in__string_after__int(pair: str, s: str, start: int) -> int:
    sub = s[start:]
    depth_arr = list(task_bracket_depth_of_matching__string__string(sub, pair))
    pos = start + next(i for i, x in enumerate(depth_arr) if x == 0)
    return pos

# @zero on (string part$) = split stream parts (string s); ziz/parser.zero.md:47
def fn_split_stream_parts__string(s: str) -> str:
    padded = s + "<-"
    depth_arr = list(task_bracket_depth_of_matching__string__string(padded, "()"))
    lt_arr = [fn_char__int_of__string(_i, padded) == "<" for _i in range(len(padded))]
    is_sep_arr = [(lt_arr[_i - 1] if _i - 1 >= 0 else 0) and fn_char__int_of__string(_i, padded) == "-" and depth_arr[_i] == 0 for _i in range(len(depth_arr))]
    pos_arr = [i for i, x in enumerate(is_sep_arr) if x]
    part_arr = [fn_trim__string(x) for x in fn_split_at(padded, pos_arr)]
    return part_arr
