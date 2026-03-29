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


# @zero on (string out$) <- main (string args$); ziz/zeta.zero.md + ziz/parser.zero.md:16
def task_main__string(args_arr: str):
    yield logo
    for r in task_test_brackets():
        yield r

# @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/zeta.zero.md + ziz/parser.zero.md:21
def task_bracket_depth_of_matching__char__string(c_arr: str, pair: str):
    d = 0
    for c in c_arr:
        if c == pair[0]:
            d = d + 1
        elif c == pair[1]:
            d = d - 1
        yield d

# @zero on (string result$) <- test brackets; ziz/zeta.zero.md + ziz/parser.zero.md:35
def task_test_brackets():
    logo = "ᕦ(ツ)ᕤ"
    pos = fn_matching__string_in__string_after__int("()", logo, 1)
    if pos == 3:
        yield "PASS brackets: logo paren at 3"
    else:
        yield "FAIL brackets: expected 3"

# @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/zeta.zero.md + ziz/parser.zero.md:30
def fn_matching__string_in__string_after__int(pair: str, s: str, start: int) -> int:
    sub = s[start:]
    depth_arr = list(task_bracket_depth_of_matching__char__string(sub, pair))
    pos = start + next(i for i, x in enumerate(depth_arr) if x == 0)
    return pos

logo: str = "ᕦ(ツ)ᕤ"


# Runtime harness: bridges OS to zero's main task
import sys

if __name__ == "__main__" or True:
    try:
        for line in task_main__string(sys.argv[1:]):
            print(line)
    except NameError:
        pass  # no main task defined
