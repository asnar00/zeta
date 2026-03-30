from _runtime import register_tests, run_tests
import parser

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


def test_zeta_0():
    '''main(["input.zero.md", "output.py"]) => "ᕦ(ツ)ᕤ"'''
    _result = list(task_main__string(["input.zero.md", "output.py"]))
    _expected = ["ᕦ(ツ)ᕤ"]
    assert _result == _expected, f"expected {_expected}, got {_result}"

register_tests('zeta', [(test_zeta_0, 'main(["input.zero.md", "output.py"]) => "ᕦ(ツ)ᕤ"')])

# @zero on (string out$) <- main (string args$); ziz/zeta.zero.md:27
def task_main__string(args_arr: str):
    yield logo

logo: str = "ᕦ(ツ)ᕤ"


import sys
if __name__ == '__main__':
    if '--test' in sys.argv:
        _names = [a for a in sys.argv[1:] if a != '--test'] or None
        sys.exit(1 if run_tests(_names) else 0)
    try:
        for line in task_main__string(sys.argv[1:]):
            print(line)
    except NameError:
        pass  # no main task defined
