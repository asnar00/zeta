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


import parser

# @zero on (string out$) <- main (string args$); ziz/zeta.zero.md:17
def task_main__string(args_arr: str):
    yield logo
    for r in parser.task_test_brackets():
        yield r

logo: str = "ᕦ(ツ)ᕤ"


# Runtime harness: bridges OS to zero's main task
import sys

if __name__ == "__main__" or True:
    try:
        for line in task_main__string(sys.argv[1:]):
            print(line)
    except NameError:
        pass  # no main task defined
