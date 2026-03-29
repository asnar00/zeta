# Platform implementation: io (Python)
# Implements the functions declared in io.zero.md


def fn_read_file__string(path: str) -> str:
    with open(path, 'r') as f:
        return f.read()


def fn_write_file__string__string(path: str, content: str):
    with open(path, 'w') as f:
        f.write(content)


def fn_print__string(message: str):
    print(message)
