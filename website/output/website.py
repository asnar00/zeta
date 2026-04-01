import not_found
import admin
import landing_page

# Platform implementation: http (Python)
# Implements the streams and tasks declared in http.zero.md

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import queue


class _Request:
    """Wraps an HTTP request with a channel for the response."""
    def __init__(self, path, method):
        self.path = path
        self.method = method
        self._response = queue.Queue()

    def _send(self, body):
        self._response.put(body)

    def _wait(self):
        return self._response.get()


# @zero on (http_request request$) <- serve http (int port)
def task_serve_http__int(port):
    q = queue.Queue()

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            req = _Request(self.path, "GET")
            q.put(req)
            body = req._wait()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(body.encode("utf-8"))

        def log_message(self, format, *args):
            pass  # silence default logging

    server = HTTPServer(("", port), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()

    while True:
        yield q.get()


# @zero http.response$
# Routes the response back to the correct client via the paired request
def _push_http_response(response):
    response.request._send(response.body)


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


# Platform implementation: runtime (Python)
# Implements the functions declared in runtime.zero.md

import sys


def _find_root_module():
    """Find the root website module (the one with __main__ entry point)."""
    for name, mod in sys.modules.items():
        if hasattr(mod, 'task_main__string') and name != __name__:
            return mod
    return None


# @zero on set feature var (string name) (string value)
def fn_set_feature_var__string__string(name: str, value: str):
    mod = _find_root_module()
    if mod is None:
        return
    # convert string value to appropriate type
    if value in ("true", "false"):
        setattr(mod, name, value == "true")
    elif value.isdigit():
        setattr(mod, name, int(value))
    else:
        setattr(mod, name, value)


# @zero on (string value) = get feature var (string name)
def fn_get_feature_var__string(name: str) -> str:
    mod = _find_root_module()
    if mod is None:
        return ""
    val = getattr(mod, name, None)
    if val is None:
        return ""
    if isinstance(val, bool):
        return "true" if val else "false"
    return str(val)


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


# @zero on (bool result) = (string s) starts with (string prefix)
def fn__string_starts_with__string(s: str, prefix: str) -> bool:
    return s.startswith(prefix)


# @zero on (string result$) = split (string s) by (string delim)
def fn_split__string_by__string(s: str, delim: str) -> list[str]:
    return s.split(delim)


# @zero on (int n) = length of (string s)
def fn_length_of__string(s: str) -> int:
    return len(s)


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


from typing import NamedTuple

class http_request(NamedTuple):
    path: str = ""
    method: str = ""

class http_response(NamedTuple):
    request: http_request = 0
    body: str = ""

# @zero on main (string args$); website/website.zero.md:76
def task_main__string(args_arr: str):
    _push_terminal_out(logo)
    request_arr = task_serve_http__int(port)
    for request in request_arr:
        _push_terminal_out(request.path)
        body = fn_handle_request__http_request(request)
        _push_http_response(http_response(request, body))

# @zero on (string body) = handle request (http_request request); website/website.zero.md:84
def fn_handle_request__http_request(request: http_request) -> str:
    body = None
    if landing_page_enabled and request.path == "/":
        body = landing_page.fn_landing_page()
    if body is None:
        if fn__string_starts_with__string(request.path, "/@admin/"):
            body = admin.fn_handle_admin__http_request(request)
    if body is None:
        body = not_found.fn_not_found()
    return body

port: int = 8084
logo: str = "ᕦ(ツ)ᕤ"
landing_page_enabled: bool = True


import sys
if __name__ == '__main__':
    try:
        result = task_main__string(sys.argv[1:])
        if hasattr(result, '__next__'):
            for line in result:
                print(line)
    except NameError:
        pass  # no main task defined
