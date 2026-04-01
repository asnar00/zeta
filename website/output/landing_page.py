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

# @zero on (string body) = landing page; landing_page.zero.md:76
def fn_landing_page() -> str:
    body = fn_read_file__string("website/index.html")
    return body
