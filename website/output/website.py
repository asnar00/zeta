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

import re
import sys
import urllib.parse


def _find_root_module():
    """Find the root website module (the one with __main__ entry point)."""
    # check __main__ first (when run directly)
    main = sys.modules.get('__main__')
    if main and hasattr(main, 'task_main__string'):
        return main
    # fallback: search all modules
    for name, mod in sys.modules.items():
        if hasattr(mod, 'task_main__string') and name != __name__:
            return mod
    return None


def _find_module(name):
    """Find a module by zero name (hyphenated)."""
    safe = name.replace("-", "_")
    return sys.modules.get(safe)


def _coerce_value(s):
    """Convert a string value to the appropriate Python type."""
    if s in ("true", "false"):
        return s == "true"
    try:
        return int(s)
    except ValueError:
        try:
            return float(s)
        except ValueError:
            return s


def _format_value(val):
    """Format a Python value as a string for RPC response."""
    if val is None:
        return "ok"
    if isinstance(val, bool):
        return "true" if val else "false"
    return str(val)


def _extract_args(s):
    """Extract parenthesized arguments from a zero call string.
    'set feature var ("landing-page-enabled") ("false")' → ["landing-page-enabled", "false"]
    """
    args = []
    i = 0
    while i < len(s):
        if s[i] == '(':
            depth = 1
            start = i + 1
            i += 1
            while i < len(s) and depth > 0:
                if s[i] == '(':
                    depth += 1
                elif s[i] == ')':
                    depth -= 1
                i += 1
            inner = s[start:i - 1].strip()
            # skip empty parens (zero-arg calls)
            if inner == "":
                continue
            # strip quotes from string args
            if inner.startswith('"') and inner.endswith('"'):
                inner = inner[1:-1]
            args.append(inner)
        else:
            i += 1
    return args


def _extract_fn_words(s):
    """Extract the function name words (everything before the first parenthesized arg).
    'set feature var ("x") ("y")' → 'set feature var'
    'stop ()' → 'stop'
    """
    paren_pos = s.find('(')
    if paren_pos == -1:
        return s.strip()
    return s[:paren_pos].strip()


def _find_function(mod, fn_words):
    """Find a compiled function in a module by its zero name words."""
    # build expected Python function name: fn_word1_word2__type1__type2
    # we don't know the types, so search by prefix
    safe_prefix = "fn_" + fn_words.replace(" ", "_").replace("-", "_")
    for attr_name in dir(mod):
        if attr_name == safe_prefix or attr_name.startswith(safe_prefix + "__"):
            fn = getattr(mod, attr_name, None)
            if callable(fn):
                return fn
    # also check imported child modules
    for attr_name in dir(mod):
        child = getattr(mod, attr_name, None)
        if hasattr(child, '__name__') and hasattr(child, '__file__'):
            for child_attr in dir(child):
                if child_attr == safe_prefix or child_attr.startswith(safe_prefix + "__"):
                    fn = getattr(child, child_attr, None)
                    if callable(fn):
                        return fn
    return None


# @zero on exit process ()
def fn_exit_process():
    import os, threading
    threading.Timer(0.5, lambda: os._exit(0)).start()



# @zero on (string result) = rpc eval (string expr)
def fn_rpc_eval__string(expr: str) -> str:
    expr = urllib.parse.unquote(expr).strip()
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"

    # assignment: module.var = value  or  var = value
    assign_match = re.match(r'^([\w.-]+)\s*=\s*(.+)$', expr)
    if assign_match and '(' not in assign_match.group(2):
        name = assign_match.group(1)
        value = assign_match.group(2).strip().strip('"')
        # resolve module.var
        if '.' in name:
            mod_name, var_name = name.rsplit('.', 1)
            target = _find_module(mod_name) or mod
        else:
            target = mod
            var_name = name
        attr_name = var_name.replace("-", "_")
        setattr(target, attr_name, _coerce_value(value))
        return f"{name} = {value}"

    # var get: module.var  or  var  (no parens, no =)
    if '(' not in expr and '=' not in expr:
        name = expr.strip()
        if '.' in name:
            mod_name, var_name = name.rsplit('.', 1)
            target = _find_module(mod_name) or mod
        else:
            target = mod
            var_name = name
        attr_name = var_name.replace("-", "_")
        val = getattr(target, attr_name, None)
        if val is not None:
            return _format_value(val)
        return f"error: {name} not found"

    # function call: fn name ("arg1") ("arg2")  or  fn name ()
    fn_words = _extract_fn_words(expr)
    args = _extract_args(expr)
    fn = _find_function(mod, fn_words)
    if fn is None:
        return f"error: function '{fn_words}' not found"
    try:
        coerced = [_coerce_value(a) for a in args]
        result = fn(*coerced)
        return _format_value(result)
    except Exception as e:
        return f"error: {e}"


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


# @zero on (string sub) = substring of (string s) from (int start)
def fn_substring_of__string_from__int(s: str, start: int) -> str:
    return s[start:]


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

# @zero on main (string args$); website/website.zero.md:79
def task_main__string(args_arr: str):
    _push_terminal_out(logo)
    request_arr = task_serve_http__int(port)
    for request in request_arr:
        _push_terminal_out(request.path)
        body = fn_handle_request__http_request(request)
        _push_http_response(http_response(request, body))

# @zero on (string body) = handle request (http-request request); website/website.zero.md:87
def fn_handle_request__http_request(request: http_request) -> str:
    body = None
    if landing_page_enabled and request.path == "/":
        body = landing_page.fn_landing_page()
    if body is None:
        if fn__string_starts_with__string(request.path, "/@rpc/"):
            expr = fn_substring_of__string_from__int(request.path, 6)
            body = fn_rpc_eval__string(expr)
    if body is None:
        body = not_found.fn_not_found()
    return body

# @zero on stop; website/website.zero.md:95
def fn_stop():
    fn_print__string("stopping")

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
