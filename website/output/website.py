from _runtime import register_tests, run_tests
import not_found
import login
import rpc
import landing_page
import background

# Platform implementation: http (Python)
# Implements the streams and tasks declared in http.zero.md

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import queue


class _Request:
    """Wraps an HTTP request with a channel for the response."""
    def __init__(self, path, method, token=""):
        self.path = path
        self.method = method
        self.token = token
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
            # extract session token from cookie
            token = ""
            cookie_header = self.headers.get("Cookie", "")
            for part in cookie_header.split(";"):
                part = part.strip()
                if part.startswith("session="):
                    token = part[8:]
            req = _Request(self.path, "GET", token)
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

    # keep a reference to the default context
    _default_ctx = None
    import sys as _sys
    _main_mod = _sys.modules.get('__main__')
    if _main_mod and hasattr(_main_mod, '_ctx_var'):
        _default_ctx = _main_mod._ctx_var.get()

    while True:
        req = q.get()
        # switch context: session-specific if token, default otherwise
        if _main_mod and hasattr(_main_mod, '_ctx_var'):
            if req.token:
                sessions = getattr(_main_mod, '_sessions', {}) if _main_mod else {}
                ctx = sessions.get(req.token, _default_ctx)
                _main_mod._ctx_var.set(ctx)
            else:
                _main_mod._ctx_var.set(_default_ctx)
        yield req


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


def _try_get_ctx(mod, name):
    """Try to get a user-scoped variable from the context. Returns value or None."""
    try:
        ctx = mod._ctx_var.get() if hasattr(mod, '_ctx_var') else None
        if ctx is None:
            return None
        if '.' in name:
            feat_name, var_name = name.rsplit('.', 1)
            safe_feat = feat_name.replace("-", "_")
            safe_var = var_name.replace("-", "_")
            section = getattr(ctx, safe_feat, None)
            if section is not None:
                return getattr(section, safe_var, None)
        return None
    except Exception:
        return None


def _try_set_ctx(mod, name, value):
    """Try to set a user-scoped variable on the context. Returns True if successful."""
    try:
        ctx = mod._ctx_var.get() if hasattr(mod, '_ctx_var') else None
        if ctx is None:
            return False
        if '.' in name:
            feat_name, var_name = name.rsplit('.', 1)
            safe_feat = feat_name.replace("-", "_")
            safe_var = var_name.replace("-", "_")
            section = getattr(ctx, safe_feat, None)
            if section is not None and hasattr(section, safe_var):
                setattr(section, safe_var, _coerce_value(value))
                return True
        return False
    except Exception:
        return False


def _find_module(name):
    """Find a module by zero name (hyphenated)."""
    safe = name.replace("-", "_")
    return sys.modules.get(safe)


def _coerce_value(s):
    """Convert a string value to the appropriate Python type for variable assignment."""
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


def _find_function(mod, fn_words, arg_count=0):
    """Find a compiled function in a module by its zero name words and argument count."""
    safe_prefix = "fn_" + fn_words.replace(" ", "_").replace("-", "_")
    # count type separators to match arg count
    def _matches(attr_name):
        if attr_name == safe_prefix:
            return arg_count == 0
        if attr_name.startswith(safe_prefix + "__"):
            type_part = attr_name[len(safe_prefix) + 2:]
            n_types = type_part.count("__") + 1
            return n_types == arg_count
        return False
    # search root module
    for attr_name in dir(mod):
        if _matches(attr_name):
            fn = getattr(mod, attr_name, None)
            if callable(fn):
                return fn
    # also check imported child modules
    for attr_name in dir(mod):
        child = getattr(mod, attr_name, None)
        if hasattr(child, '__name__') and hasattr(child, '__file__'):
            for child_attr in dir(child):
                if _matches(child_attr):
                    fn = getattr(child, child_attr, None)
                    if callable(fn):
                        return fn
    return None


def _get_sessions():
    """Get the shared sessions dict from the root module."""
    mod = _find_root_module()
    if mod is None:
        return {}
    if not hasattr(mod, '_sessions'):
        mod._sessions = {}
    return mod._sessions


# @zero on (string token) = create session ()
def fn_create_session() -> str:
    import uuid
    mod = _find_root_module()
    if mod is None:
        return ""
    token = str(uuid.uuid4())[:8]
    ctx = mod._Context()
    _get_sessions()[token] = ctx
    return token


# @zero on set session (string token)
def fn_set_session__string(token: str):
    mod = _find_root_module()
    if mod is None:
        return
    ctx = _get_sessions().get(token)
    if ctx is not None:
        mod._ctx_var.set(ctx)


# @zero on (string result) = test ()
def fn_test() -> str:
    return _run_tests_captured(None)


# @zero on (string result) = test (string feature)
def fn_test__string(feature: str) -> str:
    return _run_tests_captured([feature])


def _run_tests_captured(names):
    """Run tests and capture output as a string."""
    import io
    from contextlib import redirect_stdout
    try:
        from _runtime import run_tests
        buf = io.StringIO()
        with redirect_stdout(buf):
            run_tests(names)
        return buf.getvalue().rstrip()
    except ImportError:
        return "no tests available"


# @zero on exit process ()
def fn_exit_process():
    import os, threading
    threading.Timer(0.5, lambda: os._exit(0)).start()



def _is_user_var(attr, val):
    """Check if a module attribute is a user-defined variable (not import/platform)."""
    if attr.startswith("_") or attr.startswith("fn_") or attr.startswith("task_"):
        return False
    if callable(val) or isinstance(val, type):
        return False
    if hasattr(val, '__file__'):  # module
        return False
    if isinstance(val, (str, int, float, bool)):
        return True
    return False


def _build_platform_map(mod):
    """Build a map of function attr_name -> platform name from source comments."""
    result = {}
    try:
        import inspect
        source = inspect.getsource(mod)
        lines = source.split("\n")
        current_platform = None
        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped.startswith("# Platform implementation:"):
                current_platform = stripped.split(":")[1].strip().split(" ")[0]
            elif stripped.startswith("# @zero "):
                # check for feature source location — means it's NOT a platform fn
                if "; " in stripped:
                    current_platform = None
                # find the def line
                for j in range(i + 1, min(i + 3, len(lines))):
                    defline = lines[j].strip()
                    if defline.startswith("def fn_") or defline.startswith("def task_"):
                        attr_name = defline.split("(")[0].replace("def ", "").strip()
                        if current_platform:
                            result[attr_name] = current_platform
                        break
    except (OSError, TypeError):
        pass
    return result


def _extract_zero_signatures(mod):
    """Extract @zero source comments from a module's source file."""
    sigs = {}
    try:
        import inspect
        source = inspect.getsource(mod)
        for line in source.split("\n"):
            stripped = line.strip()
            if stripped.startswith("# @zero ") or stripped.startswith("// @zero "):
                sig = stripped.split("@zero ", 1)[1]
                # strip source location suffix
                if "; " in sig:
                    sig = sig[:sig.index("; ")]
                # find the next function def to map sig -> attr name
                continue
        # simpler: scan pairs of comment + def lines
        lines = source.split("\n")
        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped.startswith("# @zero "):
                sig = stripped.split("@zero ", 1)[1]
                if "; " in sig:
                    sig = sig[:sig.index("; ")]
                # find the def on the next non-empty line
                for j in range(i + 1, min(i + 3, len(lines))):
                    defline = lines[j].strip()
                    if defline.startswith("def fn_") or defline.startswith("def task_"):
                        attr_name = defline.split("(")[0].replace("def ", "").strip()
                        sigs[attr_name] = sig
                        break
    except (OSError, TypeError):
        pass
    return sigs


def _build_directory(mod):
    """Build a directory of available features, variables, and functions."""
    lines = []

    # collect child feature modules
    feature_modules = []
    for attr in sorted(dir(mod)):
        child = getattr(mod, attr)
        if hasattr(child, '__file__') and hasattr(child, '__name__') and attr not in ('sys', 're', 'urllib', 'queue', 'threading', 'os'):
            feature_modules.append((attr.replace("_", "-"), child))

    # extract signatures and classify: platform vs user-defined
    root_fns = set()
    zero_sigs = _extract_zero_signatures(mod)
    platform_map = _build_platform_map(mod)
    platform_lines = {}  # platform_name -> [sig_lines]
    user_fn_lines = []
    user_var_lines = []

    for attr in sorted(dir(mod)):
        val = getattr(mod, attr)
        if _is_user_var(attr, val):
            user_var_lines.append(f"  {attr.replace('_', '-')} = {_format_value(val)}")

    for attr in sorted(dir(mod)):
        is_fn = attr.startswith("fn_") and callable(getattr(mod, attr))
        is_task = attr.startswith("task_") and callable(getattr(mod, attr))
        if not is_fn and not is_task:
            continue
        root_fns.add(attr)
        sig = zero_sigs.get(attr, attr)
        plat = platform_map.get(attr)
        if plat:
            platform_lines.setdefault(plat, []).append(f"  {sig}")
        else:
            user_fn_lines.append(f"  {sig}")

    # features first, in composition order (root then children)
    lines.append("website:")
    lines.extend(user_var_lines)
    lines.extend(user_fn_lines)

    for mod_name, m in feature_modules:
        child_sigs = _extract_zero_signatures(m)
        child_lines = []
        for attr in sorted(dir(m)):
            if attr.startswith("fn_") and attr not in root_fns and callable(getattr(m, attr)):
                child_lines.append(f"  {child_sigs.get(attr, attr)}")
        if child_lines:
            lines.append(f"{mod_name}:")
            lines.extend(child_lines)

    # platforms grouped together
    if platform_lines:
        lines.append("platform:")
        for plat_name in sorted(platform_lines):
            lines.append(f"  {plat_name}:")
            lines.extend(f"    {l.strip()}" for l in platform_lines[plat_name])

    return "\n".join(lines) if lines else "empty"


# @zero on (string result) = directory ()
def fn_directory() -> str:
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"
    return _build_directory(mod)


# @zero on (string result) = rpc eval (string expr)
def fn_rpc_eval__string(expr: str) -> str:
    expr = urllib.parse.unquote(expr).strip()
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"

    # empty expression: return directory
    if not expr:
        return fn_directory()

    # assignment: feature.var = value  or  var = value
    assign_match = re.match(r'^([\w.-]+)\s*=\s*(.+)$', expr)
    if assign_match and '(' not in assign_match.group(2):
        name = assign_match.group(1)
        value = assign_match.group(2).strip().strip('"')
        # try context first (for user-scoped vars)
        if '.' in name and _try_set_ctx(mod, name, value):
            return f"{name} = {value}"
        # fall back to module attribute
        if '.' in name:
            mod_name, var_name = name.rsplit('.', 1)
            target = _find_module(mod_name) or mod
        else:
            target = mod
            var_name = name
        attr_name = var_name.replace("-", "_")
        setattr(target, attr_name, _coerce_value(value))
        return f"{name} = {value}"

    # var get: feature.var  or  var  (no parens, no =)
    if '(' not in expr and '=' not in expr:
        name = expr.strip()
        # try context first (for user-scoped vars)
        ctx_val = _try_get_ctx(mod, name)
        if ctx_val is not None:
            return _format_value(ctx_val)
        # fall back to module attribute
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
    fn = _find_function(mod, fn_words, len(args))
    if fn is None:
        return f"error: function '{fn_words}' not found"
    try:
        # pass args as strings — the compiled function handles its own types
        result = fn(*args)
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


# @zero on (string result) = replace (string needle) in (string s) with (string replacement)
def fn_replace__string_in__string_with__string(needle: str, s: str, replacement: str) -> str:
    return s.replace(needle, replacement)


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


import contextvars

class _Context:
    class background:
        colour: str = "#34988b"
    class landing_page:
        enabled: bool = True
        background: str = "#34988b"
    def __init__(self):
        self.background = _Context.background()
        self.landing_page = _Context.landing_page()

_ctx_var: contextvars.ContextVar['_Context'] = contextvars.ContextVar('_ctx', default=_Context())

def _get_ctx() -> '_Context':
    import sys
    _main = sys.modules.get('__main__')
    if _main and hasattr(_main, '_ctx_var'):
        return _main._ctx_var.get()
    return _ctx_var.get()


from typing import NamedTuple

def test_website_0():
    '''trim ("  hello  ") => "hello"'''
    _result = fn_trim__string("  hello  ")
    _expected = "hello"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_1():
    '''trim ("already") => "already"'''
    _result = fn_trim__string("already")
    _expected = "already"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_2():
    '''char (0) of ("hello") => "h"'''
    _result = fn_char__int_of__string(0, "hello")
    _expected = "h"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_3():
    '''char (4) of ("hello") => "o"'''
    _result = fn_char__int_of__string(4, "hello")
    _expected = "o"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_4():
    '''("hello world") starts with ("hello") => true'''
    _result = fn__string_starts_with__string("hello world", "hello")
    _expected = True
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_5():
    '''("hello world") starts with ("world") => false'''
    _result = fn__string_starts_with__string("hello world", "world")
    _expected = False
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_6():
    '''split ("a/b/c") by ("/") => ["a", "b", "c"]'''
    _result = fn_split__string_by__string("a/b/c", "/")
    _expected = ["a", "b", "c"]
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_7():
    '''split ("hello") by ("/") => ["hello"]'''
    _result = fn_split__string_by__string("hello", "/")
    _expected = ["hello"]
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_8():
    '''length of ("hello") => 5'''
    _result = fn_length_of__string("hello")
    _expected = 5
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_9():
    '''length of ("") => 0'''
    _result = fn_length_of__string("")
    _expected = 0
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_10():
    '''replace ("world") in ("hello world") with ("zero") => "hello zero"'''
    _result = fn_replace__string_in__string_with__string("world", "hello world", "zero")
    _expected = "hello zero"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_11():
    '''substring of ("hello world") from (6) => "world"'''
    _result = fn_substring_of__string_from__int("hello world", 6)
    _expected = "world"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_12():
    '''substring of ("abc") from (0) => "abc"'''
    _result = fn_substring_of__string_from__int("abc", 0)
    _expected = "abc"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_13():
    '''handle request (http-request(path="/")) => "ᕦ(ツ)ᕤ"'''
    _result = fn_handle_request__http_request(http_request(path="/"))
    _expected = "ᕦ(ツ)ᕤ"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_website_14():
    '''handle request (http-request(path="/nope")) => "ᕦ(ツ)ᕤ"'''
    _result = fn_handle_request__http_request(http_request(path="/nope"))
    _expected = "ᕦ(ツ)ᕤ"
    assert _result == _expected, f"expected {_expected}, got {_result}"

register_tests('website', [(test_website_0, 'trim ("  hello  ") => "hello"'), (test_website_1, 'trim ("already") => "already"'), (test_website_2, 'char (0) of ("hello") => "h"'), (test_website_3, 'char (4) of ("hello") => "o"'), (test_website_4, '("hello world") starts with ("hello") => true'), (test_website_5, '("hello world") starts with ("world") => false'), (test_website_6, 'split ("a/b/c") by ("/") => ["a", "b", "c"]'), (test_website_7, 'split ("hello") by ("/") => ["hello"]'), (test_website_8, 'length of ("hello") => 5'), (test_website_9, 'length of ("") => 0'), (test_website_10, 'replace ("world") in ("hello world") with ("zero") => "hello zero"'), (test_website_11, 'substring of ("hello world") from (6) => "world"'), (test_website_12, 'substring of ("abc") from (0) => "abc"'), (test_website_13, 'handle request (http-request(path="/")) => "ᕦ(ツ)ᕤ"'), (test_website_14, 'handle request (http-request(path="/nope")) => "ᕦ(ツ)ᕤ"')])

class http_request(NamedTuple):
    path: str = ""
    method: str = ""
    token: str = ""

class http_response(NamedTuple):
    request: http_request = 0
    body: str = ""

class user(NamedTuple):
    name: str = ""
    phone: str = ""
    role: str = ""

# @zero on main (string args$); website/website.zero.md:127
def task_main__string(args_arr: str):
    _push_terminal_out(logo)
    request_arr = task_serve_http__int(port)
    for request in request_arr:
        _push_terminal_out(request.path)
        body = fn_handle_request__http_request(request)
        _push_http_response(http_response(request, body))

# @zero on (string body) = handle request (http-request request); website/website.zero.md:135
def fn_handle_request__http_request(request: http_request) -> str:
    body = None
    if _get_ctx().landing_page.enabled and request.path == "/":
        body = landing_page.fn_landing_page()
    if body is None:
        if fn__string_starts_with__string(request.path, "/@rpc/"):
            expr = fn_substring_of__string_from__int(request.path, 6)
            body = fn_rpc_eval__string(expr)
    if body is None:
        body = not_found.fn_not_found()
    return body if body is not None else ""

# @zero on stop; website/website.zero.md:143
def fn_stop():
    fn_print__string("stopping")

port: int = 8084
logo: str = "ᕦ(ツ)ᕤ"


import sys
if __name__ == '__main__':
    if '--test' in sys.argv:
        _names = [a for a in sys.argv[1:] if a != '--test'] or None
        sys.exit(1 if run_tests(_names) else 0)
    try:
        result = task_main__string(sys.argv[1:])
        if hasattr(result, '__next__'):
            for line in result:
                print(line)
    except NameError:
        pass  # no main task defined
