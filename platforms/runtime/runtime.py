# Platform implementation: runtime (Python)
# Implements the functions declared in runtime.zero.md

import re
import sys
import urllib.parse

# @shared-runtime-start
_test_registry = {}

def register_tests(feature, tests):
    _test_registry[feature] = tests

def _call_expr(desc):
    """Extract the call expression (LHS of =>) from a test description."""
    if " => " in desc:
        return desc[:desc.index(" => ")].strip()
    return desc

def run_tests(names=None):
    results = []
    for feat, tests in _test_registry.items():
        if names and feat not in names:
            continue
        for fn, desc in tests:
            try:
                fn()
                results.append((feat, desc, True, None))
            except Exception as e:
                results.append((feat, desc, False, str(e).split("\n")[0]))
    passing_calls = set()
    for feat, desc, passed, err in results:
        if passed:
            passing_calls.add(_call_expr(desc))
    total_fail = 0
    current_feat = None
    passed = failed = overridden = 0
    for feat, desc, ok, err in results:
        if feat != current_feat:
            if current_feat is not None:
                _print_feat_summary(current_feat, passed, failed, overridden)
            current_feat = feat
            passed = failed = overridden = 0
            print(f"{feat}:")
        if ok:
            passed += 1
            print(f"  PASS {desc}")
        elif _call_expr(desc) in passing_calls:
            overridden += 1
            print(f"  OVERRIDDEN {desc}")
        else:
            failed += 1
            total_fail += 1
            print(f"  FAIL {desc}: {err}")
    if current_feat is not None:
        _print_feat_summary(current_feat, passed, failed, overridden)
    return total_fail

def _print_feat_summary(feat, passed, failed, overridden):
    parts = [f"{passed} passed"]
    if failed:
        parts.append(f"{failed} failed")
    if overridden:
        parts.append(f"{overridden} overridden")
    print(f"{feat}: {', '.join(parts)}")
# @shared-runtime-end


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
                _save_sessions()
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


def _try_match_call(mod, expr):
    """Try to match expr against all known function signatures.
    Handles method-style calls like (s) contains (sub)."""
    sigs = _extract_zero_signatures(mod)
    # also check child modules
    for attr in dir(mod):
        child = getattr(mod, attr, None)
        if hasattr(child, '__name__') and hasattr(child, '__file__'):
            sigs.update(_extract_zero_signatures(child))
    for fn_attr, proto in sigs.items():
        fn, args = _try_match_proto(expr, proto, fn_attr, mod)
        if fn is not None:
            return fn, args
    return None, None


def _try_match_proto(expr, proto, fn_attr, mod):
    """Try to match an expression against a zero prototype string.
    Returns (fn, args) or (None, None)."""
    # parse prototype into word/param pattern
    # e.g. "on (bool result) = (string s) contains (string substring)"
    # becomes pattern: [("param",), ("word", "contains"), ("param",)]
    import re
    rhs = proto
    # strip "on " prefix and result declaration
    if rhs.startswith("on "):
        rhs = rhs[3:]
    result_match = re.match(r"\([^)]+\)\s*=\s*(.*)", rhs)
    if result_match:
        rhs = result_match.group(1).strip()
    elif rhs.startswith("(") and "=" in rhs:
        return None, None
    pattern = []
    remaining = rhs.strip()
    while remaining:
        remaining = remaining.strip()
        if not remaining:
            break
        param_match = re.match(r"\(\w[\w\s-]*\s+\w[\w-]*\$?\)", remaining)
        if param_match:
            pattern.append(("param",))
            remaining = remaining[param_match.end():]
            continue
        word_match = re.match(r"(\w[\w-]*)", remaining)
        if word_match:
            pattern.append(("word", word_match.group(1)))
            remaining = remaining[word_match.end():]
            continue
        break
    if not pattern:
        return None, None
    # try to match expr against this pattern
    args = []
    pos = 0
    s = expr.strip()
    for item in pattern:
        while pos < len(s) and s[pos] == " ":
            pos += 1
        if pos >= len(s) and item[0] == "word":
            return None, None
        if item[0] == "word":
            word = item[1]
            if not s[pos:].startswith(word):
                return None, None
            end = pos + len(word)
            if end < len(s) and s[end] not in " (":
                return None, None
            pos = end
        elif item[0] == "param":
            if pos >= len(s) or s[pos] != "(":
                return None, None
            depth = 1
            start = pos + 1
            pos += 1
            while pos < len(s) and depth > 0:
                if s[pos] == "(": depth += 1
                elif s[pos] == ")": depth -= 1
                pos += 1
            inner = s[start:pos - 1].strip()
            if inner.startswith('"') and inner.endswith('"'):
                inner = inner[1:-1]
            args.append(inner)
    # check we consumed the whole expression
    remaining = s[pos:].strip()
    if remaining and remaining != "()":
        return None, None
    # find the function
    fn = getattr(mod, fn_attr, None)
    if fn is None:
        for attr in dir(mod):
            child = getattr(mod, attr, None)
            if hasattr(child, '__name__') and hasattr(child, '__file__'):
                fn = getattr(child, fn_attr, None)
                if fn and callable(fn):
                    break
    if fn and callable(fn):
        return fn, args
    return None, None


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


def _sessions_path():
    """Path to the sessions persistence file."""
    import os
    mod = _find_root_module()
    if mod and hasattr(mod, '__file__'):
        return os.path.join(os.path.dirname(os.path.abspath(mod.__file__)), "sessions.json")
    return "sessions.json"


def _serialize_ctx(ctx):
    """Serialize a _Context object to a dict."""
    data = {}
    for attr in dir(ctx):
        if attr.startswith("_"):
            continue
        feat = getattr(ctx, attr)
        if hasattr(feat, '__dict__'):
            data[attr] = {k: v for k, v in feat.__dict__.items() if not k.startswith("_")}
    return data


def _deserialize_ctx(data, ctx_class):
    """Deserialize a dict into a _Context object."""
    ctx = ctx_class()
    for feat_name, feat_data in data.items():
        feat = getattr(ctx, feat_name, None)
        if feat is None:
            continue
        for k, v in feat_data.items():
            if hasattr(feat, k):
                setattr(feat, k, v)
    return ctx


def _save_sessions():
    """Save all sessions and name mappings to disk."""
    import json
    sessions = _get_sessions()
    data = {
        "sessions": {token: _serialize_ctx(ctx) for token, ctx in sessions.items()},
        "names": _get_session_names(),
    }
    try:
        with open(_sessions_path(), "w") as f:
            json.dump(data, f, indent=2)
    except Exception as e:
        print(f"sessions: save error: {e}")


def _load_sessions():
    """Load sessions and name mappings from disk."""
    import json, os
    mod = _find_root_module()
    if mod is None:
        return
    path = _sessions_path()
    if not os.path.exists(path):
        return
    try:
        with open(path) as f:
            data = json.load(f)
        # handle both old format (flat) and new format (with names)
        if "sessions" in data:
            session_data = data["sessions"]
            name_data = data.get("names", {})
        else:
            session_data = data
            name_data = {}
        sessions = _get_sessions()
        for token, ctx_data in session_data.items():
            sessions[token] = _deserialize_ctx(ctx_data, mod._Context)
        name_map = _get_session_names()
        name_map.update(name_data)
        print(f"sessions: loaded {len(sessions)} ({len(name_map)} named)")
    except Exception as e:
        print(f"sessions: load error: {e}")


def _get_sessions():
    """Get the shared sessions dict from the root module."""
    mod = _find_root_module()
    if mod is None:
        return {}
    if not hasattr(mod, '_sessions'):
        mod._sessions = {}
        _load_sessions()
    return mod._sessions


# @zero on (string token) = create session (string name)
def fn_create_session__string(name: str) -> str:
    import uuid
    mod = _find_root_module()
    if mod is None:
        return ""
    sessions = _get_sessions()
    # check for existing session for this user
    name_map = _get_session_names()
    if name in name_map:
        return name_map[name]
    # create new session
    token = str(uuid.uuid4())[:8]
    ctx = mod._Context()
    sessions[token] = ctx
    name_map[name] = token
    _save_sessions()
    return token


def _get_session_names():
    """Get the shared name→token mapping from the root module."""
    mod = _find_root_module()
    if mod is None:
        return {}
    if not hasattr(mod, '_session_names'):
        mod._session_names = {}
    return mod._session_names


# @zero input uint random$
def _get_random() -> int:
    import random
    return random.getrandbits(32)


# @zero on (string result) = random digits (int n)
def fn_random_digits__int(n: int) -> str:
    return "".join(str(_get_random() % 10) for _ in range(n))


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


# @zero on (string result) = functions ()
def fn_functions() -> str:
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"
    return _build_directory(mod)


def _build_feature_tree(tree):
    """Build a feature tree with box-drawing characters from _FEATURE_TREE data."""
    # tree is [(name, summary, extends), ...]
    children = {}  # parent -> [child entries]
    root = None
    for name, summary, extends in tree:
        if extends is None:
            root = (name, summary)
        else:
            children.setdefault(extends, []).append((name, summary))
    if root is None:
        return "no root feature"
    lines = [f"{root[0]} \u2014 {root[1]}"]
    _render_children(lines, root[0], children, "")
    return "\n".join(lines)


def _render_children(lines, parent, children, prefix):
    """Recursively render child features with box-drawing connectors."""
    kids = children.get(parent, [])
    for i, (name, summary) in enumerate(kids):
        is_last = (i == len(kids) - 1)
        connector = "\u2514\u2500 " if is_last else "\u251c\u2500 "
        lines.append(f"{prefix}{connector}{name} \u2014 {summary}")
        child_prefix = prefix + ("   " if is_last else "\u2502  ")
        _render_children(lines, name, children, child_prefix)


# @zero on (string result) = features ()
def fn_features() -> str:
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"
    tree = getattr(mod, '_FEATURE_TREE', None)
    if not tree:
        return "no feature tree available"
    return _build_feature_tree(tree)


# @zero Call input$
# The input stream — receives a Call for every input-tagged function call.
_input_stream = None


def _push_runtime_input(call):
    """Push a Call into the input$ stream (if it exists)."""
    if _input_stream is not None:
        _input_stream.append(call)


def _register_input_stream(stream):
    """Register the input$ stream. Called by the compiled module."""
    global _input_stream
    _input_stream = stream


# @zero on (string result) = rpc eval (string expr)
def fn_rpc_eval__string(expr: str) -> str:
    expr = urllib.parse.unquote(expr).strip()
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"

    # empty expression: return features + functions
    if not expr:
        return fn_features() + "\n\n" + fn_functions()

    # assignment: feature.var = value  or  var = value
    # brackets indicate a function call, not an assignment
    assign_match = re.match(r'^([\w.-]+)\s*=\s*(.+)$', expr)
    if assign_match and '(' not in expr and '[' not in expr:
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

    # var get: no brackets and no = means it's a variable read
    if '(' not in expr and '[' not in expr and '=' not in expr:
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

    # function call — try signature matching first, then naive fn_words
    fn, args = _try_match_call(mod, expr)
    if fn is None:
        fn_words = _extract_fn_words(expr)
        args = _extract_args(expr)
        fn = _find_function(mod, fn_words, len(args))
    if fn is None:
        return f"error: function '{_extract_fn_words(expr)}' not found"
    try:
        result = fn(*args)
        return _format_value(result)
    except Exception as e:
        return f"error: {e}"


# @zero on (string json) = serialise [items$]
def fn_serialise(items) -> str:
    import json
    def _serialise_item(v):
        if v is None:
            return None
        if isinstance(v, (str, int, float, bool)):
            return v
        if isinstance(v, list):
            return [_serialise_item(x) for x in v]
        if hasattr(v, '__dict__'):
            return {k: _serialise_item(val) for k, val in v.__dict__.items()
                    if not k.startswith('_')}
        return str(v)
    data = {
        "values": [_serialise_item(v) for v in items],
    }
    dt = getattr(items, 'dt', 0)
    if dt:
        data["dt"] = dt
    cap = getattr(items, 'capacity', 0)
    if cap:
        data["capacity"] = cap
    t0 = getattr(items, 't0', 0)
    if t0:
        data["t0"] = t0
    ts = getattr(items, '_timestamps', [])
    if ts:
        data["timestamps"] = ts
    return json.dumps(data)


# @zero on (string result$) = deserialise (string json)
def fn_deserialise__string(json_str: str):
    import json, sys
    data = json.loads(json_str)
    values = data.get("values", [])
    mod = sys.modules.get('__main__')
    stream_cls = getattr(mod, '_Stream', None) if mod else None
    result = stream_cls(values) if stream_cls else list(values)
    for attr in ('dt', 'capacity', 't0'):
        val = data.get(attr, 0)
        if val and hasattr(result, '__dict__'):
            object.__setattr__(result, attr, val)
    ts = data.get("timestamps", [])
    if ts and hasattr(result, '_timestamps'):
        object.__setattr__(result, '_timestamps', ts)
    return result
