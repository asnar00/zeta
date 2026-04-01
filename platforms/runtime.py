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
