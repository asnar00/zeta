"""Python emitter: converts zero IR into Python 3.12+ source code."""

import re
import sys
from parser import process
from emit_base import (
    SYMBOL_WORDS as _SYMBOL_WORDS,
    check_compatibility as _check_compatibility,
    get_base_name as _get_base_name,
    make_function_name as _make_function_name,
    make_function_name_from_reduce as _make_function_name_from_reduce,
    collect_array_refs as _collect_array_refs,
    rewrite_array_ref as _rewrite_array_ref,
    replace_underscore as _replace_underscore,
    collect_all_fields as _collect_all_fields,
    compute_dispatch_groups,
    make_task_fn_name,
    make_task_call_fn_name,
    source_comment,
)

# Python builtins that don't need type aliases
_BUILTINS = {"int", "float", "string"}

# Zero type names to Python type names
_PY_TYPE_MAP = {"string": "str", "char": "str", "time": "float", "number": "float"}


_enum_values = {}  # populated by emit(), maps value -> "type.value"

# set of feature names that have per-user variables (for context access detection)
_context_features = set()

# set of emitted function names that are non-deterministic platform functions
_nondeterministic_fn_names = set()

# platform function name prefixes that are non-deterministic (return values from
# the outside world: user input, randomness, time, network, sessions)
_NONDETERMINISTIC_PREFIXES = {
    "fn_input", "fn_random_digits", "fn_create_session",
    "fn_get_cookie", "fn_choose",
    "fn_connect_to", "fn_request__string_on",
    "fn_receive_message_on",
    "fn_every__number_do",
}


def _init_globals(ir: dict) -> dict:
    """Initialize module-level enum values and context features. Returns enums dict."""
    global _enum_values, _context_features, _nondeterministic_fn_names
    _context_features = set()
    _nondeterministic_fn_names = set()
    var_owners = ir.get("_var_owners", {})
    all_user = ir.get("_all_user_vars", [])
    for v in all_user:
        _context_features.add(var_owners.get(v["name"], "default"))
    for fn in ir["functions"]:
        if fn.get("_platform") and fn.get("result"):
            name = _make_function_name(fn["signature_parts"])
            if any(name.startswith(p) for p in _NONDETERMINISTIC_PREFIXES):
                _nondeterministic_fn_names.add(name)
    enums = {t["name"]: t for t in ir["types"] if t["kind"] == "enum"}
    _enum_values = {}
    for ename, edata in enums.items():
        py_name = _py_name(ename)
        for val in edata["values"]:
            _enum_values[val] = f"{py_name}.{val}"
    return enums


def _has_concurrently(ir: dict) -> bool:
    """Check if any function uses concurrently blocks."""
    return any(
        any(stmt.get("kind") == "concurrently" for stmt in fn.get("body", []))
        for fn in ir["functions"]
    )


def _emit_tests_sections(ir: dict) -> list[str]:
    """Emit test sections grouped by feature."""
    tests = ir.get("tests", [])
    if not tests:
        return []
    by_feature = {}
    for t in tests:
        feat = t.get("feature", ir.get("test_feature", "test"))
        by_feature.setdefault(feat, []).append(t)
    return [_emit_test_section(feat_tests, feat_name)
            for feat_name, feat_tests in by_feature.items()]


def _emit_definitions(ir: dict, enums: dict) -> list[str]:
    """Emit type declarations, tasks, and functions."""
    sections = []
    all_types = {t["name"]: t for t in ir["types"]}
    for typ in ir["types"]:
        code = _emit_type(typ, enums, all_types)
        if code:
            sections.append(code)
    src = ir.get("source_file")
    for task in ir.get("tasks", []):
        if not task.get("abstract"):
            sections.append(source_comment(task, src, "#") + "\n" + _emit_task(task, ir.get("uses", [])))
    for fn in ir["functions"]:
        if not fn.get("abstract"):
            sections.append(source_comment(fn, src, "#") + "\n" + _emit_function(fn, ir))
    return sections


def _emit_variables_section(ir: dict) -> list[str]:
    """Emit variable declarations and bare statements."""
    platform_stream_names = {u["name"].replace("$", "") for u in ir.get("uses", [])}
    user_var_names = {v["name"] for v in ir.get("_all_user_vars", [])}
    var_lines = []
    for var in ir["variables"]:
        if var["name"] in user_var_names:
            continue
        if var["name"] in platform_stream_names and var.get("array") and var.get("value") is None:
            continue
        if var.get("_platform") and var.get("array") and var.get("value") is None:
            continue
        var_lines.append(_emit_variable(var))
    for stmt in ir.get("statements", []):
        var_lines.append(_emit_expr(stmt))
    return var_lines


def _apply_module_prefixes(result: str, ir: dict) -> str:
    """Prefix cross-module function calls with their module name."""
    module_map = ir.get("module_map")
    current_module = ir.get("current_module")
    if module_map and current_module:
        for fn_name, mod_name in module_map.items():
            if mod_name != current_module:
                result = result.replace(f"{fn_name}(", f"{mod_name}.{fn_name}(")
    return result


_STREAM_HELPER = """\
class _Stream(list):
    \"\"\"A list that supports stream timing properties (dt, length, t0).\"\"\"
    pass"""

_UNDEFINED_HELPER = """\
def _raise_undefined(name):
    raise RuntimeError(f"function not defined: {name}")"""

_ZERO_RAISE_HELPER = """\
class _ZeroRaise(Exception):
    def __init__(self, name, args=None):
        self.name = name
        self.args_list = args or []
        super().__init__(f"{name}({', '.join(str(a) for a in self.args_list)})")"""


def _has_raise(ir: dict) -> bool:
    """Check if any function body contains a raise node or handlers are defined."""
    if ir.get("handlers"):
        return True
    for fn in ir["functions"]:
        if _walk_has_kind(fn.get("body", []), "raise"):
            return True
    return False


def _walk_has_kind(stmts, kind):
    """Recursively check if any statement in a list has the given kind."""
    for stmt in stmts:
        if not isinstance(stmt, dict):
            continue
        if stmt.get("kind") == kind:
            return True
        for branch in stmt.get("branches", []):
            if isinstance(branch, dict) and _walk_has_kind(branch.get("body", []), kind):
                return True
    return False


_BB_FALLBACK = """\
# blackbox fallback (overridden when blackbox platform is loaded)
import time as _time
def _bb_record_stream(_name, _iter):
    _dt = getattr(_iter, 'dt', 0)
    for _v in _iter:
        yield _v
        if _dt and _dt > 0:
            _time.sleep(_dt)
def _bb_record_call(_name, _result):
    return _result"""


def _needs_bb_fallback(ir: dict) -> bool:
    """Check if the emitted code will use blackbox recording functions."""
    if ir.get("tasks"):
        return True
    if _nondeterministic_fn_names:
        return True
    return False


def _has_timed_streams(ir: dict) -> bool:
    """Check if any variable or task body uses timed streams."""
    for var in ir.get("variables", []):
        if _value_has_timing(var.get("value")):
            return True
        if var.get("stream_props"):
            return True
    for task in ir.get("tasks", []):
        for node in task.get("body", []):
            if not isinstance(node, dict):
                continue
            if node.get("kind") == "var_decl":
                if _value_has_timing(node.get("value")):
                    return True
            if node.get("kind") in ("emit", "emit_external"):
                if _value_has_timing(node.get("value")):
                    return True
            if node.get("kind") == "for_each":
                for bn in node.get("body", []):
                    if isinstance(bn, dict) and bn.get("kind") in ("emit", "emit_external"):
                        if _value_has_timing(bn.get("value")):
                            return True
    return False


def _value_has_timing(val):
    """Check if a value node contains timed_step markers."""
    if not isinstance(val, dict):
        return False
    if val.get("kind") == "timed_step":
        return True
    if val.get("kind") == "stream":
        for step in val.get("steps", []):
            if isinstance(step, dict) and step.get("kind") == "timed_step":
                return True
    return False


def _emit_preamble(ir: dict) -> list[str]:
    """Emit imports and concurrently helper if needed."""
    sections = []
    if _needs_bb_fallback(ir):
        sections.append(_BB_FALLBACK)
    if _has_timed_streams(ir):
        sections.append(_STREAM_HELPER)
    imports = _collect_imports(ir)
    if imports:
        sections.append("\n".join(imports))
    if _has_concurrently(ir):
        sections.append(_CONCURRENTLY_HELPER)
    if ir.get("errors"):
        sections.append(_UNDEFINED_HELPER)
    if _has_raise(ir):
        sections.append(_ZERO_RAISE_HELPER)
    return sections


def emit(ir: dict) -> str:
    """Emit Python source code from a zero IR dict."""
    _check_compatibility(ir)
    enums = _init_globals(ir)
    sections = _emit_preamble(ir)
    sections.extend(_emit_tests_sections(ir))
    sections.extend(_emit_definitions(ir, enums))

    concrete_fns = [fn for fn in ir["functions"] if not fn.get("abstract")]
    dispatch_sections = _generate_dispatchers(concrete_fns, ir.get("types", []))

    user_vars = ir.get("_all_user_vars", [])
    if user_vars:
        sections.insert(0, _emit_context_class(user_vars, ir))

    var_lines = _emit_variables_section(ir)
    if var_lines:
        sections.append("\n".join(var_lines))
    sections.extend(dispatch_sections)

    result = "\n\n".join(sections) + "\n" if sections else ""
    return _apply_module_prefixes(result, ir)


# --- tests ---

def _emit_single_test(test: dict, feature_name: str, index: int) -> tuple:
    """Emit a single test function. Returns (lines, name, desc)."""
    name = f"test_{feature_name.replace('-', '_')}_{index}"
    call_code = _emit_expr(test["call"])
    expected_code = _emit_expr(test["expected"]) if isinstance(test["expected"], dict) else repr(test["expected"])
    is_task = test["call"].get("kind") == "task_call"
    expected_is_array = (test["expected"].get("kind") == "array_lit" or
                         (test["expected"].get("kind") == "raw" and test["expected"].get("value", "").startswith("[")))
    if is_task and not expected_is_array:
        expected_code = f"[{expected_code}]"
    desc = test.get("source_text", "")
    lines = [
        f"def {name}():",
        f"    '''{desc}'''",
        f"    _result = {call_code}",
        f"    _expected = {expected_code}",
        f'    assert _result == _expected, f"expected {{_expected}}, got {{_result}}"',
        "",
    ]
    return lines, name, desc


def _emit_test_section(tests: list[dict], feature_name: str) -> str:
    """Emit test functions and register them for a feature."""
    lines = []
    test_entries = []
    for i, test in enumerate(tests):
        test_lines, name, desc = _emit_single_test(test, feature_name, i)
        lines.extend(test_lines)
        test_entries.append((name, desc))
    entries = ", ".join(f"({n}, {repr(d)})" for n, d in test_entries)
    lines.append(f"register_tests({repr(feature_name)}, [{entries}])")
    return "\n".join(lines)


# --- dispatchers ---

def _emit_single_dispatcher(base: str, sorted_fns: list[dict]) -> str:
    """Generate a single dispatch function for a group of overloaded functions."""
    dispatcher_name = "fn_" + base
    param_names = [p["name"] for p in sorted_fns[0]["params"]]
    params_str = ", ".join(param_names)
    ret_type = sorted_fns[0]["result"]["type"] if sorted_fns[0].get("result") else None
    ret_ann = f" -> {_py_type_ann(ret_type)}" if ret_type else ""
    lines = [f"def {dispatcher_name}({params_str}){ret_ann}:"]
    for i, fn in enumerate(sorted_fns):
        specific_name = _make_function_name(fn["signature_parts"])
        param_type = fn["params"][0]["type"] if fn["params"] else None
        if param_type:
            keyword = "if" if i == 0 else "elif"
            lines.append(f"    {keyword} isinstance({param_names[0]}, {param_type}):")
            lines.append(f"        return {specific_name}({params_str})")
    last_name = _make_function_name(sorted_fns[-1]["signature_parts"])
    lines.append(f"    return {last_name}({params_str})")
    return "\n".join(lines)


def _generate_dispatchers(functions: list[dict], types: list[dict]) -> list[str]:
    """Generate dispatch functions for function groups with multiple definitions."""
    dispatch_groups = compute_dispatch_groups(functions, types)
    return [_emit_single_dispatcher(base, sorted_fns)
            for base, sorted_fns in dispatch_groups.items()]


# --- imports ---

def _needs_reduce(ir: dict) -> bool:
    """Check if the IR uses reduce expressions."""
    if any(isinstance(v.get("value"), dict) and v["value"].get("kind") == "reduce"
           for v in ir["variables"]):
        return True
    return any(any(isinstance(s, dict) and s.get("kind") == "assign"
                   and isinstance(s.get("value"), dict) and s["value"].get("kind") == "reduce"
                   for s in fn.get("body", []))
               for fn in ir["functions"])


def _needs_zip_longest(ir: dict) -> bool:
    """Check if the IR uses multi-array binop expressions."""
    return any(isinstance(v.get("value"), dict) and v["value"].get("kind") == "binop"
               and v.get("array") and len(_collect_array_refs(v["value"])) > 1
               for v in ir["variables"])


def _collect_imports(ir: dict) -> list[str]:
    """Collect needed import statements."""
    imports = []
    has_enum = any(t["kind"] == "enum" for t in ir["types"])
    has_struct = any(t["kind"] == "struct" for t in ir["types"])
    if _needs_reduce(ir):
        imports.append("import functools")
    if _has_concurrently(ir):
        imports.append("import threading")
    if _needs_zip_longest(ir):
        imports.append("from itertools import zip_longest")
    if has_enum:
        imports.append("from enum import Enum")
    if has_struct:
        imports.append("from typing import NamedTuple")
    return imports


_CONCURRENTLY_HELPER = """\
def _concurrently(*fns):
    threads = [threading.Thread(target=fn) for fn in fns]
    for t in threads:
        t.start()
    for t in threads:
        t.join()"""


# --- naming helpers ---

def _py_name(name: str) -> str:
    """Convert a zero name to a valid Python identifier (hyphens to underscores)."""
    return name.replace("-", "_")


def _py_type_ann(zero_type: str) -> str:
    """Map a zero type to its Python annotation."""
    mapped = _PY_TYPE_MAP.get(zero_type)
    if mapped:
        return mapped
    return zero_type.replace("-", "_")


def _is_context_access(object_name: str) -> bool:
    """Check if a member access is a user-context variable lookup."""
    return object_name in _context_features


def _py_default_for_type(zero_type: str) -> str:
    """Return the Python default/zero value for a zero type."""
    defaults = {"string": '""', "int": "0", "uint": "0", "float": "0.0",
                "number": "0", "bool": "False", "char": '""'}
    if zero_type in defaults:
        return defaults[zero_type]
    return f"{_safe(zero_type)}()"


_PY_RESERVED = {
    "and", "as", "assert", "async", "await", "break", "class", "continue",
    "def", "del", "elif", "else", "except", "finally", "for", "from",
    "global", "if", "import", "in", "is", "lambda", "nonlocal", "not",
    "or", "pass", "raise", "return", "try", "while", "with", "yield",
}


def _safe(name: str) -> str:
    """Convert a zero name to a Python-safe name."""
    result = name.replace("-", "_")
    if result in _PY_RESERVED:
        result = result + "_"
    return result


def _safe_target(target: str) -> str:
    """Convert a zero assignment target to Python-safe form.
    Handles: name, name$[key], name.field"""
    if "$[" in target:
        parts = target.split("$[", 1)
        arr_name = _safe(parts[0]) + "_arr"
        key = _safe(parts[1].rstrip("]"))
        return f"{arr_name}[{key}]"
    if target.endswith("$"):
        return _safe(target.replace("$", "_arr"))
    return _safe(target)


# --- types ---

def _qualify_default(field: dict, enums: dict) -> str:
    """Qualify a field default with its enum type name if needed."""
    if field["default"] is None:
        return ""
    default_val = field["default"]
    if field["type"] in enums:
        enum = enums[field["type"]]
        if str(default_val) in enum["values"]:
            py_name = _py_name(field["type"])
            return f" = {py_name}.{default_val}"
    return f" = {default_val}"


def _emit_struct_type(typ: dict, enums: dict, all_types: dict) -> str:
    """Emit a struct as a NamedTuple class."""
    py_name = _py_name(typ["name"])
    lines = [f"class {py_name}(NamedTuple):"]
    all_fields = _collect_all_fields(typ, all_types)
    for field in all_fields:
        default = _qualify_default(field, enums)
        lines.append(f"    {field['name']}: {_py_type_ann(field['type'])}{default}")
    if not all_fields:
        lines.append("    pass")
    return "\n".join(lines)


def _emit_type(typ: dict, enums: dict = None, all_types: dict = None) -> str | None:
    """Emit a single type declaration."""
    enums = enums or {}
    all_types = all_types or {}
    if typ["kind"] == "numeric":
        if typ["name"] in _BUILTINS:
            return None
        return f"type {typ['name']} = {typ['base']}"
    elif typ["kind"] == "enum":
        py_name = _py_name(typ["name"])
        lines = [f"class {py_name}(Enum):"]
        for val in typ["values"]:
            lines.append(f'    {val} = "{val}"')
        return "\n".join(lines)
    elif typ["kind"] == "struct":
        return _emit_struct_type(typ, enums, all_types)


# --- context class ---

def _emit_context_fields(user_vars: list[dict], ir: dict) -> list[str]:
    """Generate _Context class with per-feature nested classes."""
    var_owners = ir.get("_var_owners", {})
    by_feature = {}
    for v in user_vars:
        feature = var_owners.get(v["name"], "default")
        by_feature.setdefault(feature, []).append(v)
    lines = ["class _Context:"]
    for feat_name in sorted(by_feature):
        safe_feat = _safe(feat_name)
        lines.append(f"    class {safe_feat}:")
        for v in by_feature[feat_name]:
            type_ann = _py_type_ann(v["type"])
            val = _emit_value(v["value"], v["type"])
            lines.append(f"        {_safe(v['name'])}: {type_ann} = {val}")
    lines.append(f"    def __init__(self):")
    for feat_name in sorted(by_feature):
        safe_feat = _safe(feat_name)
        lines.append(f"        self.{safe_feat} = _Context.{safe_feat}()")
    return lines


def _emit_context_accessor() -> list[str]:
    """Generate contextvars infrastructure for user-context access."""
    return [
        "_ctx_var: contextvars.ContextVar['_Context'] = contextvars.ContextVar('_ctx', default=_Context())",
        "",
        "def _get_ctx() -> '_Context':",
        "    import sys",
        "    _main = sys.modules.get('__main__')",
        "    if _main and hasattr(_main, '_ctx_var'):",
        "        return _main._ctx_var.get()",
        "    return _ctx_var.get()",
        "",
    ]


def _emit_context_class(user_vars: list[dict], ir: dict) -> str:
    """Generate a _Context class from per-user variables, plus contextvars setup."""
    lines = ["import contextvars", ""]
    lines.extend(_emit_context_fields(user_vars, ir))
    lines.append("")
    lines.extend(_emit_context_accessor())
    return "\n".join(lines)


# --- variables ---

def _emit_variable(var: dict) -> str:
    """Emit a variable declaration."""
    name = _safe(var["name"] + "_arr" if var["array"] else var["name"])
    type_ann = _py_type_ann(var["type"])
    if var["array"]:
        base = _emit_array_variable(name, type_ann, var)
        stream_props = var.get("stream_props", {})
        if stream_props:
            lines = [base]
            # wrap in _Stream if not already
            if "_Stream" not in base:
                # handle list(...) from task calls
                lines[0] = lines[0].replace(f"{name} = list(", f"{name} = _Stream(list(")
                if lines[0] != base and lines[0].endswith(")"):
                    lines[0] = lines[0] + ")"
                # handle plain [...] from literals
                elif f"{name} = [" in lines[0]:
                    lines[0] = lines[0].replace(f"{name} = [", f"{name} = _Stream([")
                    if lines[0].endswith("]"):
                        lines[0] = lines[0] + ")"
            for key, expr in stream_props.items():
                lines.append(f"{name}.{key} = {_emit_expr(expr)}")
            return "\n".join(lines)
        return base
    else:
        value_str = _emit_value(var["value"], var["type"])
        return f"{name}: {type_ann} = {value_str}"


def _emit_dict_array_variable(name: str, type_ann: str, val: dict) -> str | None:
    """Emit array variable when value is a dict (expressions, streams, etc.)."""
    if "range" in val:
        return f"{name}: list[{type_ann}] = {_emit_range(val)}"
    kind = val.get("kind")
    if kind == "stream":
        return _emit_stream(name, val)
    if kind == "timed_step":
        inner = val["value"]
        lines = []
        if inner.get("kind") == "task_call":
            fn_name = make_task_call_fn_name(inner)
            args = ", ".join(a.replace("$", "_arr") for a in inner["args"])
            lines.append(f"{name} = _Stream(list({fn_name}({args})))")
        else:
            lines.append(f"{name} = _Stream({_emit_expr(inner)})")
        if val.get("dt"):
            lines.append(f"{name}.dt = {_emit_expr(val['dt'])}")
        if val.get("length"):
            lines.append(f"{name}.length = {_emit_expr(val['length'])}")
        return "\n".join(lines)
    if kind == "task_call":
        return _emit_task_call(name, type_ann, val)
    if kind == "where":
        return f"{name} = {_emit_expr(val)}"
    if kind == "sort":
        return f"{name} = {_emit_sort(val)}"
    if "kind" in val:
        return f"{name}: list[{type_ann}] = {_emit_array_map_expr(val)}"
    return None


def _emit_array_variable(name: str, type_ann: str, var: dict) -> str:
    """Emit an array variable declaration."""
    if var.get("map"):
        key_type = _py_type_ann(var["key_type"])
        return f"{name}: dict[{key_type}, {type_ann}] = {{}}"
    val = var["value"]
    size = var["size"]
    if val is None and size is None:
        return f"{name}: list[{type_ann}] = []"
    elif val is None and size is not None:
        return f"{name}: list[{type_ann}] = [0] * {size}"
    elif isinstance(val, (int, float)) and size is not None:
        return f"{name}: list[{type_ann}] = [{val}] * {size}"
    elif isinstance(val, list):
        items = ", ".join(_emit_element(v) for v in val)
        return f"{name}: list[{type_ann}] = [{items}]"
    elif isinstance(val, dict):
        result = _emit_dict_array_variable(name, type_ann, val)
        if result:
            return result
    return f"{name}: list[{type_ann}] = {val}"


# --- streams ---

def _emit_stream_loop(name: str, steps: list, terminate: dict, emit_fn) -> list[str]:
    """Emit the loop portion of a streaming expression.
    while: gates the next value (exclude boundary). Compute, check, append if true.
    until: includes the triggering value. Compute, append, check, break if true."""
    lines = []
    for step in steps[1:-1]:
        lines.append(f"{name}.append({emit_fn(step)})")
    repeat_expr = emit_fn(steps[-1])
    if terminate["kind"] == "until":
        cond = emit_fn(terminate["condition"])
        lines.append("while True:")
        lines.append(f"    _next = {repeat_expr}")
        lines.append(f"    {name}.append(_next)")
        lines.append(f"    if {cond.replace(f'{name}[-1]', '_next')}:")
        lines.append("        break")
    elif terminate["kind"] == "while":
        cond = emit_fn(terminate["condition"])
        lines.append("while True:")
        lines.append(f"    _next = {repeat_expr}")
        lines.append(f"    if not ({cond.replace(f'{name}[-1]', '_next')}):")
        lines.append("        break")
        lines.append(f"    {name}.append(_next)")
    return lines


def _unwrap_timed_step(node):
    """Unwrap a timed_step node, returning (value_node, dt_expr, length_expr).
    For non-timed steps, returns (node, None, None)."""
    if isinstance(node, dict) and node.get("kind") == "timed_step":
        return node["value"], node.get("dt"), node.get("length")
    return node, None, None


def _emit_stream(name: str, stream: dict) -> str:
    """Emit a streaming expression. Timed steps emit dt/length metadata on the array."""
    steps = stream["steps"]
    terminate = stream["terminate"]
    timing_lines = []

    def _emit_stream_expr(node):
        inner, _, _ = _unwrap_timed_step(node)
        s = _emit_expr(inner)
        return s.replace("__last__", f"{name}[-1]")

    # collect timing from all steps (even on the literal fast path)
    has_timing = False
    for step in steps:
        _, dt, length = _unwrap_timed_step(step)
        if dt:
            timing_lines.append(f"{name}.dt = {_emit_expr(dt)}")
            has_timing = True
        if length:
            timing_lines.append(f"{name}.length = {_emit_expr(length)}")
            has_timing = True

    # use _Stream instead of plain list when timing is present
    ctor = "_Stream" if has_timing else ""
    plain_steps = [_unwrap_timed_step(s)[0] for s in steps]
    if terminate is None and all(s.get("kind") == "literal" for s in plain_steps):
        items = ", ".join(str(s["value"]) for s in plain_steps)
        lines = [f"{name} = {ctor}([{items}])"] if has_timing else [f"{name} = [{items}]"]
    else:
        lines = [f"{name} = {ctor}([{_emit_stream_expr(steps[0])}])"] if has_timing else [f"{name} = [{_emit_stream_expr(steps[0])}]"]
        if len(steps) > 1:
            if terminate is None:
                for step in steps[1:]:
                    lines.append(f"{name}.append({_emit_stream_expr(step)})")
            else:
                lines.extend(_emit_stream_loop(name, steps, terminate, _emit_stream_expr))
    lines.extend(timing_lines)
    return "\n".join(lines)


def _emit_task_call(name: str, type_ann: str, call: dict) -> str:
    """Emit a task call — materialise generator into a list."""
    fn_name = make_task_call_fn_name(call)
    args = ", ".join(a.replace("$", "_arr") for a in call["args"])
    return f"{name} = list({fn_name}({args}))"


def _emit_element(v) -> str:
    """Emit a single array element — raw value or AST node."""
    if isinstance(v, dict) and "kind" in v:
        return _emit_expr(v)
    return str(v)


def _emit_range(val: dict) -> str:
    """Emit a range expression, handling both literal and variable endpoints."""
    start = val["start"]
    end = val["end"]
    if val["range"] == "through":
        if isinstance(end, int):
            return f"list(range({start}, {end + 1}))"
        else:
            return f"list(range({start}, {end} + 1))"
    else:  # "to"
        return f"list(range({start}, {end}))"


# --- array mapping ---

def _has_index_underscore(node: dict) -> bool:
    """Check if an expression contains _ used as a position reference."""
    return _contains_name(node, "_")


def _contains_name(node, name: str) -> bool:
    """Check if an expression tree contains a specific name."""
    if isinstance(node, list):
        return any(_contains_name(v, name) for v in node)
    if not isinstance(node, dict):
        return False
    if node.get("kind") == "name" and node.get("value") == name:
        return True
    return any(_contains_name(v, name) for v in node.values() if isinstance(v, (dict, list)))


def _emit_indexed_map_expr(node: dict) -> str:
    """Emit index-based array mapping for expressions using _-relative indexing."""
    array_refs = list(dict.fromkeys(_collect_array_refs(node)))
    if array_refs:
        len_src = array_refs[0].replace("$", "_arr")
    else:
        len_src = _find_length_source(node)
        if not len_src:
            return _emit_expr(node)
    rewritten = _rewrite_for_indexed(node)
    expr = _emit_expr(rewritten)
    return f"[{expr} for _i in range(len({len_src}))]"


def _find_length_source(node: dict) -> str | None:
    """Find a variable name suitable for determining length in indexed mapping."""
    if not isinstance(node, dict):
        return None
    if node.get("kind") in ("fn_call", "array_fn_call"):
        for arg in node.get("args", []):
            if isinstance(arg, dict) and arg.get("kind") == "name" and arg["value"] != "_":
                return arg["value"]
    for v in node.values():
        if isinstance(v, dict):
            result = _find_length_source(v)
            if result:
                return result
        elif isinstance(v, list):
            for item in v:
                if isinstance(item, dict):
                    result = _find_length_source(item)
                    if result:
                        return result
    return None


def _rewrite_index_with_underscore(node: dict) -> dict:
    """Rewrite arr$[_ - 1] to a bounds-checked index expression."""
    arr_node = node["array"]
    if arr_node.get("kind") == "name" and arr_node["value"].endswith("$"):
        arr_str = arr_node["value"].replace("$", "_arr")
    else:
        arr_str = _emit_expr(arr_node)
    idx = _rewrite_for_indexed(node["index"])
    idx_str = _emit_expr(idx)
    return {"kind": "raw", "value": f"({arr_str}[{idx_str}] if {idx_str} >= 0 else 0)"}


def _rewrite_for_indexed(node: dict) -> dict:
    """Rewrite an expression for index-based iteration.
    - array$  -> array_arr[_i]
    - array$[_ - N]  -> array_arr[_i - N] with bounds check
    - bare _  -> _i (in index expressions)
    """
    if not isinstance(node, dict):
        return node
    kind = node.get("kind")
    if kind == "index" and _contains_name(node.get("index", {}), "_"):
        return _rewrite_index_with_underscore(node)
    if kind == "name" and node.get("value", "").endswith("$"):
        py_name = node["value"].replace("$", "_arr")
        return {"kind": "raw", "value": f"{py_name}[_i]"}
    if kind == "name" and node.get("value") == "_":
        return {"kind": "raw", "value": "_i"}
    result = {}
    for key, val in node.items():
        if isinstance(val, dict):
            result[key] = _rewrite_for_indexed(val)
        elif isinstance(val, list):
            result[key] = [_rewrite_for_indexed(v) if isinstance(v, dict) else v for v in val]
        else:
            result[key] = val
    return result


def _task_call_param_types_py(call: dict) -> list[str]:
    """Extract param types from a task_call's signature_parts."""
    return [p.strip("()") for p in call["signature_parts"] if p.startswith("(")]


def _coerce_task_arg_py(arg: str, param_type: str) -> str:
    """Coerce a task call argument."""
    if arg.endswith("$"):
        return f"{arg[:-1]}_arr"
    return arg


def _emit_multi_array_map(node: dict, array_refs: list[str]) -> str:
    """Emit zip_longest-based comprehension for multi-array expressions."""
    py_arrs = [a.replace("$", "_arr") for a in array_refs]
    loop_vars = [chr(ord("a") + i) for i in range(len(array_refs))]
    rewritten = node
    for arr, var in zip(array_refs, loop_vars):
        rewritten = _rewrite_array_ref(rewritten, arr, var)
    expr = _emit_expr(rewritten)
    vars_str = ", ".join(loop_vars)
    arrs_str = ", ".join(py_arrs)
    return f"[{expr} for {vars_str} in zip_longest({arrs_str}, fillvalue=0)]"


def _emit_array_map_expr(node: dict) -> str:
    """Emit a list comprehension for array-mapped expressions."""
    if _has_index_underscore(node):
        return _emit_indexed_map_expr(node)
    array_refs = list(dict.fromkeys(_collect_array_refs(node)))  # deduplicate, preserve order
    if len(array_refs) == 0:
        return _emit_expr(node)
    if len(array_refs) == 1:
        arr_name = array_refs[0]
        py_arr = arr_name.replace("$", "_arr")
        expr = _emit_expr(_rewrite_array_ref(node, arr_name, "x"))
        return f"[{expr} for x in {py_arr}]"
    return _emit_multi_array_map(node, array_refs)


# --- values ---

def _emit_value(value, zero_type: str) -> str:
    """Emit a value expression."""
    if isinstance(value, dict) and value.get("kind") == "reduce":
        return _emit_reduce(value, zero_type)
    if isinstance(value, dict) and value.get("kind") == "call":
        return _emit_call_value(value)
    if isinstance(value, dict) and "kind" in value:
        return _emit_expr(value)
    return str(value)


def _emit_reduce(node: dict, zero_type: str) -> str:
    """Emit a functools.reduce call."""
    arr = node["array"].replace("$", "_arr")
    if "op" in node:
        return f"functools.reduce(lambda a, b: a {node['op']} b, {arr})"
    else:
        fn_name = _make_function_name_from_reduce(node["fn_parts"], zero_type)
        return f"functools.reduce({fn_name}, {arr})"


def _emit_call_value(call: dict) -> str:
    """Emit a constructor/call value."""
    name = call["name"]
    if not call["args"]:
        return f"{name}()"
    if isinstance(call["args"][0], dict) and "name" in call["args"][0]:
        parts = [f"{a['name']}={a['value']}" for a in call["args"]]
    else:
        parts = [str(a) for a in call["args"]]
    return f"{name}({', '.join(parts)})"


def _emit_sort(node: dict) -> str:
    """Emit a sort expression."""
    arr = _emit_expr(node["array"])
    if node["key"] is None:
        result = f"sorted({arr})"
    else:
        key_expr = _emit_expr(_replace_underscore(node["key"], "x"))
        result = f"sorted({arr}, key=lambda x: {key_expr})"
    if node.get("descending"):
        result = result[:-1] + ", reverse=True)"
    return result


# --- control flow ---

def _emit_if_block(node: dict, is_task: bool = False) -> str:
    """Emit an if/else if/else block."""
    lines = []
    for i, branch in enumerate(node["branches"]):
        if i == 0:
            cond = _emit_expr(branch["condition"])
            lines.append(f"if {cond}:")
        elif branch["condition"] is not None:
            cond = _emit_expr(branch["condition"])
            lines.append(f"elif {cond}:")
        else:
            lines.append("else:")
        for stmt in branch["body"]:
            if isinstance(stmt, dict) and stmt.get("kind") == "if_block":
                for bl in _emit_if_block(stmt, is_task).split("\n"):
                    lines.append(f"    {bl}")
            elif isinstance(stmt, dict) and stmt.get("kind") == "emit":
                lines.append(f"    yield {_emit_expr(stmt['value'])}")
            else:
                lines.append(f"    {_emit_expr(stmt)}")
    return "\n".join(lines)


def _platform_stream_fn(stream_name: str, uses: list[dict]) -> str:
    """Map a platform stream name to its Python handler function name."""
    for use in uses:
        if use["name"] == stream_name:
            return f"_push_{use['platform']}_{use['name'].replace('$', '')}"
    return f"_push_{stream_name.replace('$', '')}"


# --- tasks ---

def _emit_task_header(task: dict) -> tuple:
    """Build function name and parameter string for a task. Returns (fn_name, params_str)."""
    all_params = task.get("input_streams", []) + task.get("params", [])
    param_strs = []
    for p in all_params:
        pname = p["name"] + "_arr" if p in task.get("input_streams", []) else p["name"]
        param_strs.append(f"{pname}: {_py_type_ann(p['type'])}")
    fn_name = "task_" + "_".join(task["name_parts"])
    for p in all_params:
        fn_name += f"__{p['type']}"
    return fn_name, ", ".join(param_strs)


def _emit_task_for_each(node: dict, uses: list, base_indent: str, extra_indent: str) -> list[str]:
    """Emit a for_each block inside a task.
    Wraps the iterator with _bb_record_stream for flight recording."""
    lines = []
    iter_expr = _emit_expr(node["iter"])
    iter_name = iter_expr.replace("_arr", "$") if iter_expr.endswith("_arr") else iter_expr
    lines.append(f"{base_indent}{extra_indent}for {node['name']} in _bb_record_stream({iter_name!r}, {iter_expr}):")
    loop_indent = base_indent + extra_indent + "    "
    for body_node in node["body"]:
        bk = body_node.get("kind", "")
        if bk == "emit":
            lines.append(f"{loop_indent}yield {_emit_expr(body_node['value'])}")
        elif bk == "emit_external":
            handler = _platform_stream_fn(body_node["stream"], uses)
            lines.append(f"{loop_indent}{handler}({_emit_expr(body_node['value'])})")
        elif bk == "if_block":
            for bl in _emit_if_block(body_node, is_task=True).split("\n"):
                lines.append(f"{loop_indent}{bl}")
        else:
            lines.extend(_indent(_emit_expr(body_node), loop_indent))
    return lines


def _emit_task_consume(node: dict, base_indent: str) -> str:
    """Emit a consume or consume_call loop header.
    Wraps the iterator with _bb_record_stream for flight recording."""
    kind = node.get("kind")
    if kind == "consume":
        stream_name = node["stream"].replace("$", "_arr")
        return f"{base_indent}for {node['name']} in _bb_record_stream({stream_name!r}, {stream_name}):"
    call = node["call"]
    if call.get("kind") == "task_call":
        call_fn = make_task_call_fn_name(call)
        args = ", ".join(a.replace("$", "_arr") for a in call["args"])
        stream_label = call_fn
        return f"{base_indent}for {node['name']} in _bb_record_stream({stream_label!r}, {call_fn}({args})):"
    iter_expr = _emit_expr(call)
    return f"{base_indent}for {node['name']} in _bb_record_stream(\"stream\", {iter_expr}):"


def _is_task_inner_call(node: dict) -> bool:
    """Check if node is a task call (or timed task call) inside a task body."""
    if node.get("kind") != "var_decl" or not node.get("array"):
        return False
    val = node.get("value")
    if not isinstance(val, dict):
        return False
    if val.get("kind") == "task_call":
        return True
    if val.get("kind") == "timed_step" and isinstance(val.get("value"), dict) and val["value"].get("kind") == "task_call":
        return True
    return False


def _emit_task_inner_call(node: dict, base_indent: str, extra_indent: str) -> str:
    """Emit a task call assignment inside a task body."""
    val = node["value"]
    name = node["name"] + "_arr"
    # unwrap timed_step if present
    if val.get("kind") == "timed_step":
        call = val["value"]
        call_fn = make_task_call_fn_name(call)
        args = ", ".join(a.replace("$", "_arr") for a in call["args"])
        lines = [f"{base_indent}{extra_indent}{name} = _Stream({call_fn}({args}))"]
        if val.get("dt"):
            lines.append(f"{base_indent}{extra_indent}{name}.dt = {_emit_expr(val['dt'])}")
        if val.get("length"):
            lines.append(f"{base_indent}{extra_indent}{name}.length = {_emit_expr(val['length'])}")
        return "\n".join(lines)
    call = val
    call_fn = make_task_call_fn_name(call)
    args = ", ".join(a.replace("$", "_arr") for a in call["args"])
    return f"{base_indent}{extra_indent}{name} = {call_fn}({args})"


def _emit_task_body_node(node, uses, base_indent, extra_indent):
    """Emit a single task body node. Returns (lines, new_extra_indent)."""
    kind = node.get("kind")
    if kind == "for_each":
        return _emit_task_for_each(node, uses, base_indent, extra_indent), extra_indent
    if kind in ("consume", "consume_call"):
        return [_emit_task_consume(node, base_indent)], "    "
    if kind == "emit":
        return [f"{base_indent}{extra_indent}yield {_emit_expr(node['value'])}"], extra_indent
    if kind == "emit_stream":
        # inline stream: yield all values from it
        tmp = "_stream_tmp"
        stream_code = _emit_stream(tmp, node["stream"])
        lines = [f"{base_indent}{extra_indent}{line}" for line in stream_code.split("\n")]
        lines.append(f"{base_indent}{extra_indent}for _v in {tmp}:")
        lines.append(f"{base_indent}{extra_indent}    yield _v")
        return lines, extra_indent
    if kind == "emit_external":
        handler = _platform_stream_fn(node["stream"], uses)
        val = node["value"]
        # anonymous timed stream: out$ <- task() at ((1) hz)
        if isinstance(val, dict) and val.get("kind") == "timed_step":
            inner = val["value"]
            if inner.get("kind") == "task_call":
                fn_name = make_task_call_fn_name(inner)
                args = ", ".join(a.replace("$", "_arr") for a in inner["args"])
                tmp = "_anon_stream"
                lines = [f"{base_indent}{extra_indent}{tmp} = _Stream(list({fn_name}({args})))"]
                if val.get("dt"):
                    lines.append(f"{base_indent}{extra_indent}{tmp}.dt = {_emit_expr(val['dt'])}")
                if val.get("length"):
                    lines.append(f"{base_indent}{extra_indent}{tmp}.length = {_emit_expr(val['length'])}")
                lines.append(f"{base_indent}{extra_indent}for _v in _bb_record_stream('{tmp}', {tmp}):")
                lines.append(f"{base_indent}{extra_indent}    {handler}(_v)")
                return lines, extra_indent
        val_expr = _emit_expr(val)
        # if the value is a stream variable, iterate it with timing
        if isinstance(val, dict) and val.get("kind") == "name" and val.get("value", "").endswith("$"):
            arr_name = val_expr
            lines = [
                f"{base_indent}{extra_indent}for _v in _bb_record_stream({arr_name!r}, {arr_name}):",
                f"{base_indent}{extra_indent}    {handler}(_v)",
            ]
            return lines, extra_indent
        return [f"{base_indent}{extra_indent}{handler}({val_expr})"], extra_indent
    if kind == "if_block":
        block_lines = [f"{base_indent}{extra_indent}{bl}"
                       for bl in _emit_if_block(node, is_task=True).split("\n")]
        return block_lines, extra_indent
    if _is_task_inner_call(node):
        return [_emit_task_inner_call(node, base_indent, extra_indent)], extra_indent
    return [f"{base_indent}{extra_indent}{_emit_expr(node)}"], extra_indent


def _emit_task(task: dict, uses: list[dict] = None) -> str:
    """Emit a task as a Python generator function (or plain function for void tasks)."""
    uses = uses or []
    fn_name, params_str = _emit_task_header(task)
    lines = [f"def {fn_name}({params_str}):"]
    base_indent = "    "
    extra_indent = ""
    for node in task["body"]:
        if isinstance(node, str):
            lines.append(f"{base_indent}{extra_indent}{node}")
            continue
        new_lines, extra_indent = _emit_task_body_node(node, uses, base_indent, extra_indent)
        lines.extend(new_lines)
    return "\n".join(lines)


# --- functions ---

def _emit_guarded_body(fn_body: list[dict], result_var: str, needs_guard: bool) -> list[str]:
    """Emit function body with result-assignment guards."""
    lines = []
    seen_conditional = False
    for stmt in fn_body:
        code = _emit_expr(stmt)
        if needs_guard and _assigns_result(stmt, result_var):
            if seen_conditional:
                lines.extend(_indent(f"if {result_var} is None:", "    "))
                lines.extend(_indent(code, "        "))
            else:
                lines.extend(_indent(code, "    "))
                if stmt.get("kind") == "if_block":
                    seen_conditional = True
        else:
            lines.extend(_indent(code, "    "))
    return lines


def _emit_result_function(fn: dict, name: str, params: str, handlers: list = None) -> str:
    """Emit a function that returns a result value."""
    ret_type = _py_type_ann(fn["result"]["type"])
    result_var = fn["result"]["name"]
    if result_var.endswith("$"):
        result_var = result_var.replace("$", "_arr")
    needs_guard = _result_needs_guard(fn["body"], result_var)
    has_cond = any(s.get("kind") == "if_block" and _assigns_result(s, result_var) for s in fn["body"])
    lines = [f"def {name}({params}) -> {ret_type}:"]
    if needs_guard or has_cond:
        lines.append(f"    {result_var} = None")
    body_lines = _emit_guarded_body(fn["body"], result_var, needs_guard)
    if has_cond:
        default_val = _py_default_for_type(fn["result"]["type"])
        body_lines.append(f"    return {result_var} if {result_var} is not None else {default_val}")
    else:
        body_lines.append(f"    return {result_var}")
    lines.extend(_wrap_with_handlers(body_lines, handlers or []))
    return "\n".join(lines)


def _get_handlers_for(fn_name: str, handlers: list) -> list:
    """Get handler bindings for a function by matching its word parts."""
    fn_words = fn_name.replace("fn_", "", 1).split("__")[0]
    result = []
    for h in handlers:
        target = h["target_fn"].replace(" ", "_")
        if fn_words == target:
            result.append(h)
    return result


def _wrap_with_handlers(body_lines: list[str], handlers: list) -> list[str]:
    """Wrap function body lines in try/except for handler bindings."""
    if not handlers:
        return body_lines
    wrapped = ["    try:"]
    for line in body_lines:
        wrapped.append("    " + line)
    wrapped.append("    except _ZeroRaise as _e:")
    for i, h in enumerate(handlers):
        handler_fn = "fn_" + h["handler_name"].replace(" ", "_")
        for p in h["params"]:
            handler_fn += f"__{_safe(p['type'])}"
        keyword = "if" if i == 0 else "elif"
        wrapped.append(f'        {keyword} _e.name == {repr(h["handler_name"])}:')
        wrapped.append(f"            {handler_fn}(*_e.args_list)")
    wrapped.append("        else:")
    wrapped.append("            raise")
    return wrapped


def _emit_function(fn: dict, ir: dict = None) -> str:
    """Emit a function definition."""
    ir = ir or {}
    name = _make_function_name(fn["signature_parts"])
    param_strs = []
    for p in fn["params"]:
        pname = p["name"].replace("$", "_arr")
        if p["type"] is not None:
            param_strs.append(f"{pname}: {_py_type_ann(p['type'])}")
        else:
            param_strs.append(pname)
    params = ", ".join(param_strs)
    handlers = _get_handlers_for(name, ir.get("handlers", []))
    if fn["result"] is not None:
        return _emit_result_function(fn, name, params, handlers)
    if _has_scoped_hooks(fn["body"]):
        return _emit_function_with_hooks(fn, name, params, ir)
    lines = [f"def {name}({params}):"]
    body_lines = []
    for stmt in fn["body"]:
        body_lines.extend(_indent(_emit_expr(stmt), "    "))
    lines.extend(_wrap_with_handlers(body_lines, handlers))
    return "\n".join(lines)


def _assigns_result(stmt: dict, result_var: str) -> bool:
    """Check if a statement assigns to the result variable."""
    if stmt.get("kind") == "assign" and stmt.get("target") == result_var:
        return True
    if stmt.get("kind") == "if_block":
        for branch in stmt.get("branches", []):
            for s in branch.get("body", []):
                if _assigns_result(s, result_var):
                    return True
    return False


def _result_needs_guard(body: list[dict], result_var: str) -> bool:
    """Check if a function body has multiple assignments to the result variable,
    where at least one is inside a conditional (requiring early-out semantics)."""
    assign_count = sum(1 for s in body if _assigns_result(s, result_var))
    has_conditional = any(s.get("kind") == "if_block" and _assigns_result(s, result_var) for s in body)
    return assign_count > 1 and has_conditional


def _indent(text: str, prefix: str) -> list[str]:
    """Indent each line of a multi-line string."""
    return [prefix + line for line in text.split("\n")]


# --- expression emitter ---

def _emit_concurrently_expr(node: dict) -> str:
    """Emit a concurrently block as threaded lambda calls."""
    block_calls = []
    for block in node["blocks"]:
        if len(block) == 1:
            block_calls.append(f"lambda: {block[0]}")
        else:
            stmts = "; ".join(block)
            block_calls.append(f"lambda: ({stmts})")
    return f"_concurrently({', '.join(block_calls)})"


def _emit_reduce_expr(node: dict) -> str:
    """Emit a reduce expression."""
    arr = node["array"].replace("$", "_arr")
    if "op" in node:
        return f"functools.reduce(lambda a, b: a {node['op']} b, {arr})"
    fn_name = _make_function_name_from_reduce(node["fn_parts"], "int")
    return f"functools.reduce({fn_name}, {arr})"


def _emit_var_decl_array(name: str, node: dict) -> str:
    """Emit an array variable declaration inside an expression context."""
    val = node["value"]
    if isinstance(val, list):
        items = ", ".join(_emit_element(v) for v in val)
        return f"{name} = [{items}]"
    if isinstance(val, dict) and "range" in val:
        return f"{name} = {_emit_range(val)}"
    if isinstance(val, dict) and val.get("kind") == "task_call":
        fn_name = make_task_call_fn_name(val)
        args = ", ".join(_coerce_task_arg_py(a, t)
                         for a, t in zip(val["args"], _task_call_param_types_py(val)))
        return f"{name} = list({fn_name}({args}))"
    if isinstance(val, dict) and val.get("kind") == "stream" \
            and len(val.get("steps", [])) == 1 and val.get("terminate") is None:
        return f"{name} = {_emit_array_map_expr(val['steps'][0])}"
    if isinstance(val, dict) and val.get("kind") == "fn_call" \
            and any(isinstance(a, dict) and a.get("kind") == "array_fn_call" for a in val.get("args", [])):
        fn_name = _make_function_name(val["signature_parts"])
        inner = _emit_expr(val["args"][0])
        return f"{name} = [{fn_name}(x) for x in {inner}]"
    if isinstance(val, dict) and "kind" in val:
        return f"{name} = {_emit_array_map_expr(val)}"
    return f"{name} = {val}"


def _emit_string_build(steps: list) -> str:
    """Emit a string_build expression as concatenation with str() conversion."""
    parts = []
    for step in steps:
        expr = _emit_expr(step)
        if step.get("kind") == "literal" and isinstance(step.get("value"), str):
            parts.append(expr)
        else:
            parts.append(f"str({expr})")
    return " + ".join(parts)


def _emit_var_decl_expr(node: dict) -> str:
    """Emit a var_decl expression."""
    name = _safe(node["name"]) + "_arr" if node.get("array") else _safe(node["name"])
    if node.get("map"):
        key_type = _py_type_ann(node["key_type"])
        val_type = _py_type_ann(node["type"])
        return f"{name}: dict[{key_type}, {val_type}] = {{}}"
    if node.get("array"):
        return _emit_var_decl_array(name, node)
    val = node.get("value")
    if isinstance(val, dict) and val.get("kind") == "string_build":
        return f"{name} = {_emit_string_build(val['steps'])}"
    return f"{name} = {_emit_expr(node['value'])}"


def _emit_slice_expr(node: dict) -> str:
    """Emit a slice expression."""
    arr = _emit_expr(node["array"])
    start = _emit_expr(node["start"]) if node["start"] is not None else ""
    end = ""
    if node["end"] is not None:
        end = _emit_expr(node["end"])
        if node["inclusive"]:
            if isinstance(node["end"].get("value"), int):
                end = str(node["end"]["value"] + 1)
            else:
                end = f"{end} + 1"
    return f"{arr}[{start}:{end}]"


def _emit_filter_expr(node: dict) -> str:
    """Emit where/first_where/index_of_first_where/indices_where."""
    kind = node["kind"]
    arr = _emit_expr(node["array"])
    cond = _emit_expr(_replace_underscore(node["condition"], "x"))
    if kind == "where":
        return f"[x for x in {arr} if {cond}]"
    if kind == "first_where":
        return f"next((x for x in {arr} if {cond}), type({arr}[0])() if {arr} else None)"
    if kind == "index_of_first_where":
        return f"next(i for i, x in enumerate({arr}) if {cond})"
    return f"[i for i, x in enumerate({arr}) if {cond}]"


def _emit_array_fn_expr(node: dict) -> str:
    """Emit an array function expression."""
    if node["name"] == "length_of":
        return f"len({_emit_expr(node['args'][0])})"
    return str(node)


def _emit_member_expr(node: dict) -> str:
    """Emit a member access expression."""
    if node["field"] == "char$":
        return f"list({_safe(node['object'])})"
    if _is_context_access(node["object"]):
        return f"_get_ctx().{_safe(node['object'])}.{_safe(node['field'])}"
    return f"{_safe(node['object'])}.{_safe(node['field'])}"


def _emit_name_expr(node: dict) -> str:
    """Emit a name reference."""
    val = node["value"]
    if val in _enum_values:
        return _enum_values[val]
    if val.endswith("$"):
        return _safe(val.replace("$", "_arr"))
    return _safe(val)


def _emit_simple_expr(node: dict) -> str | None:
    """Handle expression kinds that need minimal logic. Returns None if unhandled."""
    kind = node["kind"]
    if kind == "assign":
        return f"{_safe_target(node['target'])} = {_emit_expr(node['value'])}"
    if kind == "call":
        return f"{_safe(node['name'])}({', '.join(_emit_expr(a) for a in node['args'])})"
    if kind in ("fn_call", "array_fn_call"):
        fn_name = _make_function_name(node['signature_parts'])
        call_expr = f"{fn_name}({', '.join(_emit_expr(a) for a in node['args'])})"
        if fn_name in _nondeterministic_fn_names:
            return f"_bb_record_call({fn_name!r}, {call_expr})"
        return call_expr
    if kind == "task_call":
        return f"list({make_task_call_fn_name(node)}({', '.join(a.replace('$', '_arr') for a in node['args'])}))"
    if kind == "binop":
        return f"{_emit_expr(node['left'])} {node['op']} {_emit_expr(node['right'])}"
    if kind == "index":
        return f"{_emit_expr(node['array'])}[{_emit_expr(node['index'])}]"
    if kind == "literal":
        return str(node["value"])
    if kind == "ternary":
        return f"({_emit_expr(node['true'])}) if ({_emit_expr(node['condition'])}) else ({_emit_expr(node['false'])})"
    if kind == "raw":
        val = node["value"]
        if re.match(r'[a-zA-Z_]\w*[\s(]', val):
            return f"_raise_undefined({repr(val)})"
        return val
    return None


def _emit_scoped_hook(node: dict) -> str:
    """Placeholder — scoped hooks are handled at the function level."""
    return "pass  # scoped hook (handled at function level)"


def _has_scoped_hooks(body):
    """Check if a function body contains scoped hook nodes."""
    return any(isinstance(s, dict) and s.get("kind") == "scoped_hook" for s in body)


def _emit_function_with_hooks(fn, name, params, ir):
    """Emit a function that contains scoped hooks — patches hooked functions."""
    hooks = [s for s in fn["body"] if isinstance(s, dict) and s.get("kind") == "scoped_hook"]
    stmts = [s for s in fn["body"] if not (isinstance(s, dict) and s.get("kind") == "scoped_hook")]

    lines = [f"def {name}({params}):"]
    lines.append("    import threading as _th")

    # collect hooks by hooked function
    hooked_fns = {}
    for h in hooks:
        hook_fn = "fn_" + h["hook_fn"].replace(" ", "_").replace("-", "_") + "__string"
        hooked_fns.setdefault(hook_fn, []).append(h)

    # save originals and create patched versions
    for hook_fn, hook_list in hooked_fns.items():
        lines.append(f"    _orig_{hook_fn} = globals().get('{hook_fn}', {hook_fn})")
        lines.append(f"    def _patched_{hook_fn}(_prompt, _orig=_orig_{hook_fn}):")
        for h in hook_list:
            args = h["hook_args"]
            arg_check = f"_prompt == {repr(args[0])}" if args else "True"
            body_lines = [_emit_expr(s) for s in h["body"]]
            lines.append(f"        if {arg_check}:")
            lines.append(f"            def _do():")
            lines.append(f"                import time; time.sleep(0.5)")
            for bl in body_lines:
                lines.append(f"                {bl}")
            lines.append(f"            _th.Thread(target=_do, daemon=True).start()")
        lines.append(f"        return _orig(_prompt)")
        lines.append(f"    globals()['{hook_fn}'] = _patched_{hook_fn}")

    # try/finally to restore originals
    lines.append("    try:")
    for stmt in stmts:
        code = _emit_expr(stmt)
        for cl in code.split("\n"):
            lines.append(f"        {cl}")
    lines.append("    finally:")
    for hook_fn in hooked_fns:
        lines.append(f"        globals()['{hook_fn}'] = _orig_{hook_fn}")

    return "\n".join(lines)


def _emit_expr(node: dict) -> str:
    """Emit a Python expression from an AST node."""
    kind = node["kind"]
    if kind == "string_build":
        return _emit_string_build(node["steps"])
    if kind == "scoped_hook":
        return _emit_scoped_hook(node)
    if kind == "placeholder":
        return f"pass  # TODO: {node['text']}"
    if kind == "raise":
        args = ", ".join(repr(a) for a in node["args"])
        return f'raise _ZeroRaise({repr(node["name"])}, [{args}])'
    if kind == "if_block":
        return _emit_if_block(node)
    if kind == "emit":
        return f"yield {_emit_expr(node['value'])}"
    if kind == "concurrently":
        return _emit_concurrently_expr(node)
    if kind == "reduce":
        return _emit_reduce_expr(node)
    if kind == "var_decl":
        return _emit_var_decl_expr(node)
    if kind == "array_fn":
        return _emit_array_fn_expr(node)
    if kind in ("where", "first_where", "index_of_first_where", "indices_where"):
        return _emit_filter_expr(node)
    if kind == "slice":
        return _emit_slice_expr(node)
    if kind == "member":
        return _emit_member_expr(node)
    if kind == "name":
        return _emit_name_expr(node)
    result = _emit_simple_expr(node)
    return result if result is not None else str(node)


# --- CLI ---

if __name__ == "__main__":
    source = open(sys.argv[1]).read()
    ir = process(source)
    print(emit(ir))
