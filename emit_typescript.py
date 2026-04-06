"""TypeScript emitter: converts zero IR into TypeScript source code."""

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
    make_task_call_fn_name,
    source_comment,
)

# TypeScript builtin that needs no alias
_BUILTINS = {"number"}

# Zero numeric base types to TS types
_TS_NUMERIC = {"int": "number", "float": "number", "int | float": "number", "char": "string", "bool": "boolean"}

_enum_values = {}  # populated by emit(), maps value -> "type.value"

# set of feature names that have per-user variables (for context access detection)
_context_features_ts = set()

# set of variable names that are keyed collections (Maps)
_map_vars_ts = set()

# set of function base names that should be emitted as RPC calls (client bundle only)
_rpc_targets_ts = set()

# set of function names that are async in client context (gui platform functions)
_client_async_fns_ts = set()


_CONCURRENTLY_HELPER_TS = """\
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}"""


# --- globals setup ---

def _init_globals_ts(ir: dict) -> tuple:
    """Initialize module-level state from IR. Returns (structs, enums)."""
    global _enum_values, _context_features_ts, _map_vars_ts, _rpc_targets_ts, _client_async_fns_ts
    _context_features_ts = set()
    var_owners = ir.get("_var_owners", {})
    all_user = ir.get("_all_user_vars", [])
    for v in all_user:
        _context_features_ts.add(var_owners.get(v["name"], "default"))
    structs = {t["name"]: t for t in ir["types"] if t["kind"] == "struct"}
    enums = {t["name"]: t for t in ir["types"] if t["kind"] == "enum"}
    _enum_values = {}
    for ename, edata in enums.items():
        ts_name = _ts_name(ename)
        for val in edata["values"]:
            _enum_values[val] = f"{ts_name}.{val}"
    _map_vars_ts = {_safe(v["name"] + "_arr") for v in ir["variables"] if v.get("map")}
    _rpc_targets_ts = ir.get("_rpc_targets", set())
    _client_async_fns_ts = ir.get("_client_async_fns", set())
    return structs, enums


def _has_concurrently(ir: dict) -> bool:
    """Check if any function uses concurrently blocks."""
    return any(
        any(stmt.get("kind") == "concurrently" for stmt in fn.get("body", []))
        for fn in ir["functions"]
    )


# --- emit orchestrator ---

def _emit_tests_sections_ts(ir: dict, structs: dict) -> list[str]:
    """Emit test sections grouped by feature."""
    tests = ir.get("tests", [])
    if not tests:
        return []
    by_feature = {}
    for t in tests:
        feat = t.get("feature", ir.get("test_feature", "test"))
        by_feature.setdefault(feat, []).append(t)
    return [_emit_test_section_ts(feat_tests, feat_name, structs)
            for feat_name, feat_tests in by_feature.items()]


def _emit_variables_section_ts(ir: dict, structs: dict) -> list[str]:
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
        var_lines.append(_emit_variable(var, structs))
    for stmt in ir.get("statements", []):
        var_lines.append(_emit_expr(stmt, structs) + ";")
    return var_lines


def _emit_definitions_ts(ir: dict, structs: dict, enums: dict) -> list[str]:
    """Emit types, tasks, and functions."""
    sections = []
    for typ in ir["types"]:
        code = _emit_type(typ, enums, structs)
        if code:
            sections.append(code)
    src = ir.get("source_file")
    for task in ir.get("tasks", []):
        if not task.get("abstract"):
            sections.append(source_comment(task, src, "//") + "\n" + _emit_task_ts(task, structs, ir.get("uses", [])))
    async_fns = _compute_async_fns(ir["functions"])
    if ir.get("_force_async_all"):
        async_fns = {_make_function_name(fn["signature_parts"]) for fn in ir["functions"]
                     if not fn.get("abstract")}
        async_fns |= ir.get("_client_async_fns", set())
    for fn in ir["functions"]:
        if not fn.get("abstract"):
            sections.append(source_comment(fn, src, "//") + "\n" + _emit_function(fn, structs, async_fns, ir))
    return sections


def _apply_module_prefixes_ts(result: str, ir: dict) -> str:
    """Prefix cross-module function calls with their module name."""
    module_map = ir.get("module_map")
    current_module = ir.get("current_module")
    if module_map and current_module:
        for fn_name, mod_name in module_map.items():
            if mod_name != current_module:
                result = result.replace(f"{fn_name}(", f"{mod_name}.{fn_name}(")
    return result


def _maybe_prepend_context(sections: list[str], ir: dict):
    """Prepend context class if there are per-user variables."""
    user_vars = ir.get("_all_user_vars", [])
    if user_vars:
        sections.insert(0, _emit_context_class_ts(user_vars, ir))


_UNDEFINED_HELPER_TS = """\
function _raise_undefined(name: string): never {
    throw new Error(`function not defined: ${name}`);
}"""

_ZERO_RAISE_HELPER_TS = """\
class _ZeroRaise extends Error {
    zeroName: string;
    argsList: any[];
    constructor(name: string, args: any[] = []) {
        super(`${name}(${args.join(', ')})`);
        this.zeroName = name;
        this.argsList = args;
    }
}"""


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


def emit(ir: dict) -> str:
    """Emit TypeScript source code from a zero IR dict."""
    _check_compatibility(ir)
    structs, enums = _init_globals_ts(ir)
    sections = []
    # emit blackbox fallback for standalone builds (no platform prepend).
    # feature-modular builds have blackbox.ts prepended, so skip to avoid redeclaration.
    if ir.get("tasks") and not ir.get("module_map"):
        sections.append(
            "// blackbox fallback (standalone build)\n"
            "function _bb_record_stream(_name: string, _iter: any): any { return _iter; }\n"
            "function _bb_record_call(_name: string, _result: any): any { return _result; }"
        )
    if _has_concurrently(ir):
        sections.append(_CONCURRENTLY_HELPER_TS)
    if ir.get("errors"):
        sections.append(_UNDEFINED_HELPER_TS)
    if _has_raise(ir) and not ir.get("_suppress_zero_raise"):
        sections.append(_ZERO_RAISE_HELPER_TS)
    sections.extend(_emit_tests_sections_ts(ir, structs))
    _maybe_prepend_context(sections, ir)
    var_lines = _emit_variables_section_ts(ir, structs)
    if var_lines:
        sections.append("\n".join(var_lines))
    sections.extend(_emit_definitions_ts(ir, structs, enums))
    concrete_fns = [fn for fn in ir["functions"] if not fn.get("abstract")]
    sections.extend(_generate_dispatchers_ts(concrete_fns, ir.get("types", [])))
    result = "\n\n".join(sections) + "\n" if sections else ""
    return _apply_module_prefixes_ts(result, ir)


# --- tests ---

def _emit_single_test_ts(test: dict, feature_name: str, index: int, structs: dict) -> tuple:
    """Emit a single test function. Returns (lines, name, desc)."""
    name = f"test_{feature_name.replace('-', '_')}_{index}"
    call_code = _emit_expr(test["call"], structs)
    expected_code = _emit_expr(test["expected"], structs) if isinstance(test["expected"], dict) else repr(test["expected"])
    is_task = test["call"].get("kind") == "task_call"
    expected_is_array = (test["expected"].get("kind") == "array_lit" or
                         (test["expected"].get("kind") == "raw" and test["expected"].get("value", "").startswith("[")))
    if is_task and not expected_is_array:
        expected_code = f"[{expected_code}]"
    use_json = is_task or expected_is_array
    cmp = "JSON.stringify(_result) !== JSON.stringify(_expected)" if use_json else "_result !== _expected"
    desc = test.get("source_text", "")
    lines = [
        f"function {name}(): void {{",
        f"    // {desc}",
        f"    const _result = {call_code};",
        f"    const _expected = {expected_code};",
        f'    if ({cmp}) throw new Error(`expected ${{_expected}}, got ${{_result}}`);',
        "}",
        "",
    ]
    return lines, name, desc


def _emit_test_section_ts(tests: list[dict], feature_name: str, structs: dict) -> str:
    """Emit test functions and register them for a feature."""
    lines = []
    test_entries = []
    for i, test in enumerate(tests):
        test_lines, name, desc = _emit_single_test_ts(test, feature_name, i, structs)
        lines.extend(test_lines)
        test_entries.append((name, desc))
    entries = ", ".join(f"[{n}, {repr(d)}]" for n, d in test_entries)
    lines.append(f"register_tests({repr(feature_name)}, [{entries}]);")
    return "\n".join(lines)


# --- dispatchers ---

def _emit_single_dispatcher_ts(base: str, sorted_fns: list[dict]) -> str:
    """Generate a single dispatch function."""
    dispatcher_name = "fn_" + base
    param_names = [p["name"] for p in sorted_fns[0]["params"]]
    params_str = ", ".join(param_names)
    ret_type = _ts_type(sorted_fns[0]["result"]["type"]) if sorted_fns[0].get("result") else "void"
    lines = [f"function {dispatcher_name}({params_str}): {ret_type} {{"]
    for i, fn in enumerate(sorted_fns):
        specific_name = _make_function_name(fn["signature_parts"])
        param_type = fn["params"][0]["type"] if fn["params"] else None
        if param_type:
            keyword = "if" if i == 0 else "} else if"
            lines.append(f"    {keyword} ({param_names[0]} instanceof {param_type}) {{")
            lines.append(f"        return {specific_name}({params_str});")
    last_name = _make_function_name(sorted_fns[-1]["signature_parts"])
    lines.append(f"    }} else {{")
    lines.append(f"        return {last_name}({params_str});")
    lines.append(f"    }}")
    lines.append("}")
    return "\n".join(lines)


def _generate_dispatchers_ts(functions: list[dict], types: list[dict]) -> list[str]:
    """Generate dispatch functions for function groups with multiple definitions."""
    dispatch_groups = compute_dispatch_groups(functions, types)
    return [_emit_single_dispatcher_ts(base, sorted_fns)
            for base, sorted_fns in dispatch_groups.items()]


# --- naming helpers ---

def _ts_name(name: str) -> str:
    """Convert a zero name to a valid TS identifier (hyphens to underscores)."""
    return name.replace("-", "_")


def _ts_type(zero_type: str) -> str:
    """Map a zero type to its TS equivalent."""
    if zero_type in _TS_NUMERIC:
        return _TS_NUMERIC[zero_type]
    if re.match(r"(int|uint|float)\d+$", zero_type):
        return "number"
    if zero_type == "uint":
        return "number"
    return zero_type.replace("-", "_")


def _safe(name: str) -> str:
    """Convert a zero name to a TypeScript-safe name."""
    return name.replace("-", "_")


def _is_context_access_ts(object_name: str) -> bool:
    """Check if a member access is a user-context variable lookup."""
    return object_name in _context_features_ts


# --- types ---

def _qualify_default_ts(field: dict, enums: dict, structs: dict = None) -> str:
    """Qualify a field default with its enum/struct type name if needed."""
    structs = structs or {}
    default_val = field["default"]
    if field["type"] in structs:
        return f"{_ts_name(field['type'])}()"
    if field["type"] in enums:
        enum = enums[field["type"]]
        if str(default_val) in enum["values"]:
            ts_name = _ts_name(field["type"])
            return f"{ts_name}.{default_val}"
    return str(default_val)


def _emit_struct_type_ts(typ: dict, enums: dict, structs: dict) -> str:
    """Emit a struct as an interface + factory function."""
    name = _ts_name(typ["name"])
    all_fields = _collect_all_fields(typ, structs)
    iface_lines = [f"interface {name} {{"]
    for field in all_fields:
        iface_lines.append(f"    readonly {field['name']}: {_ts_type(field['type'])};")
    iface_lines.append("}")
    factory_args = []
    for field in all_fields:
        default = _qualify_default_ts(field, enums, structs)
        factory_args.append(f"{field['name']}: args.{field['name']} ?? {default}")
    factory_body = "{ " + ", ".join(factory_args) + " }"
    factory_lines = [
        f"function {name}(args: Partial<{name}> = {{}}): {name} {{",
        f"    return {factory_body};",
        "}",
    ]
    return "\n".join(iface_lines) + "\n\n" + "\n".join(factory_lines)


def _emit_type(typ: dict, enums: dict = None, structs: dict = None) -> str | None:
    """Emit a single type declaration."""
    enums = enums or {}
    structs = structs or {}
    if typ["kind"] == "numeric":
        ts_base = _ts_type(typ["base"])
        if typ["name"] in _BUILTINS:
            return None
        return f"type {_ts_name(typ['name'])} = {ts_base};"
    elif typ["kind"] == "enum":
        name = _ts_name(typ["name"])
        lines = [f"enum {name} {{"]
        for val in typ["values"]:
            lines.append(f'    {val} = "{val}",')
        lines.append("}")
        return "\n".join(lines)
    elif typ["kind"] == "struct":
        return _emit_struct_type_ts(typ, enums, structs)


# --- context class ---

def _emit_context_feature_classes(by_feature: dict) -> list[str]:
    """Generate per-feature nested context classes."""
    lines = []
    for feat_name in sorted(by_feature):
        safe_feat = _safe(feat_name)
        lines.append(f"class _Ctx_{safe_feat} {{")
        for v in by_feature[feat_name]:
            type_ann = _ts_type(v["type"])
            val = _emit_value(v["value"], v["type"], {})
            lines.append(f"    {_safe(v['name'])}: {type_ann} = {val};")
        lines.append("}")
        lines.append("")
    return lines


def _emit_context_wrapper(by_feature: dict) -> list[str]:
    """Generate the _Context wrapper class and accessor."""
    lines = ["class _Context {"]
    for feat_name in sorted(by_feature):
        safe_feat = _safe(feat_name)
        lines.append(f"    {safe_feat} = new _Ctx_{safe_feat}();")
    lines.append("}")
    lines.append("")
    lines.append("const _ctx_storage = new AsyncLocalStorage<_Context>();")
    lines.append("const _default_ctx = new _Context();")
    lines.append("")
    lines.append("function _get_ctx(): _Context {")
    lines.append("    return _ctx_storage.getStore() ?? _default_ctx;")
    lines.append("}")
    lines.append("")
    return lines


def _emit_context_class_ts(user_vars: list[dict], ir: dict) -> str:
    """Generate a _Context class from per-user variables, plus AsyncLocalStorage setup."""
    var_owners = ir.get("_var_owners", {})
    by_feature = {}
    for v in user_vars:
        feature = var_owners.get(v["name"], "default")
        by_feature.setdefault(feature, []).append(v)
    lines = ["import { AsyncLocalStorage } from 'async_hooks';", ""]
    lines.extend(_emit_context_feature_classes(by_feature))
    lines.extend(_emit_context_wrapper(by_feature))
    return "\n".join(lines)


# --- variables ---

def _emit_variable(var: dict, structs: dict) -> str:
    """Emit a variable declaration."""
    name = _safe(var["name"] + "_arr" if var["array"] else var["name"])
    type_ann = _ts_type(var["type"])
    if var["array"]:
        return _emit_array_variable(name, type_ann, var, structs)
    else:
        value_str = _emit_value(var["value"], var["type"], structs)
        return f"const {name}: {type_ann} = {value_str};"


def _emit_range_array_ts(name: str, type_ann: str, val: dict) -> str:
    """Emit a range-based array in TypeScript."""
    if val["range"] == "through":
        length = val["end"] - val["start"] + 1
    else:
        length = val["end"] - val["start"]
    start = val["start"]
    if start == 0:
        return f"const {name}: readonly {type_ann}[] = Array.from({{ length: {length} }}, (_, i) => i);"
    return f"const {name}: readonly {type_ann}[] = Array.from({{ length: {length} }}, (_, i) => i + {start});"


def _emit_dict_array_variable_ts(name: str, type_ann: str, val: dict, structs: dict) -> str | None:
    """Emit array variable when value is a dict."""
    if "range" in val:
        return _emit_range_array_ts(name, type_ann, val)
    kind = val.get("kind")
    if kind == "stream":
        return _emit_stream_ts(name, type_ann, val, structs)
    if kind == "task_call":
        return _emit_task_call_ts(name, type_ann, val)
    if kind in ("where", "sort"):
        return f"const {name}: {type_ann}[] = {_emit_expr(val, structs or {})};"
    if "kind" in val:
        return f"const {name}: readonly {type_ann}[] = {_emit_array_map_expr(val, structs)};"
    return None


def _emit_array_variable(name: str, type_ann: str, var: dict, structs: dict = None) -> str:
    """Emit an array variable declaration."""
    if var.get("map"):
        key_type = _ts_type(var["key_type"])
        return f"const {name}: Map<{key_type}, {type_ann}> = new Map();"
    val = var["value"]
    size = var["size"]
    if val is None and size is None:
        return f"const {name}: readonly {type_ann}[] = [];"
    elif val is None and size is not None:
        return f"const {name}: readonly {type_ann}[] = Array({size}).fill(0);"
    elif isinstance(val, (int, float)) and size is not None:
        return f"const {name}: readonly {type_ann}[] = Array({size}).fill({val});"
    elif isinstance(val, list):
        items = ", ".join(_emit_element(v, structs) for v in val)
        return f"const {name}: readonly {type_ann}[] = [{items}];"
    elif isinstance(val, dict):
        result = _emit_dict_array_variable_ts(name, type_ann, val, structs)
        if result:
            return result
    return f"const {name}: readonly {type_ann}[] = {val};"


# --- streams ---

def _emit_stream_loop_ts(name: str, steps: list, terminate: dict, emit_fn) -> list[str]:
    """Emit the loop portion of a streaming expression (TypeScript)."""
    lines = []
    for step in steps[1:-1]:
        lines.append(f"{name}.push({emit_fn(step)});")
    repeat_expr = emit_fn(steps[-1])
    if terminate["kind"] == "until":
        cond = emit_fn(terminate["condition"])
        lines.append(f"while (!({cond})) {{")
        lines.append(f"    {name}.push({repeat_expr});")
        lines.append("}")
    elif terminate["kind"] == "while":
        cond = emit_fn(terminate["condition"])
        lines.append(f"while ({cond}) {{")
        lines.append(f"    {name}.push({repeat_expr});")
        lines.append("}")
    return lines


def _emit_stream_ts(name: str, type_ann: str, stream: dict, structs: dict = None) -> str:
    """Emit a streaming expression in TypeScript."""
    steps = stream["steps"]
    terminate = stream["terminate"]
    last = f"{name}[{name}.length - 1]"

    def _emit_ts_stream_expr(node):
        s = _emit_expr(node, structs or {})
        return s.replace("__last__", last)

    if terminate is None and all(s.get("kind") == "literal" for s in steps):
        items = ", ".join(str(s["value"]) for s in steps)
        return f"const {name}: {type_ann}[] = [{items}];"
    lines = [f"const {name}: {type_ann}[] = [{_emit_ts_stream_expr(steps[0])}];"]
    if len(steps) > 1:
        if terminate is None:
            for step in steps[1:]:
                lines.append(f"{name}.push({_emit_ts_stream_expr(step)});")
        else:
            lines.extend(_emit_stream_loop_ts(name, steps, terminate, _emit_ts_stream_expr))
    return "\n".join(lines)


def _emit_task_call_ts(name: str, type_ann: str, call: dict) -> str:
    """Emit a task call — spread generator into array."""
    fn_name = make_task_call_fn_name(call)
    args = ", ".join(_coerce_task_arg(a, t, call) for a, t in
                     zip(call["args"], _task_call_param_types(call)))
    return f"const {name}: {type_ann}[] = [...{fn_name}({args})];"


def _task_call_param_types(call: dict) -> list[str]:
    """Extract param types from a task_call's signature_parts."""
    return [p.strip("()") for p in call["signature_parts"] if p.startswith("(")]


def _coerce_task_arg(arg: str, param_type: str, call: dict) -> str:
    """Coerce a task call argument — spread strings into char arrays for char params."""
    if param_type == "char":
        base = arg[:-1] if arg.endswith("$") else arg
        return f"[...{base}]"
    name = arg.replace("$", "_arr") if arg.endswith("$") else arg
    return name


def _emit_element(v, structs: dict = None) -> str:
    """Emit a single array element — raw value or AST node."""
    if isinstance(v, dict) and "kind" in v:
        return _emit_expr(v, structs or {})
    return str(v)


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


def _rewrite_index_with_underscore_ts(node: dict, structs: dict) -> dict:
    """Rewrite arr$[_ - 1] to a bounds-checked index expression (TypeScript)."""
    arr_node = node["array"]
    if arr_node.get("kind") == "name" and arr_node["value"].endswith("$"):
        arr_str = arr_node["value"].replace("$", "_arr")
    else:
        arr_str = _emit_expr(arr_node, structs)
    idx = _rewrite_for_indexed_ts(node["index"], structs)
    idx_str = _emit_expr(idx, structs)
    return {"kind": "raw", "value": f"({idx_str} >= 0 ? {arr_str}[{idx_str}] : 0)"}


def _rewrite_for_indexed_ts(node: dict, structs: dict) -> dict:
    """Rewrite an expression for index-based iteration (TypeScript)."""
    if not isinstance(node, dict):
        return node
    kind = node.get("kind")
    if kind == "index" and _contains_name(node.get("index", {}), "_"):
        return _rewrite_index_with_underscore_ts(node, structs)
    if kind == "name" and node.get("value", "").endswith("$"):
        ts_name = node["value"].replace("$", "_arr")
        return {"kind": "raw", "value": f"{ts_name}[_i]"}
    if kind == "name" and node.get("value") == "_":
        return {"kind": "raw", "value": "_i"}
    result = {}
    for key, val in node.items():
        if isinstance(val, dict):
            result[key] = _rewrite_for_indexed_ts(val, structs)
        elif isinstance(val, list):
            result[key] = [_rewrite_for_indexed_ts(v, structs) if isinstance(v, dict) else v for v in val]
        else:
            result[key] = val
    return result


def _emit_indexed_map_expr_ts(node: dict, structs: dict) -> str:
    """Emit index-based array mapping for expressions using _-relative indexing."""
    array_refs = list(dict.fromkeys(_collect_array_refs(node)))
    if array_refs:
        len_src = array_refs[0].replace("$", "_arr")
    else:
        len_src = _find_length_source(node)
        if not len_src:
            return _emit_expr(node, structs)
    rewritten = _rewrite_for_indexed_ts(node, structs)
    expr = _emit_expr(rewritten, structs)
    return f"Array.from({{ length: {len_src}.length }}, (_, _i) => {expr})"


def _find_length_source(node) -> str | None:
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


def _emit_multi_array_map_ts(node: dict, array_refs: list[str], structs: dict) -> str:
    """Emit Array.from-based mapping for multi-array expressions."""
    ts_arrs = [a.replace("$", "_arr") for a in array_refs]
    rewritten = node
    for arr, ts_arr in zip(array_refs, ts_arrs):
        rewritten = _rewrite_array_ref(rewritten, arr, f"({ts_arr}[i] ?? 0)")
    expr = _emit_expr(rewritten, structs)
    lengths = ", ".join(f"{a}.length" for a in ts_arrs)
    return f"Array.from({{ length: Math.max({lengths}) }}, (_, i) => {expr})"


def _emit_array_map_expr(node: dict, structs: dict) -> str:
    """Emit array-mapped expressions using .map() or Array.from()."""
    if _has_index_underscore(node):
        return _emit_indexed_map_expr_ts(node, structs)
    array_refs = list(dict.fromkeys(_collect_array_refs(node)))
    if len(array_refs) == 0:
        return _emit_expr(node, structs)
    if len(array_refs) == 1:
        arr_name = array_refs[0]
        ts_arr = arr_name.replace("$", "_arr")
        expr = _emit_expr(_rewrite_array_ref(node, arr_name, "x"), structs)
        return f"{ts_arr}.map(x => {expr})"
    return _emit_multi_array_map_ts(node, array_refs, structs)


# --- values ---

def _emit_value(value, zero_type: str, structs: dict) -> str:
    """Emit a value expression."""
    if isinstance(value, dict) and value.get("kind") == "reduce":
        return _emit_reduce(value, zero_type)
    if isinstance(value, dict) and value.get("kind") == "call":
        return _emit_call_value(value, structs)
    if isinstance(value, dict) and "kind" in value:
        return _emit_expr(value, structs)
    if isinstance(value, bool):
        return "true" if value else "false"
    return str(value)


def _emit_reduce(node: dict, zero_type: str) -> str:
    """Emit a .reduce() call."""
    arr = node["array"].replace("$", "_arr")
    if "op" in node:
        return f"{arr}.reduce((a, b) => a {node['op']} b)"
    else:
        fn_name = _make_function_name_from_reduce(node["fn_parts"], zero_type)
        return f"{arr}.reduce({fn_name})"


def _emit_call_value(call: dict, structs: dict) -> str:
    """Emit a constructor/call value."""
    name = call["name"]
    if not call["args"]:
        return f"{name}()"
    if isinstance(call["args"][0], dict) and "name" in call["args"][0]:
        parts = [f"{a['name']}: {a['value']}" for a in call["args"]]
        return f"{name}({{ {', '.join(parts)} }})"
    else:
        if name in structs:
            fields = structs[name]["fields"]
            parts = [f"{fields[i]['name']}: {call['args'][i]}" for i in range(len(call["args"]))]
            return f"{name}({{ {', '.join(parts)} }})"
        parts = [str(a) for a in call["args"]]
        return f"{name}({', '.join(parts)})"


# --- async detection ---

def _find_directly_async_fns(fn_names: dict) -> set[str]:
    """Find functions that directly contain concurrently blocks."""
    async_set = set()
    for name, fn in fn_names.items():
        for stmt in fn.get("body", []):
            if isinstance(stmt, dict) and stmt.get("kind") == "concurrently":
                async_set.add(name)
                break
    return async_set


def _calls_any_of(stmt, fn_names: set[str]) -> bool:
    """Check if a statement calls any function in the given set."""
    if not isinstance(stmt, dict):
        return False
    if stmt.get("kind") == "call":
        if f"fn_{stmt.get('name', '')}" in fn_names:
            return True
    if stmt.get("kind") == "fn_call":
        name = _make_function_name(stmt.get("signature_parts", []))
        if name in fn_names:
            return True
    if stmt.get("kind") == "raw":
        val = stmt.get("value", "")
        for fn_name in fn_names:
            short = fn_name[3:] if fn_name.startswith("fn_") else fn_name
            if short + "()" in val or short + "(" in val:
                return True
    return False


def _compute_async_fns(functions: list[dict]) -> set[str]:
    """Compute which functions need to be async."""
    fn_names = {_make_function_name(fn["signature_parts"]): fn for fn in functions}
    async_set = _find_directly_async_fns(fn_names)
    changed = True
    while changed:
        changed = False
        for name, fn in fn_names.items():
            if name in async_set:
                continue
            for stmt in fn.get("body", []):
                if _calls_any_of(stmt, async_set):
                    async_set.add(name)
                    changed = True
                    break
    return async_set


# --- control flow ---

def _emit_if_block_ts(node: dict, structs: dict, result_type: str = "void", async_fns: set = None, result_var: str = None) -> str:
    """Emit an if/else if/else block in TypeScript."""
    lines = []
    for i, branch in enumerate(node["branches"]):
        if i == 0:
            cond = _emit_expr(branch["condition"], structs)
            lines.append(f"if ({cond}) {{")
        elif branch["condition"] is not None:
            cond = _emit_expr(branch["condition"], structs)
            lines.append(f"}} else if ({cond}) {{")
        else:
            lines.append("} else {")
        for stmt in branch["body"]:
            lines.append(f"    {_emit_stmt(stmt, result_type, structs, async_fns, result_var=result_var)}")
    lines.append("}")
    return "\n".join(lines)


def _emit_task_if_branch_body(branch: dict, structs: dict) -> list[str]:
    """Emit the body of a single if/else branch in a task."""
    lines = []
    for stmt in branch["body"]:
        kind = stmt.get("kind", "")
        if kind == "if_block":
            for bl in _emit_task_if_block_ts(stmt, structs).split("\n"):
                lines.append(f"    {bl}")
        elif kind == "assign":
            lines.append(f"    {stmt['target']} = {_emit_expr(stmt['value'], structs)};")
        elif kind == "emit":
            lines.append(f"    yield {_emit_expr(stmt['value'], structs)};")
        else:
            lines.append(f"    {_emit_expr(stmt, structs)};")
    return lines


def _emit_task_if_block_ts(node: dict, structs: dict = None) -> str:
    """Emit an if/else block in a task body."""
    structs = structs or {}
    lines = []
    for i, branch in enumerate(node["branches"]):
        if i == 0:
            cond = _emit_expr(branch["condition"], structs)
            lines.append(f"if ({cond}) {{")
        elif branch["condition"] is not None:
            cond = _emit_expr(branch["condition"], structs)
            lines.append(f"}} else if ({cond}) {{")
        else:
            lines.append("} else {")
        lines.extend(_emit_task_if_branch_body(branch, structs))
    lines.append("}")
    return "\n".join(lines)


def _platform_stream_fn_ts(stream_name: str, uses: list[dict]) -> str:
    """Map a platform stream name to its TypeScript handler function name."""
    for use in uses:
        if use["name"] == stream_name:
            return f"_push_{use['platform']}_{use['name'].replace('$', '')}"
    return f"_push_{stream_name.replace('$', '')}"


# --- tasks ---

def _emit_task_header_ts(task: dict) -> tuple:
    """Build function signature for a task. Returns (fn_name, params_str, opening_line)."""
    all_params = task.get("input_streams", []) + task.get("params", [])
    param_strs = []
    for p in all_params:
        pname = p["name"] + "_arr" if p in task.get("input_streams", []) else p["name"]
        ts_type = _ts_type(p["type"])
        param_strs.append(f"{pname}: readonly {ts_type}[]" if p in task.get("input_streams", []) else f"{pname}: {ts_type}")
    fn_name = "task_" + "_".join(task["name_parts"])
    for p in all_params:
        fn_name += f"__{p['type']}"
    params_str = ", ".join(param_strs)
    is_void = task["output"] is None
    has_platform_streams = bool(task.get("platform_streams"))
    if is_void:
        if has_platform_streams:
            opening = f"async function {fn_name}({params_str}): Promise<void> {{"
        else:
            opening = f"function {fn_name}({params_str}): void {{"
    else:
        output_type = _ts_type(task["output"]["type"])
        opening = f"function* {fn_name}({params_str}): Generator<{output_type}> {{"
    return fn_name, params_str, opening


def _emit_task_for_each_ts(node: dict, structs: dict, uses: list, has_platform_streams: bool,
                           base_indent: str, extra_indent: str) -> list[str]:
    """Emit a for_each block inside a task (TypeScript).
    Wraps the iterator with _bb_record_stream for flight recording."""
    lines = []
    iter_expr = _emit_expr(node["iter"], structs)
    iter_name = iter_expr.replace("_arr", "$") if iter_expr.endswith("_arr") else iter_expr
    for_keyword = "for await" if has_platform_streams else "for"
    lines.append(f'{base_indent}{extra_indent}{for_keyword} (const {node["name"]} of _bb_record_stream("{iter_name}", {iter_expr})) {{')
    loop_indent = base_indent + extra_indent + "    "
    for body_node in node["body"]:
        bk = body_node.get("kind", "")
        if bk == "emit":
            lines.append(f"{loop_indent}yield {_emit_expr(body_node['value'], structs)};")
        elif bk == "emit_external":
            handler = _platform_stream_fn_ts(body_node["stream"], uses)
            lines.append(f"{loop_indent}{handler}({_emit_expr(body_node['value'], structs)});")
        elif bk == "if_block":
            for bl in _emit_task_if_block_ts(body_node, structs).split("\n"):
                lines.append(f"{loop_indent}{bl}")
        elif bk == "assign":
            lines.append(f"{loop_indent}{body_node['target']} = {_emit_expr(body_node['value'], structs)};")
        else:
            lines.append(f"{loop_indent}{_emit_expr(body_node, structs)};")
    lines.append(f"{base_indent}{extra_indent}}}")
    return lines


def _emit_task_consume_ts(node: dict, structs: dict, base_indent: str) -> str:
    """Emit a consume or consume_call loop header (TypeScript).
    Wraps the iterator with _bb_record_stream for flight recording."""
    kind = node.get("kind")
    if kind == "consume":
        stream_name = node["stream"].replace("$", "_arr")
        return f'{base_indent}for (const {node["name"]} of _bb_record_stream("{stream_name}", {stream_name})) {{'
    call = node["call"]
    if call.get("kind") == "task_call":
        call_fn = make_task_call_fn_name(call)
        args = ", ".join(_coerce_task_arg(a, t, call) for a, t in
                         zip(call["args"], _task_call_param_types(call)))
        return f'{base_indent}for (const {node["name"]} of _bb_record_stream("{call_fn}", {call_fn}({args}))) {{'
    iter_expr = _emit_expr(call, structs)
    return f'{base_indent}for (const {node["name"]} of _bb_record_stream("stream", {iter_expr})) {{'


def _emit_task_var_decl_ts(node: dict, structs: dict, base_indent: str, extra_indent: str) -> str:
    """Emit a var_decl inside a task body (TypeScript)."""
    name = node["name"] + "_arr" if node.get("array") else node["name"]
    val = node.get("value")
    if node.get("array") and isinstance(val, dict) and val.get("kind") == "task_call":
        call_fn = make_task_call_fn_name(val)
        args = ", ".join(_coerce_task_arg(a, t, val) for a, t in
                         zip(val["args"], _task_call_param_types(val)))
        return f"{base_indent}{extra_indent}const {name} = {call_fn}({args});"
    if node.get("array") and isinstance(val, list):
        items = ", ".join(_emit_expr(v, structs) if isinstance(v, dict) else str(v) for v in val)
        return f"{base_indent}{extra_indent}const {name} = [{items}];"
    if isinstance(val, dict):
        return f"{base_indent}{extra_indent}let {name} = {_emit_expr(val, structs)};"
    return f"{base_indent}{extra_indent}let {name} = {val};"


def _emit_task_body_node_ts(node, structs, uses, has_platform_streams, base_indent, extra_indent):
    """Emit a single task body node. Returns (lines, new_extra_indent)."""
    kind = node.get("kind")
    if kind == "for_each":
        return _emit_task_for_each_ts(node, structs, uses, has_platform_streams, base_indent, extra_indent), extra_indent
    if kind in ("consume", "consume_call"):
        return [_emit_task_consume_ts(node, structs, base_indent)], "    "
    if kind == "emit":
        return [f"{base_indent}{extra_indent}yield {_emit_expr(node['value'], structs)};"], extra_indent
    if kind == "emit_external":
        handler = _platform_stream_fn_ts(node["stream"], uses)
        return [f"{base_indent}{extra_indent}{handler}({_emit_expr(node['value'], structs)});"], extra_indent
    if kind == "if_block":
        block_lines = [f"{base_indent}{extra_indent}{bl}"
                       for bl in _emit_task_if_block_ts(node, structs).split("\n")]
        return block_lines, extra_indent
    if kind == "var_decl":
        return [_emit_task_var_decl_ts(node, structs, base_indent, extra_indent)], extra_indent
    if kind == "assign":
        return [f"{base_indent}{extra_indent}{node['target']} = {_emit_expr(node['value'], structs)};"], extra_indent
    return [f"{base_indent}{extra_indent}{_emit_expr(node, structs)};"], extra_indent


def _emit_task_ts(task: dict, structs: dict = None, uses: list[dict] = None) -> str:
    """Emit a task as a TypeScript generator or plain function."""
    uses = uses or []
    structs = structs or {}
    fn_name, params_str, opening = _emit_task_header_ts(task)
    has_platform_streams = bool(task.get("platform_streams"))
    lines = [opening]
    base_indent = "    "
    extra_indent = ""
    for node in task["body"]:
        if isinstance(node, str):
            lines.append(f"{base_indent}{extra_indent}{node}")
            continue
        new_lines, extra_indent = _emit_task_body_node_ts(
            node, structs, uses, has_platform_streams, base_indent, extra_indent
        )
        lines.extend(new_lines)
    if extra_indent:
        lines.append(f"{base_indent}}}")
    lines.append("}")
    return "\n".join(lines)


# --- functions ---

def _assigns_result_ts(stmt: dict, result_var: str) -> bool:
    """Check if a statement assigns to the result variable."""
    if stmt.get("kind") == "assign" and stmt.get("target") == result_var:
        return True
    if stmt.get("kind") == "if_block":
        for branch in stmt.get("branches", []):
            for s in branch.get("body", []):
                if _assigns_result_ts(s, result_var):
                    return True
    return False


def _result_needs_guard_ts(body: list[dict], result_var: str) -> bool:
    """Check if a function body needs result-assignment guards."""
    assign_count = sum(1 for s in body if _assigns_result_ts(s, result_var))
    has_conditional = any(s.get("kind") == "if_block" and _assigns_result_ts(s, result_var) for s in body)
    return assign_count > 1 and has_conditional


def _emit_guarded_body_ts(fn_body, result_var, ret_type, needs_guard, structs, async_fns) -> list[str]:
    """Emit function body with result-assignment guards (TypeScript)."""
    lines = []
    multi_assign = sum(1 for s in fn_body if _assigns_result_ts(s, result_var)) > 1
    rv = result_var if needs_guard or multi_assign or any(
        isinstance(s, dict) and s.get("kind") == "if_block" for s in fn_body
    ) else None
    seen_conditional = False
    for stmt in fn_body:
        code = _emit_stmt(stmt, ret_type, structs, async_fns, result_var=rv)
        if needs_guard and _assigns_result_ts(stmt, result_var):
            if seen_conditional:
                lines.append(f"    if ({result_var} === undefined) {{")
                lines.append(f"        {code}")
                lines.append(f"    }}")
            else:
                lines.append(f"    {code}")
                if stmt.get("kind") == "if_block":
                    seen_conditional = True
        else:
            lines.append(f"    {code}")
    return lines


def _emit_result_function_ts(fn, name, params, ret_type, result_var, prefix, structs, async_fns, handlers=None) -> str:
    """Emit a function that returns a result (TypeScript)."""
    has_if = any(isinstance(s, dict) and s.get("kind") == "if_block" for s in fn["body"])
    needs_guard = _result_needs_guard_ts(fn["body"], result_var)
    multi_assign = sum(1 for s in fn["body"] if _assigns_result_ts(s, result_var)) > 1
    lines = [f"{prefix} {name}({params}): {ret_type} {{"]
    if has_if or needs_guard or multi_assign:
        lines.append(f"    let {result_var}: {ret_type} = undefined!;")
    body_lines = _emit_guarded_body_ts(fn["body"], result_var, ret_type, needs_guard, structs, async_fns)
    body_lines.append(f"    return {result_var};")
    lines.extend(_wrap_with_handlers_ts(body_lines, handlers or []))
    lines.append("}")
    return "\n".join(lines)


def _emit_function_with_hooks_ts(fn, name, params, prefix, structs, async_fns):
    """Emit a TS function that contains scoped hooks."""
    hooks = [s for s in fn["body"] if isinstance(s, dict) and s.get("kind") == "scoped_hook"]
    stmts = [s for s in fn["body"] if not (isinstance(s, dict) and s.get("kind") == "scoped_hook")]

    lines = [f"{prefix} {name}({params}): void {{"]

    # collect hooks by hooked function
    hooked_fns = {}
    for h in hooks:
        hook_fn = "fn_" + h["hook_fn"].replace(" ", "_").replace("-", "_") + "__string"
        hooked_fns.setdefault(hook_fn, []).append(h)

    # save originals and create patched versions
    for hook_fn, hook_list in hooked_fns.items():
        lines.append(f"    const _orig_{hook_fn} = {hook_fn};")
        # build the patched function
        lines.append(f"    const _patched = async function(_prompt: any) {{")
        for h in hook_list:
            args = h["hook_args"]
            arg_check = f"_prompt === {repr(args[0])}" if args else "true"
            body_lines = [_emit_expr(s, structs) for s in h["body"]]
            lines.append(f"        if ({arg_check}) {{")
            lines.append(f"            setTimeout(async () => {{")
            for bl in body_lines:
                lines.append(f"                {bl};")
            lines.append(f"            }}, 500);")
            lines.append(f"        }}")
        lines.append(f"        return _orig_{hook_fn}(_prompt);")
        lines.append(f"    }};")
        lines.append(f"    (globalThis as any).{hook_fn} = _patched;")

    # try/finally
    lines.append(f"    try {{")
    for stmt in stmts:
        code = _emit_stmt(stmt, "void", structs, async_fns)
        lines.append(f"        {code}")
    lines.append(f"    }} finally {{")
    for hook_fn in hooked_fns:
        lines.append(f"        (globalThis as any).{hook_fn} = _orig_{hook_fn};")
    lines.append(f"    }}")
    lines.append("}")
    return "\n".join(lines)


def _build_fn_params_ts(fn: dict) -> str:
    """Build the parameter string for a function."""
    param_strs = []
    for p in fn["params"]:
        pname = p["name"].replace("$", "_arr")
        if p["type"] is not None:
            param_strs.append(f"{pname}: {_ts_type(p['type'])}")
        else:
            param_strs.append(pname)
    return ", ".join(param_strs)


def _get_handlers_for_ts(fn_name: str, handlers: list) -> list:
    """Get handler bindings for a function by matching its word parts."""
    fn_words = fn_name.replace("fn_", "", 1).split("__")[0]
    result = []
    for h in handlers:
        target = h["target_fn"].replace(" ", "_")
        if fn_words == target:
            result.append(h)
    return result


def _wrap_with_handlers_ts(body_lines: list[str], handlers: list) -> list[str]:
    """Wrap function body lines in try/catch for handler bindings (TypeScript)."""
    if not handlers:
        return body_lines
    wrapped = ["    try {"]
    for line in body_lines:
        wrapped.append("    " + line)
    wrapped.append("    } catch (_e) {")
    wrapped.append("        if (_e instanceof _ZeroRaise) {")
    for i, h in enumerate(handlers):
        handler_fn = "fn_" + h["handler_name"].replace(" ", "_")
        for p in h["params"]:
            handler_fn += f"__{_safe(p['type'])}"
        args = ", ".join(f"_e.argsList[{j}]" for j in range(len(h["params"])))
        keyword = "if" if i == 0 else "} else if"
        wrapped.append(f'            {keyword} (_e.zeroName === {repr(h["handler_name"])}) {{')
        wrapped.append(f"                {handler_fn}({args});")
    wrapped.append("            } else { throw _e; }")
    wrapped.append("        } else { throw _e; }")
    wrapped.append("    }")
    return wrapped


def _emit_function(fn: dict, structs: dict, async_fns: set[str] = None, ir: dict = None) -> str:
    """Emit a function definition."""
    async_fns = async_fns or set()
    ir = ir or {}
    name = _make_function_name(fn["signature_parts"])
    params = _build_fn_params_ts(fn)
    prefix = "async function" if name in async_fns else "function"
    handlers = _get_handlers_for_ts(name, ir.get("handlers", []))
    if fn["result"] is not None:
        ret_type = _ts_type(fn["result"]["type"])
        result_var = fn["result"]["name"]
        if result_var.endswith("$"):
            result_var = result_var.replace("$", "_arr")
            ret_type = f"{ret_type}[]"
        return _emit_result_function_ts(fn, name, params, ret_type, result_var, prefix, structs, async_fns, handlers)
    if any(isinstance(s, dict) and s.get("kind") == "scoped_hook" for s in fn["body"]):
        return _emit_function_with_hooks_ts(fn, name, params, prefix, structs, async_fns)
    lines = [f"{prefix} {name}({params}): void {{"]
    body_lines = []
    for stmt in fn["body"]:
        body_lines.append(f"    {_emit_stmt(stmt, 'void', structs, async_fns)}")
    lines.extend(_wrap_with_handlers_ts(body_lines, handlers))
    lines.append("}")
    return "\n".join(lines)


# --- statement emitter ---

def _emit_async_check(node: dict, structs: dict, async_fns: set) -> str | None:
    """Check if a statement calls an async function; return awaited form if so."""
    if node["kind"] == "call" and f"fn_{node['name']}" in async_fns:
        return f"await {_emit_expr(node, structs)};"
    if node["kind"] == "fn_call":
        fn_name = _make_function_name(node.get("signature_parts", []))
        if fn_name in async_fns:
            return f"await {_emit_expr(node, structs)};"
    if node["kind"] == "raw":
        val = node["value"]
        for fn_name in async_fns:
            short = fn_name[3:] if fn_name.startswith("fn_") else fn_name
            if val.strip().startswith(short + "(") or val.strip() == short + "()":
                return f"await {_emit_expr(node, structs)};"
    return None


def _emit_stmt(node: dict, result_type: str, structs: dict, async_fns: set[str] = None, result_var: str = None) -> str:
    """Emit a TypeScript statement from an AST node."""
    async_fns = async_fns or set()
    if node["kind"] == "if_block":
        return _emit_if_block_ts(node, structs, result_type, async_fns, result_var)
    if node["kind"] == "concurrently":
        return _emit_concurrently_ts(node, structs)
    if node["kind"] == "assign":
        if "$[" in node["target"]:
            return _emit_assign_expr_ts(node, structs) + ";"
        value = _emit_expr(node["value"], structs)
        if result_var and node["target"] == result_var:
            return f"{node['target']} = {value};"
        return f"const {node['target']}: {result_type} = {value};"
    async_result = _emit_async_check(node, structs, async_fns)
    if async_result:
        return async_result
    return _emit_expr(node, structs) + ";"


def _emit_concurrently_ts(node: dict, structs: dict) -> str:
    """Emit a concurrently block as Promise.all."""
    block_calls = []
    for block in node["blocks"]:
        if len(block) == 1:
            block_calls.append(f"() => {block[0]}")
        else:
            stmts = "; ".join(block)
            block_calls.append(f"() => {{ {stmts} }}")
    return f"await _concurrently({', '.join(block_calls)});"


# --- expression emitter ---

def _emit_call_expr_ts(node: dict, structs: dict) -> str:
    """Emit a call expression, handling struct constructors."""
    name = node["name"]
    safe_name = _safe(name)
    if name in structs:
        args = node["args"]
        parts = []
        fields = structs[name]["fields"]
        for i, a in enumerate(args):
            if isinstance(a, dict) and a.get("kind") == "raw" and "=" in a.get("value", ""):
                k, v = a["value"].split("=", 1)
                parts.append(f"{k.strip()}: {v.strip()}")
            else:
                parts.append(f"{fields[i]['name']}: {_emit_expr(a, structs)}")
        return f"{safe_name}({{ {', '.join(parts)} }})"
    args = ", ".join(_emit_expr(a, structs) for a in node["args"])
    return f"{safe_name}({args})"


def _emit_sort_expr_ts(node: dict, structs: dict) -> str:
    """Emit a sort expression."""
    arr = _emit_expr(node["array"], structs)
    if node["key"] is None:
        result = f"[...{arr}].sort()"
    else:
        key = _emit_expr(_replace_underscore(node["key"], "a"), structs)
        key_b = _emit_expr(_replace_underscore(node["key"], "b"), structs)
        result = f"[...{arr}].sort((a, b) => {key} - {key_b})"
    if node.get("descending"):
        result += ".reverse()"
    return result


def _emit_filter_expr_ts(node: dict, structs: dict) -> str:
    """Emit where/first_where/index_of_first_where/indices_where."""
    kind = node["kind"]
    arr = _emit_expr(node["array"], structs)
    cond = _emit_expr(_replace_underscore(node["condition"], "x"), structs)
    if kind == "where":
        return f"{arr}.filter(x => {cond})"
    if kind == "first_where":
        return f"{arr}.find(x => {cond})!"
    if kind == "index_of_first_where":
        return f"{arr}.findIndex(x => {cond})"
    return f"{arr}.map((x, i) => ({cond}) ? i : -1).filter(i => i >= 0)"


def _emit_slice_expr_ts(node: dict, structs: dict) -> str:
    """Emit a slice expression."""
    arr = _emit_expr(node["array"], structs)
    start = _emit_expr(node["start"], structs) if node["start"] is not None else "0"
    if node["end"] is not None:
        end = _emit_expr(node["end"], structs)
        if node["inclusive"]:
            if isinstance(node["end"].get("value"), int):
                end = str(node["end"]["value"] + 1)
            else:
                end = f"{end} + 1"
        return f"{arr}.slice({start}, {end})"
    return f"{arr}.slice({start})"


def _emit_member_expr_ts(node: dict) -> str:
    """Emit a member access expression."""
    if node["field"] == "char$":
        return f"[...{node['object']}]"
    if _is_context_access_ts(node["object"]):
        return f"_get_ctx().{_safe(node['object'])}.{_safe(node['field'])}"
    return f"{_safe(node['object'])}.{_safe(node['field'])}"


def _emit_name_expr_ts(node: dict) -> str:
    """Emit a name reference."""
    val = node["value"]
    if val in _enum_values:
        return _enum_values[val]
    if val.endswith("$"):
        return _safe(val.replace("$", "_arr"))
    return _safe(val)


def _emit_literal_expr_ts(node: dict) -> str:
    """Emit a literal value."""
    v = node["value"]
    if v is True:
        return "true"
    if v is False:
        return "false"
    return str(v)


def _emit_reduce_expr_ts(node: dict) -> str:
    """Emit a reduce expression."""
    arr = node["array"].replace("$", "_arr")
    if "op" in node:
        return f"{arr}.reduce((a, b) => a {node['op']} b)"
    fn_name = _make_function_name_from_reduce(node["fn_parts"], "int")
    return f"{arr}.reduce({fn_name})"


def _emit_range_expr_ts(name: str, r: dict) -> str:
    """Emit a const range array declaration in an expression context."""
    if r["range"] == "through":
        length = r["end"] - r["start"] + 1 if isinstance(r["end"], int) else f"{r['end']} - {r['start']} + 1"
    else:
        length = r["end"] - r["start"] if isinstance(r["end"], int) else f"{r['end']} - {r['start']}"
    start = r["start"]
    if isinstance(length, int) and start == 0:
        return f"const {name} = Array.from({{ length: {length} }}, (_, i) => i)"
    return f"const {name} = Array.from({{ length: {length} }}, (_, i) => i + {start})"


def _emit_var_decl_array_ts(name: str, node: dict, structs: dict) -> str:
    """Emit an array variable declaration inside an expression context (TypeScript)."""
    val = node["value"]
    if isinstance(val, list):
        items = ", ".join(_emit_expr(v, structs) if isinstance(v, dict) else str(v) for v in val)
        return f"const {name} = [{items}]"
    if isinstance(val, dict) and "range" in val:
        return _emit_range_expr_ts(name, val)
    if isinstance(val, dict) and val.get("kind") == "task_call":
        call_fn = make_task_call_fn_name(val)
        args = ", ".join(_coerce_task_arg(a, t, val) for a, t in
                         zip(val["args"], _task_call_param_types(val)))
        return f"const {name} = [...{call_fn}({args})]"
    if isinstance(val, dict) and val.get("kind") == "stream" \
            and len(val.get("steps", [])) == 1 and val.get("terminate") is None:
        return f"const {name} = {_emit_array_map_expr(val['steps'][0], structs)}"
    if isinstance(val, dict) and val.get("kind") == "fn_call" \
            and any(isinstance(a, dict) and a.get("kind") == "array_fn_call" for a in val.get("args", [])):
        fn_name = _make_function_name(val["signature_parts"])
        inner = _emit_expr(val["args"][0], structs)
        return f"const {name} = {inner}.map(x => {fn_name}(x))"
    if isinstance(val, dict) and "kind" in val:
        return f"const {name} = {_emit_array_map_expr(val, structs)}"
    return f"const {name} = {val}"


def _needs_await(value_node):
    """Check if a value expression needs await (async gui platform call).
    RPC targets are excluded — _emit_rpc_call already adds await."""
    if not isinstance(value_node, dict):
        return False
    if value_node.get("kind") == "fn_call":
        fn_name = _make_function_name(value_node["signature_parts"])
        return fn_name in _client_async_fns_ts and fn_name not in _rpc_targets_ts
    return False


def _emit_var_decl_expr_ts(node: dict, structs: dict) -> str:
    """Emit a var_decl expression."""
    name = node["name"] + "_arr" if node.get("array") else node["name"]
    if node.get("array"):
        return _emit_var_decl_array_ts(name, node, structs)
    value_expr = _emit_expr(node['value'], structs)
    if _needs_await(node.get('value')):
        return f"const {name} = await {value_expr}"
    return f"const {name} = {value_expr}"


def _emit_assign_expr_ts(node: dict, structs: dict) -> str:
    """Emit an assignment expression."""
    target = node['target']
    value = _emit_expr(node['value'], structs)
    if "$[" in target:
        parts = target.split("$[", 1)
        arr_name = _safe(parts[0]) + "_arr"
        key = _safe(parts[1].rstrip("]"))
        return f"{arr_name}.set({key}, {value})"
    return f"{_safe(target)} = {value}"


def _emit_task_call_expr_ts(node: dict) -> str:
    """Emit a task call expression (spread generator)."""
    fn_name = make_task_call_fn_name(node)
    args = ", ".join(_coerce_task_arg(a, t, node) for a, t in
                     zip(node["args"], _task_call_param_types(node)))
    return f"[...{fn_name}({args})]"


def _emit_rpc_call(node: dict, structs: dict) -> str:
    """Emit a function call as an RPC fetch to /@rpc/."""
    sig_parts = node["signature_parts"]
    args = node["args"]
    # build the zero-syntax call string: word (arg) word (arg) ...
    rpc_parts = []
    arg_idx = 0
    for part in sig_parts:
        if part.startswith("(") or part.startswith("["):
            arg_expr = _emit_expr(args[arg_idx], structs)
            rpc_parts.append(f'" + encodeURIComponent("(" + {arg_expr} + ")") + "')
            arg_idx += 1
        else:
            rpc_parts.append(part)
    rpc_expr = " ".join(rpc_parts)
    return f'await _rpc("{rpc_expr}")'


def _emit_simple_expr_ts(node: dict, structs: dict) -> str | None:
    """Handle expression kinds that need minimal logic. Returns None if unhandled."""
    kind = node["kind"]
    if kind in ("fn_call", "array_fn_call"):
        fn_name = _make_function_name(node["signature_parts"])
        if fn_name in _rpc_targets_ts:
            return _emit_rpc_call(node, structs)
        args = ", ".join(_emit_expr(a, structs) for a in node["args"])
        return f"{fn_name}({args})"
    if kind == "task_call":
        return _emit_task_call_expr_ts(node)
    if kind == "index":
        arr = _emit_expr(node['array'], structs)
        idx = _emit_expr(node['index'], structs)
        if arr in _map_vars_ts:
            return f"{arr}.get({idx}) ?? \"\""
        return f"{arr}[{idx}]"
    if kind == "ternary":
        return f"({_emit_expr(node['condition'], structs)}) ? ({_emit_expr(node['true'], structs)}) : ({_emit_expr(node['false'], structs)})"
    if kind == "raw":
        val = node["value"]
        if re.match(r'[a-zA-Z_]\w*[\s(]', val):
            return f'_raise_undefined({repr(val)})'
        return val
    return None


def _emit_binop_ts(node: dict, structs: dict) -> str:
    """Emit a binary operation expression."""
    left = _emit_expr(node["left"], structs)
    right = _emit_expr(node["right"], structs)
    op = {"and": "&&", "or": "||"}.get(node["op"], node["op"])
    return f"{left} {op} {right}"


def _emit_array_fn_expr_ts(node: dict, structs: dict) -> str:
    """Emit an array function expression."""
    if node["name"] == "length_of":
        return f"{_emit_expr(node['args'][0], structs)}.length"
    return str(node)


def _emit_collection_expr_ts(node: dict, structs: dict) -> str | None:
    """Emit collection operations (filter, sort, slice). Returns None if not handled."""
    kind = node["kind"]
    if kind in ("where", "first_where", "index_of_first_where", "indices_where"):
        return _emit_filter_expr_ts(node, structs)
    if kind == "sort":
        return _emit_sort_expr_ts(node, structs)
    if kind == "slice":
        return _emit_slice_expr_ts(node, structs)
    return None


def _emit_leaf_expr_ts(node: dict) -> str | None:
    """Emit leaf expressions (member, name, literal). Returns None if not a leaf."""
    kind = node["kind"]
    if kind == "member":
        return _emit_member_expr_ts(node)
    if kind == "name":
        return _emit_name_expr_ts(node)
    if kind == "literal":
        return _emit_literal_expr_ts(node)
    return None


def _emit_expr(node: dict, structs: dict) -> str:
    """Emit a TypeScript expression from an AST node."""
    kind = node["kind"]
    if kind == "scoped_hook":
        return "/* scoped hook (handled at function level) */"
    if kind == "placeholder":
        return f"/* TODO: {node['text']} */"
    if kind == "raise":
        args = ", ".join(repr(a) for a in node["args"])
        return f'throw new _ZeroRaise({repr(node["name"])}, [{args}])'
    if kind == "emit":
        return f"yield {_emit_expr(node['value'], structs)}"
    if kind == "call":
        return _emit_call_expr_ts(node, structs)
    if kind == "reduce":
        return _emit_reduce_expr_ts(node)
    if kind == "var_decl":
        return _emit_var_decl_expr_ts(node, structs)
    if kind == "assign":
        return _emit_assign_expr_ts(node, structs)
    if kind == "binop":
        return _emit_binop_ts(node, structs)
    if kind == "array_fn":
        return _emit_array_fn_expr_ts(node, structs)
    collection = _emit_collection_expr_ts(node, structs)
    if collection is not None:
        return collection
    leaf = _emit_leaf_expr_ts(node)
    if leaf is not None:
        return leaf
    result = _emit_simple_expr_ts(node, structs)
    return result if result is not None else str(node)


# --- CLI ---

if __name__ == "__main__":
    source = open(sys.argv[1]).read()
    ir = process(source)
    print(emit(ir))
