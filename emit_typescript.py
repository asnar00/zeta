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


_CONCURRENTLY_HELPER_TS = """\
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}"""


def emit(ir: dict) -> str:
    """Emit TypeScript source code from a zero IR dict."""
    _check_compatibility(ir)
    global _enum_values
    sections = []

    # collect type info for translation
    structs = {t["name"]: t for t in ir["types"] if t["kind"] == "struct"}
    enums = {t["name"]: t for t in ir["types"] if t["kind"] == "enum"}
    _enum_values = {}
    for ename, edata in enums.items():
        ts_name = _ts_name(ename)
        for val in edata["values"]:
            _enum_values[val] = f"{ts_name}.{val}"

    has_concurrently = any(
        any(stmt.get("kind") == "concurrently" for stmt in fn.get("body", []))
        for fn in ir["functions"]
    )
    if has_concurrently:
        sections.append(_CONCURRENTLY_HELPER_TS)

    # emit test functions at the top, grouped by feature
    tests = ir.get("tests", [])
    if tests:
        by_feature = {}
        for t in tests:
            feat = t.get("feature", ir.get("test_feature", "test"))
            by_feature.setdefault(feat, []).append(t)
        for feat_name, feat_tests in by_feature.items():
            sections.append(_emit_test_section_ts(feat_tests, feat_name, structs))

    for typ in ir["types"]:
        code = _emit_type(typ, enums, structs)
        if code:
            sections.append(code)

    # skip platform stream declarations
    platform_stream_names = {u["name"].replace("$", "") for u in ir.get("uses", [])}
    var_lines = []
    for var in ir["variables"]:
        if var["name"] in platform_stream_names and var.get("array") and var.get("value") is None:
            continue
        if var.get("_platform") and var.get("array") and var.get("value") is None:
            continue
        var_lines.append(_emit_variable(var, structs))
    # bare function call statements
    for stmt in ir.get("statements", []):
        var_lines.append(_emit_expr(stmt, structs) + ";")
    if var_lines:
        sections.append("\n".join(var_lines))

    src = ir.get("source_file")

    for task in ir.get("tasks", []):
        if task.get("abstract"):
            continue
        sections.append(source_comment(task, src, "//") + "\n" + _emit_task_ts(task, structs, ir.get("uses", [])))

    # compute which functions are async (contain concurrently or call async fns)
    async_fns = _compute_async_fns(ir["functions"])

    for fn in ir["functions"]:
        if not fn.get("abstract"):
            sections.append(source_comment(fn, src, "//") + "\n" + _emit_function(fn, structs, async_fns))

    # generate dispatch functions for multiple dispatch groups
    concrete_fns = [fn for fn in ir["functions"] if not fn.get("abstract")]
    dispatch_sections = _generate_dispatchers_ts(concrete_fns, ir.get("types", []))
    sections.extend(dispatch_sections)

    result = "\n\n".join(sections) + "\n" if sections else ""

    # prefix cross-module function calls if module_map is provided
    module_map = ir.get("module_map")
    current_module = ir.get("current_module")
    if module_map and current_module:
        for fn_name, mod_name in module_map.items():
            if mod_name != current_module:
                result = result.replace(f"{fn_name}(", f"{mod_name}.{fn_name}(")

    return result


def _emit_test_section_ts(tests: list[dict], feature_name: str, structs: dict) -> str:
    """Emit test functions and register them for a feature (TypeScript)."""
    lines = []
    test_entries = []  # (function_name, description)
    for i, test in enumerate(tests):
        name = f"test_{feature_name.replace('-', '_')}_{i}"
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
        test_entries.append((name, desc))
        lines.append(f"function {name}(): void {{")
        lines.append(f"    // {desc}")
        lines.append(f"    const _result = {call_code};")
        lines.append(f"    const _expected = {expected_code};")
        lines.append(f'    if ({cmp}) throw new Error(`expected ${{_expected}}, got ${{_result}}`);')
        lines.append("}")
        lines.append("")

    # register tests
    entries = ", ".join(f"[{n}, {repr(d)}]" for n, d in test_entries)
    lines.append(f"register_tests({repr(feature_name)}, [{entries}]);")

    return "\n".join(lines)


def _generate_dispatchers_ts(functions: list[dict], types: list[dict]) -> list[str]:
    """Generate dispatch functions for function groups with multiple definitions."""
    dispatch_groups = compute_dispatch_groups(functions, types)
    sections = []
    for base, sorted_fns in dispatch_groups.items():
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

        sections.append("\n".join(lines))

    return sections


def _ts_name(name: str) -> str:
    """Convert a zero name to a valid TS identifier (hyphens to underscores)."""
    return name.replace("-", "_")


def _ts_type(zero_type: str) -> str:
    """Map a zero type to its TS equivalent."""
    if zero_type in _TS_NUMERIC:
        return _TS_NUMERIC[zero_type]
    # concrete numeric types (int32, uint8, float64, etc.) map to number
    if re.match(r"(int|uint|float)\d+$", zero_type):
        return "number"
    # abstract unsigned integers map to number
    if zero_type == "uint":
        return "number"
    return zero_type.replace("-", "_")


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
        name = _ts_name(typ["name"])
        # collect all fields including inherited parent fields
        all_fields = _collect_all_fields(typ, structs)
        # interface
        iface_lines = [f"interface {name} {{"]
        for field in all_fields:
            iface_lines.append(f"    readonly {field['name']}: {_ts_type(field['type'])};")
        iface_lines.append("}")
        # factory function
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


def _qualify_default_ts(field: dict, enums: dict, structs: dict = None) -> str:
    """Qualify a field default with its enum/struct type name if needed."""
    structs = structs or {}
    default_val = field["default"]

    # struct-typed fields: use factory call
    if field["type"] in structs:
        return f"{_ts_name(field['type'])}()"

    if field["type"] in enums:
        enum = enums[field["type"]]
        if str(default_val) in enum["values"]:
            ts_name = _ts_name(field["type"])
            return f"{ts_name}.{default_val}"
    return str(default_val)


def _safe(name: str) -> str:
    """Convert a zero name to a TypeScript-safe name."""
    return name.replace("-", "_")


def _emit_variable(var: dict, structs: dict) -> str:
    """Emit a variable declaration."""
    name = _safe(var["name"] + "_arr" if var["array"] else var["name"])
    type_ann = _ts_type(var["type"])

    if var["array"]:
        return _emit_array_variable(name, type_ann, var, structs)
    else:
        value_str = _emit_value(var["value"], var["type"], structs)
        return f"const {name}: {type_ann} = {value_str};"


def _emit_array_variable(name: str, type_ann: str, var: dict, structs: dict = None) -> str:
    """Emit an array variable declaration."""
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
    elif isinstance(val, dict) and "range" in val:
        if val["range"] == "through":
            length = val["end"] - val["start"] + 1
        else:  # "to"
            length = val["end"] - val["start"]
        start = val["start"]
        if start == 0:
            return f"const {name}: readonly {type_ann}[] = Array.from({{ length: {length} }}, (_, i) => i);"
        else:
            return f"const {name}: readonly {type_ann}[] = Array.from({{ length: {length} }}, (_, i) => i + {start});"

    elif isinstance(val, dict) and val.get("kind") == "stream":
        return _emit_stream_ts(name, type_ann, val, structs)
    elif isinstance(val, dict) and val.get("kind") == "task_call":
        return _emit_task_call_ts(name, type_ann, val)
    elif isinstance(val, dict) and val.get("kind") in ("where", "sort"):
        return f"const {name}: {type_ann}[] = {_emit_expr(val, structs or {})};"
    elif isinstance(val, dict) and "kind" in val:
        # mapped expression over arrays
        return f"const {name}: readonly {type_ann}[] = {_emit_array_map_expr(val, structs)};"

    return f"const {name}: readonly {type_ann}[] = {val};"


def _emit_stream_ts(name: str, type_ann: str, stream: dict, structs: dict = None) -> str:
    """Emit a streaming expression in TypeScript."""
    steps = stream["steps"]
    terminate = stream["terminate"]
    last = f"{name}[{name}.length - 1]"

    def _emit_ts_stream_expr(node):
        s = _emit_expr(node, structs or {})
        return s.replace("__last__", last)

    # simple case: all literals, no terminator
    if terminate is None and all(s.get("kind") == "literal" for s in steps):
        items = ", ".join(str(s["value"]) for s in steps)
        return f"const {name}: {type_ann}[] = [{items}];"

    lines = [f"const {name}: {type_ann}[] = [{_emit_ts_stream_expr(steps[0])}];"]

    if len(steps) > 1:
        if terminate is None:
            for step in steps[1:]:
                lines.append(f"{name}.push({_emit_ts_stream_expr(step)});")
        else:
            # with a terminator: steps[1:-1] are extra seeds, steps[-1] is repeat
            for step in steps[1:-1]:
                lines.append(f"{name}.push({_emit_ts_stream_expr(step)});")
            repeat_expr = _emit_ts_stream_expr(steps[-1])
            if terminate["kind"] == "until":
                cond = _emit_ts_stream_expr(terminate["condition"])
                lines.append(f"while (!({cond})) {{")
                lines.append(f"    {name}.push({repeat_expr});")
                lines.append("}")
            elif terminate["kind"] == "while":
                cond = _emit_ts_stream_expr(terminate["condition"])
                lines.append(f"while ({cond}) {{")
                lines.append(f"    {name}.push({repeat_expr});")
                lines.append("}")

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
        # for char params, spread into array; use base name for string$ lens
        base = arg[:-1] if arg.endswith("$") else arg
        return f"[...{base}]"
    name = arg.replace("$", "_arr") if arg.endswith("$") else arg
    return name


def _emit_element(v, structs: dict = None) -> str:
    """Emit a single array element — raw value or AST node."""
    if isinstance(v, dict) and "kind" in v:
        return _emit_expr(v, structs or {})
    return str(v)


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


def _rewrite_for_indexed_ts(node: dict, structs: dict) -> dict:
    """Rewrite an expression for index-based iteration (TypeScript)."""
    if not isinstance(node, dict):
        return node
    kind = node.get("kind")

    if kind == "index" and _contains_name(node.get("index", {}), "_"):
        arr_node = node["array"]
        if arr_node.get("kind") == "name" and arr_node["value"].endswith("$"):
            arr_str = arr_node["value"].replace("$", "_arr")
        else:
            arr_str = _emit_expr(arr_node, structs)
        idx = _rewrite_for_indexed_ts(node["index"], structs)
        idx_str = _emit_expr(idx, structs)
        return {"kind": "raw", "value": f"({idx_str} >= 0 ? {arr_str}[{idx_str}] : 0)"}

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
    """Emit index-based array mapping for expressions using _-relative indexing (TypeScript)."""
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


def _emit_array_map_expr(node: dict, structs: dict) -> str:
    """Emit array-mapped expressions using .map() or Array.from()."""
    if _has_index_underscore(node):
        return _emit_indexed_map_expr_ts(node, structs)

    array_refs = list(dict.fromkeys(_collect_array_refs(node)))  # deduplicate, preserve order
    if len(array_refs) == 0:
        return _emit_expr(node, structs)
    if len(array_refs) == 1:
        # single array: arr.map(x => expr)
        arr_name = array_refs[0]
        ts_arr = arr_name.replace("$", "_arr")
        expr = _emit_expr(_rewrite_array_ref(node, arr_name, "x"), structs)
        return f"{ts_arr}.map(x => {expr})"
    else:
        # multiple arrays: Array.from({ length: Math.max(...) }, (_, i) => expr)
        ts_arrs = [a.replace("$", "_arr") for a in array_refs]
        rewritten = node
        for arr, ts_arr in zip(array_refs, ts_arrs):
            rewritten = _rewrite_array_ref(rewritten, arr, f"({ts_arr}[i] ?? 0)")
        expr = _emit_expr(rewritten, structs)
        lengths = ", ".join(f"{a}.length" for a in ts_arrs)
        return f"Array.from({{ length: Math.max({lengths}) }}, (_, i) => {expr})"



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
        # named args -> object literal
        parts = [f"{a['name']}: {a['value']}" for a in call["args"]]
        return f"{name}({{ {', '.join(parts)} }})"
    else:
        # positional args -> object literal using field names from struct
        if name in structs:
            fields = structs[name]["fields"]
            parts = [f"{fields[i]['name']}: {call['args'][i]}" for i in range(len(call["args"]))]
            return f"{name}({{ {', '.join(parts)} }})"
        parts = [str(a) for a in call["args"]]
        return f"{name}({', '.join(parts)})"


# --- functions ---

def _compute_async_fns(functions: list[dict]) -> set[str]:
    """Compute which functions need to be async.
    A function is async if it contains a concurrently block,
    or if it calls an async function."""
    # build name -> function mapping
    fn_names = {}
    for fn in functions:
        name = _make_function_name(fn["signature_parts"])
        fn_names[name] = fn

    # first pass: functions with concurrently blocks
    async_set = set()
    for name, fn in fn_names.items():
        for stmt in fn.get("body", []):
            if isinstance(stmt, dict) and stmt.get("kind") == "concurrently":
                async_set.add(name)
                break

    # propagate: if a function's body calls an async function, it's also async
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


def _calls_any_of(stmt, fn_names: set[str]) -> bool:
    """Check if a statement calls any function in the given set."""
    if not isinstance(stmt, dict):
        return False
    if stmt.get("kind") == "call":
        call_name = stmt.get("name", "")
        if f"fn_{call_name}" in fn_names:
            return True
    if stmt.get("kind") == "fn_call":
        name = _make_function_name(stmt.get("signature_parts", []))
        if name in fn_names:
            return True
    # check raw strings for function calls
    if stmt.get("kind") == "raw":
        val = stmt.get("value", "")
        for fn_name in fn_names:
            # strip fn_ prefix for matching against raw call text
            short = fn_name[3:] if fn_name.startswith("fn_") else fn_name
            if short + "()" in val or short + "(" in val:
                return True
    return False


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


def _emit_task_if_block_ts(node: dict, structs: dict = None) -> str:
    """Emit an if/else block in a task body — uses bare assignment, not const."""
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
    lines.append("}")
    return "\n".join(lines)


def _platform_stream_fn_ts(stream_name: str, uses: list[dict]) -> str:
    """Map a platform stream name to its TypeScript handler function name."""
    for use in uses:
        if use["name"] == stream_name:
            return f"_push_{use['platform']}_{use['name'].replace('$', '')}"
    return f"_push_{stream_name.replace('$', '')}"


def _emit_task_ts(task: dict, structs: dict = None, uses: list[dict] = None) -> str:
    """Emit a task as a TypeScript generator function (or plain function for void tasks)."""
    uses = uses or []
    name_parts = task["name_parts"]
    all_params = task.get("input_streams", []) + task.get("params", [])
    param_strs = []
    for p in all_params:
        pname = p["name"] + "_arr" if p in task.get("input_streams", []) else p["name"]
        ts_type = _ts_type(p["type"])
        param_strs.append(f"{pname}: readonly {ts_type}[]" if p in task.get("input_streams", []) else f"{pname}: {ts_type}")
    params_str = ", ".join(param_strs)

    fn_name = "task_" + "_".join(name_parts)
    for p in all_params:
        fn_name += f"__{p['type']}"

    is_void = task["output"] is None
    has_platform_streams = bool(task.get("platform_streams"))
    if is_void:
        if has_platform_streams:
            lines = [f"async function {fn_name}({params_str}): Promise<void> {{"]
        else:
            lines = [f"function {fn_name}({params_str}): void {{"]
    else:
        output_type = _ts_type(task["output"]["type"])
        lines = [f"function* {fn_name}({params_str}): Generator<{output_type}> {{"]

    base_indent = "    "
    extra_indent = ""

    for node in task["body"]:
        if isinstance(node, str):
            lines.append(f"{base_indent}{extra_indent}{node}")
            continue

        kind = node.get("kind")

        if kind == "for_each":
            iter_expr = _emit_expr(node["iter"], structs)
            for_keyword = "for await" if has_platform_streams else "for"
            lines.append(f"{base_indent}{extra_indent}{for_keyword} (const {node['name']} of {iter_expr}) {{")
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

        elif kind == "consume":
            stream_name = node["stream"].replace("$", "_arr")
            lines.append(f"{base_indent}for (const {node['name']} of {stream_name}) {{")
            extra_indent = "    "

        elif kind == "consume_call":
            call = node["call"]
            if call.get("kind") == "task_call":
                call_fn = make_task_call_fn_name(call)
                args = ", ".join(_coerce_task_arg(a, t, call) for a, t in
                                 zip(call["args"], _task_call_param_types(call)))
                lines.append(f"{base_indent}for (const {node['name']} of {call_fn}({args})) {{")
            else:
                lines.append(f"{base_indent}for (const {node['name']} of {_emit_expr(call, structs)}) {{")
            extra_indent = "    "

        elif kind == "emit":
            lines.append(f"{base_indent}{extra_indent}yield {_emit_expr(node['value'], structs)};")

        elif kind == "emit_external":
            handler = _platform_stream_fn_ts(node["stream"], uses)
            lines.append(f"{base_indent}{extra_indent}{handler}({_emit_expr(node['value'], structs)});")

        elif kind == "if_block":
            block_code = _emit_task_if_block_ts(node, structs)
            for bl in block_code.split("\n"):
                lines.append(f"{base_indent}{extra_indent}{bl}")

        elif kind == "var_decl":
            name = node["name"] + "_arr" if node.get("array") else node["name"]
            val = node.get("value")
            if node.get("array") and isinstance(val, dict) and val.get("kind") == "task_call":
                call_fn = make_task_call_fn_name(val)
                args = ", ".join(_coerce_task_arg(a, t, val) for a, t in
                                 zip(val["args"], _task_call_param_types(val)))
                # inside a task body, assign the generator lazily (don't spread)
                lines.append(f"{base_indent}{extra_indent}const {name} = {call_fn}({args});")
            elif node.get("array") and isinstance(val, list):
                items = ", ".join(_emit_expr(v, structs) if isinstance(v, dict) else str(v) for v in val)
                lines.append(f"{base_indent}{extra_indent}const {name} = [{items}];")
            elif isinstance(val, dict):
                lines.append(f"{base_indent}{extra_indent}let {name} = {_emit_expr(val, structs)};")
            else:
                lines.append(f"{base_indent}{extra_indent}let {name} = {val};")

        elif kind == "assign":
            lines.append(f"{base_indent}{extra_indent}{node['target']} = {_emit_expr(node['value'], structs)};")

        else:
            lines.append(f"{base_indent}{extra_indent}{_emit_expr(node, structs)};")

    # close braces for consume loops
    if extra_indent:
        lines.append(f"{base_indent}}}")
    lines.append("}")

    return "\n".join(lines)


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
    """Check if a function body needs result-assignment-as-return guards."""
    assign_count = sum(1 for s in body if _assigns_result_ts(s, result_var))
    has_conditional = any(s.get("kind") == "if_block" and _assigns_result_ts(s, result_var) for s in body)
    return assign_count > 1 and has_conditional


def _emit_function(fn: dict, structs: dict, async_fns: set[str] = None) -> str:
    """Emit a function definition."""
    async_fns = async_fns or set()
    name = _make_function_name(fn["signature_parts"])
    param_strs = []
    for p in fn["params"]:
        pname = p["name"].replace("$", "_arr")
        if p["type"] is not None:
            param_strs.append(f"{pname}: {_ts_type(p['type'])}")
        else:
            param_strs.append(pname)
    params = ", ".join(param_strs)
    is_async = name in async_fns
    prefix = "async function" if is_async else "function"

    if fn["result"] is not None:
        ret_type = _ts_type(fn["result"]["type"])
        result_var = fn["result"]["name"]
        if result_var.endswith("$"):
            result_var = result_var.replace("$", "_arr")
            ret_type = f"{ret_type}[]"
        # if body has if_blocks, declare result var with let at top
        has_if = any(isinstance(s, dict) and s.get("kind") == "if_block" for s in fn["body"])
        needs_guard = _result_needs_guard_ts(fn["body"], result_var)
        lines = [f"{prefix} {name}({params}): {ret_type} {{"]
        if has_if or needs_guard:
            lines.append(f"    let {result_var}: {ret_type} = undefined!;")
        rv = result_var if (has_if or needs_guard) else None
        seen_conditional_assign = False
        for stmt in fn["body"]:
            code = _emit_stmt(stmt, ret_type, structs, async_fns, result_var=rv)
            if needs_guard and _assigns_result_ts(stmt, result_var):
                if seen_conditional_assign:
                    lines.append(f"    if ({result_var} === undefined) {{")
                    lines.append(f"        {code}")
                    lines.append(f"    }}")
                else:
                    lines.append(f"    {code}")
                    if stmt.get("kind") == "if_block":
                        seen_conditional_assign = True
            else:
                lines.append(f"    {code}")
        lines.append(f"    return {result_var};")
    else:
        lines = [f"{prefix} {name}({params}): void {{"]
        for stmt in fn["body"]:
            lines.append(f"    {_emit_stmt(stmt, 'void', structs, async_fns)}")
    lines.append("}")
    return "\n".join(lines)



# --- expression emitter ---

def _emit_stmt(node: dict, result_type: str, structs: dict, async_fns: set[str] = None, result_var: str = None) -> str:
    """Emit a TypeScript statement from an AST node."""
    async_fns = async_fns or set()
    if node["kind"] == "if_block":
        return _emit_if_block_ts(node, structs, result_type, async_fns, result_var)
    if node["kind"] == "concurrently":
        return _emit_concurrently_ts(node, structs)
    if node["kind"] == "assign":
        value = _emit_expr(node["value"], structs)
        if result_var and node["target"] == result_var:
            return f"{node['target']} = {value};"
        return f"const {node['target']}: {result_type} = {value};"
    # check if this is a call to an async function — add await
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
    args = ", ".join(block_calls)
    return f"await _concurrently({args});"



def _emit_expr(node: dict, structs: dict) -> str:
    """Emit a TypeScript expression from an AST node."""
    kind = node["kind"]

    if kind == "emit":
        return f"yield {_emit_expr(node['value'], structs)}"

    elif kind == "call":
        name = node["name"]
        safe_name = _safe(name)
        if name in structs:
            # struct constructor -> object literal via factory
            args = node["args"]
            parts = []
            fields = structs[name]["fields"]
            for i, a in enumerate(args):
                # named arg: raw value like 'path="/"'
                if isinstance(a, dict) and a.get("kind") == "raw" and "=" in a.get("value", ""):
                    k, v = a["value"].split("=", 1)
                    parts.append(f"{k.strip()}: {v.strip()}")
                else:
                    parts.append(f"{fields[i]['name']}: {_emit_expr(a, structs)}")
            return f"{safe_name}({{ {', '.join(parts)} }})"
        args = ", ".join(_emit_expr(a, structs) for a in node["args"])
        return f"{safe_name}({args})"

    elif kind in ("fn_call", "array_fn_call"):
        fn_name = _make_function_name(node["signature_parts"])
        args = ", ".join(_emit_expr(a, structs) for a in node["args"])
        return f"{fn_name}({args})"

    elif kind == "task_call":
        fn_name = make_task_call_fn_name(node)
        args = ", ".join(_coerce_task_arg(a, t, node) for a, t in
                         zip(node["args"], _task_call_param_types(node)))
        return f"[...{fn_name}({args})]"

    elif kind == "binop":
        left = _emit_expr(node["left"], structs)
        right = _emit_expr(node["right"], structs)
        op = {"and": "&&", "or": "||"}.get(node["op"], node["op"])
        return f"{left} {op} {right}"

    elif kind == "array_fn":
        if node["name"] == "length_of":
            arg = _emit_expr(node["args"][0], structs)
            return f"{arg}.length"

    elif kind == "where":
        arr = _emit_expr(node["array"], structs)
        cond = _emit_expr(_replace_underscore(node["condition"], "x"), structs)
        return f"{arr}.filter(x => {cond})"

    elif kind == "first_where":
        arr = _emit_expr(node["array"], structs)
        cond = _emit_expr(_replace_underscore(node["condition"], "x"), structs)
        return f"{arr}.find(x => {cond})"

    elif kind == "index_of_first_where":
        arr = _emit_expr(node["array"], structs)
        cond = _emit_expr(_replace_underscore(node["condition"], "x"), structs)
        return f"{arr}.findIndex(x => {cond})"

    elif kind == "indices_where":
        arr = _emit_expr(node["array"], structs)
        cond = _emit_expr(_replace_underscore(node["condition"], "x"), structs)
        return f"{arr}.map((x, i) => ({cond}) ? i : -1).filter(i => i >= 0)"

    elif kind == "sort":
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

    elif kind == "index":
        arr = _emit_expr(node["array"], structs)
        idx = _emit_expr(node["index"], structs)
        return f"{arr}[{idx}]"

    elif kind == "slice":
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
        else:
            return f"{arr}.slice({start})"

    elif kind == "member":
        if node["field"] == "char$":
            return f"[...{node['object']}]"
        return f"{_safe(node['object'])}.{_safe(node['field'])}"

    elif kind == "name":
        val = node["value"]
        if val in _enum_values:
            return _enum_values[val]
        if val.endswith("$"):
            return _safe(val.replace("$", "_arr"))
        return _safe(val)

    elif kind == "literal":
        v = node["value"]
        if v is True:
            return "true"
        if v is False:
            return "false"
        return str(v)

    elif kind == "ternary":
        true = _emit_expr(node["true"], structs)
        cond = _emit_expr(node["condition"], structs)
        false = _emit_expr(node["false"], structs)
        return f"({cond}) ? ({true}) : ({false})"

    elif kind == "reduce":
        arr = node["array"].replace("$", "_arr")
        if "op" in node:
            return f"{arr}.reduce((a, b) => a {node['op']} b)"
        fn_name = _make_function_name_from_reduce(node["fn_parts"], "int")
        return f"{arr}.reduce({fn_name})"

    elif kind == "var_decl":
        name = node["name"] + "_arr" if node.get("array") else node["name"]
        if node.get("array"):
            val = node["value"]
            if isinstance(val, list):
                items = ", ".join(_emit_expr(v, structs) if isinstance(v, dict) else str(v) for v in val)
                return f"const {name} = [{items}]"
            elif isinstance(val, dict) and "range" in val:
                r = val
                if r["range"] == "through":
                    length = r["end"] - r["start"] + 1 if isinstance(r["end"], int) else f"{r['end']} - {r['start']} + 1"
                else:
                    length = r["end"] - r["start"] if isinstance(r["end"], int) else f"{r['end']} - {r['start']}"
                start = r["start"]
                if isinstance(length, int):
                    if start == 0:
                        return f"const {name} = Array.from({{ length: {length} }}, (_, i) => i)"
                    return f"const {name} = Array.from({{ length: {length} }}, (_, i) => i + {start})"
                return f"const {name} = Array.from({{ length: {length} }}, (_, i) => i + {start})"
            elif isinstance(val, dict) and val.get("kind") == "task_call":
                call_fn = make_task_call_fn_name(val)
                args = ", ".join(_coerce_task_arg(a, t, val) for a, t in
                                 zip(val["args"], _task_call_param_types(val)))
                return f"const {name} = [...{call_fn}({args})]"
            elif isinstance(val, dict) and val.get("kind") == "stream" and len(val.get("steps", [])) == 1 and val.get("terminate") is None:
                return f"const {name} = {_emit_array_map_expr(val['steps'][0], structs)}"
            elif isinstance(val, dict) and val.get("kind") == "fn_call" and \
                    any(isinstance(a, dict) and a.get("kind") == "array_fn_call" for a in val.get("args", [])):
                fn_name = _make_function_name(val["signature_parts"])
                inner = _emit_expr(val["args"][0], structs)
                return f"const {name} = {inner}.map(x => {fn_name}(x))"
            elif isinstance(val, dict) and "kind" in val:
                return f"const {name} = {_emit_array_map_expr(val, structs)}"
            return f"const {name} = {val}"
        return f"const {name} = {_emit_expr(node['value'], structs)}"

    elif kind == "assign":
        return f"{node['target']} = {_emit_expr(node['value'], structs)}"

    elif kind == "raw":
        return node["value"]

    return str(node)


# --- CLI ---

if __name__ == "__main__":
    source = open(sys.argv[1]).read()
    ir = process(source)
    print(emit(ir))
