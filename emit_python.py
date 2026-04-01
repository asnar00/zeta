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
_PY_TYPE_MAP = {"string": "str", "char": "str"}


_enum_values = {}  # populated by emit(), maps value -> "type.value"


def emit(ir: dict) -> str:
    """Emit Python source code from a zero IR dict."""
    _check_compatibility(ir)
    global _enum_values
    sections = []

    # collect enum names for qualifying default values and expression references
    enums = {t["name"]: t for t in ir["types"] if t["kind"] == "enum"}
    _enum_values = {}
    for ename, edata in enums.items():
        py_name = _py_name(ename)
        for val in edata["values"]:
            _enum_values[val] = f"{py_name}.{val}"

    imports = _collect_imports(ir)
    if imports:
        sections.append("\n".join(imports))

    has_concurrently = any(
        any(stmt.get("kind") == "concurrently" for stmt in fn.get("body", []))
        for fn in ir["functions"]
    )
    if has_concurrently:
        sections.append(_CONCURRENTLY_HELPER)

    # emit test functions at the top, grouped by feature
    tests = ir.get("tests", [])
    if tests:
        # group tests by feature
        by_feature = {}
        for t in tests:
            feat = t.get("feature", ir.get("test_feature", "test"))
            by_feature.setdefault(feat, []).append(t)
        for feat_name, feat_tests in by_feature.items():
            sections.append(_emit_test_section(feat_tests, feat_name))

    # build type lookup for parent field resolution
    all_types = {t["name"]: t for t in ir["types"]}

    for typ in ir["types"]:
        code = _emit_type(typ, enums, all_types)
        if code:
            sections.append(code)

    src = ir.get("source_file")

    for task in ir.get("tasks", []):
        if task.get("abstract"):
            continue
        sections.append(source_comment(task, src, "#") + "\n" + _emit_task(task, ir.get("uses", [])))

    for fn in ir["functions"]:
        if not fn.get("abstract"):
            sections.append(source_comment(fn, src, "#") + "\n" + _emit_function(fn))

    # generate dispatch functions for multiple dispatch groups
    concrete_fns = [fn for fn in ir["functions"] if not fn.get("abstract")]
    dispatch_sections = _generate_dispatchers(concrete_fns, ir.get("types", []))

    # variables last — they may reference functions and tasks
    # skip platform stream declarations (handled by platform implementations)
    platform_stream_names = {u["name"].replace("$", "") for u in ir.get("uses", [])}
    var_lines = []
    for var in ir["variables"]:
        if var["name"] in platform_stream_names and var.get("array") and var.get("value") is None:
            continue
        if var.get("_platform") and var.get("array") and var.get("value") is None:
            continue
        var_lines.append(_emit_variable(var))
    # bare function call statements
    for stmt in ir.get("statements", []):
        var_lines.append(_emit_expr(stmt))
    if var_lines:
        sections.append("\n".join(var_lines))
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


def _emit_test_section(tests: list[dict], feature_name: str) -> str:
    """Emit test functions and register them for a feature."""
    lines = []
    test_entries = []  # (name, desc)
    for i, test in enumerate(tests):
        name = f"test_{feature_name}_{i}"
        call_code = _emit_expr(test["call"])
        expected_code = _emit_expr(test["expected"]) if isinstance(test["expected"], dict) else repr(test["expected"])
        is_task = test["call"].get("kind") == "task_call"
        expected_is_array = (test["expected"].get("kind") == "array_lit" or
                             (test["expected"].get("kind") == "raw" and test["expected"].get("value", "").startswith("[")))
        if is_task and not expected_is_array:
            expected_code = f"[{expected_code}]"
        desc = test.get("source_text", "")
        test_entries.append((name, desc))
        lines.append(f"def {name}():")
        lines.append(f"    '''{desc}'''")
        lines.append(f"    _result = {call_code}")
        lines.append(f"    _expected = {expected_code}")
        lines.append(f'    assert _result == _expected, f"expected {{_expected}}, got {{_result}}"')
        lines.append("")

    # register tests
    entries = ", ".join(f"({n}, {repr(d)})" for n, d in test_entries)
    lines.append(f"register_tests({repr(feature_name)}, [{entries}])")

    return "\n".join(lines)


def _generate_dispatchers(functions: list[dict], types: list[dict]) -> list[str]:
    """Generate dispatch functions for function groups with multiple definitions."""
    dispatch_groups = compute_dispatch_groups(functions, types)
    sections = []
    for base, sorted_fns in dispatch_groups.items():
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

        sections.append("\n".join(lines))

    return sections


def _collect_imports(ir: dict) -> list[str]:
    """Collect needed import statements."""
    imports = []
    has_enum = any(t["kind"] == "enum" for t in ir["types"])
    has_struct = any(t["kind"] == "struct" for t in ir["types"])
    has_reduce = (any(isinstance(v.get("value"), dict) and v["value"].get("kind") == "reduce"
                      for v in ir["variables"])
                  or any(any(isinstance(s, dict) and s.get("kind") == "assign"
                             and isinstance(s.get("value"), dict) and s["value"].get("kind") == "reduce"
                             for s in fn.get("body", []))
                         for fn in ir["functions"]))
    has_zip_longest = any(isinstance(v.get("value"), dict) and v["value"].get("kind") == "binop"
                          and v.get("array") and len(_collect_array_refs(v["value"])) > 1
                          for v in ir["variables"])
    has_concurrently = any(
        any(stmt.get("kind") == "concurrently" for stmt in fn.get("body", []))
        for fn in ir["functions"]
    )
    if has_reduce:
        imports.append("import functools")
    if has_concurrently:
        imports.append("import threading")
    if has_zip_longest:
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


def _py_name(name: str) -> str:
    """Convert a zero name to a valid Python identifier (hyphens to underscores)."""
    return name.replace("-", "_")


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
        py_name = _py_name(typ["name"])
        lines = [f"class {py_name}(NamedTuple):"]
        # collect all fields: parent fields first, then own fields
        all_fields = _collect_all_fields(typ, all_types)
        for field in all_fields:
            default = _qualify_default(field, enums)
            lines.append(f"    {field['name']}: {_py_type_ann(field['type'])}{default}")
        if not all_fields:
            lines.append("    pass")
        return "\n".join(lines)



def _qualify_default(field: dict, enums: dict) -> str:
    """Qualify a field default with its enum type name if needed."""
    if field["default"] is None:
        return ""
    default_val = field["default"]
    # check if the field's type is an enum and the default is one of its values
    if field["type"] in enums:
        enum = enums[field["type"]]
        if str(default_val) in enum["values"]:
            py_name = _py_name(field["type"])
            return f" = {py_name}.{default_val}"
    return f" = {default_val}"


def _py_type_ann(zero_type: str) -> str:
    """Map a zero type to its Python annotation."""
    return _PY_TYPE_MAP.get(zero_type, zero_type)


def _emit_variable(var: dict) -> str:
    """Emit a variable declaration."""
    name = var["name"] + "_arr" if var["array"] else var["name"]
    type_ann = _py_type_ann(var["type"])

    if var["array"]:
        return _emit_array_variable(name, type_ann, var)
    else:
        value_str = _emit_value(var["value"], var["type"])
        return f"{name}: {type_ann} = {value_str}"


def _emit_array_variable(name: str, type_ann: str, var: dict) -> str:
    """Emit an array variable declaration."""
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
    elif isinstance(val, dict) and "range" in val:
        return f"{name}: list[{type_ann}] = {_emit_range(val)}"
    elif isinstance(val, dict) and val.get("kind") == "stream":
        return _emit_stream(name, val)
    elif isinstance(val, dict) and val.get("kind") == "task_call":
        return _emit_task_call(name, type_ann, val)
    elif isinstance(val, dict) and val.get("kind") == "where":
        return f"{name} = {_emit_expr(val)}"
    elif isinstance(val, dict) and val.get("kind") == "sort":
        return f"{name} = {_emit_sort(val)}"
    elif isinstance(val, dict) and "kind" in val:
        # mapped expression over arrays
        return f"{name}: list[{type_ann}] = {_emit_array_map_expr(val)}"

    return f"{name}: list[{type_ann}] = {val}"


def _emit_stream(name: str, stream: dict) -> str:
    """Emit a streaming expression."""
    steps = stream["steps"]
    terminate = stream["terminate"]

    def _emit_stream_expr(node):
        """Emit an expression, replacing __last__ with array[-1]."""
        s = _emit_expr(node)
        return s.replace("__last__", f"{name}[-1]")

    # if no terminator and all steps are simple literals, just emit a list
    if terminate is None and all(s.get("kind") == "literal" for s in steps):
        items = ", ".join(str(s["value"]) for s in steps)
        return f"{name} = [{items}]"

    # first step is always the seed
    lines = [f"{name} = [{_emit_stream_expr(steps[0])}]"]

    if len(steps) > 1:
        if terminate is None:
            # no loop — just append each remaining step once
            for step in steps[1:]:
                lines.append(f"{name}.append({_emit_stream_expr(step)})")
        else:
            # with a terminator: steps[1:-1] are extra seeds, steps[-1] is repeat
            for step in steps[1:-1]:
                lines.append(f"{name}.append({_emit_stream_expr(step)})")
            repeat_expr = _emit_stream_expr(steps[-1])
            if terminate["kind"] == "until":
                cond = _emit_stream_expr(terminate["condition"])
                lines.append(f"while not ({cond}):")
                lines.append(f"    {name}.append({repeat_expr})")
            elif terminate["kind"] == "while":
                cond = _emit_stream_expr(terminate["condition"])
                lines.append(f"while {cond}:")
                lines.append(f"    {name}.append({repeat_expr})")

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
        # no array refs — find any name arg to use for length (e.g. string length)
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
    # fn_call args: look for plain name args (not _)
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


def _rewrite_for_indexed(node: dict) -> dict:
    """Rewrite an expression for index-based iteration.
    - array$  → array_arr[_i]
    - array$[_ - N]  → array_arr[_i - N] with bounds check
    - bare _  → _i (in index expressions)
    """
    if not isinstance(node, dict):
        return node
    kind = node.get("kind")

    # index with _ reference: arr$[_ - 1] → (arr_arr[_i - 1] if _i - 1 >= 0 else default)
    if kind == "index" and _contains_name(node.get("index", {}), "_"):
        # emit the array name directly (not rewritten to arr[_i])
        arr_node = node["array"]
        if arr_node.get("kind") == "name" and arr_node["value"].endswith("$"):
            arr_str = arr_node["value"].replace("$", "_arr")
        else:
            arr_str = _emit_expr(arr_node)
        idx = _rewrite_for_indexed(node["index"])
        idx_str = _emit_expr(idx)
        return {"kind": "raw", "value": f"({arr_str}[{idx_str}] if {idx_str} >= 0 else 0)"}

    # bare array ref: arr$ → arr_arr[_i]
    if kind == "name" and node.get("value", "").endswith("$"):
        py_name = node["value"].replace("$", "_arr")
        return {"kind": "raw", "value": f"{py_name}[_i]"}

    # _ in index context → _i
    if kind == "name" and node.get("value") == "_":
        return {"kind": "raw", "value": "_i"}

    # recurse
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


def _emit_array_map_expr(node: dict) -> str:
    """Emit a list comprehension for array-mapped expressions."""
    # if expression uses _ as an index reference, use index-based iteration
    if _has_index_underscore(node):
        return _emit_indexed_map_expr(node)

    array_refs = list(dict.fromkeys(_collect_array_refs(node)))  # deduplicate, preserve order
    if len(array_refs) == 0:
        return _emit_expr(node)
    if len(array_refs) == 1:
        # single array: [expr for x in arr]
        arr_name = array_refs[0]
        py_arr = arr_name.replace("$", "_arr")
        expr = _emit_expr(_rewrite_array_ref(node, arr_name, "x"))
        return f"[{expr} for x in {py_arr}]"
    else:
        # multiple arrays: [expr for a, b in zip_longest(arr1, arr2, fillvalue=0)]
        py_arrs = [a.replace("$", "_arr") for a in array_refs]
        loop_vars = [chr(ord("a") + i) for i in range(len(array_refs))]
        rewritten = node
        for arr, var in zip(array_refs, loop_vars):
            rewritten = _rewrite_array_ref(rewritten, arr, var)
        expr = _emit_expr(rewritten)
        vars_str = ", ".join(loop_vars)
        arrs_str = ", ".join(py_arrs)
        return f"[{expr} for {vars_str} in zip_longest({arrs_str}, fillvalue=0)]"



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
        # function reduce — build function name from fn_parts
        fn_name = _make_function_name_from_reduce(node["fn_parts"], zero_type)
        return f"functools.reduce({fn_name}, {arr})"



def _emit_call_value(call: dict) -> str:
    """Emit a constructor/call value."""
    name = call["name"]
    if not call["args"]:
        return f"{name}()"
    if isinstance(call["args"][0], dict) and "name" in call["args"][0]:
        # named args
        parts = [f"{a['name']}={a['value']}" for a in call["args"]]
    else:
        parts = [str(a) for a in call["args"]]
    return f"{name}({', '.join(parts)})"


# --- functions ---

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


def _emit_task(task: dict, uses: list[dict] = None) -> str:
    """Emit a task as a Python generator function (or plain function for void tasks)."""
    uses = uses or []
    name_parts = task["name_parts"]
    all_params = task.get("input_streams", []) + task.get("params", [])
    param_strs = []
    for p in all_params:
        pname = p["name"] + "_arr" if p in task.get("input_streams", []) else p["name"]
        param_strs.append(f"{pname}: {_py_type_ann(p['type'])}")
    params_str = ", ".join(param_strs)

    # build function name from name parts + param types
    fn_name = "task_" + "_".join(name_parts)
    for p in all_params:
        fn_name += f"__{p['type']}"

    is_void = task["output"] is None
    output_stream_name = None if is_void else task["output"]["name"] + "$"

    lines = [f"def {fn_name}({params_str}):"]

    # translate body lines with indent tracking
    base_indent = "    "
    extra_indent = ""

    for node in task["body"]:
        if isinstance(node, str):
            lines.append(f"{base_indent}{extra_indent}{node}")
            continue

        kind = node.get("kind")

        if kind == "for_each":
            iter_expr = _emit_expr(node["iter"])
            lines.append(f"{base_indent}{extra_indent}for {node['name']} in {iter_expr}:")
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

        elif kind == "consume":
            stream_name = node["stream"].replace("$", "_arr")
            lines.append(f"{base_indent}for {node['name']} in {stream_name}:")
            extra_indent = "    "

        elif kind == "consume_call":
            call = node["call"]
            if call.get("kind") == "task_call":
                call_fn = make_task_call_fn_name(call)
                args = ", ".join(a.replace("$", "_arr") for a in call["args"])
                lines.append(f"{base_indent}for {node['name']} in {call_fn}({args}):")
            else:
                lines.append(f"{base_indent}for {node['name']} in {_emit_expr(call)}:")
            extra_indent = "    "

        elif kind == "emit":
            lines.append(f"{base_indent}{extra_indent}yield {_emit_expr(node['value'])}")

        elif kind == "emit_external":
            handler = _platform_stream_fn(node["stream"], uses)
            lines.append(f"{base_indent}{extra_indent}{handler}({_emit_expr(node['value'])})")

        elif kind == "if_block":
            block_code = _emit_if_block(node, is_task=True)
            for bl in block_code.split("\n"):
                lines.append(f"{base_indent}{extra_indent}{bl}")

        elif kind == "var_decl" and node.get("array") and isinstance(node.get("value"), dict) and node["value"].get("kind") == "task_call":
            # task call inside a task body: assign the generator lazily, don't materialise
            call = node["value"]
            call_fn = make_task_call_fn_name(call)
            args = ", ".join(a.replace("$", "_arr") for a in call["args"])
            name = node["name"] + "_arr"
            lines.append(f"{base_indent}{extra_indent}{name} = {call_fn}({args})")

        else:
            # parsed expression node (var_decl, assign, fn_call, etc.)
            lines.append(f"{base_indent}{extra_indent}{_emit_expr(node)}")

    return "\n".join(lines)


def _emit_function(fn: dict) -> str:
    """Emit a function definition."""
    name = _make_function_name(fn["signature_parts"])
    param_strs = []
    for p in fn["params"]:
        pname = p["name"].replace("$", "_arr")
        if p["type"] is not None:
            param_strs.append(f"{pname}: {_py_type_ann(p['type'])}")
        else:
            param_strs.append(pname)
    params = ", ".join(param_strs)

    if fn["result"] is not None:
        ret_type = _py_type_ann(fn["result"]["type"])
        result_var = fn["result"]["name"]
        if result_var.endswith("$"):
            result_var = result_var.replace("$", "_arr")
        needs_guard = _result_needs_guard(fn["body"], result_var)
        lines = [f"def {name}({params}) -> {ret_type}:"]
        if needs_guard:
            lines.append(f"    {result_var} = None")
        seen_conditional_assign = False
        for stmt in fn["body"]:
            code = _emit_expr(stmt)
            if needs_guard and _assigns_result(stmt, result_var):
                if seen_conditional_assign:
                    guard_line = f"if {result_var} is None:"
                    lines.extend(_indent(guard_line, "    "))
                    lines.extend(_indent(code, "        "))
                else:
                    lines.extend(_indent(code, "    "))
                    if stmt.get("kind") == "if_block":
                        seen_conditional_assign = True
            else:
                lines.extend(_indent(code, "    "))
        lines.append(f"    return {result_var}")
    else:
        lines = [f"def {name}({params}):"]
        for stmt in fn["body"]:
            lines.extend(_indent(_emit_expr(stmt), "    "))
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

def _emit_expr(node: dict) -> str:
    """Emit a Python expression from an AST node."""
    kind = node["kind"]

    if kind == "if_block":
        return _emit_if_block(node)

    elif kind == "emit":
        return f"yield {_emit_expr(node['value'])}"

    elif kind == "concurrently":
        block_calls = []
        for block in node["blocks"]:
            if len(block) == 1:
                block_calls.append(f"lambda: {block[0]}")
            else:
                stmts = "; ".join(block)
                block_calls.append(f"lambda: ({stmts})")
        args = ", ".join(block_calls)
        return f"_concurrently({args})"

    elif kind == "reduce":
        arr = node["array"].replace("$", "_arr")
        if "op" in node:
            return f"functools.reduce(lambda a, b: a {node['op']} b, {arr})"
        fn_name = _make_function_name_from_reduce(node["fn_parts"], "int")
        return f"functools.reduce({fn_name}, {arr})"

    elif kind == "var_decl":
        name = node["name"] + "_arr" if node.get("array") else node["name"]
        if node.get("array"):
            val = node["value"]
            # reuse array variable emission logic
            if isinstance(val, list):
                items = ", ".join(_emit_element(v) for v in val)
                return f"{name} = [{items}]"
            elif isinstance(val, dict) and "range" in val:
                return f"{name} = {_emit_range(val)}"
            elif isinstance(val, dict) and val.get("kind") == "task_call":
                fn_name = make_task_call_fn_name(val)
                args = ", ".join(_coerce_task_arg_py(a, t) for a, t in
                                 zip(val["args"], _task_call_param_types_py(val)))
                return f"{name} = list({fn_name}({args}))"
            elif isinstance(val, dict) and val.get("kind") == "stream" and len(val.get("steps", [])) == 1 and val.get("terminate") is None:
                # single-step stream without terminator = array-mapped expression
                return f"{name} = {_emit_array_map_expr(val['steps'][0])}"
            elif isinstance(val, dict) and val.get("kind") == "fn_call" and \
                    any(isinstance(a, dict) and a.get("kind") == "array_fn_call" for a in val.get("args", [])):
                # scalar function wrapping array function = map the scalar over results
                fn_name = _make_function_name(val["signature_parts"])
                inner = _emit_expr(val["args"][0])
                return f"{name} = [{fn_name}(x) for x in {inner}]"
            elif isinstance(val, dict) and "kind" in val:
                return f"{name} = {_emit_array_map_expr(val)}"
            return f"{name} = {val}"
        return f"{name} = {_emit_expr(node['value'])}"

    elif kind == "assign":
        return f"{node['target']} = {_emit_expr(node['value'])}"

    elif kind == "call":
        name = node["name"]
        args = ", ".join(_emit_expr(a) for a in node["args"])
        return f"{name}({args})"

    elif kind in ("fn_call", "array_fn_call"):
        fn_name = _make_function_name(node["signature_parts"])
        args = ", ".join(_emit_expr(a) for a in node["args"])
        return f"{fn_name}({args})"

    elif kind == "task_call":
        fn_name = make_task_call_fn_name(node)
        args = ", ".join(a.replace("$", "_arr") for a in node["args"])
        return f"list({fn_name}({args}))"

    elif kind == "binop":
        left = _emit_expr(node["left"])
        right = _emit_expr(node["right"])
        return f"{left} {node['op']} {right}"

    elif kind == "array_fn":
        if node["name"] == "length_of":
            arg = _emit_expr(node["args"][0])
            return f"len({arg})"

    elif kind == "where":
        arr = _emit_expr(node["array"])
        cond = _emit_expr(_replace_underscore(node["condition"], "x"))
        return f"[x for x in {arr} if {cond}]"

    elif kind == "first_where":
        arr = _emit_expr(node["array"])
        cond = _emit_expr(_replace_underscore(node["condition"], "x"))
        return f"next(x for x in {arr} if {cond})"

    elif kind == "index_of_first_where":
        arr = _emit_expr(node["array"])
        cond = _emit_expr(_replace_underscore(node["condition"], "x"))
        return f"next(i for i, x in enumerate({arr}) if {cond})"

    elif kind == "indices_where":
        arr = _emit_expr(node["array"])
        cond = _emit_expr(_replace_underscore(node["condition"], "x"))
        return f"[i for i, x in enumerate({arr}) if {cond}]"

    elif kind == "index":
        arr = _emit_expr(node["array"])
        idx = _emit_expr(node["index"])
        return f"{arr}[{idx}]"

    elif kind == "slice":
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

    elif kind == "member":
        if node["field"] == "char$":
            return f"list({node['object']})"
        return f"{node['object']}.{node['field']}"

    elif kind == "name":
        val = node["value"]
        if val in _enum_values:
            return _enum_values[val]
        if val.endswith("$"):
            return val.replace("$", "_arr")
        return val


    elif kind == "literal":
        return str(node["value"])

    elif kind == "ternary":
        true = _emit_expr(node["true"])
        cond = _emit_expr(node["condition"])
        false = _emit_expr(node["false"])
        return f"({true}) if ({cond}) else ({false})"

    elif kind == "raw":
        return node["value"]

    return str(node)


# --- CLI ---

if __name__ == "__main__":
    source = open(sys.argv[1]).read()
    ir = process(source)
    print(emit(ir))
