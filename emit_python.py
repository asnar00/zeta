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
)

# Python builtins that don't need type aliases
_BUILTINS = {"int", "float", "string"}

# Zero type names to Python type names
_PY_TYPE_MAP = {"string": "str"}


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

    # build type lookup for parent field resolution
    all_types = {t["name"]: t for t in ir["types"]}

    for typ in ir["types"]:
        code = _emit_type(typ, enums, all_types)
        if code:
            sections.append(code)

    for task in ir.get("tasks", []):
        sections.append(_emit_task(task))

    for fn in ir["functions"]:
        if not fn.get("abstract"):
            sections.append(_emit_function(fn))

    # generate dispatch functions for multiple dispatch groups
    concrete_fns = [fn for fn in ir["functions"] if not fn.get("abstract")]
    dispatch_sections = _generate_dispatchers(concrete_fns, ir.get("types", []))

    # variables last — they may reference functions and tasks
    var_lines = []
    for var in ir["variables"]:
        var_lines.append(_emit_variable(var))
    # bare function call statements
    for stmt in ir.get("statements", []):
        var_lines.append(_emit_expr(stmt))
    if var_lines:
        sections.append("\n".join(var_lines))
    sections.extend(dispatch_sections)

    return "\n\n".join(sections) + "\n" if sections else ""


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


def _emit_array_map_expr(node: dict) -> str:
    """Emit a list comprehension for array-mapped expressions."""
    array_refs = list(dict.fromkeys(_collect_array_refs(node)))  # deduplicate, preserve order
    if len(array_refs) == 0:
        return _emit_expr(node)
    elif len(array_refs) == 1:
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



def _emit_if_block(node: dict) -> str:
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
            lines.append(f"    {_emit_expr(stmt)}")
    return "\n".join(lines)


def _emit_task(task: dict) -> str:
    """Emit a task as a Python generator function."""
    name_parts = task["name_parts"]
    all_params = task.get("input_streams", []) + task.get("params", [])
    param_strs = []
    for p in all_params:
        pname = p["name"] + "_arr" if p in task.get("input_streams", []) else p["name"]
        param_strs.append(f"{pname}: {_py_type_ann(p['type'])}")
    params_str = ", ".join(param_strs)

    # build function name from name parts + param types
    fn_name = "fn_" + "_".join(name_parts)
    for p in all_params:
        fn_name += f"__{p['type']}"

    output_stream_name = task["output"]["name"] + "$"

    lines = [f"def {fn_name}({params_str}):"]

    # translate body lines with indent tracking
    # base indent inside the generator is 4 spaces
    # consume (for loop) adds another level
    base_indent = "    "
    extra_indent = ""

    for body_line in task["body"]:
        # consume: type name <- stream$
        consume_match = re.match(r"\w+\s+(\w+)\s*<-\s*(\w+\$)", body_line)
        if consume_match:
            var_name = consume_match.group(1)
            stream_name = consume_match.group(2).replace("$", "_arr")
            lines.append(f"{base_indent}for {var_name} in {stream_name}:")
            extra_indent = "    "
            continue

        # emit: output$ <- expr
        emit_match = re.match(r"(\w+\$)\s*<-\s*(.*)", body_line.strip())
        if emit_match and emit_match.group(1) == output_stream_name:
            expr = emit_match.group(2).strip()
            lines.append(f"{base_indent}{extra_indent}yield {expr}")
            continue

        # if block
        if_match = re.match(r"if\s+\((.+)\)", body_line.strip())
        if if_match:
            cond = if_match.group(1)
            lines.append(f"{base_indent}{extra_indent}if {cond}:")
            extra_indent += "    "
            continue

        lines.append(f"{base_indent}{extra_indent}{body_line}")

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
        lines = [f"def {name}({params}) -> {ret_type}:"]
        for stmt in fn["body"]:
            lines.extend(_indent(_emit_expr(stmt), "    "))
        lines.append(f"    return {result_var}")
    else:
        lines = [f"def {name}({params}):"]
        for stmt in fn["body"]:
            lines.extend(_indent(_emit_expr(stmt), "    "))
    return "\n".join(lines)


def _indent(text: str, prefix: str) -> list[str]:
    """Indent each line of a multi-line string."""
    return [prefix + line for line in text.split("\n")]



# --- expression emitter ---

def _emit_expr(node: dict) -> str:
    """Emit a Python expression from an AST node."""
    kind = node["kind"]

    if kind == "if_block":
        return _emit_if_block(node)

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
