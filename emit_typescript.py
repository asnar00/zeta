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
)

# TypeScript builtin that needs no alias
_BUILTINS = {"number"}

# Zero numeric base types to TS types
_TS_NUMERIC = {"int": "number", "float": "number", "int | float": "number"}

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

    for typ in ir["types"]:
        code = _emit_type(typ, enums, structs)
        if code:
            sections.append(code)

    var_lines = []
    for var in ir["variables"]:
        var_lines.append(_emit_variable(var, structs))
    if var_lines:
        sections.append("\n".join(var_lines))

    for task in ir.get("tasks", []):
        sections.append(_emit_task_ts(task))

    # compute which functions are async (contain concurrently or call async fns)
    async_fns = _compute_async_fns(ir["functions"])

    for fn in ir["functions"]:
        sections.append(_emit_function(fn, structs, async_fns))

    # generate dispatch functions for multiple dispatch groups
    dispatch_sections = _generate_dispatchers_ts(ir["functions"], ir.get("types", []))
    sections.extend(dispatch_sections)

    return "\n\n".join(sections) + "\n" if sections else ""


def _generate_dispatchers_ts(functions: list[dict], types: list[dict]) -> list[str]:
    """Generate dispatch functions for function groups with multiple definitions."""
    dispatch_groups = compute_dispatch_groups(functions, types)
    sections = []
    for base, sorted_fns in dispatch_groups.items():
        dispatcher_name = "fn_" + base
        param_names = [p["name"] for p in sorted_fns[0]["params"]]
        params_str = ", ".join(param_names)

        ret_type = sorted_fns[0]["result"]["type"] if sorted_fns[0].get("result") else "void"

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
    return _TS_NUMERIC.get(zero_type, zero_type)


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
            iface_lines.append(f"    readonly {field['name']}: {field['type']};")
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
        return f"{field['type']}()"

    if field["type"] in enums:
        enum = enums[field["type"]]
        if str(default_val) in enum["values"]:
            ts_name = _ts_name(field["type"])
            return f"{ts_name}.{default_val}"
    return str(default_val)


def _emit_variable(var: dict, structs: dict) -> str:
    """Emit a variable declaration."""
    name = var["name"] + "_arr" if var["array"] else var["name"]
    type_ann = var["type"]

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
        return f"{name} = {_emit_expr(val, structs or {})};"
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
    args = ", ".join(a.replace("$", "_arr") for a in call["args"])
    return f"{name} = [...{fn_name}({args})]"


def _emit_element(v, structs: dict = None) -> str:
    """Emit a single array element — raw value or AST node."""
    if isinstance(v, dict) and "kind" in v:
        return _emit_expr(v, structs or {})
    return str(v)


def _emit_array_map_expr(node: dict, structs: dict) -> str:
    """Emit array-mapped expressions using .map() or Array.from()."""
    array_refs = list(dict.fromkeys(_collect_array_refs(node)))  # deduplicate, preserve order
    if len(array_refs) == 0:
        return _emit_expr(node, structs)
    elif len(array_refs) == 1:
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


def _emit_task_ts(task: dict) -> str:
    """Emit a task as a TypeScript generator function."""
    name_parts = task["name_parts"]
    all_params = task.get("input_streams", []) + task.get("params", [])
    param_strs = []
    for p in all_params:
        pname = p["name"] + "_arr" if p in task.get("input_streams", []) else p["name"]
        param_strs.append(f"{pname}: {p['type']}[]" if p in task.get("input_streams", []) else f"{pname}: {p['type']}")
    params_str = ", ".join(param_strs)

    fn_name = "fn_" + "_".join(name_parts)
    for p in all_params:
        fn_name += f"__{p['type']}"

    output_type = task["output"]["type"]
    output_stream_name = task["output"]["name"] + "$"

    lines = [f"function* {fn_name}({params_str}): Generator<{output_type}> {{"]

    base_indent = "    "
    extra_indent = ""

    for body_line in task["body"]:
        consume_match = re.match(r"\w+\s+(\w+)\s*<-\s*(\w+\$)", body_line)
        if consume_match:
            var_name = consume_match.group(1)
            stream_name = consume_match.group(2).replace("$", "_arr")
            lines.append(f"{base_indent}for (const {var_name} of {stream_name}) {{")
            extra_indent = "    "
            continue

        emit_match = re.match(r"(\w+\$)\s*<-\s*(.*)", body_line.strip())
        if emit_match and emit_match.group(1) == output_stream_name:
            expr = emit_match.group(2).strip()
            lines.append(f"{base_indent}{extra_indent}yield {expr};")
            continue

        if_match = re.match(r"if\s+\((.+)\)", body_line.strip())
        if if_match:
            cond = if_match.group(1)
            lines.append(f"{base_indent}{extra_indent}if ({cond}) {{")
            extra_indent += "    "
            continue

        lines.append(f"{base_indent}{extra_indent}{body_line}")

    # close braces
    if extra_indent:
        indent = base_indent
        while extra_indent:
            extra_indent = extra_indent[4:]
            lines.append(f"{indent}{'    ' * (len(extra_indent) // 4)}}}")
    lines.append("}")

    return "\n".join(lines)


def _emit_function(fn: dict, structs: dict, async_fns: set[str] = None) -> str:
    """Emit a function definition."""
    async_fns = async_fns or set()
    name = _make_function_name(fn["signature_parts"])
    param_strs = []
    for p in fn["params"]:
        pname = p["name"].replace("$", "_arr")
        if p["type"] is not None:
            param_strs.append(f"{pname}: {p['type']}")
        else:
            param_strs.append(pname)
    params = ", ".join(param_strs)
    is_async = name in async_fns
    prefix = "async function" if is_async else "function"

    if fn["result"] is not None:
        ret_type = fn["result"]["type"]
        result_var = fn["result"]["name"]
        # if body has if_blocks, declare result var with let at top
        has_if = any(isinstance(s, dict) and s.get("kind") == "if_block" for s in fn["body"])
        lines = [f"{prefix} {name}({params}): {ret_type} {{"]
        if has_if:
            lines.append(f"    let {result_var}: {ret_type};")
        for stmt in fn["body"]:
            lines.append(f"    {_emit_stmt(stmt, ret_type, structs, async_fns, result_var=result_var if has_if else None)}")
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

    if kind == "call":
        name = node["name"]
        if name in structs:
            # struct constructor -> object literal via factory
            fields = structs[name]["fields"]
            args = node["args"]
            parts = [f"{fields[i]['name']}: {_emit_expr(a, structs)}" for i, a in enumerate(args)]
            return f"{name}({{ {', '.join(parts)} }})"
        args = ", ".join(_emit_expr(a, structs) for a in node["args"])
        return f"{name}({args})"

    elif kind in ("fn_call", "array_fn_call"):
        fn_name = _make_function_name(node["signature_parts"])
        args = ", ".join(_emit_expr(a, structs) for a in node["args"])
        return f"{fn_name}({args})"

    elif kind == "binop":
        left = _emit_expr(node["left"], structs)
        right = _emit_expr(node["right"], structs)
        return f"{left} {node['op']} {right}"

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
