"""Shared emitter base: common logic for all target language emitters."""

import re

# Symbol-to-word mapping for function names (shared across all targets)
SYMBOL_WORDS = {
    "+": "plus", "-": "minus", "*": "times", "/": "div", "%": "mod",
    "==": "eq", "!=": "neq", "<": "lt", ">": "gt", "<=": "lte", ">=": "gte",
    "&": "band", "|": "bor", "^": "bxor", "<<": "shl", ">>": "shr",
}

SUPPORTED_VERSION = 1
SUPPORTED_FEATURES = {
    "numeric_types", "enums", "structs", "arrays", "functions",
    "void_functions", "concurrently", "array_map", "array_reduce",
    "fn_calls", "strings", "streaming", "tasks", "conditionals",
    "slicing", "array_fns", "indexing", "filtering", "sorting",
}


def check_compatibility(ir: dict):
    """Check that an emitter can handle the given IR."""
    version = ir.get("version", 0)
    if version > SUPPORTED_VERSION:
        raise ValueError(f"IR version {version} is not supported (max {SUPPORTED_VERSION})")
    unsupported = ir.get("features", set()) - SUPPORTED_FEATURES
    if unsupported:
        raise ValueError(f"unsupported features: {', '.join(sorted(unsupported))}")


def get_base_name(sig_parts: list[str]) -> str:
    """Extract the base function name (words only, no type params)."""
    words = []
    for part in sig_parts:
        if not part.startswith("(") and not part.startswith("["):
            words.append(part)
    return "_".join(words)


def make_function_name(signature_parts: list[str]) -> str:
    """Generate a function name from signature parts.
    Words stay as-is, symbols become words, param types get double-underscore prefix.
    """
    result = "fn"
    for part in signature_parts:
        param_match = re.match(r"\((\w+)(?:\s+\w+\$?)?\)", part)
        array_param_match = re.match(r"\[(\w+\$?)\]", part)
        if param_match:
            result += f"__{param_match.group(1)}"
        elif array_param_match:
            pass  # generic array params omitted from name
        elif part in SYMBOL_WORDS:
            if result and not result.endswith("_"):
                result += "_"
            result += SYMBOL_WORDS[part]
        else:
            if result and not result.endswith("_"):
                result += "_"
            result += part
    return result


def make_function_name_from_reduce(fn_parts: list[str], zero_type: str) -> str:
    """Build a function name from reduce fn_parts, replacing array/accumulator refs with types."""
    result = "fn"
    for part in fn_parts:
        if re.match(r"\(\w+\$\)", part):
            result += f"__{zero_type}"
        elif part in ("(..)", "(_)"):
            result += f"__{zero_type}"
        elif part in SYMBOL_WORDS:
            if not result.endswith("_"):
                result += "_"
            result += SYMBOL_WORDS[part]
        else:
            if not result.endswith("_"):
                result += "_"
            result += part
    return result


def collect_array_refs(node: dict) -> list[str]:
    """Collect all array reference names (with $) from an expression AST."""
    refs = []
    if node["kind"] == "name" and node["value"].endswith("$"):
        refs.append(node["value"])
    elif node["kind"] == "binop":
        refs.extend(collect_array_refs(node["left"]))
        refs.extend(collect_array_refs(node["right"]))
    elif node["kind"] == "fn_call":
        for arg in node["args"]:
            refs.extend(collect_array_refs(arg))
    return refs


def rewrite_array_ref(node: dict, array_name: str, replacement: str) -> dict:
    """Replace an array reference with a replacement in an AST.
    For Python, replacement is a loop variable name.
    For TS, replacement can be an expression like '(arr[i] ?? 0)'."""
    if node["kind"] == "name" and node["value"] == array_name:
        return {"kind": "raw" if "(" in replacement or "[" in replacement else "name", "value": replacement}
    elif node["kind"] == "binop":
        return {
            "kind": "binop",
            "op": node["op"],
            "left": rewrite_array_ref(node["left"], array_name, replacement),
            "right": rewrite_array_ref(node["right"], array_name, replacement),
        }
    elif node["kind"] == "fn_call":
        return {
            "kind": "fn_call",
            "signature_parts": node["signature_parts"],
            "args": [rewrite_array_ref(a, array_name, replacement) for a in node["args"]],
        }
    return node


def replace_underscore(node: dict, replacement: str) -> dict:
    """Replace _ references with a replacement name in an AST."""
    if not isinstance(node, dict):
        return node
    if node.get("kind") == "name" and node.get("value") == "_":
        return {"kind": "name", "value": replacement}
    if node.get("kind") == "member" and node.get("object") == "_":
        return {"kind": "member", "object": replacement, "field": node["field"]}
    result = {}
    for key, val in node.items():
        if isinstance(val, dict):
            result[key] = replace_underscore(val, replacement)
        elif isinstance(val, list):
            result[key] = [replace_underscore(v, replacement) if isinstance(v, dict) else v for v in val]
        else:
            result[key] = val
    return result


def compute_dispatch_groups(functions: list[dict], types: list[dict]) -> dict:
    """Group functions by base name and sort by type specificity.
    Returns {base_name: [sorted_fns]} for groups with multiple definitions."""
    groups = {}
    for fn in functions:
        base = get_base_name(fn["signature_parts"])
        if base not in groups:
            groups[base] = []
        groups[base].append(fn)

    type_parents = {}
    for t in types:
        if t["kind"] == "struct":
            type_parents[t["name"]] = t.get("parents", [])

    result = {}
    for base, fns in groups.items():
        if len(fns) <= 1:
            continue

        def _specificity(fn):
            param_type = fn["params"][0]["type"] if fn["params"] else ""
            parents = type_parents.get(param_type, [])
            depth = 0
            visited = set()
            to_check = list(parents)
            while to_check:
                p = to_check.pop(0)
                if p not in visited:
                    visited.add(p)
                    depth += 1
                    to_check.extend(type_parents.get(p, []))
            return -depth

        result[base] = sorted(fns, key=_specificity)

    return result


def make_task_fn_name(task: dict) -> str:
    """Build the function name for a task."""
    name_parts = task["name_parts"]
    all_params = task.get("input_streams", []) + task.get("params", [])
    fn_name = "task_" + "_".join(name_parts)
    for p in all_params:
        fn_name += f"__{p['type']}"
    return fn_name


def collect_all_fields(typ: dict, all_types: dict) -> list[dict]:
    """Collect all fields including inherited parent fields."""
    fields = []
    for parent_name in typ.get("parents", []):
        parent = all_types.get(parent_name)
        if parent and parent["kind"] == "struct":
            fields.extend(collect_all_fields(parent, all_types))
    fields.extend(typ["fields"])
    return fields


def zero_prototype(fn: dict) -> str:
    """Reconstruct the zero declaration prototype from a function IR dict."""
    sig = " ".join(fn["signature_parts"])
    if fn.get("result"):
        return f"on ({fn['result']['type']} {fn['result']['name']}) = {sig}"
    return f"on {sig}"


def zero_task_prototype(task: dict) -> str:
    """Reconstruct the zero declaration prototype from a task IR dict."""
    out = task["output"]
    parts = []
    for p in task.get("input_streams", []):
        parts.append(f"({p['type']} {p['name']}$)")
    for p in task.get("params", []):
        parts.append(f"({p['type']} {p['name']})")
    name = " ".join(task["name_parts"])
    # interleave name parts and params
    sig = " ".join(task["name_parts"])
    for p in task.get("input_streams", []) + task.get("params", []):
        pname = p["name"] + "$" if p in task.get("input_streams", []) else p["name"]
        sig += f" ({p['type']} {pname})"
    if out is None:
        return f"on {sig}"
    return f"on ({out['type']} {out['name']}$) <- {sig}"


def source_comment(fn_or_task: dict, source_file: str = None, comment_char: str = "#") -> str:
    """Build a @zero source comment for a function or task."""
    if "name_parts" in fn_or_task:
        proto = zero_task_prototype(fn_or_task)
    else:
        proto = zero_prototype(fn_or_task)
    loc = ""
    if source_file and fn_or_task.get("source_line"):
        loc = f"; {source_file}:{fn_or_task['source_line']}"
    return f"{comment_char} @zero {proto}{loc}"


def make_task_call_fn_name(call: dict) -> str:
    """Build the function name for a task call."""
    sig_parts = call["signature_parts"]
    fn_name = "task_" + "_".join(p for p in sig_parts if not p.startswith("("))
    for p in sig_parts:
        if p.startswith("("):
            fn_name += f"__{p[1:-1]}"
    return fn_name
