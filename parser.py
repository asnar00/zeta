"""Zero language processor: parses zero source into a language-agnostic IR."""

import re

IR_VERSION = 1


class ZeroParseError(Exception):
    """Error raised when zero source cannot be parsed."""

    def __init__(self, message: str, line_num: int, line_text: str, column: int = 0):
        self.message = message
        self.line_num = line_num
        self.line_text = line_text
        self.column = column
        super().__init__(self.format())

    def format(self) -> str:
        pointer = " " * self.column + "^"
        return f"line {self.line_num}: {self.message}\n    {self.line_text}\n    {pointer}"


def _is_markdown(source: str) -> bool:
    """Detect if source is markdown (has # headers or ``` fences)."""
    for line in source.split("\n"):
        stripped = line.strip()
        if stripped.startswith("# ") or stripped.startswith("## ") or stripped.startswith("```"):
            return True
        if stripped.startswith("*") and stripped.endswith("*") and len(stripped) > 2:
            return True
    return False


def _extract_code(source: str) -> tuple[str, list[int]]:
    """Extract code from markdown source.
    Collects indented lines (removing 4-space indent) and fenced code blocks.
    Returns (code_string, line_map) where line_map[i] is the 1-based source line
    number for code line i."""
    lines = source.split("\n")
    code_lines = []
    line_map = []  # code line index -> original 1-based line number
    in_fence = False
    for idx, line in enumerate(lines):
        orig_line = idx + 1
        # fenced code block
        if line.strip().startswith("```"):
            in_fence = not in_fence
            continue
        if in_fence:
            code_lines.append(line)
            line_map.append(orig_line)
            continue
        # indented line (4 spaces or tab)
        if line.startswith("    ") or line.startswith("\t"):
            code_lines.append(line)
            line_map.append(orig_line)
        elif line.strip() == "":
            code_lines.append("")
            line_map.append(orig_line)
        # non-indented non-empty lines are prose — skip
    return "\n".join(code_lines), line_map


def process(source: str, recover: bool = False, source_file: str = None) -> dict:
    """Parse zero source text and return an IR dict.
    If recover=True, collects errors and continues instead of raising on first error."""
    ir = {"version": IR_VERSION, "features": set(), "types": [], "variables": [], "functions": [], "tasks": []}
    if source_file:
        ir["source_file"] = source_file
    # extract code from markdown if source looks like markdown
    line_map = None
    if _is_markdown(source):
        source, line_map = _extract_code(source)
    lines = source.split("\n")

    def _src_line(code_idx: int) -> int:
        """Map a code line index to the original source line number."""
        if line_map is not None and code_idx < len(line_map):
            return line_map[code_idx]
        return code_idx + 1

    # first pass: collect function and task signatures for call resolution
    fn_signatures = _collect_signatures(lines)
    task_signatures = _collect_task_signatures(lines)

    errors = []
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        try:
            if line.startswith("type "):
                typ, consumed = _parse_type(lines, i, _src_line)
                ir["types"].append(typ)
                i += consumed
            elif _is_task_def(line):
                task, consumed = _parse_task(lines, i, _src_line)
                ir["tasks"].append(task)
                i += consumed
            elif line.startswith("on "):
                fn, consumed = _parse_function(lines, i, fn_signatures, _src_line, task_signatures)
                ir["functions"].append(fn)
                i += consumed
            elif line and not line.startswith("#"):
                # try as a bare function call statement (has parens, no '=')
                if "(" in line and "=" not in line:
                    fn_call = _try_parse_fn_call(line, fn_signatures)
                    if fn_call:
                        ir.setdefault("statements", []).append(fn_call)
                        i += 1
                        continue
                var = _parse_variable(line, fn_signatures, task_signatures)
                if var:
                    ir["variables"].append(var)
                else:
                    ir.setdefault("warnings", []).append(
                        f"line {_src_line(i)}: unrecognized: {line}"
                    )
                i += 1
            else:
                i += 1
        except ZeroParseError as e:
            if not recover:
                raise
            errors.append(e)
            # skip to next top-level declaration
            i += 1
            while i < len(lines):
                next_line = lines[i].strip()
                if (next_line.startswith("type ") or next_line.startswith("on ")
                        or _is_task_def(next_line) or next_line == ""):
                    break
                i += 1

    if errors:
        ir["errors"] = errors

    # detect features used
    ir["features"] = _detect_features(ir)
    return ir


def _detect_features(ir: dict) -> set[str]:
    """Detect which language features this IR uses."""
    features = set()

    for typ in ir["types"]:
        if typ["kind"] == "numeric":
            features.add("numeric_types")
        elif typ["kind"] == "enum":
            features.add("enums")
        elif typ["kind"] == "struct":
            features.add("structs")

    for var in ir["variables"]:
        if var.get("array"):
            features.add("arrays")
        val = var.get("value")
        if isinstance(val, dict):
            if val.get("kind") == "reduce":
                features.add("array_reduce")
            elif val.get("kind") == "stream":
                features.add("streaming")
            elif val.get("kind") in ("binop", "fn_call") and _has_array_refs(val):
                features.add("array_map")
            elif val.get("kind") == "fn_call":
                features.add("fn_calls")
        if var.get("type") == "string":
            features.add("strings")

    for fn in ir["functions"]:
        if fn.get("result") is not None:
            features.add("functions")
        else:
            features.add("void_functions")
        for stmt in fn.get("body", []):
            if isinstance(stmt, dict):
                if stmt.get("kind") == "concurrently":
                    features.add("concurrently")
                if stmt.get("kind") == "assign" and isinstance(stmt.get("value"), dict):
                    if stmt["value"].get("kind") == "reduce":
                        features.add("array_reduce")
                _detect_fn_calls(stmt, features)

    if ir.get("tasks"):
        features.add("tasks")

    # detect slicing in expressions
    _detect_in_exprs(ir, features)

    # check for conditionals in function bodies
    for fn in ir["functions"]:
        for stmt in fn.get("body", []):
            if isinstance(stmt, dict) and stmt.get("kind") == "if_block":
                features.add("conditionals")

    for var in ir["variables"]:
        val = var.get("value")
        if isinstance(val, dict) and val.get("kind") == "task_call":
            features.add("tasks")

    return features


def _detect_in_exprs(ir: dict, features: set):
    """Walk all expressions to detect features like slicing."""
    def _walk(node):
        if not isinstance(node, dict):
            return
        if node.get("kind") == "slice":
            features.add("slicing")
        if node.get("kind") == "array_fn":
            features.add("array_fns")
        if node.get("kind") == "index":
            features.add("indexing")
        if node.get("kind") in ("where", "first_where", "index_of_first_where"):
            features.add("filtering")
        if node.get("kind") == "sort":
            features.add("sorting")
        for key in ("value", "left", "right", "condition", "true", "false", "array", "start", "end"):
            child = node.get(key)
            if isinstance(child, dict):
                _walk(child)
        for arg in node.get("args", []):
            if isinstance(arg, dict):
                _walk(arg)
    for fn in ir["functions"]:
        for stmt in fn.get("body", []):
            _walk(stmt)


def _has_array_refs(node: dict) -> bool:
    """Check if an AST node contains array references ($)."""
    if node.get("kind") == "name" and node.get("value", "").endswith("$"):
        return True
    for key in ("left", "right", "value"):
        child = node.get(key)
        if isinstance(child, dict) and _has_array_refs(child):
            return True
    for arg in node.get("args", []):
        if isinstance(arg, dict) and _has_array_refs(arg):
            return True
    return False


def _detect_fn_calls(node: dict, features: set):
    """Recursively detect fn_call nodes in an AST."""
    if not isinstance(node, dict):
        return
    if node.get("kind") == "fn_call":
        features.add("fn_calls")
    for key in ("value", "left", "right", "condition", "true", "false"):
        child = node.get(key)
        if isinstance(child, dict):
            _detect_fn_calls(child, features)
    for arg in node.get("args", []):
        if isinstance(arg, dict):
            _detect_fn_calls(arg, features)


def _collect_signatures(lines: list[str]) -> list[dict]:
    """First pass: collect function signatures for resolving calls in bodies."""
    signatures = []
    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("on "):
            continue
        # try value-returning: on (type result) = signature
        match = re.match(r"on\s+\((\w+)\s+(\w+)\)\s*=\s*(.*)", stripped)
        if match:
            rhs = match.group(3).strip()
        else:
            # try void: on signature
            void_match = re.match(r"on\s+(.*)", stripped)
            if not void_match:
                continue
            rhs = void_match.group(1).strip()
        params, sig_parts = _parse_signature(rhs)
        # build a pattern: list of ("word", word) or ("param", type) or ("array_param", name) tokens
        pattern = []
        has_array_params = False
        for part in sig_parts:
            param_match = re.match(r"\((\w+)\s+\w+\$?\)", part)
            array_param_match = re.match(r"\[(\w+\$?)\]", part)
            if array_param_match:
                pattern.append(("array_param", array_param_match.group(1)))
                has_array_params = True
            elif param_match:
                pattern.append(("param", param_match.group(1)))
            else:
                pattern.append(("word", part))
        signatures.append({
            "pattern": pattern,
            "has_array_params": has_array_params,
            "signature_parts": [
                f"[{p[1]}]" if p[0] == "array_param" else f"({p[1]})" if p[0] == "param" else p[1] for p in pattern
            ],
        })
    return signatures


def _parse_type(lines: list[str], start: int, src_line=None) -> tuple[dict, int]:
    """Parse a type declaration starting at the given line index.
    Returns (type_dict, number_of_lines_consumed)."""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1

    # check for 'type =' (missing name)
    if re.match(r"type\s*=", line):
        raise ZeroParseError("expected type name after 'type'", line_num, line, column=5)

    # abstract base type or struct without '=': "type animal" possibly followed by indented fields
    abstract_match = re.match(r"type\s+(\w+)\s*$", line)
    if abstract_match:
        # check if next line is indented and looks like a field (not another declaration)
        has_fields = False
        if start + 1 < len(lines) and lines[start + 1] and lines[start + 1][0:1].isspace():
            next_stripped = lines[start + 1].strip()
            if next_stripped and not next_stripped.startswith("type ") and not next_stripped.startswith("on "):
                has_fields = True
        if has_fields:
            return _parse_struct_type(abstract_match.group(1), lines, start, line_num, src_line=src_line)
        return {"kind": "struct", "name": abstract_match.group(1), "fields": [], "parents": []}, 1

    # type composition: "type dog = animal +" or "type pet = animal + named +"
    comp_match = re.match(r"type\s+(\w+)\s*=\s*(.+\+)\s*$", line)
    if comp_match:
        name = comp_match.group(1)
        rhs = comp_match.group(2).strip()
        # split on + to get parent types (last + has nothing after it)
        parts = [p.strip() for p in rhs.split("+") if p.strip()]
        parents = parts
        return _parse_struct_type(name, lines, start, line_num, parents=parents, src_line=src_line)

    match = re.match(r"type\s+(\S+)\s*=\s*(.*)", line)
    if not match:
        # find where '=' should be — after the second token
        after_name = re.match(r"type\s+(\S+)\s*(.*)", line)
        col = len("type ") + len(after_name.group(1)) if after_name else len(line)
        raise ZeroParseError("expected '=' in type declaration", line_num, line, column=col)

    name = match.group(1)
    rest = match.group(2).strip()

    # numeric type with agent instruction: type int32 = ... description
    if rest.startswith("..."):
        return _parse_numeric_type(name, rest), 1

    # check if rest is empty (struct with fields on next lines)
    if rest == "":
        return _parse_struct_type(name, lines, start, line_num, src_line=src_line)

    # union or enum: distinguish by whether values look like type names or enum values
    # enum: single words separated by |, not matching known type patterns
    parts = [p.strip() for p in rest.split("|")]
    if _looks_like_enum(parts):
        return {"kind": "enum", "name": name, "values": parts}, 1
    else:
        # union type alias like "int | float"
        base = " | ".join(parts)
        return {"kind": "numeric", "name": name, "base": base, "size": None}, 1


_NUMERIC_BASES = {"int", "uint", "float"}


def _looks_like_enum(parts: list[str]) -> bool:
    """Return True if the parts look like enum values rather than type names."""
    for p in parts:
        if p in _NUMERIC_BASES:
            return False
        if re.match(r"(int|uint|float)\d+", p):
            return False
    return True


def _parse_numeric_type(name: str, rest: str) -> dict:
    """Parse a numeric type from its name and the RHS after '='."""
    # extract size from name if present
    size_match = re.search(r"\d+$", name)
    size = int(size_match.group()) if size_match else None

    # determine base type
    if name.startswith("float"):
        base = "float"
    else:
        base = "int"

    return {"kind": "numeric", "name": name, "base": base, "size": size}


def _parse_struct_type(name: str, lines: list[str], start: int, line_num: int = None, parents: list = None, src_line=None) -> tuple[dict, int]:
    """Parse a struct type with indented field lines."""
    if line_num is None:
        line_num = src_line(start) if src_line else start + 1
    if parents is None:
        parents = []
    fields = []
    i = start + 1
    while i < len(lines):
        field_line = lines[i]
        # struct fields are more indented than the type line
        if not field_line or not field_line[0:1].isspace() or field_line.strip() == "":
            break
        # check it's more indented (continuation)
        stripped = field_line.strip()
        if stripped.startswith("type ") or stripped.startswith("on "):
            break
        # stop if line looks like a variable declaration (has $ or [)
        if "$" in stripped or re.match(r"\S+\s+\w+\[", stripped):
            break
        # struct fields must be more indented than the type line
        type_indent = len(lines[start]) - len(lines[start].lstrip())
        field_indent = len(field_line) - len(field_line.lstrip())
        if field_indent <= type_indent:
            break
        fields.extend(_parse_struct_fields(stripped))
        i += 1
    if not fields and not parents:
        raise ZeroParseError("expected struct fields after type declaration", line_num, lines[start].strip(), column=len(lines[start].strip()))
    return {"kind": "struct", "name": name, "fields": fields, "parents": parents}, i - start


def _parse_struct_fields(line: str) -> list[dict]:
    """Parse a struct field line like 'number x, y, z = 0'."""
    # format: type name1, name2, ... = default
    match = re.match(r"(\S+)\s+(.+)", line)
    type_name = match.group(1)
    rest = match.group(2)

    # split off default value (0 if not specified)
    default = 0
    if "=" in rest:
        names_part, default_part = rest.rsplit("=", 1)
        default = _parse_literal(default_part.strip())
    else:
        names_part = rest

    names = [n.strip() for n in names_part.split(",")]
    return [{"name": n, "type": type_name, "default": default} for n in names]


def _parse_variable(line: str, fn_sigs: list = None, task_sigs: list = None) -> dict | None:
    """Parse a variable declaration like 'int32 i = 10' or 'int i$[4] = 1'."""
    # match: type name[$][size] [= value]
    match = re.match(r"(\S+)\s+(\w+)(\$)?\s*(.*)", line)
    if not match:
        return None

    type_name = match.group(1)
    var_name = match.group(2)
    is_array = match.group(3) is not None
    rest = match.group(4).strip()

    if is_array:
        return _parse_array_variable(type_name, var_name, rest, fn_sigs, task_sigs)
    else:
        return _parse_scalar_variable(type_name, var_name, rest, fn_sigs)


def _parse_scalar_variable(type_name: str, var_name: str, rest: str, fn_sigs: list = None) -> dict:
    """Parse a scalar variable from its type, name, and remaining text."""
    value = None
    if rest.startswith("="):
        rhs = rest[1:].strip()
        # check for index of first in [...] where (...)
        index_first = _try_parse_index_of_first_where(rhs, fn_sigs)
        if index_first:
            value = index_first
        # check for first of [...] where (...)
        elif _try_parse_first_where(rhs, fn_sigs):
            value = _try_parse_first_where(rhs, fn_sigs)
        # check for reduce expression (contains _ as standalone reduce marker)
        elif _is_reduce_expr(rhs):
            value = _parse_reduce(rhs)
        # check for constructor call: name(...)
        elif re.match(r"(\w+)\(([^)]*)\)$", rhs):
            call_match = re.match(r"(\w+)\(([^)]*)\)", rhs)
            value = _parse_constructor_call(call_match.group(1), call_match.group(2))
        # try function call
        elif fn_sigs and _try_parse_fn_call(rhs, fn_sigs):
            value = _try_parse_fn_call(rhs, fn_sigs)
        else:
            # try as expression (handles indexing, slicing, binops, etc.)
            parsed = _parse_expr(rhs, fn_sigs)
            if parsed.get("kind") == "literal":
                value = parsed["value"]  # keep raw value for simple literals
            else:
                value = parsed
    return {"name": var_name, "type": type_name, "array": False, "value": value}


def _try_parse_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse '[array$] where (condition)'."""
    match = re.match(r"\[(\w+\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        # parse condition, replacing _ with __elem__
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "where", "array": array_expr, "condition": cond}
    return None


def _try_parse_first_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'first of [array$] where (condition)'."""
    match = re.match(r"first of \[(\w+\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "first_where", "array": array_expr, "condition": cond}
    return None


def _try_parse_index_of_first_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'index of first in [array$] where (condition)'."""
    match = re.match(r"index of first in \[(\w+\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "index_of_first_where", "array": array_expr, "condition": cond}
    return None


def _try_parse_sort(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'sort [array$]' or 'sort [array$] by (key_expr)'."""
    # sort with key: sort [a$] by (_.field)
    match_by = re.match(r"sort\s+\[(\w+\$?)\]\s+by\s+\((.+)\)(\s+descending)?$", rhs)
    if match_by:
        array_expr = _parse_expr(match_by.group(1), fn_sigs)
        key_expr = _parse_expr(match_by.group(2), fn_sigs)
        descending = match_by.group(3) is not None
        return {"kind": "sort", "array": array_expr, "key": key_expr, "descending": descending}
    # simple sort: sort [a$]
    match_simple = re.match(r"sort\s+\[(\w+\$?)\]$", rhs)
    if match_simple:
        array_expr = _parse_expr(match_simple.group(1), fn_sigs)
        return {"kind": "sort", "array": array_expr, "key": None, "descending": False}
    return None


def _is_reduce_expr(s: str) -> bool:
    """Check if an expression is a reduce (contains _ as the reduce marker)."""
    # _ must appear as a standalone token or inside parens as (_)
    return bool(re.search(r'(?<!\w)_(?!\w)|\(_\)', s))


def _parse_reduce(rhs: str) -> dict:
    """Parse a reduce expression like 'i$ + _' or 'smaller of (i$) and (_)'."""
    # operator reduce: array$ op _
    op_match = re.match(r"(\w+\$)\s*([+\-*/%&|^]|<<|>>)\s*_\s*$", rhs)
    if op_match:
        return {"kind": "reduce", "array": op_match.group(1), "op": op_match.group(2)}

    # function reduce: words and (params) containing one array$ and one (_)
    # extract the array reference and keep the full expression as fn_parts
    parts = re.findall(r"\([^)]*\)|\S+", rhs)
    array_ref = None
    for part in parts:
        # look inside parens for array ref
        inner_match = re.match(r"\((\w+\$)\)", part)
        if inner_match:
            array_ref = inner_match.group(1)
            break
    return {"kind": "reduce", "array": array_ref, "fn_parts": parts}


def _parse_constructor_call(name: str, args_str: str) -> dict:
    """Parse a constructor call like 'vector(1, 2, 3)' or 'vector(z=2, x=1)'."""
    if not args_str.strip():
        return {"kind": "call", "name": name, "args": []}

    args = [a.strip() for a in args_str.split(",")]

    # check if named args (contains '=')
    if "=" in args[0]:
        parsed = []
        for arg in args:
            k, v = arg.split("=", 1)
            parsed.append({"name": k.strip(), "value": _parse_literal(v.strip())})
        return {"kind": "call", "name": name, "args": parsed}
    else:
        return {"kind": "call", "name": name, "args": [_parse_literal(a) for a in args]}


def _parse_array_variable(type_name: str, var_name: str, rest: str, fn_sigs: list = None, task_sigs: list = None) -> dict:
    """Parse an array variable from its type, name (without $), and remaining text."""
    size = None
    value = None

    if rest.startswith("="):
        rhs = rest[1:].strip()
        # check for where filter: = [...] where (...)
        where = _try_parse_where(rhs, fn_sigs)
        if where:
            return {"name": var_name, "type": type_name, "array": True, "size": None, "value": where}
        # check for sort: = sort [...] or sort [...] by (...)
        sort = _try_parse_sort(rhs, fn_sigs)
        if sort:
            return {"name": var_name, "type": type_name, "array": True, "size": None, "value": sort}

    # check for task call: <- task name (args)
    if rest.startswith("<-") and task_sigs:
        rhs = rest[2:].strip()
        task_call = _try_parse_task_call(rhs, task_sigs)
        if task_call:
            return {"name": var_name, "type": type_name, "array": True, "size": None, "value": task_call}

    # check for streaming: <- expr <- expr [while/until (cond)]
    if rest.startswith("<-"):
        value = _parse_stream(rest, var_name, fn_sigs)
        return {"name": var_name, "type": type_name, "array": True, "size": None, "value": value}

    # check for [size] suffix
    size_match = re.match(r"\[(\d+)\]\s*(.*)", rest)
    if size_match:
        size = int(size_match.group(1))
        rest = size_match.group(2).strip()

    if rest.startswith("="):
        rhs = rest[1:].strip()
        # check for [... through ...] or [... to ...]
        range_match = re.match(r"\[(\w+)\s+(through|to)\s+(\w+)\]", rhs)
        if range_match:
            start = _parse_literal(range_match.group(1))
            end = _parse_literal(range_match.group(3))
            value = {
                "range": range_match.group(2),
                "start": start,
                "end": end,
            }
        # check for [...] literal or array of expressions
        elif rhs.startswith("["):
            inner = rhs[1:-1]
            value = _parse_array_elements(inner, fn_sigs)
        # check for expression (contains operators or array refs)
        elif _is_expression(rhs):
            value = _parse_expr(rhs, fn_sigs)
        # try function call
        elif fn_sigs and _try_parse_fn_call(rhs, fn_sigs):
            value = _try_parse_fn_call(rhs, fn_sigs)
        else:
            value = _parse_literal(rhs)

    # no rest at all — empty array
    return {"name": var_name, "type": type_name, "array": True, "size": size, "value": value}


def _parse_array_elements(inner: str, fn_sigs: list = None) -> list:
    """Parse comma-separated array elements, respecting parentheses.
    Returns list of parsed values — literals stay as raw values, expressions become AST nodes."""
    elements = []
    depth = 0
    current = ""
    for ch in inner:
        if ch == "(":
            depth += 1
            current += ch
        elif ch == ")":
            depth -= 1
            current += ch
        elif ch == "," and depth == 0:
            elements.append(_parse_array_element(current.strip(), fn_sigs))
            current = ""
        else:
            current += ch
    if current.strip():
        elements.append(_parse_array_element(current.strip(), fn_sigs))
    return elements


def _parse_array_element(s: str, fn_sigs: list = None) -> any:
    """Parse a single array element — try literal first, fall back to expression."""
    # try simple literal
    try:
        return int(s)
    except ValueError:
        pass
    try:
        return float(s)
    except ValueError:
        pass
    # it's an expression
    return _parse_expr(s, fn_sigs)


def _parse_stream(rest: str, var_name: str, fn_sigs: list = None) -> dict:
    """Parse a streaming expression: <- expr <- expr ... [while/until (cond)]"""
    # split on top-level <- tokens, respecting parens
    parts = _split_stream_parts(rest)

    steps = []
    terminate = None

    for part in parts:
        part = part.strip()
        # check for while/until terminator
        while_match = re.match(r"(while|until)\s+\((.+)\)\s*$", part)
        if while_match:
            kind = while_match.group(1)
            cond_str = while_match.group(2)
            # replace self-references: var$ -> __last__
            cond_str = cond_str.replace(var_name + "$", "__last__")
            terminate = {
                "kind": kind,
                "condition": _parse_expr(cond_str, fn_sigs),
            }
            continue

        # check if this part has a trailing while/until
        for kw in ("until", "while"):
            kw_match = re.match(r"(.+?)\s+" + kw + r"\s+\((.+)\)\s*$", part)
            if kw_match:
                part = kw_match.group(1).strip()
                cond_str = kw_match.group(2).replace(var_name + "$", "__last__")
                terminate = {
                    "kind": kw,
                    "condition": _parse_expr(cond_str, fn_sigs),
                }
                break

        if part:
            # replace self-references in expression
            expr_str = part.replace(var_name + "$", "__last__")
            steps.append(_parse_expr(expr_str, fn_sigs))

    return {"kind": "stream", "steps": steps, "terminate": terminate}


def _split_stream_parts(rest: str) -> list[str]:
    """Split a stream expression on top-level <- tokens."""
    parts = []
    depth = 0
    current = ""
    i = 0
    while i < len(rest):
        if rest[i] == "(":
            depth += 1
            current += rest[i]
        elif rest[i] == ")":
            depth -= 1
            current += rest[i]
        elif rest[i:i+2] == "<-" and depth == 0:
            if current.strip():
                parts.append(current.strip())
            current = ""
            i += 2
            continue
        else:
            current += rest[i]
        i += 1
    if current.strip():
        parts.append(current.strip())
    return parts


def _is_expression(s: str) -> bool:
    """Return True if s looks like an expression rather than a simple literal."""
    # contains operators or array references ($)
    if "$" in s:
        return True
    if any(op in s for op in [" + ", " - ", " * ", " / ", " % ", " & ", " | ", " ^ ", " << ", " >> "]):
        return True
    return False


def _parse_literal(s: str):
    """Parse a literal value string into a Python value."""
    try:
        return int(s)
    except ValueError:
        pass
    try:
        return float(s)
    except ValueError:
        pass
    return s


# --- functions ---

def _parse_function(lines: list[str], start: int, fn_signatures: list = None, src_line=None, task_signatures: list = None) -> tuple[dict, int]:
    """Parse a function definition starting with 'on'."""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1

    # try value-returning function: on (type result) = signature
    result_match = re.match(r"on\s+\((\w+)\s+(\w+)\)\s*=\s*(.*)", line)
    if result_match:
        result = {"name": result_match.group(2), "type": result_match.group(1)}
        rhs = result_match.group(3).strip()
        params, signature_parts = _parse_signature(rhs)
    else:
        # try void function: on signature (no result, no '=')
        void_match = re.match(r"on\s+(.*)", line)
        if not void_match or not void_match.group(1).strip():
            raise ZeroParseError("expected function signature after 'on'", line_num, line, column=3)
        result = None
        rhs = void_match.group(1).strip()
        # strip trailing empty parens from simple names: "hello()" -> "hello"
        params, signature_parts = _parse_signature(rhs)

    # collect indented body lines (must be more indented than the 'on' line)
    # keep raw lines for indent-aware grouping, strip after
    fn_indent = len(lines[start]) - len(lines[start].lstrip())
    raw_body_lines = []
    i = start + 1
    while i < len(lines):
        body_line = lines[i]
        if not body_line or body_line.strip() == "":
            break
        line_indent = len(body_line) - len(body_line.lstrip())
        if line_indent <= fn_indent:
            break
        raw_body_lines.append(body_line)
        i += 1
    body_lines = [l.strip() for l in raw_body_lines]

    if not body_lines:
        # platform/abstract function — no body, just a signature
        return {
            "result": result,
            "params": params,
            "signature_parts": signature_parts,
            "body": [],
            "abstract": True,
            "source_line": line_num,
        }, i - start

    # group multi-line constructs (concurrently, if/else), then parse
    grouped = _group_blocks(body_lines, raw_body_lines, fn_signatures)

    # parse grouped body into AST and check SSA
    body = []
    assigned = set()
    for idx, item in enumerate(grouped):
        if isinstance(item, dict) and item.get("kind") in ("concurrently", "if_block"):
            body.append(item)
            continue
        stmt = _parse_expression(item, fn_signatures, task_signatures)
        if stmt["kind"] == "assign":
            if stmt["target"] in assigned:
                body_line_num = src_line(start + 1 + idx) if src_line else start + 2 + idx
                raise ZeroParseError(
                    f"variable '{stmt['target']}' already assigned (SSA violation)",
                    body_line_num, item, column=0)
            assigned.add(stmt["target"])
        body.append(stmt)

    return {
        "result": result,
        "params": params,
        "signature_parts": signature_parts,
        "body": body,
        "source_line": line_num,
    }, i - start


def _group_blocks(body_lines: list[str], raw_body_lines: list[str], fn_sigs: list = None) -> list:
    """Group multi-line constructs (concurrently, if/else) using indentation.
    Returns a list where each element is a string or a dict node."""
    result = []
    i = 0
    while i < len(body_lines):
        if body_lines[i] == "concurrently":
            conc_indent = len(raw_body_lines[i]) - len(raw_body_lines[i].lstrip())
            blocks = []
            current_block = []
            i += 1
            while i < len(body_lines):
                line_indent = len(raw_body_lines[i]) - len(raw_body_lines[i].lstrip())
                if body_lines[i] == "and" and line_indent == conc_indent:
                    blocks.append(current_block)
                    current_block = []
                    i += 1
                    continue
                if line_indent <= conc_indent and body_lines[i] != "and":
                    break
                current_block.append(body_lines[i])
                i += 1
            blocks.append(current_block)
            blocks = [b for b in blocks if b]
            result.append({"kind": "concurrently", "blocks": blocks})
        elif re.match(r"if\s+\(.+\)", body_lines[i]):
            if_node, consumed = _group_if_block(body_lines, raw_body_lines, i, fn_sigs)
            result.append(if_node)
            i += consumed
        else:
            result.append(body_lines[i])
            i += 1
    return result


def _group_if_block(body_lines, raw_body_lines, start, fn_sigs=None):
    """Group an if/else if/else block into a single AST node."""
    if_indent = len(raw_body_lines[start]) - len(raw_body_lines[start].lstrip())
    branches = []
    i = start

    while i < len(body_lines):
        line = body_lines[i]
        line_indent = len(raw_body_lines[i]) - len(raw_body_lines[i].lstrip())

        # if or else if
        if_match = re.match(r"if\s+\((.+)\)$", line)
        else_if_match = re.match(r"else if\s+\((.+)\)$", line)
        else_match = (line == "else")

        if if_match and line_indent == if_indent and len(branches) == 0:
            cond_str = if_match.group(1)
            cond = _parse_expr(cond_str, fn_sigs)
            body, body_consumed = _collect_indented(body_lines, raw_body_lines, i, if_indent, fn_sigs)
            branches.append({"condition": cond, "body": body})
            i += 1 + body_consumed
        elif else_if_match and line_indent == if_indent:
            cond_str = else_if_match.group(1)
            cond = _parse_expr(cond_str, fn_sigs)
            body, body_consumed = _collect_indented(body_lines, raw_body_lines, i, if_indent, fn_sigs)
            branches.append({"condition": cond, "body": body})
            i += 1 + body_consumed
        elif else_match and line_indent == if_indent:
            body, body_consumed = _collect_indented(body_lines, raw_body_lines, i, if_indent, fn_sigs)
            branches.append({"condition": None, "body": body})
            i += 1 + body_consumed
        else:
            break

    return {"kind": "if_block", "branches": branches}, i - start


def _collect_indented(body_lines, raw_body_lines, start, parent_indent, fn_sigs=None):
    """Collect lines indented more than parent_indent after start.
    Returns (parsed body lines, count of raw lines consumed)."""
    body = []
    i = start + 1
    while i < len(body_lines):
        line_indent = len(raw_body_lines[i]) - len(raw_body_lines[i].lstrip())
        if line_indent <= parent_indent:
            break
        body.append(_parse_expression(body_lines[i], fn_sigs))
        i += 1
    return body, i - start - 1


# --- tasks ---

def _is_task_def(line: str) -> bool:
    """Check if a line is a task definition: on (type name$) <- ..."""
    return bool(re.match(r"on\s+\(\w+\s+\w+\$\)\s*<-", line))


def _collect_task_signatures(lines: list[str]) -> list[dict]:
    """First pass: collect task signatures for resolving task calls."""
    signatures = []
    for line in lines:
        stripped = line.strip()
        match = re.match(r"on\s+\(\w+\s+\w+\$\)\s*<-\s*(.*)", stripped)
        if not match:
            continue
        rhs = match.group(1).strip()
        params, sig_parts = _parse_signature(rhs)
        pattern = []
        for part in sig_parts:
            param_match = re.match(r"\((\w+)\s+\w+(\$)?\)", part)
            if param_match:
                is_stream = param_match.group(2) is not None
                pattern.append(("stream_param" if is_stream else "param", param_match.group(1)))
            else:
                pattern.append(("word", part))
        signatures.append({
            "pattern": pattern,
            "signature_parts": [
                f"({p[1]})" if p[0] in ("param", "stream_param") else p[1] for p in pattern
            ],
        })
    return signatures


def _parse_task(lines: list[str], start: int, src_line=None) -> tuple[dict, int]:
    """Parse a task definition: on (type name$) <- signature"""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1

    match = re.match(r"on\s+\((\w+)\s+(\w+)\$\)\s*<-\s*(.*)", line)
    if not match:
        raise ZeroParseError("expected task definition", line_num, line, column=3)

    output_type = match.group(1)
    output_name = match.group(2)
    rhs = match.group(3).strip()

    # parse signature — separate stream params ($) from scalar params
    all_params, sig_parts = _parse_signature(rhs)
    input_streams = []
    scalar_params = []
    name_parts = []

    for part in sig_parts:
        param_match = re.match(r"\((\w+)\s+(\w+)(\$)?\)", part)
        if param_match:
            ptype = param_match.group(1)
            pname = param_match.group(2)
            is_stream = param_match.group(3) is not None
            if is_stream:
                input_streams.append({"name": pname, "type": ptype})
            else:
                scalar_params.append({"name": pname, "type": ptype})
        else:
            name_parts.append(part)

    # collect body
    fn_indent = len(lines[start]) - len(lines[start].lstrip())
    body_lines = []
    i = start + 1
    while i < len(lines):
        body_line = lines[i]
        if not body_line or body_line.strip() == "":
            break
        line_indent = len(body_line) - len(body_line.lstrip())
        if line_indent <= fn_indent:
            break
        body_lines.append(body_line.strip())
        i += 1

    if not body_lines:
        raise ZeroParseError("expected task body", line_num, line, column=len(line))

    return {
        "name_parts": name_parts,
        "output": {"name": output_name, "type": output_type},
        "input_streams": input_streams,
        "params": scalar_params,
        "body": body_lines,  # keep as raw strings for now
        "source_line": line_num,
    }, i - start


def _try_parse_task_call(rhs: str, task_sigs: list) -> dict | None:
    """Try to match rhs against known task signatures.
    Returns a task_call node or None."""
    for sig in task_sigs:
        result = _try_match_task_sig(rhs, sig)
        if result is not None:
            return result
    return None


def _try_match_task_sig(s: str, sig: dict) -> dict | None:
    """Try to match string s against a single task signature pattern."""
    pattern = sig["pattern"]
    pos = 0
    args = []

    for kind, value in pattern:
        while pos < len(s) and s[pos] == " ":
            pos += 1
        if pos >= len(s):
            return None

        if kind == "word":
            if not s[pos:].startswith(value):
                return None
            end = pos + len(value)
            if end < len(s) and s[end] not in " (":
                return None
            pos = end
        elif kind in ("param", "stream_param"):
            if s[pos] != "(":
                return None
            close = _matching_paren(s, pos)
            if close == -1:
                return None
            inner = s[pos + 1:close].strip()
            args.append(inner)
            pos = close + 1

    remaining = s[pos:].strip()
    if remaining:
        return None

    return {
        "kind": "task_call",
        "signature_parts": sig["signature_parts"],
        "args": args,
    }


def _parse_signature(rhs: str) -> tuple[list[dict], list[str]]:
    """Parse the RHS of a function prototype into params and signature parts.
    e.g. '(vector a) + (vector b)' -> params, ['(vector a)', '+', '(vector b)']
    """
    params = []
    parts = []
    # strip trailing empty parens from void function names: "hello()" -> "hello"
    remaining = re.sub(r"\(\)\s*$", "", rhs).strip()

    while remaining:
        remaining = remaining.strip()
        if not remaining:
            break

        # typed parameter: (type name) or (type name$)
        param_match = re.match(r"\((\w+)\s+(\w+\$?)\)", remaining)
        if param_match:
            type_name = param_match.group(1)
            var_name = param_match.group(2)
            params.append({"name": var_name, "type": type_name})
            parts.append(f"({type_name} {var_name})")
            remaining = remaining[param_match.end():]
            continue

        # array parameter (generic): [name$]
        array_param_match = re.match(r"\[(\w+\$?)\]", remaining)
        if array_param_match:
            var_name = array_param_match.group(1)
            params.append({"name": var_name, "type": None})
            parts.append(f"[{var_name}]")
            remaining = remaining[array_param_match.end():]
            continue

        # word or symbol token
        token_match = re.match(r"(\w+|\S+)", remaining)
        if token_match:
            parts.append(token_match.group(1))
            remaining = remaining[token_match.end():]
            continue

        break

    return params, parts


# --- expression parser ---

def _parse_expression(line: str, fn_signatures: list = None, task_signatures: list = None) -> dict:
    """Parse a zero expression line into an AST node."""
    # array variable declaration with task call: type name$ <- task call
    task_decl = re.match(r"(\w+)\s+(\w+)\$\s*(<-\s*.+)", line)
    if task_decl and _looks_like_type(task_decl.group(1)):
        type_name = task_decl.group(1)
        var_name = task_decl.group(2)
        rest = task_decl.group(3)
        arr_var = _parse_array_variable(type_name, var_name, rest, fn_signatures, task_signatures)
        return {"kind": "var_decl", **arr_var}

    # variable declaration: type name[$] = value
    var_decl = re.match(r"(\w+)\s+(\w+)(\$)?\s*=\s*(.*)", line)
    if var_decl and _looks_like_type(var_decl.group(1)):
        type_name = var_decl.group(1)
        var_name = var_decl.group(2)
        is_array = var_decl.group(3) is not None
        rhs = var_decl.group(4).strip()
        if is_array:
            arr_var = _parse_array_variable(type_name, var_name, "= " + rhs, fn_signatures, task_signatures)
            return {"kind": "var_decl", **arr_var}
        else:
            value = _parse_expr(rhs, fn_signatures)
            return {"kind": "var_decl", "name": var_name, "type": type_name, "array": False, "value": value}

    # assignment: target = value
    if "=" in line:
        # find the first '=' that isn't part of ==, !=, <=, >=
        idx = _find_assignment_eq(line)
        if idx is not None:
            target = line[:idx].strip()
            value_str = line[idx + 1:].strip()
            # check for reduce expression (contains _ as reduce marker)
            if _is_reduce_expr(value_str):
                return {"kind": "assign", "target": target, "value": _parse_reduce(value_str)}
            return {"kind": "assign", "target": target, "value": _parse_expr(value_str, fn_signatures)}

    return _parse_expr(line, fn_signatures)


def _looks_like_type(s: str) -> bool:
    """Return True if s looks like a type name (not a variable name)."""
    # known type patterns: int, uint, float, intN, uintN, floatN, number, or any word
    # that starts with a lowercase letter (zero types are always lowercase)
    # We check against common type prefixes
    if re.match(r"(int|uint|float)\d*$", s):
        return True
    if s in ("number", "string", "bool"):
        return True
    # any other word could be a user-defined type — but we can't distinguish
    # from a variable name without more context. Use a heuristic: if the next
    # token after it is also a word (not an operator), it's likely a type.
    return True  # conservative: treat as type, let downstream handle it


def _find_assignment_eq(line: str) -> int | None:
    """Find the index of the assignment '=' in a line, skipping ==, !=, <=, >=."""
    i = 0
    while i < len(line):
        if line[i] == "=" and (i == 0 or line[i - 1] not in "!<>=") and (i + 1 >= len(line) or line[i + 1] != "="):
            return i
        i += 1
    return None


def _parse_expr(s: str, fn_sigs: list = None) -> dict:
    """Parse a zero expression string into an AST node."""
    s = s.strip()
    fn_sigs = fn_sigs or []

    # ternary: (expr) if (cond) else (expr)
    ternary = _try_parse_ternary(s, fn_sigs)
    if ternary:
        return ternary

    # try binary operation at top level
    binop = _try_parse_binop(s, fn_sigs)
    if binop:
        return binop

    # parenthesized expression
    if s.startswith("(") and s.endswith(")") and _matching_paren(s, 0) == len(s) - 1:
        return _parse_expr(s[1:-1], fn_sigs)

    # function/constructor call: name(args)
    call_match = re.match(r"(\w+)\((.*)?\)$", s)
    if call_match:
        name = call_match.group(1)
        args_str = call_match.group(2) or ""
        args = _parse_call_args(args_str, fn_sigs) if args_str.strip() else []
        return {"kind": "call", "name": name, "args": args}

    # open-syntax function call
    fn_call = _try_parse_fn_call(s, fn_sigs)
    if fn_call:
        return fn_call

    # builtin array function: length of [expr]
    length_match = re.match(r"length of \[(.+)\]$", s)
    if length_match:
        inner = length_match.group(1).strip()
        return {"kind": "array_fn", "name": "length_of", "args": [_parse_expr(inner, fn_sigs)]}

    # array operations: name$[...] — slice, index, or open-ended slice
    bracket_match = re.match(r"(\w+\$?)\[(.+?)\]$", s)
    if bracket_match:
        array_expr = _parse_expr(bracket_match.group(1), fn_sigs)
        inner = bracket_match.group(2)

        # slice with to/through: name$[expr to expr] or name$[expr through expr]
        range_match = re.match(r"(.+?)\s+(to|through)\s+(.+?)$", inner)
        if range_match:
            start_expr = _parse_expr(range_match.group(1), fn_sigs)
            end_expr = _parse_expr(range_match.group(3), fn_sigs)
            inclusive = range_match.group(2) == "through"
            return {"kind": "slice", "array": array_expr, "start": start_expr, "end": end_expr, "inclusive": inclusive}

        # open-ended slice: name$[expr:] or name$[expr onwards]
        onwards_match = re.match(r"(.+?)\s+onwards$", inner)
        if inner.endswith(":") or onwards_match:
            if onwards_match:
                start_expr = _parse_expr(onwards_match.group(1).strip(), fn_sigs)
            else:
                start_expr = _parse_expr(inner[:-1].strip(), fn_sigs)
            return {"kind": "slice", "array": array_expr, "start": start_expr, "end": None, "inclusive": False}

        # open-start slice: name$[:expr]
        if inner.startswith(":"):
            end_expr = _parse_expr(inner[1:].strip(), fn_sigs)
            return {"kind": "slice", "array": array_expr, "start": None, "end": end_expr, "inclusive": False}

        # single index: name$[expr]
        index_expr = _parse_expr(inner, fn_sigs)
        return {"kind": "index", "array": array_expr, "index": index_expr}

    # numeric literal (must check before member access so 1.5 isn't parsed as member)
    try:
        return {"kind": "literal", "value": int(s)}
    except ValueError:
        pass
    try:
        return {"kind": "literal", "value": float(s)}
    except ValueError:
        pass

    # string literal
    if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        return {"kind": "literal", "value": s}

    # member access: obj.field
    if "." in s and re.match(r"(\w+)\.(\w+)$", s):
        m = re.match(r"(\w+)\.(\w+)$", s)
        return {"kind": "member", "object": m.group(1), "field": m.group(2)}

    # name (including array names with $ suffix)
    if re.match(r"\w+\$?$", s):
        return {"kind": "name", "value": s}

    return {"kind": "raw", "value": s}


def _try_parse_fn_call(s: str, fn_sigs: list) -> dict | None:
    """Try to match s against known function signatures.
    Returns an fn_call AST node or None."""
    for sig in fn_sigs:
        result = _try_match_signature(s, sig, fn_sigs)
        if result is not None:
            return result
    return None


def _try_match_signature(s: str, sig: dict, fn_sigs: list) -> dict | None:
    """Try to match string s against a single function signature pattern.
    Pattern is a list of ("word", w), ("param", type), or ("array_param", name) tokens.
    Returns fn_call or array_fn_call node or None."""
    pattern = sig["pattern"]
    pos = 0
    args = []

    for i, (kind, value) in enumerate(pattern):
        # skip leading whitespace
        while pos < len(s) and s[pos] == " ":
            pos += 1

        if pos >= len(s):
            return None

        if kind == "word":
            # expect the literal word at this position
            if not s[pos:].startswith(value):
                return None
            end = pos + len(value)
            # word must end at a boundary (space, paren, bracket, or end of string)
            if end < len(s) and s[end] not in " ([":
                return None
            pos = end

        elif kind == "param":
            # expect a parenthesized expression
            while pos < len(s) and s[pos] == " ":
                pos += 1
            if pos >= len(s) or s[pos] != "(":
                return None
            close = _matching_paren(s, pos)
            if close == -1:
                return None
            inner = s[pos + 1:close].strip()
            args.append(_parse_expr(inner, fn_sigs))
            pos = close + 1

        elif kind == "array_param":
            # expect a square-bracketed expression
            while pos < len(s) and s[pos] == " ":
                pos += 1
            if pos >= len(s) or s[pos] != "[":
                return None
            close = _find_matching_bracket(s, pos)
            if close == -1:
                return None
            inner = s[pos + 1:close].strip()
            args.append(_parse_expr(inner, fn_sigs))
            pos = close + 1

    # make sure we consumed the whole string
    remaining = s[pos:].strip()
    if remaining:
        return None

    node_kind = "array_fn_call" if sig.get("has_array_params") else "fn_call"
    return {
        "kind": node_kind,
        "signature_parts": sig["signature_parts"],
        "args": args,
    }


def _find_matching_bracket(s: str, start: int) -> int:
    """Find the matching ] for a [ at position start."""
    depth = 0
    for i in range(start, len(s)):
        if s[i] == "[":
            depth += 1
        elif s[i] == "]":
            depth -= 1
            if depth == 0:
                return i
    return -1


def _try_parse_ternary(s: str, fn_sigs: list = None) -> dict | None:
    """Try to parse '(x) if (cond) else (y)'."""
    # find ' if ' and ' else ' at the top level
    if_idx = _find_keyword(s, " if ")
    if if_idx is None:
        return None
    else_idx = _find_keyword(s, " else ", if_idx + 4)
    if else_idx is None:
        return None

    true_part = s[:if_idx].strip()
    cond_part = s[if_idx + 4:else_idx].strip()
    false_part = s[else_idx + 6:].strip()

    return {
        "kind": "ternary",
        "condition": _parse_expr(cond_part, fn_sigs),
        "true": _parse_expr(true_part, fn_sigs),
        "false": _parse_expr(false_part, fn_sigs),
    }


_BINOPS = [("<", "lt"), (">", "gt"), ("<=", "lte"), (">=", "gte"),
           ("==", "eq"), ("!=", "neq"), ("+", "plus"), ("-", "minus"),
           ("*", "times"), ("/", "div"), ("%", "mod")]


def _try_parse_binop(s: str, fn_sigs: list = None) -> dict | None:
    """Try to parse a binary operation at the top level (outside parens)."""
    # scan for operators outside parentheses, lowest precedence first
    # order: bitwise or, xor, and, comparison, shift, additive, multiplicative
    for ops in [["|"], ["^"], ["&"], ["<", ">", "<=", ">=", "==", "!="],
                ["<<", ">>"], ["+", "-"], ["*", "/", "%"]]:
        best = _find_top_level_op(s, ops)
        if best is not None:
            op, idx, op_len = best
            left = s[:idx].strip()
            right = s[idx + op_len:].strip()
            if left and right:
                return {
                    "kind": "binop",
                    "op": op,
                    "left": _parse_expr(left, fn_sigs),
                    "right": _parse_expr(right, fn_sigs),
                }
    return None


def _find_top_level_op(s: str, ops: list[str]) -> tuple[str, int, int] | None:
    """Find the rightmost top-level occurrence of any operator in ops.
    Returns (op, index, op_length) or None."""
    depth = 0
    best = None
    i = 0
    while i < len(s):
        if s[i] == "(":
            depth += 1
        elif s[i] == ")":
            depth -= 1
        elif depth == 0:
            for op in sorted(ops, key=len, reverse=True):
                if s[i:i + len(op)] == op:
                    # make sure it's surrounded by spaces or at boundaries
                    before_ok = (i == 0 or s[i - 1] == " ")
                    after_ok = (i + len(op) >= len(s) or s[i + len(op)] == " ")
                    if before_ok and after_ok:
                        best = (op, i, len(op))
        i += 1
    return best


def _find_keyword(s: str, keyword: str, start: int = 0) -> int | None:
    """Find keyword at top level (outside parentheses)."""
    depth = 0
    i = start
    while i < len(s) - len(keyword) + 1:
        if s[i] == "(":
            depth += 1
        elif s[i] == ")":
            depth -= 1
        elif depth == 0 and s[i:i + len(keyword)] == keyword:
            return i
        i += 1
    return None


def _matching_paren(s: str, start: int) -> int:
    """Find the index of the matching closing paren."""
    depth = 0
    for i in range(start, len(s)):
        if s[i] == "(":
            depth += 1
        elif s[i] == ")":
            depth -= 1
            if depth == 0:
                return i
    return -1


def _parse_call_args(args_str: str, fn_sigs: list = None) -> list[dict]:
    """Parse comma-separated call arguments into AST nodes."""
    args = []
    depth = 0
    current = ""
    for ch in args_str:
        if ch == "(":
            depth += 1
            current += ch
        elif ch == ")":
            depth -= 1
            current += ch
        elif ch == "," and depth == 0:
            args.append(_parse_expr(current.strip(), fn_sigs))
            current = ""
        else:
            current += ch
    if current.strip():
        args.append(_parse_expr(current.strip(), fn_sigs))
    return args
