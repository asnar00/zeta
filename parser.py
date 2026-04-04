"""Zero language processor: parses zero source into a language-agnostic IR."""

import re

IR_VERSION = 1

# Identifier pattern: letters, digits, underscores, with hyphens between letter-runs.
# Matches: landing-page, http-request, x, my-var, not-found
# Does NOT match: -foo, foo-, 3-bar
# Use W in regex patterns where \w+ previously matched identifiers.
ID = r'[a-zA-Z_]\w*(?:-[a-zA-Z_]\w*)*'
W = ID  # shorthand for use in regex string building



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


# Sections that contain zero code (indented blocks or fenced blocks are extracted)
_CODE_SECTIONS = {"interface", "definition", "tests"}


def _update_section(line):
    """Check if a line is a ## section header and return the section name, or None."""
    stripped = line.strip()
    if stripped.startswith("## "):
        return stripped[3:].strip().lower()
    return None


def _extract_code_line(line, in_fence, code_lines, line_map, orig_line, in_code_section):
    """Process a single line during markdown code extraction."""
    if line.strip().startswith("```"):
        if in_code_section:
            return not in_fence
        return in_fence
    if in_fence and in_code_section:
        code_lines.append(line)
        line_map.append(orig_line)
        return in_fence
    if not in_code_section:
        return in_fence
    if line.startswith("    ") or line.startswith("\t"):
        code_lines.append(line)
        line_map.append(orig_line)
    elif line.strip() == "":
        code_lines.append("")
        line_map.append(orig_line)
    return in_fence


def _extract_code(source: str) -> tuple[str, list[int]]:
    """Extract code from markdown source.
    Collects indented lines and fenced code blocks from code-bearing sections
    (## interface, ## definition, ## tests). Other sections are skipped.
    Returns (code_string, line_map) where line_map[i] is the 1-based source line
    number for code line i."""
    lines = source.split("\n")
    code_lines = []
    line_map = []
    in_fence = False
    in_code_section = True  # before any section header, treat as code
    for idx, line in enumerate(lines):
        section = _update_section(line)
        if section is not None:
            in_code_section = section in _CODE_SECTIONS
            continue
        in_fence = _extract_code_line(line, in_fence, code_lines, line_map, idx + 1, in_code_section)
    return "\n".join(code_lines), line_map


def _make_src_line_fn(line_map):
    """Create a source line mapping function."""
    def _src_line(code_idx: int) -> int:
        if line_map is not None and code_idx < len(line_map):
            return line_map[code_idx]
        return code_idx + 1
    return _src_line


def _init_ir(source_file=None):
    """Create a fresh IR dict."""
    ir = {"version": IR_VERSION, "features": set(), "types": [], "variables": [], "functions": [], "tasks": [], "uses": []}
    if source_file:
        ir["source_file"] = source_file
    return ir


def _parse_use_line(line, ir):
    """Parse a 'use' declaration line."""
    refs = [r.strip() for r in line[4:].split(",")]
    for ref in refs:
        parts = ref.split(".")
        if len(parts) == 2:
            ir["uses"].append({"platform": parts[0], "name": parts[1]})


def _parse_test_line(line, ir, fn_signatures, task_signatures, _src_line, i):
    """Parse a test example line: call_expr => expected_value."""
    idx = line.index(" => ")
    call_str = line[:idx].strip()
    expected_str = line[idx + 4:].strip()
    call_node = _try_parse_fn_call(call_str, fn_signatures)
    if call_node is None:
        call_node = _try_parse_task_call(call_str, task_signatures)
    if call_node is not None:
        expected_node = _parse_expr(expected_str, fn_signatures)
        ir.setdefault("tests", []).append({
            "call": call_node,
            "expected": expected_node,
            "source_line": _src_line(i),
            "source_text": line,
        })


def _parse_variable_or_statement(line, ir, fn_signatures, task_signatures):
    """Parse a non-empty, non-comment line as variable, call, or statement."""
    if "(" in line and "=" not in line:
        fn_call = _try_parse_fn_call(line, fn_signatures)
        if fn_call:
            ir.setdefault("statements", []).append(fn_call)
            return True
    var = _parse_variable(line, fn_signatures, task_signatures)
    if var:
        ir["variables"].append(var)
        return True
    stmt = _parse_expression(line, fn_signatures, task_signatures)
    if stmt and stmt.get("kind") != "raw":
        ir.setdefault("statements", []).append(stmt)
        return True
    return False


def _skip_to_next_declaration(lines, i):
    """Skip forward to next top-level declaration after an error."""
    i += 1
    while i < len(lines):
        next_line = lines[i].strip()
        if (next_line.startswith("type ") or next_line.startswith("on ")
                or _is_task_def(next_line) or next_line == ""):
            break
        i += 1
    return i


def _mark_platform_streams(ir):
    """Mark platform stream variables (uninitialized arrays matching use declarations)."""
    use_stream_names = {u["name"].replace("$", "") for u in ir["uses"]}
    has_abstract = (any(t.get("abstract") for t in ir.get("tasks", []))
                    or any(f.get("abstract") for f in ir.get("functions", [])))
    for var in ir["variables"]:
        if var.get("array") and var.get("value") is None and not var.get("scope"):
            if var["name"] in use_stream_names or has_abstract:
                var["_platform"] = True


def _looks_like_call(s: str) -> bool:
    """Check if a string looks like an unresolved function call."""
    if "(" not in s or ")" not in s:
        return False
    if "=" in s or s.startswith('"') or s.startswith("'"):
        return False
    return bool(re.match(rf"{W}\s*\(", s) or re.match(rf"{W}\s+{W}.*\(", s))


def _check_undefined_calls(ir):
    """Check for raw nodes that look like undefined function calls."""
    for fn in ir["functions"]:
        for stmt in fn.get("body", []):
            _check_raw_calls(stmt, ir)


def _check_raw_calls(node, ir):
    """Recursively check for raw nodes that look like undefined function calls."""
    if not isinstance(node, dict):
        return
    if node.get("kind") == "raw" and _looks_like_call(node["value"]):
        ir.setdefault("errors", []).append(
            ZeroParseError(f"function not defined: {node['value']}", 0, node["value"]))
    for key in ("value", "left", "right", "condition", "true", "false"):
        child = node.get(key)
        if isinstance(child, dict):
            _check_raw_calls(child, ir)
    for arg in node.get("args", []):
        if isinstance(arg, dict):
            _check_raw_calls(arg, ir)
    for branch in node.get("branches", []):
        if isinstance(branch, dict):
            for s in branch.get("body", []):
                _check_raw_calls(s, ir)


def _try_parse_task_line(line, lines, i, ir, fn_signatures, task_signatures, _src_line):
    """Try to parse a task or void task line. Returns new i or None."""
    if _is_task_def(line):
        platform_streams = {u["name"] for u in ir["uses"]}
        task, consumed = _parse_task(lines, i, _src_line, fn_signatures, task_signatures, platform_streams)
        ir["tasks"].append(task)
        return i + consumed
    if _is_void_task(line, lines, i, ir.get("uses", [])):
        platform_streams = {u["name"] for u in ir["uses"]}
        task, consumed = _parse_void_task(lines, i, _src_line, fn_signatures, task_signatures, platform_streams)
        ir["tasks"].append(task)
        return i + consumed
    return None


def _parse_top_level_line(line, lines, i, ir, fn_signatures, task_signatures, _src_line):
    """Dispatch a single top-level line to the appropriate parser. Returns new i."""
    if line.startswith("use "):
        _parse_use_line(line, ir)
        return i + 1
    if line.startswith("type "):
        typ, consumed = _parse_type(lines, i, _src_line)
        ir["types"].append(typ)
        return i + consumed
    task_result = _try_parse_task_line(line, lines, i, ir, fn_signatures, task_signatures, _src_line)
    if task_result is not None:
        return task_result
    if line.startswith("on "):
        fn, consumed = _parse_function(lines, i, fn_signatures, _src_line, task_signatures)
        ir["functions"].append(fn)
        return i + consumed
    if " => " in line:
        _parse_test_line(line, ir, fn_signatures, task_signatures, _src_line, i)
        return i + 1
    if line and not line.startswith("#"):
        if not _parse_variable_or_statement(line, ir, fn_signatures, task_signatures):
            ir.setdefault("warnings", []).append(f"line {_src_line(i)}: unrecognized: {line}")
        return i + 1
    return i + 1


def _process_lines(lines, ir, fn_signatures, task_signatures, _src_line, recover):
    """Process all lines, collecting errors if recover=True."""
    errors = []
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        try:
            i = _parse_top_level_line(line, lines, i, ir, fn_signatures, task_signatures, _src_line)
        except ZeroParseError as e:
            if not recover:
                raise
            errors.append(e)
            i = _skip_to_next_declaration(lines, i)
    return errors


def process(source: str, recover: bool = False, source_file: str = None) -> dict:
    """Parse zero source text and return an IR dict.
    If recover=True, collects errors and continues instead of raising on first error."""
    ir = _init_ir(source_file)
    line_map = None
    if _is_markdown(source):
        source, line_map = _extract_code(source)
    lines = source.split("\n")
    _src_line = _make_src_line_fn(line_map)
    fn_signatures = _collect_signatures(lines)
    task_signatures = _collect_task_signatures(lines)
    errors = _process_lines(lines, ir, fn_signatures, task_signatures, _src_line, recover)
    if errors:
        ir["errors"] = errors
    _mark_platform_streams(ir)
    _check_undefined_calls(ir)
    ir["features"] = _detect_features(ir)
    return ir


def parse_tests(test_pairs: list[dict], source: str) -> list[dict]:
    """Parse test call/expected pairs against known signatures in source.

    Each item in test_pairs should have 'call' and 'expected' strings.
    Returns list of dicts with parsed 'call' and 'expected' IR nodes.
    """
    if _is_markdown(source):
        source, _ = _extract_code(source)
    lines = source.split("\n")
    fn_sigs = _collect_signatures(lines)
    task_sigs = _collect_task_signatures(lines)
    results = []
    for pair in test_pairs:
        call_node = _try_parse_fn_call(pair["call"], fn_sigs)
        if call_node is None:
            call_node = _try_parse_task_call(pair["call"], task_sigs)
        if call_node is None:
            continue  # skip tests we can't parse
        expected_node = _parse_expr(pair["expected"], fn_sigs)
        results.append({
            "call": call_node,
            "expected": expected_node,
            "source_text": f"{pair['call']} => {pair['expected']}",
        })
    return results


def _detect_type_features(ir, features):
    """Detect features from type declarations."""
    for typ in ir["types"]:
        if typ["kind"] == "numeric":
            features.add("numeric_types")
        elif typ["kind"] == "enum":
            features.add("enums")
        elif typ["kind"] == "struct":
            features.add("structs")


def _detect_variable_features(ir, features):
    """Detect features from variable declarations."""
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
    for var in ir["variables"]:
        val = var.get("value")
        if isinstance(val, dict) and val.get("kind") == "task_call":
            features.add("tasks")


def _detect_function_features(ir, features):
    """Detect features from function declarations."""
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
    for fn in ir["functions"]:
        for stmt in fn.get("body", []):
            if isinstance(stmt, dict) and stmt.get("kind") == "if_block":
                features.add("conditionals")


def _detect_features(ir: dict) -> set[str]:
    """Detect which language features this IR uses."""
    features = set()
    _detect_type_features(ir, features)
    _detect_variable_features(ir, features)
    _detect_function_features(ir, features)
    if ir.get("tasks"):
        features.add("tasks")
    _detect_in_exprs(ir, features)
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
        if node.get("kind") in ("where", "first_where", "index_of_first_where", "indices_where"):
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


def _build_signature_pattern(sig_parts):
    """Build a pattern list from parsed signature parts."""
    pattern = []
    has_array_params = False
    for part in sig_parts:
        param_match = re.match(rf"\(({W})\s+{W}\$?\)", part)
        typed_array_match = re.match(rf"\[({W})\s+({W}\$?)\]", part)
        array_param_match = re.match(rf"\[({W}\$?)\]", part)
        if typed_array_match:
            pattern.append(("array_param", typed_array_match.group(1)))
            has_array_params = True
        elif array_param_match:
            pattern.append(("array_param", array_param_match.group(1)))
            has_array_params = True
        elif param_match:
            pattern.append(("param", param_match.group(1)))
        else:
            pattern.append(("word", part))
    return pattern, has_array_params


def _collect_signatures(lines: list[str]) -> list[dict]:
    """First pass: collect function signatures for resolving calls in bodies."""
    signatures = []
    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("on "):
            continue
        match = re.match(rf"on\s+\(({W})\s+({W}\$?)\)\s*=\s*(.*)", stripped)
        if match:
            rhs = match.group(3).strip()
        else:
            void_match = re.match(r"on\s+(.*)", stripped)
            if not void_match:
                continue
            rhs = void_match.group(1).strip()
        params, sig_parts = _parse_signature(rhs)
        pattern, has_array_params = _build_signature_pattern(sig_parts)
        signatures.append({
            "pattern": pattern,
            "has_array_params": has_array_params,
            "signature_parts": [
                f"[{p[1]}]" if p[0] == "array_param" else f"({p[1]})" if p[0] == "param" else p[1] for p in pattern
            ],
        })
    return signatures


def _parse_abstract_or_struct(abstract_match, lines, start, line_num, src_line):
    """Handle abstract base type or struct without '='."""
    has_fields = False
    if start + 1 < len(lines) and lines[start + 1] and lines[start + 1][0:1].isspace():
        next_stripped = lines[start + 1].strip()
        if next_stripped and not next_stripped.startswith("type ") and not next_stripped.startswith("on "):
            has_fields = True
    if has_fields:
        return _parse_struct_type(abstract_match.group(1), lines, start, line_num, src_line=src_line)
    return {"kind": "struct", "name": abstract_match.group(1), "fields": [], "parents": []}, 1


def _parse_type_rhs(name, rest):
    """Parse the RHS of 'type name = rest' for enum/union/numeric."""
    if rest.startswith("..."):
        return _parse_numeric_type(name, rest), 1
    if rest == "":
        return None  # signal to caller to parse struct
    parts = [p.strip() for p in rest.split("|")]
    if _looks_like_enum(parts):
        return {"kind": "enum", "name": name, "values": parts}, 1
    else:
        base = " | ".join(parts)
        return {"kind": "numeric", "name": name, "base": base, "size": None}, 1


def _raise_missing_eq(line, line_num):
    """Raise error for missing '=' in type declaration."""
    after_name = re.match(r"type\s+(\S+)\s*(.*)", line)
    col = len("type ") + len(after_name.group(1)) if after_name else len(line)
    raise ZeroParseError("expected '=' in type declaration", line_num, line, column=col)


def _parse_type(lines: list[str], start: int, src_line=None) -> tuple[dict, int]:
    """Parse a type declaration starting at the given line index."""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1
    if re.match(r"type\s*=", line):
        raise ZeroParseError("expected type name after 'type'", line_num, line, column=5)
    abstract_match = re.match(rf"type\s+({W})\s*$", line)
    if abstract_match:
        return _parse_abstract_or_struct(abstract_match, lines, start, line_num, src_line)
    comp_match = re.match(rf"type\s+({W})\s*=\s*(.+\+)\s*$", line)
    if comp_match:
        name = comp_match.group(1)
        rhs = comp_match.group(2).strip()
        parts = [p.strip() for p in rhs.split("+") if p.strip()]
        return _parse_struct_type(name, lines, start, line_num, parents=parts, src_line=src_line)
    match = re.match(r"type\s+(\S+)\s*=\s*(.*)", line)
    if not match:
        _raise_missing_eq(line, line_num)
    name = match.group(1)
    rest = match.group(2).strip()
    result = _parse_type_rhs(name, rest)
    if result is None:
        return _parse_struct_type(name, lines, start, line_num, src_line=src_line)
    return result


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
    size_match = re.search(r"\d+$", name)
    size = int(size_match.group()) if size_match else None
    if name.startswith("float"):
        base = "float"
    else:
        base = "int"
    return {"kind": "numeric", "name": name, "base": base, "size": size}


def _is_struct_field_line(field_line, stripped, lines, start):
    """Check if a line should stop struct field parsing."""
    if not field_line or not field_line[0:1].isspace() or field_line.strip() == "":
        return False
    if stripped.startswith("type ") or stripped.startswith("on "):
        return False
    if "$" in stripped or re.match(rf"\S+\s+{W}\[", stripped):
        return False
    type_indent = len(lines[start]) - len(lines[start].lstrip())
    field_indent = len(field_line) - len(field_line.lstrip())
    if field_indent <= type_indent:
        return False
    return True


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
        stripped = field_line.strip()
        if not _is_struct_field_line(field_line, stripped, lines, start):
            break
        fields.extend(_parse_struct_fields(stripped))
        i += 1
    if not fields and not parents:
        raise ZeroParseError("expected struct fields after type declaration", line_num, lines[start].strip(), column=len(lines[start].strip()))
    return {"kind": "struct", "name": name, "fields": fields, "parents": parents}, i - start


def _parse_struct_fields(line: str) -> list[dict]:
    """Parse a struct field line like 'number x, y, z = 0'."""
    match = re.match(r"(\S+)\s+(.+)", line)
    type_name = match.group(1)
    rest = match.group(2)
    default = 0
    if "=" in rest:
        names_part, default_part = rest.rsplit("=", 1)
        default = _parse_literal(default_part.strip())
    else:
        names_part = rest
    names = [n.strip() for n in names_part.split(",")]
    return [{"name": n, "type": type_name, "default": default} for n in names]


def _extract_scope_prefix(line):
    """Extract optional scope prefix (shared) from a line."""
    if line.startswith("shared "):
        return "shared", line[7:]
    return None, line


def _parse_variable(line: str, fn_sigs: list = None, task_sigs: list = None) -> dict | None:
    """Parse a variable declaration like 'int32 i = 10' or 'int i$[4] = 1'."""
    scope, stripped = _extract_scope_prefix(line)
    match = re.match(rf"(\S+)\s+({W})(\$)?\s*(.*)", stripped)
    if not match:
        return None
    type_name = match.group(1)
    var_name = match.group(2)
    is_array = match.group(3) is not None
    rest = match.group(4).strip()
    if is_array:
        result = _parse_array_variable(type_name, var_name, rest, fn_sigs, task_sigs)
    else:
        result = _parse_scalar_variable(type_name, var_name, rest, fn_sigs)
    if result and scope:
        result["scope"] = scope
    return result


def _parse_scalar_rhs(rhs, fn_sigs):
    """Parse the RHS of a scalar variable assignment."""
    indices = _try_parse_indices_where(rhs, fn_sigs)
    if indices:
        return indices
    if _try_parse_index_of_first_where(rhs, fn_sigs):
        return _try_parse_index_of_first_where(rhs, fn_sigs)
    if _try_parse_first_where(rhs, fn_sigs):
        return _try_parse_first_where(rhs, fn_sigs)
    if _is_reduce_expr(rhs):
        return _parse_reduce(rhs)
    if re.match(rf"({W})\(([^)]*)\)$", rhs):
        call_match = re.match(rf"({W})\(([^)]*)\)", rhs)
        return _parse_constructor_call(call_match.group(1), call_match.group(2))
    if fn_sigs and _try_parse_fn_call(rhs, fn_sigs):
        return _try_parse_fn_call(rhs, fn_sigs)
    parsed = _parse_expr(rhs, fn_sigs)
    if parsed.get("kind") == "literal":
        return parsed["value"]
    return parsed


def _parse_scalar_variable(type_name: str, var_name: str, rest: str, fn_sigs: list = None) -> dict:
    """Parse a scalar variable from its type, name, and remaining text."""
    value = None
    if rest.startswith("="):
        rhs = rest[1:].strip()
        value = _parse_scalar_rhs(rhs, fn_sigs)
    return {"name": var_name, "type": type_name, "array": False, "value": value}


def _try_parse_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse '[array$] where (condition)'."""
    match = re.match(rf"\[({W}\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "where", "array": array_expr, "condition": cond}
    return None


def _try_parse_first_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'first of [array$] where (condition)'."""
    match = re.match(rf"first of \[({W}\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "first_where", "array": array_expr, "condition": cond}
    return None


def _try_parse_index_of_first_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'index of first in [array$] where (condition)'."""
    match = re.match(rf"index of first in \[({W}\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "index_of_first_where", "array": array_expr, "condition": cond}
    return None


def _try_parse_indices_where(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'indices of [array$] where (condition)'."""
    match = re.match(rf"indices of \[({W}\$?)\]\s+where\s+\((.+)\)$", rhs)
    if match:
        array_expr = _parse_expr(match.group(1), fn_sigs)
        cond_str = match.group(2)
        cond = _parse_expr(cond_str, fn_sigs)
        return {"kind": "indices_where", "array": array_expr, "condition": cond}
    return None


def _try_parse_sort(rhs: str, fn_sigs: list = None) -> dict | None:
    """Try to parse 'sort [array$]' or 'sort [array$] by (key_expr)'."""
    match_by = re.match(rf"sort\s+\[({W}\$?)\]\s+by\s+\((.+)\)(\s+descending)?$", rhs)
    if match_by:
        array_expr = _parse_expr(match_by.group(1), fn_sigs)
        key_expr = _parse_expr(match_by.group(2), fn_sigs)
        descending = match_by.group(3) is not None
        return {"kind": "sort", "array": array_expr, "key": key_expr, "descending": descending}
    match_simple = re.match(rf"sort\s+\[({W}\$?)\]$", rhs)
    if match_simple:
        array_expr = _parse_expr(match_simple.group(1), fn_sigs)
        return {"kind": "sort", "array": array_expr, "key": None, "descending": False}
    return None


def _is_reduce_expr(s: str) -> bool:
    """Check if an expression is a reduce (contains _ as the reduce marker)."""
    return bool(re.search(r'(?<!\w)_(?!\w)|\(_\)', s))


def _parse_reduce(rhs: str) -> dict:
    """Parse a reduce expression like 'i$ + _' or 'smaller of (i$) and (_)'."""
    op_match = re.match(rf"({W}\$)\s*([+\-*/%&|^]|<<|>>)\s*_\s*$", rhs)
    if op_match:
        return {"kind": "reduce", "array": op_match.group(1), "op": op_match.group(2)}
    parts = re.findall(r"\([^)]*\)|\S+", rhs)
    array_ref = None
    for part in parts:
        inner_match = re.match(rf"\(({W}\$)\)", part)
        if inner_match:
            array_ref = inner_match.group(1)
            break
    return {"kind": "reduce", "array": array_ref, "fn_parts": parts}


def _parse_constructor_call(name: str, args_str: str) -> dict:
    """Parse a constructor call like 'vector(1, 2, 3)' or 'vector(z=2, x=1)'."""
    if not args_str.strip():
        return {"kind": "call", "name": name, "args": []}
    args = [a.strip() for a in args_str.split(",")]
    if "=" in args[0]:
        parsed = []
        for arg in args:
            k, v = arg.split("=", 1)
            parsed.append({"name": k.strip(), "value": _parse_literal(v.strip())})
        return {"kind": "call", "name": name, "args": parsed}
    else:
        return {"kind": "call", "name": name, "args": [_parse_literal(a) for a in args]}


def _parse_array_with_assignment(type_name, var_name, rhs, fn_sigs, task_sigs):
    """Parse the '= rhs' portion of an array variable."""
    where = _try_parse_where(rhs, fn_sigs)
    if where:
        return {"name": var_name, "type": type_name, "array": True, "size": None, "value": where}
    sort = _try_parse_sort(rhs, fn_sigs)
    if sort:
        return {"name": var_name, "type": type_name, "array": True, "size": None, "value": sort}
    return None


def _parse_array_rhs(rhs, fn_sigs):
    """Parse the RHS value of an array assignment (range, literal, expression, fn_call, literal)."""
    range_match = re.match(r"\[(\w+)\s+(through|to)\s+(\w+)\]", rhs)
    if range_match:
        start = _parse_literal(range_match.group(1))
        end = _parse_literal(range_match.group(3))
        return {"range": range_match.group(2), "start": start, "end": end}
    if rhs.startswith("["):
        inner = rhs[1:-1]
        return _parse_array_elements(inner, fn_sigs)
    if _is_expression(rhs):
        return _parse_expr(rhs, fn_sigs)
    if fn_sigs and _try_parse_fn_call(rhs, fn_sigs):
        return _try_parse_fn_call(rhs, fn_sigs)
    return _parse_literal(rhs)


def _try_parse_array_stream_or_task(type_name, var_name, rest, fn_sigs, task_sigs):
    """Try to parse an array variable with <- (stream or task call). Returns result or None."""
    if rest.startswith("<-") and task_sigs:
        rhs = rest[2:].strip()
        task_call = _try_parse_task_call(rhs, task_sigs)
        if task_call:
            return {"name": var_name, "type": type_name, "array": True, "size": None, "value": task_call}
    if rest.startswith("<-"):
        value = _parse_stream(rest, var_name, fn_sigs)
        return {"name": var_name, "type": type_name, "array": True, "size": None, "value": value}
    return None


_BUILTIN_TYPES = {"string", "int", "uint", "float", "number", "bool", "char"}


def _parse_array_variable(type_name: str, var_name: str, rest: str, fn_sigs: list = None, task_sigs: list = None) -> dict:
    """Parse an array variable from its type, name (without $), and remaining text."""
    size = None
    value = None
    if rest.startswith("="):
        rhs = rest[1:].strip()
        early = _parse_array_with_assignment(type_name, var_name, rhs, fn_sigs, task_sigs)
        if early:
            return early
    stream_result = _try_parse_array_stream_or_task(type_name, var_name, rest, fn_sigs, task_sigs)
    if stream_result:
        return stream_result
    key_type_match = re.match(rf"\[({W})\]\s*(.*)", rest)
    if key_type_match and key_type_match.group(1) in _BUILTIN_TYPES:
        return _parse_array_keyed_collection(type_name, var_name, key_type_match, fn_sigs)
    size_match = re.match(r"\[(\d+)\]\s*(.*)", rest)
    if size_match:
        size = int(size_match.group(1))
        rest = size_match.group(2).strip()
    if rest.startswith("="):
        value = _parse_array_rhs(rest[1:].strip(), fn_sigs)
    return {"name": var_name, "type": type_name, "array": True, "size": size, "value": value}


def _parse_array_keyed_collection(type_name, var_name, key_type_match, fn_sigs):
    """Parse a keyed collection (map) array variable."""
    key_type = key_type_match.group(1)
    rest = key_type_match.group(2).strip()
    default = None
    if rest.startswith("="):
        rhs = rest[1:].strip()
        default = _parse_expr(rhs, fn_sigs) if _is_expression(rhs) else _parse_literal(rhs)
    return {"name": var_name, "type": type_name, "array": True, "map": True,
            "key_type": key_type, "size": None, "value": None, "default": default}


def _parse_array_elements(inner: str, fn_sigs: list = None) -> list:
    """Parse comma-separated array elements, respecting parentheses."""
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
    try:
        return int(s)
    except ValueError:
        pass
    try:
        return float(s)
    except ValueError:
        pass
    return _parse_expr(s, fn_sigs)


def _parse_stream_part(part, var_name, fn_sigs):
    """Process a single stream part, returning (step_or_none, terminate_or_none)."""
    part = part.strip()
    while_match = re.match(r"(while|until)\s+\((.+)\)\s*$", part)
    if while_match:
        kind = while_match.group(1)
        cond_str = while_match.group(2).replace(var_name + "$", "__last__")
        return None, {"kind": kind, "condition": _parse_expr(cond_str, fn_sigs)}
    for kw in ("until", "while"):
        kw_match = re.match(r"(.+?)\s+" + kw + r"\s+\((.+)\)\s*$", part)
        if kw_match:
            part = kw_match.group(1).strip()
            cond_str = kw_match.group(2).replace(var_name + "$", "__last__")
            terminate = {"kind": kw, "condition": _parse_expr(cond_str, fn_sigs)}
            expr_str = part.replace(var_name + "$", "__last__")
            return _parse_expr(expr_str, fn_sigs), terminate
    if part:
        expr_str = part.replace(var_name + "$", "__last__")
        return _parse_expr(expr_str, fn_sigs), None
    return None, None


def _parse_stream(rest: str, var_name: str, fn_sigs: list = None) -> dict:
    """Parse a streaming expression: <- expr <- expr ... [while/until (cond)]"""
    parts = _split_stream_parts(rest)
    steps = []
    terminate = None
    for part in parts:
        step, term = _parse_stream_part(part, var_name, fn_sigs)
        if term:
            terminate = term
        if step:
            steps.append(step)
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
    if "$" in s:
        return True
    if any(op in s for op in [" + ", " - ", " * ", " / ", " % ", " & ", " | ", " ^ ", " << ", " >> "]):
        return True
    return False


def _parse_literal(s: str):
    """Parse a literal value string into a Python value."""
    if s == "true":
        return True
    if s == "false":
        return False
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

def _parse_fn_signature(line, line_num):
    """Parse the 'on' line to extract result, params, and signature_parts."""
    result_match = re.match(rf"on\s+\(({W})\s+({W}\$?)\)\s*=\s*(.*)", line)
    if result_match:
        result = {"name": result_match.group(2), "type": result_match.group(1)}
        rhs = result_match.group(3).strip()
        params, signature_parts = _parse_signature(rhs)
        return result, params, signature_parts
    void_match = re.match(r"on\s+(.*)", line)
    if not void_match or not void_match.group(1).strip():
        raise ZeroParseError("expected function signature after 'on'", line_num, line, column=3)
    rhs = void_match.group(1).strip()
    params, signature_parts = _parse_signature(rhs)
    return None, params, signature_parts


def _collect_indented_body(lines, start):
    """Collect indented body lines after a definition line. Returns (raw_body_lines, next_i)."""
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
    return raw_body_lines, i


def _parse_fn_body(grouped, fn_signatures, task_signatures, result, src_line, start):
    """Parse grouped body into AST and check SSA."""
    body = []
    assigned = set()
    for idx, item in enumerate(grouped):
        if isinstance(item, dict) and item.get("kind") in ("concurrently", "if_block"):
            body.append(item)
            continue
        stmt = _parse_expression(item, fn_signatures, task_signatures)
        if stmt["kind"] == "var_decl":
            assigned.add(stmt["name"])
        elif stmt["kind"] == "assign":
            target = stmt["target"]
            result_name = result["name"] if result else None
            if "[" not in target and target != result_name:
                if target in assigned:
                    body_line_num = src_line(start + 1 + idx) if src_line else start + 2 + idx
                    raise ZeroParseError(
                        f"variable '{target}' already assigned (SSA violation)",
                        body_line_num, item, column=0)
                assigned.add(target)
        body.append(stmt)
    return body


def _parse_function(lines: list[str], start: int, fn_signatures: list = None, src_line=None, task_signatures: list = None) -> tuple[dict, int]:
    """Parse a function definition starting with 'on'."""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1
    result, params, signature_parts = _parse_fn_signature(line, line_num)
    raw_body_lines, i = _collect_indented_body(lines, start)
    body_lines = [l.strip() for l in raw_body_lines]
    if not body_lines:
        return {
            "result": result, "params": params, "signature_parts": signature_parts,
            "body": [], "abstract": True, "source_line": line_num,
        }, i - start
    grouped = _group_blocks(body_lines, raw_body_lines, fn_signatures)
    body = _parse_fn_body(grouped, fn_signatures, task_signatures, result, src_line, start)
    return {
        "result": result, "params": params, "signature_parts": signature_parts,
        "body": body, "source_line": line_num,
    }, i - start


def _group_concurrently_block(body_lines, raw_body_lines, i):
    """Group a 'concurrently' block. Returns (node, new_i)."""
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
    return {"kind": "concurrently", "blocks": blocks}, i


def _group_blocks(body_lines: list[str], raw_body_lines: list[str], fn_sigs: list = None) -> list:
    """Group multi-line constructs (concurrently, if/else) using indentation."""
    result = []
    i = 0
    while i < len(body_lines):
        if body_lines[i] == "concurrently":
            node, i = _group_concurrently_block(body_lines, raw_body_lines, i)
            result.append(node)
        elif re.match(r"if\s+\(.+\)", body_lines[i]):
            if_node, consumed = _group_if_block(body_lines, raw_body_lines, i, fn_sigs)
            result.append(if_node)
            i += consumed
        else:
            result.append(body_lines[i])
            i += 1
    return result


def _match_if_branch(line, line_indent, if_indent, branches_count, body_lines, raw_body_lines, i, if_indent_val, fn_sigs):
    """Try to match and process an if/else-if/else branch. Returns (branch_or_none, body_consumed)."""
    if_match = re.match(r"if\s+\((.+)\)$", line)
    else_if_match = re.match(r"else if\s+\((.+)\)$", line)
    else_match = (line == "else")
    if if_match and line_indent == if_indent and branches_count == 0:
        cond = _parse_expr(if_match.group(1), fn_sigs)
        body, body_consumed = _collect_indented(body_lines, raw_body_lines, i, if_indent, fn_sigs)
        return {"condition": cond, "body": body}, 1 + body_consumed
    if else_if_match and line_indent == if_indent:
        cond = _parse_expr(else_if_match.group(1), fn_sigs)
        body, body_consumed = _collect_indented(body_lines, raw_body_lines, i, if_indent, fn_sigs)
        return {"condition": cond, "body": body}, 1 + body_consumed
    if else_match and line_indent == if_indent:
        body, body_consumed = _collect_indented(body_lines, raw_body_lines, i, if_indent, fn_sigs)
        return {"condition": None, "body": body}, 1 + body_consumed
    return None, 0


def _group_if_block(body_lines, raw_body_lines, start, fn_sigs=None):
    """Group an if/else if/else block into a single AST node."""
    if_indent = len(raw_body_lines[start]) - len(raw_body_lines[start].lstrip())
    branches = []
    i = start
    while i < len(body_lines):
        line = body_lines[i]
        line_indent = len(raw_body_lines[i]) - len(raw_body_lines[i].lstrip())
        branch, consumed = _match_if_branch(line, line_indent, if_indent, len(branches), body_lines, raw_body_lines, i, if_indent, fn_sigs)
        if branch is not None:
            branches.append(branch)
            i += consumed
        else:
            break
    return {"kind": "if_block", "branches": branches}, i - start


def _collect_indented(body_lines, raw_body_lines, start, parent_indent, fn_sigs=None):
    """Collect lines indented more than parent_indent after start.
    Groups nested if/else blocks before parsing."""
    inner_lines = []
    inner_raw = []
    i = start + 1
    while i < len(body_lines):
        line_indent = len(raw_body_lines[i]) - len(raw_body_lines[i].lstrip())
        if line_indent <= parent_indent:
            break
        inner_lines.append(body_lines[i])
        inner_raw.append(raw_body_lines[i])
        i += 1
    grouped = _group_blocks(inner_lines, inner_raw, fn_sigs)
    body = []
    for item in grouped:
        if isinstance(item, dict):
            body.append(item)
        else:
            body.append(_parse_expression(item, fn_sigs))
    return body, i - start - 1


# --- tasks ---

def _is_task_def(line: str) -> bool:
    """Check if a line is a task definition: on (type name$) <- ..."""
    return bool(re.match(rf"on\s+\({W}\s+{W}\$\)\s*<-", line))


def _scan_body_for_platform_streams(lines, start, platform_streams):
    """Scan body lines for emit to platform streams or task-like constructs."""
    fn_indent = len(lines[start]) - len(lines[start].lstrip())
    i = start + 1
    while i < len(lines):
        body_line = lines[i]
        if not body_line or body_line.strip() == "":
            break
        line_indent = len(body_line) - len(body_line.lstrip())
        if line_indent <= fn_indent:
            break
        stripped = body_line.strip()
        emit_match = re.match(rf"({W}\$)\s*<-", stripped)
        if emit_match and emit_match.group(1) in platform_streams:
            return True
        if stripped.startswith("for each "):
            return True
        consume_match = re.match(rf"{W}\s+{W}\$?\s*<-", stripped)
        if consume_match:
            return True
        i += 1
    return False


def _is_void_task(line: str, lines: list[str], start: int, uses: list[dict]) -> bool:
    """Check if a void function should be parsed as a void task."""
    if not line.startswith("on ") or _is_task_def(line):
        return False
    if re.match(rf"on\s+\({W}\s+{W}\$?\)\s*=", line):
        return False
    platform_streams = {u["name"] for u in uses}
    if not platform_streams:
        return False
    return _scan_body_for_platform_streams(lines, start, platform_streams)


def _classify_sig_parts(sig_parts):
    """Split signature parts into (input_streams, scalar_params, name_parts)."""
    input_streams = []
    scalar_params = []
    name_parts = []
    for part in sig_parts:
        param_match = re.match(rf"\(({W})\s+({W})(\$)?\)", part)
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
    return input_streams, scalar_params, name_parts


def _parse_void_task(lines: list[str], start: int, src_line=None, fn_signatures: list = None, task_signatures: list = None, platform_streams: set = None) -> tuple[dict, int]:
    """Parse a void task: on name (params) with body that emits to platform streams."""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1
    void_match = re.match(r"on\s+(.*)", line)
    rhs = void_match.group(1).strip()
    all_params, sig_parts = _parse_signature(rhs)
    input_streams, scalar_params, name_parts = _classify_sig_parts(sig_parts)
    raw_body_lines, i = _collect_indented_body(lines, start)
    body_lines = [l.strip() for l in raw_body_lines]
    parsed_body = _parse_task_body(body_lines, raw_body_lines, None, fn_signatures, task_signatures, platform_streams)
    return {
        "name_parts": name_parts, "output": None,
        "input_streams": input_streams, "params": scalar_params,
        "body": parsed_body, "source_line": line_num,
        "platform_streams": list(platform_streams or []),
    }, i - start


def _collect_task_signatures(lines: list[str]) -> list[dict]:
    """First pass: collect task signatures for resolving task calls."""
    signatures = []
    for line in lines:
        stripped = line.strip()
        match = re.match(rf"on\s+\({W}\s+{W}\$\)\s*<-\s*(.*)", stripped)
        if not match:
            continue
        rhs = match.group(1).strip()
        params, sig_parts = _parse_signature(rhs)
        pattern = []
        for part in sig_parts:
            param_match = re.match(rf"\(({W})\s+{W}(\$)?\)", part)
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


def _build_task_result(name_parts, output, input_streams, scalar_params, body, line_num, **extra):
    """Build a task result dict."""
    result = {"name_parts": name_parts, "output": output,
              "input_streams": input_streams, "params": scalar_params,
              "body": body, "source_line": line_num}
    result.update(extra)
    return result


def _parse_task(lines: list[str], start: int, src_line=None, fn_signatures: list = None, task_signatures: list = None, platform_streams: set = None) -> tuple[dict, int]:
    """Parse a task definition: on (type name$) <- signature"""
    line = lines[start].strip()
    line_num = src_line(start) if src_line else start + 1
    match = re.match(rf"on\s+\(({W})\s+({W})\$\)\s*<-\s*(.*)", line)
    if not match:
        raise ZeroParseError("expected task definition", line_num, line, column=3)
    output_type = match.group(1)
    output_name = match.group(2)
    rhs = match.group(3).strip()
    all_params, sig_parts = _parse_signature(rhs)
    input_streams, scalar_params, name_parts = _classify_sig_parts(sig_parts)
    raw_body_lines, i = _collect_indented_body(lines, start)
    body_lines = [l.strip() for l in raw_body_lines]
    output = {"name": output_name, "type": output_type}
    if not body_lines:
        return _build_task_result(name_parts, output, input_streams, scalar_params, [], line_num, abstract=True), i - start
    output_stream = output_name + "$"
    parsed_body = _parse_task_body(body_lines, raw_body_lines, output_stream, fn_signatures, task_signatures, platform_streams)
    return _build_task_result(name_parts, output, input_streams, scalar_params, parsed_body, line_num), i - start


def _parse_task_for_each(stripped, fn_sigs):
    """Try to parse a 'for each' task body line."""
    for_each_match = re.match(rf"for each \(({W})\) in \((.+)\)$", stripped)
    if for_each_match:
        iter_expr = _parse_expr(for_each_match.group(2).strip(), fn_sigs)
        return {"kind": "for_each", "name": for_each_match.group(1), "iter": iter_expr}
    return None


def _parse_task_consume(stripped, fn_sigs, task_sigs):
    """Try to parse a 'consume' task body line."""
    consume_match = re.match(rf"({W})\s+({W})\s*<-\s*(.*)", stripped)
    if not consume_match or re.match(rf"({W}\$)\s*<-", stripped):
        return None
    consume_type = consume_match.group(1)
    consume_name = consume_match.group(2)
    consume_rhs = consume_match.group(3).strip()
    if re.match(rf"{W}\$$", consume_rhs):
        return {"kind": "consume", "type": consume_type, "name": consume_name, "stream": consume_rhs}
    rhs_stripped = re.sub(r"\(\)\s*$", "", consume_rhs).strip()
    task_call = _try_parse_task_call(rhs_stripped, task_sigs) if task_sigs else None
    if not task_call:
        task_call = _try_parse_task_call(consume_rhs, task_sigs) if task_sigs else None
    fn_call = _try_parse_fn_call(consume_rhs, fn_sigs) if not task_call and fn_sigs else None
    if task_call or fn_call:
        return {"kind": "consume_call", "type": consume_type, "name": consume_name, "call": task_call or fn_call}
    return None


def _parse_task_emit(stripped, output_stream, platform_streams, fn_sigs):
    """Try to parse an 'emit' task body line."""
    emit_match = re.match(rf"({W}\$)\s*<-\s*(.*)", stripped)
    if not emit_match:
        return None
    stream_name = emit_match.group(1)
    expr_str = emit_match.group(2).strip()
    if stream_name == output_stream:
        return {"kind": "emit", "value": _parse_expr(expr_str, fn_sigs)}
    if platform_streams and stream_name in platform_streams:
        return {"kind": "emit_external", "stream": stream_name, "value": _parse_expr(expr_str, fn_sigs)}
    return None


def _parse_task_conditional(stripped, fn_sigs):
    """Try to parse an if/elif/else task body line."""
    if_match = re.match(r"if\s+\((.+)\)$", stripped)
    if if_match:
        return {"kind": "task_if", "condition": _parse_expr(if_match.group(1), fn_sigs)}
    elif_match = re.match(r"else if\s+\((.+)\)$", stripped)
    if elif_match:
        return {"kind": "task_elif", "condition": _parse_expr(elif_match.group(1), fn_sigs)}
    if stripped == "else":
        return {"kind": "task_else"}
    return None


def _parse_task_body_line(stripped, output_stream, platform_streams, fn_sigs, task_sigs):
    """Parse a single task body line into a node."""
    node = _parse_task_for_each(stripped, fn_sigs)
    if node:
        return node
    node = _parse_task_consume(stripped, fn_sigs, task_sigs)
    if node:
        return node
    node = _parse_task_emit(stripped, output_stream, platform_streams, fn_sigs)
    if node:
        return node
    node = _parse_task_conditional(stripped, fn_sigs)
    if node:
        return node
    return _parse_expression(stripped, fn_sigs, task_sigs)


def _parse_task_body(body_lines: list[str], raw_body_lines: list[str], output_stream: str, fn_sigs: list = None, task_sigs: list = None, platform_streams: set = None) -> list:
    """Parse task body lines into structured AST nodes, grouping if/elif/else blocks."""
    nodes_with_indent = []
    for line, raw_line in zip(body_lines, raw_body_lines):
        stripped = line.strip()
        indent = len(raw_line) - len(raw_line.lstrip())
        node = _parse_task_body_line(stripped, output_stream, platform_streams, fn_sigs, task_sigs)
        nodes_with_indent.append((indent, node))
    return _group_task_if_blocks(nodes_with_indent)


def _group_task_for_each(nodes_with_indent, i, indent, node):
    """Group a for_each node with its indented body."""
    body_with_indent = []
    i += 1
    while i < len(nodes_with_indent):
        ni, nn = nodes_with_indent[i]
        if ni > indent:
            body_with_indent.append((ni, nn))
            i += 1
        else:
            break
    return {
        "kind": "for_each", "name": node["name"],
        "iter": node["iter"],
        "body": _group_task_if_blocks(body_with_indent),
    }, i


def _collect_indented_nodes(nodes_with_indent, i, indent):
    """Collect nodes indented deeper than indent. Returns (body_with_indent, new_i)."""
    body = []
    while i < len(nodes_with_indent):
        ni, nn = nodes_with_indent[i]
        if ni > indent:
            body.append((ni, nn))
            i += 1
        else:
            break
    return body, i


def _group_task_if_branch(nodes_with_indent, i, indent):
    """Group an if/elif/else block from task nodes."""
    node = nodes_with_indent[i][1]
    branches = []
    cond = node["condition"]
    body_with_indent, i = _collect_indented_nodes(nodes_with_indent, i + 1, indent)
    while i < len(nodes_with_indent):
        ni, nn = nodes_with_indent[i]
        if ni == indent and nn.get("kind") == "task_elif":
            branches.append({"condition": cond, "body": _group_task_if_blocks(body_with_indent)})
            cond = nn["condition"]
            body_with_indent, i = _collect_indented_nodes(nodes_with_indent, i + 1, indent)
        elif ni == indent and nn.get("kind") == "task_else":
            branches.append({"condition": cond, "body": _group_task_if_blocks(body_with_indent)})
            cond = None
            body_with_indent, i = _collect_indented_nodes(nodes_with_indent, i + 1, indent)
        else:
            break
    branches.append({"condition": cond, "body": _group_task_if_blocks(body_with_indent)})
    return {"kind": "if_block", "branches": branches}, i


def _group_task_if_blocks(nodes_with_indent: list) -> list:
    """Group task_if/task_elif/task_else/for_each nodes into structured blocks."""
    result = []
    i = 0
    while i < len(nodes_with_indent):
        indent, node = nodes_with_indent[i]
        if node["kind"] == "for_each":
            grouped, i = _group_task_for_each(nodes_with_indent, i, indent, node)
            result.append(grouped)
        elif node["kind"] == "task_if":
            grouped, i = _group_task_if_branch(nodes_with_indent, i, indent)
            result.append(grouped)
        else:
            result.append(node)
            i += 1
    return result


def _try_parse_task_call(rhs: str, task_sigs: list) -> dict | None:
    """Try to match rhs against known task signatures."""
    for sig in task_sigs:
        result = _try_match_task_sig(rhs, sig)
        if result is not None:
            return result
    return None


def _match_task_pattern_token(s, pos, kind, value):
    """Match a single token in a task signature pattern. Returns (args_addition, new_pos) or None."""
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
        return [], end
    if kind in ("param", "stream_param"):
        if s[pos] != "(":
            return None
        close = _matching_paren(s, pos)
        if close == -1:
            return None
        inner = s[pos + 1:close].strip()
        return [inner], close + 1
    return None


def _try_match_task_sig(s: str, sig: dict) -> dict | None:
    """Try to match string s against a single task signature pattern."""
    pattern = sig["pattern"]
    pos = 0
    args = []
    for kind, value in pattern:
        result = _match_task_pattern_token(s, pos, kind, value)
        if result is None:
            return None
        new_args, pos = result
        args.extend(new_args)
    remaining = s[pos:].strip()
    if remaining:
        return None
    return {"kind": "task_call", "signature_parts": sig["signature_parts"], "args": args}


def _try_match_sig_param(remaining, params, parts):
    """Try to match a param or array param token. Returns new remaining or None."""
    param_match = re.match(rf"\(({W})\s+({W}\$?)\)", remaining)
    if param_match:
        type_name, var_name = param_match.group(1), param_match.group(2)
        params.append({"name": var_name, "type": type_name})
        parts.append(f"({type_name} {var_name})")
        return remaining[param_match.end():]
    typed_array_match = re.match(rf"\[({W})\s+({W}\$?)\]", remaining)
    if typed_array_match:
        type_name, var_name = typed_array_match.group(1), typed_array_match.group(2)
        params.append({"name": var_name, "type": type_name})
        parts.append(f"[{type_name} {var_name}]")
        return remaining[typed_array_match.end():]
    array_param_match = re.match(rf"\[({W}\$?)\]", remaining)
    if array_param_match:
        var_name = array_param_match.group(1)
        params.append({"name": var_name, "type": None})
        parts.append(f"[{var_name}]")
        return remaining[array_param_match.end():]
    return None


def _try_match_sig_token(remaining, params, parts):
    """Try to match one token from a signature RHS. Returns new remaining or None."""
    remaining = remaining.strip()
    if not remaining:
        return None
    result = _try_match_sig_param(remaining, params, parts)
    if result is not None:
        return result
    token_match = re.match(r"(\w+|\S+)", remaining)
    if token_match:
        parts.append(token_match.group(1))
        return remaining[token_match.end():]
    return None


def _parse_signature(rhs: str) -> tuple[list[dict], list[str]]:
    """Parse the RHS of a function prototype into params and signature parts."""
    params = []
    parts = []
    remaining = re.sub(r"\(\)\s*$", "", rhs).strip()
    while remaining:
        new_remaining = _try_match_sig_token(remaining, params, parts)
        if new_remaining is None:
            break
        remaining = new_remaining
    return params, parts


# --- expression parser ---

def _try_parse_task_var_decl(line, fn_signatures, task_signatures):
    """Try to parse an array variable declaration with task call."""
    task_decl = re.match(rf"({W})\s+({W})\$\s*(<-\s*.+)", line)
    if task_decl and _looks_like_type(task_decl.group(1)):
        type_name = task_decl.group(1)
        var_name = task_decl.group(2)
        rest = task_decl.group(3)
        arr_var = _parse_array_variable(type_name, var_name, rest, fn_signatures, task_signatures)
        return {"kind": "var_decl", **arr_var}
    return None


def _try_parse_map_decl(line, fn_signatures):
    """Try to parse a map declaration."""
    map_decl = re.match(rf"({W})\s+({W})\$\[({W})\]\s*(.*)", line)
    if map_decl and _looks_like_type(map_decl.group(1)):
        type_name = map_decl.group(1)
        var_name = map_decl.group(2)
        key_type = map_decl.group(3)
        rest = map_decl.group(4).strip()
        default = None
        if rest.startswith("="):
            rhs = rest[1:].strip()
            default = _parse_expr(rhs, fn_signatures) if _is_expression(rhs) else _parse_literal(rhs)
        return {"kind": "var_decl", "name": var_name, "type": type_name, "array": True,
                "map": True, "key_type": key_type, "size": None, "value": None, "default": default}
    return None


def _try_parse_scalar_decl(line, fn_signatures, task_signatures):
    """Try to parse a scalar/array variable declaration."""
    var_decl = re.match(rf"({W})\s+({W})(\$)?\s*=\s*(.*)", line)
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
    return None


def _try_parse_assignment(line, fn_signatures):
    """Try to parse an assignment expression."""
    if "=" not in line:
        return None
    idx = _find_assignment_eq(line)
    if idx is None:
        return None
    target = line[:idx].strip()
    value_str = line[idx + 1:].strip()
    if _is_reduce_expr(value_str) and "index of first" not in value_str and "where" not in value_str:
        return {"kind": "assign", "target": target, "value": _parse_reduce(value_str)}
    return {"kind": "assign", "target": target, "value": _parse_expr(value_str, fn_signatures)}


def _parse_raise(line: str) -> dict:
    """Parse 'raise name (args)' into a raise node."""
    rest = line[6:].strip()
    parts = re.findall(r"\([^)]*\)|\S+", rest)
    name_parts = []
    args = []
    for part in parts:
        if part.startswith("(") and part.endswith(")"):
            args.append(part[1:-1].strip())
        else:
            name_parts.append(part)
    return {"kind": "raise", "name": " ".join(name_parts), "args": args}


def _parse_expression(line: str, fn_signatures: list = None, task_signatures: list = None) -> dict:
    """Parse a zero expression line into an AST node."""
    if line.startswith("..."):
        return {"kind": "placeholder", "text": line[3:].strip()}
    if line.startswith("raise "):
        return _parse_raise(line)
    result = _try_parse_task_var_decl(line, fn_signatures, task_signatures)
    if result:
        return result
    result = _try_parse_map_decl(line, fn_signatures)
    if result:
        return result
    result = _try_parse_scalar_decl(line, fn_signatures, task_signatures)
    if result:
        return result
    result = _try_parse_assignment(line, fn_signatures)
    if result:
        return result
    return _parse_expr(line, fn_signatures)


def _looks_like_type(s: str) -> bool:
    """Return True if s looks like a type name (not a variable name).
    Built-in types are lowercase keywords; user-defined types start with uppercase."""
    if re.match(r"(int|uint|float)\d*$", s):
        return True
    if s in ("number", "string", "bool", "char"):
        return True
    if s[0:1].isupper():
        return True
    return False


def _find_assignment_eq(line: str) -> int | None:
    """Find the index of the assignment '=' in a line, skipping ==, !=, <=, >=."""
    i = 0
    while i < len(line):
        if line[i] == "=" and (i == 0 or line[i - 1] not in "!<>=") and (i + 1 >= len(line) or line[i + 1] != "="):
            return i
        i += 1
    return None


def _try_parse_slice(array_name, inner, fn_sigs):
    """Try to parse a slice expression from bracket contents."""
    array_expr = _parse_expr(array_name, fn_sigs)
    range_match = re.match(r"(.+?)\s+(to|through)\s+(.+?)$", inner)
    if range_match:
        start_expr = _parse_expr(range_match.group(1), fn_sigs)
        end_expr = _parse_expr(range_match.group(3), fn_sigs)
        inclusive = range_match.group(2) == "through"
        return {"kind": "slice", "array": array_expr, "start": start_expr, "end": end_expr, "inclusive": inclusive}
    onwards_match = re.match(r"(.+?)\s+onwards$", inner)
    if inner.endswith(":") or onwards_match:
        if onwards_match:
            start_expr = _parse_expr(onwards_match.group(1).strip(), fn_sigs)
        else:
            start_expr = _parse_expr(inner[:-1].strip(), fn_sigs)
        return {"kind": "slice", "array": array_expr, "start": start_expr, "end": None, "inclusive": False}
    if inner.startswith(":"):
        end_expr = _parse_expr(inner[1:].strip(), fn_sigs)
        return {"kind": "slice", "array": array_expr, "start": None, "end": end_expr, "inclusive": False}
    return None


def _parse_bracket_expr(array_name: str, inner: str, fn_sigs: list = None) -> dict:
    """Parse array bracket operations: slicing, indexing."""
    result = _try_parse_slice(array_name, inner, fn_sigs)
    if result:
        return result
    array_expr = _parse_expr(array_name, fn_sigs)
    index_expr = _parse_expr(inner, fn_sigs)
    return {"kind": "index", "array": array_expr, "index": index_expr}


def _try_parse_call_expr(s, fn_sigs):
    """Try to parse a function/constructor call: name(args)."""
    call_match = re.match(rf"({W})\((.*)?\)$", s)
    if call_match:
        name = call_match.group(1)
        args_str = call_match.group(2) or ""
        args = _parse_call_args(args_str, fn_sigs) if args_str.strip() else []
        return {"kind": "call", "name": name, "args": args}
    return None


def _try_parse_literal(s):
    """Try to parse a literal value (int, float, bool, string)."""
    try:
        return {"kind": "literal", "value": int(s)}
    except ValueError:
        pass
    try:
        return {"kind": "literal", "value": float(s)}
    except ValueError:
        pass
    if s == "true":
        return {"kind": "literal", "value": True}
    if s == "false":
        return {"kind": "literal", "value": False}
    if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        return {"kind": "literal", "value": s}
    return None


def _try_parse_leaf(s):
    """Try to parse a member access or name leaf node."""
    if "." in s and re.match(rf"({W})\.({W}\$?)$", s):
        m = re.match(rf"({W})\.({W}\$?)$", s)
        return {"kind": "member", "object": m.group(1), "field": m.group(2)}
    if re.match(rf"{W}\$?$", s):
        return {"kind": "name", "value": s}
    return None


def _try_parse_filter_expr(s, fn_sigs):
    """Try to parse filter expressions (first_where, index_of_first_where, indices_where)."""
    for parser in (_try_parse_first_where, _try_parse_index_of_first_where, _try_parse_indices_where):
        result = parser(s, fn_sigs)
        if result:
            return result
    return None


def _parse_expr(s: str, fn_sigs: list = None) -> dict:
    """Parse a zero expression string into an AST node."""
    s = s.strip()
    fn_sigs = fn_sigs or []
    ternary = _try_parse_ternary(s, fn_sigs)
    if ternary:
        return ternary
    filter_expr = _try_parse_filter_expr(s, fn_sigs)
    if filter_expr:
        return filter_expr
    bracket_match = re.match(rf"({W}\$?)\[(.+)\]$", s)
    if bracket_match:
        return _parse_bracket_expr(bracket_match.group(1), bracket_match.group(2), fn_sigs)
    fn_call = _try_parse_fn_call(s, fn_sigs)
    if fn_call:
        return fn_call
    binop = _try_parse_binop(s, fn_sigs)
    if binop:
        return binop
    if s.startswith("(") and s.endswith(")") and _matching_paren(s, 0) == len(s) - 1:
        return _parse_expr(s[1:-1], fn_sigs)
    return _parse_expr_leaf(s, fn_sigs)


def _parse_expr_leaf(s, fn_sigs):
    """Parse a leaf expression: call, length_of, literal, member, or name."""
    call = _try_parse_call_expr(s, fn_sigs)
    if call:
        return call
    length_match = re.match(r"length of \[(.+)\]$", s)
    if length_match:
        inner = length_match.group(1).strip()
        return {"kind": "array_fn", "name": "length_of", "args": [_parse_expr(inner, fn_sigs)]}
    bracket_match = re.match(rf"({W}\$?)\[(.+)\]$", s)
    if bracket_match:
        return _parse_bracket_expr(bracket_match.group(1), bracket_match.group(2), fn_sigs)
    lit = _try_parse_literal(s)
    if lit:
        return lit
    leaf = _try_parse_leaf(s)
    if leaf:
        return leaf
    return {"kind": "raw", "value": s}


def _looks_like_call(s: str) -> bool:
    """Check if a string looks like an unresolved function call."""
    if "(" not in s or ")" not in s:
        return False
    if "=" in s or s.startswith('"') or s.startswith("'"):
        return False
    return bool(re.match(rf"{W}\s*\(", s) or re.match(rf"{W}\s+{W}.*\(", s))


def _try_parse_fn_call(s: str, fn_sigs: list) -> dict | None:
    """Try to match s against known function signatures."""
    for sig in fn_sigs:
        result = _try_match_signature(s, sig, fn_sigs)
        if result is not None:
            return result
    return None


def _match_fn_word(s, pos, value):
    """Match a literal word in a function signature. Returns new pos or None."""
    if not s[pos:].startswith(value):
        return None
    end = pos + len(value)
    if end < len(s) and s[end] not in " ([":
        return None
    return end


def _match_fn_param(s, pos, fn_sigs):
    """Match a parenthesized parameter. Returns (arg, new_pos) or None."""
    while pos < len(s) and s[pos] == " ":
        pos += 1
    if pos >= len(s) or s[pos] != "(":
        return None
    close = _matching_paren(s, pos)
    if close == -1:
        return None
    inner = s[pos + 1:close].strip()
    return _parse_expr(inner, fn_sigs), close + 1


def _match_fn_array_param(s, pos, fn_sigs):
    """Match a square-bracketed array parameter. Returns (arg, new_pos) or None."""
    while pos < len(s) and s[pos] == " ":
        pos += 1
    if pos >= len(s) or s[pos] != "[":
        return None
    close = _find_matching_bracket(s, pos)
    if close == -1:
        return None
    inner = s[pos + 1:close].strip()
    return _parse_expr(inner, fn_sigs), close + 1


def _match_sig_pattern_token(s, pos, kind, value, fn_sigs, args):
    """Match one token in a function signature. Returns new pos or None."""
    while pos < len(s) and s[pos] == " ":
        pos += 1
    if pos >= len(s):
        return None
    if kind == "word":
        return _match_fn_word(s, pos, value)
    if kind == "param":
        result = _match_fn_param(s, pos, fn_sigs)
        if result is None:
            return None
        arg, pos = result
        args.append(arg)
        return pos
    if kind == "array_param":
        result = _match_fn_array_param(s, pos, fn_sigs)
        if result is None:
            return None
        arg, pos = result
        args.append(arg)
        return pos
    return None


def _try_match_signature(s: str, sig: dict, fn_sigs: list) -> dict | None:
    """Try to match string s against a single function signature pattern."""
    pattern = sig["pattern"]
    pos = 0
    args = []
    for kind, value in pattern:
        pos = _match_sig_pattern_token(s, pos, kind, value, fn_sigs, args)
        if pos is None:
            return None
    remaining = s[pos:].strip()
    if remaining and remaining != "()":
        return None
    node_kind = "array_fn_call" if sig.get("has_array_params") else "fn_call"
    return {"kind": node_kind, "signature_parts": sig["signature_parts"], "args": args}


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
    for ops in [["or"], ["and"], ["|"], ["^"], ["&"], ["<", ">", "<=", ">=", "==", "!="],
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
    """Find the rightmost top-level occurrence of any operator in ops."""
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
