"""Feature parser: extracts feature definitions from zero source.

Parses feature declarations, function definitions, extension directives,
and type extensions into the dict format expected by the composer.
"""

import re

from parser import W


def _new_feature(name, extends):
    """Create a fresh feature dict."""
    return {
        "name": name,
        "extends": extends,
        "functions": {},
        "extensions": [],
        "type_extensions": [],
        "types": [],
        "variables": [],
        "tests": [],
        "uses": [],
    }


def _try_parse_feature_decl(stripped):
    """Try to match a feature declaration line. Returns (name, extends) or None."""
    m = re.match(r"feature\s+(\S+)(?:\s+extends\s+(\S+))?$", stripped)
    if m:
        return m.group(1), m.group(2)
    return None


def _parse_use_line(stripped, current):
    """Parse a use declaration and add to current feature."""
    use_match = re.match(r"use\s+(.*)", stripped)
    if not use_match:
        return False
    refs = [r.strip() for r in use_match.group(1).split(",")]
    for ref in refs:
        parts = ref.split(".")
        if len(parts) == 2:
            current["uses"].append({"platform": parts[0], "name": parts[1]})
    return True


def _try_parse_extension_directive(stripped):
    """Try to match 'in fn(), before/after/on/replace step()'. Returns match or None."""
    return re.match(
        r"in\s+(\w[\w\s]*?)\(\),\s*(before|after|on|replace)\s+(\w[\w\s]*?)\(\)$",
        stripped
    )


def _try_parse_handler_binding(stripped):
    """Try to match 'in fn (), on handler (type args)'. Returns (target_fn, handler_name, params) or None."""
    m = re.match(
        rf"in\s+({W}[\w\s]*?)\s*\(\),\s*on\s+({W}[\w\s]*?)(?:\s*\(([^)]*)\))?\s*$",
        stripped
    )
    if not m:
        return None
    target_fn = m.group(1).strip()
    handler_name = m.group(2).strip()
    params_str = m.group(3) or ""
    params = []
    if params_str.strip():
        for p in params_str.split(","):
            p = p.strip()
            parts = p.split()
            if len(parts) == 2:
                params.append({"type": parts[0], "name": parts[1]})
    return target_fn, handler_name, params


def _parse_extension(lines, i, stripped, current):
    """Parse an extension directive with its body. Returns lines consumed or None."""
    ext_match = _try_parse_extension_directive(stripped)
    if ext_match:
        target_fn = ext_match.group(1).strip()
        kind = ext_match.group(2)
        target_step = ext_match.group(3).strip()
        insert_lines, consumed = _collect_body(lines, i)
        current["extensions"].append({
            "kind": kind, "target_fn": target_fn,
            "target_step": target_step, "insert": insert_lines,
        })
        return 1 + consumed
    return None


def _parse_handler_binding(lines, i, stripped, current):
    """Parse 'in X (), on Y (args)' handler binding. Returns lines consumed or None."""
    result = _try_parse_handler_binding(stripped)
    if not result:
        return None
    target_fn, handler_name, params = result
    current.setdefault("handlers", []).append({
        "target_fn": target_fn,
        "handler_name": handler_name,
        "params": params,
    })
    return 1


def _parse_function_def(lines, i, stripped, current):
    """Parse a function definition (on ...). Returns lines consumed or None."""
    if not stripped.startswith("on "):
        return None
    fn_name = _extract_function_name(stripped)
    body_lines, consumed = _collect_body(lines, i)
    current["functions"][fn_name] = {"signature": stripped, "body": body_lines}
    return 1 + consumed


def _parse_simple_extension(lines, i, stripped, current):
    """Parse a simple before/after/replace extension. Returns lines consumed or None."""
    simple_ext = re.match(r"(before|after|replace)\s+(.*)", stripped)
    if not simple_ext:
        return None
    kind = simple_ext.group(1)
    target_fn = _extract_function_name("on " + simple_ext.group(2).strip())
    insert_lines, consumed = _collect_body(lines, i)
    current["extensions"].append({
        "kind": kind, "target_fn": target_fn,
        "target_step": None, "insert": insert_lines,
    })
    return 1 + consumed


def _parse_type_extension(lines, i, stripped, current):
    """Parse a type extension (type X += or extend X). Returns lines consumed or None."""
    type_ext_match = re.match(r"type\s+(\S+)\s*\+=$", stripped) or re.match(r"extend\s+(\S+)$", stripped)
    if not type_ext_match:
        return None
    target_type = type_ext_match.group(1)
    field_lines, consumed = _collect_body(lines, i)
    for fl in field_lines:
        current["type_extensions"].append({
            "target_type": target_type, "fields": fl.strip(),
        })
    return 1 + consumed


def _parse_type_decl(lines, i, stripped, current):
    """Parse a type declaration with optional indented fields. Returns new index or None."""
    if not stripped.startswith("type "):
        return None
    type_lines = [lines[i]]
    j = i + 1
    while j < len(lines):
        next_line = lines[j]
        if not next_line or not next_line[0:1].isspace() or next_line.strip() == "":
            break
        type_lines.append(next_line)
        j += 1
    current["types"].append("\n".join(type_lines))
    return j


def _parse_feature_line(lines, i, current):
    """Try to parse a feature body line. Returns new index, or None if unhandled."""
    stripped = lines[i].strip()
    if _parse_use_line(stripped, current):
        return i + 1
    consumed = _parse_extension(lines, i, stripped, current)
    if consumed is not None:
        return i + consumed
    consumed = _parse_handler_binding(lines, i, stripped, current)
    if consumed is not None:
        return i + consumed
    consumed = _parse_function_def(lines, i, stripped, current)
    if consumed is not None:
        return i + consumed
    consumed = _parse_simple_extension(lines, i, stripped, current)
    if consumed is not None:
        return i + consumed
    consumed = _parse_type_extension(lines, i, stripped, current)
    if consumed is not None:
        return i + consumed
    new_i = _parse_type_decl(lines, i, stripped, current)
    if new_i is not None:
        return new_i
    if stripped and not stripped.startswith("#"):
        current["variables"].append(stripped)
    return i + 1


def _try_parse_test_line(stripped):
    """Try to parse a test line (call => expected). Returns dict or None."""
    if " => " not in stripped:
        return None
    idx = stripped.index(" => ")
    return {"call": stripped[:idx].strip(), "expected": stripped[idx + 4:].strip()}


def _start_feature(feat, features, pending_tests):
    """Create a new feature and attach any pending tests."""
    current = _new_feature(*feat)
    if pending_tests:
        current["tests"].extend(pending_tests)
        pending_tests.clear()
    features.append(current)
    return current


def parse_features(source: str) -> list[dict]:
    """Parse zero source containing feature definitions."""
    lines = source.split("\n")
    features = []
    current = None
    pending_tests = []
    i = 0
    while i < len(lines):
        stripped = lines[i].strip()
        feat = _try_parse_feature_decl(stripped)
        if feat:
            current = _start_feature(feat, features, pending_tests)
            i += 1
        elif _try_parse_test_line(stripped):
            pending_tests.append(_try_parse_test_line(stripped))
            i += 1
        elif current is None:
            i += 1
        else:
            i = _parse_feature_line(lines, i, current)
    return features


def _collect_body(lines: list[str], start: int) -> tuple[list[str], int]:
    """Collect indented body lines after a declaration at start."""
    body = []
    i = start + 1
    while i < len(lines):
        line = lines[i]
        if not line or line.strip() == "":
            break
        if not line[0:1].isspace():
            break
        body.append(line)
        i += 1
    return body, i - start - 1


def _extract_task_fn_name(line):
    """Extract function name from a task definition: on (type name$) <- signature."""
    task_match = re.match(rf"on\s+\({W}\s+{W}\$\)\s*<-\s*(.*)", line)
    if not task_match:
        return None
    rhs = task_match.group(1).strip()
    parts = []
    for token in re.findall(r"\([\w\s\$]+\)|\S+", rhs):
        if token.startswith("("):
            break
        parts.append(token)
    return " ".join(parts) if parts else rhs.split("(")[0].strip()


def _extract_result_fn_name(line):
    """Extract function name from a value-returning function: on (type result) = ..."""
    match = re.match(rf"on\s+\({W}\s+{W}\$?\)\s*=\s*(.*)", line)
    if not match:
        return None
    rhs = match.group(1).strip()
    parts = []
    for token in re.findall(r"\[[\w\s\$]+\]|\([\w\s]+\)|\S+", rhs):
        if token.startswith("(") or token.startswith("["):
            break
        parts.append(token)
    return " ".join(parts) if parts else rhs.split("(")[0].strip()


def _extract_void_fn_name(line):
    """Extract function name from a void function: on name() or on name (params)."""
    void_match = re.match(r"on\s+(.*)", line)
    if not void_match:
        return None
    rhs = void_match.group(1).strip()
    rhs = re.sub(r"\(\)\s*$", "", rhs).strip()
    parts = []
    for token in re.findall(rf"\({W}\s+{W}\)|\S+", rhs):
        if token.startswith("("):
            break
        parts.append(token)
    return " ".join(parts) if parts else rhs


def _extract_function_name(line: str) -> str:
    """Extract the function name from an 'on' line."""
    result = _extract_task_fn_name(line)
    if result:
        return result
    result = _extract_result_fn_name(line)
    if result:
        return result
    result = _extract_void_fn_name(line)
    if result:
        return result
    return line
