"""Feature parser: extracts feature definitions from zero source.

Parses feature declarations, function definitions, extension directives,
and type extensions into the dict format expected by the composer.
"""

import re


def parse_features(source: str) -> list[dict]:
    """Parse zero source containing feature definitions.

    Returns a list of feature dicts, each with:
        name, extends, functions, extensions, type_extensions, types
    """
    lines = source.split("\n")
    features = []
    current = None
    i = 0

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        # feature declaration
        feat_match = re.match(r"feature\s+(\S+)(?:\s+extends\s+(\S+))?$", stripped)
        if feat_match:
            current = {
                "name": feat_match.group(1),
                "extends": feat_match.group(2),
                "functions": {},
                "extensions": [],
                "type_extensions": [],
                "types": [],
                "variables": [],
            }
            features.append(current)
            i += 1
            continue

        if current is None:
            i += 1
            continue

        # extension directive: in fn(), before/after/on/replace step()
        ext_match = re.match(
            r"in\s+(\w[\w\s]*?)\(\),\s*(before|after|on|replace)\s+(\w[\w\s]*?)\(\)$",
            stripped
        )
        if ext_match:
            target_fn = ext_match.group(1).strip()
            kind = ext_match.group(2)
            target_step = ext_match.group(3).strip()
            # collect indented body
            insert_lines, consumed = _collect_body(lines, i)
            current["extensions"].append({
                "kind": kind,
                "target_fn": target_fn,
                "target_step": target_step,
                "insert": insert_lines,
            })
            i += 1 + consumed
            continue

        # function definition: on (type result) = signature  OR  on signature()  OR  on (type name$) <- ...
        if stripped.startswith("on "):
            fn_name = _extract_function_name(stripped)
            body_lines, consumed = _collect_body(lines, i)
            current["functions"][fn_name] = {
                "signature": stripped,
                "body": body_lines,
            }
            i += 1 + consumed
            continue

        # simple extension: before/after/replace fn_signature
        simple_ext = re.match(r"(before|after|replace)\s+(.*)", stripped)
        if simple_ext:
            kind = simple_ext.group(1)
            target_fn = _extract_function_name("on " + simple_ext.group(2).strip())
            insert_lines, consumed = _collect_body(lines, i)
            current["extensions"].append({
                "kind": kind,
                "target_fn": target_fn,
                "target_step": None,
                "insert": insert_lines,
            })
            i += 1 + consumed
            continue

        # type extension: "type X +=" or "extend X"
        type_ext_match = re.match(r"type\s+(\S+)\s*\+=$", stripped) or re.match(r"extend\s+(\S+)$", stripped)
        if type_ext_match:
            target_type = type_ext_match.group(1)
            field_lines, consumed = _collect_body(lines, i)
            for fl in field_lines:
                current["type_extensions"].append({
                    "target_type": target_type,
                    "fields": fl.strip(),
                })
            i += 1 + consumed
            continue

        # type declaration
        if stripped.startswith("type "):
            type_lines = [line]
            j = i + 1
            while j < len(lines):
                next_line = lines[j]
                if not next_line or not next_line[0:1].isspace() or next_line.strip() == "":
                    break
                type_lines.append(next_line)
                j += 1
            current["types"].append("\n".join(type_lines))
            i = j
            continue

        # anything else: collect as variable/raw code
        if stripped and not stripped.startswith("#"):
            current["variables"].append(stripped)

        i += 1

    return features


def _collect_body(lines: list[str], start: int) -> tuple[list[str], int]:
    """Collect indented body lines after a declaration at start.
    Returns (body_lines, number_of_body_lines_consumed)."""
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


def _extract_function_name(line: str) -> str:
    """Extract the function name from an 'on' line.

    Handles both:
        on (type result) = name (params)  ->  name
        on name()                         ->  name
        on multi word name()              ->  multi word name
        on name (type param)              ->  name
    """
    # task: on (type name$) <- signature
    task_match = re.match(r"on\s+\(\w+\s+\w+\$\)\s*<-\s*(.*)", line)
    if task_match:
        rhs = task_match.group(1).strip()
        parts = []
        for token in re.findall(r"\([\w\s\$]+\)|\S+", rhs):
            if token.startswith("("):
                break
            parts.append(token)
        return " ".join(parts) if parts else rhs.split("(")[0].strip()

    # value-returning: on (type result) = ...
    match = re.match(r"on\s+\(\w+\s+\w+\)\s*=\s*(.*)", line)
    if match:
        rhs = match.group(1).strip()
        # extract words before first (
        parts = []
        for token in re.findall(r"\([\w\s]+\)|\S+", rhs):
            if token.startswith("("):
                break
            parts.append(token)
        return " ".join(parts) if parts else rhs.split("(")[0].strip()

    # void: on name() or on name (params)
    void_match = re.match(r"on\s+(.*)", line)
    if void_match:
        rhs = void_match.group(1).strip()
        # strip trailing ()
        rhs = re.sub(r"\(\)\s*$", "", rhs).strip()
        # extract words before first typed param (type name)
        parts = []
        for token in re.findall(r"\(\w+\s+\w+\)|\S+", rhs):
            if token.startswith("("):
                break
            parts.append(token)
        return " ".join(parts) if parts else rhs

    return line
