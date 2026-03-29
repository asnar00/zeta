"""Feature composer: combines feature nubs into composed zero source.

Takes a list of feature definitions (with functions, extensions, type extensions)
and produces a single composed zero source string that can be fed to the parser.
"""


def compose(features: list[dict]) -> str:
    """Compose a list of features into a single zero source string.

    Each feature is a dict with:
        name: str
        extends: str | None
        functions: dict of name -> {signature, body}
        extensions: list of {kind, target_fn, target_step, insert}
        type_extensions: list of {target_type, fields}
        types: list of str (optional, raw type declaration lines)
    """
    # collect all functions across features
    all_functions = {}
    for feature in features:
        for fn_name, fn_def in feature.get("functions", {}).items():
            all_functions[fn_name] = dict(fn_def)

    # collect all extensions
    all_extensions = []
    for feature in features:
        all_extensions.extend(feature.get("extensions", []))

    # apply extensions to function bodies
    for ext in all_extensions:
        target_fn = ext["target_fn"]
        if target_fn not in all_functions:
            continue
        fn = all_functions[target_fn]
        fn["body"] = _apply_extension(fn["body"], ext)

    # collect and extend types
    type_lines = []
    type_map = {}  # name -> list of lines
    for feature in features:
        for type_block in feature.get("types", []):
            lines = type_block.split("\n")
            # extract type name from first line
            first = lines[0].strip()
            if first.startswith("type "):
                import re
                m = re.match(r"type\s+(\S+)", first)
                if m:
                    type_map[m.group(1)] = lines

    # apply type extensions
    for feature in features:
        for text in feature.get("type_extensions", []):
            target = text["target_type"]
            if target in type_map:
                # match indent of existing field lines
                existing = type_map[target]
                field_indent = "    "  # default
                for line in existing[1:]:  # skip 'type X =' line
                    if line.strip():
                        field_indent = line[:len(line) - len(line.lstrip())]
                        break
                type_map[target].append(field_indent + text["fields"])

    for name, lines in type_map.items():
        type_lines.extend(lines)

    # collect variables
    all_variables = []
    for feature in features:
        all_variables.extend(feature.get("variables", []))

    # assemble output
    output_parts = []

    if type_lines:
        output_parts.append("\n".join(type_lines))

    if all_variables:
        output_parts.append("\n".join(all_variables))

    for fn_name, fn_def in all_functions.items():
        output_parts.append(fn_def["signature"] + "\n" + "\n".join(fn_def["body"]))

    return "\n\n".join(output_parts) + "\n"


def _apply_extension(body: list[str], ext: dict) -> list[str]:
    """Apply a single extension to a function body.

    ext has: kind (before/after/on), target_step (str or None), insert (list of lines)
    If target_step is None, before/after applies to the whole body.
    """
    target_step = ext["target_step"]
    insert_lines = ext["insert"]
    kind = ext["kind"]

    # whole-body extension (no target step)
    if target_step is None:
        if kind == "before":
            return insert_lines + body
        elif kind == "after":
            return body + insert_lines
        elif kind == "replace":
            return insert_lines
        return body

    new_body = []
    for line in body:
        stripped = line.strip()
        # check if this line contains the target step call
        if _matches_step(stripped, target_step):
            if kind == "before":
                new_body.extend(insert_lines)
                new_body.append(line)
            elif kind == "after":
                new_body.append(line)
                new_body.extend(insert_lines)
            elif kind == "on":
                # wrap in concurrently block
                indent = _get_indent(line)
                new_body.append(f"{indent}concurrently")
                new_body.append(f"{indent}    {stripped}")
                new_body.append(f"{indent}and")
                for ins in insert_lines:
                    new_body.append(f"{indent}    {ins.strip()}")
            elif kind == "replace":
                new_body.extend(insert_lines)
        else:
            new_body.append(line)

    return new_body


def _matches_step(stripped: str, target_step: str) -> bool:
    """Check if a line matches a target step (function call)."""
    # strip trailing () for matching
    call = stripped.rstrip("()")
    target = target_step.rstrip("()")
    return call == target or stripped == target_step or stripped == target_step + "()"


def _get_indent(line: str) -> str:
    """Extract the leading whitespace from a line."""
    return line[:len(line) - len(line.lstrip())]
