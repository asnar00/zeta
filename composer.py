"""Feature composer: combines feature nubs into composed zero source.

Takes a list of feature definitions (with functions, extensions, type extensions)
and produces a single composed zero source string that can be fed to the parser.
"""


def _collect_all_functions(features: list[dict]) -> dict:
    """Collect all functions across features into a single dict."""
    all_functions = {}
    for feature in features:
        for fn_name, fn_def in feature.get("functions", {}).items():
            all_functions[fn_name] = dict(fn_def)
    return all_functions


def _collect_tagged_extensions(features: list[dict]) -> list[dict]:
    """Collect all extensions tagged with their source feature name."""
    all_extensions = []
    for feature in features:
        for ext in feature.get("extensions", []):
            ext_copy = dict(ext)
            ext_copy["_feature"] = feature["name"]
            all_extensions.append(ext_copy)
    return all_extensions


def _apply_all_extensions(all_functions: dict, all_extensions: list, dynamic_features: set):
    """Apply all extensions to their target function bodies."""
    for ext in all_extensions:
        target_fn = ext["target_fn"]
        if target_fn in all_functions:
            all_functions[target_fn]["body"] = _apply_extension(
                all_functions[target_fn]["body"], ext, dynamic_features)


def _collect_and_extend_types(features: list[dict]) -> list[str]:
    """Collect type declarations and apply type extensions."""
    type_map = {}
    for feature in features:
        for type_block in feature.get("types", []):
            lines = type_block.split("\n")
            first = lines[0].strip()
            if first.startswith("type "):
                import re
                m = re.match(r"type\s+(\S+)", first)
                if m:
                    type_map[m.group(1)] = lines
    for feature in features:
        for text in feature.get("type_extensions", []):
            target = text["target_type"]
            if target in type_map:
                field_indent = _find_field_indent(type_map[target])
                type_map[target].append(field_indent + text["fields"])
    type_lines = []
    for name, lines in type_map.items():
        type_lines.extend(lines)
    return type_lines


def _find_field_indent(type_lines: list[str]) -> str:
    """Find the indentation used for fields in a type declaration."""
    for line in type_lines[1:]:
        if line.strip():
            return line[:len(line) - len(line.lstrip())]
    return "    "


def _collect_all_variables(features: list[dict]) -> list[str]:
    """Collect all variable declarations across features."""
    all_variables = []
    for feature in features:
        all_variables.extend(feature.get("variables", []))
    return all_variables


def _collect_all_uses(features: list[dict]) -> list[str]:
    """Collect all use declarations across features."""
    all_uses = []
    for feature in features:
        for use in feature.get("uses", []):
            all_uses.append(f"use {use['platform']}.{use['name']}")
    return all_uses


def _assemble_output(all_uses, type_lines, all_variables, all_functions):
    """Assemble the final composed output string."""
    output_parts = []
    if all_uses:
        output_parts.append("\n".join(all_uses))
    if type_lines:
        output_parts.append("\n".join(type_lines))
    if all_variables:
        output_parts.append("\n".join(all_variables))
    for fn_name, fn_def in all_functions.items():
        output_parts.append(fn_def["signature"] + "\n" + "\n".join(fn_def["body"]))
    return "\n\n".join(output_parts) + "\n"


def compose(features: list[dict], dynamic_features: set = None) -> str:
    """Compose a list of features into a single zero source string."""
    dynamic_features = dynamic_features or set()
    all_functions = _collect_all_functions(features)
    all_extensions = _collect_tagged_extensions(features)
    _apply_all_extensions(all_functions, all_extensions, dynamic_features)
    type_lines = _collect_and_extend_types(features)
    all_variables = _collect_all_variables(features)
    all_uses = _collect_all_uses(features)
    return _assemble_output(all_uses, type_lines, all_variables, all_functions)


def _collect_feature_parts(features: list[dict]) -> tuple:
    """Collect types, variables, and uses per feature. Returns (types, vars, uses) dicts."""
    feature_types = {}
    feature_vars = {}
    feature_uses = {}
    for feature in features:
        name = feature["name"]
        feature_types[name] = [tb for tb in feature.get("types", [])]
        feature_vars[name] = feature.get("variables", [])
        uses = [f"use {u['platform']}.{u['name']}" for u in feature.get("uses", [])]
        feature_uses[name] = uses
    return feature_types, feature_vars, feature_uses


def _assemble_feature(name, fn_owner, all_functions, feature_types, feature_vars, feature_uses):
    """Assemble the output for a single feature."""
    parts = []
    if feature_uses.get(name):
        parts.append("\n".join(feature_uses[name]))
    if feature_types.get(name):
        parts.append("\n".join(feature_types[name]))
    if feature_vars.get(name):
        parts.append("\n".join(feature_vars[name]))
    for fn_name, fn_def in all_functions.items():
        if fn_owner[fn_name] == name:
            parts.append(fn_def["signature"] + "\n" + "\n".join(fn_def["body"]))
    return "\n\n".join(parts) + "\n" if parts else ""


def compose_per_feature(features: list[dict], dynamic_features: set = None) -> dict:
    """Compose features and return a dict of {feature_name: zero_source}."""
    dynamic_features = dynamic_features or set()
    fn_owner = {}
    all_functions = {}
    for feature in features:
        for fn_name, fn_def in feature.get("functions", {}).items():
            all_functions[fn_name] = dict(fn_def)
            fn_owner[fn_name] = feature["name"]
    all_extensions = _collect_tagged_extensions(features)
    _apply_all_extensions(all_functions, all_extensions, dynamic_features)
    feature_types, feature_vars, feature_uses = _collect_feature_parts(features)
    return {f["name"]: _assemble_feature(
        f["name"], fn_owner, all_functions, feature_types, feature_vars, feature_uses
    ) for f in features}


def _apply_whole_body_extension(body, insert_lines, kind):
    """Apply a before/after/replace extension to the whole function body."""
    if kind == "before":
        return insert_lines + body
    elif kind == "after":
        return body + insert_lines
    elif kind == "replace":
        return insert_lines
    return body


def _apply_targeted_extension(body, insert_lines, kind, target_step):
    """Apply an extension at a specific step within a function body."""
    new_body = []
    for line in body:
        stripped = line.strip()
        if not _matches_step(stripped, target_step):
            new_body.append(line)
            continue
        if kind == "before":
            new_body.extend(insert_lines)
            new_body.append(line)
        elif kind == "after":
            new_body.append(line)
            new_body.extend(insert_lines)
        elif kind == "on":
            indent = _get_indent(line)
            new_body.append(f"{indent}concurrently")
            new_body.append(f"{indent}    {stripped}")
            new_body.append(f"{indent}and")
            for ins in insert_lines:
                new_body.append(f"{indent}    {ins.strip()}")
        elif kind == "replace":
            new_body.extend(insert_lines)
    return new_body


def _apply_extension(body: list[str], ext: dict, dynamic_features: set = None) -> list[str]:
    """Apply a single extension to a function body."""
    dynamic_features = dynamic_features or set()
    insert_lines = ext["insert"]
    feature_name = ext.get("_feature", "")
    if feature_name in dynamic_features:
        insert_lines = _wrap_dynamic(insert_lines, feature_name)
    if ext["target_step"] is None:
        return _apply_whole_body_extension(body, insert_lines, ext["kind"])
    return _apply_targeted_extension(body, insert_lines, ext["kind"], ext["target_step"])


def _matches_step(stripped: str, target_step: str) -> bool:
    """Check if a line matches a target step (function call)."""
    call = stripped.rstrip("()")
    target = target_step.rstrip("()")
    return call == target or stripped == target_step or stripped == target_step + "()"


def _wrap_into_if_condition(insert_lines, feature_name):
    """Inject the enabled check into the first if-condition using 'and'."""
    wrapped = []
    for line in insert_lines:
        stripped = line.strip()
        if stripped.startswith("if ("):
            indent = line[:len(line) - len(stripped)]
            inner = stripped[4:-1]
            wrapped.append(f"{indent}if ({feature_name}.enabled and {inner})")
            wrapped.extend(insert_lines[len(wrapped):])
            return wrapped, True
        wrapped.append(line)
    return wrapped, False


def _wrap_whole_block_dynamic(insert_lines, feature_name):
    """Wrap all extension lines in a dynamic feature guard block."""
    base_indent = "        "
    for line in insert_lines:
        if line.strip():
            base_indent = line[:len(line) - len(line.lstrip())]
            break
    wrapped = [f"{base_indent}if ({feature_name}.enabled)"]
    for line in insert_lines:
        if line.strip():
            wrapped.append("    " + line)
        else:
            wrapped.append("")
    return wrapped


def _wrap_dynamic(insert_lines: list[str], feature_name: str) -> list[str]:
    """Wrap extension lines in a dynamic feature guard."""
    wrapped, injected = _wrap_into_if_condition(insert_lines, feature_name)
    if injected:
        return wrapped
    return _wrap_whole_block_dynamic(insert_lines, feature_name)


def _get_indent(line: str) -> str:
    """Extract the leading whitespace from a line."""
    return line[:len(line) - len(line.lstrip())]
