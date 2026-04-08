"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py [--verbose] input.md [input2.md ...] output.py
       python3 zeta.py [--verbose] features.md output_dir/

Run generated output with --test [names] to execute tests.
"""

import os
import sys
from parser import process
from parser import _is_markdown, _extract_code, _collect_signatures
import log

_EMITTERS = {
    ".py": "emit_python",
    ".ts": "emit_typescript",
}

_PLATFORM_EXT = {
    ".py": ".py",
    ".ts": ".ts",
}


def _print_usage():
    print("Usage: python3 zeta.py [--verbose] input.md [input2.md ...] output.ext")
    print("       python3 zeta.py [--verbose] features.md output_dir/")
    print("Supported extensions:", ", ".join(_EMITTERS))


def _reject_zeta_output(out_dir):
    """Exit if the output directory is the zeta source directory."""
    zeta_dir = os.path.dirname(os.path.abspath(__file__))
    if os.path.normpath(os.path.abspath(out_dir)) == os.path.normpath(zeta_dir):
        print(f"Error: refusing to write output to the zeta source directory ({zeta_dir})")
        print("Use a subdirectory or different path for output files.")
        sys.exit(1)


def _load_emitter(output_path):
    """Import the emitter module for the given output file extension."""
    ext = "." + output_path.rsplit(".", 1)[-1] if "." in output_path else ""
    if ext not in _EMITTERS:
        print(f"Unsupported output format: {ext}")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)
    module = __import__(_EMITTERS[ext])
    return ext, module.emit


def _read_sources(input_paths):
    """Read all input files and return their contents."""
    with log.section("read source"):
        sources = []
        for path in input_paths:
            src = open(path).read()
            log.log(f"{len(src)} chars from {path}")
            sources.append(src)
    return sources


def _compose_sources(input_paths, sources):
    """If multiple inputs, compose them; otherwise return the single source."""
    if len(input_paths) == 1:
        return sources[0], {}

    from feature_parser import parse_features
    from composer import compose

    with log.section("compose features"):
        code_parts = [_extract_code(s)[0] if _is_markdown(s) else s for s in sources]
        combined = "\n".join(code_parts)
        features = parse_features(combined)
        for f in features:
            log.log(f"feature: {f['name']}" + (f" extends {f['extends']}" if f['extends'] else ""))
        feature_tests = {f["name"]: f["tests"] for f in features if f.get("tests")}
        source = compose(features)
        log.log(f"{len(source)} chars composed")
    return source, feature_tests


def _platform_dir():
    return os.path.join(os.path.dirname(__file__), "platforms")


def _list_platform_files(ext):
    """List all platform files with the given extension, scanning subdirectories."""
    pdir = _platform_dir()
    result = []
    if not os.path.isdir(pdir):
        return result
    for entry in sorted(os.listdir(pdir)):
        subdir = os.path.join(pdir, entry)
        if not os.path.isdir(subdir) or entry.startswith("_"):
            continue
        for fname in sorted(os.listdir(subdir)):
            if fname.endswith(ext):
                result.append(os.path.join(subdir, fname))
    return result


def _prepend_platform_interfaces(source):
    """Load platform .zero.md signatures and prepend them to source."""
    files = _list_platform_files(".zero.md")
    if not files:
        return source
    with log.section("load platform interfaces"):
        for path in files:
            fname = os.path.basename(path)
            log.log(fname)
            with open(path) as pf:
                plat_src = pf.read()
                if _is_markdown(plat_src):
                    plat_src, _ = _extract_code(plat_src)
                source = plat_src + "\n" + source
    return source


def _log_ir_summary(ir):
    log.log(f"{len(ir['types'])} types, {len(ir['functions'])} functions, "
            f"{len(ir.get('tasks', []))} tasks, {len(ir['variables'])} variables")
    log.log(f"features: {', '.join(sorted(ir['features']))}")


def _parse_feature_tests_into_ir(ir, feature_tests, source):
    """Parse feature tests and merge them into the IR."""
    if not feature_tests:
        return
    from parser import parse_tests
    all_tests = []
    for feat_name, tests in feature_tests.items():
        parsed = parse_tests(tests, source)
        for t in parsed:
            t["feature"] = feat_name
        all_tests.extend(parsed)
        log.log(f"{feat_name}: {len(parsed)} tests")
    if all_tests:
        ir["tests"] = all_tests


def _log_ir_diagnostics(ir):
    if ir.get("tests"):
        log.log(f"{len(ir['tests'])} tests total")
    if ir.get("errors"):
        for e in ir["errors"]:
            log.log(f"error: {e.format()}")
    if ir.get("warnings"):
        for w in ir["warnings"]:
            log.log(f"warning: {w}")


def _parse_and_log(source, input_paths, feature_tests):
    """Parse source into IR, parse feature tests, log results."""
    with log.section("parse"):
        source_label = " + ".join(input_paths)
        ir = process(source, source_file=source_label)
        _log_ir_summary(ir)
        _parse_feature_tests_into_ir(ir, feature_tests, source)
        _log_ir_diagnostics(ir)
    return ir


def _collect_platform_code(ext):
    """Collect platform prepend/append code for the given extension."""
    platform_ext = _PLATFORM_EXT.get(ext, "")
    main_ext = ".main" + platform_ext
    prepend = []
    append = []
    with log.section("platform code"):
        for path in _list_platform_files(platform_ext):
            fname = os.path.basename(path)
            content = _strip_shared_runtime(open(path).read())
            if fname.endswith(main_ext):
                log.log(f"{fname} (append)")
                append.append(content)
            else:
                log.log(f"{fname} (prepend)")
                prepend.append(content)
    return prepend, append


def _write_runtime(ir, ext, output_path):
    """Write the test runtime file if tests exist. Returns has_tests."""
    has_tests = bool(ir.get("tests"))
    if not has_tests:
        return False
    runtime = _load_shared_runtime(ext)
    runtime_path = os.path.join(os.path.dirname(output_path) or ".", f"_runtime{ext}")
    with open(runtime_path, "w") as f:
        f.write(runtime)
    return True


def _prepend_runtime_import(output, ext):
    """Prepend the runtime import statement for test support."""
    if ext == ".py":
        return "from _runtime import register_tests, run_tests\n\n" + output
    elif ext == ".ts":
        return "import { register_tests, run_tests } from './_runtime.js';\n\n" + output
    return output


def _assemble_output(output, prepend, append, has_tests, ext):
    """Combine platform code, runtime imports, and harness into final output."""
    if prepend:
        output = "\n\n".join(prepend) + "\n\n" + output
    if has_tests:
        output = _prepend_runtime_import(output, ext)
    harness = "\n\n".join(append) if append else ""
    output += _main_entry_point(harness, has_tests, ext)
    return output


def _parse_args():
    """Parse command-line arguments into (args, flags)."""
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    flags = {a for a in sys.argv[1:] if a.startswith("--")}
    if "--verbose" in flags:
        log.enable()
    if len(args) < 2:
        _print_usage()
        sys.exit(1)
    return args, flags


def _write_and_compile(output, output_path, ext, input_paths):
    """Write output file, compile-check it, and print summary."""
    with open(output_path, "w") as f:
        f.write(output)
    _compile(ext, [output_path])
    print(f"{' + '.join(input_paths)} -> {output_path}")


def main():
    args, flags = _parse_args()

    if len(args) == 2 and args[0].endswith("features.md") and args[1].endswith("/"):
        _build_features(args[0], args[1], flags)
        return

    input_paths = args[:-1]
    output_path = args[-1]

    _reject_zeta_output(os.path.dirname(os.path.abspath(output_path)))
    ext, emit = _load_emitter(output_path)
    sources = _read_sources(input_paths)
    source, feature_tests = _compose_sources(input_paths, sources)
    source = _prepend_platform_interfaces(source)
    ir = _parse_and_log(source, input_paths, feature_tests)

    with log.section(f"emit {ext}"):
        output = emit(ir)
        log.log(f"{len(output)} chars, {output.count(chr(10))} lines")

    prepend, append = _collect_platform_code(ext)
    has_tests = _write_runtime(ir, ext, output_path)
    output = _assemble_output(output, prepend, append, has_tests, ext)
    _write_and_compile(output, output_path, ext, input_paths)




def _load_shared_runtime(ext):
    """Load the shared test runtime code from platform files.
    Extracts the section between @shared-runtime-start and @shared-runtime-end."""
    suffix = '.py' if ext == '.py' else '.ts'
    for path in _list_platform_files(suffix):
        if 'runtime' in os.path.basename(path):
            content = open(path).read()
            start = content.find('# @shared-runtime-start' if ext == '.py' else '// @shared-runtime-start')
            end = content.find('# @shared-runtime-end' if ext == '.py' else '// @shared-runtime-end')
            if start >= 0 and end >= 0:
                start = content.index("\n", start) + 1
                return content[start:end]
    return ''



def _main_entry_point(harness: str, has_tests: bool, ext: str) -> str:
    """Generate a combined main entry point with test infrastructure and platform harness."""
    if not harness and not has_tests:
        return ""
    if ext == ".py":
        return _main_entry_point_py(harness, has_tests)
    elif ext == ".ts":
        return _main_entry_point_ts(harness, has_tests)
    return ""


def _main_entry_point_py(harness: str, has_tests: bool) -> str:
    lines = ["\n\nimport sys"]
    lines.append("if __name__ == '__main__':")
    if has_tests:
        lines.append("    if '--test' in sys.argv:")
        lines.append("        _names = [a for a in sys.argv[1:] if a != '--test'] or None")
        lines.append("        sys.exit(1 if run_tests(_names) else 0)")
    if harness:
        harness_body = _extract_harness_body(harness)
        if harness_body:
            lines.append(harness_body)
    return "\n".join(lines) + "\n"


def _main_entry_point_ts(harness: str, has_tests: bool) -> str:
    lines = []
    if has_tests:
        lines.append("\n\nif (process.argv.includes('--test')) {")
        lines.append("    const _names = process.argv.slice(2).filter(a => a !== '--test');")
        lines.append("    process.exit(run_tests(_names.length ? _names : undefined) ? 1 : 0);")
        if harness:
            lines.append("} else {")
            harness_body = _extract_harness_body(harness)
            if harness_body:
                lines.append(harness_body)
            lines.append("}")
        else:
            lines.append("}")
    elif harness:
        lines.append("\n\n" + harness)
    return "\n".join(lines) + "\n"


def _extract_harness_body(harness: str) -> str:
    """Extract the body of a platform harness, stripping its wrapper."""
    lines = harness.strip().split("\n")
    body_lines = []
    in_body = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("import sys") or stripped.startswith("# "):
            continue
        if stripped.startswith("if __name__"):
            in_body = True
            continue
        if in_body:
            body_lines.append(line)
        elif stripped:
            body_lines.append(line)
    return "\n".join(body_lines) if body_lines else ""


def _compile(ext: str, files: list[str]):
    """Compile/validate output files for the given platform."""
    import subprocess

    if ext == ".py":
        for f in files:
            result = subprocess.run(
                [sys.executable, "-m", "py_compile", f],
                capture_output=True, text=True
            )
            if result.returncode != 0:
                print(f"compile error: {f}")
                print(result.stderr.strip())
                sys.exit(1)
    elif ext == ".ts":
        result = subprocess.run(
            ["npx", "tsc", "--strict", "--noEmit", "--types", "node"] + files,
            capture_output=True, text=True
        )
        if result.returncode != 0:
            print(f"compile error (tsc --strict):")
            print(result.stdout.strip() or result.stderr.strip())
            sys.exit(1)


def _find_feature_file(name: str, search_dir: str) -> str:
    """Find a feature's .zero.md file by name, searching common locations."""
    import glob
    if name.endswith(".zero.md") and os.path.exists(name):
        return name
    candidates = [
        f"{name}.zero.md",
        os.path.join(name, f"{name}.zero.md"),
        os.path.join(search_dir, f"{name}.zero.md"),
        os.path.join(search_dir, name, f"{name}.zero.md"),
    ]
    for pattern in [f"*/{name}.zero.md", f"**/{name}.zero.md"]:
        candidates.extend(glob.glob(pattern, recursive=True))
    for c in candidates:
        if os.path.exists(c):
            return c
    return None


def _parse_feature_entry(line, search_dir):
    """Parse a single line from features.md into a feature entry dict."""
    parts = line.split()
    name = parts[0]
    dynamic = "dynamic" in parts[1:]
    config = {}
    for p in parts[1:]:
        if "=" in p:
            k, v = p.split("=", 1)
            config[k] = v
    path = _find_feature_file(name, search_dir)
    if path is None:
        print(f"Error: cannot find feature '{name}' (searched from {search_dir})")
        sys.exit(1)
    clean_name = os.path.basename(path).replace(".zero.md", "")
    return {"name": clean_name, "path": path, "dynamic": dynamic, "config": config}


def _parse_feature_list(features_path: str) -> list[dict]:
    """Parse a features.md file into feature entries."""
    search_dir = os.path.dirname(features_path) or "."
    entries = []
    for line in open(features_path).read().strip().split("\n"):
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        entries.append(_parse_feature_entry(line, search_dir))
    return entries


def _read_feature_list(features_path):
    """Read the feature list and log it."""
    with log.section("read features"):
        entries = _parse_feature_list(features_path)
        log.log(f"{len(entries)} features")
        for e in entries:
            flags_list = []
            if e["dynamic"]:
                flags_list.append("dynamic")
            if e["config"]:
                flags_list.append(str(e["config"]))
            log.log(f"  {e['name']}" + (f" ({', '.join(flags_list)})" if flags_list else ""))
    return entries


def _read_feature_sources(feature_entries):
    """Read and extract code from each feature file."""
    with log.section("read source"):
        code_parts = []
        for entry in feature_entries:
            src = open(entry["path"]).read()
            log.log(f"{len(src)} chars from {entry['path']}")
            if _is_markdown(src):
                src, _ = _extract_code(src)
            for key, val in entry["config"].items():
                src += f"\n{key} = {val}\n"
            code_parts.append(src)
    return code_parts


def _compose_feature_set(code_parts, dynamic_set):
    """Parse and compose features from code parts."""
    from feature_parser import parse_features
    from composer import compose_per_feature
    with log.section("compose features"):
        combined = "\n".join(code_parts)
        features = parse_features(combined)
        per_feature = compose_per_feature(features, dynamic_set)
        for name, source in per_feature.items():
            log.log(f"{name}: {len(source)} chars")
    return features, per_feature


def _detect_platform_component(source):
    """Detect @client or @server annotation in a platform source file."""
    for line in source.split("\n"):
        stripped = line.strip()
        if stripped == "@client":
            return "client"
        if stripped == "@server":
            return "server"
    return "server"  # default: server


def _collect_platform_signatures(code, plat_name):
    """Extract function signature base names from platform code."""
    from emit_base import get_base_name
    sigs = _collect_signatures(code.split("\n"))
    return {get_base_name(s["signature_parts"]) for s in sigs}


def _load_platform_interface_code():
    """Load all platform .zero.md files and extract their code.
    Returns (plat_code, client_fn_bases) where client_fn_bases is a set of
    function base names that belong to @client platforms."""
    plat_code = ""
    client_fn_bases = set()
    for path in _list_platform_files(".zero.md"):
        with open(path) as pf:
            plat_src = pf.read()
            plat_name = os.path.basename(path).replace(".zero.md", "")
            component = _detect_platform_component(plat_src)
            if _is_markdown(plat_src):
                plat_src, _ = _extract_code(plat_src)
            if component == "client":
                client_fn_bases |= _collect_platform_signatures(plat_src, plat_name)
            plat_code = plat_src + "\n" + plat_code
    return plat_code, client_fn_bases


def _strip_shared_runtime(content):
    """Remove @shared-runtime-start/end section from platform code."""
    import re
    return re.sub(
        r'(?://|#) @shared-runtime-start\n.*?(?://|#) @shared-runtime-end\n?',
        '', content, flags=re.DOTALL
    )


def _load_platform_implementations():
    """Load platform implementation files, split into prepend and append groups.
    Strips @shared-runtime sections (those go in _runtime files instead)."""
    prepend = {ext: [] for ext in _PLATFORM_EXT}
    append = {ext: [] for ext in _PLATFORM_EXT}
    for ext, pext in _PLATFORM_EXT.items():
        main_ext = ".main" + pext
        for path in _list_platform_files(pext):
            fname = os.path.basename(path)
            content = _strip_shared_runtime(open(path).read())
            if fname.endswith(main_ext):
                append[ext].append(content)
            else:
                prepend[ext].append(content)
    return prepend, append


def _find_root_feature(features):
    """Find the feature that nothing extends (the root)."""
    for f in features:
        if f["extends"] is None:
            return f["name"]
    return None


def _build_fn_ownership(features):
    """Build a map from function name to owning feature name."""
    fn_owner = {}
    for f in features:
        for fn_name in f.get("functions", {}):
            fn_owner[fn_name] = f["name"]
    return fn_owner


def _parse_full_source(full_source):
    """Parse composed source and log summary."""
    with log.section("parse"):
        ir = process(full_source, source_file="composed")
        log.log(f"{len(ir['types'])} types, {len(ir['functions'])} functions, "
                f"{len(ir.get('tasks', []))} tasks, {len(ir['variables'])} variables")
    return ir


def _init_ir_by_feature(features):
    """Initialize empty IR buckets for each feature."""
    return {f["name"]: {"types": [], "variables": [], "functions": [], "tasks": []}
            for f in features}


def _find_fn_owner(base_name, fn_owner):
    """Match a function's base name against feature function names."""
    for feat_fn_name, feat_name in fn_owner.items():
        normalized_feat = "_".join(feat_fn_name.split())
        if (base_name == normalized_feat or
                base_name.startswith(normalized_feat + "_") or
                base_name.startswith(normalized_feat + "__")):
            return feat_name
    return None


def _map_functions_to_features(ir, fn_owner, ir_by_feature, root_name):
    """Assign each IR function to its owning feature."""
    from emit_base import get_base_name
    for fn in ir["functions"]:
        base = get_base_name(fn["signature_parts"])
        owner = _find_fn_owner(base, fn_owner)
        if owner is None:
            owner = root_name
            fn["_platform"] = True
        ir_by_feature[owner]["functions"].append(fn)


def _map_tasks_to_features(ir, fn_owner, ir_by_feature, root_name):
    """Assign each IR task to its owning feature."""
    for task in ir.get("tasks", []):
        task_name = " ".join(task["name_parts"])
        owner = None
        for feat_fn_name, feat_name in fn_owner.items():
            if task_name == feat_fn_name or task_name.startswith(feat_fn_name + " "):
                owner = feat_name
                break
        if owner is None:
            owner = root_name
        ir_by_feature[owner]["tasks"].append(task)


def _map_variables_to_features(ir, features, ir_by_feature, root_name):
    """Assign each IR variable to its owning feature. Returns var_owners map."""
    var_owners = {}
    for f in features:
        for v in f.get("variables", []):
            var_owners[v] = f["name"]
    ir_var_owners = {}
    for var in ir["variables"]:
        owner = root_name
        for var_line, feat_name in var_owners.items():
            if var["name"] in var_line:
                owner = feat_name
                break
        ir_by_feature[owner]["variables"].append(var)
        ir_var_owners[var["name"]] = owner
    return ir_var_owners


def _assign_tests_to_features(ir, features, ir_by_feature, root_name, full_source):
    """Parse and assign tests to their owning features."""
    from parser import parse_tests
    features_with_tests = []
    if ir.get("tests"):
        ir_by_feature[root_name].setdefault("tests", []).extend(ir["tests"])
        features_with_tests.append(root_name)
        log.log(f"platform: {len(ir['tests'])} tests")
        del ir["tests"]
    for f in features:
        if f.get("tests"):
            parsed = parse_tests(f["tests"], full_source)
            if parsed:
                ir_by_feature[f["name"]].setdefault("tests", []).extend(parsed)
                if f["name"] not in features_with_tests:
                    features_with_tests.append(f["name"])
            log.log(f"{f['name']}: {len(parsed)} tests")
    return features_with_tests


def _build_module_map(ir_by_feature):
    """Build map from emitted function name to feature module name."""
    from emit_base import make_function_name, make_task_fn_name
    module_map = {}
    for feat_name, feat_ir_data in ir_by_feature.items():
        safe_name = feat_name.replace("-", "_")
        for fn in feat_ir_data["functions"]:
            if fn.get("_platform"):
                continue
            emitted_name = make_function_name(fn["signature_parts"])
            module_map[emitted_name] = safe_name
        for task in feat_ir_data["tasks"]:
            emitted_name = make_task_fn_name(task)
            module_map[emitted_name] = safe_name
    return module_map


def _compute_dep_order(features):
    """Return the dependency order of non-root features."""
    return [f["name"] for f in features if f["extends"] is not None]


def _ensure_package_json(output_dir):
    """Write package.json for ESM support if it doesn't exist."""
    pkg_path = os.path.join(output_dir, "package.json")
    if not os.path.exists(pkg_path):
        with open(pkg_path, "w") as f:
            f.write('{ "type": "module" }\n')


def _write_runtime_files(features_with_tests, output_dir):
    """Write _runtime files for each target if any features have tests."""
    if not features_with_tests:
        return
    for ext in _PLATFORM_EXT:
        runtime = _load_shared_runtime(ext)
        runtime_path = os.path.join(output_dir, f"_runtime{ext}")
        with open(runtime_path, "w") as f:
            f.write(runtime)


def _find_feat_source_file(feat_name, input_paths):
    """Find the source file path for a feature by name."""
    for p in input_paths:
        if feat_name in p:
            return p
    return f"{feat_name}.zero.md"


def _build_feat_ir(feat_name, ctx, input_paths):
    """Build the IR dict for a single feature."""
    ir = ctx["ir"]
    ir_by_feature = ctx["ir_by_feature"]
    feat_ir = dict(ir)
    feat_ir["functions"] = ir_by_feature[feat_name]["functions"]
    feat_ir["tasks"] = ir_by_feature[feat_name]["tasks"]
    feat_ir["variables"] = ir_by_feature[feat_name]["variables"]
    feat_ir["source_file"] = _find_feat_source_file(feat_name, input_paths)
    feat_ir["module_map"] = ctx["module_map"]
    feat_ir["current_module"] = feat_name.replace("-", "_")
    feat_ir["_var_owners"] = ctx["ir_var_owners"]
    feat_ir["_all_user_vars"] = [v for v in ir["variables"]
                                  if v.get("scope") not in ("shared", "input") and not v.get("_platform")]
    feat_ir["_all_tasks"] = ir.get("tasks", [])
    if ir_by_feature[feat_name].get("tests"):
        feat_ir["tests"] = ir_by_feature[feat_name]["tests"]
        feat_ir["test_feature"] = feat_name
    return feat_ir


def _add_child_imports(code, feat_name, ext, ctx):
    """Add imports for non-root features (runtime, cross-module)."""
    ir_by_feature = ctx["ir_by_feature"]
    module_map = ctx["module_map"]
    imports = []
    if ir_by_feature[feat_name].get("tests"):
        if ext == ".py":
            imports.append("from _runtime import register_tests")
        elif ext == ".ts":
            imports.append("import { register_tests } from './_runtime.js';")
    for mod_name in set(module_map.values()):
        if mod_name != feat_name and f"{mod_name}." in code:
            if ext == ".py":
                imports.append(f"import {mod_name}")
            elif ext == ".ts":
                imports.append(f"import * as {mod_name} from './{mod_name}.js';")
    if imports:
        code = "\n".join(imports) + "\n\n" + code
    return code


def _add_ts_exports(code):
    """Add 'export' keyword to all top-level function declarations."""
    lines = code.split('\n')
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        if stripped.startswith(('function ', 'function* ', 'async function ', 'async function* ')):
            indent = line[:len(line) - len(stripped)]
            lines[i] = indent + 'export ' + stripped
    return '\n'.join(lines)


def _add_root_preamble(code, ext, ctx, platform_append):
    """Add imports, child module imports, and harness to root feature code."""
    imports = []
    has_tests = bool(ctx["features_with_tests"])
    if has_tests and ext == ".py":
        imports.append("from _runtime import register_tests, run_tests")
    elif has_tests and ext == ".ts":
        imports.append("import { register_tests, run_tests } from './_runtime.js';")
    for dep in ctx["dep_order"]:
        safe_dep = dep.replace("-", "_")
        if ext == ".py":
            imports.append(f"import {safe_dep}")
        elif ext == ".ts":
            imports.append(f"import './{safe_dep}.js';")
            imports.append(f"import * as {safe_dep} from './{safe_dep}.js';")
    if imports:
        code = "\n".join(imports) + "\n\n" + code
    harness = "\n\n".join(platform_append[ext]) if platform_append[ext] else ""
    code += _main_entry_point(harness, has_tests, ext)
    return code


def _finalize_feature_code(code, feat_name, ext, ctx, platform_append):
    """Add imports, exports, and harness to feature code."""
    root_name = ctx["root_name"]
    if feat_name != root_name:
        code = _add_child_imports(code, feat_name, ext, ctx)
    if ext == ".ts":
        code = _add_ts_exports(code)
    if feat_name == root_name:
        code = _add_root_preamble(code, ext, ctx, platform_append)
    return code


def _read_feature_summary(path):
    """Read the one-line summary from a .zero.md file (the *...* line)."""
    try:
        with open(path) as f:
            for line in f:
                line = line.strip()
                if line.startswith("*") and line.endswith("*") and len(line) > 2:
                    return line[1:-1]
    except OSError:
        pass
    return ""


def _build_feature_tree_data(features, input_paths):
    """Build a list of (name, summary, extends) tuples for the feature tree."""
    path_by_name = {}
    for p in input_paths:
        name = os.path.basename(p).replace(".zero.md", "")
        path_by_name[name] = p
    tree = []
    for f in features:
        summary = _read_feature_summary(path_by_name.get(f["name"], ""))
        tree.append((f["name"], summary, f["extends"]))
    return tree


def _emit_feature_tree_const(tree_data, ext):
    """Emit the _FEATURE_TREE constant for the root module."""
    if ext == ".py":
        entries = ", ".join(f'("{n}", "{s}", {repr(e)})' for n, s, e in tree_data)
        return f"\n_FEATURE_TREE = [{entries}]\n"
    elif ext == ".ts":
        entries = ", ".join(
            f'["{n}", "{s}", {f"{chr(34)}{e}{chr(34)}" if e else "null"}]'
            for n, s, e in tree_data
        )
        return f"\nconst _FEATURE_TREE: [string, string, string | null][] = [{entries}];\n"
    return ""


def _compute_build_fingerprint(per_feature, platform_prepend, feature_tree_data):
    """Compute a build fingerprint from the composed source and platform code."""
    import hashlib
    h = hashlib.sha256()
    # feature tree (order matters)
    for name, summary, extends in (feature_tree_data or []):
        h.update(f"{name}:{extends}\n".encode())
    # composed zero source per feature
    for feat_name in sorted(per_feature):
        h.update(per_feature[feat_name].encode())
    # platform implementation code
    for ext in sorted(platform_prepend):
        for code in platform_prepend[ext]:
            h.update(code.encode())
    # git commit if available
    git_commit = _get_git_commit()
    if git_commit:
        h.update(git_commit.encode())
    return h.hexdigest()[:16], git_commit or ""


def _get_git_commit():
    """Get the current git commit hash, or empty string if not in a repo."""
    import subprocess
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True, text=True, timeout=5
        )
        if result.returncode == 0:
            return result.stdout.strip()[:12]
    except Exception:
        pass
    return ""


def _emit_build_fingerprint(fingerprint, git_commit, feature_tree_data, ext):
    """Emit the _BUILD_FINGERPRINT constant for the root module."""
    tree_str = ",".join(f"{n}" for n, _, _ in (feature_tree_data or []))
    if ext == ".py":
        return (f'\n_BUILD_FINGERPRINT = {{"hash": "{fingerprint}", '
                f'"git": "{git_commit}", "features": "{tree_str}"}}\n')
    elif ext == ".ts":
        return (f'\nconst _BUILD_FINGERPRINT: {{hash: string, git: string, features: string}} = '
                f'{{"hash": "{fingerprint}", "git": "{git_commit}", "features": "{tree_str}"}};\n')
    return ""


def _emit_all_features(per_feature, ctx, input_paths,
                       platform_prepend, platform_append, output_dir,
                       feature_tree_data=None):
    """Emit per-feature output files for each target language."""
    fingerprint, git_commit = _compute_build_fingerprint(
        per_feature, platform_prepend, feature_tree_data
    )
    log.log(f"build fingerprint: {fingerprint} (git: {git_commit or 'none'})")
    for ext in _EMITTERS:
        emitter_module = __import__(_EMITTERS[ext])
        emit = emitter_module.emit
        pext = _PLATFORM_EXT[ext]
        with log.section(f"emit {ext}"):
            for feat_name in per_feature:
                feat_ir = _build_feat_ir(feat_name, ctx, input_paths)
                code = emit(feat_ir)
                if platform_prepend[ext]:
                    code = "\n\n".join(platform_prepend[ext]) + "\n\n" + code
                code = _finalize_feature_code(
                    code, feat_name, ext, ctx, platform_append
                )
                if feat_name == ctx["root_name"] and feature_tree_data:
                    code += _emit_feature_tree_const(feature_tree_data, ext)
                    code += _emit_build_fingerprint(
                        fingerprint, git_commit, feature_tree_data, ext
                    )
                safe_name = feat_name.replace("-", "_")
                out_path = os.path.join(output_dir, f"{safe_name}{pext}")
                with open(out_path, "w") as f:
                    f.write(code)
                log.log(f"{feat_name} -> {out_path}")


def _report_errors(ir):
    """Print any parse errors and placeholders collected during the build."""
    errors = ir.get("errors", [])
    if errors:
        print(f"\n{len(errors)} error(s):")
        for e in errors:
            print(f"  {e.format()}")
    placeholders = _collect_placeholders(ir)
    if placeholders:
        print(f"\n{len(placeholders)} placeholder(s):")
        for p in placeholders:
            print(f"  ... {p}")


def _collect_placeholders(ir):
    """Collect all placeholder texts from function bodies."""
    result = []
    for fn in ir.get("functions", []):
        for stmt in fn.get("body", []):
            _walk_placeholders(stmt, result)
    return result


def _walk_placeholders(node, result):
    """Recursively collect placeholder texts from an AST node."""
    if not isinstance(node, dict):
        return
    if node.get("kind") == "placeholder":
        result.append(node["text"])
    for branch in node.get("branches", []):
        if isinstance(branch, dict):
            for s in branch.get("body", []):
                _walk_placeholders(s, result)




def _build_client_ir(ctx):
    """Build an IR containing only client-side functions and tasks, with RPC targets set."""
    from emit_base import get_base_name
    ir = ctx["ir"]
    placement = ir.get("_placement", {})
    client_fns = [fn for fn in ir["functions"]
                  if not fn.get("abstract")
                  and placement.get(get_base_name(fn["signature_parts"])) == "client"]
    client_tasks = [dict(task, platform_streams=["_client"])
                    for task in ir.get("tasks", [])
                    if not task.get("abstract")
                    and placement.get(_task_base_name(task)) == "client"]
    # server functions called by client code become RPC targets
    rpc_targets = set()
    for fn in client_fns:
        _collect_server_calls(fn.get("body", []), placement, rpc_targets)
    for task in client_tasks:
        _collect_server_calls(task.get("body", []), placement, rpc_targets)
    client_ir = dict(ir)
    client_ir["functions"] = client_fns
    client_ir["variables"] = []
    client_ir["tasks"] = client_tasks
    client_ir["types"] = [t for t in ir["types"] if t["kind"] != "struct" or
                          _type_used_by(t["name"], client_fns)]
    client_ir["_rpc_targets"] = rpc_targets
    client_ir["_force_async_all"] = True  # all client functions are async
    # gui platform functions are also async in client context (DOM-based)
    from emit_base import make_function_name
    client_fn_bases = ir.get("_client_fn_bases", set())
    client_async = set()
    for fn in ir["functions"]:
        if fn.get("abstract"):
            base = get_base_name(fn["signature_parts"])
            if base in client_fn_bases:
                client_async.add(make_function_name(fn["signature_parts"]))
    client_ir["_client_async_fns"] = client_async
    client_ir["tests"] = []
    return client_ir


def _type_used_by(type_name, functions):
    """Check if a type is referenced by any of the given functions."""
    for fn in functions:
        for p in fn.get("params", []):
            if p.get("type") == type_name:
                return True
        if fn.get("result") and fn["result"].get("type") == type_name:
            return True
    return False


def _collect_server_calls(body, placement, rpc_targets):
    """Collect function names of server functions called from client code."""
    for stmt in body:
        if not isinstance(stmt, dict):
            continue
        _walk_for_server_calls(stmt, placement, rpc_targets)


def _walk_for_server_calls(node, placement, rpc_targets):
    """Recursively walk an AST node collecting server fn_call targets."""
    from emit_base import get_base_name, make_function_name
    if not isinstance(node, dict):
        return
    if node.get("kind") == "fn_call":
        base = get_base_name(node["signature_parts"])
        if placement.get(base) == "server":
            rpc_targets.add(make_function_name(node["signature_parts"]))
    for key in ("value", "left", "right", "condition", "true", "false"):
        child = node.get(key)
        if isinstance(child, dict):
            _walk_for_server_calls(child, placement, rpc_targets)
    for arg in node.get("args", []):
        if isinstance(arg, dict):
            _walk_for_server_calls(arg, placement, rpc_targets)
    for step in node.get("steps", []):
        if isinstance(step, dict):
            _walk_for_server_calls(step, placement, rpc_targets)
    for branch in node.get("branches", []):
        if isinstance(branch, dict):
            for s in branch.get("body", []):
                _walk_for_server_calls(s, placement, rpc_targets)




def _emit_client_bundle(ctx, output_dir):
    """Emit a client.js bundle containing client-side functions with RPC stubs."""
    import emit_typescript
    client_ir = _build_client_ir(ctx)
    if not client_ir["functions"]:
        return
    # suppress the emitter's _ZeroRaise (we provide our own JS version)
    client_ir["_suppress_zero_raise"] = True
    code = emit_typescript.emit(client_ir)
    # strip TypeScript type annotations for plain JS
    code = _strip_ts_types(code)
    # build the bundle from platform .client.js files + compiled zero code
    parts = []
    parts.append("// client.js — generated by zeta")
    parts.append("console.log('[zero] client.js loaded');")
    for path in _list_platform_files(".client.js"):
        parts.append(open(path).read())
    parts.append(code)
    bundle = "\n".join(parts)
    # write with content hash for cache busting
    import hashlib
    content_hash = hashlib.md5(bundle.encode()).hexdigest()[:8]
    filename = f"client.{content_hash}.js"
    # remove old client.*.js files
    import glob
    for old in glob.glob(os.path.join(output_dir, "client.*.js")):
        os.remove(old)
    out_path = os.path.join(output_dir, filename)
    with open(out_path, "w") as f:
        f.write(bundle)
    # update the script tag in index.html to reference the hashed filename
    _update_script_tag(filename)
    log.log(f"client bundle -> {out_path}")



def _update_script_tag(filename):
    """Update index.html to reference the hashed client JS filename."""
    import re
    html_path = os.path.join("website", "index.html")
    if not os.path.exists(html_path):
        return
    html = open(html_path).read()
    new_html = re.sub(
        r'<script src="/@client/client[^"]*\.js"></script>',
        f'<script src="/@client/{filename}"></script>',
        html
    )
    if new_html != html:
        with open(html_path, "w") as f:
            f.write(new_html)


def _strip_ts_types(code):
    """Strip TypeScript type annotations to produce valid JavaScript."""
    import re
    # remove : type annotations from function params and return types
    # (type): -> ):
    code = re.sub(r':\s*(?:readonly\s+)?[\w\[\]|<>,\s!]+(?=\s*[,)\{=])', '', code)
    # remove ': type' after let/const declarations
    code = re.sub(r'((?:let|const)\s+\w+)\s*:\s*[\w\[\]|<>,\s!]+\s*=', r'\1 =', code)
    # remove 'as any' casts
    code = re.sub(r'\s+as\s+\w+', '', code)
    # remove export keyword
    code = re.sub(r'^export\s+', '', code, flags=re.MULTILINE)
    # remove interface/type declarations (standalone lines)
    code = re.sub(r'^(?:interface|type)\s+.*\{[^}]*\}\s*$', '', code, flags=re.MULTILINE | re.DOTALL)
    return code


def _compile_all_outputs(output_dir):
    """Compile/validate all output files in the output directory."""
    for ext, pext in _PLATFORM_EXT.items():
        files = [os.path.join(output_dir, f)
                 for f in os.listdir(output_dir) if f.endswith(pext)]
        if files:
            _compile(ext, files)


def _map_ir_to_features(ir, features, fn_owner, root_name, full_source):
    """Map IR elements to features and return the per-feature IR data."""
    ir_by_feature = _init_ir_by_feature(features)
    _map_functions_to_features(ir, fn_owner, ir_by_feature, root_name)
    _map_tasks_to_features(ir, fn_owner, ir_by_feature, root_name)
    ir_var_owners = _map_variables_to_features(ir, features, ir_by_feature, root_name)
    ir["_var_owners"] = ir_var_owners
    features_with_tests = _assign_tests_to_features(
        ir, features, ir_by_feature, root_name, full_source
    )
    return ir_by_feature, ir_var_owners, features_with_tests


def _collect_handlers(features):
    """Collect all handler bindings from features."""
    handlers = []
    for f in features:
        for h in f.get("handlers", []):
            handlers.append(h)
    return handlers


def _task_base_name(task):
    """Get the base name for a task from its name_parts."""
    return "_".join(p.replace("-", "_") for p in task["name_parts"])


def _classify_function_placement(ir, client_fn_bases):
    """Classify each function/task as 'client' or 'server' based on platform calls.
    A function is client-side if it directly calls any @client platform function.
    Propagation: if a non-platform function calls a client function, it's also client."""
    from emit_base import get_base_name

    # first pass: direct client callers (functions)
    placement = {}
    for fn in ir["functions"]:
        if fn.get("abstract"):
            continue
        base = get_base_name(fn["signature_parts"])
        if _body_calls_any(fn.get("body", []), client_fn_bases):
            placement[base] = "client"
        else:
            placement[base] = "server"

    # first pass: direct client callers (tasks)
    for task in ir.get("tasks", []):
        if task.get("abstract"):
            continue
        base = _task_base_name(task)
        if _body_calls_any(task.get("body", []), client_fn_bases):
            placement[base] = "client"
        else:
            placement[base] = "server"

    # propagate: if a server function/task calls a client one, it becomes client
    changed = True
    client_bases = {b for b, p in placement.items() if p == "client"}
    while changed:
        changed = False
        for fn in ir["functions"]:
            if fn.get("abstract"):
                continue
            base = get_base_name(fn["signature_parts"])
            if placement.get(base) == "client":
                continue
            if _body_calls_any(fn.get("body", []), client_bases):
                placement[base] = "client"
                client_bases.add(base)
                changed = True
        for task in ir.get("tasks", []):
            if task.get("abstract"):
                continue
            base = _task_base_name(task)
            if placement.get(base) == "client":
                continue
            if _body_calls_any(task.get("body", []), client_bases):
                placement[base] = "client"
                client_bases.add(base)
                changed = True

    return placement


def _body_calls_any(body, fn_bases):
    """Check if any statement in the body calls a function in fn_bases."""
    from emit_base import get_base_name, make_function_name
    for stmt in body:
        if not isinstance(stmt, dict):
            continue
        if stmt.get("kind") == "fn_call":
            base = get_base_name(stmt["signature_parts"])
            if base in fn_bases:
                return True
        if stmt.get("kind") == "call":
            name = stmt.get("name", "").replace("-", "_")
            if name in fn_bases:
                return True
        # recurse into if blocks
        for branch in stmt.get("branches", []):
            if isinstance(branch, dict):
                if _body_calls_any(branch.get("body", []), fn_bases):
                    return True
    return False


def _build_feature_context(features, dynamic_set, plat_code, input_paths, client_fn_bases=None):
    """Parse composed source and map IR elements to features."""
    from composer import compose
    client_fn_bases = client_fn_bases or set()

    root_name = _find_root_feature(features)
    fn_owner = _build_fn_ownership(features)
    full_source = plat_code + "\n" + compose(features, dynamic_set)
    ir = _parse_full_source(full_source)

    ir_by_feature, ir_var_owners, features_with_tests = _map_ir_to_features(
        ir, features, fn_owner, root_name, full_source
    )
    ir["handlers"] = _collect_handlers(features)
    ir["_placement"] = _classify_function_placement(ir, client_fn_bases)
    ir["_client_fn_bases"] = client_fn_bases

    return {
        "ir": ir, "ir_by_feature": ir_by_feature, "ir_var_owners": ir_var_owners,
        "features_with_tests": features_with_tests,
        "module_map": _build_module_map(ir_by_feature),
        "dep_order": _compute_dep_order(features), "root_name": root_name,
        "client_fn_bases": client_fn_bases,
    }


def _read_feature_inputs(features_path):
    """Read feature list and source code, return entries, code, paths, dynamic set."""
    feature_entries = _read_feature_list(features_path)
    code_parts = _read_feature_sources(feature_entries)
    input_paths = [e["path"] for e in feature_entries]
    dynamic_set = {e["name"] for e in feature_entries if e["dynamic"]}
    return feature_entries, code_parts, input_paths, dynamic_set


def _build_features(features_path, output_dir, flags):
    """Build per-feature output files from a features.md listing."""
    if "--verbose" in flags:
        log.enable()

    _reject_zeta_output(output_dir)
    os.makedirs(output_dir, exist_ok=True)

    _, code_parts, input_paths, dynamic_set = _read_feature_inputs(features_path)
    features, per_feature = _compose_feature_set(code_parts, dynamic_set)
    platform_prepend, platform_append = _load_platform_implementations()
    plat_code, client_fn_bases = _load_platform_interface_code()

    ctx = _build_feature_context(features, dynamic_set, plat_code, input_paths, client_fn_bases)
    feature_tree_data = _build_feature_tree_data(features, input_paths)

    _ensure_package_json(output_dir)
    _write_runtime_files(ctx["features_with_tests"], output_dir)
    _emit_all_features(
        per_feature, ctx, input_paths,
        platform_prepend, platform_append, output_dir,
        feature_tree_data=feature_tree_data,
    )
    _compile_all_outputs(output_dir)
    if ctx.get("client_fn_bases"):
        _emit_client_bundle(ctx, output_dir)
    print(f"built {len(per_feature)} features -> {output_dir}")
    _report_errors(ctx["ir"])


if __name__ == "__main__":
    main()
