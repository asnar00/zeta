"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py [--verbose] input.md [input2.md ...] output.py
       python3 zeta.py [--verbose] features.md output_dir/

Run generated output with --test [names] to execute tests.
"""

import os
import sys
from parser import process
from parser import _is_markdown, _extract_code
import log

_EMITTERS = {
    ".py": "emit_python",
    ".ts": "emit_typescript",
}

_PLATFORM_EXT = {
    ".py": ".py",
    ".ts": ".ts",
}


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    flags = {a for a in sys.argv[1:] if a.startswith("--")}

    if "--verbose" in flags:
        log.enable()

    if len(args) < 2:
        print("Usage: python3 zeta.py [--verbose] input.md [input2.md ...] output.ext")
        print("       python3 zeta.py [--verbose] features.md output_dir/")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

    # check if first arg is a features.md file and second is a directory
    if len(args) == 2 and args[0].endswith("features.md") and args[1].endswith("/"):
        _build_features(args[0], args[1], flags)
        return

    input_paths = args[:-1]
    output_path = args[-1]

    # safety: don't write output into the zeta source directory
    zeta_dir = os.path.dirname(os.path.abspath(__file__))
    out_dir = os.path.dirname(os.path.abspath(output_path)) or zeta_dir
    if os.path.normpath(out_dir) == os.path.normpath(zeta_dir):
        print(f"Error: refusing to write output to the zeta source directory ({zeta_dir})")
        print("Use a subdirectory or different path for output files.")
        sys.exit(1)

    # determine emitter from output extension
    ext = "." + output_path.rsplit(".", 1)[-1] if "." in output_path else ""
    if ext not in _EMITTERS:
        print(f"Unsupported output format: {ext}")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

    # import the right emitter
    module = __import__(_EMITTERS[ext])
    emit = module.emit

    # read all input files
    with log.section("read source"):
        sources = []
        for path in input_paths:
            src = open(path).read()
            log.log(f"{len(src)} chars from {path}")
            sources.append(src)

    # if multiple inputs, use feature composition
    if len(input_paths) > 1:
        from feature_parser import parse_features
        from composer import compose

        with log.section("compose features"):
            # extract code from markdown for each source
            code_parts = []
            for src in sources:
                if _is_markdown(src):
                    src, _ = _extract_code(src)
                code_parts.append(src)
            combined = "\n".join(code_parts)
            features = parse_features(combined)
            for f in features:
                log.log(f"feature: {f['name']}" + (f" extends {f['extends']}" if f['extends'] else ""))
            # collect feature tests before composing (compose doesn't include tests)
            _feature_tests = {f["name"]: f["tests"] for f in features if f.get("tests")}
            source = compose(features)
            log.log(f"{len(source)} chars composed")
    else:
        source = sources[0]
        _feature_tests = {}

    # collect platform signatures so the parser knows about them
    platform_dir = os.path.join(os.path.dirname(__file__), "platforms")
    if os.path.isdir(platform_dir):
        with log.section("load platform interfaces"):
            for fname in sorted(os.listdir(platform_dir)):
                if fname.endswith(".zero.md"):
                    log.log(fname)
                    with open(os.path.join(platform_dir, fname)) as pf:
                        plat_src = pf.read()
                        # extract code from platform markdown
                        if _is_markdown(plat_src):
                            plat_src, _ = _extract_code(plat_src)
                        source = plat_src + "\n" + source

    with log.section("parse"):
        source_label = " + ".join(input_paths)
        ir = process(source, source_file=source_label)
        log.log(f"{len(ir['types'])} types, {len(ir['functions'])} functions, "
                f"{len(ir.get('tasks', []))} tasks, {len(ir['variables'])} variables")
        log.log(f"features: {', '.join(sorted(ir['features']))}")
        # parse feature tests (from feature_parser) against full source signatures
        _test_features = []
        if _feature_tests:
            from parser import parse_tests
            all_tests = []
            for feat_name, tests in _feature_tests.items():
                parsed = parse_tests(tests, source)
                for t in parsed:
                    t["feature"] = feat_name
                all_tests.extend(parsed)
                if parsed:
                    _test_features.append(feat_name)
                log.log(f"{feat_name}: {len(parsed)} tests")
            if all_tests:
                ir["tests"] = all_tests
        if ir.get("tests"):
            log.log(f"{len(ir['tests'])} tests total")
        if ir.get("errors"):
            for e in ir["errors"]:
                log.log(f"error: {e.format()}")
        if ir.get("warnings"):
            for w in ir["warnings"]:
                log.log(f"warning: {w}")

    with log.section(f"emit {ext}"):
        output = emit(ir)
        log.log(f"{len(output)} chars, {output.count(chr(10))} lines")

    # prepend platform implementations, append platform entry points
    platform_ext = _PLATFORM_EXT.get(ext, "")
    main_ext = ".main" + platform_ext  # e.g. ".main.py", ".main.ts"
    if os.path.isdir(platform_dir):
        with log.section("platform code"):
            prepend = []
            append = []
            for fname in sorted(os.listdir(platform_dir)):
                if fname.endswith(".zero.md"):
                    continue
                if fname.endswith(main_ext):
                    log.log(f"{fname} (append)")
                    with open(os.path.join(platform_dir, fname)) as pf:
                        append.append(pf.read())
                elif fname.endswith(platform_ext):
                    log.log(f"{fname} (prepend)")
                    with open(os.path.join(platform_dir, fname)) as pf:
                        prepend.append(pf.read())
            if prepend:
                output = "\n\n".join(prepend) + "\n\n" + output

    # write _runtime and add import if there are tests
    has_tests = bool(ir.get("tests"))
    if has_tests:
        runtime = _RUNTIME_PY if ext == ".py" else _RUNTIME_TS
        runtime_path = os.path.join(os.path.dirname(output_path) or ".", f"_runtime{ext}")
        with open(runtime_path, "w") as f:
            f.write(runtime)
        if ext == ".py":
            output = "from _runtime import register_tests, run_tests\n\n" + output
        elif ext == ".ts":
            output = "import { register_tests, run_tests } from './_runtime.js';\n\n" + output

    # combine platform harness and test runner into one entry point
    harness = "\n\n".join(append) if append else ""
    output += _main_entry_point(harness, has_tests, ext)

    with open(output_path, "w") as f:
        f.write(output)

    # compile/validate output
    _compile(ext, [output_path])

    print(f"{' + '.join(input_paths)} -> {output_path}")


_RUNTIME_PY = '''\
_test_registry = {}

def register_tests(feature, tests):
    _test_registry[feature] = tests

def run_tests(names=None):
    total = 0
    for feat, tests in _test_registry.items():
        if names and feat not in names:
            continue
        print(f"{feat}:")
        passed = failed = 0
        for fn, desc in tests:
            try:
                fn()
                passed += 1
                print(f"  PASS {desc}")
            except Exception as e:
                failed += 1
                print(f"  FAIL {desc}: {e}")
        print(f"{feat}: {passed} passed, {failed} failed")
        total += failed
    return total
'''

_RUNTIME_TS = '''\
export const _test_registry: Map<string, [() => void, string][]> = new Map();

export function register_tests(feature: string, tests: [() => void, string][]): void {
    _test_registry.set(feature, tests);
}

export function run_tests(names?: string[]): number {
    let total = 0;
    for (const [feat, tests] of _test_registry) {
        if (names && names.length && !names.includes(feat)) continue;
        console.log(`${feat}:`);
        let passed = 0;
        let failed = 0;
        for (const [fn, desc] of tests) {
            try {
                fn();
                passed++;
                console.log(`  PASS ${desc}`);
            } catch (e: any) {
                failed++;
                console.log(`  FAIL ${desc}: ${e.message}`);
            }
        }
        console.log(`${feat}: ${passed} passed, ${failed} failed`);
        total += failed;
    }
    return total;
}
'''


def _main_entry_point(harness: str, has_tests: bool, ext: str) -> str:
    """Generate a combined main entry point with test infrastructure and platform harness."""
    if not harness and not has_tests:
        return ""
    if ext == ".py":
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
    elif ext == ".ts":
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
    return ""


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


def _build_features(features_path: str, output_dir: str, flags: set):
    """Build per-feature output files from a features.md listing."""
    from feature_parser import parse_features
    from composer import compose_per_feature

    if "--verbose" in flags:
        log.enable()

    # safety: don't write output into the zeta source directory
    zeta_dir = os.path.dirname(os.path.abspath(__file__))
    if os.path.normpath(os.path.abspath(output_dir)) == os.path.normpath(zeta_dir):
        print(f"Error: refusing to write output to the zeta source directory ({zeta_dir})")
        print("Use a subdirectory or different path for output files.")
        sys.exit(1)

    os.makedirs(output_dir, exist_ok=True)

    # read feature list
    with log.section("read features"):
        feature_lines = open(features_path).read().strip().split("\n")
        input_paths = [line.strip() for line in feature_lines if line.strip()]
        log.log(f"{len(input_paths)} features")

    # read and extract code from each feature file
    with log.section("read source"):
        code_parts = []
        for path in input_paths:
            src = open(path).read()
            log.log(f"{len(src)} chars from {path}")
            if _is_markdown(src):
                src, _ = _extract_code(src)
            code_parts.append(src)

    # parse features and compose per-feature
    with log.section("compose features"):
        combined = "\n".join(code_parts)
        features = parse_features(combined)
        per_feature = compose_per_feature(features)
        for name, source in per_feature.items():
            log.log(f"{name}: {len(source)} chars")

    # load platform interface code (for prepending to source before parsing)
    platform_dir = os.path.join(os.path.dirname(__file__), "platforms")
    plat_code = ""
    if os.path.isdir(platform_dir):
        for fname in sorted(os.listdir(platform_dir)):
            if fname.endswith(".zero.md"):
                with open(os.path.join(platform_dir, fname)) as pf:
                    plat_src = pf.read()
                    if _is_markdown(plat_src):
                        plat_src, _ = _extract_code(plat_src)
                    plat_code = plat_src + "\n" + plat_code

    # load platform implementation files
    platform_prepend = {ext: [] for ext in _PLATFORM_EXT}
    platform_append = {ext: [] for ext in _PLATFORM_EXT}
    if os.path.isdir(platform_dir):
        for fname in sorted(os.listdir(platform_dir)):
            if fname.endswith(".zero.md"):
                continue
            for ext, pext in _PLATFORM_EXT.items():
                main_ext = ".main" + pext
                if fname.endswith(main_ext):
                    with open(os.path.join(platform_dir, fname)) as pf:
                        platform_append[ext].append(pf.read())
                elif fname.endswith(pext):
                    with open(os.path.join(platform_dir, fname)) as pf:
                        platform_prepend[ext].append(pf.read())

    # find the root feature (the one nothing extends)
    root_name = None
    for f in features:
        if f["extends"] is None:
            root_name = f["name"]
            break

    # build function ownership map: fn_name -> feature_name
    fn_owner = {}
    for f in features:
        for fn_name in f.get("functions", {}):
            fn_owner[fn_name] = f["name"]

    # parse ALL composed source together (so all signatures are visible)
    from composer import compose
    full_composed = compose(features)
    full_source = plat_code + "\n" + full_composed

    with log.section("parse"):
        ir = process(full_source, source_file="composed")
        log.log(f"{len(ir['types'])} types, {len(ir['functions'])} functions, "
                f"{len(ir.get('tasks', []))} tasks, {len(ir['variables'])} variables")

    # map IR functions/tasks to their owning feature
    # function names in IR use signature_parts; match against feature fn names
    from emit_base import get_base_name
    ir_by_feature = {f["name"]: {"types": [], "variables": [], "functions": [], "tasks": []}
                     for f in features}

    for fn in ir["functions"]:
        base = get_base_name(fn["signature_parts"])
        # find owner by matching against feature function names
        owner = None
        for feat_fn_name, feat_name in fn_owner.items():
            # normalize to underscore-separated words for comparison
            normalized_feat = "_".join(feat_fn_name.split())
            # prefix match: feature name may be shorter than full signature
            if base == normalized_feat or base.startswith(normalized_feat + "_") or base.startswith(normalized_feat + "__"):
                owner = feat_name
                break
        if owner is None:
            owner = root_name  # default to root
            fn["_platform"] = True  # mark as platform function
        ir_by_feature[owner]["functions"].append(fn)

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

    # variables go to the feature that defined them
    var_owners = {}
    for f in features:
        for v in f.get("variables", []):
            var_owners[v] = f["name"]
    for var in ir["variables"]:
        # match by name
        owner = root_name
        for var_line, feat_name in var_owners.items():
            if var["name"] in var_line:
                owner = feat_name
                break
        ir_by_feature[owner]["variables"].append(var)

    # parse and assign tests per feature
    from parser import parse_tests
    features_with_tests = []
    for f in features:
        if f.get("tests"):
            parsed = parse_tests(f["tests"], full_source)
            if parsed:
                ir_by_feature[f["name"]]["tests"] = parsed
                features_with_tests.append(f["name"])
            log.log(f"{f['name']}: {len(parsed)} tests")

    # build module map: emitted_function_name → feature_name
    from emit_base import make_function_name, make_task_fn_name as _make_task_fn_name
    module_map = {}
    for feat_name, feat_ir_data in ir_by_feature.items():
        for fn in feat_ir_data["functions"]:
            if fn.get("_platform"):
                continue  # platform functions are available directly, not via module
            emitted_name = make_function_name(fn["signature_parts"])
            module_map[emitted_name] = feat_name
        for task in feat_ir_data["tasks"]:
            emitted_name = _make_task_fn_name(task)
            module_map[emitted_name] = feat_name

    # figure out dependency order
    feature_deps = {}
    for f in features:
        feature_deps[f["name"]] = f["extends"]
    dep_order = []
    for f in features:
        if f["extends"] is None:
            continue
        if f["name"] not in dep_order:
            dep_order.append(f["name"])

    # write package.json for ESM support (needed for tsx to resolve .js → .ts)
    pkg_path = os.path.join(output_dir, "package.json")
    if not os.path.exists(pkg_path):
        with open(pkg_path, "w") as f:
            f.write('{ "type": "module" }\n')

    # write _runtime files if any features have tests
    if features_with_tests:
        for ext, pext in _PLATFORM_EXT.items():
            runtime = _RUNTIME_PY if ext == ".py" else _RUNTIME_TS
            runtime_path = os.path.join(output_dir, f"_runtime{ext}")
            with open(runtime_path, "w") as f:
                f.write(runtime)

    # emit per-feature output for each target
    for ext in _EMITTERS:
        emitter_module = __import__(_EMITTERS[ext])
        emit = emitter_module.emit
        pext = _PLATFORM_EXT[ext]

        with log.section(f"emit {ext}"):
            for feat_name in per_feature:
                feat_ir = dict(ir)  # copy base IR
                feat_ir["functions"] = ir_by_feature[feat_name]["functions"]
                feat_ir["tasks"] = ir_by_feature[feat_name]["tasks"]
                feat_ir["variables"] = ir_by_feature[feat_name]["variables"]
                # find the source file for this feature
                feat_source_file = None
                for p in input_paths:
                    if feat_name in p:
                        feat_source_file = p
                        break
                feat_ir["source_file"] = feat_source_file or f"{feat_name}.zero.md"
                # pass module_map so emitter can prefix cross-module calls
                feat_ir["module_map"] = module_map
                feat_ir["current_module"] = feat_name
                # add tests for this feature
                if ir_by_feature[feat_name].get("tests"):
                    feat_ir["tests"] = ir_by_feature[feat_name]["tests"]
                    feat_ir["test_feature"] = feat_name

                code = emit(feat_ir)

                # prepend platform implementations to all features
                if platform_prepend[ext]:
                    code = "\n\n".join(platform_prepend[ext]) + "\n\n" + code

                # non-root features
                if feat_name != root_name:
                    # child features with tests import from _runtime
                    if ir_by_feature[feat_name].get("tests"):
                        if ext == ".py":
                            code = f"from _runtime import register_tests\n\n" + code
                        elif ext == ".ts":
                            code = f"import {{ register_tests }} from './_runtime.js';\n\n" + code
                    if ext == ".ts":
                        code = code.replace("function ", "export function ")
                        code = code.replace("function* ", "export function* ")

                # root feature gets platform code, imports, harness
                if feat_name == root_name:
                    imports = []
                    has_tests = bool(features_with_tests)
                    if has_tests and ext == ".py":
                        imports.append("from _runtime import register_tests, run_tests")
                    elif has_tests and ext == ".ts":
                        imports.append("import { register_tests, run_tests } from './_runtime.js';")
                    for dep in dep_order:
                        if ext == ".py":
                            imports.append(f"import {dep}")
                        elif ext == ".ts":
                            imports.append(f"import './{dep}.js';")  # side-effect: registers tests
                            imports.append(f"import * as {dep} from './{dep}.js';")
                    if imports:
                        code = "\n".join(imports) + "\n\n" + code
                    harness = "\n\n".join(platform_append[ext]) if platform_append[ext] else ""
                    code += _main_entry_point(harness, has_tests, ext)

                out_path = os.path.join(output_dir, f"{feat_name}{pext}")
                with open(out_path, "w") as f:
                    f.write(code)
                log.log(f"{feat_name} -> {out_path}")

    # compile/validate all output files
    for ext, pext in _PLATFORM_EXT.items():
        files = [os.path.join(output_dir, f) for f in os.listdir(output_dir) if f.endswith(pext)]
        if files:
            _compile(ext, files)

    print(f"built {len(per_feature)} features -> {output_dir}")


if __name__ == "__main__":
    main()
