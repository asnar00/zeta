"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py [--verbose] input.md [input2.md ...] output.py
       python3 zeta.py [--verbose] features.md output_dir/
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
            source = compose(features)
            log.log(f"{len(source)} chars composed")
    else:
        source = sources[0]

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
            if append:
                output = output + "\n\n" + "\n\n".join(append)

    with open(output_path, "w") as f:
        f.write(output)

    print(f"{' + '.join(input_paths)} -> {output_path}")


def _build_features(features_path: str, output_dir: str, flags: set):
    """Build per-feature output files from a features.md listing."""
    from feature_parser import parse_features
    from composer import compose_per_feature

    if "--verbose" in flags:
        log.enable()

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

    # emit per-feature output for each target
    for ext in _EMITTERS:
        module = __import__(_EMITTERS[ext])
        emit = module.emit
        pext = _PLATFORM_EXT[ext]

        with log.section(f"emit {ext}"):
            for feat_name in per_feature:
                feat_ir = dict(ir)  # copy base IR
                feat_ir["functions"] = ir_by_feature[feat_name]["functions"]
                feat_ir["tasks"] = ir_by_feature[feat_name]["tasks"]
                feat_ir["variables"] = ir_by_feature[feat_name]["variables"]
                feat_ir["source_file"] = f"ziz/{feat_name}.zero.md"

                code = emit(feat_ir)

                # root feature gets platform code and harness
                if feat_name == root_name:
                    if platform_prepend[ext]:
                        code = "\n\n".join(platform_prepend[ext]) + "\n\n" + code
                    if platform_append[ext]:
                        code = code + "\n\n" + "\n\n".join(platform_append[ext])

                out_path = os.path.join(output_dir, f"{feat_name}.zero{pext}")
                with open(out_path, "w") as f:
                    f.write(code)
                log.log(f"{feat_name} -> {out_path}")

    print(f"built {len(per_feature)} features -> {output_dir}")


if __name__ == "__main__":
    main()
