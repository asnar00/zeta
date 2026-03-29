"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py [--verbose] input.md [input2.md ...] output.py
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
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

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


if __name__ == "__main__":
    main()
