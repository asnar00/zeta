"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py [--verbose] input.md output.py
"""

import os
import sys
from parser import process
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

    if len(args) != 2:
        print("Usage: python3 zeta.py [--verbose] input.md output.ext")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

    input_path = args[0]
    output_path = args[1]

    # determine emitter from output extension
    ext = "." + output_path.rsplit(".", 1)[-1] if "." in output_path else ""
    if ext not in _EMITTERS:
        print(f"Unsupported output format: {ext}")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

    # import the right emitter
    module = __import__(_EMITTERS[ext])
    emit = module.emit

    with log.section("read source"):
        source = open(input_path).read()
        log.log(f"{len(source)} chars from {input_path}")

    # collect platform signatures so the parser knows about them
    platform_dir = os.path.join(os.path.dirname(__file__), "platforms")
    if os.path.isdir(platform_dir):
        with log.section("load platform interfaces"):
            for fname in sorted(os.listdir(platform_dir)):
                if fname.endswith(".zero.md"):
                    log.log(fname)
                    with open(os.path.join(platform_dir, fname)) as pf:
                        source = pf.read() + "\n" + source

    with log.section("parse"):
        ir = process(source, source_file=input_path)
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

    print(f"{input_path} -> {output_path}")


if __name__ == "__main__":
    main()
