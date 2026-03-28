"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py input.md output.py
"""

import os
import sys
from parser import process

_EMITTERS = {
    ".py": "emit_python",
    ".ts": "emit_typescript",
}

_PLATFORM_EXT = {
    ".py": ".py",
    ".ts": ".ts",
}


def main():
    if len(sys.argv) != 3:
        print("Usage: python3 zeta.py input.md output.ext")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    # determine emitter from output extension
    ext = "." + output_path.rsplit(".", 1)[-1] if "." in output_path else ""
    if ext not in _EMITTERS:
        print(f"Unsupported output format: {ext}")
        print("Supported extensions:", ", ".join(_EMITTERS))
        sys.exit(1)

    # import the right emitter
    module = __import__(_EMITTERS[ext])
    emit = module.emit

    source = open(input_path).read()

    # collect platform signatures so the parser knows about them
    platform_dir = os.path.join(os.path.dirname(__file__), "platforms")
    if os.path.isdir(platform_dir):
        for fname in sorted(os.listdir(platform_dir)):
            if fname.endswith(".zero.md"):
                with open(os.path.join(platform_dir, fname)) as pf:
                    source = pf.read() + "\n" + source

    ir = process(source)
    output = emit(ir)

    # prepend platform implementations if they exist
    platform_dir = os.path.join(os.path.dirname(__file__), "platforms")
    platform_ext = _PLATFORM_EXT.get(ext, "")
    if os.path.isdir(platform_dir):
        platform_code = []
        for fname in sorted(os.listdir(platform_dir)):
            if fname.endswith(platform_ext) and not fname.endswith(".zero.md"):
                with open(os.path.join(platform_dir, fname)) as pf:
                    platform_code.append(pf.read())
        if platform_code:
            output = "\n\n".join(platform_code) + "\n\n" + output

    with open(output_path, "w") as f:
        f.write(output)

    print(f"{input_path} -> {output_path}")


if __name__ == "__main__":
    main()
