"""zeta: translate zero programs to any target language.

Usage: python3 zeta.py input.md output.py
"""

import sys
from parser import process

_EMITTERS = {
    ".py": "emit_python",
    ".ts": "emit_typescript",
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
    ir = process(source)
    output = emit(ir)

    with open(output_path, "w") as f:
        f.write(output)

    print(f"{input_path} -> {output_path}")


if __name__ == "__main__":
    main()
