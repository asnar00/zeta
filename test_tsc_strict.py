"""TypeScript strict type-checking tests.

Validates that generated TypeScript passes tsc --strict --noEmit.
This catches type annotation errors that tsx (esbuild) silently ignores."""

import subprocess
import tempfile
import os
import pytest
from parser import process
from emit_typescript import emit


def _check_tsc(source: str):
    """Compile zero to TypeScript and run tsc --strict --noEmit."""
    ir = process(source)
    code = emit(ir)

    with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
        f.write(code)
        f.flush()
        try:
            result = subprocess.run(
                ["npx", "tsc", "--strict", "--noEmit", f.name],
                capture_output=True, text=True, timeout=15,
            )
            if result.returncode != 0:
                pytest.fail(
                    f"tsc --strict failed:\n{result.stdout}\n\nGenerated code:\n{code}"
                )
        finally:
            os.unlink(f.name)


# --- basic types ---

def test_tsc_int_variable():
    _check_tsc("    int i = 42")

def test_tsc_float_variable():
    _check_tsc("    float f = 1.5")

def test_tsc_string_variable():
    _check_tsc('    string s = "hello"')

def test_tsc_concrete_types():
    _check_tsc("""\
    type int32 = ... 32-bit signed integer
    type uint8 = ... 8-bit unsigned integer
    type float32 = ... 32-bit float
    int32 x = 10
    uint8 y = 255
    float32 z = 1.5""")


# --- structs ---

def test_tsc_struct():
    _check_tsc("""\
    type vector =
        int x, y, z = 0
    vector v = vector(1, 2, 3)""")

def test_tsc_struct_defaults():
    _check_tsc("""\
    type vector =
        int x, y, z = 0
    vector v = vector()""")

def test_tsc_type_composition():
    _check_tsc("""\
    type animal
        string name = ""
    type dog = animal +
        string breed = "unknown"
    dog d = dog(name="Rex", breed="labrador")""")


# --- enums ---

def test_tsc_enum():
    _check_tsc("    type colour = red | green | blue")

def test_tsc_enum_with_struct():
    _check_tsc("""\
    type direction = north | south | east | west
    type entity =
        direction facing = north
        int x, y = 0""")


# --- functions ---

def test_tsc_pure_function():
    _check_tsc("""\
    on (int n) = double (int x)
        n = x * 2""")

def test_tsc_ternary():
    _check_tsc("""\
    on (int n) = smaller of (int a) and (int b)
        n = (a) if (a < b) else (b)""")

def test_tsc_if_else():
    _check_tsc("""\
    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else if (n < 0)
            s = "negative"
        else
            s = "zero" """)

def test_tsc_function_calling_function():
    _check_tsc("""\
    on (int n) = double (int x)
        n = x * 2
    on (int n) = quadruple (int x)
        n = double (double (x))""")

def test_tsc_var_decl_in_body():
    _check_tsc("""\
    on (int r) = sum three (int a) and (int b) and (int c)
        int vals$ = [a, b, c]
        r = vals$ + _""")


# --- arrays ---

def test_tsc_array_literal():
    _check_tsc("    int i$ = [1, 2, 3, 4]")

def test_tsc_array_range():
    _check_tsc("    int i$ = [1 through 4]")

def test_tsc_array_map():
    _check_tsc("""\
    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2""")

def test_tsc_array_reduce():
    _check_tsc("""\
    int i$ = [1, 2, 3, 4]
    int sum = i$ + _""")


# --- streaming ---

def test_tsc_stream_until():
    _check_tsc("    int i$ <- 1 <- (i$ + 1) until (i$ == 5)")

def test_tsc_3step_stream():
    _check_tsc("    int i$ <- 0 <- 1 <- (i$ + i$) until (i$ > 50)")


# --- tasks ---

def test_tsc_task():
    _check_tsc("""\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)""")


# --- where and sort ---

def test_tsc_where():
    _check_tsc("""\
    int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)""")

def test_tsc_sort():
    _check_tsc("""\
    int i$ = [3, 1, 4, 1, 5, 9]
    int sorted$ = sort [i$]""")


# --- slicing and indexing ---

def test_tsc_index():
    _check_tsc("""\
    int i$ = [10, 20, 30]
    int first = i$[0]""")

def test_tsc_slice():
    _check_tsc("""\
    int i$ = [10, 20, 30, 40, 50]
    int mid$ = i$[1 to 3]""")


# --- comprehensive ---

def test_tsc_comprehensive():
    """A large program combining many features — the acid test."""
    _check_tsc("""\
    type number = int | float
    type vector =
        number x, y, z = 0
    type direction = north | south | east | west

    on (number n) = dot (vector a) and (vector b)
        n = a.x * b.x + a.y * b.y + a.z * b.z

    on (int n) = double (int x)
        n = x * 2

    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else
            s = "non-positive"

    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n

    vector a = vector(1, 2, 3)
    vector b = vector(4, 5, 6)

    int i$ = [1, 2, 3, 4, 5]
    int doubled$ = double (i$)
    int sum = i$ + _
    int evens$ = [i$] where (_ % 2 == 0)
    int sorted$ = sort [i$]
    int even$ <- only evens from (i$)

    int stream$ <- 1 <- (stream$ * 2) until (stream$ > 100)
    int multi$ <- 0 <- 1 <- (multi$ + multi$) until (multi$ > 50)""")
