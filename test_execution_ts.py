"""Execution tests: verify that emitted TypeScript code actually runs correctly.

Mirrors test_execution.py but compiles to TypeScript and runs with npx tsx."""

import json
import subprocess
import tempfile
import os
import pytest
from parser import process
from emit_typescript import emit


def _run(source: str, expression: str):
    """Compile zero source to TypeScript, run it, and evaluate an expression."""
    ir = process(source)
    code = emit(ir)
    # append a console.log that outputs JSON for the expression
    full = code + f"\nconsole.log(JSON.stringify({expression}));\n"

    with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
        f.write(full)
        f.flush()
        try:
            result = subprocess.run(
                ["npx", "tsx", f.name],
                capture_output=True, text=True, timeout=10,
            )
            if result.returncode != 0:
                pytest.fail(f"TypeScript execution failed:\n{result.stderr}\n\nGenerated code:\n{code}")
            output = result.stdout.strip()
            if not output:
                pytest.fail(f"No output from TypeScript.\nstderr: {result.stderr}\n\nGenerated code:\n{code}")
            return json.loads(output)
        finally:
            os.unlink(f.name)


# --- basic types and variables ---

def test_ts_exec_int_variable():
    assert _run("    int i = 10", "i") == 10

def test_ts_exec_float_variable():
    assert _run("    float f = 1.5", "f") == 1.5

def test_ts_exec_string_variable():
    assert _run('    string s = "hello"', "s") == "hello"


# --- structs ---

def test_ts_exec_struct_constructor():
    source = """\
    type vector =
        int x, y, z = 0
    vector v = vector(1, 2, 3)"""
    assert _run(source, "v.x") == 1
    assert _run(source, "v.z") == 3

def test_ts_exec_struct_defaults():
    source = """\
    type vector =
        int x, y, z = 0
    vector v = vector()"""
    assert _run(source, "v.x") == 0


# --- functions ---

def test_ts_exec_pure_function():
    source = """\
    on (int n) = double (int x)
        n = x * 2"""
    assert _run(source, "fn_double__int(5)") == 10

def test_ts_exec_ternary():
    source = """\
    on (int n) = smaller of (int a) and (int b)
        n = (a) if (a < b) else (b)"""
    assert _run(source, "fn_smaller_of__int_and__int(3, 7)") == 3
    assert _run(source, "fn_smaller_of__int_and__int(9, 2)") == 2

def test_ts_exec_recursion():
    source = """\
    on (int n) = factorial of (int x)
        n = (1) if (x == 1) else (x * factorial of (x - 1))"""
    assert _run(source, "fn_factorial_of__int(5)") == 120

def test_ts_exec_function_calling_function():
    source = """\
    on (int n) = double (int x)
        n = x * 2
    on (int n) = quadruple (int x)
        n = double (double (x))"""
    assert _run(source, "fn_quadruple__int(3)") == 12


# --- arrays ---

def test_ts_exec_array_literal():
    source = "    int i$ = [1, 2, 3, 4]"
    assert _run(source, "i_arr") == [1, 2, 3, 4]

def test_ts_exec_array_range_through():
    source = "    int i$ = [1 through 4]"
    assert _run(source, "i_arr") == [1, 2, 3, 4]

def test_ts_exec_array_map():
    source = """\
    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2"""
    assert _run(source, "j_arr") == [2, 4, 6, 8]

def test_ts_exec_array_reduce():
    source = """\
    int i$ = [1, 2, 3, 4]
    int sum = i$ + _"""
    assert _run(source, "sum") == 10


# --- streaming ---

def test_ts_exec_stream_simple():
    source = "    int i$ <- 1 <- 2 <- 3"
    assert _run(source, "i_arr") == [1, 2, 3]

def test_ts_exec_stream_until():
    source = "    int i$ <- 1 <- (i$ + 1) until (i$ == 5)"
    assert _run(source, "i_arr") == [1, 2, 3, 4, 5]

def test_ts_exec_stream_while():
    source = "    int i$ <- 0 <- (i$ + 1) while (i$ < 4)"
    assert _run(source, "i_arr") == [0, 1, 2, 3, 4]


# --- 3-step streaming ---

def test_ts_exec_3step_stream_no_terminator():
    source = "    int i$ <- 10 <- 20 <- 30"
    assert _run(source, "i_arr") == [10, 20, 30]

def test_ts_exec_3step_stream_with_until():
    source = "    int i$ <- 0 <- 1 <- (i$ + i$) until (i$ > 50)"
    result = _run(source, "i_arr")
    assert result == [0, 1, 2, 4, 8, 16, 32, 64]


# --- conditionals ---

def test_ts_exec_if_else():
    source = """\
    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else if (n < 0)
            s = "negative"
        else
            s = "zero" """
    assert _run(source, 'fn_describe__int(5)') == "positive"
    assert _run(source, 'fn_describe__int(-3)') == "negative"
    assert _run(source, 'fn_describe__int(0)') == "zero"


# --- array slicing and indexing ---

def test_ts_exec_array_index():
    source = """\
    int i$ = [10, 20, 30, 40]
    int first = i$[0]
    int last = i$[3]"""
    assert _run(source, "first") == 10
    assert _run(source, "last") == 40

def test_ts_exec_array_slice():
    source = """\
    int i$ = [10, 20, 30, 40, 50]
    int mid$ = i$[1 to 4]"""
    assert _run(source, "mid_arr") == [20, 30, 40]


# --- where ---

def test_ts_exec_where():
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)"""
    assert _run(source, "evens_arr") == [2, 4, 6]


# --- sort ---

def test_ts_exec_sort():
    source = """\
    int i$ = [3, 1, 4, 1, 5, 9]
    int sorted$ = sort [i$]"""
    assert _run(source, "sorted_arr") == [1, 1, 3, 4, 5, 9]


# --- enums ---

def test_ts_exec_enum():
    source = """\
    type colour = red | green | blue"""
    assert _run(source, "colour.red") == "red"


# --- named function map ---

def test_ts_exec_named_fn_map():
    source = """\
    on (int n) = double (int x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)"""
    assert _run(source, "j_arr") == [2, 4, 6, 8]


# --- bitwise operators ---

def test_ts_exec_bitwise_and():
    source = """\
    on (int r) = low nibble of (int x)
        r = x & 15"""
    assert _run(source, "fn_low_nibble_of__int(0xAB)") == 0x0B

def test_ts_exec_shift():
    source = """\
    on (int r) = shift left (int x) by (int n)
        r = x << n"""
    assert _run(source, "fn_shift_left__int_by__int(1, 4)") == 16


# --- length of ---

def test_ts_exec_length_of():
    source = """\
    int i$ = [10, 20, 30]
    int n = length of [i$]"""
    assert _run(source, "n") == 3


# --- self-map ---

def test_ts_exec_self_map():
    source = """\
    int i$ = [1, 2, 3, 4]
    int sq$ = i$ * i$"""
    assert _run(source, "sq_arr") == [1, 4, 9, 16]


# --- chained operations ---

def test_ts_exec_map_then_reduce():
    source = """\
    on (int n) = double (int x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int doubled$ = double (i$)
    int sum = doubled$ + _"""
    assert _run(source, "sum") == 20
