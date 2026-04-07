"""Execution tests: verify that emitted TypeScript code actually runs correctly.

Mirrors test_execution.py but compiles to TypeScript and runs with npx tsx."""

import json
import subprocess
import tempfile
import os
import pytest
from parser import process
from emit_typescript import emit


_TS_PLATFORM_PRELUDE = """
function fn__number_seconds(n: number): number { return n; }
function fn__number_ms(n: number): number { return n / 1000; }
function fn__number_hz(n: number): number { return 1 / n; }
function fn__number_bpm(n: number): number { return 60 / n; }
function fn_to_int__string(s: string): number { const n = parseInt(s, 10); return isNaN(n) ? 0 : n; }
function fn_dt_of(items: any): number { return items?.dt ?? 0; }
function fn_capacity_of(items: any): number { return items?.capacity ?? 0; }
function fn_t0_of(items: any): number { return items?.t0 ?? 0; }
"""

# functions that depend on _Stream (defined in emitter output, must come after)
_TS_PLATFORM_SUFFIX = """
function fn_snapshot(items: any): any {
    if (typeof _Stream !== 'undefined') {
        const copy = new _Stream([...items]);
        if (items.dt !== undefined) copy.dt = items.dt;
        if (items.capacity !== undefined) copy.capacity = items.capacity;
        if (items.t0 !== undefined) copy.t0 = items.t0;
        if (items._timestamps) copy._timestamps = [...items._timestamps];
        return copy;
    }
    return [...items];
}
"""


def _run(source: str, expression: str):
    """Compile zero source to TypeScript, run it, and evaluate an expression."""
    ir = process(source)
    code = emit(ir)
    # append a console.log that outputs JSON for the expression
    full = _TS_PLATFORM_PRELUDE + code + _TS_PLATFORM_SUFFIX + f"\nconsole.log(JSON.stringify({expression}));\n"

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
    assert _run(source, "i_arr") == [0, 1, 2, 3]


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


# --- var_decl and reduce in function body ---

def test_ts_exec_reduce_in_function_body():
    source = """\
    on (int r) = sum squares of (int a) and (int b) and (int c)
        int vals$ = [a, b, c]
        int sq$ = vals$ * vals$
        r = sq$ + _"""
    assert _run(source, "fn_sum_squares_of__int_and__int_and__int(3, 4, 5)") == 50

def test_ts_exec_var_decl_in_body():
    source = """\
    on (int r) = compute (int x)
        int doubled = x * 2
        r = doubled + 1"""
    assert _run(source, "fn_compute__int(5)") == 11

def test_ts_exec_array_decl_in_body():
    source = """\
    on (int r) = sum three (int a) and (int b) and (int c)
        int vals$ = [a, b, c]
        r = vals$ + _"""
    assert _run(source, "fn_sum_three__int_and__int_and__int(3, 4, 5)") == 12


# --- tasks ---

def test_ts_exec_task_filter():
    source = """\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)"""
    assert _run(source, "even_arr") == [2, 4, 6]


# --- type composition ---

def test_ts_exec_type_composition():
    source = """\
    type animal
        string name = ""
    type dog = animal +
        string breed = "unknown"
    dog d = dog(name="Rex", breed="labrador")"""
    assert _run(source, "d.name") == "Rex"
    assert _run(source, "d.breed") == "labrador"


# --- string operations ---

def test_ts_exec_string_concat():
    source = """\
    on (string r) = greet (string name)
        r = "hello " + name"""
    assert _run(source, 'fn_greet__string("world")') == "hello world"

def test_ts_exec_string_equality():
    source = """\
    on (string r) = check (string s)
        r = ("yes") if (s == "ok") else ("no")"""
    assert _run(source, 'fn_check__string("ok")') == "yes"
    assert _run(source, 'fn_check__string("nope")') == "no"


# --- stream doubling ---

def test_ts_exec_stream_doubling():
    source = """\
    int dbl$ <- 1 <- (dbl$ * 2) until (dbl$ > 100)"""
    assert _run(source, "dbl_arr") == [1, 2, 4, 8, 16, 32, 64, 128]


# --- named fn reduce ---

def test_ts_exec_named_fn_reduce():
    source = """\
    on (int n) = smaller of (int a) and (int b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int min = smaller of (i$) and (_)"""
    assert _run(source, "min") == 1


# --- string build operator ---

def test_ts_exec_string_build_literals():
    source = '    string s <- "hello" <- " " <- "world"'
    assert _run(source, "s") == "hello world"

def test_ts_exec_string_build_number():
    source = '    string s <- "value: " <- 42'
    assert _run(source, "s") == "value: 42"

def test_ts_exec_string_build_mixed():
    source = """\
    on (string s) = describe (int x) and (int y)
        string s <- "(" <- x <- ", " <- y <- ")"
    """
    assert _run(source, 'fn_describe__int_and__int(3, 7)') == "(3, 7)"


# --- time expressions ---

def test_ts_exec_time_seconds():
    source = "    time t = (1) seconds"
    assert _run(source, "t") == 1.0

def test_ts_exec_time_hz():
    source = "    time t = (10) hz"
    assert _run(source, "t") == 0.1

def test_ts_exec_time_ms():
    source = "    time t = (500) ms"
    assert _run(source, "t") == 0.5


# --- stream timing ---

def test_ts_exec_stream_at():
    source = "    int i$ <- 10 <- (i$ - 1) while (i$ > 0) at ((1) seconds)"
    assert _run(source, "[...i_arr]") == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    assert _run(source, "i_arr.dt") == 1.0

def test_ts_exec_stream_at_per_step():
    source = "    int i$ <- 10 at ((1) hz) <- 20 at ((100) hz)"
    assert _run(source, "[...i_arr]") == [10, 20]
    assert _run(source, "i_arr.dt") == 0.01

def test_ts_exec_stream_capacity():
    source = """\
    on (int i$) <- count to (int n)
        i$ <- 1 <- (i$ + 1) while (i$ <= n)
    int i$(dt = (1) seconds, capacity = (5) seconds)
    i$ <- count to (10)"""
    assert _run(source, "[...i_arr]") == [6, 7, 8, 9, 10]

def test_ts_exec_dt_of():
    source = "    int i$ <- 1 <- 2 <- 3 at ((10) hz)"
    assert _run(source, "fn_dt_of(i_arr)") == 0.1

def test_ts_exec_capacity_of():
    source = "    int i$(dt = (1) hz, capacity = (30) seconds)"
    assert _run(source, "fn_capacity_of(i_arr)") == 30.0

def test_ts_exec_sparse_stream():
    source = "    int i$(capacity = (100) seconds)"
    assert _run(source, "fn_capacity_of(i_arr)") == 100.0
    assert _run(source, "fn_dt_of(i_arr)") == 0

def test_ts_exec_snapshot_values():
    source = """\
    int i$ <- 1 <- 2 <- 3 at ((1) hz)
    int j$ = snapshot [i$]"""
    assert _run(source, "[...j_arr]") == [1, 2, 3]

def test_ts_exec_snapshot_timing():
    source = """\
    int i$ <- 1 <- 2 <- 3 at ((10) hz)
    int j$ = snapshot [i$]"""
    assert _run(source, "fn_dt_of(j_arr)") == 0.1


# --- to int ---

def test_ts_exec_to_int():
    source = """\
    on (int n) = parse (string s)
        n = to int (s)
    """
    assert _run(source, 'fn_parse__string("42")') == 42

def test_ts_exec_to_int_invalid():
    source = """\
    on (int n) = parse (string s)
        n = to int (s)
    """
    assert _run(source, 'fn_parse__string("abc")') == 0
