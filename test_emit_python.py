"""Tests for the Python emitter."""

from parser import process
from emit_python import emit


def _translate(source: str) -> str:
    """Helper: parse zero source and emit Python."""
    return emit(process(source))


def _check(report, title, source, *assertions):
    """Run translation, log to report, and check assertions."""
    out = _translate(source)
    report.add(title, source, out)
    for assertion in assertions:
        assertion(out)


# --- numeric types ---

def test_emit_concrete_int(report):
    _check(report, "concrete int type",
           "    type int32 = ... 32-bit signed integer",
           lambda out: _assert_in("type int32 = int", out))

def test_emit_concrete_float(report):
    _check(report, "concrete float type",
           "    type float32 = ... 32-bit IEEE floating-point number",
           lambda out: _assert_in("type float32 = float", out))

def test_emit_abstract_uint(report):
    _check(report, "abstract uint type",
           "    type uint = ... any-size unsigned integer",
           lambda out: _assert_in("type uint = int", out))

def test_emit_builtin_int_skipped(report):
    _check(report, "builtin int (no alias)",
           "    type int = ... any-size signed integer",
           lambda out: _assert_not_in("type int", out))

def test_emit_builtin_float_skipped(report):
    _check(report, "builtin float (no alias)",
           "    type float = ... any-size IEEE floating-point number",
           lambda out: _assert_not_in("type float", out))

def test_emit_union_type(report):
    _check(report, "union type",
           "    type number = int | float",
           lambda out: _assert_in("type number = int | float", out))


# --- enumerations ---

def test_emit_enum(report):
    _check(report, "enumeration",
           "    type tri-state = no | yes | maybe",
           lambda out: _assert_in("from enum import Enum", out),
           lambda out: _assert_in("class tri_state(Enum):", out),
           lambda out: _assert_in('    no = "no"', out),
           lambda out: _assert_in('    yes = "yes"', out),
           lambda out: _assert_in('    maybe = "maybe"', out))


# --- structure types ---

def test_emit_struct(report):
    source = """\
    type vector =
        number x, y, z = 0"""
    _check(report, "structure type",
           source,
           lambda out: _assert_in("from typing import NamedTuple", out),
           lambda out: _assert_in("class vector(NamedTuple):", out),
           lambda out: _assert_in("    x: float = 0", out),
           lambda out: _assert_in("    y: float = 0", out),
           lambda out: _assert_in("    z: float = 0", out))


# --- struct with enum default ---

def test_emit_struct_enum_default(report):
    source = """\
    type direction = north | south | east | west
    type entity =
        direction facing = north"""
    _check(report, "struct with enum default",
           source,
           lambda out: _assert_in("facing: direction = direction.north", out))


# --- variables ---

def test_emit_scalar_variable(report):
    _check(report, "scalar int variable",
           "    int32 i = 10",
           lambda out: _assert_in("i: int32 = 10", out))

def test_emit_float_variable(report):
    _check(report, "scalar float variable",
           "    float f = 1.02",
           lambda out: _assert_in("f: float = 1.02", out))

def test_emit_string_variable(report):
    _check(report, "string variable",
           '    string s = "hello"',
           lambda out: _assert_in('s: str = "hello"', out))

def test_emit_struct_constructor(report):
    _check(report, "struct constructor (positional)",
           "    vector v = vector(1, 2, 3)",
           lambda out: _assert_in("v: vector = vector(1, 2, 3)", out))

def test_emit_struct_constructor_named(report):
    _check(report, "struct constructor (named)",
           "    vector v = vector(z=2, x=1)",
           lambda out: _assert_in("v: vector = vector(z=2, x=1)", out))


# --- arrays ---

def test_emit_array_empty(report):
    _check(report, "empty array",
           "    int i$",
           lambda out: _assert_in("i_arr: list[int] = []", out))

def test_emit_array_sized(report):
    _check(report, "sized array",
           "    int i$[4]",
           lambda out: _assert_in("i_arr: list[int] = [0] * 4", out))

def test_emit_array_sized_fill(report):
    _check(report, "sized array with fill",
           "    int i$[4] = 1",
           lambda out: _assert_in("i_arr: list[int] = [1] * 4", out))

def test_emit_array_literal(report):
    _check(report, "array literal",
           "    int i$ = [1, 2, 3, 4]",
           lambda out: _assert_in("i_arr: list[int] = [1, 2, 3, 4]", out))

def test_emit_array_through(report):
    _check(report, "array with through range",
           "    int i$ = [1 through 4]",
           lambda out: _assert_in("i_arr: list[int] = list(range(1, 5))", out))

def test_emit_array_to(report):
    _check(report, "array with to range",
           "    int i$ = [0 to 4]",
           lambda out: _assert_in("i_arr: list[int] = list(range(0, 4))", out))


# --- streaming ---

def test_emit_stream_simple(report):
    _check(report, "simple stream",
           "    int i$ <- 1 <- 2 <- 3 <- 4",
           lambda out: _assert_in("i_arr = [1, 2, 3, 4]", out))

def test_emit_stream_self_ref(report):
    _check(report, "self-referencing stream",
           "    int i$ <- 1 <- (i$ + 1)",
           lambda out: _assert_in("i_arr = [1]", out),
           lambda out: _assert_in("i_arr.append(i_arr[-1] + 1)", out))

def test_emit_stream_until(report):
    _check(report, "stream with until",
           "    int i$ <- 1 <- (i$ + 1) until (i$ == 4)",
           lambda out: _assert_in("i_arr = [1]", out),
           lambda out: _assert_in("while not (i_arr[-1] == 4):", out),
           lambda out: _assert_in("i_arr.append(i_arr[-1] + 1)", out))

def test_emit_stream_while(report):
    _check(report, "stream with while",
           "    int i$ <- 0 <- (i$ + 1) while (i$ < 4)",
           lambda out: _assert_in("i_arr = [0]", out),
           lambda out: _assert_in("while i_arr[-1] < 4:", out),
           lambda out: _assert_in("i_arr.append(i_arr[-1] + 1)", out))


# --- array mapping ---

def test_emit_array_map_scalar(report):
    source = """\
    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2"""
    _check(report, "array map with scalar",
           source,
           lambda out: _assert_in("j_arr: list[int] = [x * 2 for x in i_arr]", out))

def test_emit_array_map_two_arrays(report):
    source = """\
    int i$ = [1, 2, 3, 4, 5]
    int j$ = [1, 4, 7]
    int k$ = i$ + j$"""
    _check(report, "array map with two arrays",
           source,
           lambda out: _assert_in("k_arr: list[int] = [a + b for a, b in zip_longest(i_arr, j_arr, fillvalue=0)]", out))


# --- array literal with expressions ---

def test_emit_array_literal_with_fn_calls(report):
    source = """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [double (1), double (2), double (3)]"""
    _check(report, "array literal with fn calls",
           source,
           lambda out: _assert_in("i_arr: list[int] = [fn_double__number(1), fn_double__number(2), fn_double__number(3)]", out))


# --- named function mapped over array ---

def test_emit_named_fn_map_single(report):
    source = """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)"""
    _check(report, "named function mapped over array",
           source,
           lambda out: _assert_in("j_arr: list[int] = [fn_double__number(x) for x in i_arr]", out))

def test_emit_named_fn_map_with_scalar(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)"""
    _check(report, "named function map with scalar arg",
           source,
           lambda out: _assert_in("j_arr: list[int] = [fn_smaller_of__number_and__number(x, 3) for x in i_arr]", out))


# --- array reduction ---

def test_emit_reduce_operator(report):
    source = """\
    int i$ = [1, 2, 3, 4]
    int sum = i$ + _"""
    _check(report, "array reduce with operator",
           source,
           lambda out: _assert_in("sum: int = functools.reduce(lambda a, b: a + b, i_arr)", out))

def test_emit_reduce_named_function(report):
    source = """\
    int i$ = [1, 2, 3, 4]
    int min = smaller of (i$) and (_)"""
    _check(report, "array reduce with named function",
           source,
           lambda out: _assert_in("min: int = functools.reduce(fn_smaller_of__int_and__int, i_arr)", out))


# --- functions ---

def test_emit_function_operator(report):
    source = """\
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)"""
    _check(report, "operator function",
           source,
           lambda out: _assert_in("def fn__vector_plus__vector(a: vector, b: vector) -> vector:", out),
           lambda out: _assert_in("    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)", out),
           lambda out: _assert_in("    return v", out))

def test_emit_void_function(report):
    source = """\
    on hello()
        print "hello world" """
    _check(report, "void function",
           source,
           lambda out: _assert_in("def fn_hello():", out),
           lambda out: _assert_not_in("return", out))

def test_emit_void_function_with_params(report):
    source = """\
    on greet (string name)
        print name"""
    _check(report, "void function with params",
           source,
           lambda out: _assert_in("def fn_greet__string(name: str):", out),
           lambda out: _assert_not_in("return", out))

def test_emit_concurrently(report):
    source = """\
    on run()
        concurrently
            hello()
        and
            beep()"""
    _check(report, "concurrently block",
           source,
           lambda out: _assert_in("_concurrently(lambda: hello(), lambda: beep())", out),
           lambda out: _assert_in("def _concurrently(*fns):", out))

def test_emit_function_named(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""
    _check(report, "named function",
           source,
           lambda out: _assert_in("def fn_smaller_of__number_and__number(a: float, b: float) -> float:", out),
           lambda out: _assert_in("    return n", out))


# --- filtering (where) ---

def test_emit_where(report):
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)"""
    _check(report, "where filter",
           source,
           lambda out: _assert_in("evens_arr = [x for x in i_arr if x % 2 == 0]", out))

def test_emit_first_of_where(report):
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int first_even = first of [i$] where (_ % 2 == 0)"""
    _check(report, "first of where",
           source,
           lambda out: _assert_in("next((x for x in i_arr if x % 2 == 0), type(i_arr[0])() if i_arr else None)", out))


# --- sorting ---

def test_emit_sort_simple(report):
    source = """\
    int i$ = [3, 1, 4, 1, 5]
    int sorted$ = sort [i$]"""
    _check(report, "sort simple",
           source,
           lambda out: _assert_in("sorted_arr = sorted(i_arr)", out))

def test_emit_sort_by(report):
    source = """\
    type person =
        int age = 0
    person people$ = [...]
    person sorted$ = sort [people$] by (_.age)"""
    _check(report, "sort by key",
           source,
           lambda out: _assert_in("sorted_arr = sorted(people_arr, key=lambda x: x.age)", out))


# --- array slicing ---

def test_emit_slice_to(report):
    source = """\
    on (int result$) = first three of (int items$)
        result$ = items$[0 to 3]"""
    _check(report, "slice with to",
           source,
           lambda out: _assert_in("items_arr[0:3]", out))

def test_emit_slice_through(report):
    source = """\
    on (int result$) = mid of (int items$)
        result$ = items$[1 through 3]"""
    _check(report, "slice with through",
           source,
           lambda out: _assert_in("items_arr[1:4]", out))


# --- type composition ---

def test_emit_subtype_flattened(report):
    source = """\
    type animal
        string name = ""
    type dog = animal +
        string breed = "unknown" """
    _check(report, "subtype flattens parent fields",
           source,
           lambda out: _assert_in("class dog(NamedTuple):", out),
           lambda out: _assert_in("    name: str", out),
           lambda out: _assert_in("    breed: str", out))


# --- multiple dispatch ---

def test_emit_single_dispatch_no_table(report):
    """Single definition — no dispatch table, just a direct function."""
    source = """\
    on (string s) = describe (int n)
        s = "a number" """
    _check(report, "single dispatch (no table)",
           source,
           lambda out: _assert_in("def fn_describe__int(n: int) -> str:", out),
           lambda out: _assert_not_in("_dispatch", out))

def test_emit_multiple_dispatch(report):
    """Multiple definitions → dispatch table."""
    source = """\
    type animal
    type dog = animal +
        string breed = "unknown"
    type cat = animal +
        int lives = 9
    on (string s) = describe (animal a)
        s = "an animal"
    on (string s) = describe (dog d)
        s = "a dog"
    on (string s) = describe (cat c)
        s = "a cat" """
    _check(report, "multiple dispatch",
           source,
           lambda out: _assert_in("def fn_describe__animal(a: animal)", out),
           lambda out: _assert_in("def fn_describe__dog(d: dog)", out),
           lambda out: _assert_in("def fn_describe__cat(c: cat)", out),
           lambda out: _assert_in("def fn_describe(", out),  # dispatcher function
           lambda out: _assert_in("isinstance", out))


# --- recursion ---

def test_emit_recursive_function(report):
    source = """\
    on (int n) = factorial of (int x)
        n = (1) if (x == 1) else (x * factorial of (x - 1))"""
    _check(report, "recursive function",
           source,
           lambda out: _assert_in("def fn_factorial_of__int(x: int) -> int:", out),
           lambda out: _assert_in("fn_factorial_of__int(x - 1)", out))


# --- array function calls ---

def test_emit_array_fn_definition(report):
    source = """\
    on (int n) = count of [items$]
        n = length of [items$]"""
    _check(report, "array function definition",
           source,
           lambda out: _assert_in("def fn_count_of(items_arr) -> int:", out))

def test_emit_array_fn_call(report):
    source = """\
    on (int n) = count of [items$]
        n = length of [items$]
    int i$ = [1, 2, 3]
    int c = count of [i$]"""
    _check(report, "array function call",
           source,
           lambda out: _assert_in("c: int = fn_count_of(i_arr)", out))


# --- array indexing ---

def test_emit_array_index(report):
    source = """\
    on (int r) = first of (int items$)
        r = items$[0]"""
    _check(report, "array index",
           source,
           lambda out: _assert_in("items_arr[0]", out))

def test_emit_slice_open_end(report):
    source = """\
    on (int result$) = rest of (int items$)
        result$ = items$[1:]"""
    _check(report, "slice open end",
           source,
           lambda out: _assert_in("items_arr[1:]", out))

def test_emit_slice_open_start(report):
    source = """\
    on (int result$) = first three of (int items$)
        result$ = items$[:3]"""
    _check(report, "slice open start",
           source,
           lambda out: _assert_in("items_arr[:3]", out))


# --- length of ---

def test_emit_length_of(report):
    source = """\
    on (int n) = size of (int items$)
        n = length of [items$]"""
    _check(report, "length of array",
           source,
           lambda out: _assert_in("n = len(items_arr)", out))


# --- conditional blocks ---

def test_emit_if_else(report):
    source = """\
    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else
            s = "zero" """
    _check(report, "if/else block",
           source,
           lambda out: _assert_in("if n > 0:", out),
           lambda out: _assert_in("else:", out))

def test_emit_if_else_if_else(report):
    source = """\
    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else if (n < 0)
            s = "negative"
        else
            s = "zero" """
    _check(report, "if/else if/else block",
           source,
           lambda out: _assert_in("if n > 0:", out),
           lambda out: _assert_in("elif n < 0:", out),
           lambda out: _assert_in("else:", out))


# --- range with variables ---

def test_emit_range_with_variable(report):
    source = """\
    on (int r) = sum up to (int n)
        int i$ = [1 through n]
        r = i$ + _"""
    _check(report, "range with variable endpoint",
           source,
           lambda out: _assert_in("i_arr = list(range(1, n + 1))", out))


# --- variable declarations in function body ---

def test_emit_body_var_decl(report):
    source = """\
    on (int r) = sum of (int a) and (int b)
        int c = a + b
        r = c"""
    _check(report, "var declaration in function body",
           source,
           lambda out: _assert_in("c = a + b", out))

def test_emit_body_array_decl(report):
    source = """\
    on (int r) = sum list
        int i$ = [1, 2, 3, 4]
        r = i$ + _"""
    _check(report, "array declaration in function body",
           source,
           lambda out: _assert_in("i_arr = [1, 2, 3, 4]", out))


# --- function calls in expressions ---

def test_emit_function_call_in_body(report):
    source = """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = quadruple (number x)
        n = double (double (x))"""
    _check(report, "function calling function",
           source,
           lambda out: _assert_in("n = fn_double__number(fn_double__number(x))", out))

def test_emit_function_call_multiword(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)"""
    _check(report, "multi-word function call",
           source,
           lambda out: _assert_in("fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)", out))


# --- enum values in expressions ---

def test_emit_enum_value_in_expression(report):
    source = """\
    type direction = north | south | east | west
    on (int dx) = x step for (direction d)
        dx = (1) if (d == east) else (0)"""
    _check(report, "enum value in expression",
           source,
           lambda out: _assert_in("d == direction.east", out))


# --- fn call in scalar variable ---

def test_emit_fn_call_in_scalar_var(report):
    source = """\
    type vector =
        number x, y, z = 0
    on (number n) = length of (vector v)
        n = v.x * v.x + v.y * v.y + v.z * v.z
    number d = length of (vector(1, 2, 3))"""
    _check(report, "fn call assigned to scalar variable",
           source,
           lambda out: _assert_in("d: float = fn_length_of__vector(vector(1, 2, 3))", out))


# --- tasks ---

def test_emit_task_filter(report):
    source = """\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)"""
    _check(report, "task: filter evens",
           source,
           lambda out: _assert_in("def task_only_evens_from__int(numbers_arr", out),
           lambda out: _assert_in("yield n", out),
           lambda out: _assert_in("even_arr = list(task_only_evens_from__int(all_arr))", out))


# --- end-to-end ---

def test_e2e_full_program(report):
    source = """\
    type int32 = ... 32-bit signed integer
    type float32 = ... 32-bit IEEE floating-point number
    type number = int | float
    type tri-state = no | yes | maybe
    type vector =
        number x, y, z = 0

    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""

    out = _translate(source)
    report.add("full program", source, out)

    expected = """\
from enum import Enum
from typing import NamedTuple

type int32 = int

type float32 = float

type number = int | float

class tri_state(Enum):
    no = "no"
    yes = "yes"
    maybe = "maybe"

class vector(NamedTuple):
    x: float = 0
    y: float = 0
    z: float = 0

# @zero on (vector v) = (vector a) + (vector b)
def fn__vector_plus__vector(a: vector, b: vector) -> vector:
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    return v

# @zero on (number n) = smaller of (number a) and (number b)
def fn_smaller_of__number_and__number(a: float, b: float) -> float:
    n = (a) if (a < b) else (b)
    return n
"""
    assert out == expected


# --- helpers ---

def _assert_in(needle, haystack):
    assert needle in haystack, f"Expected to find:\n  {needle}\nin:\n{haystack}"

def _assert_not_in(needle, haystack):
    assert needle not in haystack, f"Expected NOT to find:\n  {needle}\nin:\n{haystack}"
