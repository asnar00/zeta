"""Tests for the TypeScript emitter."""

from parser import process
from emit_typescript import emit


def _translate(source: str) -> str:
    return emit(process(source))


def _check(report, title, source, *assertions):
    out = _translate(source)
    report.add(title, source, out)
    for assertion in assertions:
        assertion(out)


def _assert_in(needle, haystack):
    assert needle in haystack, f"Expected to find:\n  {needle}\nin:\n{haystack}"

def _assert_not_in(needle, haystack):
    assert needle not in haystack, f"Expected NOT to find:\n  {needle}\nin:\n{haystack}"


# --- numeric types ---

def test_emit_int_maps_to_number(report):
    """int variables should use 'number' type in TypeScript."""
    _check(report, "ts: int maps to number",
           "    int i = 42",
           lambda out: _assert_in("const i: number", out))

def test_emit_concrete_int(report):
    _check(report, "ts: concrete int type",
           "    type int32 = ... 32-bit signed integer",
           lambda out: _assert_in("type int32 = number;", out))

def test_emit_concrete_float(report):
    _check(report, "ts: concrete float type",
           "    type float32 = ... 32-bit IEEE floating-point number",
           lambda out: _assert_in("type float32 = number;", out))

def test_emit_abstract_uint(report):
    _check(report, "ts: abstract uint type",
           "    type uint = ... any-size unsigned integer",
           lambda out: _assert_in("type uint = number;", out))

def test_emit_abstract_int(report):
    _check(report, "ts: abstract int type",
           "    type int = ... any-size signed integer",
           lambda out: _assert_in("type int = number;", out))

def test_emit_abstract_float(report):
    _check(report, "ts: abstract float type",
           "    type float = ... any-size IEEE floating-point number",
           lambda out: _assert_in("type float = number;", out))

def test_emit_builtin_number_skipped(report):
    _check(report, "ts: builtin number (no alias)",
           "    type number = int | float",
           lambda out: _assert_not_in("type number", out))

# --- enumerations ---

def test_emit_enum(report):
    _check(report, "ts: enumeration",
           "    type tri-state = no | yes | maybe",
           lambda out: _assert_in("enum tri_state {", out),
           lambda out: _assert_in('    no = "no",', out),
           lambda out: _assert_in('    yes = "yes",', out),
           lambda out: _assert_in('    maybe = "maybe",', out))


# --- structure types ---

def test_emit_struct(report):
    source = """\
    type vector =
        number x, y, z = 0"""
    _check(report, "ts: structure type",
           source,
           lambda out: _assert_in("interface vector {", out),
           lambda out: _assert_in("    readonly x: number;", out),
           lambda out: _assert_in("    readonly y: number;", out),
           lambda out: _assert_in("    readonly z: number;", out))

def test_emit_struct_factory(report):
    source = """\
    type vector =
        number x, y, z = 0"""
    _check(report, "ts: struct factory function",
           source,
           lambda out: _assert_in("function vector(args: Partial<vector> = {}): vector {", out),
           lambda out: _assert_in("return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };", out))


# --- struct defaults ---

def test_emit_nested_struct_factory(report):
    source = """\
    type vector =
        number x, y, z = 0
    type line =
        vector start, end"""
    _check(report, "ts: nested struct factory",
           source,
           lambda out: _assert_in("args.start ?? vector()", out),
           lambda out: _assert_in("args.end ?? vector()", out))


# --- struct with enum default ---

def test_emit_struct_enum_default(report):
    source = """\
    type direction = north | south | east | west
    type entity =
        direction facing = north"""
    _check(report, "ts: struct with enum default",
           source,
           lambda out: _assert_in("args.facing ?? direction.north", out))


# --- variables ---

def test_emit_scalar_variable(report):
    _check(report, "ts: scalar int variable",
           "    int32 i = 10",
           lambda out: _assert_in("const i: number = 10;", out))

def test_emit_float_variable(report):
    _check(report, "ts: scalar float variable",
           "    float f = 1.02",
           lambda out: _assert_in("const f: number = 1.02;", out))

def test_emit_string_variable(report):
    _check(report, "ts: string variable",
           '    string s = "hello"',
           lambda out: _assert_in('const s: string = "hello";', out))

def test_emit_struct_constructor(report):
    _check(report, "ts: struct constructor (positional)",
           "    vector v = vector(1, 2, 3)",
           lambda out: _assert_in("const v: vector = vector(", out))

def test_emit_struct_constructor_named(report):
    _check(report, "ts: struct constructor (named)",
           "    vector v = vector(z=2, x=1)",
           lambda out: _assert_in("const v: vector = vector({ z: 2, x: 1 });", out))


# --- arrays ---

def test_emit_array_empty(report):
    _check(report, "ts: empty array",
           "    int i$",
           lambda out: _assert_in("const i_arr: readonly number[] = [];", out))

def test_emit_array_sized(report):
    _check(report, "ts: sized array",
           "    int i$[4]",
           lambda out: _assert_in("const i_arr: readonly number[] = Array(4).fill(0);", out))

def test_emit_array_sized_fill(report):
    _check(report, "ts: sized array with fill",
           "    int i$[4] = 1",
           lambda out: _assert_in("const i_arr: readonly number[] = Array(4).fill(1);", out))

def test_emit_array_literal(report):
    _check(report, "ts: array literal",
           "    int i$ = [1, 2, 3, 4]",
           lambda out: _assert_in("const i_arr: readonly number[] = [1, 2, 3, 4];", out))

def test_emit_array_through(report):
    _check(report, "ts: array with through range",
           "    int i$ = [1 through 4]",
           lambda out: _assert_in("const i_arr: readonly number[] = Array.from({ length: 4 }, (_, i) => i + 1);", out))

def test_emit_array_to(report):
    _check(report, "ts: array with to range",
           "    int i$ = [0 to 4]",
           lambda out: _assert_in("const i_arr: readonly number[] = Array.from({ length: 4 }, (_, i) => i);", out))


# --- streaming ---

def test_emit_stream_simple(report):
    _check(report, "ts: simple stream",
           "    int i$ <- 1 <- 2 <- 3 <- 4",
           lambda out: _assert_in("const i_arr: number[] = [1, 2, 3, 4];", out))

def test_emit_stream_until(report):
    _check(report, "ts: stream with until",
           "    int i$ <- 1 <- (i$ + 1) until (i$ == 4)",
           lambda out: _assert_in("const i_arr: number[] = [1];", out),
           lambda out: _assert_in("while (!(i_arr[i_arr.length - 1] == 4))", out),
           lambda out: _assert_in("i_arr.push(i_arr[i_arr.length - 1] + 1)", out))

def test_emit_stream_while(report):
    _check(report, "ts: stream with while",
           "    int i$ <- 0 <- (i$ + 1) while (i$ < 4)",
           lambda out: _assert_in("const i_arr: number[] = [0];", out),
           lambda out: _assert_in("while (i_arr[i_arr.length - 1] < 4)", out),
           lambda out: _assert_in("i_arr.push(i_arr[i_arr.length - 1] + 1)", out))


# --- array mapping ---

def test_emit_array_map_scalar(report):
    source = """\
    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2"""
    _check(report, "ts: array map with scalar",
           source,
           lambda out: _assert_in("const j_arr: readonly number[] = i_arr.map(x => x * 2);", out))

def test_emit_array_map_two_arrays(report):
    source = """\
    int i$ = [1, 2, 3, 4, 5]
    int j$ = [1, 4, 7]
    int k$ = i$ + j$"""
    _check(report, "ts: array map with two arrays",
           source,
           lambda out: _assert_in("const k_arr: readonly number[] = Array.from({ length: Math.max(i_arr.length, j_arr.length) }, (_, i) => (i_arr[i] ?? 0) + (j_arr[i] ?? 0));", out))


# --- named function mapped over array ---

def test_emit_named_fn_map_single(report):
    source = """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)"""
    _check(report, "ts: named function mapped over array",
           source,
           lambda out: _assert_in("const j_arr: readonly number[] = i_arr.map(x => fn_double__number(x));", out))

def test_emit_named_fn_map_with_scalar(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)"""
    _check(report, "ts: named function map with scalar arg",
           source,
           lambda out: _assert_in("const j_arr: readonly number[] = i_arr.map(x => fn_smaller_of__number_and__number(x, 3));", out))


# --- array reduction ---

def test_emit_reduce_operator(report):
    source = """\
    int i$ = [1, 2, 3, 4]
    int sum = i$ + _"""
    _check(report, "ts: array reduce with operator",
           source,
           lambda out: _assert_in("const sum: number = i_arr.reduce((a, b) => a + b);", out))

def test_emit_reduce_named_function(report):
    source = """\
    int i$ = [1, 2, 3, 4]
    int min = smaller of (i$) and (_)"""
    _check(report, "ts: array reduce with named function",
           source,
           lambda out: _assert_in("const min: number = i_arr.reduce(fn_smaller_of__int_and__int);", out))


# --- concurrently ---

def test_emit_concurrently(report):
    source = """\
    on run()
        concurrently
            hello()
        and
            beep()"""
    _check(report, "ts: concurrently block",
           source,
           lambda out: _assert_in("await _concurrently(() => hello(), () => beep());", out),
           lambda out: _assert_in("async function _concurrently", out))

def test_emit_concurrently_function_is_async(report):
    """A function containing concurrently should be marked async."""
    source = """\
    on run()
        concurrently
            hello()
        and
            beep()"""
    _check(report, "ts: async function with concurrently",
           source,
           lambda out: _assert_in("async function fn_run()", out))

def test_emit_concurrently_caller_is_async(report):
    """A function calling an async function should also be async with await."""
    source = """\
    on run()
        concurrently
            hello()
        and
            beep()
    on main()
        run()"""
    _check(report, "ts: async propagation to caller",
           source,
           lambda out: _assert_in("async function fn_run()", out),
           lambda out: _assert_in("async function fn_main()", out),
           lambda out: _assert_in("await run()", out))

def test_emit_non_async_stays_sync(report):
    """A function with no concurrently and no async calls stays sync."""
    source = """\
    on (number n) = double (number x)
        n = x * 2"""
    _check(report, "ts: sync function stays sync",
           source,
           lambda out: _assert_not_in("async", out))


# --- functions ---

def test_emit_function_operator(report):
    source = """\
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)"""
    _check(report, "ts: operator function",
           source,
           lambda out: _assert_in("function fn__vector_plus__vector(a: vector, b: vector): vector {", out),
           lambda out: _assert_in("return v;", out))

def test_emit_function_named(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""
    _check(report, "ts: named function",
           source,
           lambda out: _assert_in("function fn_smaller_of__number_and__number(a: number, b: number): number {", out),
           lambda out: _assert_in("return n;", out))

def test_emit_ternary_syntax(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""
    _check(report, "ts: ternary syntax",
           source,
           lambda out: _assert_in("(a < b) ? (a) : (b)", out))


# --- function calls in expressions ---

def test_emit_function_call_in_body(report):
    source = """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = quadruple (number x)
        n = double (double (x))"""
    _check(report, "ts: function calling function",
           source,
           lambda out: _assert_in("fn_double__number(fn_double__number(x))", out))

def test_emit_function_call_multiword(report):
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)"""
    _check(report, "ts: multi-word function call",
           source,
           lambda out: _assert_in("fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)", out))


# --- tasks ---

def test_emit_task_filter(report):
    source = """\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)"""
    _check(report, "ts: task: filter evens",
           source,
           lambda out: _assert_in("function* fn_only_evens_from__int(numbers_arr", out),
           lambda out: _assert_in("yield n;", out),
           lambda out: _assert_in("even_arr = [...fn_only_evens_from__int(all_arr)]", out))


# --- type composition ---

def test_emit_subtype_flattened(report):
    source = """\
    type animal
        string name = ""
    type dog = animal +
        string breed = "unknown" """
    _check(report, "ts: subtype flattens parent fields",
           source,
           lambda out: _assert_in("readonly name: string;", out),
           lambda out: _assert_in("readonly breed: string;", out))


# --- where ---

def test_emit_where(report):
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)"""
    _check(report, "ts: where filter",
           source,
           lambda out: _assert_in("i_arr.filter(x => x % 2 == 0)", out))

def test_emit_first_of_where(report):
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int first_even = first of [i$] where (_ % 2 == 0)"""
    _check(report, "ts: first of where",
           source,
           lambda out: _assert_in("i_arr.find(x => x % 2 == 0)", out))


# --- sort ---

def test_emit_sort_simple(report):
    source = """\
    int i$ = [3, 1, 4]
    int sorted$ = sort [i$]"""
    _check(report, "ts: sort simple",
           source,
           lambda out: _assert_in("[...i_arr].sort()", out))

def test_emit_sort_by(report):
    source = """\
    type person =
        int age = 0
    person people$ = [...]
    person sorted$ = sort [people$] by (_.age)"""
    _check(report, "ts: sort by key",
           source,
           lambda out: _assert_in("[...people_arr].sort((a, b) => a.age - b.age)", out))


# --- array function calls ---

def test_emit_array_fn_call(report):
    source = """\
    on (int n) = count of [items$]
        n = length of [items$]
    int i$ = [1, 2, 3]
    int c = count of [i$]"""
    _check(report, "ts: array function call",
           source,
           lambda out: _assert_in("fn_count_of(i_arr)", out))


# --- multiple dispatch ---

def test_emit_multiple_dispatch(report):
    source = """\
    type animal
    type dog = animal +
        string breed = "unknown"
    on (string s) = describe (animal a)
        s = "an animal"
    on (string s) = describe (dog d)
        s = "a dog" """
    _check(report, "ts: multiple dispatch",
           source,
           lambda out: _assert_in("function fn_describe__animal(a: animal): string {", out),
           lambda out: _assert_in("function fn_describe__dog(d: dog): string {", out),
           lambda out: _assert_in("function fn_describe(", out),
           lambda out: _assert_in("instanceof", out))


# --- end-to-end ---

def test_e2e_full_program(report):
    source = """\
    type number = int | float
    type vector =
        number x, y, z = 0

    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""

    out = _translate(source)
    report.add("ts: full program", source, out)

    expected = """\
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

function fn__vector_plus__vector(a: vector, b: vector): vector {
    const v: vector = vector({ x: a.x + b.x, y: a.y + b.y, z: a.z + b.z });
    return v;
}

function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
"""
    assert out == expected
