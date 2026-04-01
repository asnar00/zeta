"""Execution tests: verify that emitted Python code actually runs correctly."""

from parser import process
from emit_python import emit


def _run(source: str, expression: str):
    """Compile zero source to Python, execute it, and evaluate an expression."""
    ir = process(source)
    code = emit(ir)
    env = {}
    exec(code, env)
    return eval(expression, env)


# --- basic types and variables ---

def test_exec_int_variable():
    assert _run("    int i = 10", "i") == 10

def test_exec_float_variable():
    assert _run("    float f = 1.5", "f") == 1.5

def test_exec_string_variable():
    assert _run('    string s = "hello"', "s") == "hello"


# --- structs ---

def test_exec_struct_constructor():
    source = """\
    type vector =
        int x, y, z = 0
    vector v = vector(1, 2, 3)"""
    assert _run(source, "v.x") == 1
    assert _run(source, "v.z") == 3

def test_exec_struct_defaults():
    source = """\
    type vector =
        int x, y, z = 0
    vector v = vector()"""
    assert _run(source, "v.x") == 0


# --- functions ---

def test_exec_pure_function():
    source = """\
    on (int n) = double (int x)
        n = x * 2"""
    assert _run(source, "fn_double__int(5)") == 10

def test_exec_ternary():
    source = """\
    on (int n) = smaller of (int a) and (int b)
        n = (a) if (a < b) else (b)"""
    assert _run(source, "fn_smaller_of__int_and__int(3, 7)") == 3
    assert _run(source, "fn_smaller_of__int_and__int(9, 2)") == 2

def test_exec_recursion():
    source = """\
    on (int n) = factorial of (int x)
        n = (1) if (x == 1) else (x * factorial of (x - 1))"""
    assert _run(source, "fn_factorial_of__int(5)") == 120

def test_exec_function_calling_function():
    source = """\
    on (int n) = double (int x)
        n = x * 2
    on (int n) = quadruple (int x)
        n = double (double (x))"""
    assert _run(source, "fn_quadruple__int(3)") == 12


# --- arrays ---

def test_exec_array_literal():
    source = "    int i$ = [1, 2, 3, 4]"
    assert _run(source, "i_arr") == [1, 2, 3, 4]

def test_exec_array_range_through():
    source = "    int i$ = [1 through 4]"
    assert _run(source, "i_arr") == [1, 2, 3, 4]

def test_exec_array_range_to():
    source = "    int i$ = [0 to 4]"
    assert _run(source, "i_arr") == [0, 1, 2, 3]

def test_exec_array_map():
    source = """\
    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2"""
    assert _run(source, "j_arr") == [2, 4, 6, 8]

def test_exec_array_reduce():
    source = """\
    int i$ = [1, 2, 3, 4]
    int sum = i$ + _"""
    assert _run(source, "sum") == 10

def test_exec_array_map_two_arrays():
    source = """\
    int i$ = [1, 2, 3]
    int j$ = [10, 20, 30]
    int k$ = i$ + j$"""
    assert _run(source, "k_arr") == [11, 22, 33]


# --- streaming ---

def test_exec_stream_simple():
    source = "    int i$ <- 1 <- 2 <- 3"
    assert _run(source, "i_arr") == [1, 2, 3]

def test_exec_stream_until():
    source = "    int i$ <- 1 <- (i$ + 1) until (i$ == 5)"
    assert _run(source, "i_arr") == [1, 2, 3, 4, 5]

def test_exec_stream_while():
    source = "    int i$ <- 0 <- (i$ + 1) while (i$ < 4)"
    assert _run(source, "i_arr") == [0, 1, 2, 3, 4]


# --- conditionals ---

def test_exec_if_else():
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

def test_exec_array_index():
    source = """\
    int i$ = [10, 20, 30, 40]
    int first = i$[0]
    int last = i$[3]"""
    assert _run(source, "first") == 10
    assert _run(source, "last") == 40

def test_exec_array_slice():
    source = """\
    int i$ = [10, 20, 30, 40, 50]
    int mid$ = i$[1 to 4]"""
    assert _run(source, "mid_arr") == [20, 30, 40]

def test_exec_array_slice_through():
    source = """\
    int i$ = [10, 20, 30, 40, 50]
    int mid$ = i$[1 through 3]"""
    assert _run(source, "mid_arr") == [20, 30, 40]

def test_exec_array_slice_onwards():
    source = """\
    int i$ = [10, 20, 30, 40, 50]
    int tail$ = i$[2 onwards]"""
    assert _run(source, "tail_arr") == [30, 40, 50]

def test_exec_string_slice_onwards():
    source = """\
    string s = "hello world"
    string tail = s[6 onwards]"""
    assert _run(source, "tail") == "world"


# --- index of first ---

def test_exec_index_of_first():
    source = """\
    int i$ = [10, 20, 30, 40, 50]
    int pos = index of first in [i$] where (_ > 25)"""
    assert _run(source, "pos") == 2

def test_exec_index_of_first_at_start():
    source = """\
    int i$ = [5, 1, 2, 3]
    int pos = index of first in [i$] where (_ == 5)"""
    assert _run(source, "pos") == 0


# --- where ---

def test_exec_where():
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)"""
    assert _run(source, "evens_arr") == [2, 4, 6]


# --- sort ---

def test_exec_sort():
    source = """\
    int i$ = [3, 1, 4, 1, 5, 9]
    int sorted$ = sort [i$]"""
    assert _run(source, "sorted_arr") == [1, 1, 3, 4, 5, 9]


# --- type composition and multiple dispatch ---

def test_exec_type_composition():
    source = """\
    type animal
        string name = ""
    type dog = animal +
        string breed = "unknown"
    dog d = dog(name="Rex", breed="labrador")"""
    assert _run(source, "d.name") == "Rex"
    assert _run(source, "d.breed") == "labrador"

def test_exec_multiple_dispatch():
    source = """\
    type animal
        string name = ""
    type dog = animal +
        string breed = "unknown"
    on (string s) = describe (animal a)
        s = "animal: " + a.name
    on (string s) = describe (dog d)
        s = "dog: " + d.breed"""
    assert _run(source, 'fn_describe(animal(name="Rex"))') == "animal: Rex"
    assert _run(source, 'fn_describe(dog(name="Rex", breed="lab"))') == "dog: lab"


# --- tasks ---

def test_exec_task_filter():
    source = """\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)"""
    assert _run(source, "even_arr") == [2, 4, 6]


# --- enums ---

def test_exec_enum():
    source = """\
    type colour = red | green | blue"""
    assert _run(source, "colour.red.value") == "red"
    assert _run(source, "colour.green.value") == "green"

def test_exec_enum_in_expression():
    source = """\
    type direction = north | south | east | west
    on (int dx) = x step for (direction d)
        dx = (1) if (d == direction.east) else (0)"""
    assert _run(source, "fn_x_step_for__direction(direction.east)") == 1
    assert _run(source, "fn_x_step_for__direction(direction.north)") == 0

def test_exec_struct_with_enum_default():
    source = """\
    type direction = north | south | east | west
    type entity =
        direction facing = north
        int x, y = 0"""
    assert _run(source, "entity().facing.value") == "north"


# --- named function map over array ---

def test_exec_named_fn_map():
    source = """\
    on (int n) = double (int x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)"""
    assert _run(source, "j_arr") == [2, 4, 6, 8]

def test_exec_named_fn_map_with_scalar():
    source = """\
    on (int n) = smaller of (int a) and (int b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)"""
    assert _run(source, "j_arr") == [3, 1, 3, 1, 3]


# --- named function reduce ---

def test_exec_named_fn_reduce():
    source = """\
    on (int n) = smaller of (int a) and (int b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int min = smaller of (i$) and (_)"""
    assert _run(source, "min") == 1


# --- void functions ---

def test_exec_void_function():
    source = """\
    on greet (string name)
        x = name"""
    # just verify it compiles and the function exists
    env = {}
    from parser import process as p
    from emit_python import emit as e
    exec(e(p(source)), env)
    assert "fn_greet__str" in env or "fn_greet__string" in env


# --- bitwise operators ---

def test_exec_bitwise_and():
    source = """\
    on (int r) = low nibble of (int x)
        r = x & 15"""
    assert _run(source, "fn_low_nibble_of__int(0xAB)") == 0x0B

def test_exec_bitwise_or():
    source = """\
    on (int r) = combine (int a) with (int b)
        r = a | b"""
    assert _run(source, "fn_combine__int_with__int(0x0F, 0xF0)") == 0xFF

def test_exec_shift():
    source = """\
    on (int r) = shift left (int x) by (int n)
        r = x << n"""
    assert _run(source, "fn_shift_left__int_by__int(1, 4)") == 16


# --- array functions ---

def test_exec_length_of():
    source = """\
    int i$ = [10, 20, 30]
    int n = length of [i$]"""
    assert _run(source, "n") == 3

def test_exec_length_of_empty():
    source = """\
    int i$
    int n = length of [i$]"""
    assert _run(source, "n") == 0


# --- array self-map (same array twice) ---

def test_exec_self_map():
    source = """\
    int i$ = [1, 2, 3, 4]
    int sq$ = i$ * i$"""
    assert _run(source, "sq_arr") == [1, 4, 9, 16]


# --- streaming self-reference ---

def test_exec_stream_doubling():
    """Build doubling sequence using streaming."""
    source = """\
    int dbl$ <- 1 <- (dbl$ * 2) until (dbl$ > 100)"""
    result = _run(source, "dbl_arr")
    assert result == [1, 2, 4, 8, 16, 32, 64, 128]


# --- chained operations ---

def test_exec_map_then_reduce():
    source = """\
    on (int n) = double (int x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int doubled$ = double (i$)
    int sum = doubled$ + _"""
    assert _run(source, "sum") == 20

def test_exec_reduce_in_function_body():
    source = """\
    on (int r) = sum squares of (int a) and (int b) and (int c)
        int vals$ = [a, b, c]
        int sq$ = vals$ * vals$
        r = sq$ + _"""
    assert _run(source, "fn_sum_squares_of__int_and__int_and__int(3, 4, 5)") == 50

# --- 3-step streaming ---

def test_exec_3step_stream_no_terminator():
    """3 literal steps with no loop: just three values."""
    source = "    int i$ <- 10 <- 20 <- 30"
    assert _run(source, "i_arr") == [10, 20, 30]

def test_exec_3step_stream_with_until():
    """Two seeds then a doubling repeat with terminator."""
    source = "    int i$ <- 0 <- 1 <- (i$ + i$) until (i$ > 50)"
    result = _run(source, "i_arr")
    assert result[0] == 0
    assert result[1] == 1
    assert result == [0, 1, 2, 4, 8, 16, 32, 64]

def test_exec_3step_stream_with_while():
    """Two seeds then a repeat with while terminator."""
    source = "    int i$ <- 5 <- 10 <- (i$ + 1) while (i$ < 14)"
    result = _run(source, "i_arr")
    assert result[:2] == [5, 10]
    assert result == [5, 10, 11, 12, 13, 14]

def test_exec_4step_stream_no_terminator():
    """4 literal steps with no loop."""
    source = "    int i$ <- 1 <- 2 <- 3 <- 4"
    assert _run(source, "i_arr") == [1, 2, 3, 4]

def test_exec_map_store_retrieve():
    """Keyed collection: store a value by key, retrieve it."""
    source = """\
    on (string result) = test map ()
        string codes$[string]
        codes$["phone1"] = "1234"
        codes$["phone2"] = "5678"
        result = codes$["phone1"]
    """
    assert _run(source, "fn_test_map()") == "1234"

def test_exec_map_overwrite():
    """Keyed collection: overwriting a key."""
    source = """\
    on (string result) = test map ()
        string codes$[string]
        codes$["key"] = "old"
        codes$["key"] = "new"
        result = codes$["key"]
    """
    assert _run(source, "fn_test_map()") == "new"

def test_exec_map_variable_key():
    """Keyed collection: using a variable as the key."""
    source = """\
    on (string result) = test map (string key)
        string data$[string]
        data$[key] = "found"
        result = data$[key]
    """
    assert _run(source, 'fn_test_map__string("hello")') == "found"

def test_exec_sort_descending():
    source = """\
    type person =
        string name = ""
        int age = 0
    person people$ = [person(name="alice", age=30), person(name="bob", age=20), person(name="carol", age=40)]
    person sorted$ = sort [people$] by (_.age)"""
    result = _run(source, "[p.age for p in sorted_arr]")
    assert result == [20, 30, 40]
