"""Tests for the zero processor."""

import pytest
from parser import process, ZeroParseError


# --- numeric types ---

def test_concrete_int_type():
    ir = process("    type int32 = ... 32-bit signed integer")
    assert ir["types"] == [
        {"kind": "numeric", "name": "int32", "base": "int", "size": 32}
    ]

def test_concrete_uint_type():
    ir = process("    type uint8 = ... 8-bit unsigned integer")
    assert ir["types"] == [
        {"kind": "numeric", "name": "uint8", "base": "int", "size": 8}
    ]

def test_concrete_float_type():
    ir = process("    type float32 = ... 32-bit IEEE floating-point number")
    assert ir["types"] == [
        {"kind": "numeric", "name": "float32", "base": "float", "size": 32}
    ]

def test_abstract_int_type():
    ir = process("    type int = ... any-size signed integer")
    assert ir["types"] == [
        {"kind": "numeric", "name": "int", "base": "int", "size": None}
    ]

def test_abstract_uint_type():
    ir = process("    type uint = ... any-size unsigned integer")
    assert ir["types"] == [
        {"kind": "numeric", "name": "uint", "base": "int", "size": None}
    ]

def test_abstract_float_type():
    ir = process("    type float = ... any-size IEEE floating-point number")
    assert ir["types"] == [
        {"kind": "numeric", "name": "float", "base": "float", "size": None}
    ]

def test_union_type():
    ir = process("    type number = int | float")
    assert ir["types"] == [
        {"kind": "numeric", "name": "number", "base": "int | float", "size": None}
    ]

# --- enumerations ---

def test_enum_type():
    ir = process("    type tri-state = no | yes | maybe")
    assert ir["types"] == [
        {"kind": "enum", "name": "tri-state", "values": ["no", "yes", "maybe"]}
    ]

# --- structure types ---

# --- type composition ---

def test_abstract_base_type():
    """A type with no fields is abstract."""
    ir = process("    type animal")
    assert ir["types"] == [
        {"kind": "struct", "name": "animal", "fields": [], "parents": []}
    ]

def test_subtype():
    """type dog = animal + fields."""
    source = """\
    type animal
    type dog = animal +
        string breed = "unknown" """
    ir = process(source)
    assert len(ir["types"]) == 2
    dog = ir["types"][1]
    assert dog["name"] == "dog"
    assert dog["parents"] == ["animal"]
    assert len(dog["fields"]) == 1
    assert dog["fields"][0]["name"] == "breed"

def test_multi_composition():
    """type pet = animal + named + fields."""
    source = """\
    type animal
    type named
        string name = ""
    type pet = animal + named +
        string owner = "" """
    ir = process(source)
    pet = ir["types"][2]
    assert pet["name"] == "pet"
    assert pet["parents"] == ["animal", "named"]
    assert pet["fields"][0]["name"] == "owner"


# --- structure types ---

def test_struct_type():
    source = """\
    type vector =
        number x, y, z = 0"""
    ir = process(source)
    assert ir["types"] == [
        {"kind": "struct", "name": "vector", "parents": [], "fields": [
            {"name": "x", "type": "number", "default": 0},
            {"name": "y", "type": "number", "default": 0},
            {"name": "z", "type": "number", "default": 0},
        ]}
    ]


# --- variables ---

def test_simple_int_variable():
    ir = process("    int32 i = 10")
    assert ir["variables"] == [
        {"name": "i", "type": "int32", "array": False, "value": 10}
    ]

def test_simple_float_variable():
    ir = process("    float f = 1.02")
    assert ir["variables"] == [
        {"name": "f", "type": "float", "array": False, "value": 1.02}
    ]

def test_struct_constructor_empty():
    ir = process("    vector v = vector()")
    assert ir["variables"] == [
        {"name": "v", "type": "vector", "array": False,
         "value": {"kind": "call", "name": "vector", "args": []}}
    ]

def test_struct_constructor_positional():
    ir = process("    vector v = vector(1, 2, 3)")
    assert ir["variables"] == [
        {"name": "v", "type": "vector", "array": False,
         "value": {"kind": "call", "name": "vector", "args": [1, 2, 3]}}
    ]

def test_struct_constructor_named():
    ir = process("    vector v = vector(z=2, x=1)")
    assert ir["variables"] == [
        {"name": "v", "type": "vector", "array": False,
         "value": {"kind": "call", "name": "vector",
                   "args": [{"name": "z", "value": 2}, {"name": "x", "value": 1}]}}
    ]


# --- arrays ---

def test_array_empty():
    ir = process("    int i$")
    assert ir["variables"] == [
        {"name": "i", "type": "int", "array": True, "size": None, "value": None}
    ]

def test_array_sized():
    ir = process("    int i$[4]")
    assert ir["variables"] == [
        {"name": "i", "type": "int", "array": True, "size": 4, "value": None}
    ]

def test_array_sized_with_fill():
    ir = process("    int i$[4] = 1")
    assert ir["variables"] == [
        {"name": "i", "type": "int", "array": True, "size": 4, "value": 1}
    ]

def test_array_literal():
    ir = process("    int i$ = [1, 2, 3, 4]")
    assert ir["variables"] == [
        {"name": "i", "type": "int", "array": True, "size": None,
         "value": [1, 2, 3, 4]}
    ]

def test_array_through_range():
    ir = process("    int i$ = [1 through 4]")
    assert ir["variables"] == [
        {"name": "i", "type": "int", "array": True, "size": None,
         "value": {"range": "through", "start": 1, "end": 4}}
    ]

def test_array_to_range():
    ir = process("    int i$ = [0 to 4]")
    assert ir["variables"] == [
        {"name": "i", "type": "int", "array": True, "size": None,
         "value": {"range": "to", "start": 0, "end": 4}}
    ]


# --- functions ---

def test_function_operator():
    source = """\
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)"""
    ir = process(source)
    assert len(ir["functions"]) == 1
    fn = ir["functions"][0]
    assert fn["result"] == {"name": "v", "type": "vector"}
    assert fn["params"] == [
        {"name": "a", "type": "vector"},
        {"name": "b", "type": "vector"},
    ]
    assert fn["signature_parts"] == ["(vector a)", "+", "(vector b)"]

def test_function_named():
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["result"] == {"name": "n", "type": "number"}
    assert fn["params"] == [
        {"name": "a", "type": "number"},
        {"name": "b", "type": "number"},
    ]
    assert fn["signature_parts"] == ["smaller", "of", "(number a)", "and", "(number b)"]

def test_function_body_ast_assign():
    source = """\
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)"""
    ir = process(source)
    fn = ir["functions"][0]
    assert len(fn["body"]) == 1
    stmt = fn["body"][0]
    assert stmt["kind"] == "assign"
    assert stmt["target"] == "v"
    # the value should be a constructor call
    assert stmt["value"]["kind"] == "call"
    assert stmt["value"]["name"] == "vector"
    assert len(stmt["value"]["args"]) == 3

def test_function_body_ast_binop():
    source = """\
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)"""
    ir = process(source)
    fn = ir["functions"][0]
    arg0 = fn["body"][0]["value"]["args"][0]
    assert arg0["kind"] == "binop"
    assert arg0["op"] == "+"
    assert arg0["left"] == {"kind": "member", "object": "a", "field": "x"}
    assert arg0["right"] == {"kind": "member", "object": "b", "field": "x"}

def test_function_body_ast_ternary():
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    assert stmt["kind"] == "assign"
    assert stmt["target"] == "n"
    val = stmt["value"]
    assert val["kind"] == "ternary"
    assert val["true"] == {"kind": "name", "value": "a"}
    assert val["false"] == {"kind": "name", "value": "b"}
    assert val["condition"]["kind"] == "binop"
    assert val["condition"]["op"] == "<"


# --- bitwise operators ---

def test_function_body_bitwise_and():
    source = """\
    on (uint8 r) = (uint8 a) & (uint8 b)
        r = a & b"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    assert stmt["value"]["kind"] == "binop"
    assert stmt["value"]["op"] == "&"

def test_function_body_bitwise_or():
    source = """\
    on (uint8 r) = (uint8 a) | (uint8 b)
        r = a | b"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    assert stmt["value"]["kind"] == "binop"
    assert stmt["value"]["op"] == "|"

def test_function_body_bitwise_xor():
    source = """\
    on (uint8 r) = (uint8 a) ^ (uint8 b)
        r = a ^ b"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    assert stmt["value"]["kind"] == "binop"
    assert stmt["value"]["op"] == "^"

def test_function_body_shift_left():
    source = """\
    on (uint8 r) = (uint8 a) << (uint8 b)
        r = a << b"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    assert stmt["value"]["kind"] == "binop"
    assert stmt["value"]["op"] == "<<"

def test_function_body_shift_right():
    source = """\
    on (uint8 r) = (uint8 a) >> (uint8 b)
        r = a >> b"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    assert stmt["value"]["kind"] == "binop"
    assert stmt["value"]["op"] == ">>"

def test_bitwise_precedence():
    """& should bind tighter than |"""
    source = """\
    on (uint8 r) = mask (uint8 a) with (uint8 b) or (uint8 c)
        r = a & b | c"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    # should parse as (a & b) | c
    val = stmt["value"]
    assert val["kind"] == "binop"
    assert val["op"] == "|"
    assert val["left"]["kind"] == "binop"
    assert val["left"]["op"] == "&"


# --- array mapping ---

def test_array_map_scalar():
    """i$ * 2 should parse as an array variable with a mapped binop."""
    source = """\
    int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2"""
    ir = process(source)
    j = ir["variables"][1]
    assert j["name"] == "j"
    assert j["array"] is True
    val = j["value"]
    assert val["kind"] == "binop"
    assert val["op"] == "*"
    assert val["left"] == {"kind": "name", "value": "i$"}
    assert val["right"] == {"kind": "literal", "value": 2}

def test_array_map_two_arrays():
    """i$ + j$ should parse as a binop with two array references."""
    source = """\
    int i$ = [1, 2, 3, 4, 5]
    int j$ = [1, 4, 7]
    int k$ = i$ + j$"""
    ir = process(source)
    k = ir["variables"][2]
    assert k["name"] == "k"
    assert k["array"] is True
    val = k["value"]
    assert val["kind"] == "binop"
    assert val["op"] == "+"
    assert val["left"] == {"kind": "name", "value": "i$"}
    assert val["right"] == {"kind": "name", "value": "j$"}


# --- array reduction ---

def test_array_reduce_operator():
    """i$ + _ should parse as a reduce."""
    source = """\
    int i$ = [1, 2, 3, 4]
    int sum = i$ + _"""
    ir = process(source)
    s = ir["variables"][1]
    assert s["name"] == "sum"
    assert s["array"] is False
    val = s["value"]
    assert val["kind"] == "reduce"
    assert val["op"] == "+"
    assert val["array"] == "i$"

def test_array_reduce_named_function():
    """smaller of (i$) and (_) should parse as a reduce."""
    source = """\
    int i$ = [1, 2, 3, 4]
    int min = smaller of (i$) and (_)"""
    ir = process(source)
    m = ir["variables"][1]
    assert m["name"] == "min"
    assert m["array"] is False
    val = m["value"]
    assert val["kind"] == "reduce"
    assert val["array"] == "i$"
    assert val["fn_parts"] == ["smaller", "of", "(i$)", "and", "(_)"]


# --- function calls in variable assignments ---

def test_variable_with_fn_call():
    """A scalar variable assigned from a function call."""
    source = """\
    on (number n) = double (number x)
        n = x * 2
    number y = double (5)"""
    ir = process(source)
    var = ir["variables"][0]
    assert var["name"] == "y"
    assert var["value"]["kind"] == "fn_call"
    assert var["value"]["args"][0] == {"kind": "literal", "value": 5}

def test_variable_with_multiword_fn_call():
    """A scalar variable assigned from a multi-word function call."""
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    number m = smaller of (3) and (7)"""
    ir = process(source)
    var = ir["variables"][0]
    assert var["value"]["kind"] == "fn_call"
    assert var["value"]["args"][0] == {"kind": "literal", "value": 3}
    assert var["value"]["args"][1] == {"kind": "literal", "value": 7}


# --- struct implicit defaults ---

def test_struct_fields_without_defaults_get_zero():
    """Fields without explicit defaults should default to 0."""
    source = """\
    type pair =
        int first
        int second"""
    ir = process(source)
    fields = ir["types"][0]["fields"]
    assert fields[0]["default"] == 0
    assert fields[1]["default"] == 0

def test_struct_mixed_defaults():
    """Fields without defaults get 0, fields with defaults keep theirs."""
    source = """\
    type config =
        int width = 800
        int height = 600
        float scale"""
    ir = process(source)
    fields = ir["types"][0]["fields"]
    assert fields[0]["default"] == 800
    assert fields[1]["default"] == 600
    assert fields[2]["default"] == 0


# --- struct followed by variable ---

def test_struct_followed_by_array_variable():
    """A variable after a struct should not be consumed as a field."""
    source = """\
    type vector =
        number x, y, z = 0
    vector v$[3]"""
    ir = process(source)
    assert len(ir["types"]) == 1
    assert ir["types"][0]["name"] == "vector"
    assert len(ir["types"][0]["fields"]) == 3
    assert len(ir["variables"]) == 1
    assert ir["variables"][0]["name"] == "v"
    assert ir["variables"][0]["array"] is True


# --- SSA violation ---

def test_error_ssa_violation():
    """Multiple assignments to the same variable should be an error."""
    source = """\
    on (int r) = bad (int a)
        r = a + 1
        r = a + 2"""
    with pytest.raises(ZeroParseError) as exc_info:
        process(source)
    err = exc_info.value
    assert "already assigned" in str(err)


# --- conditional blocks ---

def test_if_else_block():
    """Parse if/else block in function body."""
    source = """\
    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else
            s = "zero" """
    ir = process(source)
    fn = ir["functions"][0]
    assert len(fn["body"]) == 1
    cond = fn["body"][0]
    assert cond["kind"] == "if_block"
    assert len(cond["branches"]) == 2
    assert cond["branches"][0]["condition"] is not None  # if
    assert cond["branches"][1]["condition"] is None  # else

def test_if_else_if_else_block():
    """Parse if/else if/else chain."""
    source = """\
    on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else if (n < 0)
            s = "negative"
        else
            s = "zero" """
    ir = process(source)
    fn = ir["functions"][0]
    cond = fn["body"][0]
    assert cond["kind"] == "if_block"
    assert len(cond["branches"]) == 3
    assert cond["branches"][0]["condition"]["op"] == ">"
    assert cond["branches"][1]["condition"]["op"] == "<"
    assert cond["branches"][2]["condition"] is None


# --- tasks ---

def test_task_definition():
    """Parse a task definition with stream input and output."""
    source = """\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n"""
    ir = process(source)
    assert len(ir["tasks"]) == 1
    task = ir["tasks"][0]
    assert task["name_parts"] == ["only", "evens", "from"]
    assert task["output"] == {"name": "even", "type": "int"}
    assert task["input_streams"] == [{"name": "numbers", "type": "int"}]
    assert len(task["body"]) > 0

def test_task_with_scalar_param():
    """A task can take scalar params alongside stream inputs."""
    source = """\
    on (int i$) <- count down from (int n)
        i$ <- n <- (i$ - 1) while (i$ > 1)"""
    ir = process(source)
    task = ir["tasks"][0]
    assert task["name_parts"] == ["count", "down", "from"]
    assert task["output"] == {"name": "i", "type": "int"}
    assert task["params"] == [{"name": "n", "type": "int"}]

def test_task_call():
    """A task call assigns a stream result."""
    source = """\
    on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)"""
    ir = process(source)
    assert len(ir["variables"]) == 2
    var = ir["variables"][1]
    assert var["name"] == "even"
    assert var["array"] is True
    assert var["value"]["kind"] == "task_call"


# --- filtering arrays (where) ---

def test_where_filter():
    """[a$] where (cond) filters an array."""
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)"""
    ir = process(source)
    var = ir["variables"][1]
    assert var["name"] == "evens"
    assert var["array"] is True
    val = var["value"]
    assert val["kind"] == "where"
    assert val["array"]["value"] == "i$"
    assert val["condition"]["kind"] == "binop"

def test_first_of_where():
    """first of [a$] where (cond) returns a scalar."""
    source = """\
    int i$ = [1, 2, 3, 4, 5, 6]
    int first_even = first of [i$] where (_ % 2 == 0)"""
    ir = process(source)
    var = ir["variables"][1]
    assert var["name"] == "first_even"
    assert var["array"] is False
    assert var["value"]["kind"] == "first_where"


# --- sorting ---

def test_sort_simple():
    """sort [a$] sorts an array."""
    source = """\
    int i$ = [3, 1, 4, 1, 5]
    int sorted$ = sort [i$]"""
    ir = process(source)
    var = ir["variables"][1]
    assert var["value"]["kind"] == "sort"
    assert var["value"]["key"] is None

def test_sort_by():
    """sort [a$] by (_.field) sorts by a key expression."""
    source = """\
    type person =
        int age = 0
    person people$ = [...]
    person sorted$ = sort [people$] by (_.age)"""
    ir = process(source)
    var = ir["variables"][1]
    assert var["value"]["kind"] == "sort"
    assert var["value"]["key"] is not None


# --- array slicing ---

def test_slice_to():
    """a$[1 to 3] slices exclusive end."""
    source = """\
    on (int result$) = first three of (int items$)
        result$ = items$[0 to 3]"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][0]
    val = stmt["value"]
    assert val["kind"] == "slice"
    assert val["start"]["value"] == 0
    assert val["end"]["value"] == 3
    assert val["inclusive"] is False

def test_slice_through():
    """a$[1 through 3] slices inclusive end."""
    source = """\
    on (int result$) = mid of (int items$)
        result$ = items$[1 through 3]"""
    ir = process(source)
    fn = ir["functions"][0]
    val = fn["body"][0]["value"]
    assert val["kind"] == "slice"
    assert val["inclusive"] is True

def test_slice_with_variable():
    """Slice endpoints can be variables."""
    source = """\
    on (int result$) = take (int n) from (int items$)
        result$ = items$[0 to n]"""
    ir = process(source)
    fn = ir["functions"][0]
    val = fn["body"][0]["value"]
    assert val["kind"] == "slice"
    assert val["end"]["kind"] == "name"
    assert val["end"]["value"] == "n"


# --- multiple dispatch ---

def test_multiple_dispatch_parsed():
    """Multiple definitions of the same function with different types."""
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
    ir = process(source)
    # should have 3 functions all named "describe"
    fns = ir["functions"]
    assert len(fns) == 3
    # all have different param types
    types = {fn["params"][0]["type"] for fn in fns}
    assert types == {"animal", "dog", "cat"}


# --- recursion ---

def test_recursive_function_parses():
    """A function that calls itself should parse."""
    source = """\
    on (int n) = factorial of (int x)
        n = (1) if (x == 1) else (x * factorial of (x - 1))"""
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["result"] == {"name": "n", "type": "int"}
    val = fn["body"][0]["value"]
    assert val["kind"] == "ternary"
    # the else branch should contain a fn_call to factorial
    false_branch = val["false"]
    assert false_branch["kind"] == "binop"
    assert false_branch["op"] == "*"
    assert false_branch["right"]["kind"] == "fn_call"


# --- array function definitions and calls ---

def test_array_fn_definition():
    """Parse an array function with untyped (generic) params."""
    source = """\
    on (bool result) = [items$] starts with [prefix$]
        result = (items$[0 to length of [prefix$]] == prefix$) & _"""
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["result"] == {"name": "result", "type": "bool"}
    # signature_parts should reflect [] params
    assert "(items$)" in str(fn["signature_parts"]) or "items$" in str(fn["signature_parts"])

def test_array_fn_definition_generic_params():
    """Array function params without types are generic."""
    source = """\
    on (int n) = length of [items$]
        n = 0"""
    ir = process(source)
    fn = ir["functions"][0]
    # items$ has no type — it's generic
    assert any(p["name"] == "items$" and p["type"] is None for p in fn["params"])

def test_array_fn_call():
    """Call a user-defined array function with [...] syntax."""
    source = """\
    on (bool result) = [items$] starts with [prefix$]
        result = (items$[0 to length of [prefix$]] == prefix$) & _
    int i$ = [1, 2, 3, 4]
    bool b = [i$] starts with [1, 2]"""
    ir = process(source)
    var = ir["variables"][1]
    assert var["name"] == "b"
    assert var["value"]["kind"] == "array_fn_call"
    assert len(var["value"]["args"]) == 2

def test_array_fn_call_in_expression():
    """Array function call used inside a larger expression."""
    source = """\
    on (int n) = count of [items$]
        n = 0
    on (bool b) = is empty (int items$)
        b = (count of [items$] == 0)"""
    ir = process(source)
    fn = ir["functions"][1]
    # the body should contain an array_fn_call inside the == binop
    val = fn["body"][0]["value"]
    assert val["kind"] == "binop"
    assert val["left"]["kind"] == "array_fn_call"


# --- array indexing ---

def test_array_index():
    """a$[i] returns a single element."""
    source = """\
    on (int r) = first of (int items$)
        r = items$[0]"""
    ir = process(source)
    val = ir["functions"][0]["body"][0]["value"]
    assert val["kind"] == "index"
    assert val["array"]["value"] == "items$"
    assert val["index"]["value"] == 0

def test_array_index_variable():
    """Index can be a variable."""
    source = """\
    on (int r) = nth (int n) of (int items$)
        r = items$[n]"""
    ir = process(source)
    val = ir["functions"][0]["body"][0]["value"]
    assert val["kind"] == "index"
    assert val["index"]["value"] == "n"

def test_slice_open_end():
    """a$[i:] slices from i to end."""
    source = """\
    on (int result$) = rest of (int items$)
        result$ = items$[1:]"""
    ir = process(source)
    val = ir["functions"][0]["body"][0]["value"]
    assert val["kind"] == "slice"
    assert val["start"]["value"] == 1
    assert val["end"] is None

def test_slice_open_start():
    """a$[:j] slices from start to j."""
    source = """\
    on (int result$) = first three of (int items$)
        result$ = items$[:3]"""
    ir = process(source)
    val = ir["functions"][0]["body"][0]["value"]
    assert val["kind"] == "slice"
    assert val["start"] is None
    assert val["end"]["value"] == 3


# --- length of ---

def test_length_of():
    """length of [a$] returns the array length."""
    source = """\
    on (int n) = size of (int items$)
        n = length of [items$]"""
    ir = process(source)
    fn = ir["functions"][0]
    val = fn["body"][0]["value"]
    assert val["kind"] == "array_fn"
    assert val["name"] == "length_of"
    assert val["args"][0] == {"kind": "name", "value": "items$"}

def test_length_of_in_slice():
    """length of can be used inside a slice expression."""
    source = """\
    on (bool b) = starts with prefix (int items$) (int prefix$)
        b = (items$[0 to length of [prefix$]] == prefix$) & _"""
    ir = process(source)
    fn = ir["functions"][0]
    val = fn["body"][0]["value"]
    # the reduce & _ wraps the == which wraps the slice
    # just check it parsed without error and has the right structure
    assert val["kind"] == "reduce"


# --- streaming ---

def test_stream_simple():
    """Simple stream with literal appends."""
    ir = process("    int i$ <- 1 <- 2 <- 3 <- 4")
    var = ir["variables"][0]
    assert var["name"] == "i"
    assert var["array"] is True
    assert var["value"]["kind"] == "stream"
    assert len(var["value"]["steps"]) == 4
    assert var["value"]["terminate"] is None

def test_stream_self_ref():
    """Stream with self-reference, no terminator."""
    ir = process("    int i$ <- 1 <- (i$ + 1)")
    var = ir["variables"][0]
    val = var["value"]
    assert val["kind"] == "stream"
    assert len(val["steps"]) == 2
    assert val["steps"][1]["kind"] == "binop"
    assert val["terminate"] is None

def test_stream_until():
    """Stream with until terminator."""
    ir = process("    int i$ <- 1 <- (i$ + 1) until (i$ == 4)")
    var = ir["variables"][0]
    val = var["value"]
    assert val["kind"] == "stream"
    assert val["terminate"]["kind"] == "until"
    assert val["terminate"]["condition"]["kind"] == "binop"
    assert val["terminate"]["condition"]["op"] == "=="

def test_stream_while():
    """Stream with while terminator."""
    ir = process("    int i$ <- 0 <- (i$ + 1) while (i$ < 4)")
    var = ir["variables"][0]
    val = var["value"]
    assert val["kind"] == "stream"
    assert val["terminate"]["kind"] == "while"
    assert val["terminate"]["condition"]["op"] == "<"


# --- concurrently ---

def test_concurrently_two_blocks():
    """Parse a concurrently...and block in a function body."""
    source = """\
    on run()
        concurrently
            hello()
        and
            beep()"""
    ir = process(source)
    fn = ir["functions"][0]
    assert len(fn["body"]) == 1
    stmt = fn["body"][0]
    assert stmt["kind"] == "concurrently"
    assert len(stmt["blocks"]) == 2

def test_concurrently_three_blocks():
    """Concurrently can have more than two blocks."""
    source = """\
    on run()
        concurrently
            a()
        and
            b()
        and
            c()"""
    ir = process(source)
    stmt = ir["functions"][0]["body"][0]
    assert stmt["kind"] == "concurrently"
    assert len(stmt["blocks"]) == 3

def test_concurrently_with_surrounding_code():
    """Concurrently can appear between other statements."""
    source = """\
    on run()
        count down()
        concurrently
            hello()
        and
            beep()
        goodbye()"""
    ir = process(source)
    fn = ir["functions"][0]
    assert len(fn["body"]) == 3
    assert fn["body"][0]["kind"] in ("raw", "call")  # count down()
    assert fn["body"][1]["kind"] == "concurrently"
    assert fn["body"][2]["kind"] in ("raw", "call")  # goodbye()


# --- void functions ---

def test_void_function():
    """A function with no '=' has no return type or result variable."""
    source = """\
    on hello()
        print "hello world" """
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["result"] is None
    assert fn["params"] == []
    assert fn["signature_parts"] == ["hello"]

def test_void_function_with_params():
    """A void function can still have parameters."""
    source = """\
    on greet (string name)
        print name"""
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["result"] is None
    assert fn["params"] == [{"name": "name", "type": "string"}]
    assert fn["signature_parts"] == ["greet", "(string name)"]

def test_void_function_multiword():
    """A void function can have a multi-word name."""
    source = """\
    on count down()
        print [10 through 1]"""
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["result"] is None
    assert fn["signature_parts"] == ["count", "down"]


# --- function calls in expressions ---

def test_function_call_in_body():
    """A function body should recognise calls to other functions."""
    source = """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = quadruple (number x)
        n = double (double (x))"""
    ir = process(source)
    fn = ir["functions"][1]
    stmt = fn["body"][0]
    assert stmt["kind"] == "assign"
    val = stmt["value"]
    # outer call: double(...)
    assert val["kind"] == "fn_call"
    assert val["signature_parts"] == ["double", "(number)"]
    assert len(val["args"]) == 1
    # inner call: double(x)
    inner = val["args"][0]
    assert inner["kind"] == "fn_call"
    assert inner["signature_parts"] == ["double", "(number)"]

def test_function_call_multiword():
    """Multi-word function calls should be recognised."""
    source = """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = smaller of (smaller of (a) and (b)) and (c)"""
    ir = process(source)
    fn = ir["functions"][1]
    stmt = fn["body"][0]
    val = stmt["value"]
    # outer: smaller of (...) and (...)
    assert val["kind"] == "fn_call"
    assert val["signature_parts"] == ["smaller", "of", "(number)", "and", "(number)"]
    assert len(val["args"]) == 2
    # first arg is itself a call
    assert val["args"][0]["kind"] == "fn_call"

def test_function_call_single_arg():
    """Single-arg function calls in expressions."""
    source = """\
    on (number n) = negate (number x)
        n = 0 - x
    on (number n) = double negate (number x)
        n = negate (negate (x))"""
    ir = process(source)
    fn = ir["functions"][1]
    stmt = fn["body"][0]
    val = stmt["value"]
    assert val["kind"] == "fn_call"
    assert val["args"][0]["kind"] == "fn_call"


# --- array literal with expressions ---

def test_array_literal_with_fn_calls():
    """Array literals should support expressions, not just literals."""
    source = """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [double (1), double (2), double (3)]"""
    ir = process(source)
    var = ir["variables"][0]
    assert var["array"] is True
    assert isinstance(var["value"], list)
    assert var["value"][0]["kind"] == "fn_call"

def test_array_literal_with_variables():
    """Array literals should support variable names."""
    source = """\
    on (int r) = foo (int a) and (int b)
        int vals$ = [a, b, a + b]
        r = vals$ + _"""
    ir = process(source)
    fn = ir["functions"][0]
    decl = fn["body"][0]
    assert decl["value"][0] == {"kind": "name", "value": "a"}
    assert decl["value"][2]["kind"] == "binop"


# --- range with variables ---

def test_array_range_with_variable():
    """Range endpoints can be variables, not just literals."""
    source = """\
    on (int r) = sum up to (int n)
        int i$ = [1 through n]
        r = i$ + _"""
    ir = process(source)
    fn = ir["functions"][0]
    decl = fn["body"][0]
    assert decl["kind"] == "var_decl"
    assert decl["array"] is True
    val = decl["value"]
    assert val["range"] == "through"
    assert val["start"] == 1
    assert val["end"] == "n"


# --- variable declarations in function body ---

def test_function_body_variable_declaration():
    """Function bodies should support variable declarations."""
    source = """\
    on (int r) = sum of (int a) and (int b)
        int c = a + b
        r = c"""
    ir = process(source)
    fn = ir["functions"][0]
    assert len(fn["body"]) == 2
    assert fn["body"][0]["kind"] == "var_decl"
    assert fn["body"][0]["name"] == "c"
    assert fn["body"][0]["type"] == "int"
    assert fn["body"][0]["value"]["kind"] == "binop"

def test_function_body_array_declaration():
    """Function bodies should support array declarations."""
    source = """\
    on (int r) = sum list (int n)
        int i$ = [1, 2, 3, 4]
        r = i$ + _"""
    ir = process(source)
    fn = ir["functions"][0]
    assert fn["body"][0]["kind"] == "var_decl"
    assert fn["body"][0]["name"] == "i"
    assert fn["body"][0]["array"] is True


# --- reduce in function body ---

def test_reduce_in_body():
    """A function body should support reduce expressions."""
    source = """\
    on (int r) = sum of list
        int i$ = [1, 2, 3]
        r = i$ + _"""
    ir = process(source)
    fn = ir["functions"][0]
    stmt = fn["body"][1]
    assert stmt["kind"] == "assign"
    assert stmt["value"]["kind"] == "reduce"
    assert stmt["value"]["op"] == "+"
    assert stmt["value"]["array"] == "i$"


# --- empty function body ---

def test_abstract_function_no_body():
    """A function with no body is abstract (platform declaration)."""
    source = "    on (int r) = nothing (int a)"
    ir = process(source)
    fn = ir["functions"][0]
    assert fn.get("abstract") is True
    assert fn["body"] == []


# --- error reporting ---

def test_error_type_missing_equals():
    with pytest.raises(ZeroParseError) as exc_info:
        process("    type int32 32-bit signed integer")
    err = exc_info.value
    assert err.line_num == 1
    assert "expected '='" in str(err)
    assert err.column == 10  # points after 'int32', where '=' should be

def test_error_type_missing_name():
    with pytest.raises(ZeroParseError) as exc_info:
        process("    type = int | float")
    err = exc_info.value
    assert err.line_num == 1
    assert "expected type name" in str(err)

def test_void_function_is_valid():
    """'on smaller of (number a) and (number b)' is now a valid void function."""
    source = """\
    on smaller of (number a) and (number b)
        print a"""
    ir = process(source)
    assert ir["functions"][0]["result"] is None

def test_abstract_void_function_no_body():
    """A void function with no body is abstract."""
    ir = process("    on hello()")
    fn = ir["functions"][0]
    assert fn.get("abstract") is True

def test_error_struct_no_fields():
    with pytest.raises(ZeroParseError) as exc_info:
        process("    type vector =\n")
    err = exc_info.value
    assert err.line_num == 1
    assert "expected struct fields" in str(err)

def test_error_has_pointer():
    """Error messages should include a pointer to the problem location."""
    with pytest.raises(ZeroParseError) as exc_info:
        process("    type int32 32-bit signed integer")
    formatted = exc_info.value.format()
    assert "^" in formatted
    assert "type int32 32-bit signed integer" in formatted
