"""Tests for the feature composer."""

from composer import compose
from feature_parser import parse_features
from parser import process
from emit_python import emit as emit_py


# --- basic feature ---

def test_single_feature():
    """A single feature with no extensions passes through unchanged."""
    features = [
        {
            "name": "Hello",
            "extends": None,
            "functions": {
                "run": {
                    "signature": "on run()",
                    "body": ["    hello()"],
                },
                "hello": {
                    "signature": "on hello()",
                    "body": ["    print \"hello world\""],
                },
            },
            "extensions": [],
            "type_extensions": [],
        }
    ]
    result = compose(features)
    assert "on run()" in result
    assert "    hello()" in result
    assert "on hello()" in result


# --- before ---

def test_before_extension():
    """A 'before' extension inserts code before the target step."""
    features = [
        {
            "name": "Hello",
            "extends": None,
            "functions": {
                "run": {
                    "signature": "on run()",
                    "body": ["    hello()"],
                },
                "hello": {
                    "signature": "on hello()",
                    "body": ["    print \"hello world\""],
                },
            },
            "extensions": [],
            "type_extensions": [],
        },
        {
            "name": "Countdown",
            "extends": "Hello",
            "functions": {
                "count down": {
                    "signature": "on count down()",
                    "body": ["    print [10 through 1]"],
                },
            },
            "extensions": [
                {"kind": "before", "target_fn": "run", "target_step": "hello", "insert": ["    count down()"]},
            ],
            "type_extensions": [],
        },
    ]
    result = compose(features)
    lines = result.strip().split("\n")
    # find run() body
    run_start = next(i for i, l in enumerate(lines) if l.strip() == "on run()")
    run_body = []
    for l in lines[run_start + 1:]:
        if l and not l[0].isspace():
            break
        run_body.append(l.strip())
    # count down() should come before hello()
    assert "count down()" in run_body
    assert "hello()" in run_body
    assert run_body.index("count down()") < run_body.index("hello()")


# --- after ---

def test_after_extension():
    """An 'after' extension inserts code after the target step."""
    features = [
        {
            "name": "Hello",
            "extends": None,
            "functions": {
                "run": {
                    "signature": "on run()",
                    "body": ["    hello()"],
                },
                "hello": {
                    "signature": "on hello()",
                    "body": ["    print \"hello world\""],
                },
            },
            "extensions": [],
            "type_extensions": [],
        },
        {
            "name": "Bye",
            "extends": "Hello",
            "functions": {
                "goodbye": {
                    "signature": "on goodbye()",
                    "body": ["    print \"bye!\""],
                },
            },
            "extensions": [
                {"kind": "after", "target_fn": "run", "target_step": "hello", "insert": ["    goodbye()"]},
            ],
            "type_extensions": [],
        },
    ]
    result = compose(features)
    lines = result.strip().split("\n")
    run_start = next(i for i, l in enumerate(lines) if l.strip() == "on run()")
    run_body = []
    for l in lines[run_start + 1:]:
        if l and not l[0].isspace():
            break
        run_body.append(l.strip())
    assert run_body.index("hello()") < run_body.index("goodbye()")


# --- on (concurrent) ---

def test_on_extension():
    """An 'on' extension wraps the target step in a concurrently block."""
    features = [
        {
            "name": "Hello",
            "extends": None,
            "functions": {
                "run": {
                    "signature": "on run()",
                    "body": ["    hello()"],
                },
                "hello": {
                    "signature": "on hello()",
                    "body": ["    print \"hello world\""],
                },
            },
            "extensions": [],
            "type_extensions": [],
        },
        {
            "name": "Beep",
            "extends": "Hello",
            "functions": {
                "beep": {
                    "signature": "on beep()",
                    "body": ["    play audio (\"beep.wav\")"],
                },
            },
            "extensions": [
                {"kind": "on", "target_fn": "run", "target_step": "hello", "insert": ["    beep()"]},
            ],
            "type_extensions": [],
        },
    ]
    result = compose(features)
    assert "concurrently" in result
    assert "hello()" in result
    assert "beep()" in result


# --- combined ---

def test_all_extensions_combined():
    """before + on + after all applied to the same function."""
    features = [
        {
            "name": "Hello",
            "extends": None,
            "functions": {
                "run": {
                    "signature": "on run()",
                    "body": ["    hello()"],
                },
                "hello": {
                    "signature": "on hello()",
                    "body": ["    print \"hello world\""],
                },
            },
            "extensions": [],
            "type_extensions": [],
        },
        {
            "name": "Countdown",
            "extends": "Hello",
            "functions": {
                "count down": {
                    "signature": "on count down()",
                    "body": ["    print [10 through 1]"],
                },
            },
            "extensions": [
                {"kind": "before", "target_fn": "run", "target_step": "hello", "insert": ["    count down()"]},
            ],
            "type_extensions": [],
        },
        {
            "name": "Bye",
            "extends": "Hello",
            "functions": {
                "goodbye": {
                    "signature": "on goodbye()",
                    "body": ["    print \"bye!\""],
                },
            },
            "extensions": [
                {"kind": "after", "target_fn": "run", "target_step": "hello", "insert": ["    goodbye()"]},
            ],
            "type_extensions": [],
        },
        {
            "name": "Beep",
            "extends": "Hello",
            "functions": {
                "beep": {
                    "signature": "on beep()",
                    "body": ["    play audio (\"beep.wav\")"],
                },
            },
            "extensions": [
                {"kind": "on", "target_fn": "run", "target_step": "hello", "insert": ["    beep()"]},
            ],
            "type_extensions": [],
        },
    ]
    result = compose(features)
    lines = [l.strip() for l in result.strip().split("\n") if l.strip()]
    # find run body
    run_start = lines.index("on run()")
    run_body = []
    for l in lines[run_start + 1:]:
        if l.startswith("on "):
            break
        run_body.append(l)
    # order should be: count down, concurrently(hello, beep), goodbye
    assert "count down()" in run_body
    assert "concurrently" in run_body
    assert "goodbye()" in run_body
    cd_idx = run_body.index("count down()")
    conc_idx = run_body.index("concurrently")
    gb_idx = run_body.index("goodbye()")
    assert cd_idx < conc_idx < gb_idx


# --- type extension ---

def test_type_extension():
    """extend type adds fields to an existing type."""
    features = [
        {
            "name": "Colour",
            "extends": None,
            "functions": {},
            "extensions": [],
            "type_extensions": [],
            "types": ["    type colour =\n        number r, g, b = 0"],
        },
        {
            "name": "Alpha",
            "extends": "Colour",
            "functions": {},
            "extensions": [],
            "type_extensions": [
                {"target_type": "colour", "fields": "number alpha = 1"},
            ],
            "types": [],
        },
    ]
    result = compose(features)
    assert "type colour =" in result
    assert "number r, g, b = 0" in result
    assert "number alpha = 1" in result


# --- end-to-end: feature source → parser → composer → emitter ---

def test_e2e_type_extension():
    """Full pipeline: feature source with type extension → Python output."""
    source = """\
feature Colour
type colour =
    number r, g, b = 0

feature Alpha extends Colour
type colour +=
    number alpha = 1
"""
    features = parse_features(source)
    composed = compose(features)
    ir = process(composed)
    py = emit_py(ir)
    assert "class colour(NamedTuple):" in py
    assert "    r: number = 0" in py
    assert "    g: number = 0" in py
    assert "    b: number = 0" in py
    assert "    alpha: number = 1" in py

def test_e2e_hello_world():
    """Full pipeline: hello world features → Python output."""
    source = """\
feature Hello
on run()
    hello()

on hello()
    print "hello world"

feature Countdown extends Hello
in run(), before hello()
    count down()
on count down()
    print [10 through 1]

feature Bye extends Hello
in run(), after hello()
    goodbye()

on goodbye()
    print "bye!"
"""
    features = parse_features(source)
    composed = compose(features)
    ir = process(composed)
    py = emit_py(ir)
    assert "def fn_run():" in py
    assert "def fn_hello():" in py
    assert "def fn_count_down():" in py
    assert "def fn_goodbye():" in py
