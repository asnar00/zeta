"""Tests for parsing feature syntax from zero source."""

from feature_parser import parse_features


# --- basic feature ---

def test_single_feature():
    source = """\
feature Hello
on run()
    hello()

on hello()
    print "hello world"
"""
    features = parse_features(source)
    assert len(features) == 1
    f = features[0]
    assert f["name"] == "Hello"
    assert f["extends"] is None
    assert "run" in f["functions"]
    assert "hello" in f["functions"]

def test_feature_with_extends():
    source = """\
feature Countdown extends Hello
in run(), before hello()
    count down()
on count down()
    print [10 through 1]
"""
    features = parse_features(source)
    f = features[0]
    assert f["name"] == "Countdown"
    assert f["extends"] == "Hello"


# --- extensions ---

def test_before_extension():
    source = """\
feature Countdown extends Hello
in run(), before hello()
    count down()
on count down()
    print [10 through 1]
"""
    features = parse_features(source)
    f = features[0]
    assert len(f["extensions"]) == 1
    ext = f["extensions"][0]
    assert ext["kind"] == "before"
    assert ext["target_fn"] == "run"
    assert ext["target_step"] == "hello"
    assert "    count down()" in ext["insert"]

def test_after_extension():
    source = """\
feature Bye extends Hello
in run(), after hello()
    goodbye()

on goodbye()
    print "bye!"
"""
    features = parse_features(source)
    ext = features[0]["extensions"][0]
    assert ext["kind"] == "after"
    assert ext["target_fn"] == "run"
    assert ext["target_step"] == "hello"
    assert "    goodbye()" in ext["insert"]

def test_on_extension():
    source = """\
feature Beep extends Hello
in run(), on hello()
    beep()

on beep()
    play audio ("beep.wav")
"""
    features = parse_features(source)
    ext = features[0]["extensions"][0]
    assert ext["kind"] == "on"
    assert ext["target_fn"] == "run"
    assert ext["target_step"] == "hello"

def test_replace_extension():
    source = """\
feature Loud extends Hello
in run(), replace hello()
    shout()

on shout()
    print "HELLO WORLD!"
"""
    features = parse_features(source)
    ext = features[0]["extensions"][0]
    assert ext["kind"] == "replace"
    assert ext["target_fn"] == "run"
    assert ext["target_step"] == "hello"


# --- type extensions ---

def test_type_extension():
    source = """\
feature Alpha extends Colour
type colour +=
    number alpha = 1
"""
    features = parse_features(source)
    f = features[0]
    assert len(f["type_extensions"]) == 1
    te = f["type_extensions"][0]
    assert te["target_type"] == "colour"
    assert te["fields"] == "number alpha = 1"

def test_type_extension_multiple_fields():
    source = """\
feature Extra extends Base
type point +=
    number z = 0
    number w = 0
"""
    features = parse_features(source)
    f = features[0]
    assert len(f["type_extensions"]) == 2
    assert f["type_extensions"][0]["fields"] == "number z = 0"
    assert f["type_extensions"][1]["fields"] == "number w = 0"

def test_type_extension_old_syntax():
    """Old 'extend X' syntax still works."""
    source = """\
feature Alpha extends Colour
extend colour
    number alpha = 1
"""
    features = parse_features(source)
    assert len(features[0]["type_extensions"]) == 1


# --- void functions in features ---

def test_void_function_in_feature():
    source = """\
feature Hello
on run()
    hello()

on hello()
    print "hello world"
"""
    features = parse_features(source)
    f = features[0]
    fn_run = f["functions"]["run"]
    assert fn_run["signature"] == "on run()"
    assert "    hello()" in fn_run["body"]

def test_value_function_in_feature():
    source = """\
feature Math
on (number n) = double (number x)
    n = x * 2
"""
    features = parse_features(source)
    f = features[0]
    assert "double" in f["functions"]
    fn = f["functions"]["double"]
    assert fn["signature"] == "on (number n) = double (number x)"


# --- multiple features in one source ---

def test_multiple_features():
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
"""
    features = parse_features(source)
    assert len(features) == 2
    assert features[0]["name"] == "Hello"
    assert features[1]["name"] == "Countdown"


# --- multi-line extension body ---

def test_multiline_extension_body():
    source = """\
feature Setup extends Hello
in run(), before hello()
    init()
    setup logging()
    prepare()
"""
    features = parse_features(source)
    ext = features[0]["extensions"][0]
    assert len(ext["insert"]) == 3
    assert "    init()" in ext["insert"]
    assert "    setup logging()" in ext["insert"]
    assert "    prepare()" in ext["insert"]
