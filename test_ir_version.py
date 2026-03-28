"""Tests for IR versioning and feature contracts."""

import pytest
from parser import process, IR_VERSION
from emit_python import emit as emit_py
from emit_typescript import emit as emit_ts


# --- IR version stamping ---

def test_ir_has_version():
    ir = process("    int32 i = 10")
    assert "version" in ir
    assert ir["version"] == IR_VERSION

def test_ir_has_features():
    ir = process("    int32 i = 10")
    assert "features" in ir
    assert isinstance(ir["features"], set)


# --- feature detection ---

def test_features_numeric_types():
    ir = process("    type int32 = ... 32-bit signed integer")
    assert "numeric_types" in ir["features"]

def test_features_enums():
    ir = process("    type tri-state = no | yes | maybe")
    assert "enums" in ir["features"]

def test_features_structs():
    source = """\
    type vector =
        number x, y, z = 0"""
    ir = process(source)
    assert "structs" in ir["features"]

def test_features_arrays():
    ir = process("    int i$[4]")
    assert "arrays" in ir["features"]

def test_features_functions():
    source = """\
    on (number n) = double (number x)
        n = x * 2"""
    ir = process(source)
    assert "functions" in ir["features"]

def test_features_void_functions():
    source = """\
    on hello()
        print "hello" """
    ir = process(source)
    assert "void_functions" in ir["features"]

def test_features_concurrently():
    source = """\
    on run()
        concurrently
            a()
        and
            b()"""
    ir = process(source)
    assert "concurrently" in ir["features"]

def test_features_array_map():
    source = """\
    int i$ = [1, 2, 3]
    int j$ = i$ * 2"""
    ir = process(source)
    assert "array_map" in ir["features"]

def test_features_array_reduce():
    source = """\
    int i$ = [1, 2, 3]
    int sum = i$ + _"""
    ir = process(source)
    assert "array_reduce" in ir["features"]

def test_features_fn_calls():
    source = """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = quad (number x)
        n = double (double (x))"""
    ir = process(source)
    assert "fn_calls" in ir["features"]

def test_features_strings():
    ir = process('    string s = "hello"')
    assert "strings" in ir["features"]


# --- emitter compatibility ---

def test_python_emitter_accepts_compatible_ir():
    ir = process("    int32 i = 10")
    emit_py(ir)  # should not raise

def test_typescript_emitter_accepts_compatible_ir():
    ir = process("    int32 i = 10")
    emit_ts(ir)  # should not raise

def test_python_emitter_rejects_future_version():
    ir = process("    int32 i = 10")
    ir["version"] = 999
    with pytest.raises(ValueError, match="IR version"):
        emit_py(ir)

def test_typescript_emitter_rejects_future_version():
    ir = process("    int32 i = 10")
    ir["version"] = 999
    with pytest.raises(ValueError, match="IR version"):
        emit_ts(ir)

def test_emitter_rejects_unsupported_feature():
    ir = process("    int32 i = 10")
    ir["features"].add("quantum_entanglement")
    with pytest.raises(ValueError, match="unsupported.*quantum_entanglement"):
        emit_py(ir)
