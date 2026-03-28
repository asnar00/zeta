# zeta architecture
*shared design decisions for the zero translator*

## pipeline

```
zero source (.md)
  → parser: parse declarations (types, variables, functions, tasks)
  → parser: parse expressions into ASTs
  → IR (dict) with version and feature set
  → emitter: check compatibility, walk IR, emit target language code
  → output (.py, .ts, ...)
```

For feature-modular programs:

```
feature sources (.md)
  → feature_parser: extract features, extensions, type extensions
  → composer: combine nubs, apply before/after/on/replace
  → composed zero source
  → parser → IR → emitter (as above)
```

## modules

- **`parser.py`** — parses zero source into an IR dict. Handles types, variables, function signatures, tasks, and expression parsing (body strings → ASTs).
- **`emit_python.py`** — emits Python 3.12+. Checks IR version and feature compatibility.
- **`emit_typescript.py`** — emits TypeScript. Checks IR version and feature compatibility.
- **`feature_parser.py`** — parses feature declarations, extensions, and type extensions from zero source.
- **`composer.py`** — combines feature nubs into composed zero source with before/after/on/replace.
- **`zeta.py`** — CLI entry point: `python3 zeta.py input.md output.py|ts`.
- **`probe.py`** — systematic feature-combination tester.

## IR format

The IR is a dict with keys: `version`, `features`, `types`, `variables`, `functions`, `tasks`.

### version and features

```python
{ "version": 1,
  "features": {"numeric_types", "enums", "structs", "arrays", "functions", ...} }
```

Emitters declare which version and features they support. Mismatch = error.

### types

```python
{ "kind": "numeric", "name": "int32", "base": "int", "size": 32 }
{ "kind": "numeric", "name": "number", "base": "int | float" }
{ "kind": "enum", "name": "tri-state", "values": ["no", "yes", "maybe"] }
{ "kind": "struct", "name": "vector",
  "fields": [
    { "name": "x", "type": "number", "default": 0 },
    { "name": "y", "type": "number", "default": 0 },
    { "name": "z", "type": "number", "default": 0 }
  ]
}
```

### variables

```python
{ "name": "i", "type": "int32", "array": false, "value": 10 }
{ "name": "i", "type": "int", "array": true, "size": 4, "value": null }
{ "name": "i", "type": "int", "array": true, "value": [1, 2, 3, 4] }
{ "name": "i", "type": "int", "array": true,
  "value": { "range": "through", "start": 1, "end": 4 } }
{ "name": "i", "type": "int", "array": true,
  "value": { "kind": "stream", "steps": [...], "terminate": {...} } }
```

### functions

```python
{ "result": { "name": "v", "type": "vector" },
  "params": [{ "name": "a", "type": "vector" }, ...],
  "signature_parts": ["(vector a)", "+", "(vector b)"],
  "body": [<AST nodes>]
}
```

Void functions have `"result": null`.

### tasks

```python
{ "name_parts": ["only", "evens", "from"],
  "output": { "name": "even", "type": "int" },
  "input_streams": [{ "name": "numbers", "type": "int" }],
  "params": [],
  "body": [<raw lines>]
}
```

## expression AST

```python
{ "kind": "assign", "target": "v", "value": <expr> }
{ "kind": "call", "name": "vector", "args": [<expr>, ...] }
{ "kind": "fn_call", "signature_parts": [...], "args": [<expr>, ...] }
{ "kind": "binop", "op": "+", "left": <expr>, "right": <expr> }
{ "kind": "member", "object": "a", "field": "x" }
{ "kind": "name", "value": "a" }
{ "kind": "literal", "value": 42 }
{ "kind": "ternary", "condition": <expr>, "true": <expr>, "false": <expr> }
{ "kind": "reduce", "op": "+", "array": "i$" }
{ "kind": "stream", "steps": [...], "terminate": {...} }
{ "kind": "task_call", "signature_parts": [...], "args": [...] }
{ "kind": "if_block", "branches": [{ "condition": <expr>, "body": [...] }, ...] }
{ "kind": "concurrently", "blocks": [[<lines>], ...] }
{ "kind": "var_decl", "name": "c", "type": "int", "value": <expr> }
```

## key decisions

1. **Types stay as zero names in the IR** — the emitter resolves to target-language types.
2. **Function naming lives in the emitter** — each target applies its own convention.
3. **Expression parsing is shared** — zero expressions are language-agnostic.
4. **Two-pass parsing** — first pass collects function/task signatures, second pass resolves calls in bodies.
5. **Struct fields default to 0** if no explicit default.
6. **IR versioning** — parser stamps version and feature set; emitters check compatibility.
7. **Modular by language feature** — each construct is handled independently, easy to add new features.
