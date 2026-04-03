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
- **`emit_base.py`** — shared emitter infrastructure: function naming, type dispatch grouping, array ref rewriting, source comments. Used by both target emitters.
- **`emit_python.py`** — emits Python 3.12+. Checks IR version and feature compatibility.
- **`emit_typescript.py`** — emits TypeScript. Checks IR version and feature compatibility.
- **`feature_parser.py`** — parses feature declarations, extensions, and type extensions from zero source.
- **`composer.py`** — combines feature nubs into composed zero source with before/after/on/replace.
- **`zeta.py`** — CLI entry point: `python3 zeta.py input.md output.py|ts`. Orchestrates the pipeline for both single-file and feature-modular builds.
- **`probe.py`** — systematic feature-combination tester.

## code structure

All `.py` files follow a strict rule: **every function is 25 lines or fewer** (one screen of code). Commented code blocks are extracted into named functions, so the code reads as a sequence of clear steps rather than a wall of inline logic.

### zeta.py — pipeline orchestrator

`main()` reads as a short sequence of named steps: parse args, load emitter, read sources, compose, prepend platform interfaces, parse, emit, assemble output, write and compile. The feature-modular path (`_build_features`) follows the same pattern: read inputs, compose per-feature, build a context (IR mapped to features), emit all features, compile.

Helper functions group naturally: arg parsing, safety checks, source reading, platform loading, IR mapping (functions/tasks/variables to features), module map construction, and per-feature code generation with imports/exports/harness.

### emit_base.py — shared emitter logic

Function naming (`make_function_name`), type dispatch grouping (`compute_dispatch_groups` with `_type_depth` for inheritance-aware specificity sorting), array ref rewriting, underscore replacement, and source comment generation. Both emitters import these rather than duplicating logic.

### emit_python.py / emit_typescript.py — target emitters

Both emitters follow the same internal structure:

1. **Globals setup** — `_init_globals` / `_init_globals_ts` populates enum value maps and context feature sets from the IR.
2. **Orchestrator** — `emit()` calls phase functions in order: preamble (imports/helpers), tests, definitions (types/tasks/functions), context class, variables, dispatchers. Ends with cross-module prefix rewriting.
3. **Section emitters** — each IR element kind has its own emitter: `_emit_type` → `_emit_struct_type`, `_emit_task` → `_emit_task_header` + `_emit_task_body_node` + per-kind handlers, `_emit_function` → `_emit_result_function` + `_emit_guarded_body`.
4. **Expression dispatch** — `_emit_expr` is a flat dispatch that delegates to per-kind handlers: `_emit_call_expr`, `_emit_var_decl_expr`, `_emit_filter_expr`, `_emit_slice_expr`, `_emit_simple_expr` (for one-liner kinds like binop, index, ternary, raw), and `_emit_leaf_expr` (member, name, literal).

The two emitters make consistent decisions — same function naming patterns, same decomposition strategy, same grouping of expression kinds — so navigating one teaches you the other.

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
{ "kind": "to_chars", "value": "name" }    # string$ lens: string → char[]
{ "kind": "indices_where", "array": <expr>, "condition": <expr> }
```

## testing

Tests are derived from `=>` examples in `## interface` and `## tests` sections of `.zero.md` feature files:

    matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3

Each example becomes a test function in the output. A shared `_runtime.py`/`_runtime.ts` provides the test registry and runner. Run with `--test [feature_names]`.

The build pipeline automatically validates all output:
- Python: `py_compile` on each `.py` file
- TypeScript: `tsc --strict --noEmit` on all `.ts` files

## platforms

Platform-specific functions live in `platforms/`:

- `io.zero.md` — declares `read file`, `write file`, `print`
- `string.zero.md` — declares `trim`, `split at`

Each has `.py` and `.ts` implementation files. Platform code is prepended to all feature output files. Platform functions are excluded from the cross-module map (they're available directly, not via imports).

## self-hosting (ziz/)

The `ziz/` directory contains zero programs that implement parts of zeta itself:

- `zeta.zero.md` — base feature (logo, main entry point)
- `parser.zero.md` — parser functions (bracket matching, split stream parts)

Built via `python3 zeta.py ziz/features.md ziz/output/`. Output includes per-feature `.py` and `.ts` files, `_runtime` test infrastructure, and a `package.json` for ESM support.

## key decisions

1. **Types stay as zero names in the IR** — the emitter resolves to target-language types.
2. **Function naming lives in the emitter** — each target applies its own convention.
3. **Expression parsing is shared** — zero expressions are language-agnostic.
4. **Two-pass parsing** — first pass collects function/task signatures, second pass resolves calls in bodies.
5. **Struct fields default to 0** if no explicit default.
6. **IR versioning** — parser stamps version and feature set; emitters check compatibility.
7. **Modular by language feature** — each construct is handled independently, easy to add new features.
8. **Safety catch** — zeta refuses to write output into its own source directory.
9. **Decomposed streams over imperative loops** — the preferred zero style decomposes loop accumulators into parallel array streams, avoiding mutable state. See `split stream parts` in `atoz.md`.
10. **25-line function limit** — every function fits on one screen. Commented code blocks become named functions. This keeps the code navigable and talkable — you can refer to `_emit_task_for_each` instead of "lines 740–780 of emit_python.py".
