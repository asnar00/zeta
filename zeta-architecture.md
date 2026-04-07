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
- `string.zero.md` — declares `trim`, `split at`, `to int`
- `time.zero.md` — declares `seconds`, `ms`, `hz`, `bpm`, `now`
- `terminal.zero.md` — declares `out$`, `in$` (stdout/stdin streams)
- `blackbox.zero.md` — flight recorder (see below)

Each has `.py` and `.ts` implementation files. Platform code is prepended to all feature output files. Platform functions are excluded from the cross-module map (they're available directly, not via imports).

Platform directories can contain multiple file types:
- `.zero.md` — interface declarations (prepended as zero source)
- `.py` / `.ts` — server-side implementations (prepended to compiled output)
- `.client.js` — browser-side implementations (assembled into client JS bundle)
- `.main.py` / `.main.ts` — entry-point code (appended, not prepended)

All files are auto-discovered by scanning `platforms/*/`.

## blackbox (flight recorder)

The blackbox platform provides fault diagnosis through continuous recording and replay.

### principle

All computation in zero is deterministic. All non-determinism enters through streams (HTTP requests, user input, random values, clock reads). The blackbox records every value that enters through a non-deterministic stream. Given the recorded stream values and the code fingerprint, the entire execution can be replayed deterministically.

### architecture

Each participant (client or server) runs its own ring buffer. The buffer holds a rolling 60-second window divided into 10-second *moments*:

    state₀ → actions → state₁ → actions → ... → state₅ → actions

Each moment opens with a **keyframe** (full serialized context state) followed by a sequence of timed **actions** (boundary-crossing function calls with correlation IDs, arguments, results, and elapsed time).

### emitter instrumentation

The emitters (`emit_python.py`, `emit_typescript.py`) auto-instrument at two points:

1. **Stream consumption** — every `for each` loop in a task and every `consume_call` wraps its iterator with `_bb_record_stream(name, iterator)`. This records every value that enters from a non-deterministic stream (HTTP requests, terminal input, task-produced sequences).

2. **Non-deterministic platform calls** — platform function calls that return values from the outside world (`random digits`, `create session`, `input`, `connect to`, etc.) are wrapped with `_bb_record_call(name, result)`. An allow-list (`_NONDETERMINISTIC_PREFIXES`) identifies these; deterministic platform functions (`trim`, `starts_with`, `length_of`) are never recorded.

Both emitters include a no-op fallback (`_bb_record_stream` passes through, `_bb_record_call` passes through) that the blackbox platform overrides when loaded. Standalone code works without blackbox.

### client-side hooks

The browser client (`blackbox.client.js`) additionally hooks:
- `_rpc()` — records every fetch-based RPC call to the server
- `_client_eval()` — records every command received via WebSocket

Auto-starts on page load, persists fault reports to `localStorage` for offline resilience.

### fault report flow

1. User hits "report fault", types a comment
2. Client freezes its ring buffer, serialises it with the comment, persists to `localStorage`
3. On connectivity, client uploads via `/@rpc/report fault (...)`
4. Server receives the client report, attaches its own server-side moments from the same time window
5. Bundled report stored server-side, retrievable via `get fault (id)`

### platform surface

The blackbox platform requires only 7 thin OS primitives:

| Primitive | Python | Browser |
|---|---|---|
| `elapsed time ()` | `time.monotonic()` | `performance.now()` |
| `every (ms) do (callback)` | `threading.Timer` | `setInterval` |
| `cancel timer (id)` | `timer.cancel()` | `clearInterval` |
| `store locally (key, value)` | JSON file | `localStorage` |
| `retrieve locally (key)` | JSON file | `localStorage` |
| `stored keys (prefix)` | JSON file | `localStorage` |
| `remove locally (key)` | JSON file | `localStorage` |

Everything else (buffer management, moment rotation, freezing, correlation, upload) is built on these primitives plus the existing `remote` and `runtime` platforms.

### multi-device correlation

Every action that crosses a component boundary is tagged with a correlation ID. When Alice's client sends an RPC, it logs `(t, send_rpc, correlation: "c7")`. The server logs `(t, recv_rpc, correlation: "s3")`. Correlation IDs stitch the distributed trace together after the fact, without requiring real-time synchronisation between devices.

### replay (future)

Given a fault package (N device buffers + fingerprints), replay:
1. Look up build fingerprint → reconstruct exact code
2. Spin up isolated replica with that build
3. Inject keyframes as initial state
4. Feed recorded stream values back through the system
5. Verify state at moment boundaries — divergence pinpoints the bug

## self-hosting (ziz/)

The `ziz/` directory contains zero programs that implement parts of zeta itself:

- `zeta.zero.md` — base feature (logo, main entry point)
- `parser.zero.md` — parser functions (bracket matching, split stream parts)

Built via `python3 zeta.py ziz/features.md ziz/output/`. Output includes per-feature `.py` and `.ts` files, `_runtime` test infrastructure, and a `package.json` for ESM support.

## timed streams

The `time` type represents a duration. Unit functions (`seconds`, `ms`, `hz`, `bpm`) construct time values. The `at` modifier attaches timing to any stream:

```zero
int i$ <- count down from (10) at ((1) hz)
out$ <- count down from (10) at ((1) hz)
int data$ <- [1, 2, 3] at ((44100) hz)
int i$(dt = (1) hz)
```

### how timing works

Every stream `$` is a sequence of values. Timing is optional metadata:
- No timing: iterate instantly (plain array)
- Regular timing (`dt`): timestamps computed as `t = t0 + i * dt`
- Sparse timing: per-element timestamps (merged concurrent streams)

The `_Stream(list)` class in Python extends `list` with `dt`, `length`, and `t0` properties. The `_bb_record_stream` wrapper checks `dt` and sleeps between values for real-time playback.

### stream piping

`sink$ <- source$` iterates the source with timing and pushes each value to the sink:

```zero
out$ <- count down from (10) at ((1) hz)
```

Compiles to an anonymous `_Stream` with `dt` attached, iterated through `_bb_record_stream`, pushing each value to `_push_terminal_out`.

### forms

Three equivalent ways to create a timed stream:

1. **Inline `at`**: `int i$ <- expr at ((1) hz)`
2. **Declaration properties**: `int i$(dt = (1) hz)` then `i$ <- expr`
3. **Anonymous piping**: `out$ <- task() at ((1) hz)` — no named variable needed

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
