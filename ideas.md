# ideas
*future directions for zeta*

## DONE (implemented)

These items have been completed and are tracked here for historical context:

- **IR versioning and feature contracts** — implemented. Parser stamps version + features, emitters check compatibility.
- **Platform interfaces** — implemented as `platforms/` directory. Abstract declarations in `.zero.md`, target implementations in `.py`/`.ts`. `zeta.py` prepends both.
- **Sort** — implemented with `sort [a$]` and `sort [a$] by (_.field)`.
- **Where/filter** — implemented with `[a$] where (_.field >= x)` and `first of [a$] where (...)`.
- **Multiple dispatch** — implemented with isinstance dispatch tables in both emitters.
- **Type composition** — `type dog = animal +` with parent field flattening.
- **`_` as current element** — replaces `..`, used in reduce, where, sort.
- **Conditional blocks** — if/else if/else in functions and tasks.
- **Tasks** — coroutine-style stream producers/consumers, emitted as generators.
- **Streaming** — `<-` operator with while/until.
- **Array functions** — `[...]` call syntax with generic (untyped) params.
- **Void functions** — `on name()` with no return type.
- **Concurrently** — `_concurrently` helper in both targets.
- **Bitwise operators** — full set with correct precedence.
- **Strings** — type mapping, concat, equality, indexing, slicing.
- **Markdown extraction** — parser strips prose, extracts code blocks.
- **Abstract functions** — bodyless function declarations for platform interfaces.
- **Execution tests** — 48 Python + 39 TypeScript execution tests.
- **3-step streaming** — fixed: multi-seed streams with terminators now work correctly.
- **Error recovery** — parser supports `recover=True` mode, collects multiple errors.
- **Source line mapping** — errors in markdown files report correct original line numbers.
- **Shared emitter base** — `emit_base.py` with shared logic for function naming, dispatch, array ops.
- **Structured logging** — `--verbose` flag on zeta.py with timed, hierarchical stage output.
- **Parser warnings** — unrecognized lines reported instead of silently dropped.

## next: self-hosting

Write zeta's translator in zero. All blockers resolved:

1. Start with the expression parser — recursive descent, written as zero functions
2. Type definitions for the IR as zero structs with type composition
3. Multiple dispatch for the emitter (one `emit` definition per AST node type)
4. Platform I/O for file read/write
5. Tasks for the main parse loop (streaming through lines)
6. `where` for name lookups (replacing dicts)

Bootstrap path: Python zeta compiles zero-zeta to Python, then zero-zeta can compile itself.

## additional emitters

### rust
- Most natural third target — types map 1:1 (`int32`→`i32`, `uint8`→`u8`, `float32`→`f32`)
- Struct parameters as `&T` (immutable reference) since zero is pure
- Concrete type enforcement happens natively — no wrapping/masking
- Ternary → `if cond { a } else { b }` (expression in Rust)
- Would prove the IR is truly language-agnostic (three very different targets)

### C / C++
- `const&` for struct params (C++), `const*` with `->` (C)
- Concrete types via `<stdint.h>` (`int32_t`, `uint8_t`, etc.)
- Multiple dispatch → vtables (C++) or function pointer arrays (C)

### WebAssembly (WAT)
- Concrete numeric types map directly to wasm types
- Structs need flattening into linear memory
- Expressions reordered into postfix (stack-based)
- Zero's purity and SSA map well to wasm's design

### direct assembly (ARM / RISC-V / x86)
- SSA simplifies register allocation
- Pure functions mean no aliasing concerns
- Could emit `.s` files or go fully self-contained

## machine code learning via probes

Use a C compiler as a *teacher* to learn instruction patterns per architecture. Compile small C probe functions, disassemble, generalise patterns, store per-architecture. The assembly emitter stitches patterns — no manual needed.

## concrete type enforcement

Currently deferred:
- Integer overflow wrapping for int8/uint32 etc.
- Python: masking. TS: bitwise tricks. Rust/C: native.
- Float precision: float32 needs special handling in Python.

## int/float assignment semantics

Division always returns float; assignment to int truncates. Emitters need type-aware wrapping.

## atoz: anything to zero

Agent-powered tool that reads existing code and translates it *into* zero. Forces decomposition into pure functions. The inverse of zeta.

### atoz stress test: claude code

Anthropic's Claude Code leaked via npm source maps in March 2026 — 512k lines of TypeScript, 1,900 files. Once atoz is functional, translate the entire codebase to zero. This would be the ultimate stress test for both atoz (can it decompose a real-world 512k-line codebase?) and zero (can the language express a production agent system?). The decomposed zero version would also be a proof point for the language — if it can express Claude Code more clearly than the TypeScript original, that says something.

## per-user context: feature flags, preferences, and implicit parameters

Feature-scoped variables are currently global (one value for all users). Make them **per-user**: each user (identified by IP, cookie, or login) gets their own context object holding all feature-scoped variable values.

The emitter threads the context as a hidden first parameter through all function calls — the zero source never sees it. At the HTTP boundary, the platform creates or looks up the context per request.

**What this gives you:**
- Per-user feature flags (`landing_page_enabled`) — same infrastructure as LaunchDarkly, but falls out from zero's model
- Per-user preferences (`theme`, `language`, `font_size`) — same mechanism, just different variables
- Clean function signatures — `book flight ("LHR") ("JFK") (tomorrow)` instead of threading `class`, `time_preference`, `airlines`, `routing` as parameters. The preferences live on the context, read implicitly by the function.
- Testability — swap the context object per-test to simulate different users/configurations

**Implementation:** one dict lookup per request to find the user's context, then bare attribute access (`ctx.landing_page_enabled`) for every guard — same speed as a global on the hot path. The hidden `ctx` parameter is like `self` in Python — the emitter injects it, the programmer doesn't write it.

This is algebraic effects for configuration: an implicit parameter that flows through all calls, swappable at the boundary.

## round-trip correctness testing

Prove atoz + zeta preserve semantics by running original tests against re-emitted code. Multi-target agreement: emit to all targets, assert they agree.

## language evolution and migration

Migration agent reads syntax changelog, rewrites zero source to new forms. Version-controlled translation rules are the single source of truth.

## streaming vs map: relationship to firm up

`j$ = i$ + 1` (map) and `j$ <- (i$ + 1)` (stream) produce the same result for the simple case. Questions:
- Is map sugar for simple streaming?
- Are they semantically distinct (parallel-safe vs sequential)?
- Does the distinction matter for SIMD/GPU targets?

## more array builtins

Can be written in zero now using existing primitives:
- `reverse [a$]` — streamable
- `[a$] starts with [b$]` — slice + reduce with `&`
- `[a$] ends with [b$]` — same from the end
- `[a$] contains [b$]` — task: slide window
- `split [a$] at [delim]` — task: accumulate until delimiter
- `join [a$$] with [sep$]` — task: emit with separator
- `index of [b] in [a$]` — streaming with counter

## 3-step streaming bug (RESOLVED)

Fixed. With a terminator, steps[0..N-2] are initial seeds and steps[N-1] is the loop repeat expression. Both emitters updated.

## task examples to develop

- Byte-to-codepoint decoder (consume 1-2 bytes, emit 1 code point)
- Sum pairs (consume 2, emit 1)
- Run-length decoding (consume 1, emit N)
- Tokenizer (consume M chars, emit 1 token)
- Line-by-line parser (consume variable lines, emit IR nodes)

## parser hardening

The regex-based parser is fragile:
- Order-dependent checks that silently mis-parse
- Indentation handling is inconsistent (strip vs raw vs check)
- No formal grammar — adding features means more special cases
- Consider: write a proper grammar-driven parser in zero as part of self-hosting

## coroutine fusion: optimising pure stream chains into loops

All tasks are coroutines. The emitter defaults to async coroutines — correct everywhere, handles I/O naturally (`serve http`, file reads, etc.).

But pure computational chains (no awaits, no I/O) don't need the overhead. The emitter can detect when a chain of coroutines is purely computational and fuse them into a plain loop. For example, `bracket depth of` feeding into `index of first ... where` is just a `for` loop with an early return — no need for two generators yielding back and forth.

**Strategy:**
- Emit async coroutines by default — always correct
- Detect pure chains (no I/O, no platform calls) and fuse into loops
- SSA decomposition is for the human; fused loops are for the machine
- Same zero source, different output depending on what's optimal

This also clarifies the streaming vs map question: `j$ = i$ + 1` (map) could compile to a fused SIMD/parallel operation, while `j$ <- (i$ + 1)` (stream) implies sequential coroutine semantics. The optimiser decides.

## emitter deduplication (PARTIALLY RESOLVED)

`emit_base.py` now contains shared logic: function naming, array refs, dispatch groups, underscore replacement, field collection. Both emitters import from it. Further deduplication possible but diminishing returns until a third emitter is added.
