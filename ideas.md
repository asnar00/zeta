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

## time-indexed streams

Streams can be time-indexed by declaring a `rate` (values per second):

    float audio$ (rate = 44100)
    float volume$ (rate = 1000)
    world state$ (rate = 60)
    frame pixels$$ (rate = 30)
    int plain$                     # rate = 0, position-indexed (default)

**Indexing:** `stream$[T]` where T is a time in seconds does `stream$[floor(T * rate)]`. For `rate = 0`, T is the index directly (today's behaviour).

**Cross-rate SSA:** when streams with different rates appear in the same expression, the compiler generates resampling:

    float output$ = audio$ * volume$

`audio$` at 44100Hz, `volume$` at 1000Hz — the compiler knows every 44.1 audio samples, volume advances one step. No manual interpolation.

**Observability via RPC:** reading a time-stream via `/@rpc/audio$` returns the current value (at the current time). `/@rpc/audio$[0.5 to 1.0]` returns a time slice — half a second of audio. Every stream becomes observable and debuggable in real-time.

**What this unifies:**
- Audio: time-stream of samples
- Video: time-stream of frames
- Game state: time-stream of world snapshots
- Sensor data: time-stream of readings
- UI state: time-stream of view models
- Network events: time-stream of messages

All are SSA on time-indexed sequences. The same decomposition rules (one concern per stream, parallel streams, query with `first of`/`where`) apply to real-time data.

**Platform scheduling:** a task producing a time-stream is a coroutine that yields at its declared rate. The platform schedules it — real-time for audio/video, as-fast-as-possible for computation. The coroutine fusion optimiser (see above) can merge pure computational chains into tight loops regardless of rate.

**Cross-device streaming:** a time-stream produced on one device (camera on phone, `rate = 30`) can be consumed on another (renderer on laptop) via the RPC bridge. Rates are declared, so the consumer knows how to resample if needed.

## multi-component deployment: one product, multiple devices

A product is made of multiple components running on different devices — server, laptop, phone, tablet — all forming a single interface. The zero source describes *what* (features, functions, data). A separate `components.md` describes *where* (which component runs which features, which target language).

**Example:**
```
component server (py)
    website
    not-found
    rpc

component laptop (ts)
    debugger

component phone (ts)
    colour-picker
    chat
```

**Build:** each component gets its own compilation pass — same zero source, different target, different feature set. Output is one binary/bundle per component.

**Cross-component calls:** when the laptop's debugger calls `pick colour ()` which lives on the phone, the emitter generates an RPC stub on the laptop and a handler on the phone. The call looks local in zero — the deployment config decides it's remote, and the emitter generates the plumbing.

**The RPC bridge already exists** — `rpc eval` parses zero syntax and dispatches to compiled functions. Cross-component calls are the same mechanism over WebSocket/HTTP between devices instead of within one process.

**Per-component context:** each device has its own user identity, permissions, and feature flags. The context object (see per-user context idea) travels with RPC calls so the receiving component knows who's asking.

**No assumptions in zero source:** a feature doesn't know what device it runs on. The same `colour-picker` feature could run on a phone, a tablet, or embedded in the laptop UI. The deployment config decides.

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

## raise / on: happy-path-only code with external error handling

Zero code should describe the happy path. Error conditions are handled separately, by whoever is composing the feature into their application.

**`raise`** signals a named error condition with arguments:

    on (string code) = request login (string name)
        User found = first of [users$] where (_.name == name)
        code = generate code (found)
        pending-codes$[found.phone] = code

Here `first of ... where` raises `not found` internally if nothing matches. The feature author doesn't write error-handling code — they write what the function does when everything works.

**`in X, on Y`** handles a named error within a specific call:

    in login (), on not found ()
        show message ("unknown user")

    in login (), on invalid code (string code)
        show message ("invalid code")

The handler sits outside the call stack. It can:
- **Abort** — show a message, don't resume (simplest, implemented first)
- **Fix and resume** — correct the input, return to the raise point (future)
- **Retry** — ask the user again, re-enter the call (future)
- **Delegate to an agent** — let an AI figure out what to do (future)

**Separation of concerns:**
- Feature author writes the happy path
- Platform functions raise when things go wrong
- Integrator decides what to do about it (via `in X, on Y` handlers)

**Implementation (v1):** `raise name (args)` compiles to throw/raise an exception carrying the handler name. `in X, on Y` compiles to try/catch around the call to X, matching by handler name. No resumption in v1.

**`...` placeholders:** The `...` prefix marks a deliberate gap — a function call that doesn't exist yet:

    ... send (code) to (found)

Compiles to a no-op. The build reports placeholders separately from errors. A coding agent seeing `...` knows "this is where future work plugs in."

## feature-granular logging

Per-user, per-feature logging that compiles to a single condition check when disabled:

    log ("request login called for " + name)

Compiles to:

    if _get_ctx().login.logging:
        _emit_log("login", "request login called for " + name)

Each feature gets a `logging` boolean in its context section (per-user, defaulting to false). Toggle live:

    /@rpc/login.logging = true

Log messages route through the WebSocket for real-time streaming. A debugging tool connects to a user's session and subscribes to their log stream — see exactly what their browser or server code is doing.

Minimal overhead when off (one condition check). Instant visibility when on. Per-user so production traffic is unaffected while debugging one user's session.

**Relationship to blackbox:** Feature-granular logging is for *live* debug streaming (developer watches a user's session in real time). Blackbox is for *post-hoc* fault capture (user reports a problem, recording is uploaded later). They share the same `log()` call site — feature-granular logging streams it live, blackbox captures it silently in the ring buffer. Complementary, not competing.

## emitter deduplication (PARTIALLY RESOLVED)

`emit_base.py` now contains shared logic: function naming, array refs, dispatch groups, underscore replacement, field collection. Both emitters import from it. Further deduplication possible but diminishing returns until a third emitter is added.
