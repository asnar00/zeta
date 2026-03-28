# ideas
*future directions for zeta*

## additional emitters

### rust
- Most natural target after Python/TS — types map 1:1 (`int32`→`i32`, `uint8`→`u8`, `float32`→`f32`)
- Struct parameters as `&T` (immutable reference) since zero is pure — safe and avoids copies
- Immutability by default matches zero's SSA
- Concrete type enforcement happens natively — no wrapping/masking needed
- Ternary → `if cond { a } else { b }` (expression in Rust)

### C / C++
- Very similar to Rust in terms of type mapping
- C++: `const&` for struct params; C: `const*` with `->` member access
- Concrete types via `<stdint.h>` (`int32_t`, `uint8_t`, etc.)
- Struct construction differs slightly between C and C++

### WebAssembly (WAT)
- Low-level typed bytecode — more complex emitter
- Concrete numeric types map directly to wasm types (`i32`, `i64`, `f32`, `f64`)
- Structs need flattening — fields passed individually or via linear memory
- Expressions must be reordered into postfix (stack-based)
- No ternary — requires `if/else` blocks
- Zero's purity and SSA map well to wasm's design

### direct assembly (ARM / RISC-V / x86)
- Big jump: need register allocation, instruction selection, struct layout, calling conventions
- Zero's SSA simplifies register allocation — each variable assigned once, naive mapping possible
- Pure functions mean no aliasing concerns
- Simple types mean no vtables or dynamic dispatch
- Could emit `.s` files and use system assembler/linker, or go fully self-contained

## machine code learning via probes

Use a C compiler as a *teacher* to learn architecture-specific instruction patterns:

1. Generate small C probe functions covering primitives: int add, float add, struct field load, function call, return, etc.
2. Compile with `-O2` on the target architecture
3. Disassemble and extract instruction patterns
4. Generalise patterns (replace specific registers with placeholders)
5. Store as a pattern file per architecture
6. The assembly emitter stitches patterns together — no architecture manual needed

**Advantages:**
- Get optimised instruction sequences for free from the C compiler
- Portable — run probes on ARM → ARM patterns, RISC-V → RISC-V patterns
- Self-bootstrapping — C compiler only needed once at probe time, then patterns are stored
- Testable — verify stitched output against C compiler output for known functions
- Incremental — probe new patterns as language features are added

**Challenges:**
- Register flexibility — probes use specific registers, need to generalise
- Addressing modes — struct field access at varying offsets
- Calling conventions — fixed per architecture, but need probing

Could be a separate tool: `zeta-probe.py` generates C, compiles, disassembles, writes pattern files.

## concrete type enforcement

Currently deferred. When implemented:

- **Integer assignment wrapping** — `int8 c = a + b` where result overflows must wrap (C-style modular arithmetic)
- **Python emitter** — wrap RHS in masking: `((a + b + 128) % 256) - 128` for int8, or helper functions
- **TS emitter** — bitwise tricks: `((a + b) << 24) >> 24` for int8 sign extension
- **Rust/C/C++ emitters** — native enforcement, no extra work
- **Float precision** — `float32` would need numpy/struct in Python; TS/JS floats are already float64

## int/float assignment semantics

Zero rule: if input and output vars are both int, the result is truncated on assignment. `(1 + 2) / 2` → `1` when assigned to `int`, `1.5` when assigned to `float`.

- Division itself is always float
- Emitters need type-aware assignment: Python wraps with `int(...)`, TS with `Math.trunc(...)`
- Requires at least basic type tracking in the emitter
- Simple approach: if result variable type is any int variant, wrap entire RHS in truncation

## atoz: anything to zero

The inverse of zeta — an agent-powered tool that reads existing code in any language and translates it *into* zero.

**Use cases:**
- **Bootstrap** — convert an existing C/Rust/Python library into zero, then emit to any target via zeta
- **Reverse engineer** — read unfamiliar source and produce a clean zero specification
- **Migrate** — adopt zero as the canonical source for a project, then emit to whatever target is needed
- **Document** — zero's literate style (natural language + code blocks) means atoz output is simultaneously documentation and source code

**Key insight:** zero's constraints (pure functions, SSA, open syntax) force the translator to decompose messy imperative code into clean functional units. It's not just syntax conversion — it's a structural refactoring.

**Why an agent, not mechanical:** unlike zeta (which must be deterministic), atoz requires understanding intent — choosing good open-syntax function names, writing natural language descriptions for the feature.md format, deciding how to split stateful code into pure functions. This is a natural fit for an LLM.

## self-hosting

If zeta can emit Rust (or C), we could write zeta's own translator in zero, emit it to a compiled language, and have a native zeta compiler that compiled itself. The path:

1. Write zeta's parser and emitters as zero features
2. Use the Python zeta to emit them to Rust
3. Compile the Rust output → native zeta binary
4. The native binary can now compile itself and any other zero program

This is more interesting than rewriting in Rust for performance — it proves the language is expressive enough to describe its own toolchain.

## round-trip correctness testing

The proof that atoz + zeta preserve semantics is: **original tests pass against the re-emitted code**.

**Pipeline:**
```
original source (e.g. Python)
  → atoz → zero features
  → zeta → target language
  → run original tests against new output
```

**Levels of confidence:**

1. **Same-language round-trip** — Python → zero → Python, original tests pass. Proves atoz + zeta preserve semantics.
2. **Cross-language round-trip** — Python → zero → Rust, same test cases pass. Proves emitters produce equivalent behaviour across languages.
3. **Multi-target agreement** — emit from zero to Python, TS, Rust, C — all produce the same outputs for the same inputs. Proves emitter consistency.

**Automated cross-compiler fuzzing:** for every zero function, generate random inputs, emit to all targets, run all of them, assert they all agree. Property-based testing across languages.

**Key insight:** atoz is lossy by nature — it restructures code, renames things, splits functions. The tests are the *only* ground truth for correctness. This is fine — that's what tests are for. The zero source doesn't need to *look* like the original, it just needs to *behave* like it.

## platform interfaces and agent-written bindings

Zero programs need to interact with target-language ecosystems (web servers, databases, file I/O, etc.) without coupling to any specific library.

**Architecture:**

```
zero feature (pure logic)
  + platform interface (abstract contract)
  + platform binding (target-specific, agent-written)
  → working program
```

**Platform interfaces** are declared in zero as abstract contracts:

```zero
platform web-server
    on (server s) = create server (string name)
    on (nothing) = handle (server s) route (string path) with (function handler)
    on (nothing) = start (server s) on (int port)
```

This defines *what* capabilities the program needs, not *how* they're provided.

**Platform bindings** are target-specific glue that maps the abstract interface to a real library. An agent generates these:

- Python → flask or fastapi wrapper
- TypeScript → express wrapper
- Rust → actix or axum wrapper

**Separation of concerns:**
- **zeta** (mechanical, deterministic) — translates zero logic to target language
- **agent** (creative, one-time) — writes platform bindings per target per interface
- **zero** (pure, portable) — the application logic, target-agnostic

**Conformance testing:** each platform interface defines a test suite ("create a server, add a route, make a request, check the response"). Every target's binding must pass the same conformance tests. This ensures that swapping targets doesn't change behaviour.

**Benefits:**
- Business logic is written once in zero, runs anywhere
- Platform bindings are isolated — changing from flask to fastapi only touches the binding, not the zero source
- Agents only need to write glue code, not business logic — a well-scoped task with clear inputs/outputs
- New platforms can be added without modifying existing zero features

## IR versioning and feature contracts

Parser and emitters must agree on a contract to prevent silent failures.

**Feature coverage:** the IR declares which language features the program uses, each emitter declares which features it supports. At emit time, check the diff and fail loudly if the emitter can't handle something.

**IR version:** bumped when the IR structure changes. Emitters declare which version they target. Version mismatch → immediate error, forcing emitters to be explicitly updated.

## language evolution and migration

When zero's syntax changes, existing zero source code needs updating.

**Migration pipeline:**
1. Zero syntax changes — parser bumps IR version
2. Emitters fail against old IR version (caught by version contract)
3. A migration agent reads old zero source + a syntax changelog → rewrites to new forms
4. Round-trip tests verify nothing broke

**Syntax changelog:** a versioned record of what changed and why:
```
version 2:
  - array declaration changed from "int i$" to "int[] i"
  - function keyword changed from "on" to "fn"
```

The migration agent reads the changelog, finds affected patterns in zero source, and rewrites them. Because zero features are markdown with natural language descriptions, the agent has context about *intent* — not just blind syntax transforms.

**Single source of truth:** the translation rules files (`zero-to-py.md`, `zero-to-ts.md`) define what each syntax means. When rules change, both the emitters and the migration agent reference the same docs. This keeps everything consistent across mechanical translation and agent-driven migration.

## streaming vs map: relationship to firm up

`j$ = i$ + 1` (map) and `j$ <- (i$ + 1)` (stream) produce the same result for the simple case. The map form is declarative whole-array transform; the stream form is element-by-element and more general (supports self-reference and while/until conditions).

Questions to resolve:
- Is map just sugar for the simple streaming case?
- Should the compiler desugar `j$ = i$ + 1` into `j$ <- (i$ + 1)` internally?
- Or are they semantically distinct (map = parallel-safe, stream = sequential)?
- Does this distinction matter for targets with SIMD or GPU parallelism?

## more array builtins to implement

- `reverse [a$]` → reversed array. Python: `list(reversed(a))`, TS: `[...a].reverse()`
- `sort [a$]` → sorted array. Python: `sorted(a)`, TS: `[...a].sort()`
- `[a$] starts with [b$]` → prefix match (defined in zero using slice + reduce)
- `[a$] ends with [b$]` → suffix match
- `[a$] contains [b$]` → sub-array search
- `split [a$] at [delim]` → array-of-arrays
- `join [a$$] with [sep$]` → flatten with separator
- `index of [b] in [a$]` → position of element

## task examples to develop

A more complex task example — a byte-to-codepoint decoder that consumes variable numbers of input elements per output:

```zero
on (int code$) <- extract codes from (uint8 bytes$)
    int out <- bytes$
    if (out & 0x80)
        int next <- bytes$
        out = (out & 0x7f) | (next << 7)
    code$ <- out
```

Each invocation consumes 1 or 2 bytes and emits 1 code point. Demonstrates M:N transduction.

Other task patterns to explore:
- Sum pairs (consume 2, emit 1): `int a <- s$; int b <- s$; out$ <- a + b`
- Run-length decoding (consume 1, emit N): pull a count and a value, emit value N times
- Tokenizer (consume M chars, emit 1 token): pull chars until whitespace, emit the word
