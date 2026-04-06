# journal
*development log for zeta*

## 2026-03-28: day one — the translator exists

### the approach

Ash had been thinking about zero for years — a natural language programming language designed for agents to write, but easy for humans to read. Four previous attempts at a compiler. This time: use Claude Code as the builder, with Ash making design decisions and Claude doing the mechanical work.

The starting point: `zero.md` (the language spec), `zero-to-py.md` and `zero-to-ts.md` (translation example pairs), and the idea for `zeta.py` — a translator that reads zero source and emits Python or TypeScript.

### what got built

In one session:
- **parser.py** — zero source → IR dict. Types (numeric, enum, struct, composition), variables (scalar, array, stream), functions (open syntax, named result), tasks (coroutines with stream I/O). Two-pass: first collects signatures, second resolves calls.
- **emit_python.py** — IR → Python. NamedTuples for structs, generators for tasks, isinstance dispatch for multiple dispatch.
- **emit_typescript.py** — IR → TypeScript. Interfaces + factory functions for structs, generator functions for tasks.
- **Execution tests** — not just string matching, but actually running the generated code and checking results.

Found and fixed 7 bugs by running the emitted code. Brought TypeScript to parity with Python.

### platforms

Added the platform concept: abstract function declarations in `.zero.md` files, concrete implementations in `.py`/`.ts` files. `io.zero.md` declares `read file`, `write file`, `print`. Platform code gets prepended to the output.

### the first bug hunt

The execution tests immediately caught real bugs that string-matching tests missed: functions returning wrong values, types not mapping correctly, struct fields not being initialized. The lesson: generated code that *looks* right may not *be* right. Always execute.

### commits
- `8bb8241` Initial commit: zeta translator for the zero programming language
- `334262f` Add execution tests, fix 7 bugs found by running emitted code
- `b1b8ae3` Bring TypeScript emitter to parity with Python
- `5d1360c` Add platform I/O, markdown code extraction, abstract functions

---

## 2026-03-29: hardening, hello world, and self-hosting begins

### morning: robustness

Started with error recovery (`recover=True` mode), shared emitter base (`emit_base.py` for function naming, dispatch, field collection), structured logging (`--verbose` with timed sections), and parser warnings for unrecognized lines.

Fixed the 3-step streaming bug: with a terminator, steps[0..N-2] are initial seeds, steps[N-1] is the loop repeat. Both emitters updated.

### TypeScript execution tests and strict mode

Created 30 TypeScript execution tests that actually run the generated code with `npx tsx`. Found bugs invisible to string tests:
- if/else blocks emitting raw AST dicts instead of code
- result variable scoping in if/else (needed `let` at function scope)
- type composition missing inherited parent fields
- type annotations using zero names (`int`) instead of TS names (`number`)

Added 26 `tsc --strict` tests. All generated TypeScript passes the real TypeScript compiler in strict mode. By end of session: 359 tests.

### ᕦ(ツ)ᕤ hello world

The first zero program compiled to both targets and ran:
```zero
feature zeta
string logo = "ᕦ(ツ)ᕤ"
on (string out$) <- main (string args$)
    out$ <- logo
```

Added `@zero` source comments to all generated functions, `zero.sh` runner script, platform runtime harness.

### self-hosting begins (ziz)

Started translating zeta's own functions from Python to zero, bottom-up. First function: `_matching_paren` → `matching (pair) in (s) after (start)`.

The translation process (documented in `atoz.md`):
1. **What not how** — describe in one sentence
2. **Decompose** — separate mixed concerns
3. **Match to constructs** — each concern → one zero construct
4. **Generalise** — decomposition reveals what's really a parameter
5. **Name naturally** — call sites should read like English

**Accumulator extraction rule**: every mutable variable that changes across loop iterations IS a stream. Extract it, name it, query it.

The bracket matcher decomposed into a task (tracks depth) and a query (`index of first ... where`). Two hardcoded Python functions became one parameterised zero function.

New language features added to support this: `onwards` slice syntax, `index of first in [...] where (...)`, task call prefix (`task_` instead of `fn_`), task calls inside function bodies.

### feature composition

Built the feature composition pipeline: `feature_parser.py` extracts features/extensions, `composer.py` applies before/after/on/replace, `zeta.py` wires it all together with per-feature output files, cross-module imports, and test infrastructure.

The parser feature (`parser.zero.md`) extends the zeta feature — first real test of composition. Tests from both features compose correctly.

### commits
- `b0d2416` Add error recovery, shared emitter base, and structured logging
- `093f911` Fix 3-step streaming bug in both emitters
- `949afe6` through `81e9a08` — TS execution tests, strict mode, bug fixes
- `7f07d16` ᕦ(ツ)ᕤ hello world
- `03d40ff` through `dda3c7b` — platform harness, zero.sh, source comments
- `e84dc6b` through `0643f5f` — new language features for self-hosting
- `6adb464` First zero-translated function passes its test
- `6d715b4` through `472103e` — feature composition pipeline

---

## 2026-03-30: split stream parts and language refinements

### the decomposed stream translation

Translated `_split_stream_parts` from Python to zero. The imperative version: one loop, three accumulators (`depth`, `current`, `parts`), two-character lookahead. The zero version: 6 lines of declarative parallel streams.

```zero
on (string part$) = split stream parts (string s)
    string padded = s + "<-"
    int depth$ <- bracket depth of (padded) matching ("()")
    bool lt$ <- (char (_) of (padded) == "<")
    bool is_sep$ <- (lt$[_ - 1] and char (_) of (padded) == "-" and depth$ == 0)
    int pos$ = indices of [is_sep$] where (_)
    string part$ = trim (split [padded] at [pos$])
```

Each accumulator became one line. The `_` index reference replaced manual position tracking. A sentinel (`s + "<-"`) replaced post-loop cleanup.

### test framework from interface examples

Built a test framework that derives tests from `=>` examples in `.zero.md` interface sections. Feature parser extracts `call => expected` pairs, emitters generate test functions, shared runtime provides test registry. Tests auto-discovered from the zero source.

### language syntax refinements

- Replaced implicit consume loop with explicit `for each (name) in (stream$)` — clearer intent
- Replaced `to_chars`/`string$` lens with `char (i) of (s)` and `s.char$` — more natural

### commits
- `3a3d166` Test framework
- `90f50d2` Split stream parts: decomposed streams
- `db2d186` Document test framework, decomposed streams, architecture
- `8629598` Replace implicit consume loop with explicit for each
- `0af35c6` Replace to_chars/string$ lens with char (i) of (s)

---

## 2026-04-01: first zero website — from streams to servers

### the stream insight

Started the day by reviewing the codebase: zeta (zero-to-any translator), ziz (self-hosting), and atoz (anything-to-zero). The self-hosting work had produced two translated functions (bracket matching, split stream parts) and a solid foundation of 363 tests.

Ash wanted to shift gears — build outward-facing applications instead of self-hosting. The goal: a simple website matching the existing nøøb landing page, built in zero.

The key design conversation was about how a web server looks in zero. The breakthrough: **a server is a stream of requests, and the website is a stream transformer.** This unifies everything — a terminal app transforms stdin→stdout, a web server transforms requests→responses, a GUI transforms events→views. Same pattern, different streams.

### platform design

Two platforms emerged:
- **terminal** — provides `out$` (stdout) and `in$` (stdin)
- **http** — provides `request$` and `response$`

The `use` keyword was born: `use terminal.out$` and `use http.request$, http.response$` bind platform streams into the feature. The feature writes to `out$` and `response$` without knowing where they go — the platform decides.

### the response pairing problem

How does the response get routed back to the right request? Three approaches were discussed:
1. A callback function (`respond to (request) with (body)`) — works but not stream-like
2. A paired stream where response$ is the output end — cleaner but pairing is implicit
3. An `http-response` type that carries the request it belongs to — explicit, data-driven

Ash chose #3: `response$ <- http-response(request, body)`. The response carries its request. The platform matches them up. This also makes concurrent request handling possible — each response is stamped with which request it answers.

### coroutine fusion

All tasks are coroutines. The emitter defaults to async coroutines (handles I/O naturally), but can fuse pure computational chains into plain loops. The zero programmer writes decomposed streams; the optimizer decides the execution strategy. This was captured in ideas.md.

### building it

The implementation required:
- `use` declarations in the feature parser, composer, and main parser
- Void tasks (tasks with no declared output stream that push to platform-owned streams)
- Abstract tasks (platform-declared, no body — like `serve http`)
- `emit_external` AST nodes for pushing to platform streams
- Platform Python/TS implementations for terminal and http
- Thread-based HTTP bridging (queue between push-based HTTP and pull-based generators)

The website compiled and ran on port 8084 — the test domain via Cloudflare tunnel. 13 lines of zero, working in both Python and TypeScript.

### commits
- `78b41fc` First zero website: platform streams, use declarations, http/terminal platforms

---

## 2026-04-01: feature composition and dynamic features

### handle request and the three versions

Added `handle request` as an extensible function. Three composable features:
- **website** — base server, returns logo for all paths
- **not-found** — replaces `handle request` to return "not found"
- **landing-page** — adds root path check before the default handler

Three versions by changing features.md: v1 (logo everywhere), v2 (not found everywhere), v3 (HTML at root, not found elsewhere).

### result-as-return

The composition raised a key language question: how does a `before` extension early-out? The answer: **assigning the result variable ends the function.** No `return` keyword needed. If the before-extension's `if` block assigns `body`, the fallback `body = not found ()` is guarded with `if body is None:`. The emitter generates the guard automatically when it detects multiple assignments to the result variable, with at least one inside a conditional.

This was a deliberate design choice. The alternative — an explicit `return` keyword — was considered but rejected because it would undermine the named result convention. And having both would create two ways to return, which is confusing.

### simplified features.md

Features.md evolved from listing file paths to a configuration format:
```
website port=8084
not-found
landing-page dynamic
```
- Names are searched automatically (no path prefix needed)
- `key=value` overrides feature-scoped variables
- `dynamic` wraps the feature's extensions in a runtime enable guard

### dynamic features

The `dynamic` keyword injected `landing_page_enabled` as a guard variable. The emitter's `_wrap_dynamic` injected `and feature_enabled` into the first `if` condition of the extension. This allowed toggling features at runtime without recompiling.

### commits
- `210f037` Feature composition: handle request, result-as-return, dynamic features

---

## 2026-04-01: runtime admin and RPC

### admin endpoint

Added an admin feature that intercepted `/@admin/` paths to get/set feature variables and call functions. Used `setattr` on the root module to toggle `landing_page_enabled` at runtime. The `exit process` platform function used `os._exit(0)` with a 0.5s delay for graceful shutdown.

### zero.sh runner

`zero.sh` evolved from a simple runner to a full lifecycle tool: `--stop`, `--restart`, `--fg`, `--test`. Searches for compiled output files by name.

### hyphenated identifiers

Ash preferred `landing-page` over `landing_page`. A centralized `ID`/`W` regex pattern was defined as the single source of truth for identifier matching across the parser, feature parser, and emit_base. Hyphens stay as hyphens through the IR; conversion to underscores happens only at the emit boundary (Python/TS don't allow hyphens in identifiers).

This touched ~50 regex patterns in the parser but was done as a mechanical replacement using the `W` variable.

### RPC eval

The admin endpoint was replaced with a generic RPC endpoint. Ash's insight: send actual zero syntax over HTTP. `/@rpc/set feature var ("landing-page-enabled") ("false")` — the part after `/@rpc/` is a zero expression, URL-encoded.

The RPC handler parses the expression at runtime:
- No `(` and no `=` → variable get
- Contains `=` without `(` → variable set
- Contains `(` → function call (match by name words and argument count)

`get/set feature var` were then removed — RPC handles var access natively. The admin feature became 3 lines of zero.

### directory listing

`/@rpc/` with no expression returns a directory — features, variables, and functions with original zero signatures extracted from `@zero` source comments. Grouped by feature (composition order) then platform.

### zero-arg function calls

The parser's signature matcher was updated to accept trailing `()` on zero-arg calls. This fixed `landing page ()` and `not found ()` which previously failed to match. The fix also correctly updated a test expectation (`await run()` → `await fn_run()`).

### commits
- `133f306` Runtime admin: dynamic feature toggle via HTTP
- `0ed5092` Hyphenated identifiers, graceful stop, zero.sh runner
- `9e45058` RPC eval: zero syntax over HTTP, remove get/set feature var
- `bd165b6` RPC directory: zero syntax listing grouped by feature and platform

---

## 2026-04-01: testing framework

### platform tests

The string platform got test examples in its interface section (`trim`, `char`, `starts with`, `split by`, `length of`, `substring of`). 12 tests. Platform tests from `.zero.md` interface sections compile into the root feature's test runner.

### test via RPC

`test ()` and `test ("feature")` became platform functions callable via `/@rpc/test ()`. Tests run and output is captured as a string for the HTTP response. The RPC function dispatcher was updated to match by argument count for overloaded functions.

### test shadowing

Features declare tests for their own behaviour. When a child feature overrides a function, the parent's tests for that call expression may fail — and that's expected. The test runner detects this: if a test fails, but a later feature has a passing test for the same call expression, it's marked OVERRIDDEN (not FAIL).

The implementation: run all tests, collect results with call expressions. Build a set of passing call expressions. When reporting, if a fail's call matches a pass, it's overridden.

The `handle request (http-request(path="/"))` test in landing-page uses `read file ("website/index.html")` as the expected value — a function call, not a string literal. Exact match against the actual file content.

### commits
- `822df01` Clean up interface sections, fix hyphenated test names
- `68dc31e` Platform tests: string operations tested via interface examples
- `40e850f` Test via RPC and zero.sh: test (), test ("feature"), --test flag
- `2856ef4` Rename admin to rpc, add rpc eval interface tests
- `5d30081` RPC tests: add function calls with arguments
- `33ed8bd` Add howto.md
- `dedb346` Test shadowing: overridden tests detected across composed features

---

## 2026-04-01: design ideas captured

### multi-component deployment

A product can be multiple components on different devices — server, laptop, phone, tablet. The zero source describes *what*; a `components.md` describes *where*. Cross-component calls become RPC automatically. The emitter generates stubs and handlers.

### time-indexed streams

Streams can have a `rate` (values per second): audio at 44100, video at 30, game state at 60. `stream$[T]` indexes by time: `floor(T * rate)`. Cross-rate SSA: when streams with different rates appear in the same expression, the compiler generates resampling.

Every time-stream is observable via RPC: `/@rpc/audio$` returns the current value. `/@rpc/audio$[0.5 to 1.0]` returns a time slice.

### per-user context

Feature-scoped variables can be `shared` (one value, same for everyone) or `user` (one value per session). The context is an implicit per-session object that the runtime threads through all function calls using `contextvars` (Python) / `AsyncLocalStorage` (TypeScript).

### Claude Code atoz stress test

Anthropic leaked 512k lines of Claude Code TypeScript via npm source maps. Idea: translate it to zero using atoz once it's functional. The ultimate stress test.

### commits
- `6f574e2` Ideas: multi-component deployment across devices
- `dd78bd9` Ideas: time-indexed streams with rate, cross-rate SSA, real-time observability

---

## 2026-04-01: per-user context and login

### shared vs user scoping

The `global` keyword was rejected — it implies flat namespace, but variables are feature-scoped. `shared` and `user` were chosen instead:

```zero
shared int port = 8084          # one value, set at deploy time
user bool enabled = true        # per-session, on the context object
```

The distinction was added to `zero.md` as a language-level concept.

### context implementation

The emitter generates a `_Context` class with nested feature sections:

```python
class _Context:
    class landing_page:
        enabled: bool = True
    def __init__(self):
        self.landing_page = _Context.landing_page()
```

Python uses `contextvars.ContextVar`, TypeScript uses `AsyncLocalStorage`. `_get_ctx()` returns the current context. User vars accessed as `_get_ctx().feature.var`.

The `dynamic` keyword in features.md was removed — features now declare their own `user bool enabled` and write the guard explicitly. This is cleaner: the feature controls its own toggling.

### login feature

Two-step SMS login: `request login (phone)` generates a code, `verify login (phone) (code)` creates a session. The session token is returned to the caller. Subsequent requests carry the token as a cookie, and the HTTP platform switches to the session's context.

### the session isolation bug

A tricky bug: after disabling landing-page for one session, the default (no-session) context also saw it disabled. The debugging process revealed two issues:

1. **RPC arg coercion**: `_coerce_value("1234")` converted the string to int `1234`, so `1234 == "1234"` was false in Python. Fix: pass function args as strings, only coerce for variable assignment.

2. **Cross-module _sessions**: the runtime platform code is prepended to EVERY compiled module. Each module got its own `_sessions = {}` dict. `fn_create_session` in the login module added the token to login's `_sessions`, but the HTTP generator in the website module read website's `_sessions` — which was empty. Fix: store sessions on the root module via `_find_root_module()`, not in module-level `_sessions`.

### keyed collections

Added `string codes$[string]` map syntax — the key type in brackets. Parser distinguishes maps from sized arrays by checking if the bracket content is a type name. Python emits `dict`, TS emits `Map`. Indexed assignments (`codes$[key] = value`) are exempt from the SSA checker since they're collection mutations.

### user vs session

Design conversation clarified the distinction: a **user** is a person (permanent, has preferences and feature flags). A **session** is a temporary link between a request and a user (a browser tab, expires). Multiple sessions can point to the same user. The context comes from the user, not the session: `token → user → context`.

### persistence by default

All `shared` and `user` variables persist by default — no `persistent` keyword needed. The platform serialises on shutdown, loads on startup. Two files per application:
- `app.state.json` — shared variables
- `app.users.json` — user records with per-user feature variable overrides

The zero source doesn't change. Persistence is a platform concern.

### minimising the platform surface

Key design principle articulated: everything below the platform line costs one implementation per target. Patience pays — write it in zero where possible, even if it means extending the language. For login, the true platform primitives are: outbound HTTP request (for SMS via Vonage API), random number, current time, create/set session. Everything else — phone normalisation, code generation/storage/verification, the login flow — is zero code.

### login feature

Built the login feature with test users `_alice` (code "1234") and `_bob` (code "4321"). Two-step flow: `request login (phone)` generates and stores a code in a keyed collection, `verify login (phone) (code)` checks it and returns the user. A separate `login (phone) (code)` function wraps verify + `create session` to return a token.

Several issues surfaced during implementation:

- **`first of [...] where` with no match**: the Python `next()` threw `StopIteration`. Fixed to return the type's default value using `type(arr[0])()`.
- **Conditional result init**: functions with result assignments inside conditionals need the result variable initialised. Used `None` as sentinel (not the type default — that would break the `if result is None` guard). Added `return result if result is not None else type_default()` at the end.
- **Map variable marked as platform**: the `_platform` detection was too aggressive — marking all uninitialized arrays as platform vars when abstract functions exist. Fixed to skip vars with explicit `scope`.

### per-user background colour

The real end-to-end test: alice sees red, bob sees blue, anonymous sees teal. Required:

1. **background feature** (`background.zero.md`): declares `user string colour = "#34988b"`, extends `landing-page`.
2. **`after` extension**: `after (string body) = landing page ()` — post-processes the HTML, replacing the default teal with the user's colour via `replace ("#34988b") in (body) with (background.colour)`.
3. **`replace` string platform function**: added to both Python and TS platforms.
4. **Result variable SSA exemption**: the `after` extension reassigns `body` (already set by the original `landing page`). The SSA checker now exempts the result variable — extensions are the one place where reassignment is allowed.
5. **Cross-module context fix**: `_get_ctx()` was reading from the local module's `_ctx_var` instead of the root module's. Each module has its own `_ctx_var` (platform code is prepended to every module). Fixed `_get_ctx()` to always read from `sys.modules['__main__']._ctx_var`.
6. **var_decl SSA tracking**: the SSA checker wasn't tracking `var_decl` in the assigned set, so `int x = 1; x = 2` wasn't caught. Fixed.

The demo: login as alice via RPC, set `background.colour = #ff0000`, login as bob, set `background.colour = #0000ff`. Each user's landing page shows their colour. The default (no login) shows teal.

### literate style

Added intro paragraphs before every code block in all six website feature files. The zero convention: each code block is preceded by a natural-language explanation of what it does and why.

### commits
- `b92518a` Per-user context: shared/user variable scoping with contextvars
- `691efda` Login feature with per-session context isolation
- `485c4d6` Keyed collections: string$[string] map declarations, store, retrieve
- `99a3892` Journal: keyed collections, user vs session, persistence design
- `af1fa32` Per-user background colour: login, after extension, cross-module context
- `7105ad2` Literate style: add intro paragraphs to all website feature definitions
- `34b820e` Journal: login, per-user background, after extension, cross-module context

## 2026-04-03/04: refactoring, language evolution, integration test

### 25-line function limit

Refactored every `.py` file in the pipeline so all functions fit on one screen (≤25 lines). Commented code blocks become named functions. The codebase went from having dozens of 50-150 line functions to ~400 focused functions across 7 files. Documented as key decision #10 in zeta-architecture.md.

### language changes

**Uppercase types:** User-defined types now start with uppercase (`User`, `Http-Request`, `Vector`). Built-in types stay lowercase (`int`, `string`, `bool`). The parser's `_looks_like_type` now uses capitalisation instead of returning True for everything.

**Per-user variable default:** Variables in feature scope are per-user by default. The `user` keyword is removed. `shared` prefix opts a variable into global/module scope. This matches the mental model: most state is per-user, shared state is the exception.

**`raise` and exception handlers:** `raise unknown user (name)` throws a named exception. `in login (), on unknown user (string name)` wraps login's body in try/catch that dispatches to `on unknown user` — a regular extensible function. This lets the happy path read straight through with no conditionals.

**`...` placeholders:** `... send (code) to (found)` compiles to a no-op and is reported by the build. Marks deliberate gaps for future implementation.

**Section-aware markdown extraction:** `_extract_code` now tracks `##` sections and only extracts code from `## interface`, `## definition`, `## tests`. Other sections (`## specification`, `## integration tests`) are skipped.

### feature tree and directory

`directory()` split into `features()` (box-drawing tree with summaries) and `functions()` (the old directory listing). Zeta emits `_FEATURE_TREE` data into the root module at build time. Feature files reorganised into a tree matching the extension hierarchy.

### gui platform

New `platforms/gui.zero.md` with `input (string prompt)`, `set cookie of (string name) to (string value)`, `reload page ()`. Implemented for Python (server fallback) and TypeScript (DOM).

### connector → just an extension

The `## connector` section was proposed then removed. Wiring a feature into an app is just a regular extension in the `## definition` section. Adapters are a future topic.

### integration test

Playwright-based test in `test_integration_login.py` implements the spec from `login.zero.md ## integration tests`: three browser tabs, three users (default, alice, bob), three background colours. Currently passes using RPC-based login (HTTP calls to request login, complete login, set cookie). Next step: client-side JS emission so login() runs in the browser.

### decisions for next phase: cross-component RPC

The login flow needs to run across two components:
- **Server:** `request login`, `verify login`, `complete login`, `create session` (access shared data)
- **Client (browser):** `login`, `logo clicked` (call `input`, `set cookie of`, `reload page`)

When client-side `login()` calls `request login(name)`, that crosses the boundary and becomes a `fetch("/@rpc/...")` call. The approach:
1. Emit `login()` as JavaScript served in the HTML page
2. Server-side function calls from client code compile to `fetch` calls to the existing RPC bridge
3. Each browser context has its own session cookie, so the RPC bridge routes to the right per-user context

### cross-component RPC — implemented

Data placement is inferred from platform annotations. The gui platform is marked `@client` in gui.zero.md. The compiler:

1. **Classifies functions:** walks the IR, checks which platform functions each function calls. Direct callers of `@client` platform functions are client-side. Propagates transitively (logo_clicked calls login which calls input → both are client).
2. **Builds a client IR:** extracts client functions, identifies server functions they call as RPC targets.
3. **Emits client.js:** uses the TS emitter with `_rpc_targets` set — server calls become `await _rpc("...")` using the existing `/@rpc/` bridge. All client functions are async.
4. **Serves client.js:** HTTP handler serves `/@client/` files from the output directory. Landing page HTML includes `<script src="/@client/client.js">`.

The gui platform has two implementations: `gui.ts` (sync, server fallback using `window.prompt`) and the client bundle which has async DOM-based implementations (creates styled input elements, waits for Enter key).

Key insight: `window.prompt()` blocks the event loop and can't coexist with `async/await fetch()` in headless Chrome. The DOM-based input implementation (creates an element, returns a Promise) works correctly with async flows.

The integration test now uses actual browser interactions: Playwright clicks the logo, types into DOM input elements, presses Enter, and verifies the page reloads with the correct session cookie and background colour.

### commits
- `c2f0dab` Extract named functions: 25-line limit across all pipeline .py files
- `6788540` Feature tree, features()/functions(), per-user variable default
- `cd67437` Uppercase types, login-by-name, TS strict fixes, per-user variable default
- `fac6bc3` Connector section, login entry point, undefined call handling, nested if fix
- `c95752d` raise, ... placeholders, gui platform, happy-path login
- `5c27d3a` Exception handlers: in X(), on Y() wraps function body in try/catch
- `1c94114` Integration test passes: 3 tabs, 3 users, 3 background colours
- `0860c81` Client-side JS emission: browser login with DOM inputs, cross-component RPC
- `e04f65a` Session persistence: sessions survive server restarts
- `d3121c7` SMS platform: send real verification codes via Vonage
- `02def24` Platform tests: string, runtime, gui test specs and runner
- `fe135ad` Named sessions: login restores user's context across restarts
- `8535a16` Rename functions for natural language readability
- `09d5164` Toggle login: click logo to login or logout depending on session state
- `4668594` Toggle login/logout integration test (13 tests), HTTPS cookie fix
- `ffd78b4` Cache-busting client.js, tests through Cloudflare HTTPS

### the Cloudflare caching bug and its lessons

The login/logout toggle was implemented and all 13 integration tests passed — but the real site at `test.nøøb.org` was completely broken. Clicking the logo always showed login, never logout. The root cause: Cloudflare cached the old `client.js` which didn't contain the toggle logic. Every code update was invisible to the user.

This exposed three things:

**1. Cache-busting is a deployment requirement.** Any system serving client code through a CDN needs content-hashed filenames. The build pipeline now generates `client.HASH.js` and updates the HTML script tag on each build. Cloudflare sees a new URL and fetches fresh content.

**2. Tests must go through the real deployment path.** The integration test was running on `http://localhost:8084` — bypassing Cloudflare, HTTPS, and the CDN cache entirely. It gave false confidence. The test now runs through `https://test.nøøb.org/` (the Cloudflare URL). There's no point testing a path the user never takes.

**3. The build pipeline manages deployment, not just compilation.** Hashed filenames, script tag injection, cache headers — these aren't optional extras. They're part of what "building" means for a system with client-side code. As more client features are added, this deployment layer will grow and may deserve its own section in the architecture.

### observability: building in progress

**Architecture decided:** Three platform layers for cross-component communication:
- `websocket` — raw bidirectional channels (implemented, tested through Cloudflare)
- `remote` — request/response protocol with JSON `{id, cmd}` / `{id, result}` messages (implemented, 11 tests pass)
- `eval` — every component can parse and evaluate zero expressions (server: delegates to rpc eval; client: JS port of the same parser)

**Unified eval model:** RPC and remote are the same thing — send a zero expression to a component, get a result. The transport (HTTP GET, WebSocket) is irrelevant. Every component has a function registry and a minimal expression parser.

**Session-aware routing (in progress):** Commands are addressed to users, not channel IDs. `request ("get cookie (\"session\")") on ("_alice")` routes to `_alice`'s browser via their WebSocket. The server reads the session cookie from the WebSocket upgrade request and maintains a `user_name → channel_id` mapping. Test users (`_alice`, `_bob`) are used for test sessions.

**GUI action primitives:** `click on`, `type into`, `press on` — all execute on the client via WebSocket. Full login flow tested via 15 WebSocket-only commands. Anonymous connections routed as "anonymous" so the login flow can be tested from scratch. After login + reload, the WS reconnects with the session and routes by user name.

**`describe page ()`:** Single call returns full DOM snapshot — tag, classes, position, size, font, colour, background, text, input state. One round-trip gives enough information for layout assertions, overlap detection, visibility checks.

**Decision log:**
- WebSocket GUID must be exact `258EAFA5-E914-47DA-95CA-C5AB0DC85B11` (spent time debugging wrong GUID)
- HTTP server must be `ThreadingHTTPServer` for concurrent WebSocket connections
- WebSocket handshake must write directly to socket (BaseHTTPRequestHandler defaults to HTTP/1.0)
- Client eval uses same algorithm as server `rpc eval`: `_extract_fn_words` → `_extract_args` → `_find_function`
- Client WebSocket auto-reconnects on disconnect
- User names as channel addresses (not opaque IDs) — natural, works for drones/phones/browsers
- Anonymous connections get sequential names (guest-1, guest-2) so each browser is individually addressable in tests
- Multi-browser integration test: each tab is a separate guest or named user, test routes commands to specific browsers by name
- Test's own WebSocket gets a guest name too — must be excluded when discovering browser guests
- After logout, browser reconnects with a new guest name — test discovers it dynamically
- RPC eval now handles method-style calls like (s) contains (sub) via signature matching
- `in X, on Y` inside function bodies: next language feature for test/hook composition

### zero tests: the design

Tests are zero code. No separate test framework, no string-building, no route management. The test sets up hooks on function calls, then runs the flow:

```
on test login ()
    in login (), on input ("name")
        type ("_alice") into ("input")
        press ("Enter") on ("input")
    in login (), on input ("code")
        type ("1234") into ("input")
        press ("Enter") on ("input")
    login ()
    check (describe page ()) contains ("log out")
```

Key principles:
- Component/route concerns never appear in zero code — the compiler figures out where to run each call
- `in X, on Y` hooks work inside function bodies (not just at feature level) — sets up interception before calling X
- The same code is a test (check assertions), a demo (add narration), or documentation (the code IS the spec)
- Tests are part of the feature, not a separate file or framework

**Milestone: Playwright reduced to browser-opener.** The integration test (17 tests) now uses Playwright only to open browser contexts. All interaction (click, type, press) and all assertions (describe page, get cookie, background colour) go through the WebSocket remote channel. This is the observability model: the application tests itself through its own communication infrastructure.

### commits (continued)
- `d181228` Observability: route commands to browser via session-aware WebSocket
- `5835608` describe page (): full DOM snapshot for observability
- `0a78661` GUI action primitives: click, type, press via WebSocket
- `f775488` Journal update, anonymous WS routing
- `34b30a1` WebSocket-based integration test, sequential guest naming
