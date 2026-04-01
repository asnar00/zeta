# journal
*development log for zeta*

## 2026-04-01: first zero website — from streams to servers

### the stream insight

Started the day by reviewing the codebase: zeta (zero-to-any translator), ziz (self-hosting), and atoz (anything-to-zero). The self-hosting work had produced two translated functions (bracket matching, split stream parts) and a solid foundation of 363 tests.

The user wanted to shift gears — build outward-facing applications instead of self-hosting. The goal: a simple website matching the existing nøøb landing page, built in zero.

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

The user chose #3: `response$ <- http-response(request, body)`. The response carries its request. The platform matches them up. This also makes concurrent request handling possible — each response is stamped with which request it answers.

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

The user preferred `landing-page` over `landing_page`. A centralized `ID`/`W` regex pattern was defined as the single source of truth for identifier matching across the parser, feature parser, and emit_base. Hyphens stay as hyphens through the IR; conversion to underscores happens only at the emit boundary (Python/TS don't allow hyphens in identifiers).

This touched ~50 regex patterns in the parser but was done as a mechanical replacement using the `W` variable.

### RPC eval

The admin endpoint was replaced with a generic RPC endpoint. The user's insight: send actual zero syntax over HTTP. `/@rpc/set feature var ("landing-page-enabled") ("false")` — the part after `/@rpc/` is a zero expression, URL-encoded.

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

### commits
- `b92518a` Per-user context: shared/user variable scoping with contextvars
- `691efda` Login feature with per-session context isolation
