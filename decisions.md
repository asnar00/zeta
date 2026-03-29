# decisions
*log of autonomous improvement decisions*

Each entry records: what I chose to do, why, what changed, and the outcome.

---

## 1. fix 3-step streaming bug (2026-03-29)

**What:** Fixed the emitter bug where 3+ step streams with a terminator generated wrong code.

**Why:** This was a known bug documented in ideas.md. The emitter used `steps[1]` as the repeat expression regardless of how many steps there were, so `a$ <- 0 <- 1 <- (a$ + a$) until (a$ > 50)` would loop on `1` forever instead of using the formula.

**Fix:** With a terminator present, steps[0..N-2] are initial seeds (appended once before the loop), and steps[N-1] is the repeat expression used inside the loop. Applied to both Python and TypeScript emitters.

**Tests added:** 4 new execution tests — 3-step with until, 3-step with while, 4-step no terminator, 3-step no terminator.

**Outcome:** 293 tests pass. The workaround note in ideas.md can be considered resolved.

---

## 2. add TypeScript execution tests, fix 2 TS emitter bugs (2026-03-29)

**What:** Created `test_execution_ts.py` with 30 tests that compile zero to TypeScript and run the output with `npx tsx`. This mirrors the Python execution tests. Found and fixed 2 bugs.

**Why:** The existing TS emitter tests only checked string output, never that the generated code actually *runs*. Running it immediately found two real bugs.

**Bug 1 — if/else blocks emitted raw AST dicts:** `_emit_if_block_ts` used `_emit_expr` for branch body statements, which doesn't handle `assign` nodes. Fix: use `_emit_stmt` instead, threading `result_type` and `async_fns` through.

**Bug 2 — result variable scoping in if/else:** TS uses `const` for all assignments, but if/else branches need the result var declared as `let` at function scope. Fix: detect functions with if_blocks, emit `let result: type;` at the top, and use bare assignment inside branches.

**Tests added:** 30 TS execution tests covering: variables, structs, functions, ternary, recursion, arrays, map, reduce, streaming, 3-step streaming, if/else, slicing, indexing, where, sort, enums, named fn map, bitwise, length, self-map, chained ops.

**Outcome:** 323 tests pass (293 prior + 30 new).

---

