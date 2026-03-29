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

## 3. fix TS emitter: var_decl, reduce, assign in expressions (2026-03-29)

**What:** Added handling for `var_decl`, `reduce`, and `assign` AST nodes in the TypeScript emitter's `_emit_expr`. These were falling through to `return str(node)`, emitting raw Python dicts as code.

**Why:** Found while investigating what other bugs the TS execution tests would catch. Any function body with local variable declarations, array operations, or reduce expressions would emit garbage TS.

**Fix:** Added `var_decl` (with array and scalar variants), `reduce` (operator and function forms), and `assign` handlers to `_emit_expr` in `emit_typescript.py`.

**Tests added:** 3 new TS execution tests — reduce in function body, scalar var_decl in body, array var_decl in body.

**Outcome:** 326 tests pass.

---

## 4. fix TS emitter: type composition missing parent fields (2026-03-29)

**What:** TS emitter wasn't including inherited parent fields in struct interfaces and factory functions. `dog = animal + ...` would only emit `dog`'s own fields, losing `animal`'s fields.

**Why:** Found by the new TS execution tests. The Python emitter already had `_collect_all_fields` but the TS emitter used `typ["fields"]` directly.

**Fix:** Moved `_collect_all_fields` to `emit_base.py` (shared), updated both emitters to import it. TS `_emit_type` now uses all inherited fields.

**Tests added:** 2 new TS execution tests — task filter, type composition.

**Outcome:** 328 tests pass.

---

## 5. parser warnings for unrecognized lines (2026-03-29)

**What:** The parser now collects warnings for lines it can't parse, instead of silently skipping them. Warnings are stored in `ir["warnings"]` and shown in `--verbose` mode.

**Why:** Silent skipping is a major source of confusion — a typo in zero source means the line vanishes with no feedback. This is listed under "parser hardening" in ideas.md.

**Fix:** When `_parse_variable` returns `None` for a non-empty, non-comment line, a warning is added with the source line number and text. Warnings are surfaced in `zeta.py --verbose` output.

**Outcome:** 332 tests pass. No regressions.

---

## 6. fix TS type mapping: int/float/uint → number everywhere (2026-03-29)

**What:** The TS emitter was using raw zero type names (`int`, `float`, `uint`, `int32`, etc.) as TypeScript type annotations. These aren't valid TS types — they should all map to `number`.

**Why:** While `tsx` (esbuild) strips types and doesn't catch this, `tsc --strict` would reject the generated code. The Python emitter already mapped types correctly.

**Fix:** Enhanced `_ts_type()` to handle concrete numeric types (`int32`, `uint8`, `float64`, etc.) and `uint`. Applied `_ts_type()` in all annotation sites: variables, function params, return types, task params/outputs, dispatcher return types, struct field interfaces. Updated 17 string-match tests that were asserting the old (incorrect) type names.

**Tests added:** 1 new test for int→number mapping.

**Outcome:** 333 tests pass.

---

## 7. tsc --strict validation + fix missing const/readonly (2026-03-29)

**What:** Added 26 `tsc --strict --noEmit` tests that validate the generated TypeScript passes the real TypeScript compiler in strict mode. Fixed 3 issues found along the way.

**Why:** The TS execution tests use `tsx` (esbuild) which strips types without checking them. Invalid type annotations, missing declarations, and readonly/mutable mismatches were invisible. `tsc --strict` catches all of these.

**Bugs fixed:**
- Task call results (`even_arr = [...]`) were missing `const` declaration and type annotation
- Where/sort results (`sorted_arr = ...`) were missing `const` declaration and type annotation
- Task input stream params used `number[]` but callers pass `readonly number[]` — changed to accept `readonly`

**Tests added:** 26 tsc --strict tests covering all feature areas plus a comprehensive acid test combining everything.

**Outcome:** 359 tests pass. All generated TypeScript is `tsc --strict` clean.

---

