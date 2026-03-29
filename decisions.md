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

