# atoz
*anything to zero — translating existing code into zero*

status: learning by doing (translating zeta's own Python into zero)

## what we're discovering

By translating zeta's Python functions to zero bottom-up, we're learning what atoz needs to do. Each translation is a data point: what maps cleanly, what needs a new zero feature, what requires restructuring.

## the translation process

Translation is not line-by-line. It's a reasoning process:

1. **What not how.** Read the function and describe what it does in one sentence, ignoring the implementation. "_Matching_paren_ scans characters, tracks bracket depth, returns where depth hits zero."

2. **Decompose into data flow.** The imperative version usually mixes multiple concerns in one loop. Separate them into distinct stages. Tracking depth is one thing. Finding the position is another.

3. **Match each stage to a zero construct.** Stateful transformation over a stream → task. Finding a value in a collection → `first of ... where` / `index of first ... where`. Pure computation → function. Each concern maps to one zero construct.

4. **Generalise.** Once decomposed, unnecessary specifics become obvious. Hardcoded to `()`? The bracket characters are obviously a parameter. The Python version would need refactoring to do this — in zero it falls out naturally from the decomposition.

5. **Name it so it reads.** The call site should be understandable without documentation. `matching ("()") in (s) after (start)` — a human gets this immediately.

The pattern: *what not how → decompose → match to constructs → generalise → name naturally*.

## translations

### 1. _matching_paren + _find_matching_bracket → matching

**Python** (two separate functions, hardcoded bracket types):

    def _matching_paren(s, start):
        depth = 0
        for i in range(start, len(s)):
            if s[i] == "(": depth += 1
            elif s[i] == ")":
                depth -= 1
                if depth == 0: return i
        return -1

**Reasoning:**
- What does it do? Scans characters, tracks bracket depth, returns where depth hits zero.
- Two concerns: (1) compute depth at each character, (2) find the position where depth is zero.
- Concern 1 is a stateful stream transformation → task.
- Concern 2 is finding an index in a collection → `index of first ... where`.
- Bracket characters are just a parameter — one function replaces two.

**Zero:**

    on (int depth$) <- bracket depth of (char c$) matching (string pair)
        int d = 0
        char c <- c$
        if (c == pair[0])
            d = d + 1
        else if (c == pair[1])
            d = d - 1
        depth$ <- d

    on (int pos) = matching (string pair) in (string s) after (int start)
        string sub = s[start:]
        int depth$ <- bracket depth of (sub) matching (pair)
        pos = start + index of first in [depth$] where (_ == 0)

**Call site:**

    int pos = matching ("()") in (s) after (start)
    int pos = matching ("[]") in (s) after (start)

## blockers hit

- `string is char$` — needed string to be a character collection so tasks can consume characters directly (designed, not yet implemented)
- `index of first in [...] where (...)` — needed a new array function that returns the index, not the value (designed, not yet implemented)

## patterns observed

- **Imperative loop with mutable state → task.** A `for` loop with accumulators is a stateful stream transformation. The task body is the loop body, the accumulated variable persists across iterations.
- **Early return from a loop → `first of` / `index of first`.** A loop that scans for a condition and returns immediately maps to a query over the task's output stream.
- **Two hardcoded functions → one parameterised function.** Decomposition reveals that the variation (which brackets) is just a parameter. Zero's multi-word signatures make the parameterised version more readable, not less.

## rules of translation

*(emerging rules for how Python constructs map to zero — the seed of an automated atoz tool)*

- `for i in range(start, len(s)): if s[i] == ...` → task consuming `char$` with slicing
- Mutable accumulator in a loop → task state variable
- `return` inside a loop → `index of first ... where` or `first of ... where` on the task's output
- Two functions that differ only in constants → one function with parameters
