# first translation candidates
*simplest leaf functions from parser.py*

## candidate 1: _matching_paren (recommended start)

Pure logic, no exceptions, no mixed return types. Character-by-character scan with a depth counter.

### python

    def _matching_paren(s: str, start: int) -> int:
        depth = 0
        for i in range(start, len(s)):
            if s[i] == "(":
                depth += 1
            elif s[i] == ")":
                depth -= 1
                if depth == 0:
                    return i
        return -1

Needs: char-at-index, string length, loop. Clean and self-contained.

## candidate 2: _find_matching_bracket

Same pattern as above but for `[]` instead of `()`:

### python

    def _find_matching_bracket(s: str, start: int) -> int:
        depth = 0
        for i in range(start, len(s)):
            if s[i] == "[":
                depth += 1
            elif s[i] == "]":
                depth -= 1
                if depth == 0:
                    return i
        return -1

## candidate 3: _parse_literal

10 lines but immediately interesting — uses try/except and returns different types:

### python

    def _parse_literal(s: str):
        try:
            return int(s)
        except ValueError:
            pass
        try:
            return float(s)
        except ValueError:
            pass
        return s

Raises design questions: how does zero handle "try to parse as int, fall back"? And the return type is int | float | string — zero doesn't have union returns.

## candidate 4: _is_expression

Simple predicate — checks if a string looks like an expression:

### python

    def _is_expression(s: str) -> bool:
        if "$" in s:
            return True
        if any(op in s for op in [" + ", " - ", " * ", " / ", " % "]):
            return True
        return False

Needs: string-contains check.

## candidate 5: _looks_like_enum

Another predicate:

### python

    def _looks_like_enum(parts: list[str]) -> bool:
        for p in parts:
            if p in {"int", "uint", "float"}:
                return False
            if re.match(r"(int|uint|float)\d+", p):
                return False
        return True

Needs: string matching, set membership.

## recommendation

Start with `_matching_paren` — it's the simplest useful function and will force us to nail down string indexing and character comparison in zero.
