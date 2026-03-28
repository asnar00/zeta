"""Probe script: systematically combine language features to find spec gaps.

Usage: python3 probe.py
Outputs: probe_report.md
"""

from parser import process, ZeroParseError
from emit_python import emit as emit_py
from emit_typescript import emit as emit_ts


# --- probe definitions ---
# Each probe is (title, description, zero_source)

def generate_probes() -> list[tuple]:
    """Generate all feature-combination probes.
    Each probe is (title, description, source) or (title, description, source, expect_error)
    where expect_error is a substring expected in the error message."""
    probes = []

    # --- types × types ---
    probes.append((
        "struct with concrete type fields",
        "Can a struct use concrete numeric types?",
        """\
    type point =
        int32 x, y = 0"""
    ))

    probes.append((
        "struct with enum field",
        "Can a struct contain an enum-typed field?",
        """\
    type direction = north | south | east | west
    type entity =
        direction facing = north
        int32 x, y = 0"""
    ))

    probes.append((
        "nested struct",
        "Can a struct contain another struct as a field?",
        """\
    type vector =
        number x, y, z = 0
    type line =
        vector start, end"""
    ))

    # --- arrays × types ---
    probes.append((
        "array of structs",
        "Can we declare an array of struct type?",
        """\
    type vector =
        number x, y, z = 0
    vector v$[3]"""
    ))

    probes.append((
        "array of enums",
        "Can we declare an array of enum type?",
        """\
    type color = red | green | blue
    color c$ = [red, green, blue]"""
    ))

    probes.append((
        "array of concrete type",
        "Can we use concrete types with arrays?",
        """\
    int32 i$[4] = 1"""
    ))

    # --- functions × types ---
    probes.append((
        "function returning concrete type",
        "Can a function return a concrete numeric type?",
        """\
    on (int32 r) = (int32 a) + (int32 b)
        r = a + b"""
    ))

    probes.append((
        "function with enum parameter",
        "Can a function take an enum parameter?",
        """\
    type direction = north | south | east | west
    on (int dx) = x offset of (direction d)
        dx = 1"""
    ))

    probes.append((
        "function returning struct",
        "Can a function construct and return a struct?",
        """\
    type vector =
        number x, y, z = 0
    on (vector v) = zero vector
        v = vector()"""
    ))

    probes.append((
        "function with multiple struct params",
        "Can a function take multiple struct parameters?",
        """\
    type vector =
        number x, y, z = 0
    on (number d) = dot (vector a) and (vector b)
        d = a.x * b.x + a.y * b.y + a.z * b.z"""
    ))

    # --- functions × functions ---
    probes.append((
        "function calling another function",
        "Can a function body reference another function?",
        """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = smaller of (smaller of (a) and (b)) and (c)"""
    ))

    # --- map × types ---
    probes.append((
        "map with named function",
        "Can we map a named function over an array using open syntax?",
        """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)"""
    ))

    probes.append((
        "map over struct array",
        "Can we apply a function to an array of structs?",
        """\
    type vector =
        number x, y, z = 0
    on (number n) = length of (vector v)
        n = v.x * v.x + v.y * v.y + v.z * v.z
    vector v$[3]
    number len$ = length of (v$)"""
    ))

    probes.append((
        "map struct array with scalar",
        "Can we map a function with a struct array and a scalar?",
        """\
    type vector =
        number x, y, z = 0
    on (vector v) = scale (vector a) by (number s)
        v = vector(a.x * s, a.y * s, a.z * s)
    vector v$[3]
    vector scaled$ = scale (v$) by (2)"""
    ))

    # --- reduce × types ---
    probes.append((
        "reduce with concrete type",
        "Can we reduce an array of concrete type?",
        """\
    int32 i$ = [1, 2, 3, 4]
    int32 sum = i$ + _"""
    ))

    probes.append((
        "reduce struct array",
        "Can we reduce an array of structs with a function?",
        """\
    type vector =
        number x, y, z = 0
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    vector v$[3]
    vector total = (v$) + (_)"""
    ))

    # --- arrays × arrays ---
    probes.append((
        "map two arrays of different types",
        "Can we combine arrays of different types?",
        """\
    int i$ = [1, 2, 3]
    float f$ = [0.5, 1.5, 2.5]
    float r$ = i$ + f$"""
    ))

    probes.append((
        "chained array operations",
        "Can we chain map then reduce?",
        """\
    int i$ = [1, 2, 3, 4]
    int doubled$ = i$ * 2
    int sum = doubled$ + _"""
    ))

    # --- edge cases ---
    probes.append((
        "abstract function (no body)",
        "A function with no body is abstract (platform declaration)",
        """\
    on (int r) = nothing (int a)"""
    ))

    probes.append((
        "struct with no defaults",
        "Can a struct have fields without default values?",
        """\
    type pair =
        int first
        int second"""
    ))

    probes.append((
        "struct with mixed defaults",
        "Can some fields have defaults and others not?",
        """\
    type config =
        int width = 800
        int height = 600
        float scale"""
    ))

    probes.append((
        "multiple return assignments",
        "Should reject SSA violation",
        """\
    on (int r) = bad (int a)
        r = a + 1
        r = a + 2""",
        "already assigned"
    ))

    probes.append((
        "array in function body",
        "Can a function body create and use an array?",
        """\
    on (int r) = sum squares up to (int n)
        int i$ = [1 through n]
        int sq$ = i$ * i$
        r = sq$ + _"""
    ))

    probes.append((
        "ternary with struct",
        "Can a ternary return a struct?",
        """\
    type vector =
        number x, y, z = 0
    on (vector v) = pick (vector a) or (vector b) given (number t)
        v = (a) if (t > 0) else (b)"""
    ))

    probes.append((
        "function with single word name",
        "Can a function have just one word and one param?",
        """\
    on (number n) = negate (number x)
        n = 0 - x"""
    ))

    probes.append((
        "deeply nested expression",
        "Can expressions be deeply nested with parens?",
        """\
    on (number n) = complex (number a) and (number b) and (number c)
        n = ((a + b) * (b + c)) + ((a - c) * (b - a))"""
    ))

    # --- fn calls in variables ---
    probes.append((
        "scalar variable from fn call",
        "Can a scalar variable be assigned from a function call?",
        """\
    on (number n) = double (number x)
        n = x * 2
    number y = double (5)"""
    ))

    probes.append((
        "scalar variable from multiword fn call",
        "Can a scalar variable be assigned from a multi-word function call?",
        """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    number m = smaller of (3) and (7)"""
    ))

    # --- named fn map over array ---
    probes.append((
        "named fn map over array",
        "Can a named function be mapped over an array?",
        """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)"""
    ))

    probes.append((
        "multiword fn map with scalar",
        "Can a multi-word function be mapped with one array and one scalar arg?",
        """\
    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)"""
    ))

    # --- var decls in function bodies ---
    probes.append((
        "scalar var decl in body",
        "Can a function body declare a local variable?",
        """\
    on (int r) = sum of (int a) and (int b)
        int c = a + b
        r = c"""
    ))

    probes.append((
        "array var decl in body",
        "Can a function body declare a local array?",
        """\
    on (int r) = sum list
        int i$ = [1, 2, 3, 4]
        r = i$ + _"""
    ))

    probes.append((
        "range with variable in body",
        "Can a function body use a variable-endpoint range?",
        """\
    on (int r) = sum up to (int n)
        int i$ = [1 through n]
        r = i$ + _"""
    ))

    # --- deeper nesting ---
    probes.append((
        "map then reduce",
        "Can we map then reduce in sequence?",
        """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int doubled$ = double (i$)
    int sum = doubled$ + _"""
    ))

    probes.append((
        "fn call in fn call in body",
        "Can function calls be nested three deep?",
        """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = octuple (number x)
        n = double (double (double (x)))"""
    ))

    probes.append((
        "fn call result used in binop",
        "Can a function call be part of a larger expression?",
        """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = double plus one (number x)
        n = double (x) + 1"""
    ))

    probes.append((
        "reduce in function body",
        "Can a function body reduce a local array?",
        """\
    on (int r) = sum squares of (int a) and (int b) and (int c)
        int vals$ = [a, b, c]
        int sq$ = vals$ * vals$
        r = sq$ + _"""
    ))

    probes.append((
        "ternary with fn call",
        "Can a ternary branch contain a function call?",
        """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = maybe double (number x) given (number flag)
        n = (double (x)) if (flag > 0) else (x)"""
    ))

    probes.append((
        "struct constructor in fn call",
        "Can a function call pass a struct constructor as argument?",
        """\
    type vector =
        number x, y, z = 0
    on (number n) = length of (vector v)
        n = v.x * v.x + v.y * v.y + v.z * v.z
    number d = length of (vector(1, 2, 3))"""
    ))

    probes.append((
        "multiple fn calls in one expression",
        "Can multiple different functions appear in one expression?",
        """\
    on (number n) = double (number x)
        n = x * 2
    on (number n) = negate (number x)
        n = 0 - x
    on (number n) = weird (number x)
        n = double (x) + negate (x)"""
    ))

    probes.append((
        "array of results from fn calls",
        "Can an array literal contain function call results?",
        """\
    on (number n) = double (number x)
        n = x * 2
    int i$ = [double (1), double (2), double (3)]"""
    ))

    probes.append((
        "enum in ternary condition",
        "Can an enum value be used in a ternary?",
        """\
    type direction = north | south | east | west
    on (int dx) = x step for (direction d)
        dx = (1) if (d == east) else (0)"""
    ))

    probes.append((
        "bitwise ops in function body",
        "Can bitwise operators be used in function bodies?",
        """\
    on (uint8 r) = low nibble of (uint8 x)
        r = x & 15"""
    ))

    probes.append((
        "chained member access in expression",
        "Can member access be used inside complex expressions?",
        """\
    type vector =
        number x, y, z = 0
    on (number n) = distance squared between (vector a) and (vector b)
        number dx = a.x - b.x
        number dy = a.y - b.y
        number dz = a.z - b.z
        n = dx * dx + dy * dy + dz * dz"""
    ))

    return probes


# --- probe runner ---

class ProbeResult:
    def __init__(self, title, description, source, expect_error=None):
        self.title = title
        self.description = description
        self.source = source
        self.expect_error = expect_error
        self.parse_ok = False
        self.parse_error = None
        self.ir = None
        self.py_ok = False
        self.py_output = None
        self.py_error = None
        self.ts_ok = False
        self.ts_output = None
        self.ts_error = None

    @property
    def all_ok(self):
        if self.expect_error:
            return self.parse_error is not None and self.expect_error in self.parse_error
        return self.parse_ok and self.py_ok and self.ts_ok

    @property
    def status(self):
        if self.all_ok:
            return "OK"
        if self.expect_error:
            if self.parse_error is None:
                return f"expected error containing '{self.expect_error}' but parsed OK"
            return f"expected '{self.expect_error}' but got: {self.parse_error}"
        parts = []
        if not self.parse_ok:
            parts.append("parse failed")
        if self.parse_ok and not self.py_ok:
            parts.append("py emit failed")
        if self.parse_ok and not self.ts_ok:
            parts.append("ts emit failed")
        return ", ".join(parts)


def run_probe(title, description, source, expect_error=None) -> ProbeResult:
    result = ProbeResult(title, description, source, expect_error)

    # parse
    try:
        result.ir = process(source)
        result.parse_ok = True
    except ZeroParseError as e:
        result.parse_error = str(e)
        return result
    except Exception as e:
        result.parse_error = f"{type(e).__name__}: {e}"
        return result

    # python emit
    try:
        result.py_output = emit_py(result.ir)
        result.py_ok = True
    except Exception as e:
        result.py_error = f"{type(e).__name__}: {e}"

    # typescript emit
    try:
        result.ts_output = emit_ts(result.ir)
        result.ts_ok = True
    except Exception as e:
        result.ts_error = f"{type(e).__name__}: {e}"

    return result


def write_report(results: list[ProbeResult]):
    ok_count = sum(1 for r in results if r.all_ok)
    fail_count = len(results) - ok_count

    with open("probe_report.md", "w") as f:
        f.write("# probe report\n")
        f.write("*systematic feature combination testing*\n\n")
        f.write(f"**{len(results)} probes: {ok_count} passed, {fail_count} failed/partial**\n\n")

        # summary table
        f.write("## summary\n\n")
        f.write("| # | probe | status |\n")
        f.write("|---|-------|--------|\n")
        for i, r in enumerate(results, 1):
            status = "PASS" if r.all_ok else "FAIL"
            f.write(f"| {i} | {r.title} | {status} |\n")
        f.write("\n")

        # failures first
        failures = [r for r in results if not r.all_ok]
        if failures:
            f.write("## failures and gaps\n\n")
            for r in failures:
                f.write(f"### {r.title}\n\n")
                f.write(f"**Question:** {r.description}\n\n")
                f.write(f"**Status:** {r.status}\n\n")
                f.write("**zero input:**\n")
                f.write(f"```zero\n{r.source.strip()}\n```\n\n")
                if r.parse_error:
                    f.write(f"**parse error:**\n```\n{r.parse_error}\n```\n\n")
                if r.py_error:
                    f.write(f"**python error:**\n```\n{r.py_error}\n```\n\n")
                if r.ts_error:
                    f.write(f"**typescript error:**\n```\n{r.ts_error}\n```\n\n")
                # show partial output if available
                if r.py_output:
                    f.write(f"**python output:**\n```python\n{r.py_output.strip()}\n```\n\n")
                if r.ts_output:
                    f.write(f"**typescript output:**\n```typescript\n{r.ts_output.strip()}\n```\n\n")

        # successes
        successes = [r for r in results if r.all_ok]
        if successes:
            f.write("## passing probes\n\n")
            for r in successes:
                f.write(f"### {r.title}\n\n")
                f.write(f"**Question:** {r.description}\n\n")
                f.write("**zero input:**\n")
                f.write(f"```zero\n{r.source.strip()}\n```\n\n")
                if r.expect_error:
                    f.write(f"**correctly rejected:** {r.parse_error.splitlines()[0]}\n\n")
                else:
                    f.write("**python output:**\n")
                    f.write(f"```python\n{r.py_output.strip()}\n```\n\n")
                    f.write("**typescript output:**\n")
                    f.write(f"```typescript\n{r.ts_output.strip()}\n```\n\n")


if __name__ == "__main__":
    probes = generate_probes()
    results = [run_probe(*p) for p in probes]
    write_report(results)

    ok = sum(1 for r in results if r.all_ok)
    fail = len(results) - ok
    print(f"{len(results)} probes: {ok} passed, {fail} failed")

    if fail:
        print("\nFailures:")
        for r in results:
            if not r.all_ok:
                print(f"  - {r.title}: {r.status}")
