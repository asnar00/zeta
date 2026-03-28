# zero to py
*rules for translating zero to python*

This document defines how each zero language construct maps to Python 3.12+. Each section covers one construct, with zero examples followed by their Python translations.

## concrete numeric types

Concrete numeric types emit type aliases mapping to native `int` or `float`.

```zero
type int32 = ... 32-bit signed integer
type uint8 = ... 8-bit unsigned integer
type float32 = ... 32-bit IEEE floating-point number
```

```python
type int32 = int
type uint8 = int
type float32 = float
```

Mapping rule:

| zero type pattern | python type |
|-------------------|-------------|
| `int*`, `uint*`   | `int`       |
| `float*`          | `float`     |

## abstract numeric types

Abstract numeric types also emit type aliases.

```zero
type int = ... any-size signed integer
type uint = ... any-size unsigned integer
type float = ... any-size IEEE floating-point number
type number = int | float
```

```python
# int and float are Python builtins — no alias needed
type uint = int
type number = int | float
```

## enumerations

Enumerations map to `enum.Enum` subclasses.

```zero
type tri-state = no | yes | maybe
```

```python
from enum import Enum

class tri_state(Enum):
    no = "no"
    yes = "yes"
    maybe = "maybe"
```

Naming rule: the type name is kept lowercase (hyphens become underscores).

## structure types

Structure types map to `NamedTuple` classes. Since zero functions are pure and single-assignment, immutability is safe and gives us better performance, lower memory, and free hashability.

```zero
type vector =
    number x, y, z = 0
```

```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0
```

Naming rule: the type name is kept lowercase (hyphens become underscores).

## variables

Variables map to annotated assignments.

```zero
int32 i = 10
float f = 1.02
```

```python
i: int = 10
f: float = 1.02
```

## structure constructors

Structure constructors map directly to NamedTuple construction.

```zero
vector v = vector()
vector v = vector(1, 2, 3)
vector v = vector(z=2, x=1)
```

```python
v: vector = vector()
v: vector = vector(1, 2, 3)
v: vector = vector(z=2, x=1)
```

## functions

Functions use *open syntax* prototypes, with a *named result* pattern on the LHS and the callable signature on the RHS of the `=`.

### naming convention

The Python function name is derived from the RHS of the prototype by:
1. concatenating all words, symbols, and parameter types with single underscores
2. replacing symbols with words (`+` → `plus`, `-` → `minus`, `*` → `times`, `/` → `div`, `%` → `mod`, `==` → `eq`, `!=` → `neq`, `<` → `lt`, `>` → `gt`, `<=` → `lte`, `>=` → `gte`)
3. prefixing each parameter type with a double-underscore

### named result pattern

The LHS `(type name)` declares the return type and a result variable. The function body assigns to this variable. The translator wraps the body so the result variable is returned.

### example: operator overload

```zero
on (vector v) = (vector a) + (vector b)
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
```

```python
def fn__vector_plus__vector(a: vector, b: vector) -> vector:
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    return v
```

### example: named function

```zero
on (number n) = smaller of (number a) and (number b)
    n = (a) if (a < b) else (b)
```

```python
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n
```

## arrays

Arrays (marked with `$` suffix) map to Python `list` with type hints. The `$` is replaced with `_arr` in Python.

```zero
int i$
```

```python
i_arr: list[int] = []
```

```zero
int i$[4]
```

```python
i_arr: list[int] = [0] * 4
```

```zero
int i$[4] = 1
```

```python
i_arr: list[int] = [1] * 4
```

```zero
int i$ = [1, 2, 3, 4]
```

```python
i_arr: list[int] = [1, 2, 3, 4]
```

```zero
int i$ = [1 through 4]
```

```python
i_arr: list[int] = list(range(1, 5))  # [1, 2, 3, 4] — inclusive both ends
```

```zero
int i$ = [0 to 4]
```

```python
i_arr: list[int] = list(range(0, 4))  # [0, 1, 2, 3] — inclusive start, exclusive end
```

## strings

`string` maps to Python's `str`.

```zero
string s = "hello"
```

```python
s: str = "hello"
```

## conditional blocks

`if`/`else if`/`else` maps to `if`/`elif`/`else`.

```zero
on (string s) = describe (int n)
    if (n > 0)
        s = "positive"
    else if (n < 0)
        s = "negative"
    else
        s = "zero"
```

```python
def fn_describe__int(n: int) -> str:
    if n > 0:
        s = "positive"
    elif n < 0:
        s = "negative"
    else:
        s = "zero"
    return s
```

## array mapping

Passing an array to a function maps it. The `$` suffix becomes `_arr`.

```zero
int i$ = [1, 2, 3, 4]
int j$ = i$ * 2
```

```python
i_arr: list[int] = [1, 2, 3, 4]
j_arr: list[int] = [x * 2 for x in i_arr]
```

Two arrays: uses `zip_longest` with zero-padding.

```zero
int k$ = i$ + j$
```

```python
from itertools import zip_longest
k_arr: list[int] = [a + b for a, b in zip_longest(i_arr, j_arr, fillvalue=0)]
```

## array reduction

The `_` symbol reduces an array to a single value.

```zero
int sum = i$ + _
```

```python
import functools
sum: int = functools.reduce(lambda a, b: a + b, i_arr)
```

## streaming

The `<-` operator builds arrays incrementally.

```zero
int i$ <- 1 <- (i$ + 1) until (i$ == 4)
```

```python
i_arr = [1]
while not (i_arr[-1] == 4):
    i_arr.append(i_arr[-1] + 1)
```

## void functions

Functions without a return type use `on name()` syntax.

```zero
on hello()
    print "hello world"
```

```python
def fn_hello():
    print "hello world"
```

## tasks

Tasks emit as Python generator functions. Calls materialise with `list()`.

```zero
on (int even$) <- only evens from (int numbers$)
    int n <- numbers$
    if (n % 2 == 0)
        even$ <- n

int even$ <- only evens from (all$)
```

```python
def fn_only_evens_from__int(numbers_arr):
    for n in numbers_arr:
        if n % 2 == 0:
            yield n

even_arr = list(fn_only_evens_from__int(all_arr))
```

## concurrently

Concurrent blocks use a threading helper.

```zero
concurrently
    hello()
and
    beep()
```

```python
import threading

def _concurrently(*fns):
    threads = [threading.Thread(target=fn) for fn in fns]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

_concurrently(lambda: hello(), lambda: beep())
```

## bitwise operators

Bitwise operators pass through directly. Symbol-to-name mapping for function names:

| zero | python | function name |
|------|--------|---------------|
| `&`  | `&`    | `band`        |
| `\|` | `\|`   | `bor`         |
| `^`  | `^`    | `bxor`        |
| `<<` | `<<`   | `shl`         |
| `>>` | `>>`   | `shr`         |

## Python-specific decisions

- **Target: Python 3.12+** — enables `type x = y` alias syntax.
- **Structs use NamedTuple** — immutability matches zero's purity and SSA.
- **`string` maps to `str`** — the only type name that changes.
- **Enum values are qualified** — `direction.north` not bare `north`.
