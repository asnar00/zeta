# probe report
*systematic feature combination testing*

**44 probes: 44 passed, 0 failed/partial**

## summary

| # | probe | status |
|---|-------|--------|
| 1 | struct with concrete type fields | PASS |
| 2 | struct with enum field | PASS |
| 3 | nested struct | PASS |
| 4 | array of structs | PASS |
| 5 | array of enums | PASS |
| 6 | array of concrete type | PASS |
| 7 | function returning concrete type | PASS |
| 8 | function with enum parameter | PASS |
| 9 | function returning struct | PASS |
| 10 | function with multiple struct params | PASS |
| 11 | function calling another function | PASS |
| 12 | map with named function | PASS |
| 13 | map over struct array | PASS |
| 14 | map struct array with scalar | PASS |
| 15 | reduce with concrete type | PASS |
| 16 | reduce struct array | PASS |
| 17 | map two arrays of different types | PASS |
| 18 | chained array operations | PASS |
| 19 | abstract function (no body) | PASS |
| 20 | struct with no defaults | PASS |
| 21 | struct with mixed defaults | PASS |
| 22 | multiple return assignments | PASS |
| 23 | array in function body | PASS |
| 24 | ternary with struct | PASS |
| 25 | function with single word name | PASS |
| 26 | deeply nested expression | PASS |
| 27 | scalar variable from fn call | PASS |
| 28 | scalar variable from multiword fn call | PASS |
| 29 | named fn map over array | PASS |
| 30 | multiword fn map with scalar | PASS |
| 31 | scalar var decl in body | PASS |
| 32 | array var decl in body | PASS |
| 33 | range with variable in body | PASS |
| 34 | map then reduce | PASS |
| 35 | fn call in fn call in body | PASS |
| 36 | fn call result used in binop | PASS |
| 37 | reduce in function body | PASS |
| 38 | ternary with fn call | PASS |
| 39 | struct constructor in fn call | PASS |
| 40 | multiple fn calls in one expression | PASS |
| 41 | array of results from fn calls | PASS |
| 42 | enum in ternary condition | PASS |
| 43 | bitwise ops in function body | PASS |
| 44 | chained member access in expression | PASS |

## passing probes

### struct with concrete type fields

**Question:** Can a struct use concrete numeric types?

**zero input:**
```zero
type point =
        int32 x, y = 0
```

**python output:**
```python
from typing import NamedTuple

class point(NamedTuple):
    x: int32 = 0
    y: int32 = 0
```

**typescript output:**
```typescript
interface point {
    readonly x: int32;
    readonly y: int32;
}

function point(args: Partial<point> = {}): point {
    return { x: args.x ?? 0, y: args.y ?? 0 };
}
```

### struct with enum field

**Question:** Can a struct contain an enum-typed field?

**zero input:**
```zero
type direction = north | south | east | west
    type entity =
        direction facing = north
        int32 x, y = 0
```

**python output:**
```python
from enum import Enum
from typing import NamedTuple

class direction(Enum):
    north = "north"
    south = "south"
    east = "east"
    west = "west"

class entity(NamedTuple):
    facing: direction = direction.north
    x: int32 = 0
    y: int32 = 0
```

**typescript output:**
```typescript
enum direction {
    north = "north",
    south = "south",
    east = "east",
    west = "west",
}

interface entity {
    readonly facing: direction;
    readonly x: int32;
    readonly y: int32;
}

function entity(args: Partial<entity> = {}): entity {
    return { facing: args.facing ?? direction.north, x: args.x ?? 0, y: args.y ?? 0 };
}
```

### nested struct

**Question:** Can a struct contain another struct as a field?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    type line =
        vector start, end
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

class line(NamedTuple):
    start: vector = 0
    end: vector = 0
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

interface line {
    readonly start: vector;
    readonly end: vector;
}

function line(args: Partial<line> = {}): line {
    return { start: args.start ?? vector(), end: args.end ?? vector() };
}
```

### array of structs

**Question:** Can we declare an array of struct type?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    vector v$[3]
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

v_arr: list[vector] = [0] * 3
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

const v_arr: readonly vector[] = Array(3).fill(0);
```

### array of enums

**Question:** Can we declare an array of enum type?

**zero input:**
```zero
type color = red | green | blue
    color c$ = [red, green, blue]
```

**python output:**
```python
from enum import Enum

class color(Enum):
    red = "red"
    green = "green"
    blue = "blue"

c_arr: list[color] = [color.red, color.green, color.blue]
```

**typescript output:**
```typescript
enum color {
    red = "red",
    green = "green",
    blue = "blue",
}

const c_arr: readonly color[] = [color.red, color.green, color.blue];
```

### array of concrete type

**Question:** Can we use concrete types with arrays?

**zero input:**
```zero
int32 i$[4] = 1
```

**python output:**
```python
i_arr: list[int32] = [1] * 4
```

**typescript output:**
```typescript
const i_arr: readonly int32[] = Array(4).fill(1);
```

### function returning concrete type

**Question:** Can a function return a concrete numeric type?

**zero input:**
```zero
on (int32 r) = (int32 a) + (int32 b)
        r = a + b
```

**python output:**
```python
def fn__int32_plus__int32(a: int32, b: int32) -> int32:
    r = a + b
    return r
```

**typescript output:**
```typescript
function fn__int32_plus__int32(a: int32, b: int32): int32 {
    const r: int32 = a + b;
    return r;
}
```

### function with enum parameter

**Question:** Can a function take an enum parameter?

**zero input:**
```zero
type direction = north | south | east | west
    on (int dx) = x offset of (direction d)
        dx = 1
```

**python output:**
```python
from enum import Enum

class direction(Enum):
    north = "north"
    south = "south"
    east = "east"
    west = "west"

def fn_x_offset_of__direction(d: direction) -> int:
    dx = 1
    return dx
```

**typescript output:**
```typescript
enum direction {
    north = "north",
    south = "south",
    east = "east",
    west = "west",
}

function fn_x_offset_of__direction(d: direction): int {
    const dx: int = 1;
    return dx;
}
```

### function returning struct

**Question:** Can a function construct and return a struct?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (vector v) = zero vector
        v = vector()
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_zero_vector() -> vector:
    v = vector()
    return v
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

function fn_zero_vector(): vector {
    const v: vector = vector({  });
    return v;
}
```

### function with multiple struct params

**Question:** Can a function take multiple struct parameters?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (number d) = dot (vector a) and (vector b)
        d = a.x * b.x + a.y * b.y + a.z * b.z
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_dot__vector_and__vector(a: vector, b: vector) -> number:
    d = a.x * b.x + a.y * b.y + a.z * b.z
    return d
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

function fn_dot__vector_and__vector(a: vector, b: vector): number {
    const d: number = a.x * b.x + a.y * b.y + a.z * b.z;
    return d;
}
```

### function calling another function

**Question:** Can a function body reference another function?

**zero input:**
```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = smaller of (smaller of (a) and (b)) and (c)
```

**python output:**
```python
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n

def fn_smallest_of__number_and__number_and__number(a: number, b: number, c: number) -> number:
    n = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)
    return n
```

**typescript output:**
```typescript
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}

function fn_smallest_of__number_and__number_and__number(a: number, b: number, c: number): number {
    const n: number = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c);
    return n;
}
```

### map with named function

**Question:** Can we map a named function over an array using open syntax?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

i_arr: list[int] = [1, 2, 3, 4]
j_arr: list[int] = [fn_double__number(x) for x in i_arr]
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [1, 2, 3, 4];
const j_arr: readonly int[] = i_arr.map(x => fn_double__number(x));

function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

### map over struct array

**Question:** Can we apply a function to an array of structs?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (number n) = length of (vector v)
        n = v.x * v.x + v.y * v.y + v.z * v.z
    vector v$[3]
    number len$ = length of (v$)
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_length_of__vector(v: vector) -> number:
    n = v.x * v.x + v.y * v.y + v.z * v.z
    return n

v_arr: list[vector] = [0] * 3
len_arr: list[number] = [fn_length_of__vector(x) for x in v_arr]
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

const v_arr: readonly vector[] = Array(3).fill(0);
const len_arr: readonly number[] = v_arr.map(x => fn_length_of__vector(x));

function fn_length_of__vector(v: vector): number {
    const n: number = v.x * v.x + v.y * v.y + v.z * v.z;
    return n;
}
```

### map struct array with scalar

**Question:** Can we map a function with a struct array and a scalar?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (vector v) = scale (vector a) by (number s)
        v = vector(a.x * s, a.y * s, a.z * s)
    vector v$[3]
    vector scaled$ = scale (v$) by (2)
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_scale__vector_by__number(a: vector, s: number) -> vector:
    v = vector(a.x * s, a.y * s, a.z * s)
    return v

v_arr: list[vector] = [0] * 3
scaled_arr: list[vector] = [fn_scale__vector_by__number(x, 2) for x in v_arr]
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

const v_arr: readonly vector[] = Array(3).fill(0);
const scaled_arr: readonly vector[] = v_arr.map(x => fn_scale__vector_by__number(x, 2));

function fn_scale__vector_by__number(a: vector, s: number): vector {
    const v: vector = vector({ x: a.x * s, y: a.y * s, z: a.z * s });
    return v;
}
```

### reduce with concrete type

**Question:** Can we reduce an array of concrete type?

**zero input:**
```zero
int32 i$ = [1, 2, 3, 4]
    int32 sum = i$ + _
```

**python output:**
```python
import functools

i_arr: list[int32] = [1, 2, 3, 4]
sum: int32 = functools.reduce(lambda a, b: a + b, i_arr)
```

**typescript output:**
```typescript
const i_arr: readonly int32[] = [1, 2, 3, 4];
const sum: int32 = i_arr.reduce((a, b) => a + b);
```

### reduce struct array

**Question:** Can we reduce an array of structs with a function?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    vector v$[3]
    vector total = (v$) + (_)
```

**python output:**
```python
import functools
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn__vector_plus__vector(a: vector, b: vector) -> vector:
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    return v

v_arr: list[vector] = [0] * 3
total: vector = functools.reduce(fn__vector_plus__vector, v_arr)
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

const v_arr: readonly vector[] = Array(3).fill(0);
const total: vector = v_arr.reduce(fn__vector_plus__vector);

function fn__vector_plus__vector(a: vector, b: vector): vector {
    const v: vector = vector({ x: a.x + b.x, y: a.y + b.y, z: a.z + b.z });
    return v;
}
```

### map two arrays of different types

**Question:** Can we combine arrays of different types?

**zero input:**
```zero
int i$ = [1, 2, 3]
    float f$ = [0.5, 1.5, 2.5]
    float r$ = i$ + f$
```

**python output:**
```python
from itertools import zip_longest

i_arr: list[int] = [1, 2, 3]
f_arr: list[float] = [0.5, 1.5, 2.5]
r_arr: list[float] = [a + b for a, b in zip_longest(i_arr, f_arr, fillvalue=0)]
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [1, 2, 3];
const f_arr: readonly float[] = [0.5, 1.5, 2.5];
const r_arr: readonly float[] = Array.from({ length: Math.max(i_arr.length, f_arr.length) }, (_, i) => (i_arr[i] ?? 0) + (f_arr[i] ?? 0));
```

### chained array operations

**Question:** Can we chain map then reduce?

**zero input:**
```zero
int i$ = [1, 2, 3, 4]
    int doubled$ = i$ * 2
    int sum = doubled$ + _
```

**python output:**
```python
import functools

i_arr: list[int] = [1, 2, 3, 4]
doubled_arr: list[int] = [x * 2 for x in i_arr]
sum: int = functools.reduce(lambda a, b: a + b, doubled_arr)
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [1, 2, 3, 4];
const doubled_arr: readonly int[] = i_arr.map(x => x * 2);
const sum: int = doubled_arr.reduce((a, b) => a + b);
```

### abstract function (no body)

**Question:** A function with no body is abstract (platform declaration)

**zero input:**
```zero
on (int r) = nothing (int a)
```

**python output:**
```python

```

**typescript output:**
```typescript
function fn_nothing__int(a: int): int {
    return r;
}
```

### struct with no defaults

**Question:** Can a struct have fields without default values?

**zero input:**
```zero
type pair =
        int first
        int second
```

**python output:**
```python
from typing import NamedTuple

class pair(NamedTuple):
    first: int = 0
    second: int = 0
```

**typescript output:**
```typescript
interface pair {
    readonly first: int;
    readonly second: int;
}

function pair(args: Partial<pair> = {}): pair {
    return { first: args.first ?? 0, second: args.second ?? 0 };
}
```

### struct with mixed defaults

**Question:** Can some fields have defaults and others not?

**zero input:**
```zero
type config =
        int width = 800
        int height = 600
        float scale
```

**python output:**
```python
from typing import NamedTuple

class config(NamedTuple):
    width: int = 800
    height: int = 600
    scale: float = 0
```

**typescript output:**
```typescript
interface config {
    readonly width: int;
    readonly height: int;
    readonly scale: float;
}

function config(args: Partial<config> = {}): config {
    return { width: args.width ?? 800, height: args.height ?? 600, scale: args.scale ?? 0 };
}
```

### multiple return assignments

**Question:** Should reject SSA violation

**zero input:**
```zero
on (int r) = bad (int a)
        r = a + 1
        r = a + 2
```

**correctly rejected:** line 3: variable 'r' already assigned (SSA violation)

### array in function body

**Question:** Can a function body create and use an array?

**zero input:**
```zero
on (int r) = sum squares up to (int n)
        int i$ = [1 through n]
        int sq$ = i$ * i$
        r = sq$ + _
```

**python output:**
```python
import functools

def fn_sum_squares_up_to__int(n: int) -> int:
    i_arr = list(range(1, n + 1))
    sq_arr = [x * x for x in i_arr]
    r = functools.reduce(lambda a, b: a + b, sq_arr)
    return r
```

**typescript output:**
```typescript
function fn_sum_squares_up_to__int(n: int): int {
    {'kind': 'var_decl', 'name': 'i', 'type': 'int', 'array': True, 'size': None, 'value': {'range': 'through', 'start': 1, 'end': 'n'}};
    {'kind': 'var_decl', 'name': 'sq', 'type': 'int', 'array': True, 'size': None, 'value': {'kind': 'binop', 'op': '*', 'left': {'kind': 'name', 'value': 'i$'}, 'right': {'kind': 'name', 'value': 'i$'}}};
    const r: int = {'kind': 'reduce', 'array': 'sq$', 'op': '+'};
    return r;
}
```

### ternary with struct

**Question:** Can a ternary return a struct?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (vector v) = pick (vector a) or (vector b) given (number t)
        v = (a) if (t > 0) else (b)
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_pick__vector_or__vector_given__number(a: vector, b: vector, t: number) -> vector:
    v = (a) if (t > 0) else (b)
    return v
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

function fn_pick__vector_or__vector_given__number(a: vector, b: vector, t: number): vector {
    const v: vector = (t > 0) ? (a) : (b);
    return v;
}
```

### function with single word name

**Question:** Can a function have just one word and one param?

**zero input:**
```zero
on (number n) = negate (number x)
        n = 0 - x
```

**python output:**
```python
def fn_negate__number(x: number) -> number:
    n = 0 - x
    return n
```

**typescript output:**
```typescript
function fn_negate__number(x: number): number {
    const n: number = 0 - x;
    return n;
}
```

### deeply nested expression

**Question:** Can expressions be deeply nested with parens?

**zero input:**
```zero
on (number n) = complex (number a) and (number b) and (number c)
        n = ((a + b) * (b + c)) + ((a - c) * (b - a))
```

**python output:**
```python
def fn_complex__number_and__number_and__number(a: number, b: number, c: number) -> number:
    n = a + b * b + c + a - c * b - a
    return n
```

**typescript output:**
```typescript
function fn_complex__number_and__number_and__number(a: number, b: number, c: number): number {
    const n: number = a + b * b + c + a - c * b - a;
    return n;
}
```

### scalar variable from fn call

**Question:** Can a scalar variable be assigned from a function call?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    number y = double (5)
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

y: number = fn_double__number(5)
```

**typescript output:**
```typescript
const y: number = fn_double__number(5);

function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

### scalar variable from multiword fn call

**Question:** Can a scalar variable be assigned from a multi-word function call?

**zero input:**
```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    number m = smaller of (3) and (7)
```

**python output:**
```python
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n

m: number = fn_smaller_of__number_and__number(3, 7)
```

**typescript output:**
```typescript
const m: number = fn_smaller_of__number_and__number(3, 7);

function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

### named fn map over array

**Question:** Can a named function be mapped over an array?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

i_arr: list[int] = [1, 2, 3, 4]
j_arr: list[int] = [fn_double__number(x) for x in i_arr]
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [1, 2, 3, 4];
const j_arr: readonly int[] = i_arr.map(x => fn_double__number(x));

function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

### multiword fn map with scalar

**Question:** Can a multi-word function be mapped with one array and one scalar arg?

**zero input:**
```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)
```

**python output:**
```python
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n

i_arr: list[int] = [3, 1, 4, 1, 5]
j_arr: list[int] = [fn_smaller_of__number_and__number(x, 3) for x in i_arr]
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [3, 1, 4, 1, 5];
const j_arr: readonly int[] = i_arr.map(x => fn_smaller_of__number_and__number(x, 3));

function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

### scalar var decl in body

**Question:** Can a function body declare a local variable?

**zero input:**
```zero
on (int r) = sum of (int a) and (int b)
        int c = a + b
        r = c
```

**python output:**
```python
def fn_sum_of__int_and__int(a: int, b: int) -> int:
    c = a + b
    r = c
    return r
```

**typescript output:**
```typescript
function fn_sum_of__int_and__int(a: int, b: int): int {
    {'kind': 'var_decl', 'name': 'c', 'type': 'int', 'array': False, 'value': {'kind': 'binop', 'op': '+', 'left': {'kind': 'name', 'value': 'a'}, 'right': {'kind': 'name', 'value': 'b'}}};
    const r: int = c;
    return r;
}
```

### array var decl in body

**Question:** Can a function body declare a local array?

**zero input:**
```zero
on (int r) = sum list
        int i$ = [1, 2, 3, 4]
        r = i$ + _
```

**python output:**
```python
import functools

def fn_sum_list() -> int:
    i_arr = [1, 2, 3, 4]
    r = functools.reduce(lambda a, b: a + b, i_arr)
    return r
```

**typescript output:**
```typescript
function fn_sum_list(): int {
    {'kind': 'var_decl', 'name': 'i', 'type': 'int', 'array': True, 'size': None, 'value': [1, 2, 3, 4]};
    const r: int = {'kind': 'reduce', 'array': 'i$', 'op': '+'};
    return r;
}
```

### range with variable in body

**Question:** Can a function body use a variable-endpoint range?

**zero input:**
```zero
on (int r) = sum up to (int n)
        int i$ = [1 through n]
        r = i$ + _
```

**python output:**
```python
import functools

def fn_sum_up_to__int(n: int) -> int:
    i_arr = list(range(1, n + 1))
    r = functools.reduce(lambda a, b: a + b, i_arr)
    return r
```

**typescript output:**
```typescript
function fn_sum_up_to__int(n: int): int {
    {'kind': 'var_decl', 'name': 'i', 'type': 'int', 'array': True, 'size': None, 'value': {'range': 'through', 'start': 1, 'end': 'n'}};
    const r: int = {'kind': 'reduce', 'array': 'i$', 'op': '+'};
    return r;
}
```

### map then reduce

**Question:** Can we map then reduce in sequence?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int doubled$ = double (i$)
    int sum = doubled$ + _
```

**python output:**
```python
import functools

def fn_double__number(x: number) -> number:
    n = x * 2
    return n

i_arr: list[int] = [1, 2, 3, 4]
doubled_arr: list[int] = [fn_double__number(x) for x in i_arr]
sum: int = functools.reduce(lambda a, b: a + b, doubled_arr)
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [1, 2, 3, 4];
const doubled_arr: readonly int[] = i_arr.map(x => fn_double__number(x));
const sum: int = doubled_arr.reduce((a, b) => a + b);

function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

### fn call in fn call in body

**Question:** Can function calls be nested three deep?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    on (number n) = octuple (number x)
        n = double (double (double (x)))
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

def fn_octuple__number(x: number) -> number:
    n = fn_double__number(fn_double__number(fn_double__number(x)))
    return n
```

**typescript output:**
```typescript
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}

function fn_octuple__number(x: number): number {
    const n: number = fn_double__number(fn_double__number(fn_double__number(x)));
    return n;
}
```

### fn call result used in binop

**Question:** Can a function call be part of a larger expression?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    on (number n) = double plus one (number x)
        n = double (x) + 1
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

def fn_double_plus_one__number(x: number) -> number:
    n = fn_double__number(x) + 1
    return n
```

**typescript output:**
```typescript
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}

function fn_double_plus_one__number(x: number): number {
    const n: number = fn_double__number(x) + 1;
    return n;
}
```

### reduce in function body

**Question:** Can a function body reduce a local array?

**zero input:**
```zero
on (int r) = sum squares of (int a) and (int b) and (int c)
        int vals$ = [a, b, c]
        int sq$ = vals$ * vals$
        r = sq$ + _
```

**python output:**
```python
import functools

def fn_sum_squares_of__int_and__int_and__int(a: int, b: int, c: int) -> int:
    vals_arr = [a, b, c]
    sq_arr = [x * x for x in vals_arr]
    r = functools.reduce(lambda a, b: a + b, sq_arr)
    return r
```

**typescript output:**
```typescript
function fn_sum_squares_of__int_and__int_and__int(a: int, b: int, c: int): int {
    {'kind': 'var_decl', 'name': 'vals', 'type': 'int', 'array': True, 'size': None, 'value': [{'kind': 'name', 'value': 'a'}, {'kind': 'name', 'value': 'b'}, {'kind': 'name', 'value': 'c'}]};
    {'kind': 'var_decl', 'name': 'sq', 'type': 'int', 'array': True, 'size': None, 'value': {'kind': 'binop', 'op': '*', 'left': {'kind': 'name', 'value': 'vals$'}, 'right': {'kind': 'name', 'value': 'vals$'}}};
    const r: int = {'kind': 'reduce', 'array': 'sq$', 'op': '+'};
    return r;
}
```

### ternary with fn call

**Question:** Can a ternary branch contain a function call?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    on (number n) = maybe double (number x) given (number flag)
        n = (double (x)) if (flag > 0) else (x)
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

def fn_maybe_double__number_given__number(x: number, flag: number) -> number:
    n = (fn_double__number(x)) if (flag > 0) else (x)
    return n
```

**typescript output:**
```typescript
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}

function fn_maybe_double__number_given__number(x: number, flag: number): number {
    const n: number = (flag > 0) ? (fn_double__number(x)) : (x);
    return n;
}
```

### struct constructor in fn call

**Question:** Can a function call pass a struct constructor as argument?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (number n) = length of (vector v)
        n = v.x * v.x + v.y * v.y + v.z * v.z
    number d = length of (vector(1, 2, 3))
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_length_of__vector(v: vector) -> number:
    n = v.x * v.x + v.y * v.y + v.z * v.z
    return n

d: number = fn_length_of__vector(vector(1, 2, 3))
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

const d: number = fn_length_of__vector(vector({ x: 1, y: 2, z: 3 }));

function fn_length_of__vector(v: vector): number {
    const n: number = v.x * v.x + v.y * v.y + v.z * v.z;
    return n;
}
```

### multiple fn calls in one expression

**Question:** Can multiple different functions appear in one expression?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    on (number n) = negate (number x)
        n = 0 - x
    on (number n) = weird (number x)
        n = double (x) + negate (x)
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

def fn_negate__number(x: number) -> number:
    n = 0 - x
    return n

def fn_weird__number(x: number) -> number:
    n = fn_double__number(x) + fn_negate__number(x)
    return n
```

**typescript output:**
```typescript
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}

function fn_negate__number(x: number): number {
    const n: number = 0 - x;
    return n;
}

function fn_weird__number(x: number): number {
    const n: number = fn_double__number(x) + fn_negate__number(x);
    return n;
}
```

### array of results from fn calls

**Question:** Can an array literal contain function call results?

**zero input:**
```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [double (1), double (2), double (3)]
```

**python output:**
```python
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

i_arr: list[int] = [fn_double__number(1), fn_double__number(2), fn_double__number(3)]
```

**typescript output:**
```typescript
const i_arr: readonly int[] = [fn_double__number(1), fn_double__number(2), fn_double__number(3)];

function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

### enum in ternary condition

**Question:** Can an enum value be used in a ternary?

**zero input:**
```zero
type direction = north | south | east | west
    on (int dx) = x step for (direction d)
        dx = (1) if (d == east) else (0)
```

**python output:**
```python
from enum import Enum

class direction(Enum):
    north = "north"
    south = "south"
    east = "east"
    west = "west"

def fn_x_step_for__direction(d: direction) -> int:
    dx = (1) if (d == direction.east) else (0)
    return dx
```

**typescript output:**
```typescript
enum direction {
    north = "north",
    south = "south",
    east = "east",
    west = "west",
}

function fn_x_step_for__direction(d: direction): int {
    const dx: int = (d == direction.east) ? (1) : (0);
    return dx;
}
```

### bitwise ops in function body

**Question:** Can bitwise operators be used in function bodies?

**zero input:**
```zero
on (uint8 r) = low nibble of (uint8 x)
        r = x & 15
```

**python output:**
```python
def fn_low_nibble_of__uint8(x: uint8) -> uint8:
    r = x & 15
    return r
```

**typescript output:**
```typescript
function fn_low_nibble_of__uint8(x: uint8): uint8 {
    const r: uint8 = x & 15;
    return r;
}
```

### chained member access in expression

**Question:** Can member access be used inside complex expressions?

**zero input:**
```zero
type vector =
        number x, y, z = 0
    on (number n) = distance squared between (vector a) and (vector b)
        number dx = a.x - b.x
        number dy = a.y - b.y
        number dz = a.z - b.z
        n = dx * dx + dy * dy + dz * dz
```

**python output:**
```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

def fn_distance_squared_between__vector_and__vector(a: vector, b: vector) -> number:
    dx = a.x - b.x
    dy = a.y - b.y
    dz = a.z - b.z
    n = dx * dx + dy * dy + dz * dz
    return n
```

**typescript output:**
```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

function fn_distance_squared_between__vector_and__vector(a: vector, b: vector): number {
    {'kind': 'var_decl', 'name': 'dx', 'type': 'number', 'array': False, 'value': {'kind': 'binop', 'op': '-', 'left': {'kind': 'member', 'object': 'a', 'field': 'x'}, 'right': {'kind': 'member', 'object': 'b', 'field': 'x'}}};
    {'kind': 'var_decl', 'name': 'dy', 'type': 'number', 'array': False, 'value': {'kind': 'binop', 'op': '-', 'left': {'kind': 'member', 'object': 'a', 'field': 'y'}, 'right': {'kind': 'member', 'object': 'b', 'field': 'y'}}};
    {'kind': 'var_decl', 'name': 'dz', 'type': 'number', 'array': False, 'value': {'kind': 'binop', 'op': '-', 'left': {'kind': 'member', 'object': 'a', 'field': 'z'}, 'right': {'kind': 'member', 'object': 'b', 'field': 'z'}}};
    const n: number = dx * dx + dy * dy + dz * dz;
    return n;
}
```

