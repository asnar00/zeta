# test report
*zero to python translation examples*

## concrete int type

### zero

```zero
type int32 = ... 32-bit signed integer
```

### python

```python
type int32 = int
```

## concrete float type

### zero

```zero
type float32 = ... 32-bit IEEE floating-point number
```

### python

```python
type float32 = float
```

## abstract uint type

### zero

```zero
type uint = ... any-size unsigned integer
```

### python

```python
type uint = int
```

## builtin int (no alias)

### zero

```zero
type int = ... any-size signed integer
```

### python

```python

```

## builtin float (no alias)

### zero

```zero
type float = ... any-size IEEE floating-point number
```

### python

```python

```

## union type

### zero

```zero
type number = int | float
```

### python

```python
type number = int | float
```

## enumeration

### zero

```zero
type tri-state = no | yes | maybe
```

### python

```python
from enum import Enum

class tri_state(Enum):
    no = "no"
    yes = "yes"
    maybe = "maybe"
```

## structure type

### zero

```zero
type vector =
        number x, y, z = 0
```

### python

```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0
```

## struct with enum default

### zero

```zero
type direction = north | south | east | west
    type entity =
        direction facing = north
```

### python

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
```

## scalar int variable

### zero

```zero
int32 i = 10
```

### python

```python
i: int32 = 10
```

## scalar float variable

### zero

```zero
float f = 1.02
```

### python

```python
f: float = 1.02
```

## string variable

### zero

```zero
string s = "hello"
```

### python

```python
s: str = "hello"
```

## struct constructor (positional)

### zero

```zero
vector v = vector(1, 2, 3)
```

### python

```python
v: vector = vector(1, 2, 3)
```

## struct constructor (named)

### zero

```zero
vector v = vector(z=2, x=1)
```

### python

```python
v: vector = vector(z=2, x=1)
```

## empty array

### zero

```zero
int i$
```

### python

```python
i_arr: list[int] = []
```

## sized array

### zero

```zero
int i$[4]
```

### python

```python
i_arr: list[int] = [0] * 4
```

## sized array with fill

### zero

```zero
int i$[4] = 1
```

### python

```python
i_arr: list[int] = [1] * 4
```

## array literal

### zero

```zero
int i$ = [1, 2, 3, 4]
```

### python

```python
i_arr: list[int] = [1, 2, 3, 4]
```

## array with through range

### zero

```zero
int i$ = [1 through 4]
```

### python

```python
i_arr: list[int] = list(range(1, 5))
```

## array with to range

### zero

```zero
int i$ = [0 to 4]
```

### python

```python
i_arr: list[int] = list(range(0, 4))
```

## simple stream

### zero

```zero
int i$ <- 1 <- 2 <- 3 <- 4
```

### python

```python
i_arr = [1, 2, 3, 4]
```

## self-referencing stream

### zero

```zero
int i$ <- 1 <- (i$ + 1)
```

### python

```python
i_arr = [1]
i_arr.append(i_arr[-1] + 1)
```

## stream with until

### zero

```zero
int i$ <- 1 <- (i$ + 1) until (i$ == 4)
```

### python

```python
i_arr = [1]
while not (i_arr[-1] == 4):
    i_arr.append(i_arr[-1] + 1)
```

## stream with while

### zero

```zero
int i$ <- 0 <- (i$ + 1) while (i$ < 4)
```

### python

```python
i_arr = [0]
while i_arr[-1] < 4:
    i_arr.append(i_arr[-1] + 1)
```

## array map with scalar

### zero

```zero
int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2
```

### python

```python
i_arr: list[int] = [1, 2, 3, 4]
j_arr: list[int] = [x * 2 for x in i_arr]
```

## array map with two arrays

### zero

```zero
int i$ = [1, 2, 3, 4, 5]
    int j$ = [1, 4, 7]
    int k$ = i$ + j$
```

### python

```python
from itertools import zip_longest

i_arr: list[int] = [1, 2, 3, 4, 5]
j_arr: list[int] = [1, 4, 7]
k_arr: list[int] = [a + b for a, b in zip_longest(i_arr, j_arr, fillvalue=0)]
```

## array literal with fn calls

### zero

```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [double (1), double (2), double (3)]
```

### python

```python
# @zero on (number n) = double (number x)
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

i_arr: list[int] = [fn_double__number(1), fn_double__number(2), fn_double__number(3)]
```

## named function mapped over array

### zero

```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)
```

### python

```python
# @zero on (number n) = double (number x)
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

i_arr: list[int] = [1, 2, 3, 4]
j_arr: list[int] = [fn_double__number(x) for x in i_arr]
```

## named function map with scalar arg

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)
```

### python

```python
# @zero on (number n) = smaller of (number a) and (number b)
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n

i_arr: list[int] = [3, 1, 4, 1, 5]
j_arr: list[int] = [fn_smaller_of__number_and__number(x, 3) for x in i_arr]
```

## array reduce with operator

### zero

```zero
int i$ = [1, 2, 3, 4]
    int sum = i$ + _
```

### python

```python
import functools

i_arr: list[int] = [1, 2, 3, 4]
sum: int = functools.reduce(lambda a, b: a + b, i_arr)
```

## array reduce with named function

### zero

```zero
int i$ = [1, 2, 3, 4]
    int min = smaller of (i$) and (_)
```

### python

```python
import functools

i_arr: list[int] = [1, 2, 3, 4]
min: int = functools.reduce(fn_smaller_of__int_and__int, i_arr)
```

## operator function

### zero

```zero
on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
```

### python

```python
# @zero on (vector v) = (vector a) + (vector b)
def fn__vector_plus__vector(a: vector, b: vector) -> vector:
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    return v
```

## void function

### zero

```zero
on hello()
        print "hello world"
```

### python

```python
# @zero on hello
def fn_hello():
    print "hello world"
```

## void function with params

### zero

```zero
on greet (string name)
        print name
```

### python

```python
# @zero on greet (string name)
def fn_greet__string(name: str):
    print name
```

## concurrently block

### zero

```zero
on run()
        concurrently
            hello()
        and
            beep()
```

### python

```python
import threading

def _concurrently(*fns):
    threads = [threading.Thread(target=fn) for fn in fns]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

# @zero on run
def fn_run():
    _concurrently(lambda: hello(), lambda: beep())
```

## named function

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
```

### python

```python
# @zero on (number n) = smaller of (number a) and (number b)
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n
```

## where filter

### zero

```zero
int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)
```

### python

```python
i_arr: list[int] = [1, 2, 3, 4, 5, 6]
evens_arr = [x for x in i_arr if x % 2 == 0]
```

## first of where

### zero

```zero
int i$ = [1, 2, 3, 4, 5, 6]
    int first_even = first of [i$] where (_ % 2 == 0)
```

### python

```python
i_arr: list[int] = [1, 2, 3, 4, 5, 6]
first_even: int = next(x for x in i_arr if x % 2 == 0)
```

## sort simple

### zero

```zero
int i$ = [3, 1, 4, 1, 5]
    int sorted$ = sort [i$]
```

### python

```python
i_arr: list[int] = [3, 1, 4, 1, 5]
sorted_arr = sorted(i_arr)
```

## sort by key

### zero

```zero
type person =
        int age = 0
    person people$ = [...]
    person sorted$ = sort [people$] by (_.age)
```

### python

```python
from typing import NamedTuple

class person(NamedTuple):
    age: int = 0

people_arr: list[person] = [...]
sorted_arr = sorted(people_arr, key=lambda x: x.age)
```

## slice with to

### zero

```zero
on (int result$) = first three of (int items$)
        result$ = items$[0 to 3]
```

### python

```python
# @zero on (int result$) = first three of (int items$)
def fn_first_three_of__int(items_arr: int) -> int:
    result$ = items_arr[0:3]
    return result_arr
```

## slice with through

### zero

```zero
on (int result$) = mid of (int items$)
        result$ = items$[1 through 3]
```

### python

```python
# @zero on (int result$) = mid of (int items$)
def fn_mid_of__int(items_arr: int) -> int:
    result$ = items_arr[1:4]
    return result_arr
```

## subtype flattens parent fields

### zero

```zero
type animal
        string name = ""
    type dog = animal +
        string breed = "unknown"
```

### python

```python
from typing import NamedTuple

class animal(NamedTuple):
    name: str = ""

class dog(NamedTuple):
    name: str = ""
    breed: str = "unknown"
```

## single dispatch (no table)

### zero

```zero
on (string s) = describe (int n)
        s = "a number"
```

### python

```python
# @zero on (string s) = describe (int n)
def fn_describe__int(n: int) -> str:
    s = "a number"
    return s
```

## multiple dispatch

### zero

```zero
type animal
    type dog = animal +
        string breed = "unknown"
    type cat = animal +
        int lives = 9
    on (string s) = describe (animal a)
        s = "an animal"
    on (string s) = describe (dog d)
        s = "a dog"
    on (string s) = describe (cat c)
        s = "a cat"
```

### python

```python
from typing import NamedTuple

class animal(NamedTuple):
    pass

class dog(NamedTuple):
    breed: str = "unknown"

class cat(NamedTuple):
    lives: int = 9

# @zero on (string s) = describe (animal a)
def fn_describe__animal(a: animal) -> str:
    s = "an animal"
    return s

# @zero on (string s) = describe (dog d)
def fn_describe__dog(d: dog) -> str:
    s = "a dog"
    return s

# @zero on (string s) = describe (cat c)
def fn_describe__cat(c: cat) -> str:
    s = "a cat"
    return s

def fn_describe(d) -> str:
    if isinstance(d, dog):
        return fn_describe__dog(d)
    elif isinstance(d, cat):
        return fn_describe__cat(d)
    elif isinstance(d, animal):
        return fn_describe__animal(d)
    return fn_describe__animal(d)
```

## recursive function

### zero

```zero
on (int n) = factorial of (int x)
        n = (1) if (x == 1) else (x * factorial of (x - 1))
```

### python

```python
# @zero on (int n) = factorial of (int x)
def fn_factorial_of__int(x: int) -> int:
    n = (1) if (x == 1) else (x * fn_factorial_of__int(x - 1))
    return n
```

## array function definition

### zero

```zero
on (int n) = count of [items$]
        n = length of [items$]
```

### python

```python
# @zero on (int n) = count of [items$]
def fn_count_of(items_arr) -> int:
    n = len(items_arr)
    return n
```

## array function call

### zero

```zero
on (int n) = count of [items$]
        n = length of [items$]
    int i$ = [1, 2, 3]
    int c = count of [i$]
```

### python

```python
# @zero on (int n) = count of [items$]
def fn_count_of(items_arr) -> int:
    n = len(items_arr)
    return n

i_arr: list[int] = [1, 2, 3]
c: int = fn_count_of(i_arr)
```

## array index

### zero

```zero
on (int r) = first of (int items$)
        r = items$[0]
```

### python

```python
# @zero on (int r) = first of (int items$)
def fn_first_of__int(items_arr: int) -> int:
    r = items_arr[0]
    return r
```

## slice open end

### zero

```zero
on (int result$) = rest of (int items$)
        result$ = items$[1:]
```

### python

```python
# @zero on (int result$) = rest of (int items$)
def fn_rest_of__int(items_arr: int) -> int:
    result$ = items_arr[1:]
    return result_arr
```

## slice open start

### zero

```zero
on (int result$) = first three of (int items$)
        result$ = items$[:3]
```

### python

```python
# @zero on (int result$) = first three of (int items$)
def fn_first_three_of__int(items_arr: int) -> int:
    result$ = items_arr[:3]
    return result_arr
```

## length of array

### zero

```zero
on (int n) = size of (int items$)
        n = length of [items$]
```

### python

```python
# @zero on (int n) = size of (int items$)
def fn_size_of__int(items_arr: int) -> int:
    n = len(items_arr)
    return n
```

## if/else block

### zero

```zero
on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else
            s = "zero"
```

### python

```python
# @zero on (string s) = describe (int n)
def fn_describe__int(n: int) -> str:
    if n > 0:
        s = "positive"
    else:
        s = "zero"
    return s
```

## if/else if/else block

### zero

```zero
on (string s) = describe (int n)
        if (n > 0)
            s = "positive"
        else if (n < 0)
            s = "negative"
        else
            s = "zero"
```

### python

```python
# @zero on (string s) = describe (int n)
def fn_describe__int(n: int) -> str:
    if n > 0:
        s = "positive"
    elif n < 0:
        s = "negative"
    else:
        s = "zero"
    return s
```

## range with variable endpoint

### zero

```zero
on (int r) = sum up to (int n)
        int i$ = [1 through n]
        r = i$ + _
```

### python

```python
import functools

# @zero on (int r) = sum up to (int n)
def fn_sum_up_to__int(n: int) -> int:
    i_arr = list(range(1, n + 1))
    r = functools.reduce(lambda a, b: a + b, i_arr)
    return r
```

## var declaration in function body

### zero

```zero
on (int r) = sum of (int a) and (int b)
        int c = a + b
        r = c
```

### python

```python
# @zero on (int r) = sum of (int a) and (int b)
def fn_sum_of__int_and__int(a: int, b: int) -> int:
    c = a + b
    r = c
    return r
```

## array declaration in function body

### zero

```zero
on (int r) = sum list
        int i$ = [1, 2, 3, 4]
        r = i$ + _
```

### python

```python
import functools

# @zero on (int r) = sum list
def fn_sum_list() -> int:
    i_arr = [1, 2, 3, 4]
    r = functools.reduce(lambda a, b: a + b, i_arr)
    return r
```

## function calling function

### zero

```zero
on (number n) = double (number x)
        n = x * 2
    on (number n) = quadruple (number x)
        n = double (double (x))
```

### python

```python
# @zero on (number n) = double (number x)
def fn_double__number(x: number) -> number:
    n = x * 2
    return n

# @zero on (number n) = quadruple (number x)
def fn_quadruple__number(x: number) -> number:
    n = fn_double__number(fn_double__number(x))
    return n
```

## multi-word function call

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)
```

### python

```python
# @zero on (number n) = smaller of (number a) and (number b)
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n

# @zero on (number n) = smallest of (number a) and (number b) and (number c)
def fn_smallest_of__number_and__number_and__number(a: number, b: number, c: number) -> number:
    n = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)
    return n
```

## enum value in expression

### zero

```zero
type direction = north | south | east | west
    on (int dx) = x step for (direction d)
        dx = (1) if (d == east) else (0)
```

### python

```python
from enum import Enum

class direction(Enum):
    north = "north"
    south = "south"
    east = "east"
    west = "west"

# @zero on (int dx) = x step for (direction d)
def fn_x_step_for__direction(d: direction) -> int:
    dx = (1) if (d == direction.east) else (0)
    return dx
```

## fn call assigned to scalar variable

### zero

```zero
type vector =
        number x, y, z = 0
    on (number n) = length of (vector v)
        n = v.x * v.x + v.y * v.y + v.z * v.z
    number d = length of (vector(1, 2, 3))
```

### python

```python
from typing import NamedTuple

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

# @zero on (number n) = length of (vector v)
def fn_length_of__vector(v: vector) -> number:
    n = v.x * v.x + v.y * v.y + v.z * v.z
    return n

d: number = fn_length_of__vector(vector(1, 2, 3))
```

## task: filter evens

### zero

```zero
on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)
```

### python

```python
# @zero on (int even$) <- only evens from (int numbers$)
def task_only_evens_from__int(numbers_arr: int):
    for n in numbers_arr:
        if n % 2 == 0:
            yield n

all_arr: list[int] = [1, 2, 3, 4, 5, 6]
even_arr = list(task_only_evens_from__int(all_arr))
```

## full program

### zero

```zero
type int32 = ... 32-bit signed integer
    type float32 = ... 32-bit IEEE floating-point number
    type number = int | float
    type tri-state = no | yes | maybe
    type vector =
        number x, y, z = 0

    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
```

### python

```python
from enum import Enum
from typing import NamedTuple

type int32 = int

type float32 = float

type number = int | float

class tri_state(Enum):
    no = "no"
    yes = "yes"
    maybe = "maybe"

class vector(NamedTuple):
    x: number = 0
    y: number = 0
    z: number = 0

# @zero on (vector v) = (vector a) + (vector b)
def fn__vector_plus__vector(a: vector, b: vector) -> vector:
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
    return v

# @zero on (number n) = smaller of (number a) and (number b)
def fn_smaller_of__number_and__number(a: number, b: number) -> number:
    n = (a) if (a < b) else (b)
    return n
```

## ts: int maps to number

### zero

```zero
int i = 42
```

### python

```python
const i: number = 42;
```

## ts: concrete int type

### zero

```zero
type int32 = ... 32-bit signed integer
```

### python

```python
type int32 = number;
```

## ts: concrete float type

### zero

```zero
type float32 = ... 32-bit IEEE floating-point number
```

### python

```python
type float32 = number;
```

## ts: abstract uint type

### zero

```zero
type uint = ... any-size unsigned integer
```

### python

```python
type uint = number;
```

## ts: abstract int type

### zero

```zero
type int = ... any-size signed integer
```

### python

```python
type int = number;
```

## ts: abstract float type

### zero

```zero
type float = ... any-size IEEE floating-point number
```

### python

```python
type float = number;
```

## ts: builtin number (no alias)

### zero

```zero
type number = int | float
```

### python

```python

```

## ts: enumeration

### zero

```zero
type tri-state = no | yes | maybe
```

### python

```python
enum tri_state {
    no = "no",
    yes = "yes",
    maybe = "maybe",
}
```

## ts: structure type

### zero

```zero
type vector =
        number x, y, z = 0
```

### python

```python
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}
```

## ts: struct factory function

### zero

```zero
type vector =
        number x, y, z = 0
```

### python

```python
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}
```

## ts: nested struct factory

### zero

```zero
type vector =
        number x, y, z = 0
    type line =
        vector start, end
```

### python

```python
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

## ts: struct with enum default

### zero

```zero
type direction = north | south | east | west
    type entity =
        direction facing = north
```

### python

```python
enum direction {
    north = "north",
    south = "south",
    east = "east",
    west = "west",
}

interface entity {
    readonly facing: direction;
}

function entity(args: Partial<entity> = {}): entity {
    return { facing: args.facing ?? direction.north };
}
```

## ts: scalar int variable

### zero

```zero
int32 i = 10
```

### python

```python
const i: number = 10;
```

## ts: scalar float variable

### zero

```zero
float f = 1.02
```

### python

```python
const f: number = 1.02;
```

## ts: string variable

### zero

```zero
string s = "hello"
```

### python

```python
const s: string = "hello";
```

## ts: struct constructor (positional)

### zero

```zero
vector v = vector(1, 2, 3)
```

### python

```python
const v: vector = vector(1, 2, 3);
```

## ts: struct constructor (named)

### zero

```zero
vector v = vector(z=2, x=1)
```

### python

```python
const v: vector = vector({ z: 2, x: 1 });
```

## ts: empty array

### zero

```zero
int i$
```

### python

```python
const i_arr: readonly number[] = [];
```

## ts: sized array

### zero

```zero
int i$[4]
```

### python

```python
const i_arr: readonly number[] = Array(4).fill(0);
```

## ts: sized array with fill

### zero

```zero
int i$[4] = 1
```

### python

```python
const i_arr: readonly number[] = Array(4).fill(1);
```

## ts: array literal

### zero

```zero
int i$ = [1, 2, 3, 4]
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4];
```

## ts: array with through range

### zero

```zero
int i$ = [1 through 4]
```

### python

```python
const i_arr: readonly number[] = Array.from({ length: 4 }, (_, i) => i + 1);
```

## ts: array with to range

### zero

```zero
int i$ = [0 to 4]
```

### python

```python
const i_arr: readonly number[] = Array.from({ length: 4 }, (_, i) => i);
```

## ts: simple stream

### zero

```zero
int i$ <- 1 <- 2 <- 3 <- 4
```

### python

```python
const i_arr: number[] = [1, 2, 3, 4];
```

## ts: stream with until

### zero

```zero
int i$ <- 1 <- (i$ + 1) until (i$ == 4)
```

### python

```python
const i_arr: number[] = [1];
while (!(i_arr[i_arr.length - 1] == 4)) {
    i_arr.push(i_arr[i_arr.length - 1] + 1);
}
```

## ts: stream with while

### zero

```zero
int i$ <- 0 <- (i$ + 1) while (i$ < 4)
```

### python

```python
const i_arr: number[] = [0];
while (i_arr[i_arr.length - 1] < 4) {
    i_arr.push(i_arr[i_arr.length - 1] + 1);
}
```

## ts: array map with scalar

### zero

```zero
int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4];
const j_arr: readonly number[] = i_arr.map(x => x * 2);
```

## ts: array map with two arrays

### zero

```zero
int i$ = [1, 2, 3, 4, 5]
    int j$ = [1, 4, 7]
    int k$ = i$ + j$
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4, 5];
const j_arr: readonly number[] = [1, 4, 7];
const k_arr: readonly number[] = Array.from({ length: Math.max(i_arr.length, j_arr.length) }, (_, i) => (i_arr[i] ?? 0) + (j_arr[i] ?? 0));
```

## ts: named function mapped over array

### zero

```zero
on (number n) = double (number x)
        n = x * 2
    int i$ = [1, 2, 3, 4]
    int j$ = double (i$)
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4];
const j_arr: readonly number[] = i_arr.map(x => fn_double__number(x));

// @zero on (number n) = double (number x)
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

## ts: named function map with scalar arg

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    int i$ = [3, 1, 4, 1, 5]
    int j$ = smaller of (i$) and (3)
```

### python

```python
const i_arr: readonly number[] = [3, 1, 4, 1, 5];
const j_arr: readonly number[] = i_arr.map(x => fn_smaller_of__number_and__number(x, 3));

// @zero on (number n) = smaller of (number a) and (number b)
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

## ts: array reduce with operator

### zero

```zero
int i$ = [1, 2, 3, 4]
    int sum = i$ + _
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4];
const sum: number = i_arr.reduce((a, b) => a + b);
```

## ts: array reduce with named function

### zero

```zero
int i$ = [1, 2, 3, 4]
    int min = smaller of (i$) and (_)
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4];
const min: number = i_arr.reduce(fn_smaller_of__int_and__int);
```

## ts: concurrently block

### zero

```zero
on run()
        concurrently
            hello()
        and
            beep()
```

### python

```python
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}

// @zero on run
async function fn_run(): void {
    await _concurrently(() => hello(), () => beep());
}
```

## ts: async function with concurrently

### zero

```zero
on run()
        concurrently
            hello()
        and
            beep()
```

### python

```python
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}

// @zero on run
async function fn_run(): void {
    await _concurrently(() => hello(), () => beep());
}
```

## ts: async propagation to caller

### zero

```zero
on run()
        concurrently
            hello()
        and
            beep()
    on main()
        run()
```

### python

```python
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}

// @zero on run
async function fn_run(): void {
    await _concurrently(() => hello(), () => beep());
}

// @zero on main
async function fn_main(): void {
    await run();
}
```

## ts: sync function stays sync

### zero

```zero
on (number n) = double (number x)
        n = x * 2
```

### python

```python
// @zero on (number n) = double (number x)
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}
```

## ts: operator function

### zero

```zero
on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
```

### python

```python
// @zero on (vector v) = (vector a) + (vector b)
function fn__vector_plus__vector(a: vector, b: vector): vector {
    const v: vector = vector(a.x + b.x, a.y + b.y, a.z + b.z);
    return v;
}
```

## ts: named function

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
```

### python

```python
// @zero on (number n) = smaller of (number a) and (number b)
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

## ts: ternary syntax

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
```

### python

```python
// @zero on (number n) = smaller of (number a) and (number b)
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

## ts: function calling function

### zero

```zero
on (number n) = double (number x)
        n = x * 2
    on (number n) = quadruple (number x)
        n = double (double (x))
```

### python

```python
// @zero on (number n) = double (number x)
function fn_double__number(x: number): number {
    const n: number = x * 2;
    return n;
}

// @zero on (number n) = quadruple (number x)
function fn_quadruple__number(x: number): number {
    const n: number = fn_double__number(fn_double__number(x));
    return n;
}
```

## ts: multi-word function call

### zero

```zero
on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
    on (number n) = smallest of (number a) and (number b) and (number c)
        n = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c)
```

### python

```python
// @zero on (number n) = smaller of (number a) and (number b)
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}

// @zero on (number n) = smallest of (number a) and (number b) and (number c)
function fn_smallest_of__number_and__number_and__number(a: number, b: number, c: number): number {
    const n: number = fn_smaller_of__number_and__number(fn_smaller_of__number_and__number(a, b), c);
    return n;
}
```

## ts: task: filter evens

### zero

```zero
on (int even$) <- only evens from (int numbers$)
        int n <- numbers$
        if (n % 2 == 0)
            even$ <- n
    int all$ = [1, 2, 3, 4, 5, 6]
    int even$ <- only evens from (all$)
```

### python

```python
const all_arr: readonly number[] = [1, 2, 3, 4, 5, 6];
const even_arr: number[] = [...task_only_evens_from__int(all_arr)];

// @zero on (int even$) <- only evens from (int numbers$)
function* task_only_evens_from__int(numbers_arr: readonly number[]): Generator<number> {
    for (const n of numbers_arr) {
        if (n % 2 == 0) {
            yield n;
        }
    }
}
```

## ts: subtype flattens parent fields

### zero

```zero
type animal
        string name = ""
    type dog = animal +
        string breed = "unknown"
```

### python

```python
interface animal {
    readonly name: string;
}

function animal(args: Partial<animal> = {}): animal {
    return { name: args.name ?? "" };
}

interface dog {
    readonly name: string;
    readonly breed: string;
}

function dog(args: Partial<dog> = {}): dog {
    return { name: args.name ?? "", breed: args.breed ?? "unknown" };
}
```

## ts: where filter

### zero

```zero
int i$ = [1, 2, 3, 4, 5, 6]
    int evens$ = [i$] where (_ % 2 == 0)
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4, 5, 6];
const evens_arr: number[] = i_arr.filter(x => x % 2 == 0);
```

## ts: first of where

### zero

```zero
int i$ = [1, 2, 3, 4, 5, 6]
    int first_even = first of [i$] where (_ % 2 == 0)
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3, 4, 5, 6];
const first_even: number = i_arr.find(x => x % 2 == 0);
```

## ts: sort simple

### zero

```zero
int i$ = [3, 1, 4]
    int sorted$ = sort [i$]
```

### python

```python
const i_arr: readonly number[] = [3, 1, 4];
const sorted_arr: number[] = [...i_arr].sort();
```

## ts: sort by key

### zero

```zero
type person =
        int age = 0
    person people$ = [...]
    person sorted$ = sort [people$] by (_.age)
```

### python

```python
interface person {
    readonly age: number;
}

function person(args: Partial<person> = {}): person {
    return { age: args.age ?? 0 };
}

const people_arr: readonly person[] = [...];
const sorted_arr: person[] = [...people_arr].sort((a, b) => a.age - b.age);
```

## ts: array function call

### zero

```zero
on (int n) = count of [items$]
        n = length of [items$]
    int i$ = [1, 2, 3]
    int c = count of [i$]
```

### python

```python
const i_arr: readonly number[] = [1, 2, 3];
const c: number = fn_count_of(i_arr);

// @zero on (int n) = count of [items$]
function fn_count_of(items_arr): number {
    const n: number = items_arr.length;
    return n;
}
```

## ts: multiple dispatch

### zero

```zero
type animal
    type dog = animal +
        string breed = "unknown"
    on (string s) = describe (animal a)
        s = "an animal"
    on (string s) = describe (dog d)
        s = "a dog"
```

### python

```python
interface animal {
}

function animal(args: Partial<animal> = {}): animal {
    return {  };
}

interface dog {
    readonly breed: string;
}

function dog(args: Partial<dog> = {}): dog {
    return { breed: args.breed ?? "unknown" };
}

// @zero on (string s) = describe (animal a)
function fn_describe__animal(a: animal): string {
    const s: string = "an animal";
    return s;
}

// @zero on (string s) = describe (dog d)
function fn_describe__dog(d: dog): string {
    const s: string = "a dog";
    return s;
}

function fn_describe(d): string {
    if (d instanceof dog) {
        return fn_describe__dog(d);
    } else if (d instanceof animal) {
        return fn_describe__animal(d);
    } else {
        return fn_describe__animal(d);
    }
}
```

## ts: full program

### zero

```zero
type number = int | float
    type vector =
        number x, y, z = 0

    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
```

### python

```python
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}

function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}

// @zero on (vector v) = (vector a) + (vector b)
function fn__vector_plus__vector(a: vector, b: vector): vector {
    const v: vector = vector({ x: a.x + b.x, y: a.y + b.y, z: a.z + b.z });
    return v;
}

// @zero on (number n) = smaller of (number a) and (number b)
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

