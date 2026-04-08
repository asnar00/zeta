# test report: zero to typescript

## ts: int maps to number

### zero

```zero
int i = 42
```

### typescript

```typescript
const i: number = 42;
```

## ts: concrete int type

### zero

```zero
type int32 = ... 32-bit signed integer
```

### typescript

```typescript
type int32 = number;
```

## ts: concrete float type

### zero

```zero
type float32 = ... 32-bit IEEE floating-point number
```

### typescript

```typescript
type float32 = number;
```

## ts: abstract uint type

### zero

```zero
type uint = ... any-size unsigned integer
```

### typescript

```typescript
type uint = number;
```

## ts: abstract int type

### zero

```zero
type int = ... any-size signed integer
```

### typescript

```typescript
type int = number;
```

## ts: abstract float type

### zero

```zero
type float = ... any-size IEEE floating-point number
```

### typescript

```typescript
type float = number;
```

## ts: builtin number (no alias)

### zero

```zero
type number = int | float
```

### typescript

```typescript

```

## ts: enumeration

### zero

```zero
type tri-state = no | yes | maybe
```

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

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

## ts: struct with enum default

### zero

```zero
type direction = north | south | east | west
    type entity =
        direction facing = north
```

### typescript

```typescript
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

### typescript

```typescript
const i: number = 10;
```

## ts: scalar float variable

### zero

```zero
float f = 1.02
```

### typescript

```typescript
const f: number = 1.02;
```

## ts: string variable

### zero

```zero
string s = "hello"
```

### typescript

```typescript
const s: string = "hello";
```

## ts: struct constructor (positional)

### zero

```zero
vector v = vector(1, 2, 3)
```

### typescript

```typescript
const v: vector = vector(1, 2, 3);
```

## ts: struct constructor (named)

### zero

```zero
vector v = vector(z=2, x=1)
```

### typescript

```typescript
const v: vector = vector({ z: 2, x: 1 });
```

## ts: empty array

### zero

```zero
int i$
```

### typescript

```typescript
const i_arr: readonly number[] = [];
```

## ts: sized array

### zero

```zero
int i$[4]
```

### typescript

```typescript
const i_arr: readonly number[] = Array(4).fill(0);
```

## ts: sized array with fill

### zero

```zero
int i$[4] = 1
```

### typescript

```typescript
const i_arr: readonly number[] = Array(4).fill(1);
```

## ts: array literal

### zero

```zero
int i$ = [1, 2, 3, 4]
```

### typescript

```typescript
const i_arr: readonly number[] = [1, 2, 3, 4];
```

## ts: array with through range

### zero

```zero
int i$ = [1 through 4]
```

### typescript

```typescript
const i_arr: readonly number[] = Array.from({ length: 4 }, (_, i) => i + 1);
```

## ts: array with to range

### zero

```zero
int i$ = [0 to 4]
```

### typescript

```typescript
const i_arr: readonly number[] = Array.from({ length: 4 }, (_, i) => i);
```

## ts: simple stream

### zero

```zero
int i$ <- 1 <- 2 <- 3 <- 4
```

### typescript

```typescript
const i_arr: number[] = [1, 2, 3, 4];
```

## ts: stream with until

### zero

```zero
int i$ <- 1 <- (i$ + 1) until (i$ == 4)
```

### typescript

```typescript
const i_arr: number[] = [1];
while (true) {
    const _next = i_arr[i_arr.length - 1] + 1;
    i_arr.push(_next);
    if (_next == 4) break;
}
```

## ts: stream with while

### zero

```zero
int i$ <- 0 <- (i$ + 1) while (i$ < 4)
```

### typescript

```typescript
const i_arr: number[] = [0];
while (true) {
    const _next = i_arr[i_arr.length - 1] + 1;
    if (!(_next < 4)) break;
    i_arr.push(_next);
}
```

## ts: array map with scalar

### zero

```zero
int i$ = [1, 2, 3, 4]
    int j$ = i$ * 2
```

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
const i_arr: readonly number[] = [1, 2, 3, 4];
const sum: number = i_arr.reduce((a, b) => a + b);
```

## ts: array reduce with named function

### zero

```zero
int i$ = [1, 2, 3, 4]
    int min = smaller of (i$) and (_)
```

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}

// @zero on run
async function fn_run(): void {
    await _concurrently(() => hello(), () => beep());
}

// @zero on main
async function fn_main(): void {
    await fn_run();
}
```

## ts: sync function stays sync

### zero

```zero
on (number n) = double (number x)
        n = x * 2
```

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
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

### typescript

```typescript
// timed stream iteration fallback (standalone build)
function* _timed_iterate(_name: string, _iter: any): any { yield* _iter; }

const all_arr: readonly number[] = [1, 2, 3, 4, 5, 6];
const even_arr: number[] = [...task_only_evens_from__int(all_arr)];

// @zero on (int even$) <- only evens from (int numbers$)
function* task_only_evens_from__int(numbers_arr: readonly number[]): Generator<number> {
    for (const n of _timed_iterate("numbers_arr", numbers_arr)) {
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

### typescript

```typescript
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

### typescript

```typescript
const i_arr: readonly number[] = [1, 2, 3, 4, 5, 6];
const evens_arr: number[] = i_arr.filter(x => x % 2 == 0);
```

## ts: first of where

### zero

```zero
int i$ = [1, 2, 3, 4, 5, 6]
    int first_even = first of [i$] where (_ % 2 == 0)
```

### typescript

```typescript
const i_arr: readonly number[] = [1, 2, 3, 4, 5, 6];
const first_even: number = i_arr.find(x => x % 2 == 0)!;
```

## ts: sort simple

### zero

```zero
int i$ = [3, 1, 4]
    int sorted$ = sort [i$]
```

### typescript

```typescript
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

### typescript

```typescript
const people_arr: readonly person[] = [...];
const sorted_arr: person[] = [...people_arr].sort((a, b) => a.age - b.age);

interface person {
    readonly age: number;
}

function person(args: Partial<person> = {}): person {
    return { age: args.age ?? 0 };
}
```

## ts: array function call

### zero

```zero
on (int n) = count of [items$]
        n = length of [items$]
    int i$ = [1, 2, 3]
    int c = count of [i$]
```

### typescript

```typescript
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

### typescript

```typescript
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

