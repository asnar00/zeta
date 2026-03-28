# zero to ts
*rules for translating zero to typescript*

This document defines how each zero language construct maps to TypeScript. Each section covers one construct, with zero examples followed by their TypeScript translations.

## concrete numeric types

TypeScript has a single `number` type, so all numeric types alias to it.

```zero
type int32 = ... 32-bit signed integer
type uint8 = ... 8-bit unsigned integer
type float32 = ... 32-bit IEEE floating-point number
```

```typescript
type int32 = number;
type uint8 = number;
type float32 = number;
```

## abstract numeric types

Abstract numeric types also alias to `number`. Builtins (`number`) need no alias.

```zero
type int = ... any-size signed integer
type uint = ... any-size unsigned integer
type float = ... any-size IEEE floating-point number
type number = int | float
```

```typescript
type int = number;
type uint = number;
type float = number;
// number is a builtin — no alias needed
```

## enumerations

Enumerations map to TypeScript string enums.

```zero
type tri-state = no | yes | maybe
```

```typescript
enum tri_state {
    no = "no",
    yes = "yes",
    maybe = "maybe",
}
```

Naming rule: the type name is kept lowercase (hyphens become underscores).

## structure types

Structure types map to `interface` with `readonly` fields, since zero enforces purity and single-assignment.

```zero
type vector =
    number x, y, z = 0
```

```typescript
interface vector {
    readonly x: number;
    readonly y: number;
    readonly z: number;
}
```

Default values are applied in a factory function, not in the interface.

A factory function is emitted for each struct to handle construction with defaults:

```typescript
function vector(args: Partial<vector> = {}): vector {
    return { x: args.x ?? 0, y: args.y ?? 0, z: args.z ?? 0 };
}
```

Naming rule: the type name is kept lowercase (hyphens become underscores).

## variables

Variables map to `const` declarations with type annotations (since zero is single-assignment).

```zero
int32 i = 10
float f = 1.02
```

```typescript
const i: int32 = 10;
const f: float = 1.02;
```

## structure constructors

Structure constructors use the factory function.

```zero
vector v = vector()
vector v = vector(1, 2, 3)
vector v = vector(z=2, x=1)
```

```typescript
const v: vector = vector();
const v: vector = vector({ x: 1, y: 2, z: 3 });
const v: vector = vector({ z: 2, x: 1 });
```

Note: positional args map to fields in declaration order.

## functions

Functions use the same naming convention as Python: `fn` prefix, double-underscore before parameter types, symbols replaced with words.

```zero
on (vector v) = (vector a) + (vector b)
    v = vector(a.x + b.x, a.y + b.y, a.z + b.z)
```

```typescript
function fn__vector_plus__vector(a: vector, b: vector): vector {
    const v: vector = vector({ x: a.x + b.x, y: a.y + b.y, z: a.z + b.z });
    return v;
}
```

```zero
on (number n) = smaller of (number a) and (number b)
    n = (a) if (a < b) else (b)
```

```typescript
function fn_smaller_of__number_and__number(a: number, b: number): number {
    const n: number = (a < b) ? (a) : (b);
    return n;
}
```

Note: zero's `(x) if (cond) else (y)` maps to TypeScript's `(cond) ? (x) : (y)`.

## arrays

Arrays map to `readonly` TypeScript arrays. The `$` is replaced with `_arr`.

```zero
int i$
```

```typescript
const i_arr: readonly int[] = [];
```

```zero
int i$[4]
```

```typescript
const i_arr: readonly int[] = Array(4).fill(0);
```

```zero
int i$[4] = 1
```

```typescript
const i_arr: readonly int[] = Array(4).fill(1);
```

```zero
int i$ = [1, 2, 3, 4]
```

```typescript
const i_arr: readonly int[] = [1, 2, 3, 4];
```

```zero
int i$ = [1 through 4]
```

```typescript
const i_arr: readonly int[] = Array.from({ length: 4 }, (_, i) => i + 1);
```

```zero
int i$ = [0 to 4]
```

```typescript
const i_arr: readonly int[] = Array.from({ length: 4 }, (_, i) => i);
```

## strings

`string` stays as `string` in TypeScript (it's a builtin).

```zero
string s = "hello"
```

```typescript
const s: string = "hello";
```

## conditional blocks

`if`/`else if`/`else` maps directly.

```zero
on (string s) = describe (int n)
    if (n > 0)
        s = "positive"
    else if (n < 0)
        s = "negative"
    else
        s = "zero"
```

```typescript
function fn_describe__int(n: int): string {
    if (n > 0) {
        const s: string = "positive";
    } else if (n < 0) {
        const s: string = "negative";
    } else {
        const s: string = "zero";
    }
    return s;
}
```

## array mapping

Passing an array to a function maps it using `.map()`.

```zero
int j$ = i$ * 2
```

```typescript
const j_arr: readonly int[] = i_arr.map(x => x * 2);
```

Two arrays: uses `Array.from` with `Math.max` and nullish coalescing.

```zero
int k$ = i$ + j$
```

```typescript
const k_arr: readonly int[] = Array.from(
    { length: Math.max(i_arr.length, j_arr.length) },
    (_, i) => (i_arr[i] ?? 0) + (j_arr[i] ?? 0)
);
```

## array reduction

The `_` symbol reduces using `.reduce()`.

```zero
int sum = i$ + _
```

```typescript
const sum: int = i_arr.reduce((a, b) => a + b);
```

## streaming

The `<-` operator builds arrays incrementally.

```zero
int i$ <- 1 <- (i$ + 1) until (i$ == 4)
```

```typescript
const i_arr: int[] = [1];
while (!(i_arr[i_arr.length - 1] == 4)) {
    i_arr.push(i_arr[i_arr.length - 1] + 1);
}
```

## void functions

Functions without a return type emit `: void`.

```zero
on hello()
    print "hello world"
```

```typescript
function fn_hello(): void {
    print "hello world";
}
```

## tasks

Tasks emit as TypeScript generator functions. Calls spread with `[...]`.

```zero
on (int even$) <- only evens from (int numbers$)
    int n <- numbers$
    if (n % 2 == 0)
        even$ <- n

int even$ <- only evens from (all$)
```

```typescript
function* fn_only_evens_from__int(numbers_arr: int[]): Generator<int> {
    for (const n of numbers_arr) {
        if (n % 2 == 0) {
            yield n;
        }
    }
}

even_arr = [...fn_only_evens_from__int(all_arr)];
```

## concurrently

Concurrent blocks use an async helper. Functions containing `concurrently` are marked `async`, and this propagates to callers.

```zero
concurrently
    hello()
and
    beep()
```

```typescript
async function _concurrently(...fns: (() => any)[]) {
    await Promise.all(fns.map(fn => fn()));
}

await _concurrently(() => hello(), () => beep());
```

## bitwise operators

Bitwise operators pass through directly. Same symbol-to-name mapping as Python.

## TypeScript-specific decisions

- **All numeric types map to `number`** — TypeScript has no int/float distinction. Aliases are kept for readability.
- **`number` is a builtin** — no alias emitted for it.
- **Structs use `interface` + factory function** — readonly fields enforce immutability, factory handles defaults.
- **Nested struct defaults use factory calls** — `args.start ?? vector()` not `?? 0`.
- **Enum values are qualified** — `direction.north` not bare `north`.
- **Async contagion** — functions containing `concurrently` are `async`, callers that invoke async functions also become `async` with `await`.
