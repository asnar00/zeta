# ziz
*zeta in zero — self-hosting the translator*

status: proposal — bottom-up implementation started

## approach

Translate zeta's functions from Python to zero, one at a time, starting from the simplest leaf functions and working up. This builds the language incrementally — each function we translate either works cleanly or reveals a missing feature that we add to zero before continuing.

## open questions

To be resolved in practice as we encounter them, not upfront:

- Collections: unified `$` with compiler-inferred array vs map — when does inference happen?
- Resumable exceptions: raise/handle separation — handler selection with multiple handlers?
- String primitives: which operations does the parser actually need?
- Multiple dispatch at scale: 15+ AST node subtypes in the emitter — does isinstance scale?
- Recursive types: AST nodes referencing AST nodes — implications for Rust/C targets?
- String building: concatenation vs buffer/stream for emitter output?
- Tokeniser design: how much does it do vs the parser?
- Two-pass parsing: signature collection then call resolution — how in zero?
- Bootstrap testing: verifying zero-zeta matches Python-zeta output

## goal

Write zeta's translator in zero. Bootstrap path: Python zeta compiles zero-zeta to Python, then zero-zeta can compile itself.

## translations

Functions translated so far, in order:

1. `_matching_paren` + `_find_matching_bracket` → `matching (pair) in (s) after (start)` — bracket matching via depth-tracking task + `index of first ... where`

## collections

Zero has one collection concept: `type name$`. The programmer declares *what* they're storing. The compiler infers *how* it's stored from usage patterns:

**Indexed access** → array:

    int values$
    values$ <- 10
    values$ <- 20
    int x = values$[0]

**Keyed access** → map:

    string type_kinds$
    type_kinds$["vector"] = "struct"
    type_kinds$["int32"] = "numeric"
    string kind = type_kinds$[name]

**Access pattern rules:**
- Integer index, slicing, append (`<-`) → array
- String/typed key, keyed assignment → map
- `where`, `sort`, reduce (`+ _`) → works on both
- Mixed indexed + keyed access on the same collection → compile error

**Target mapping:**
- Array: Python `list`, TS `Array`, Rust `Vec`
- Map: Python `dict`, TS `Map`, Rust `HashMap`
- Ordered map (keyed + iterated in order): Python `dict`, TS `Map`, Rust `IndexMap`

The `$` suffix means "collection" — whether sequence or mapping is an implementation detail the programmer doesn't choose.

## is / has

Two words, same mechanism, different intent:

    type string is char$                          # I am this thing
    type character has position, health           # I have these things
    type player is character has inventory         # I am this, I have that

Both merge fields into the type and establish an is-a relationship for dispatch. The compiler treats them identically. The distinction is for the human reading the code.

**Identity** — `is` for when the type fundamentally *is* something:

    type string is char$           # a string is a character sequence
    type dog is animal             # a dog is an animal
    type image is pixel$           # an image is a pixel sequence

Collection operations (indexing, slicing, streaming, `where`, `first of`) work directly because the type *is* a collection.

**Composition** — `has` for when the type *has* capabilities:

    type position
        int x, y = 0

    type health
        int hp = 100
        int max_hp = 100

    type character has position, health
        string name = ""

Functions that take `position` or `health` accept `character`.

**Implementation:** Fields are flattened into the generated struct. `string is char$` generates a type backed by a `_char_arr` property, with indexing/slicing/length routed to it.

**Name collisions** — qualify with the subtype name:

    type position
        int x, y = 0

    type velocity
        int x, y = 0

    type particle has position, velocity

`particle.x` is ambiguous — compiler error. Use `particle.position.x` and `particle.velocity.x`. No collision, no qualification needed — `character.x` just works when only one parent has `x`.

Because `string` *is* a `char$`, all collection operations work on strings directly — indexing, slicing, `where`, `first of`, streaming into tasks. No conversion needed.

This means parser functions that scan characters just consume `char$`:

    on (int position$) <- paren matches in (char c$) with depth (int d)
        char c <- c$
        d = (d + 1) if (c == "(") else ((d - 1) if (c == ")") else (d))
        if (d == 0)
            position$ <- i

And you call them with a string directly, because a string is a `char$`:

    string s = "(hello (world))"
    int pos = first of [paren matches in (s) with depth (0)]

The task doesn't know it's receiving a string — it only sees characters. The tokeniser consumes `char$` (a string). The parser consumes `token$` (an array). Same pattern, different element types. Tasks all the way down.

## string primitives needed

The parser is fundamentally string processing. Zero needs these string operations (as platform functions or builtins) before self-hosting is practical:

    on (string c) = char at (int index) of (string s)
    on (int n) = length of (string s)
    on (string sub) = substring of (string s) from (int start) to (int end)
    on (bool b) = (string s) starts with (string prefix)
    on (bool b) = (string s) ends with (string suffix)
    on (int i) = index of (string needle) in (string haystack)
    on (string trimmed) = trim (string s)

These exist natively in every target — they're platform functions, not language features.

## IR as zero structs

    type ast_node
        string kind = ""

    type literal_node = ast_node +
        string value = ""

    type binop_node = ast_node +
        string op = ""
        ast_node left
        ast_node right

    type assign_node = ast_node +
        string target = ""
        ast_node value

    type call_node = ast_node +
        string name = ""
        ast_node args$

    type fn_node = ast_node +
        string name = ""
        string result_name = ""
        string result_type = ""
        string param_names$
        string param_types$
        ast_node body$

    type type_node
        string kind = ""
        string name = ""

    type numeric_type = type_node +
        string base = ""
        int size = 0

    type enum_type = type_node +
        string values$

    type struct_field
        string name = ""
        string type = ""
        string default = ""

    type struct_type = type_node +
        struct_field fields$
        string parents$

    type ir
        int version = 1
        type_node types$
        fn_node functions$
        ast_node variables$
        string features$

## modules

**tokeniser** — splits source into tokens, handles markdown extraction

    on (string tokens$) <- tokenise (string source$)

**parser** — recursive descent, consumes tokens, produces IR

    on (ir result) = parse (string tokens$)

Type parsing, function parsing, expression parsing as separate functions. Two-pass: first pass collects signatures, second pass resolves calls in bodies.

**emitter** — walks IR, produces target code. Multiple dispatch on node kind:

    on (string code) = emit (literal_node n)
        code = n.value

    on (string code) = emit (binop_node n)
        string left = emit (n.left)
        string right = emit (n.right)
        code = left + " " + n.op + " " + right

    on (string code) = emit (assign_node n)
        string value = emit (n.value)
        code = n.target + " = " + value

One set of emit functions per target, selected at compile time.

## resumable exceptions

status: rough sketch / proposal

Functions can `raise` to request a value they can't compute. Handlers are declared *separately* from the function, and provide the value back to the raise site. The function resumes as if nothing happened.

**Raising:**

    on (int size) = get preference (string key)
        size = lookup preferences (key)

    on (string value) = lookup preferences (string key)
        ...
        raise missing_key(key)

The function doesn't know how the error is handled. It just says "I need this and can't get it."

**Handling:**

    in lookup preferences on missing_key (string key)
        int value = ask user for (key)
        store preference (key) (value)
        return value

The handler fulfils the request. The `return value` goes back to the exact raise site — `lookup preferences` continues with that value as if it computed it normally. The caller (`get preference`) never sees the interruption.

**Key properties:**
- Functions stay pure — they declare *what* can go wrong, not *how* to recover
- Handlers are separate declarations — swappable without modifying the function
- The raise is a pause, not an exit — the handler returns a value to the raise point
- Callers are oblivious — `y = f(x)` always produces `y`, even if handlers fired inside `f`

**Use in the self-hosted parser:**

    on (ast_node result) = parse expression (string tokens$) (int pos)
        ...
        raise unexpected_token(tokens$[pos], pos)

    # recovery mode: substitute error node, continue
    in parse expression on unexpected_token (string token) (int position)
        return error_node("unexpected: " + token, position)

    # strict mode: just abort (different handler, same parser)
    in parse expression on unexpected_token (string token) (int position)
        raise fatal("parse failed at " + token)

Same parser code, different policies. Testing attaches a handler that collects errors; production attaches one that stops.

**Emitter mapping (tentative):**
- Handler becomes a callback, threaded as a hidden parameter by the emitter
- `raise` becomes a call to that callback
- If no handler is attached, raise is a runtime error (Python: exception, TS: throw)
- The caller never sees the callback — it's compiler-inserted plumbing

This is in the spirit of algebraic effects / resumable exceptions. Details to be worked through in practice.

## open questions

- **Collection type inference:** When does the compiler decide array vs map? During parsing (first use) or as a separate pass? A separate pass is cleaner.

- **ast_node as base type for dispatch:** Multiple dispatch on `emit(ast_node)` needs the dispatcher to check concrete subtypes. This works today with isinstance/instanceof — does it scale to 15+ node types?

- **Recursive types:** `binop_node` contains `ast_node` fields which could be any subtype. This works with Python/TS reference semantics but needs thought for Rust (Box<dyn AstNode>).

- **String building:** The emitter builds output strings by concatenation. Is that sufficient, or do we need a string buffer / stream pattern?

- **Handler selection:** If multiple handlers are declared for the same raise in different features, which one wins? Feature composition order? Most-specific-first?

- **Bootstrap testing:** How do we verify zero-zeta produces the same output as Python-zeta? Round-trip: compile the same .zero.md with both, diff the output.
