# zeta_old notes
*ideas for improving zeta based on the previous implementation*

## 1. grammar-driven parsing

zeta_old uses a **declarative grammar system** — rules are defined in EBNF-like notation, and the parser is generated from them. The current zeta parser is regex-based and (as `ideas.md` notes) fragile and order-dependent.

**What to take:** Define zero's grammar as data, not as a tangle of regex matches. A declarative grammar makes adding features safer — you change a rule rather than inserting another special case. This is especially relevant for self-hosting: a grammar definition in zero would be much cleaner to bootstrap from.

**What to leave:** zeta_old's grammar system is heavily engineered (initials/followers computation, complexity sorting, leaf reduction). A simpler recursive-descent parser driven by a grammar spec would get most of the benefit without the weight.

## 2. VM / IR abstraction layer

zeta_old introduces a **virtual machine IR** (`VmVar`, `VmConst`, `VmInstruction`, `VmBlock`) between parsing and code generation. The current zeta emits Python/TypeScript directly from the AST-level IR.

**What to take:** A lower-level IR would make adding new emitters much easier — the `ideas.md` Rust, C, WebAssembly, and assembly targets all want something closer to three-address code than to an AST. A thin VM layer could also unify the ~60% duplicated logic between `emit_python.py` and `emit_typescript.py`.

**What to leave:** The full VM + register manager + ISA hierarchy is overkill until hardware targets are actually in play. A simpler linearised IR (basic blocks of assignments and branches) would be a better next step.

## 3. compilation pipeline as staged modules

zeta_old decomposes compilation into explicit stages, each implemented as a pluggable `LanguageModule`:

```
parse → constructors → add symbols → resolve symbols → embracket → check types → generate
```

The current zeta is essentially two big stages: parse, then emit.

**What to take:** Introduce at least one intermediate stage — **symbol resolution / type checking** — between parsing and emission. This would catch errors earlier, simplify the emitters (they wouldn't need to guess types), and lay groundwork for self-hosting where the type system matters more.

## 4. bracket jump computation in the lexer

zeta_old pre-computes a "jump" value for every opening bracket, storing the distance to its matching close bracket. This gives the parser O(1) lookahead over nested structures.

**What to take:** If the parser moves to a token-based approach, this is a cheap trick that makes parsing significantly faster and simpler — especially for zero's `(vector a) + (vector b)` style signatures where you need to skip over parenthesised groups efficiently.

## 5. dynamic entity class generation from grammar

zeta_old auto-generates AST node classes (`zero_classes.py`) directly from the grammar rules. Grammar changes automatically propagate to the AST — no manual class updates.

**What to take:** The current zeta uses plain dicts for the IR. If moving to a proper AST, generating node classes (or at least dataclass definitions) from the grammar avoids the error-prone manual synchronisation problem. Even staying with dicts, a schema derived from the grammar could validate IR structure.

## 6. visitor pattern with scope and reference awareness

zeta_old's `Visitor` class traverses the AST with:
- method filtering (only visit nodes that have a specific method)
- reference vs value distinction (avoids following circular refs)
- child-first or parent-first ordering
- scope tracking during traversal

**What to take:** The current emitters do their own ad-hoc tree walking. A shared visitor (even a simple one) would reduce duplication between the Python and TypeScript emitters and make adding new emitters cheaper.

## 7. error recovery and source-mapped reporting

zeta_old's parser doesn't stop at the first error — it truncates bad list items, drops failed optionals, and continues. Errors carry source locations back to the original `.md` file. The report system highlights the problematic tokens visually.

**What to take:** The current parser raises on first error with no source location. Even minimal error recovery (skip to next declaration and continue) plus line numbers from the markdown extraction step would dramatically improve the developer experience — especially as zero programs get larger.

## 8. structured logging with depth limiting

zeta_old wraps compilation functions with `@log_indent`, producing hierarchically indented logs that cap at a configurable depth.

**What to take:** A lightweight version of this (`--verbose` flag on `zeta.py` that logs parse/emit stages) would help debug the translator without needing to sprinkle print statements. Useful now, essential for self-hosting.

## 9. ISA abstraction (future reference)

zeta_old defines an `ISA` base class with a RISC-V 32-bit implementation, plus QEMU integration for simulating generated machine code.

**What to take:** Not immediately useful, but the abstraction boundary is well-chosen — `pointer_type`, `load_immediate`, `encode_instruction` as the minimal interface. Worth keeping in mind for the assembly emitter idea in `ideas.md`. The "machine code learning via probes" idea could plug into this same interface.

## 10. feature extension via decorator pattern

zeta_old extends entity classes dynamically using `@Entity.method(cls)` decorators — each `LanguageModule` adds methods to AST nodes without modifying the class definition.

**What to take:** This decouples concerns cleanly — the parser module adds `parse` methods, the type checker adds `check_type` methods, etc. If the emitters were restructured as modules that attach `emit_python` / `emit_typescript` methods to shared AST nodes, the duplication problem largely goes away.

## summary: recommended priorities

1. **Error reporting with source locations** — low effort, high impact
2. **Shared visitor for emitters** — reduces the 60% duplication
3. **Grammar-driven parser** — aligns with self-hosting goal
4. **Intermediate type-resolution stage** — simplifies emitters, catches bugs earlier
5. **Structured logging** — small addition, big debugging payoff
6. **VM IR layer** — when a third emitter (Rust?) is added, not before
