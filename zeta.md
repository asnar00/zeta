# zeta
*translating zero programs to any language*

`zeta` is a system for translating zero programs into any target language / platform. It uses a coding agent (the lovely Claude Code) to process a set of examples and write a *translator* (in any deterministic language).

input: language definition, a bunch of translation examples
output: a program that translates zero to the appropriate language

    zero.md         : the language definition
    zeta.py         : the actual translator
    zero-to-py.md   : zero -> python examples
    zero-to-ts.md   : zero -> typescript examples
    
