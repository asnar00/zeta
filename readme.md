ᕦ(ツ)ᕤ
# zeta

`zeta` (**ze**ro **t**o **a**nything) is a compiler that reads in `zero` programs (expressed in *feature normal form*) and generates code in any language and for any backend. 

## zero

`zero` is an experimental programming language built around the concept of *feature modularity*. For a full explanation, look at the test [hello world](./src/test/Hello.zero.md) example.

## feature normal form

*Feature-normal form* (`fnf`) represents code as a tree of markdown files, each one defining a feature. Sub-features are stored in a folder of the same name, ordered by creation date. Markdown files look a lot like blog posts - they contain formatted text designed for human readers, with code snippets inline.

This format collects code alongside 'meta-code' - specs, tutorials, documentation, tests, musings, prompts, and so on. The `zeta` compiler scrapes the code out of the markdown files and rebuilds it if it has changed.

For an example of fnf, see the test [hello world](./src/test/Hello.zero.md) example.
