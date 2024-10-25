ᕦ(ツ)ᕤ
# scribblez
"slow is smooth, smooth is fast"

end-of-day/week notes:

today we got symbol tables working nicely. that's good!
next: expression parser. it's a two-stage thing.
at some point in the future we really do want to add "types" to the parser mechanics.
but no rewrites of that stuff until we have the whole thing working in zero.

to re-iterate our current goal:

be able to write a client/server website in pure zero, using a typescript backend (deno+browser).

where we are now: semantic analysis of the zero ast; we've got the symbol table;
the next thing is to resolve expressions so we can factor them into variables, constants, and function calls.
prove this out for operations as well.
and then get on with the ts backends.

COME ON
-------

actually before analytics we need to work on error messages from the parser methinks.
also the ast representation. let's do that first.
-------

ok so. next steps.

"analytics" - where we analyse the ast to create variables, types and functions, and convert expressions to actual function calls. => easypeasy.

so let's do it!

what we want is { _feature: xyz } to be extended with { types: ..., vars: ..., functions: ... }
and we want { _function_body: xyz } to be extended with { vars: ... }
and then finally we want something that looks at each expression and parses it to create function calls, var-refs, and so on.
That's the next stage.

So let's do it!

--------------------------------------

we have a lovely, clean, small parser that I understand.

We can tweak the leaf-computation to make it more efficient, but it's pretty damn efficient. Possibly a bit stack-heavy, but we can crunch that. There's exactly three functions in the guts of the actual parser: parse_rule, parse_term, and parse_singular_term. And those functions are small, and easy to understand.

I think the next step is to work on errors, so that error reports are clean and informative. Then we should start work on the actual expression-understander (the thing that deals with functions, variables, and converting expression-strings to actual calls and variables.

Once that's done, we can start to look at generating output code. Goal is to get py, ts and cpp output as the first pass (we'll do them all together).

It would be nice to think of a feature-modular modification to the grammar builder, so that we can implement things feature-modularly, as they're described in the zero.md document.

The objective of this exercise is to have the toolchain written in zero, implemented in either python, typescript, or c++.

------------

documentation format:

a bit like instagram; drag up and down for 1-page documents at "this level";
drag left to go "more into detail" (i.e. layer +1 in this topic). right to go back up a layer.
so at any level, you can scan up and down really quick, and burrow in (left-drag) to whatever looks interesting.

-----


now that we have the right direction for the algorithm, it's about making the code smooth, clean, and controlled; and most importantly, *understood*.

I had a go this morning with the "parent/children" style, but it just leads to complexity. So now the term structure is much simpler: each term is an array of values (which can be keywords, lex-types, or rule-names); the only constraint is that you can't mix them. Each term has a decorator, a separator, and a name. 

We need initiators and terminators; and we'll compute these separately using the memoise approach, so the actual classes remain small, rather than growing huge with methods. The functional approach is by far the easier to fit into my head.

We'll make sure everything in the grammar is tested - that way, when we morph the grammar, we'll be able to make changes super fast.

-----------------

things causing non-smoothness:
1- zero syntax is hard to parse completely
2- so relax and just make expresion = (const or word or bracket)

current weirdness is "abstract rules" should just be eg. expr := operation or variable etc
but not decorated ones. Currently they're confused and they should be separate. General issue with zero-fixed-point rules which means we need terminators, which we should probably figure out using the same as the old method, not try to memoise it.

memoising: not quite the slam dunk because of recurstion and loop-backs, but still interesting. Kind of papered it over by setting the cache before calling the function.

all in all: reasonable, but a rewrite next week will firm up the exact taxonomy of rules and terms.
------

what if a Term is actually a list of strings, eg.

    expression := (constant | ..)

    function_decl := mod:("on" | "after" | "before" | "replace") sig:signature

    feature_decl := "feature" name:<identifier> ?("extends" parent:<identifier>)

In each of these cases, we're adding complexity to a Term.

Why don't we start with the full grammar for zero, and work up from there?




----

precedence works! next is enums.

---------------------------

dealing with precedence: get it done.

    a = b + c

    "+" is stronger than "="    => rank("+") => rank("=")
    
    a = (b + c)

    a + b * c

    "*" is stronger than "+" => 

Does this work for prefix as well?

    - * x

OK: so we have to treat operations specially.

    postfix (a + +) should always left-associate, so it works
    prefix  (+ + a) should always right-associate, so it fails RN
    infix (a + b)   should find the infix-operator in the RHS,
                    and left-associate if the next operator has rank < this one, right-associate otherwise.

    a + b + c - d

    ((a + b) + c) - d

    a = b = c = d       - has to right-associate.


    + (expr)            => parse RHS first
    (expr) +            => parse LHS first
    (expr) + (expr)     => find the outer-op in LHS and RHS, decide which one.

    a + b * c

    => 

    pred (expr)         => so it's R-associative

    (expr) add (expr)   => so it could be L or R.

    We need to actually do stuff like (a = b) is always R-associative.
    a = b = c       => a = (b = (...))

    a + b * c       => a + (b * c)  (R-assoc)
                NOT    (a + b) * c

---------

debug: everything is either short ("a single-word summary") or long ("like, I want to see inside this thing").

ok, so I don't want to do that stuff. I want to do 

    position
    offset
    direction
    length

    position = position + offset
    offset = direction * length

but they are all subtypes of vectors ??

    type vec | vector
        x, y, z : number = 0

    type pos | position < vec

    type off | offset < vec

    type dir | direction < vec

    type length < number

"every position is a vec, but not every vec is a position"

and then we can do

    on (p: pos) = (q: pos) + (o: off)
        p:vec = (q:vec) + (o:vec)


    on (o: off) = (d: dir) * (l: length) ...

but others aren't allowed, so if we do

    pos + dir       => type error: doesn't exist.

The reason that's useful is that when I visualise them, I can show them as absolute positions, offsets, directions, or lengths.

    on direction d = normalise (offset o)
    on direction d = (offset o) / (length l)

This is quite an interesting tack to take, no?

    on (image i) = show (direction d)
    on (scene s) = show details (direction d)

that's quite an interesting approach to take. Note that I'm quite liking lowercase types as well! => is that bad?


-------


operator precedence scribbles:

    a + b * c

By default, this will left-assocate, because that's how parse_remaining works.

    (a + b) * c

What we should do instead is:

    parse "a" => variable a

then - we're parsing an operator, so *special code kicks in*:

    => go into the rhs (all remaining terms) and find the next top-level operator. If you don't find one, or its rank is lower than your current operator, then you left-associate. Otherwise, you parse the whole RHS and insert it into position 3.

        + a -

    a = b = c = d

    a = (b = (c = d))

so an operator can either left-associate or right-associate.
and operators have ranks.

    a << b << c

should be

    (a << b) << c

but it could equally be

    a << 1 + 2


-----

concept: "visualising what a function is doing"

it's sort of connected to the code; but really we want to look at the data structures.

example: we have a function that takes a List[Lex] ls and an index into that list. I like the idea of "showing state" by actually showing the individual lexems as little blobs running left to right, and the index as an arrow pointing at the appropriate item. So much easier to understand.

And when you think of it that way, *that* is a visualisation of how it works, that you can really understand., the code explains what's going on. And we LLM-generate the visualisation from the explanation.

----

another concept: a way of combining the pseudocode and the code in the same place.

-----

Thoughts and prayers:

- continue with this one, it's the most successful so far
- though it probably needs a few improvements
- next steps:
    done - generate AST
    done - write tests
    - operator precedence
    - optional terms
    - enums
    - error reporting
    - auto-generate sub-rules from complex sequences
    - bracket-jump optimisation
    - multiple terms between fixed-points

things that could be improved:
- should we go to LexStr? I think not, for the moment

------

Aside: how to speed up the scan-forward thing.
Do a bracket-scan. For each open-bracket, store the jump amount forward to the closing bracket.
That way, you can step right forward *instantly* without having to scan.
I LIKE IT SO MUCH.

do a bracket-scan. It's a fucking optimisation, but it's a really fucking great one.

It's super easy to figure it out, as well.

OK this is good, but let's do it after we figure out the complete parse.

------

What's next:

    try_parse_term_list(terms, ls)

and then of course we have to figure out what to do with a + b; i.e. we have the single one done, but there's more left, so what next? It will be something like:

    "we have variable, followed by "+", so the next thing to try parsing is infix/prefix/etc".

So that's how we do it! Get on with it!

----

end of day notes:
new approach called the "fixed-point" approach seems to be much more tractable.
as in, I understand it!
The idea is: identify the terms that are "fixed-points", i.e. resolve to single lexemes.
I guess you'd call them "terminals" in normal parser lingo.
Use the idea of the "scan-forward-until-outer-match" from the previous parser,
where you run forward, tracking bracket-level, until you match a target lex (by type/val) at bracket-0 level.
To this, we add the idea of "terminators": lexemes that occur *after* the end of the rule.
We have an initial compute-phase that runs through and figures out initials, fixed-points, and terminators for each rule.

The other idea is to replace the expression = constant | variable | ...
with the idea of inheritance, so we say

    expression := ...
    constant < expression := ...
    number < constant := <number>
    string < constant := <string>
    variable < expression := name:<identifier>

    and so on. So we have proper parent/children relationships between rules, which really helps.

We've simplified the definition of a term as either keyword/type/rule, with optional opt/list/sep "decorators". This means there's only one class of term, and there's no recursion at the level of the Term class. So much simpler than before.


--------------------------

New approach to grammar specification: extensible.

    expression := ...
    constant < expression := ...
    number < constant := <number>
    string < constant := <string>
    variable < expression := <identifier>
    brackets < expression := "(" expr:expression ")"
    operation < expression := ...
    prefix < operation := operator:<operator> expr:expression
    infix < operation := left:expression operator:<operator> right:expression
    postfix < operation := expr:expression operator:<operator>
    function < expression := name:<identifier> "(" parameters:expression*, ")"

This gets rid of multiple Term types; there's now just one. Much simpler.


okay so just think about argument for a second.

    expression = (constant | variable | brackets | operation | function)
    constant = (<number> | <string>)
    variable = <identifier>
    brackets = "(" expr:expression ")"
    operation = (prefix | infix | postfix)
    prefix = operator:<operator> expr:expression
    infix = left:expression operator:<operator> right:expression
    postfix = expr:expression operator:<operator>
    function = name:<identifier> "(" arguments:(argument,)* ")"
    argument = name:(arg_name)? value:expression
    arg_name = <identifier> "="


So there is definitely this idea of the tree:

    expect "expression"

    "f" => {!variable:0, !argument:1, infix:0, function:0, arg_name:0}

    so we open a hypothesis for each of these (a run!)

    expression:0
        !variable:0
        infix:0
        function:0
        !argument:1
        arg_name:1

Now, if we hit it with an "eof", only completely matched items that promote to their parent are allowed. So we need "can x promote to y" map, not a problem.

    expression:0
        !variable:0     => return variable:0

At this point, overshadowing isn't a thing yet, because we've only matched one item. So it's possible that it's a postfix. So overshadowing is premature here, we shouldn't apply it.








need a new strategy to find runs.
stay in your lanes!
but we have the right mechanics now.
tomorrow.
---

refinement of the "fresh every day" principle:
at some point, some aspect of a design becomes "ingrained", i.e. it's the same every time, doesn't improve much.
at this point, abstract it out into a section that doesn't get rewritten.
Eg. in the parser, we're experimenting with a bunch of stuff, but there's a core of display/match/meta-stuff that congeals out.
So grammar-dependent lookups now go into MetaGrammar, which means we can just keep using them and refining them, while trying different parse strategies.
Works for me!


-----

There's a generic problem with accessing "background context" variables.
Eg. the grammar, rule-map, and so on. You either have to pass them into functions, or use them as a "global" state. Neither is the right scope. Feature-scope maybe is right? But even that's not so great. You sort of want to be able to access them and not care where they come from.

Eg. "which class" when booking airline ticket (in fact all the other options)

In the parser, it's "display column width" for all the little display routines, or grammar, or rule-map.
----

OK so let's review where we are.

We have a system in place to collapse the array-of-matches nicely in place.
We have shown that collapsing in the right places reduces things properly.
We've demonstrated the power of good visualisation to improve the understanding of the problem.

And *THIS* ladies and gentlemen is something new and exciting and cool, right?
Visualisation of data structures.
Kind of obvious when you think about it: it's "helping you to see".

I think this is an important "realisation" : write the visualisation of the data structure first, and have the system just use it instead of "print".

VR is perfect for this, right? What does a VR-native development environment look like?

=> this is what we need to build. HURRY UP!

First, we need a way to draw layer X of a Match.
Let's do that! We just need to scoop out Layer X and that's fine!


--------------------------------

The "neighbour-match-check" idea definitely has merit, but I'm too tired RN.
Continue tomorrow.

The algorithm is something like:

1. there's precedence between operators and lexemes.
   eg. keywords override type, eg. "=" overrides <operator>
2. we detect "shadowing", eg. infix shadows postfix and prefix
   if there's ever an overlap between shadowing rules, the bigger one wins.
   incomplete shadowers prevent matching fully-matched shadowed rules
   ; eg. incomplete infix overrides complete prefix, until proof of opposite is received.
    eg: what definite lexemes come after expressions in the rules?
        hint: it's the closing ")" in function and brackets.
3. you run through in precedence order, and pick one winner or none.
4. if there's a winner, you reduce it (combine columns), regen the list, and move on
5. you do multiple passes until you can't reduce further
6. analyse the result to print an error.

So in our example, the first pass would be:

    - arg_name "n="
    - brackets "(6)"
    - infix "a + (6)"
    - argument "n= a + (6)"
    - function "f(n=a+(6))"

We can do something reasonably dastardly by representing the tir-lists as a bit-mask,
with a bit set for each index that this thing matches. That is extremely cunning,
because you can just take the logical "and" of the bitmask with the shifted mask of the next column.

There's a little detail needed to handle optionals and lists, but basically... should be nice.



This is potentially a *very* nice parser, super fast and with some very nice properties.


    expression = (constant | variable | brackets | operation | function)
    constant = (<number> | <string>)
    variable = <identifier>
    brackets = "(" expr:expression ")"
    operation = (prefix | infix | postfix)
    prefix = operator:<operator> expr:expression
    infix = left:expression operator:<operator> right:expression
    postfix = expr:expression operator:<operator>
    function = name:<identifier> "(" arguments:(argument ,)* ")"
    argument = name:(arg_name)? value:expression
    arg_name = <identifier> "="

Bottom-up approach: generate a list of possibles for each one.

    a               +               b
    constant:-
    variable:0
    brackets:1
    operation:-
    prefix:2
    infix:0,2
    postfix:0
    function:0,2
    argument:1
    arg_name:0


But the point is, that list is fixed according to the type of the lexeme, and there's only a finite number of rulenames, types and keywords.
So the square of N is going to be pretty small, so we can store pairs in a lookup.

    f               (               x               )



    f               (               v               =               x               )
    constant:-
    variable:0
    brackets:1
    operation:-
    prefix:2
    infix:0,2
    postfix:0
    function:0,2
    argument:1
    arg_name:0





-------------------------------------------------------

Our strategy moving forward is:

- "ratchet" of tests, proceeding from simplest to most complex

- "compute everything from first principles, live" : move things into pre-compute as it becomes obvious what you need.

=> initially, just lex/rule => [tir]

Let's think about indices as a nice way to do things. Store lex-index in the lexeme.

Tired now, so heading to bed. However:

I quite like the idea of doing "pairs"

    f(      => function
    a+      => postfix/infix with 'variable'
    a=      => argument (name)

    <identifier> <operator> => postfix/infix :2
    <number> <operator> => postfix/infix:2
    <string> operator => postfix/infix:2
    <identifier> "(" => function:2






-------------------------------


Improved test workflow:
logging sends output to a string (s_log)
if a test passes, we chuck away the log
if it fails, we print the current contents of the log and clear it.


------

doing pretty well: a good methodology is to start with the simplest test, then gradually make them more complex. test-first method! 
making progress gradually. 
---------------------------------------------
okay, back to el drawing boardo.

Let's start with expressions:

Terminals:
`<x>` means "lex of type x"
`"x"` means keyword "x"
`|` means "or"
`?()` means "optional"
`*` means "zero or more"
`name:type` means 'bind the result of parsing `type` to variable `name`

    expression = constant
               | variable
               | brackets
               | operation
               | function

    constant = <number> | <string>

    brackets = "(" exp:expression ")"

    operation = lhs:expression <operator> rhs:expression

    function = name:<identifier> "(" params:parameters ")"

    parameters = (parameter ",")*

    parameter = ?(name:<identifier> "=") val:expression

So when we have something like:

    a + b

We need some kind of parser map that maps (type) => list(rule, index)

so for this grammar:

    <number> => (constant, 0)
    "(" => (brackets, 0), (function, 1)
    "," => (parameters, 0)
    "=" => (parameter, 0)

and we also have to do this for the higher-level ones, so

    expression => (brackets, 1), (operation, 0), (operation, 2), (parameter, 1)




-------------------------------
error handling strategy:
instead of returning an error, return an AST with ['_error': Error(x)]
err(AST) => returns ast['_error'] or None;

keyword(X) => either {} or { _error: expected x }
identifier(X) => either [Lex] or { _error: expected lex_type id }
list(X) => { _list: [a, b, c], _error: blah }
optional(X) => either X or { partials, _error: whatever }

let's see how this would work in the case of


    test("param_call", parse(zero.param_call(), "a"))

with the relevant parse rules being:

    def expression(self): return any(self.constant(), self.variable(), self.function_call())

    def function_call(self): return list(any(
            set("word", any(identifier(), self.operator())),
            set("params", list(self.param_call(), ","))))
    
    def param_call(self): return debug(sequence(
            optional(sequence(set("name", identifier()), keyword("="))), 
            set("value", recurse(self.expression))))
    
    def constant(self): return any(match_type("num"), match_type("str"))

    def variable(self): return identifier()

    def operator(self): return match_type("op")

what should happen is:

    optional returns { word: "a", _error: "expected keyword =' } and RESETS reader to start.

-----
general idea: when coding, instead of breaking to doom-scroll, switch to something big that you need to read, eg. the RISC-V ISA design document.
make a reading list of big stuff that you have open that you can flip to.
a good quality of such reading material is that it inspires you to return to writing code.

---
putting in debug tooling !!! turns out to always be necessary!
figuring out a proper strategy for passing errors around.

---

ok: print tests now all work correctly. This nice, because it's ... FRIDAY!
Actually we did Fresh Every Friday yesterday, so today's day 2 of zeta2.
We now have a bidirectional parsing / printing system that runs about 1100 lines of code.
Split into util.py (all the scaffolding for the development workflow) = 375 lines, 640 lines for parser.

Source system is much simpler.
need to make the source map work correctly on write;
and set the output indent type for the writer.

What next? Maybe take the current contents of zeta.py and call them parser.py?
Then zeta.py can focus on the language grammars. Also we should continue with the modular backend/language approach, because that is totally a legitimate concern if we want it to be acceptable.

Simplifications:
- we could make the lexer a lot simpler by knowing the code layout before we start
- Writer is way too large and convoluted RN; should be as small and simple as Reader


-----------


- pedal to the metal getting deno/typescript working to point of noob.org website.
- client will run on avp via webxr, but also work on mobile and laptop form factors.

foray to into native AVP programming / swift.
First impressions: NOT good. Trying to get and set the scroll position of a window - something that should be as simple as calling a function - is a nightmare of what can only be called bureaucracy. Their API is a fucking disaster.
What's worse is that LLM-based coding doesn't work, because LLMs aren't trained on the Apple documentation, or Swift. So they suggest code that just plain doesn't work. Compared to the vscode/llm experience, it's like being bombed back to the stone ages.
This again brings me back to the central hypothesis of zero, which is that simplicity => amenability to LLM coding => better performance with small models at the edge.
Even more important to push forward now.
Also, a cool demo of multi-threaded WebASM out there (particle stuff).

so the correct technical direction is the zero => webXR/webGPU route.
no need to look at native- it's a pile of shite.

--------------------------

This morning:
printing works!
test_printer uses the zero grammar to output the ast created by parsing the text file, and it's correct!
It works for either "{" or ":" or blended indent styles (you have to set it in the Writer)
there's a bunch of little issues to do with spacing, but those are easy to modify later.
and in fact probably won't be the same for every language.

-------------------------

OK so! all parsing tests based on the single test/Hello.zero.md file now pass. Super cool.
Things that are good about this:

- call stack introspection ROCKS
- exception handling / reporting ROCKS
- parser debugger is AMAZING
- structure-based parser is super powerful
- layout-agnostic lexer/parser works a treat
- the test-assert system is FABULOUS
- I figured out lists and errors in partial things, kinda

Things I'm unhappy about:
- there's some weirdness around LexStr vs List[Lex] still (LexStr() does weird shit)
- want to do more work on partial-parsing, eg. body etc

The next step is to get hello world running properly with a typescript/deno backend.
So let's get on with that!

---------------------------

"slow is smooth, smooth is fast" => means you have to know when it's "not smooth".

a bug that makes you scratch your head for multiple hours / days is "not smooth"

when you find a bug that makes you scratch your head like this, it's because the system isn't showing you, clearly, the information you need to understand the problem.

So what we're going to do now is to make debugging parser problems smooth. It's about the workflow, not just the theory.

-------------
Okay so we can now distinguish between:

1- list of correct things terminated by something not in the list
2- list of correct things containing an incorrect thing

In the latter case, we'll return the lowest error; in the former, we'll succeed.
Mechanism: attach the error to the result of "list".
So there's only one list() atom, which is great.

--------------------


I'm super enjoying working in python.
Thinking about rule syntax. Now we have:

    def feature(self) -> Dict:
        return label('feature', sequence(
                    keyword('feature'), set('name', identifier()),
                    optional(sequence(keyword('extends'), set('parent', identifier()))),
                    block(list(any(self.function(), self.struct(), self.variable(), self.test())))))

This would be nicer as:

    feature = 'feature' (name: str) ?('extends' (parent: str)) 
                    (body: block(list(component)))
    
    component = function | struct | variable | test

    function = (modifier: 'or' | 'replace' | 'after' | 'before')
                (result: name_and_type)
    
    name_and_type = ((name: str) ':' (type: str)) | ((type: str) (name: str))
    
And this would write the code for us:

    class feature(entity):
        name: str
        parent?: str
        body: component[]
    
    parse_feature(...) => 
    print_feature(...) => 

Hm ok that's quite an interesting idea. Maybe even simpler.

    class Keyword
        

--------------------------------------------------------------

idea: name for company = "doSomething" or "do_something"

    www.do_something.org

from 'slow horses': "If I'm not back in half an hour, do something"
----

today: messing around with exception handling, faster diagnosis, and so on. Super fun finding a decent workflow examining the python callstack and stuff.

---------------------------------------------
where we are now: got parser structure in place, tidied up the lexer and moved things around a bit.
next: get all parser functions tested and the full parse of the test file working properly.

once that is done, we can start on composition, and then on backends.
Let's do it!

----------------------------------------
Aside on interesting thing to think about:
- if you can interpret as well as compile every backend, then you can do fast-cycle edit/test (just interpret the code you're changing) and then recompile/optimise using the "proper" toolchain when you have time. Best of both worlds, and you can achieve it without exotic toolchains.


----------------------------------------

Thoughts and stuff (been out ;-)

New name/concept: `vert`. **vert** is the name for the graphical engine / user interface we write, in zero, using zeta. It should have a backend for WebXR and for AVP.

So products are:

- zero: the programming language
- zeta: the compiler system
- one: the operating system
- vert: the interface

----

In general, there's a need to be able to display Lex and Parse outputs graphically, rooted in the actual text. By trying to cram as much text as we can onto the screen, we're avoiding the deeper question of how to minimise the amount of information we need to absorb. The power of a good visualisation engine for low-level data is incalculable. If it makes it easier for a human to understand (and all evidence suggests it does) then it should make it easier for a machine to understand, too.

So for instance, there's a general thing of "this region of the text connects to this random object" and we render text with that data structure in mind.

----
Today: got parser structure (parser functions output dict) working, with debug printouts so we can figure out where things go wrong. Feels nice man.

Also: got a really nice (if somewhat unelegant) lexer working now as well.

----

slow is smooth, smooth is fast.

https://gm3.medium.com/slow-is-smooth-smooth-is-fast-1c33b37a5960

-----

The scribblez file contains random musings, free-form, sometimes written in altered states. It is not to be read or taken seriously by anyone.

