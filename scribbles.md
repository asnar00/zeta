ᕦ(ツ)ᕤ
# scribblez
"slow is smooth, smooth is fast"

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

-----------------------------


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

