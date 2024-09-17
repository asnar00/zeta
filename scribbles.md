ᕦ(ツ)ᕤ
# scribblez
"slow is smooth, smooth is fast"

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

