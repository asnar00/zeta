ᕦ(ツ)ᕤ
# scribblez

OK so! all parsing tests based on the single test/Hello.zero.md file now pass. Super cool.
Things that are good about this:

1- call stack introspection ROCKS
2- exception handling / reporting ROCKS
3- parser debugger is AMAZING
4- structure-based parser is super powerful
5- layout-agnostic lexer/parser works a treat
6- the test-assert system is FABULOUS

Things I'm unhappy about:
1- there's some weirdness around LexStr vs List[Lex] still (LexStr() does weird shit)

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
    
    class component(entity):
        pass
        

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

