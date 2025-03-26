ᕦ(ツ)ᕤ
# scribblez
"slow is smooth, smooth is fast"

how to get variables in.

    on uart(c)
        write(c, 0x100000)

    the "c" should resolve to

        out: [whatever variable holds c]

but to do this, we have to know what the arg-variables are, otherwise it doesn't work.
so there has to be something which maps that program-level variable to the vmvar.

So in fact, that needs to be a property of the VmVar object; it needs to know which variable it's holding,
and we need to have access to all VmVars in the current scope.

So we need to pass down something that maps Variable => VmVar somehow.
uart(c) => I'm passing in "c=>var0"

So there's a context that we pass in; that's the replacement dictionary.
And we have to do that.


------------

conversion:

    on (char out$) << (int x)
        ...

the point is, how do we find the function? Our signature's typed-handle doesn't give us anything.
so when we come to actually do the function, what is going to happen?



----------


"let's completely rewrite vm.py" - wheres and whyfores

1- vm was written purely to handle a single basic block so is confused in many ways
2- touching the 'generate' stuff shows that it was totally confused and needs improvement

So here's the new architecture:

    class VmBlock:
        def __init__(self)
            self.params: List[Var|Const] = []
            self.instructions: List[VmInstruction] = []
            self.results: List[Var|Const] = []

A VmBlock is a list of instructions, and if you run() it from the start, it will always end up at the end, with a list of result variables or constants you can read from it. Internally, there can be jumps (loops and if/thens) but execution always ends up with the PC at the end.

We can combine blocks together to make bigger blocks. For example, we can implement if/then/else using this function:

    def if_then_else(cond: VmBlock, then: VmBlock, else: VmBlock):
        return VmBlock([...])

    def for_loop(var: VmVar, init: VmBlock, inc: VmBlock, test: VmBlock):
        return VmBlock([...])

Finally, the generate() method is implemented as a standard pass (children first); each node's generate computes a ._vm property by combining the completed ._vms of each of its child nodes.

-----------------------------

ok: 

    out$ << "ᕦ(ツ)ᕤ"

so let's think about how to do this.

1) we don't want to just punch it down to VM level, that's not right.
   so let's find the code that does that, and do something else.

having done that, we now have this thing called StreamAssignment.
How do we print it out?

OK: so firstly we have to create the array constant for the string.
If there's no "rate" then we have a simple thing.

Thoughts: x.generate() is returning the symbol set for its results;
but actually, we should find a different way to do this.
x.generate() should return a VMEntity.
and the VMEntity should have a method results() which returns a list of names.

--------------------------------
general idea for the lab: 

- play with toys
- make better ones

"playing is learning"
"learning is building"

"LEARNING" is a cool way to proceed, because you can ask anyone to come and teach you. It's the nicest relationship - you're inviting them here, they're staying in this space, and we do a lesson in the daytime, rest of the time is theirs.

I really like this idea, because I can invite anyone I like.

---------------------------------

status: 
- fixed type-checker to check aliases
- now, out$ << "ᕦ(ツ)ᕤ" is getting to VM level
- but we need to modify the VM to handle this assign somehow
- hmmm.. a VM that deals with arrays as first-class citizens? Why not?

=> that's kind of fucking awesome, right?
----------------------------------
below the line: random thoughts on an AR/AI interface, based on walking around the studio with the headset on.

- learn and recognise each object, collect a 3D mesh
   - label everything
   - ask "what is that?" if you don't know
   - build a 3D mesh of the object
   - whenever you recognise it, note its position/orientation and state (eg. book open)
   - capture lighting context and so on
   - be able to re-render any configuration of objects realistically
   - room rewinder: just rewind to previous scenes (object/pos pairs)

- add instructional layers to each object:
    - teach me how to use X
    - understand what state X is in relative to a task
    - figure out how to improve X

So for example, with the guitar or keyboard, it knows what it is and it can teach you how to use it.
With any "appliance", it can show you how to use it, or answer questions about it.
Eg. "how to make a good espresso".

Scenes "contained" within other scenes; for instance, there's a "scene" based on sitting on the sofa (whole room / coffee table), one for the electronics bench, one for the music area, one for the coffee machine area, but those are all "children" of the main scene. When you're in the parent scene, the child scenes are "semi-opaque", i.e. they appear as a blob or box, without much detail (just the status information you need, if any).

So if I'm sitting on the sofa, I see the desk as a box; 

----------------------------------

Contexts.
The conundrum is: add_symbols stage works with all features and contexts without checking whether a feature is enabled or not.
Ideally, we would only parse contexts first, rather than diving down into them; that way we can save a bunch of time parsing things we're not using, AND IN FACT THIS IS ESSENTIAL, otherwise we're going to waste a ton of time.
So we actually need to be going into this with a list of allowed feature names, and just not traversing a feature if it's not in the allowed list. That's pretty easy, actually.
But I think that also means that Contexts aren't normal program objects; they're actually build system structures.
So let's treat them that way.

So let's think about this: how would we do it?
=> we need the ability to skip a feature based entirely on its name, at the parsing stage.
then the AST will reflect only the context.
So let's look at how to do this.

First, remove ContextDef from the parsing stuff.


-------------------------

TOMORROW: make context feature-switch-on-and-off work properly again
then make hello world work

Where we are today:
1- disassembly verification is done for R+A
2- elf file generation is done for R+A
3- next: run quemu

or is that in fact the right thing to do? Well, yeah, why wouldn't it be? Just write a single result to memory, which we have to do *anyway* if we're going to see it. So I think it's like :INPUTS and :OUTPUTS => just areas in memory where you're reading and writing from
------


tomorrow: encode ARM instructions
then: make elf, disassemble-check, then run-qemu.

Policy decisions: develop both architectures simultaneously. Most important.


then: 
 - code should send the logo to prove it's working
 - then:
    code should send the final float (in binary) to the "output" channel;
    we should be able to send it the two vectors (1,2,3) and (4,5,6)
    we can also do debug-dump as final act (write the contents of memory to out)

That's nicer, because then we don't need this QEMU thing, which anyway doesn't work for hardware.

bed now.




wrote up esc and one a bit, good thoughts in that direction.
esc: https://docs.google.com/document/d/1zsPmPBISJ6LbzjUkUg1JLNgLBtr3oJMcif0AABhMio4/edit?tab=t.0
one: https://docs.google.com/document/d/1pQRexcch66GReu1-h-T0kN0oJqbIELRscbp7aKazdnI/edit?tab=t.0

Lots of fun!

---------------------------------------

Thoughts on the "build without dependencies" rationale.

One has to be pragmatic about this; you always have the choice to "use existing product X" or "make a new product P".

The great fallacy is that "use existing product X" is going to be quicker and cheaper then writing it yourself. This isn't always going to be true.

If a product is elegant, simple, discoverable, flexible and customisable, with little effort, then it's obviously better to use. But lots of products are kludgey, super complete, poorly documented, designed around one task, and impossible to customise because of complex code or broken build systems. It may be that for your limited use case, it's faster to roll your own small, hyper-focused tool (a "microtool") that lets you get the job done.

Once you have a microtool that works for you, the improvements to your workflow efficiency and adaptibilty should more than justify the cost of building it. Now, with AI democratising coding, the factors affecting that decision are changing fast.

Examples from the zeta project:
I looked outputting to MLIR, but MLIR is HUGE. There's just such a huge amount of it, and there's so much stuff to wade through. I decided to go for a small, homegrown multi-CPU-target-supporting compiler. And it was definitely the right decision.

I started by outputting assembler and using clang to assemble and link; but then got rid of the clang dependency. Instruction encoding for the subset I'm using is super easy; 30 lines for RISCV on the first attempt. (yes, that will obviously grow, but I was up and running with it in no time).

With QEMU, I'm going ahead with using it, even though it's impossible to customise (I have to fix the build script to make the build work, which is amazing to me). I could spend a bunch of time diving into the subtleties of build tools and so on, but that's a major learning effort dedicated to figuring out legacy software. For the moment, I'm ploughing ahead with QEMU, but it's a verification tool rather than in the pipeline; I don't need QEMU to deploy; so it's not such a big deal. 

Same is true of using the llvm disassembler for verification. That's fine, because I'm using it as a tool to verify the compiler. So that's OK. 

That seems like a decent rationale: if a tool is directly involved in the zero pipeline, it should be written by us (in python, then zero). Once we have correct zero code, we should be able to patch any running system, as well as examine it to understand what it does and how.

I think once we can deliver an experience of programming using literate coding, on a remote device, that should tell us whether we have something. 


------------

thoughts on esc, the user interface
rationale: well, we need VR to visualise and control the drone swarm, obviously
and obviously that is true, right? Look out into the world and see where your drones are.
what's the cheapest open-source VR headset we can find?


---------------

company name: "harmless"
harmless industries
harmless technology

from the hippocratic oath: "first, do no harm"

harmless.org seems blank but is owned 
collides with harmless.org.uk - self-harm prevention charity in the uk

but aside from that I like it. Logo still works, kind of...
--------
new architecture is much nicer!
all ISA stuff in isa.py;
vm stuff is now called vm stuff, and is in vm.py (previously codegen)
backend is in its own file, works with any ISA.

possible next steps:
1- debug output DONE
2- code generation DONE FOR RISCV
3- disassembly check
4- elf generator
-----------------------------

Characteristics of a good blogging platform:
1- small posts: "snippets"
2- snippet => expand to a strand of snippets
3- front page => latest snippet
4- "catch-up" => to get to snippet x, just follow back up to the root
5- add snippets to any "endpoint" (track them)

somehow make this fit-into-your-headable


---------------------------------------

Now: we have a verified binary risc-v output.

Next:
1. add data section
2. build an ELF file
3. run it as baremetal on QEMU following https://popovicu.com/posts/bare-metal-programming-risc-v/

Get on with it. You just need to sit down and begin.

------------------------------------------

higher level saturday night kind of thoughts.

1) a new name popped up, superexcited.org (image on t shirt)
2) a sort of a thought about intuituive ways of visualising multi-dimensional "streams".

A stream is a function (t => state), and it's continuous.
When you look at a stream "all together", it looks like a graph with t on the x-axis, and the value on the y-axis *projected somehow". So a 0-d value is a "point"; a 1-d value is a series of 0-d values making a "curve"; a 2-d value is the "landscape" that gets made when that curve undulates to make a surface; and then the 3-d value is "how that landscape changes over time"; or a 'narrative'. 

But here's the thing; what if we add a fourth dimension, let's call it 'q'. If each 'frame' of q is a narrative, then 'q' is best described by "how the narrative evolved over multiple retellings" or something like that. It's a sequence of narratives. OR something or something or nothing yeah but no but,

BUT: this notion of the 4th dimension as a "narrative dimension" (how a narrative evolves over time) seems like actually the correct way to think about it.

Or wrong, or banal, or whatever.
    
-----------------------------

OK: path we're on now is just write RiscVBackend as a class, all code in there and RegisterManager.
Goal is to get the distance sample running in qemu asap.
THEN: implement stream stuff (language level and output) so we can compile hello world.
Get on with it :-/

------------------------------

ok so I think I've figured out streams for this implementation, with my new invisible-for construct.
I know the road ahead. But let's get this executing in QEMU.
Order should be :
- get distance sample running on qemu and dumping correctly
- implement array stuff sufficient for hello world
- get hello world running on qemu
----------------------

ok, time to think a few late-night-clubbamental thoughts about streams and tings.

See, for hello world, we're going to need an in/out terminal. So we have to define the program as something that takes in keystrokes (in-stream) and outputs text (out-stream).

And that is fine, right? There is in fact an in() and out() stream.

Let's get this right.

First:

no arrays, just streams, either timed or untimed (default). We may not use the stream features of an array, in which case they should be zero cost; but if we do use them, they "just work".

    int i$ << 0 << (i$ + 1) .. (i$ == 10)
    int i$ << [0 .. 10]

Input and output text streams are done through out and in, defined:

    feature Console
        string out$
        string in$
        on out$ << string s
            out$ << s[0 .. len(s)]              # output one char at a time
        on out$ << char c
            output(c)                           # call the system output-char routine

    feature String
        type char | character > u16, u8         # 'char' can be u8 or u16
        type str | string = char$               # 'string' is a stream of characters

    feature Backend
        replace output(char c) emit             # and now that's an instruction so we can generate

The `s[0 .. len(s)]` construction is interesting because

    [0 .. < len(s)]   is the array of numbers [0, 1, 2, ..., len(s)-1 ]
    s[0 .. < len(s)] is the [] operator mapped to this array, so [s[0], s[1], ... s[n-1]]

We can also use 1..n like this s[1 .. <= n]

So this makes perfect sense, I think.

So the new syntax we need to get this working is:

- multiple invocation of << in a stream expression
- array notation, list or range: [a, b, ... ] or [a to b] formats

I think we then need do if-then-else and (dare I say it) some kind of invisible for-loop.
Invisible in that it's a private formulation that helps us generate code, but isn't actually available.

It would be something super simple like:

    for var <= start, finish, step
        some code

I think that would be fine basically.

I think this also works super nicely for bit streams as well:

    bit$ << 0[5] << [1 0 1 0 0 1] << binary(10)

    on bit$ << (i32 v):
        bit$ << v[0 to 31 inclusive]

OK and this is super fucking fantastic. Because the same things apply to this as to other streams.
It's very, very nice, basically.

So this comes down to the idea of representing the integer types as bit-arrays, which I think is super nice.
I think it also means that we can do interesting things with for instance constructing a 32-bit uint instruction from binary fields, it's a very very nice notation for when we get there.

=> ok come on!

------------------------------------------------------









---------------------------------------------------------

word of the day : "redo"
this idea that you can handle momentum loss by throwing something away, is quite cool.
based on this I should really now throw away the ARM code and just do RISC-V.
I'm not in love with the way the architecture came out. So I'm going to throw it away again.

It's unnecessarily complicated. Let's break this inheritance structure and just make small components.
eg. RegisterSet => nice
and DataSet
and InstructionSet => 

Fundamentally, use traits instead of inheritance; I think that's just a better idea.
So make components that each backend can use, but don't arrange backend classes into a complex tree.
Just too hard to follow code that way - what's been overridden, etc etc. Just too fiddly.

Try following the rule of small, simple units.


-----------------

super aside: what if you had 

    opcode: src1 src2

where src1 / 2 are relative offsets back to the instruction that generated the operand.

    6-bit opcode; 5-bit operands. => 16 bits per instruction.

If the top bit is set, then you load the next 15 bits into a shift register.


-----------

Next: arm and risc-v output for distance sample.
- correct the feature-always-on issue => two contexts
- get distance example working
- then into hello world and stream fun land.
- once working, compile into a kernel
- then get kernel running on qemu

------------
Thoughts on system organisation:
=> each system holds certain code and data
=> code and data can flow across the network
=> but the network can fail, and is slow

so we want to carefully understand the speed and latency of each connection, and therefore what kind of response times it can support
so measuring talking about frequency and latency *as part of the language* is very important.

A network pipe will have (f, l) => and so that limits the amount of data we can send over it.

Say we have machines A and B holding data a and b.  (a and b are data streams generated locally on their machine)
We want to run f(a, b) continuously. We can do this either by sending b to machine A and running f(a, b) on A; or the converse, sending a to machine B and running f(a, b) on machine B.

We can figure out which where to place this task by looking at the rate requirements of each option, compared with the network channel speed; and taking into account the performance of A and B.

For instance, if A is a high-performance machine and a is a video stream, and B is a slow machine and b is audio, and the connection speed is super slow, then we would clearly prefer to send b to A. If on the other hand, B is vastly more powerful than a, and the network is fast enough, then we might prefer to send a from A to B.

We could also add "reliability" as a measure of network quality; so a network channel is characterised by (f, l, r).

But the point here is that we want to be able to write

    f(a, b)

and *not care* about underlying details. Could all be on a single machine, or could be on a cluster, we don't care.
note that this "location transparency" (we need a better term) is recursive in scope, so a and b could be complex data structures whose various parts are themselves distributed across multiple machines.

This abstraction is the key to zero being super cool.

-------------
TODAY'S JOB: rewrite with this in mind.
backend architecture thought: (bleary):
- this attempt uses a generic Instruction format which definitely feels simpler; we just modify instructions in-place. Maybe add a tag.

That's good, but I'm thinking actually there's an issue with where RegisterManagement happens.
I think maybe it should be:

    Processor
        RegisterProcessor (risc-v, arm, etc)
        StackProcessor (webasm etc)

This would seem to push the management code into RegisterProcessor, which makes a bunch more sense.
----------------

Architecture of distributed applications:
1- each node that spins up runs a certain set of Contexts (all in parallel); each has a bunch of state and a set of enabled features.
A Context plus a set of Tasks is basically a Process, right? Let's call that a Node.
The idea is that a Node has a bunch of state (static and dynamic coming from sensors) and capabilities, and we can query the node to understand what functions we can call and what state we can access.
So we can then command the node to run function f on data d. voila.
So when any bit of code on any node calls function f on d, its request will automatically route to the node that can run it.
YES. That's it.


--------------

Interesting thing to think about re. zero streams:

in python, there's just "str" type, no separate char type. If you take str[i] you get a str with length 1, not a char.
Now what if this convention was general for all streams of things.
So consider how we define the string type (which is the simplest stream, right?)

    in is a string source
    out is a string sink

    out << "hello world"

So that would be cool.

--------------

quick thought resurfaced:
take the example of "locations" (the MemoryLocation class).
I want to find a location based on its name; and I want to find all locations that are at the current memory address I'm printing.
so right now I have:

    Location = ( name, offset, size )
    self.locations : Dict(str, Location)                    name => Location whose name matches
    self.offset_to_location : Dict(int, Location)           offset => Location whose offset matches

OK, so it's obviously completely daft to store a pair of hash tables, right? If there's (say) 10 names, then there can be 10 fixed strings in memory, and the key value then is just an index into that array. If you really want to *type in a string* then you'd need a hash table, but most of the time you don't do that - you're just passing the index/ptr around.

Two related ideas here:

    - finding things by filtering an array
    - storing pointers to things as (array/index) pairs

Finding things:

    type Location = { name: string, offset: index; size: bytes }
    location$ : Location

    match$ = all location$ (.name == name)
    match$ = all location$ (.offset == offset)

Now under the hood this can do all kinds of things (like a pair of hash tables, or something a bit more efficient). But the point is that *we don't care* what the compiler does; we just trust the compiler to do the best it can.

----

you need a keyword in python called "override" instead of pass, which means "throw not implemented if you execute this"
actually you can do this easy using a decorator, hmm

    @override def blah(params) pass

OR

    def blah(params) override_me() 

    def override_me(): raise Exception("calling fn that should have been overridden: " + caller())

That's a reasonable thing to do.
--------------

Steps needed to get to "hello world" in the language:

string literals need to go in memory -> same process as numbers
bit of pre-processing to replace the string literals with <<
I kind of want to think a little bit about the precise correct syntax.
simple model is that out$ is a char out$ => so it's just a string.

I quite like the idea of "hidden" structures that don't have language equivalents;
eg. we could do class ForLoop; and generate those exclusively from within the compiler.
We almost kind of have already with Seq and Par classes.

Get the OS working just with standard character in and out, that's canonical.
Method should be: do the C/risc-v course, in C, but also follow along with the language.
It provides a very nice set of graded goals to go through.

We just need to implement

    a << b

which does a concatenate. I wonder whether we can analyse this statically?? We should definitely look into compile time stuff at some point.

Whenever we declare a stream, we're also going to give it a fixed area - it's a circular buffer over a much longer stream.

If we want to see a stream on a different machine, writing to the stream on machine A will send it to the equivalent object on machine B.

So next task is risc-v output. We're SO damn close.

----------

OK so quick recap of last bit of gnurg.
What's been achieved:

1- building a verification step into the compiler to check our binary against that produced by assemble/dissassemble.
2- wasting a whole day fucking about with qemu trying to get it to run my binary. Long story short, chatgpt/claude know nothing but pretend they do. About either ARM instructions or how to set up QEMU. 4o is basically 3.5-level dumb, which I hadn't quite twigged. o2-mini slightly better but slow and rate-limited. Sore need for a "read the manual and help me do X" application.
3- Anyway ARM output now works, but in the process discovered ARM ISA is nightmarishly complex and large. Crazy crazy. SO many cases and edge combos and weird stuff.
4- Started the "OS in 1000 lines" book, following along, much more joy. Have followed them up to "Hello World" using C - the next task is to output RISC-V code from zeta, use the llvm assembler for the last mile, and get to "Hello world".

Great to finally be getting to grips with this level of stuff.

Next step: RISC-V assembler output. "Hello world" on QEMU.
------


rearranging instructions to improve efficiency.
This is "find distance between a and b" which is basically length(a - b), length(v) = v dot v, a dot b = blah

    mov('a_0.x', '1')
    mov('a_0.y', '2')
    mov('a_0.z', '3')
    mov('b_1.x', '4')
    mov('b_1.y', '5')
    mov('b_1.z', '6')
    sub('n_4', 'a_0.x', 'b_1.x')
    sub('n_5', 'a_0.y', 'b_1.y')
    sub('n_6', 'a_0.z', 'b_1.z')
    mov('v_3.x', 'n_4')
    mov('v_3.y', 'n_5')
    mov('v_3.z', 'n_6')
    mul('n_8', 'v_3.x', 'v_3.x')
    mul('n_10', 'v_3.y', 'v_3.y')
    mul('n_11', 'v_3.z', 'v_3.z')
    add('n_9', 'n_10', 'n_11')
    add('n_7', 'n_8', 'n_9')
    sqrt('d_2', 'n_7')

Now we could do this more compactly if we reordered it:

    sub(x, a.x, b.x)
    mul(x, x, x)
    sub(y, a.y, b.y)
    mul(y, y, y)
    add(x, x, y)
    sub(y, a.z, b.z)
    mul(y, y, y)
    add(x, x, y)
    sqrt(x, x)

In other words, reordering can reduce register allocation fairly drastically (from 18 to 2 temp vars).
so we should probably be looking at this as a first-order optimisation, right?

general heuristic is "move an instruction backwards to be as close to its generators as possible". So we'd do it like this.

    mov('a_0.x', '1')
    mov('b_1.x', '4')
    sub('n_4', 'a_0.x', 'b_1.x')
    mov('v_3.x', 'n_4')
    mul('n_8', 'v_3.x', 'v_3.x')

    mov('a_0.y', '2')
    mov('b_1.y', '5')
    sub('n_5', 'a_0.y', 'b_1.y')
    mov('v_3.y', 'n_5')
    mul('n_10', 'v_3.y', 'v_3.y')

    mov('a_0.z', '3')
    mov('b_1.z', '6')
    sub('n_6', 'a_0.z', 'b_1.z')
    mov('v_3.z', 'n_6')
    mul('n_11', 'v_3.z', 'v_3.z')
    
    add('n_9', 'n_10', 'n_11')
    add('n_7', 'n_8', 'n_9')
    sqrt('d_2', 'n_7')

Interesting:

    a.x * b.x + a.y * b.y + a.z * b.z

can be split two ways:

    (a.x * b.x + a.y * b.y) + (a.z * b.z)       # choose the last highest ranked

    (a.x * b.x) + (a.y * b.y + a.z * b.z)       # choose the first highest ranked

=> we can do that by changing a single comparison in the embracket function!

And this will change the order of computation to reduce the complexity of the mov-as-close-as-you-can algorithm.
I think is a general improvement.

That's pretty cool really. OK let's try it.

I also think if we're moving instructions around then they need to be stored as actual objects, not just text.

Having made that change, the new instruction output is:

    mov('a_0.x', '1')
    mov('a_0.y', '2')
    mov('a_0.z', '3')
    mov('b_1.x', '4')
    mov('b_1.y', '5')
    mov('b_1.z', '6')
    sub('n_4', 'a_0.x', 'b_1.x')
    sub('n_5', 'a_0.y', 'b_1.y')
    sub('n_6', 'a_0.z', 'b_1.z')
    mov('v_3.x', 'n_4')
    mov('v_3.y', 'n_5')
    mov('v_3.z', 'n_6')
    mul('n_9', 'v_3.x', 'v_3.x')
    mul('n_10', 'v_3.y', 'v_3.y')
    add('n_8', 'n_9', 'n_10')
    mul('n_11', 'v_3.z', 'v_3.z')
    add('n_7', 'n_8', 'n_11')
    sqrt('d_2', 'n_7')

If we now apply our "move up as far as you can" repeatedly, we get this:

    mov('a_0.x', '1')    
    mov('b_1.x', '4')
    sub('n_4', 'a_0.x', 'b_1.x')
    mov('v_3.x', 'n_4')
    mul('n_9', 'v_3.x', 'v_3.x')

    mov('a_0.y', '2')
    mov('b_1.y', '5')
    sub('n_5', 'a_0.y', 'b_1.y')
    mov('v_3.y', 'n_5')
    mul('n_10', 'v_3.y', 'v_3.y')
    add('n_8', 'n_9', 'n_10')

    mov('a_0.z', '3')
    mov('b_1.z', '6')
    sub('n_6', 'a_0.z', 'b_1.z')
    mov('v_3.z', 'n_6')
    mul('n_11', 'v_3.z', 'v_3.z')
    add('n_7', 'n_8', 'n_11')

    sqrt('d_2', 'n_7')

We can now apply the "remove-mov" optimisation (if we assign to var1 and immediate mov var2, var1, just assign to var2):

    mov('a_0.x', '1')    
    mov('b_1.x', '4')
    sub('v_3.x', 'a_0.x', 'b_1.x')
    mul('n_9', 'v_3.x', 'v_3.x')

    mov('a_0.y', '2')
    mov('b_1.y', '5')
    sub('v_3.y', 'a_0.y', 'b_1.y')
    mul('n_10', 'v_3.y', 'v_3.y')
    add('n_8', 'n_9', 'n_10')

    mov('a_0.z', '3')
    mov('b_1.z', '6')
    sub('v_3.z', 'a_0.z', 'b_1.z')
    mul('n_11', 'v_3.z', 'v_3.z')
    add('n_7', 'n_8', 'n_11')

    sqrt('d_2', 'n_7')

And now we have the optimal code.



---
ok so next steps:

1- concrete type substitution
2- actually run it and produce results
3- if/then/else and a test for it
4- streams/arrays/whatever


------

jan 18: we are not generating something that looks like assembler.
instruction set is defined in the backend, replacing "pass" with "emit" keyword. It emits just to a sort of middleware VM assembler, not dissimilar to LLVM or MLIR or whatever, but substantially less lines of code (backend.py currently has 300 lines) and no dependencies on byzantine legacy bullshit.

Here's the test input code we're trying out:

    on test_vectormath()
        vec a = vec(1, 2, 3)
        vec b = vec(4, 5, 6)
        number d = distance between a and b

and here's the code we spit out:

    def test_vectormath():
        var('a_0.x', 'number')
        var('a_0.y', 'number')
        var('a_0.z', 'number')
        mov('a_0.x', '1')
        mov('a_0.y', '2')
        mov('a_0.z', '3')
        var('b_1.x', 'number')
        var('b_1.y', 'number')
        var('b_1.z', 'number')
        mov('b_1.x', '4')
        mov('b_1.y', '5')
        mov('b_1.z', '6')
        var('d_2', 'number')
        var('v_3.x', 'number')
        var('v_3.y', 'number')
        var('v_3.z', 'number')
        var('n_4', 'number')
        sub('n_4', 'a_0.x', 'b_1.x')
        var('n_5', 'number')
        sub('n_5', 'a_0.y', 'b_1.y')
        var('n_6', 'number')
        sub('n_6', 'a_0.z', 'b_1.z')
        mov('v_3.x', 'n_4')
        mov('v_3.y', 'n_5')
        mov('v_3.z', 'n_6')
        var('n_7', 'number')
        var('n_8', 'number')
        mul('n_8', 'v_3.x', 'v_3.x')
        var('n_9', 'number')
        var('n_10', 'number')
        mul('n_10', 'v_3.y', 'v_3.y')
        var('n_11', 'number')
        mul('n_11', 'v_3.z', 'v_3.z')
        add('n_9', 'n_10', 'n_11')
        add('n_7', 'n_8', 'n_9')
        sqrt('d_2', 'n_7')

Which on inspection looks correct. It'll just inline all the way down until it finds "emit" functions, at which point it writes the function call to the output. So you can define any instruction set you want in the Backend like this:

    feature Backend
        type u8, u16, u32, u64
        type i8, i16, i32, i64
        type f16, f32, f64
        on (i32 r) = add(i32 a, b) emit
        on (i32 r) = sub(i32 a, b) emit
        on (i32 r) = mul(i32 a, b) emit
        on (i32 r) = div(i32 a, b) emit
        on (i32 r) = sqrt(i32 n) emit
        on (f32 r) = add(f32 a, b) emit
        on (f32 r) = sub(f32 a, b) emit
        on (f32 r) = mul(f32 a, b) emit
        on (f32 r) = div(f32 a, b) emit
        on (f32 r) = sqrt(f32 n) emit

The next step is to actually make this work for f32, so we need a type-substitution policy and figure out how to represent / apply it. The simplest one is just go replace = { 'number': 'f32', 'int': i32 } and that'll be fine. We can poke that into the config class and all is good.

We need to look into compiling things as "functions", i.e. not inline. We need to get conditionals working (jumps) and then arrays/streams so we can handle strings and printing.

It feels like that's the right order to tackle the next steps in.

-------

end of play today: jan 16 2025.

have managed to call the constructor functiondef with replacements for dest_var and arguments. So far so good.

Next: figure out why constructor Function hasn't been built.
Then make that work for constructor Function.

=> 



---

Possible things to do next:
1- if/then: my preferred form is "a if cond else b"; and I think that's an expression type.
2- separate assignment from push-chain, so we want to do "a << b << c .. cond"
3- we want array notations for things; I like the unification of string and array, but we definitely need [] something, because we want to to build them.
4- but we also super need to actually generate some python code. zeta.py

So I think the first order of business is to generate python code.
Let's do that today at soho house berlin.
-----

Fixed the scope visibility thing. Now everything (except FeatureDefs) are scoped to the feature they got declared in. FeatureDefs still have scope None; which we should probably replace with some kind of Context or Global scope, we'll see how that works out when we get into multiple contexts etc.

On that subject, some thoughts:

Context = a set of Features that are enabled, and values for their Feature-scope variables. I.e. it's a saved state plus some behaviour, not at all dissimilar to an object.

However, we want to be able to spread a Context across multiple physical Devices (which seems like a good enough name for a hardware unit, including a GPU), and there's obviously such a thing as a Cluster (a Cluster is a Device, discuss...).

A running Context can obviousy be spread out over a Cluster. So Clusters are trees, really, and they have to communicate. The bit that's interesting is the part that specifies which data and functions of the Context live on which Device/Cluster (and of course which concrete types are used for which abstract ones).
--------

need to figure out the following:

what are the rules for what scope things get defined in?
eg. if a VarDef is in scope X, then the Var it defines should also be in X.
but if a TypeDef/FunctionDef is in X, then the Type/Function is defined in global scope.
same for feature-scope variables; they are visible from anywhere.

I propose the following rule:
- if something is defined in feature X, it can be read from anywhere but only modified from X/children.

so type u8 is defined in backend, and can be seen anywhere, but only modified in a subfeature of backend. same with functions; you can call them from anywhere, but you can only extend or modify them from a child of the feature they were defined in.

I think this works for everything.


----
Next step: let's clean up the reports from each stage.
- make sure no scopes are "None" => types, functions, etc
- separate stage reports properly (embracket/split)
- make v.x report "v" and "x" properly
- get some tests going so we can catch regressions easily
- exercise the error pathways as well

Then on to code generation!

-----

actually no: we are going to do the visual feedback, we're just going to make it work.

first, rather than key it to entities, we'll key it to Lex - which means no overlaps are possible, and everything's just one lex long. Much simpler.

We'll use SourceLocation(None, None) to indicate something that isn't in the text, which makes them easy to filter.

We can combine the add/resolve thing into a single view, with two colours.

key check-types off the function name (first item with 'name' property).

go right to left, arrange things onto multiple lines just below the actual code line.


------

errors/reports kind of work, the code pattern is kind of ok,
but there's still a few like issues and stuff. but we'll figure it out.
The key point is that we need some kind of lex-range,
also we shouldn't put things in there that are generated from code, rather than the code itself. Eg. Never Function/Type/Variable, only FunctionDef/TypeDef/VariableDef/Ref

Of course that means we can't show Function/Type/Variable in this view, which is bad, but anyway. We could maybe show derived whatsits, eg. this TypeDef generated this Type, and so on.
-------
to get "hello world" example working

- errors, reports and tests from stage outputs
- result-var type-check
- use-before-declare check (statement index tag)
- stream handling so we can print
- string literals with vars "hello \(name)"
- python code generation

----------------------------------------------
OK, everything is super clean now.
=> not entirely happy with how we're handling constructor alias symbol : vec(x,y,z)
=> but it's the best I can do for the moment. 
=> We need to find a better syntax unifying name and signature aliases.
=> but in the interest of making progress, that's what's happening.
It's easy to fix it whenever we decide.

Next, we need to typecheck ResultVariable;
and check the types of lhs vs rhs. Super easy!


------------------------------------------------
let's think about this a bit more carefully.

    TypeDef.add_symbols: 
        create Type; add properties; set _resolved_type => new Type

    "x" => Variable("x", type "number")
    Now what should happen is that we traverse _resolved_types: and Type returns a scope;
    and therefore "x" gets bound to that scope.

    so what we want to see at the first retry is: does resolve get called on Type(vector)?

    that's what we want to find out.

-------------------------------------------------

resolution is a little gnarly because the correct order is:
for VariableRef "v.x": 

1- v => variable of type "vec"
2- Type(vec) => ensure that all properties have been resolved (i.e. traverse)
3- call scope => returns Type(vec)
4- then go down children with scope Type(vec) => .x => resolved correctly.

clear understanding is essential - we have to understand WHY this is not smooth.
and the answer is, because anything that's a Lex/ref has to be resolved with the right scope, but you can't ensure you have the right scope until you've resolved correctly upstream.

The *only* correct scope is the Type, which means it has to be resolved properly.
Could we do this in two passes? => yes, we probably could.
That might be the simplest solution.

First pass: "v" doesn't return a scope that's an entity, so: DON'T TRAVERSE because you've got a bad scope.

*BUT* for private stuff, you don't need the thing as the scope, so Type is still in surface scope.

OK, this makes sense.

--------------------------------------
where we left it: finding that resolve a.b.c breaks our nice pattern.
which is kind of dumb and annoying but we can do a special case for it.
what really needs to happen is:

    v.x:

    first we resolve "v" => variable(type=vector)
    then we call getscope on "v", which returns v->variable->type;
    so the scope "type" gets passed down to
    "x", which will then match correctly.

    I think what we can do however is mess with that in the resolve logic,
    by checking the parent/name (which we have for VariableRef), and recomputing the scope there.
    it's sort of weird but hm well I guess we have to.
    => the other way is to push the call inside the collect function and make it a proper pass.
    
    => but I like that less than I like this.
    => think about it while sleeping !
------


okay, it's time for a total rearchitect of this whole process... it's rotten and it needs fixing.

we have three "passes" so far: add, resolve, type-check.

How would we like the code to look?

    "for each entity in the tree, find its scope and pass it down to its children"

Seems like scope computation is just the classic thing we need to figure out. So let's do that.

We'd like to be able to do stuff like:

    "show me every lex that's not bound to something"

so I'm "attr_name in parent"



----------------------------------------------------------

got function-resolver/type-check working.
next have to check type of rhs against type of lhs.

then look at add/resolve/func processes and see if there's a nicer way to handle them.
but we are now pretty damn close to trying the py_backend.
let's do it!

----

OK so here's an interesting idea... 

record the conversation between me and claude/gpt. I.e. all the code getting pasted in and out, goal specification, etc, as we go through the process of writing the first backend, porting to mlir, all that stuff.

If the entire conversation is recorded *along with the fnf code* then maybe it's super easy to automate the process, just by recasting the conversation as a "roleplay" (eg. "user asks for X, dev replies Y") and using at as the system prompt. As long as the goals of the user is spelled out nicely at the top, then we should be able to automate the process a bit.

So something like:

    user: "dev, I want to write a generator for MLIR code"
    dev: "OK, do xyz..."
    console: xyz... => output
    file: xyz...

=> so in other words, the complete history of the interaction.

And then you add:

    "user: thanks! OK, so now I want to blahblah.

I wonder if having examples of all those conversations is really the easiest way of speeding up. One could even automate the generation of the instructions.


---------

OK so the new plan is this:

0. Implement Type Substitution Policies (TSPs) based on function-matching-induced type constraints.

1. Make a python backend so we understand how to make it work. It's super simple and we're in the groove with it, we can get it working quickly and gain good understanding.

2. Once that's going, the following needs to happen:
    2.1 write the md->lang extractor in python so we understand how it looks
    2.2 rewrite the md->lang extractor in canonical zero (fnf), modifying zero as we go!
    2.3 start using the zero version from then on

3. Now we can start writing the compiler (including py-backend) in fnf zero.

4. Once this is working and bootstrapped, we can write the MLIR backend in fnf zero (with the help of LLMs). Automating this process is the key outcome from this phase.

5. This we then get running on a bare-bones RISC-v system (no OS) but with literally every piece of chain defined in feature-modular, literate, zero. Which is what we want to achieve.

----


next step: spreading type constraints upwards; implementing a type constraint policy.
or shall we call it the type substitution policy (TSP).
we just define syntax for it: 

    float.f32 x, y, z = 0

So a TypeRef isn't just a single name, it can also be a chain of names a.b.
So we just need to add some of "constraints" tag to the FunctionCall object, 
that notes down that this is a constraint that has to spread upwards.

When we come to build the substitution policy, we'll propagate the substitution upwards.

Type Substitution Policy (TSP) is generated from the program according to some kind of logic, and we can examine the results. That's super nice, because then we can just output the resulting program via the backend grammar (using as_code).

Once you have a parser/printer, you can use it *errywhere*. I think we do have to start passing in the grammar to the printer and parser functions (or make them ... methods?) So you'd just make everything a method of Language and access self.grammar => hmmm dunno.

I feel like we should maybe think about doing a python backend first just because it's so fricking easy. We totally should.

-----

Oooh, so interesting reading about how to output MLIR from python code.
MLIR is an intermediate instruction set we can compile to WebASM, LLVM, SPIR-V.
So it's exactly what we're looking for.

MLIR code (that we'd generate from the zero code) looks like this:

    func.func @add(%arg0: i32, %arg1: i32) -> i32 {
        %0 = arith.addi %arg0, %arg1 : i32
        return %0 : i32
    }
    
Bright idea is to define MLIR grammar equivalents for types, functions, variables, etc.
Do we can define two grammars with overlapping rule-sets, and we just cast from (eg)

    zc.Function 
     
to (eg) 

    mlir.Function

and then run the parser backwards ("as_code") to output in the target language. That's rather nice, isn't it?

=> and we've just done import mlir_classes as mlir
and the backend is just mlir_backend.py

That's super nice because it's... super nice.






-----------------
So now let's think about concrete types.

I do quite like the idea that _xxx is "native". So _u16, _i32, _f32, _ieee(e,m)
How about the following notion to capture the idea "what type is {var}" - it's a pair of types, the abstract and the concrete substitute.

float.f32       number.i8

One way of thinking about this is that we're doing a text-replace of eg. number with i8 for those variables, and building the program. In fact that's a super way of thinking about it.
In other words, we have to assign concrete types to every symbol we added during add_symbols.

A "policy" is just concrete-typed everything in the source code; i.e. all type names are replaced with something concrete. Computing this replacement across the code is basically policy stuff.

---------

ok so what's next?
well, RN we're creating the functions, but we're not filling them in.
so that composition step should be next, right?
since we just have lists of statements, that should be fairly easy.
we need to look at sequence, and parallelism.
so we need some syntax for this.
how about just using the Feature.func() definition, and we're good?
we can let our code-generation inline mechanisms do the lifting.

----------

operator bracketing :done!
function resolution: done!

add_symbols needs to be top-down, and do embracketing non-recursively.
resolve needs to be bottom-up (i.e. do ast children first, then parent)

this way function resolve doesn't need to short-circuit the top-down resolve process.

what are the next steps?
we need some kind of "pass" / "future plugs in here" keyword for function bodies.
pass: done.


Function composition;
and then code rewriting using substitution, right?

----------------------------------

interlude: Christmas morning thoughts / 2024

2. thinking bigger thoughts about the compiler / IDE / LLM intersection.

zeta is a zero-to-anything compiler. It can therefore generate translation examples very quickly in one direction. The question is: can we use this power to 

- automate the creation of new backends; eg. this is zero-to-py, write me zero-to-ts.
- automate the translation of code from the backend, into zero; i.e. the reverse translation.

this means that we should get as quickly as possible to a point where we can edit code nicely in a zero.md "IDE". That's the springboard to start experimenting with this stuff- once you have a codebase in FNF.

web backends: useful because it means we can write interfaces-over-the-web which is a useful bit of kit to have. Again the idea of movable code is interesting here.

But fundamentally a backend is just called over RPC, and zero gets out of the way and makes those rpc calls invisible. The same code will work regardless of which bits of the computation are done where.

Thinking of a Context = (some features, some state); we can think about how you might split that computation across a set of Devices. A Device is any physical piece of hardware, plus some Backend. This split fundamentally describes "what runs where" given a set of Devices; but we can write code to figure out what the split is. So we can write code that takes into account stuff like
    
"this bit of data has to live here, and it can't move, so computation related to that data has to happen here also"

"this computation runs fastest on specific hardware so it has to run here"

"these two bits of hardware are on the same device, but this other one is on wifi"

In other words we have to take account of capabilities of nodes (memory, cores, speed) and edges (bandwidth, reliability) to determine where to put data X and computation Y. 

Giving code the ability to control where it runs (using a simple API) creates something that could legitimately be called an Agent; it manages a set of Contexts, and uses the runtime API to configure where it runs on a cluster.


1.

The drone project is a real thing, and I will need some people to work on it.
Clearly, the litewing team is the right place to start, so I should reach out to them and go and meet them. Chennai is calling.

A thought occurred that the two drone function/price points:

https://www.kickstarter.com/projects/2130557124/litewing-a-fun-diy-wifi-mini-drone-based-on-esp32/description

and

https://www.youtube.com/watch?v=RSEZ7TnXP-w

The crazyflie approach (ARM-cortex main system, daughterboard running neural networks on RiscV cores) gives you autonomous navigation and mapping.

The lightning flash this evening was that actually, you need both. You need a small number of high-cost, high-intelligence drones to "map out" the location of the litter and the obstacles (fences, forests, etc) so the low-cost drones can swarm safely.

The novel part of making this with zero would be to be able to write code "for the swarm" that does both mapping and AI stuff *and* the collection stuff, but with a single code base and workflow, and a "location agnostic" approach to computation.

It also really brings home the reality that for code to be portable, it has to fit nicely into existing systems. We really are writing polyglot systems, and we have to do that if we want to get anywhere in a reasonable time. It's about designing the correct backend APIs for each device, and then doing the high-level coding in zero.


-----

Ok so the next big job is function-call / operation parsing.
I *think* it's actually pretty easy: just write a function called "embracket" which replaces sequences with Bracketed() items, using operator precedence, and using type signatures.
=> need to add return types to the long_handle we compute
=> or key the function off its return type signature.
=> but this is the heart of the zero type system, so it's pretty important.
=> so let's take it slow
=> once it's working, we can look at how to tidy up the code.


-----

constructors now work, apparently.
need to do resolution on type parent/child => 
next is to be able to add brackets into a complex functioncall, to make everything work.
i.e. proper functioncall.resolve.
but then we're good to go, I think.

for code generation, boil things down to a simple Backend class, which has to define certain basic things, such as add(), sub(), mul(), div(), etc. The "virtual machine" so to speak. And then each backend implements those with different 'emit()' calls.

For some reason I'm quite liking the idea of a python backend as the first thing to try.
Just because we're very at home in it right now.



-

next: create a constructor from the typedef/struct.
then: TypeRef, to handle char$ not found. should generate char not found.
concrete types in the backend: => has to supply.
we should really be generating a few backends in parallel, just because.
we talked about typescript, but maybe it should be C or maybe even WASM.
=> the less work we do the better. Both are good, but if we're going for risc-v, we should just go for a risc-v backend and stop fucking around.

=> so then we need a dev board. Figure out the one you need for a drone.



----

new method is much nicer:
- grammar writes a .py file containing just the class definitions (__init__ methods)
- this gets forcibly re-imported, and the classes added into the rule system
- a decorator grammar.method(class) around a function turns it into a method and adds it to the class
- this is put into a separate pass ("methods") so it's all super clean and lovely
- means we can write code as code, but auto-generate the classes. it's a nice we like.

Look at zero.py to see how this feels to use.

-----

solution: add the overridden method names in the grammar; define them *outside* (in normal code)
using eg.

    FunctionCall[validate, resolve] < Expression := etcetc

    def validate_FunctionCall(fc: FunctionCall, etcetc):
        xxx

    def resolve_FunctionCall(fc: FunctionCall, etcetc):
        xxx

That's super compact and easy, all you're doing is registering that the system can call the externally defined function NodeName_function(blah).

----

non-smoothness:
1- coding in text form isn't actually a good way of doing things; find a better way.
   -> I think it might be: just call normal functions defined in global scope somehow
   -> that way your text-methods are just oneliners
2- function-scope stuff isn't clearly defined: need some way to say "this entity can be a scope"
   -> pass scope into .resolve(), track it in the resolve routine in symbol_table
3- need to figure out the function-matching, because there'll have to be type stuff

just feels non-smooth at the moment, have to figure out why.
maybe having all the different stages jammed up together actually isn't that great a win.
maybe you want to separate them out into their own area, while keeping it modular.

yeah-don't do it that way. do define_grammar, define_symbols => that way there's proper separation of concerns.
can we have test code snippets for each feature, and just test that, rather than the whole thing?
there's definitely a smarter way of handing this.

----

tomorrow: type-vars, structure extension test (Math, VectorMath)
try parsing a + b properly
then do the actual replace
report errors properly (including lex locations etc)
operator precedence (parse the dot-product expression)
declare concrete types (i8 etc) somehow

---
today: add/resolve needs to be able to check if a symbol exists already
eg. for type structure extension, adding a stub to a function, and so on.
---

Now we're ready to do scoping.
"feature scope" = names declared in features
"function scope" = names declared in functions
"type scope" = names declared inside structures

when we add stuff, we'll add (symbol, item-pointed-to, scope-object)
and scope-object will be a feature, a function, or a type.

when we read stuff, we'll look for symbol, *maybe* we'll specify a scope (usually a function or type) and we'll want item-pointed-to, and the scope it was defined in.

so the data structure we want is 

    symbol => { item, scope }

if we inherit from some feature, we should be able to see its symbols; eg. types and so on.

so basically we have

    symbol => array { scope, item }


----

Introduce language modules!
which are a great site to add symbol table stuff.
we can now add "build_symbols" and "resolve_symbols" to each one.
Context scope: Feature scope: Function scope.


---

OK: so we now have auto-generated ast classes, and we've learned quite a bit about python internals.
The result is that we now have some really nice things!
Next, we are going to see how this works when we come to building and resolving symbol tables.

----

Note that there's an even better format:

    Feature = "feature" name:NameDef ("extends" parent:Feature&)? "{" body: Component*; "}"

If we could use this to define the class:

    class Feature:
        name: NameDef
        parent: Feature | None
        body: List[Component]

and define the rules directly:

    Rule(Feature, terms=[Term(["'feature'"]), Term([var="name", vals=["NameDef"]])])

So in other words, something that writes python code based on the grammar input.

That would be amazing, wouldn't it? To be able to actually create the classes from the grammar, rather than the other way round.
Classes in python are actually super clumsy, not the best way at all.

---

OK! So next we are going to look at symbol tables.
There are two scopes: Feature scope and Function scope.
There will be two passes: the first to build the symbol table, and the second to resolve references.
Then we can look at expression resolution (mapping vars to vars, function calls to functions)
Finally look at composing Functions from individual FunctionDefs;
and then look at how to output Functions in multiple backends.

Some kind of "emit" thing is what we need, I suspect. But let's go step by step.

-----

need to figure out testing strategy: do it using test classes.
next: grammar second-order properties computed
then: parser.
then: symbol table.
then: semantic analysis.
then: code generation.

Just because: using classes for AST types, get away from this silly dictionary approach.
Found a quite nice, compact representation that allows us to refer to things of type X.
So that's nice. Grammar builder works now. Parser should be a straight lift.
Altogether rather sweet, and will make codegen so much easier.
The most important thing will be the ability to specify everything from language form to final output, in a modular, incremental way.
Tomorrow: couple of silly bugs in the output grammar, but we're good basically.
Don't know how to do the test, because the dynamic list will change. 
Guess I'll have to construct the test grammar from a known list of classes.

-----

DONE: get the ; separator special-case working
todo: explore some "real errors" in lists, unify the optional- and list- error handler cases.
todo: do some actual semantic processing
todo: modularise the whole thing
todo: get some actual output typescript running ("hello world")
------

Nested separators work! Still a little gnarly - we have to store nested state in the reader, because it's dynamic; we only handle one level of nesting; and we haven't considered what happens if there's a genuine error (right now we're assuming that an error in the last item means we overran a separator that's actually at a higher level). So the next step would be to try that.

---------------------

Two cases:

    incomplete test:

        > a => [missing rhs]

    name but no alias

        x [no "|" alias], y, z          => expected "|", got ","

I think we can fix this by doing a scan-forward for the separator?
But that can only happen if we're not nested-separator.

-----------------

how to handle nested separators...

    name_decl := type:<identifier> names:<identifier>*,
    results := "(" name_decl+, ")"

So what does a list-parsing algorithm look like that could handle this?

first: identifying nested separator "," : that's done and working.

there are two basic approaches to this:

    the "eat what you can" approach, vs the "scan forward" approach

eat-what-you-can just reads forward in the list until an error, and then *does something*
The "does something" is a backtrack of some sort, where we invalidate the last N items, rewind to that point, and continue.

Let's look at this in action on an example:

    (int x,y, float k)

so here we'll do:

    results "(int x,y, float k)"
        "("
        name_decl+, "int x,y, float k
            name_decl "int x,y, float k"
                type: "int"
                name[0]: "x"
                sep: ","
                name[1]: "y"
                sep: ","
                name[2]: "float"
                sep: !!! separator mismatch

The problem with this is that there's no way to distinguish between two scenarios:

    1- this is correct code, "float" is a type and "int" is a type
    2- this represents a user error: "float" is a variable name, and the user meant to put a comma in between.

In case 1, we should backtrack to before the last separator; so we have

    name_decl "int x,y"
    remaining: ", float k"

    and then pop up to the level above. i mean it might really be that simple.
    Of course, this can only happen IF we're in a nested-separator situation, otherwise it's a straight error. So we have to have some kind of way of storing the nesting, and knowing that we're in a nesting situation (because that depends on what rules are further up the stack).

OK let's try it.

I think to speed up progress, we need to :

1- fix the output ast format (which I think we can settle on now)
2- move to a modular grammar so we can do semantic analysis etc in modules.

    

----------

annoying ambiguity requiring more complexity

    string result, out, int x, y, z

rules are:

    c_name_decl = type name*,
    result_vars = "(" c_name_decl*, ")"

The point is here that we have two competing separators ","

So given a particular separator, it could match the lower one (the one in c_name_decl) or a higher one (the one in result_vars).

The algorithm to parse this is a bit more complex. So we have to figure it out, and it has to work on all the previous tests also.

We'll call this the "nested separator problem"


-------------------


OK so NOW we have what seems to be a functioning parser, which is amazing news.
It could use a bit of a tidy up, but it's working !!
Next: return to semantic analysis. We do need to think about factoring this into modules ("features") but let's get the basics working first.

=> compile hello world context and take it from there.


-------------

next step is modular grammar/parser/semantic analysis
split the language into little bits, and generate the code for them one by one.
=> do this after you get the first end-to-end test running.

------

zero ideas again: thinking about this parser, for instance.
what if we did something like this:

```c++
class Feature {
    name: string
    parent?: Feature
    components*: Component
    print() { s << "feature" << name << (? << "extends" << parent?.name) << "{" << 
        (* << components*) << "}"
}
```

what if we applied the same thing to structures: don't define them like this, but define them as a sentence:

```
type feature = 
    feature (string name) [extends (Feature parent)]? { [Component components]* }
```

so then this would write the class for you:

    class _feature:
        def __init__(etc)
------


thinking about errors is a super interesting exercise,
because how errors flow through the system - and how we represent that flow clearly: is critical.

grand gestures on the type system level: we had error subtypes, right?

I think it's quite interesting to think about x?, x+, x* as different array types. We should also be able to parametrise x[1], x[2], x[3] as different types:

    type bit = 0, 1                 # one bit     
    type u[N] = bit[N]  < uint      # u8 = 8-bit uint, u16 = 16-bit uint, etcetc
    type i[N] = bit[N] < int        # i8 = 8-bit int, i16 = 16-bit int, etc
    type fp[N] = 
        bit s | sign = 0
        u[A if N is M else B] m | mantissa = 0
        u[A if N is M else B] e | exponent = 0
    type u32 = u[32]

Because they are super interesting and that's a super compact notation.

Because then we have stuff like:

    type feature =
        string name
        feature parent?
        component components*
    
    on (string s) << (feature f)
        s << "feature \(f.name+) \?("extends \(f.parent?)") { \*(f.components*) }"

But that should, I think, also work the other way... 

    on (feature f) = (string s)
        s >> "feature \(f.name) \?("extends \f.parent?) { \*(f.components*) }"

        parent?.x => number?    => that's fine.

    I wonder if we actually do want to try that. Would it... work?
    It's a super interesting idea.

    


---------

we have to handle errors better in the parser. that's critical, otherwise all future steps become compromised and non-smooth. So let's do that.

why don't we do: tir in a stack, one symbol at a time, backtrack?

try this one; it's a rule, so stick the next (feature_2:0) on the end, and try with the next token, and so on.

Hm I wonder if that can work? No huge thingies.

---------------

current microgoal before we fire into expresions: make all st items => variables.
so local variables and function results should all => variable.
let's do function results first => we have a bunch of declarations, and we need to move them across to variables: so let's create those variables.
This can totally be done as a post-process: but ultimately when we come to re-do the parser and the grammar in a feature-modular way, we should make that more regular.

-----

end-of-day/week notes:

today we got symbol tables working nicely. that's good!
next: expression parser. it's a two-stage thing.
at some point in the future we really do want to add "types" to the parser mechanics.
but no rewrites of that stuff until we have the whole thing working in zero.

to re-iterate our current goal:

be able to write a client/server website in pure zero, using a typescript backend (deno+browser).

where we are now: semantic analysis of the zero ast; we've got the symbol table;
the next thing is to 

- resolve expressions so we can factor them into variables, constants, and function calls.

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

