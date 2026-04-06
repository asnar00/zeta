# insights
*language theory and how to think in zero*

Notes from conversations about what zero is trying to be, and how to think in it. Part language theory, part design philosophy, part practical guide.

---

## SSA is telling you to find the stream

In imperative code, time is hidden. A variable `count = 0` followed by `count = count + 1` in a loop — the time dimension is implicit in the execution order. You have to simulate the machine in your head to understand what's happening.

In zero, time is the stream. `count$ <- 0 <- (count$ + 1)` — the entire history is visible. You can see that count evolves, what it depends on, and how. The `$` suffix is literally saying "this has a time dimension."

When you hit an SSA violation — when you want to write `s = s + thing` — zero isn't blocking you. It's telling you that `s` is actually a value that evolves over time. It's a stream. Name it `s$`, express how each value depends on the previous one, and the code writes itself.

**The rule: if you want to mutate it, it's a stream.**

### example: string accumulation

Imperative thinking (SSA violation):

    string s = ""
    for each item in items
        s = s + "," + item       # wrong: s is being "mutated"

Stream thinking:

    string s$ <- "" <- (s$ + "," + items$)

Each element of `s$` depends on the previous element and the corresponding element of `items$`. The reduce shorthand is even simpler:

    string result = items$ + _    # fold with concatenation

### example: counting

Imperative thinking:

    int count = 0
    for each x in stream
        if (condition)
            count = count + 1     # wrong: count is being "mutated"

Stream thinking: count is a stream that increments when the condition is true. It has a value at every point in time.

---

## streams are time-indexed arrays

A stream is an array of values indexed by time. There's an implicit `dt` attached to the array that maps array indices to timestamps. `x$[t]` means "the value of `x$` at time `t`."

Anything that evolves over time is a stream: video frames, audio samples, keystrokes, network packets, HTTP requests, user actions, sensor readings. The zero approach: make the time structure explicit instead of hiding it behind mutable state and callbacks.

This connects to the blackbox flight recorder. The blackbox records what happened over time — it IS a stream:

- `Action action$` — actions arrive over time, each with a timestamp
- `Moment moment$` — every 10 seconds, a new moment crystallises from the action stream
- `Fault-Report fault$` — when the user reports, a fault crystallises from the moment stream

Each layer is a stream derived from the one below. No mutable state, no bags of key-value pairs — just values evolving over time.

### timed streams (speculative)

If `dt` were a first-class property of streams, we could express time-dependent behaviour directly:

    Audio sample$ = microphone (dt=1/44100)     # 44.1kHz audio
    Video frame$ = camera (dt=1/30)             # 30fps video
    Action action$ = user input (dt=variable)   # irregular timestamps

This would let zero handle media, real-time control, and physics simulation with the same stream model used for HTTP requests and keyboard input. The `dt` is the bridge between "array of values" and "signal over time."

---

## timed streams as the universal primitive

Every stream has a `dt` — the time between samples. Under the hood, a stream is a (possibly sparse) circular buffer with a simple multiplier to get from array index `i` to wall-clock time `t`:

    t = i * dt

Specific rates:

- `dt = 0` — instant (compute as fast as possible, the current default)
- `dt = 1.0` — one sample per second (1 hz)
- `dt = 10.0` — one sample per 10 seconds
- `dt = variable` — irregular, each element carries its own timestamp

The syntax attaches rate and window to any stream:

    Action action$ <- user input () at (variable) keep (60 seconds)
    string keyframe$ <- serialize context () at (0.1 hz) keep (60 seconds)
    int countdown$ <- count down from (10) at (1 hz)

Where `at` sets the sample rate and `keep` sets the circular buffer window. The runtime handles rotation, indexing, and cleanup automatically.

### the buffer management was the language gap

The blackbox flight recorder required hundreds of lines of buffer management code: moment rotation, index bumping, action counting, keyed collections, string-to-int conversions. All of that was compensating for not having timed streams as a primitive. With timed streams, the blackbox becomes:

    Action action$ <- ... at (variable) keep (60 seconds)
    string keyframe$ <- serialize context () at (0.1 hz) keep (60 seconds)

    on (string report) = report fault (string comment)
        report <- freeze (action$) with (keyframe$) comment (comment)

The buffer management code *was the language gap*.

### stream operations

The operations we need to express cleanly:

- **Rate**: `at (1 hz)`, `at (variable)` — when values arrive
- **Window**: `keep (60 seconds)` — how much history to retain
- **Freeze**: snapshot a stream's current window
- **Derive**: one stream from another at a different rate
- **Merge**: combine streams from different sources (multi-device)
- **Replay**: feed recorded values back into a stream

### everything is streams

The same primitive handles every domain nøøb targets:

    # drum machine
    beat$ <- pattern () at (120 bpm)

    # drone controller
    thrust$ <- pid (target$, altitude$) at (100 hz)

    # chat app
    message$ <- user input () at (variable)

    # game loop
    frame$ <- update (state$, input$) at (60 hz)

    # blackbox flight recorder
    action$ <- record user input () at (variable) keep (60 seconds)

    # audio synthesizer
    sample$ <- oscillator (440 hz) at (44100 hz)

    # video
    frame$ <- camera () at (30 hz)

They're all the same thing: values evolving over time, at different rates, derived from each other. The language just needs to express that cleanly.

---

## the game

We're groping towards a new way of expressing behaviour over time that lets us think more clearly. The imperative model (mutable state, execution order, callbacks) forces you to simulate the machine. The zero model (streams, SSA, explicit time) lets you see the structure.

Every time zero feels restrictive — every SSA violation, every temptation to use a mutable variable — it's pointing at a place where the temporal structure hasn't been made explicit yet. The restriction is the insight.
