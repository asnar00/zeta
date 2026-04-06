# blackbox
*flight recorder for fault diagnosis*

## specification

Pure-logic flight recorder built on the `blackbox` platform primitives (elapsed time, timers, local key-value store). Manages a 60-second circular buffer of 10-second moments, each containing a state keyframe followed by timed actions. On fault report, the buffer is frozen, serialised, and queued for upload.

Uses the `remote` platform for uploading fault reports to the server, and the `runtime` platform for serialising context state into keyframes.

## definition

### types

A recorded action — a boundary-crossing function call with timing and correlation:

    type Action =
        number time
        string feature
        string name
        string args
        string correlation
        string result
        number elapsed
        string kind

The kind distinguishes entry types:

    type Action-Kind = call | nondeterministic

A moment is a 10-second slice of the recording:

    type Moment =
        number start-time
        string keyframe
        Action action$

A build fingerprint captures everything needed to reconstruct the exact running code:

    type Build-Fingerprint =
        string feature-tree
        string source-hash
        string platform-versions
        string context-defaults

A frozen fault report, ready for upload:

    type Fault-Report =
        string fault-id
        string session
        string comment
        string fingerprint
        Moment moment$
        number reported-at

### variables

    shared int moment-duration = 10000
    shared int max-moments = 6
    string session-id = ""
    string fingerprint = ""
    bool recording = false

### functions

Start recording for a session. Captures the build fingerprint and begins the moment tick:

    on start recording (string session)
        session-id = session
        fingerprint = build fingerprint ()
        recording = true
        ... begin first moment with a keyframe
        every (moment-duration) do ("rotate moment")

Stop recording and discard the buffer:

    on stop recording ()
        recording = false
        ... cancel the moment timer
        ... clear the moment buffer

Rotate to the next moment — called automatically every 10 seconds:

    on rotate moment ()
        if (recording)
            ... serialise current context as keyframe
            ... close current moment, open new one
            ... if buffer is full, overwrite oldest moment

Record a boundary-crossing function call:

    on record action (string feature, string name, string args, string correlation, string result, number elapsed)
        if (recording)
            Action a = Action(elapsed time (), feature, name, args, correlation, result, elapsed, "call")
            ... append a to current moment's action list

Record a non-deterministic value for replay fidelity:

    on record nondeterministic (string feature, string call, string value)
        if (recording)
            Action a = Action(elapsed time (), feature, call, "", "", value, 0, "nondeterministic")
            ... append a to current moment's action list

Freeze the buffer and attach a user comment. Returns a fault ID:

    on (string fault) = report fault (string comment)
        fault = random digits (8)
        ... freeze current moment buffer into a Fault-Report
        ... serialise the report as JSON
        store locally ("fault:" + fault, ... serialised report)
        ... start a fresh buffer for continued recording

Upload all pending fault reports. Call on reconnect:

    on upload pending faults ()
        string keys = stored keys ("fault:")
        ... for each key, retrieve the report and upload via remote
        ... on successful upload, remove the key

Get the build fingerprint for this session:

    on (string fp) = get fingerprint ()
        fp = fingerprint

## interface

Start the flight recorder for a session:

    on start recording (string session)

Stop and discard:

    on stop recording ()

Record a boundary-crossing call (auto-inserted by the emitter at compile time):

    on record action (string feature, string name, string args, string correlation, string result, number elapsed)

Record a non-deterministic value (auto-inserted by the emitter):

    on record nondeterministic (string feature, string call, string value)

Freeze the buffer with a user comment, returns a fault ID for tracking:

    on (string fault) = report fault (string comment)

    report fault ("login button did nothing") => "12345678"

Upload any fault reports that were saved while offline:

    on upload pending faults ()

## tests

Report fault returns an 8-character ID:

    start recording ("test")
    string fault = report fault ("test fault")
    length of (fault) => 8

Recording flag is set after start:

    start recording ("test")
    recording => true
    stop recording ()
    recording => false
