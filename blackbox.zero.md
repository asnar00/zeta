# blackbox
*flight recorder for fault diagnosis*

## specification

Flight recorder that continuously captures app state and actions into a circular buffer. When a user reports a fault, the buffer is frozen, annotated with a comment, and uploaded to the server for replay-based diagnosis.

The buffer is a rolling 60-second window divided into 10-second moments. Each moment opens with a state snapshot (keyframe) followed by timed actions:

    state -> actions -> state -> actions -> ... -> state -> actions

All mutable buffer state lives in a shared keyed collection `bb$[string]`, keeping the logic pure zero with no platform-specific code. Serialization uses the `<-` string build operator.

## definition

    feature blackbox extends website

### state

All buffer state in a single keyed collection:

    shared string bb$[string]
    shared int max-moments = 6
    shared int moment-duration = 10000

### serializing an action entry

Build a pipe-delimited action entry from time, body (feature|name|args|correlation|result), elapsed, and kind:

    on (string entry) = action entry (number time) body (string body) elapsed (number elapsed) kind (string kind)
        string entry <- "" <- time <- "|" <- body <- "|" <- elapsed <- "|" <- kind

### collecting actions into a comma-separated string

Uses a task so mutable state can accumulate across iterations:

    on (string line$) <- collect actions (int count)
        string s = ""
        int i$ = [0 to count]
        for each (i) in (i$)
            string key <- "action:" <- i
            string entry = bb$[key]
            if (s == "")
                s = entry
            else
                s = s + "," + entry
        line$ <- s

### collecting moments into a comma-separated string

    on (string line$) <- collect moments (int total)
        string s = ""
        int i$ = [0 to total]
        for each (i) in (i$)
            string key <- "moment:" <- i
            string entry = bb$[key]
            if (s == "")
                s = entry
            else
                s = s + "," + entry
        line$ <- s

### serializing a moment

    on (string json) = serialize moment (string keyframe) actions (string actions) start (string start) end (number end)
        string json <- "{\"keyframe\":\"" <- keyframe <- "\",\"actions\":[" <- actions <- "],\"start_time\":" <- start <- ",\"end_time\":" <- end <- "}"

### recording actions

Record a boundary-crossing function call into the current moment:

    on record action (string feature, string name, string args, string correlation, string result, number elapsed)
        if (bb$["recording"] == "true")
            string count = bb$["action-count"]
            string body <- feature <- "|" <- name <- "|" <- args <- "|" <- correlation <- "|" <- result
            string entry = action entry (elapsed time ()) body (body) elapsed (elapsed) kind ("call")
            string key <- "action:" <- count
            bb$[key] = entry
            int n = to int (count)
            string next <- "" <- (n + 1)
            bb$["action-count"] = next

Record a non-deterministic value for replay fidelity:

    on record nondeterministic (string feature, string call, string value)
        if (bb$["recording"] == "true")
            string count = bb$["action-count"]
            string body <- feature <- "|" <- call <- "||" <- value
            string entry = action entry (elapsed time ()) body (body) elapsed (0) kind ("nondeterministic")
            string key <- "action:" <- count
            bb$[key] = entry
            int n = to int (count)
            string next <- "" <- (n + 1)
            bb$["action-count"] = next

### moment rotation

Clear the action buffer for a new moment:

    on clear action buffer ()
        bb$["action-count"] = "0"

Bump the moment index, wrapping at max-moments:

    on bump index ()
        int old-idx = to int (bb$["index"])
        int old-total = to int (bb$["total"])
        int new-idx = (0) if (old-idx + 1 == max-moments) else (old-idx + 1)
        int new-total = (old-total) if (old-total == max-moments) else (old-total + 1)
        string idx-str <- "" <- new-idx
        string total-str <- "" <- new-total
        bb$["index"] = idx-str
        bb$["total"] = total-str

Rotate to the next moment — called automatically every 10 seconds by the timer:

    on rotate moment ()
        if (bb$["recording"] == "true")
            int count = to int (bb$["action-count"])
            string actions$ <- collect actions (count)
            string actions = actions$[0]
            string start-time = bb$["start"]
            string moment = serialize moment (bb$["keyframe"]) actions (actions) start (start-time) end (elapsed time ())
            string moment-key <- "moment:" <- bb$["index"]
            bb$[moment-key] = moment
            bump index ()
            clear action buffer ()
            bb$["keyframe"] = ""
            string new-start <- "" <- elapsed time ()
            bb$["start"] = new-start

### lifecycle

Start recording for a session:

    on start recording (string session)
        bb$["recording"] = "true"
        bb$["session"] = session
        bb$["index"] = "0"
        bb$["total"] = "0"
        bb$["keyframe"] = ""
        string start <- "" <- elapsed time ()
        bb$["start"] = start
        clear action buffer ()
        every (moment-duration) do ("rotate moment")

Stop recording and discard the buffer:

    on stop recording ()
        bb$["recording"] = "false"

### fault reporting

Freeze the buffer and attach a user comment. Returns a fault ID:

    on (string fault) = report fault (string comment)
        fault = random digits (8)
        int total = length of (bb$["total"])
        string moments$ <- collect moments (total)
        string moments = moments$[0]
        string report <- "{\"fault_id\":\"" <- fault <- "\",\"session\":\"" <- bb$["session"] <- "\",\"comment\":\"" <- comment <- "\",\"moments\":[" <- moments <- "],\"reported_at\":" <- elapsed time () <- "}"
        store locally ("fault:" + fault, report)

Upload all pending fault reports:

    on upload pending faults ()
        ... retrieve stored keys with prefix "fault:", upload each via remote

Get the build fingerprint for this session:

    on (string fp) = build fingerprint ()
        ... read _BUILD_FINGERPRINT from module

Retrieve a stored fault report by ID:

    on (string result) = get fault (string fault-id)
        result = retrieve locally ("fault:" + fault-id)

## interface

Record a boundary-crossing call (auto-inserted by the emitter):

    on record action (string feature, string name, string args, string correlation, string result, number elapsed)

Record a non-deterministic value (auto-inserted by the emitter):

    on record nondeterministic (string feature, string call, string value)

Freeze the buffer with a user comment, returns a fault ID:

    on (string fault) = report fault (string comment)

Upload pending fault reports:

    on upload pending faults ()

Get the build fingerprint:

    on (string fp) = build fingerprint ()

Retrieve a fault report:

    on (string result) = get fault (string fault-id)

## tests

Report fault returns an 8-character ID:

    length of (report fault ("test")) => 8
