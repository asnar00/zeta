# blackbox
*flight recorder for fault diagnosis*

## specification

Records all input streams into a circular buffer. When a user reports a fault, the buffer is frozen and serialised for upload and replay.

The blackbox is a feature — compose it in to enable recording, remove it to disable. No compiler knowledge required.

## definition

    feature blackbox extends website

    use runtime.input$

The action type — a record of something that entered from outside:

    type Action =
        string source = ""
        string name = ""
        string args = ""
        string result = ""

Convert a Call (from the runtime input stream) to an Action:

    on (Action a) <- (Call c)
        a = Action(source = c.name, name = c.name, args = c.args, result = c.result)

The action stream — a sparse circular buffer recording all input activity:

    shared Action action$(capacity = (60) seconds)
    action$ <- input$

Freeze the buffer and return a serialised fault report:

    on (string report$) <- report fault (string comment)
        Action trace$ = snapshot [action$]
        string json = serialise [trace$]
        report$ <- "{\"comment\":\"" + comment + "\",\"trace\":" + json + "}"

Retrieve a stored fault report:

    on (string data) = get fault (string id)
        data = retrieve locally ("fault:" + id)

Extract the trace JSON from a fault report envelope:

    on (string json) = extract trace (string report)
        int start = index of ("\"trace\":") in (report)
        if (start < 0)
            json = ""
        else
            json = substring of (report) from (start + 8) to (length of (report) - 1)

Replay a fault report — deserialise its trace and feed it back through input$ with original timing:

    on replay fault (string report)
        string trace_json = extract trace (report)
        Action trace$ = deserialise (trace_json)
        replay with timing [trace$]
