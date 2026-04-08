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
        string report <- "{\"comment\":\"" <- comment <- "\",\"trace\":" <- json <- "}"

Retrieve a stored fault report:

    on (string data) = get fault (string id)
        data = retrieve locally ("fault:" + id)
