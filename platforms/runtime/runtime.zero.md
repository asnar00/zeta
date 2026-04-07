# runtime
*feature variable management and RPC*

## specification

Runtime platform for introspecting and modifying the running system. Provides RPC evaluation of zero expressions and a directory listing of available features, variables, and functions.

## interface

A record of a function call from outside the system:

    type Call
        string name = ""
        string args = ""
        string result = ""

The input stream — every call to an `input`-tagged function pushes a Call here:

    Call input$

Evaluate a zero expression at runtime (get/set variables, call functions):

    on (string result) = rpc eval (string expr)

List all available variables and functions:

    on (string result) = functions ()

Show the feature tree with one-line summaries:

    on (string result) = features ()

Run all tests, or tests for a specific feature:

    on (string result) = test ()
    on (string result) = test (string feature)

Find or create a named session, returns a token. If the user has logged in before, their context is restored:

    on input (string token) = create session (string name)

Stream of random unsigned integers (pull one value per read):

    input uint random$

Generate a random string of digits of the given length (reads from random$):

    on (string result) = random digits (int n)

Switch the current request to a session's context by token:

    on set session (string token)

Terminate the process (with a short delay for response delivery):

    on exit process ()

Serialise a stream or array to a JSON string, including values and timing metadata:

    on (string json) = serialise [items$]

Deserialise a JSON string back into a stream or array:

    on (string result$) = deserialise (string json)

## tests

Random digits produces the requested length:

    length of (random digits (1)) => 1
    length of (random digits (4)) => 4
    length of (random digits (10)) => 10

Create session returns a non-empty token:

    length of (create session ("test")) => 8
