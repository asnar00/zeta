# runtime
*feature variable management and RPC*

## specification

Runtime platform for introspecting and modifying the running system. Provides RPC evaluation of zero expressions and a directory listing of available features, variables, and functions.

## interface

Evaluate a zero expression at runtime (get/set variables, call functions):

    on (string result) = rpc eval (string expr)

List all available variables and functions:

    on (string result) = functions ()

Show the feature tree with one-line summaries:

    on (string result) = features ()

Run all tests, or tests for a specific feature:

    on (string result) = test ()
    on (string result) = test (string feature)

Create a new session context, returns a token:

    on (string token) = create session ()

Generate a random string of digits of the given length:

    on (string result) = random digits (int n)

Switch the current request to a session's context by token:

    on set session (string token)

Terminate the process (with a short delay for response delivery):

    on exit process ()

## tests

Random digits produces the requested length:

    length of (random digits (1)) => 1
    length of (random digits (4)) => 4
    length of (random digits (10)) => 10

Create session returns a non-empty token:

    length of (create session ()) => 8
