# runtime
*feature variable management and RPC*

## specification

Runtime platform for introspecting and modifying the running system. Provides RPC evaluation of zero expressions and a directory listing of available features, variables, and functions.

## interface

Evaluate a zero expression at runtime (get/set variables, call functions):

    on (string result) = rpc eval (string expr)

List all available features, variables, and functions:

    on (string result) = directory ()

Terminate the process (with a short delay for response delivery):

    on exit process ()
