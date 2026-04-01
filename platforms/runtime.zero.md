# runtime
*feature variable management*

## specification

Runtime platform for reading and writing feature-scoped variables. Used by the admin interface to toggle dynamic features at runtime.

## interface

    on exit process ()
    on (string result) = rpc eval (string expr)
    on (string result) = directory ()
