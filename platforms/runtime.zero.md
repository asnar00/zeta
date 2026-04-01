# runtime
*feature variable management*

## specification

Runtime platform for reading and writing feature-scoped variables. Used by the admin interface to toggle dynamic features at runtime.

## interface

    on set feature var (string name) (string value)
    on (string value) = get feature var (string name)
