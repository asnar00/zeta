# blackbox
*local persistence primitives*

## specification

Thin OS primitives for local key-value storage, timers, and elapsed time. Used by the blackbox flight recorder feature and other features that need local persistence.

## interface

Milliseconds since recording started:

    input number elapsed$

Call a function repeatedly at an interval. Returns a timer ID:

    on (string timer) = every (number ms) do (string callback)

Cancel a repeating timer:

    on cancel timer (string timer)

Persist a string locally by key (survives app restart):

    on store locally (string key, string value)

Retrieve a persisted string (empty string if not found):

    on (string value) = retrieve locally (string key)

List all keys matching a prefix:

    on (string result) = stored keys (string prefix)

Remove a persisted key:

    on remove locally (string key)
