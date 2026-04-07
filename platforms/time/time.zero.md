# time
*time units and conversions*

## specification

Unit functions for constructing `time` values. A `time` value represents a duration (the interval between two events, or the period of a repeating signal). The internal representation is platform-specific; zero code uses these functions to construct time values from human-readable units.

## interface

Seconds — the base unit:

    on (time t) = (number n) seconds

    (1) seconds => 1
    (0.5) seconds => 0.5

Milliseconds:

    on (time t) = (number n) ms

    (1000) ms => 1
    (500) ms => 0.5

Hertz — frequency to period:

    on (time t) = (number n) hz

    (1) hz => 1
    (10) hz => 0.1

Beats per minute — musical tempo to period:

    on (time t) = (number n) bpm

    (60) bpm => 1
    (120) bpm => 0.5

Get the current wall-clock time:

    on input (time t) = now ()

Get the sample interval of a stream:

    on (time t) = dt of [items$]

Get the capacity of a stream:

    on (time t) = capacity of [items$]

Get the start time of a stream:

    on (time t) = t0 of [items$]

Take a static copy of a stream's current contents, preserving timing:

    on (int result$) = snapshot [items$]
