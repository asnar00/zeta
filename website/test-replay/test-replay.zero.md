# test-replay
*round-trip test for blackbox replay*

## specification

Tests the blackbox replay: records actions, reports a fault, replays the fault trace, records again, and verifies the replayed actions appear in the new recording.

## definition

    feature test-replay extends blackbox

Test the round-trip: record, report, replay, record again, compare:

    on test replay ()
        inject call ("click on") with (".logo") result ("ok")
        inject call ("press") with ("Escape") result ("ok")
        string report1$ <- report fault ("replay source")
        replay fault (report1$)
        string report2$ <- report fault ("replay verify")
        bb check (report2$) contains ("click on")
        bb check (report2$) contains ("Escape")
