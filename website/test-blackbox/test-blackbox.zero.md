# test-blackbox
*integration tests for the flight recorder*

## specification

Tests the blackbox flight recorder end-to-end: records actions, reports a fault, verifies the fault report contains the recorded trace, replays the fault, and verifies the replay produces an identical recording.

## definition

    feature test-blackbox extends blackbox

Check that a string contains expected text, raise if not:

    on bb check (string actual) contains (string expected)
        bool found = (actual) contains (expected)
        if (found == false)
            raise bb check failed (expected)

    on bb check failed (string what)
        print ("FAIL: expected " + what)

Test recording — inject actions, report a fault, verify the trace contains the actions:

    on test blackbox ()
        inject call ("click on") with (".logo") result ("ok")
        inject call ("press") with ("Escape") result ("ok")
        string report$ <- report fault ("test: logo did something weird")
        bb check (report$) contains ("comment")
        bb check (report$) contains ("trace")
        bb check (report$) contains ("test: logo did something weird")
        bb check (report$) contains ("click on")
        bb check (report$) contains ("Escape")

Test replay — replay a fault report and verify the replayed actions appear in the new recording:

    on test replay ()
        inject call ("click on") with (".logo") result ("ok")
        inject call ("press") with ("Escape") result ("ok")
        string report1$ <- report fault ("replay source")
        replay fault (report1$)
        string report2$ <- report fault ("replay verify")
        bb check (report2$) contains ("click on")
        bb check (report2$) contains ("Escape")
