# test-blackbox
*integration tests for the flight recorder*

## specification

Tests the blackbox flight recorder end-to-end: records user actions, reports a fault, and verifies the fault report contains the recorded trace.

## definition

    feature test-blackbox extends blackbox

Check that a string contains expected text, raise if not:

    on bb check (string actual) contains (string expected)
        bool found = (actual) contains (expected)
        if (found == false)
            raise bb check failed (expected)

    on bb check failed (string what)
        print ("FAIL: expected " + what)

Test the full blackbox flow — do some actions, report a fault, check the trace:

    on test blackbox ()
        click on (".logo")
        press ("Escape") on ("body")
        string report$ <- report fault ("test: logo did something weird")
        bb check (report$) contains ("comment")
        bb check (report$) contains ("trace")
        bb check (report$) contains ("test: logo did something weird")
