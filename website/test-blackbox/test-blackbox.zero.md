# test-blackbox
*integration tests for the flight recorder*

## specification

Tests the blackbox flight recorder end-to-end: records user actions on the client, reports a fault, uploads it to the server, and verifies the fault report contains moments and actions.

## definition

    feature test-blackbox extends website

Check that a string contains expected text, raise if not:

    on bb check (string actual) contains (string expected)
        bool found = (actual) contains (expected)
        if (found == false)
            raise bb check failed (expected)

    on bb check failed (string what)
        print ("FAIL: expected " + what)

Test the full blackbox flow — report a fault from the browser and verify it reaches the server:

    on test blackbox ()
        click on (".logo")
        press ("Escape") on ("body")
        string fault = report fault ("test: logo did something weird")
        upload pending faults ()
        string data = get fault (fault)
        bb check (data) contains ("fault_id")
        bb check (data) contains ("moments")
        bb check (data) contains ("test: logo did something weird")
