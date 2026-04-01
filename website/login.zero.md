# login
*SMS code authentication*

## specification

Two-step login: request a code for a phone number, then verify the code to create a session. The session token is returned to the caller, who sends it as a cookie on subsequent requests. When a request carries a token, the session's context is loaded.

## interface

Request a login code (for now, returns the code directly for testing):

    request login ("+44 7700 900000") => "1234"

Verify the code and get a session token (returns the token string on success, "invalid code" on failure):

    verify login ("+44 7700 900000") ("0000") => "invalid code"

## definition

    feature login extends website

    on (string code) = request login (string phone)
        code = generate code ()
        store code (code) for (phone)

    on (string result) = verify login (string phone) (string code)
        bool valid = check code (code) for (phone)
        if (valid)
            string token = create session ()
            result = token
        else
            result = "invalid code"

    on (string code) = generate code ()
        code = "1234"

    on store code (string code) for (string phone)
        print (phone + ":" + code)

    on (bool valid) = check code (string code) for (string phone)
        valid = (code == "1234")
