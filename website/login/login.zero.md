# login
*SMS code authentication*

## specification

Adds user authentication to the website. The user clicks the logo, enters their name, receives a verification code, and enters the code to log in. On success, a session is created and the page reloads with the user's per-session context (background colour, feature flags, etc.).

The login flow is two-step: first request a code (looked up by name from a shared user database), then verify the code. Codes are stored in a keyed collection and cleared after use.

## interface

The `login` function runs the interactive login flow — inputs for a name, requests a code, inputs for the code, and creates a session on success.

## definition

    feature login extends website

A user has a name, phone number, and role:

    type User
        string name = ""
        string phone = ""
        string role = ""

The user database and pending verification codes:

    shared User users$ = [User(name="_alice", phone="+440001", role="admin"), User(name="_bob", phone="+440002", role="user"), User(name="ash", phone="+447813943023", role="admin")]
    shared string pending-codes$[string]

Toggle between login and logout based on session state:

    on toggle login ()
        string session = cookie$["session"]
        if (session == "")
            login ()
        else
            logout dialog ()

The interactive login flow — input name, request a code, input code, log in:

    on login ()
        string name$ <- input ("name")
        string code$ <- request login (name$)
        string entered$ <- input ("code")
        string token$ <- complete login (name$) with code (entered$)
        set cookie of ("session") to (token$)
        reload page ()

Log out — confirm with the user, then clear the session cookie:

    on logout dialog ()
        string choice$ <- choose ("log out") or ("cancel")
        if (choice$ == "log out")
            clear cookie ("session")
            reload page ()

Handle errors — these are regular functions, called when a raise occurs inside login:

    on unknown user (string name)
        show message ("unknown user")

    on invalid code (string code)
        show message ("invalid code")

Catch raises inside login and dispatch to the handlers:

    in login (), on unknown user (string name)
    in login (), on invalid code (string code)

Request a login code by name. Looks up the user, generates a code, and stores it:

    on (string code) = request login (string name)
        User found = first of [users$] where (_.name == name)
        if (found.name != name)
            raise unknown user (name)
        code = generate code (found)
        send sms ("Your nøøb code: " + code) to (found.phone)
        pending-codes$[found.phone] = code

Verify a code against the stored one. Returns the user on success, raises on failure:

    on (User result) = verify login (string name) with code (string code)
        User found = first of [users$] where (_.name == name)
        string stored = pending-codes$[found.phone]
        if (found.name != name or stored != code or stored == "")
            raise invalid code (code)
        pending-codes$[found.phone] = ""
        result = found

Complete login: verify the code and find or create the user's session:

    on (string token) = complete login (string name) with code (string code)
        User found = verify login (name) with code (code)
        token = create session (name)

Generate a code per user. Test users get deterministic codes; real users get random ones:

    on (string code) = generate code (User u)
        if (u.name == "_alice")
            code = "1234"
        else if (u.name == "_bob")
            code = "4321"
        else
            code = random digits (4)

Wire login into the landing page — when the logo is clicked, start the login flow:

    on logo clicked ()
        toggle login ()

Check that a snapshot contains expected text, raise if not:

    on check (string snapshot) contains (string expected)
        bool found = (snapshot) contains (expected)
        if (found == false)
            raise check failed (expected)

    on check failed (string what)
        print ("FAIL: expected " + what)

Test the login flow — hooks intercept input calls to simulate the user:

    on test login ()
        in login (), on input ("name")
            type ("_alice") into input box ("input")
            press ("Enter") on ("input")
        in login (), on input ("code")
            type ("1234") into input box ("input")
            press ("Enter") on ("input")
        login ()
        check (describe page ()) contains ("log out")
