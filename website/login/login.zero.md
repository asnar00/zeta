# login
*SMS code authentication*

## specification

Adds user authentication to the website. The user clicks the logo, enters their name, receives a verification code, and enters the code to log in. On success, a session is created and the page reloads with the user's per-session context (background colour, feature flags, etc.).

The login flow is two-step: first request a code (looked up by name from a shared user database), then verify the code. Codes are stored in a keyed collection and cleared after use. For testing, code generation is deterministic — `_alice` always gets `1234`, `_bob` always gets `4321`.

## interface

The `login` function runs the interactive login flow — inputs for a name, requests a code, inputs for the code, and creates a session on success:

    login ()

## definition

    feature login extends website

A user has a name, phone number, and role:

    type User
        string name = ""
        string phone = ""
        string role = ""

The user database and pending verification codes:

    shared User users$ = [User(name="_alice", phone="+440001", role="admin"), User(name="_bob", phone="+440002", role="user")]
    shared string pending-codes$[string]

The interactive login flow — input name, request a code, input code, log in:

    on login ()
        string name = input ("name")
        string code = request login (name)
        string entered = input ("code")
        string token = complete login (name) (entered)
        set cookie of ("session") to (token)
        reload page ()

Request a login code by name. Looks up the user, generates a code, and stores it:

    on (string code) = request login (string name)
        User found = first of [users$] where (_.name == name)
        if (found.name != name)
            raise unknown user (name)
        code = generate code (found)
        ... send (code) to (found)
        pending-codes$[found.phone] = code

Verify a code against the stored one. Returns the user on success, raises on failure:

    on (User result) = verify login (string name) (string code)
        User found = first of [users$] where (_.name == name)
        string stored = pending-codes$[found.phone]
        if (found.name != name or stored != code or stored == "")
            raise invalid code (code)
        pending-codes$[found.phone] = ""
        result = found

Complete login: verify the code and create a session:

    on (string token) = complete login (string name) (string code)
        User found = verify login (name) (code)
        token = create session ()

Generate a deterministic code per user (for testing):

    on (string code) = generate code (User u)
        if (u.name == "_alice")
            code = "1234"
        else if (u.name == "_bob")
            code = "4321"
        else
            code = "1234"

Wire login into the landing page — when the logo is clicked, start the login flow:

    on logo clicked ()
        login ()
