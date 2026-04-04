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

The interactive login flow — input name, request a code, input code, log in:

    on login ()
        string name = input ("name")
        string code = request login (name)
        string entered = input ("code")
        string token = complete login (name) (entered)
        set cookie of ("session") to (token)
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
        send sms (found.phone) ("Your nøøb code: " + code)
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
        login ()

## integration tests

Three browser tabs, three users, three background colours.

Open a default tab (no login). It should show the landing page with the default teal background (#34988b):

    open tab "default"
    navigate to "/"
    check background colour is "#34988b"

Open a second tab. Click the logo, log in as _alice, set her background to red:

    open tab "alice"
    navigate to "/"
    click logo
    type "_alice" into input
    submit
    type "1234" into input
    submit
    set background colour to "#ff0000"
    check background colour is "#ff0000"

Open a third tab. Click the logo, log in as _bob, set his background to blue:

    open tab "bob"
    navigate to "/"
    click logo
    type "_bob" into input
    submit
    type "4321" into input
    submit
    set background colour to "#0000ff"
    check background colour is "#0000ff"

Switch back to the default tab. It should still be teal:

    switch to tab "default"
    check background colour is "#34988b"

Switch to alice's tab. It should still be red:

    switch to tab "alice"
    check background colour is "#ff0000"

Switch to bob's tab. It should still be blue:

    switch to tab "bob"
    check background colour is "#0000ff"
