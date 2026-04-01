# login
*SMS code authentication*

## specification

Two-step login: request a code for a phone number, then verify the code. Returns the user on success. Codes are stored in a keyed collection. For testing, code generation is deterministic.

## interface

Request a code for a known user:

    request login ("+440001") => "1234"

Request a code for an unknown number:

    request login ("+449999") => "unknown"

Verify with the wrong code returns an empty user:

    verify login ("+440001") ("0000") => user()

## definition

    feature login extends website

A user has a name, phone number, and role:

    type user
        string name = ""
        string phone = ""
        string role = ""

The user database and pending verification codes:

    shared user users$ = [user(name="_alice", phone="+440001", role="admin"), user(name="_bob", phone="+440002", role="user")]
    shared string pending-codes$[string]

Request a login code for a phone number. Looks up the user, generates a code, and stores it. Returns "unknown" if the phone isn't in the user database:

    on (string code) = request login (string phone)
        user found = first of [users$] where (_.phone == phone)
        if (found.phone == phone)
            code = generate code (found)
            pending-codes$[phone] = code
        else
            code = "unknown"

Verify a code against the stored one. Returns the user on success, or an empty user on failure. Clears the code after use:

    on (user result) = verify login (string phone) (string code)
        string stored = pending-codes$[phone]
        if (stored == code and stored != "")
            pending-codes$[phone] = ""
            result = first of [users$] where (_.phone == phone)

Full login: verify the code and create a session if valid. Returns a session token or "invalid":

    on (string token) = login (string phone) (string code)
        user found = verify login (phone) (code)
        if (found.phone == phone)
            token = create session ()
        else
            token = "invalid"

Generate a deterministic code per user (for testing). In production, this would use `random number`:

    on (string code) = generate code (user u)
        if (u.name == "_alice")
            code = "1234"
        else if (u.name == "_bob")
            code = "4321"
        else
            code = "1234"
