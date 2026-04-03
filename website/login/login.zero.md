# login
*SMS code authentication*

## specification

Two-step login: request a code for a user name, then verify the code. Returns the user on success. Codes are stored in a keyed collection. For testing, code generation is deterministic.

## interface

Request a code for a known user:

    request login ("_alice") => "1234"

Request a code for an unknown name:

    request login ("nobody") => "unknown"

Verify with the wrong code returns an empty user:

    verify login ("_alice") ("0000") => User()

## definition

    feature login extends website

A user has a name, phone number, and role:

    type User
        string name = ""
        string phone = ""
        string role = ""

The user database and pending verification codes live on the server:

    shared User users$ = [User(name="_alice", phone="+440001", role="admin"), User(name="_bob", phone="+440002", role="user")]
    shared string pending-codes$[string]

Request a login code by name. Looks up the user, generates a code, and stores it:

    on (string code) = request login (string name)
        User found = first of [users$] where (_.name == name)
        if (found.name == name)
            code = generate code (found)
            pending-codes$[found.phone] = code
        else
            code = "unknown"

Verify a code against the stored one. Returns the user on success, or an empty user on failure:

    on (User result) = verify login (string name) (string code)
        User found = first of [users$] where (_.name == name)
        string stored = pending-codes$[found.phone]
        if (found.name == name and stored == code and stored != "")
            pending-codes$[found.phone] = ""
            result = found

Full login: verify the code and create a session if valid. Returns a session token or "invalid":

    on (string token) = login (string name) (string code)
        User found = verify login (name) (code)
        if (found.name != "")
            token = create session ()
        else
            token = "invalid"

Generate a deterministic code per user (for testing):

    on (string code) = generate code (User u)
        if (u.name == "_alice")
            code = "1234"
        else if (u.name == "_bob")
            code = "4321"
        else
            code = "1234"
