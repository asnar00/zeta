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

    type user
        string name = ""
        string phone = ""
        string role = ""

    shared user users$ = [user(name="_alice", phone="+440001", role="admin"), user(name="_bob", phone="+440002", role="user")]
    shared string pending-codes$[string]

    on (string code) = request login (string phone)
        user found = first of [users$] where (_.phone == phone)
        if (found.phone == phone)
            code = generate code (found)
            pending-codes$[phone] = code
        else
            code = "unknown"

    on (user result) = verify login (string phone) (string code)
        string stored = pending-codes$[phone]
        if (stored == code and stored != "")
            pending-codes$[phone] = ""
            result = first of [users$] where (_.phone == phone)

    on (string token) = login (string phone) (string code)
        user found = verify login (phone) (code)
        if (found.phone == phone)
            token = create session ()
        else
            token = "invalid"

    on (string code) = generate code (user u)
        if (u.name == "_alice")
            code = "1234"
        else if (u.name == "_bob")
            code = "4321"
        else
            code = "1234"
