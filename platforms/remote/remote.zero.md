# remote
*cross-component function calls and state queries*

## specification

Every component can receive and evaluate zero expressions. Any component can send a zero expression to any other component via a channel, and receive the result. This unifies RPC (HTTP-based server calls) and remote (WebSocket-based cross-component calls) into one mechanism.

Each component has:
- A function registry (the compiled functions it contains)
- A minimal expression parser (parses zero call syntax into function dispatch)
- A `features ()` and `functions ()` listing (so any component can be interrogated)

## interface

Connect to another component and return a channel:

    on input (string channel) = connect to (string url)

Send a zero expression to a component and wait for the result:

    on input (string result) = request (string command) on (string channel)

Disconnect from a component:

    on disconnect from (string channel)

## integration tests

Connect to the server, evaluate a ping:

    ... start server
    string channel = connect to ("wss://test.xn--nb-lkaa.org/@ws")
    string result = request ("ping") on (channel)
    result => "pong"
    disconnect from (channel)

Server-side: evaluate zero expressions via remote (same as RPC):

    string channel = connect to server
    request ("port") on (channel) => "8084"
    request ("not found ()") on (channel) => "not found"
    request ("trim (\"  hello  \")") on (channel) => "hello"
    disconnect from (channel)

Client-side: evaluate zero expressions on the browser component:

    ... start server with browser
    string channel = connect to browser
    request ("get cookie (\"session\")") on (channel) => ""
    request ("functions ()") on (channel)
    check result contains "login"
    check result contains "input"

Client-side: query DOM state via zero expressions:

    string channel = connect to browser
    ... navigate to "/"
    request ("style (\"background-color\") of (\"body\")") on (channel)
    check result contains "rgb"

Client-side: trigger actions via zero expressions:

    string channel = connect to browser
    ... navigate to "/"
    request ("click on (\".logo\")") on (channel) => "ok"
    request ("element (\"input\") exists") on (channel) => "true"

Multiple requests on the same channel:

    string channel = connect to server
    string a = request ("echo:hello") on (channel)
    a => "hello"
    string b = request ("echo:world") on (channel)
    b => "world"
    disconnect from (channel)

Requests are independent across channels:

    string ch1 = connect to server
    string ch2 = connect to server
    string r1 = request ("echo:one") on (ch1)
    string r2 = request ("echo:two") on (ch2)
    r1 => "one"
    r2 => "two"
    disconnect from (ch1)
    disconnect from (ch2)
