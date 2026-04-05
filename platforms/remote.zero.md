# remote
*cross-component function calls and state queries*

## specification

Lets any component send commands to and query state from any other component. Built on top of the WebSocket platform. The caller sends a command string and waits for a result. The receiver interprets the command via `handle remote request`.

Components connect to each other via `connect to`. Each connection is a channel. Commands are sent with `request ... on` and handled by the receiver's `handle remote request` function.

## interface

Connect to another component and return a channel:

    on (string channel) = connect to (string url)

Send a command to a component and wait for the result:

    on (string result) = request (string command) on (string channel)

Disconnect from a component:

    on disconnect from (string channel)

Handle an incoming command (implemented by each component):

    on (string result) = handle remote request (string command)

## integration tests

Connect to the server, send a request, receive a response:

    ... start server
    string channel = connect to ("wss://test.xn--nb-lkaa.org/@ws")
    string result = request ("ping") on (channel)
    result => "pong"
    disconnect from (channel)

Multiple requests on the same channel:

    string channel = connect to ("wss://test.xn--nb-lkaa.org/@ws")
    string a = request ("echo:hello") on (channel)
    a => "hello"
    string b = request ("echo:world") on (channel)
    b => "world"
    disconnect from (channel)

Requests are independent across channels:

    string ch1 = connect to ("wss://test.xn--nb-lkaa.org/@ws")
    string ch2 = connect to ("wss://test.xn--nb-lkaa.org/@ws")
    string r1 = request ("echo:one") on (ch1)
    string r2 = request ("echo:two") on (ch2)
    r1 => "one"
    r2 => "two"
    disconnect from (ch1)
    disconnect from (ch2)

The default handler responds to ping and echo commands:

    connect to server
    request "ping" on channel
    check result is "pong"
    request "echo:hello" on channel
    check result is "hello"
    disconnect

Query browser DOM state via remote:

    connect to browser
    request "style:background-color:body" on channel
    check result contains "rgb"
    disconnect

Send a browser action via remote:

    connect to browser
    request "click:.logo" on channel
    check: input element appears
    disconnect
