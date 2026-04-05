# eval
*expression evaluation and function dispatch*

## specification

Every component can evaluate zero expressions at runtime. This platform provides the machinery: parse a zero expression string, match it against the component's registered functions, call the function, and return the result as a string.

Also provides introspection: list available functions and features on the current component.

## interface

Evaluate a zero expression and return the result:

    on (string result) = eval (string expr)

List all available functions on this component:

    on (string result) = functions ()

List the feature tree on this component:

    on (string result) = features ()

## integration tests

Evaluate simple expressions on the server:

    ... start server
    string channel = connect to server
    request ("eval (\"port\")") on (channel) => "8084"
    request ("eval (\"not found ()\")") on (channel) => "not found"
    request ("eval (\"trim (\\\"  hello  \\\")\")") on (channel) => "hello"

Evaluate simple expressions on the client:

    ... start server with browser
    string channel = connect to browser
    request ("eval (\"get cookie (\\\"session\\\")\")") on (channel) => ""

List functions on the server:

    string channel = connect to server
    string result = request ("functions ()") on (channel)
    check result contains "handle request"
    check result contains "not found"

List functions on the client:

    string channel = connect to browser
    string result = request ("functions ()") on (channel)
    check result contains "login"
    check result contains "input"
    check result contains "toggle login"
