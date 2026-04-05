# websocket
*persistent bidirectional communication*

## specification

Provides a persistent channel between components (client and server). Used by the compiler-generated middleware for cross-component function calls. User code does not call these directly — the compiler generates the plumbing.

## interface

Open a WebSocket connection on a path:

    on (string channel) = open channel (string path)

Send a message on a channel:

    on send message (string data) on (string channel)

Wait for and return the next message on a channel:

    on (string data) = receive message on (string channel)

Close a channel:

    on close channel (string channel)
