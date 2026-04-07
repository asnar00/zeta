# websocket
*persistent bidirectional communication*

## specification

Provides a persistent channel between components (client and server). Used by the compiler-generated middleware for cross-component function calls. User code does not call these directly — the compiler generates the plumbing.

## interface

Open a WebSocket connection on a path:

    on input (string channel) = open channel (string path)

Send a message on a channel:

    on send message (string data) on (string channel)

Wait for and return the next message on a channel:

    on input (string data) = receive message on (string channel)

Close a channel:

    on close channel (string channel)

## integration tests

Open a channel, send a message, receive it back:

    open channel ("/@ws")
    check channel is connected
    send message ("hello") on channel
    check server received "hello"
    server sends message ("world") on channel
    check receive message on channel is "world"
    close channel
    check channel is disconnected

Multiple channels are independent:

    open channel "a" on ("/@ws")
    open channel "b" on ("/@ws")
    send message ("for a") on channel "a"
    send message ("for b") on channel "b"
    check server received "for a" on channel "a"
    check server received "for b" on channel "b"
    close channel "a"
    close channel "b"

Channel survives across multiple message exchanges:

    open channel ("/@ws")
    send message ("one") on channel
    check server received "one"
    send message ("two") on channel
    check server received "two"
    send message ("three") on channel
    check server received "three"
    close channel
