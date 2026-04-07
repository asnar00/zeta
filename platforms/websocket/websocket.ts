// Platform implementation: websocket (TypeScript)
// Implements the functions declared in websocket.zero.md
// Server-side stub — the real client implementation lives in the client bundle.

// @zero on input (string channel$) <- open channel (string path)
function* task_open_channel__string(path: string): Generator<string> {
    yield "";
}

// @zero on send message (string data) on (string channel)
function fn_send_message__string_on__string(data: string, channel: string): void {
}

// @zero on input (string data$) <- receive message on (string channel)
function* task_receive_message_on__string(channel: string): Generator<string> {
    yield "";
}

// @zero on close channel (string channel)
function fn_close_channel__string(channel: string): void {
}
