// Platform implementation: remote (TypeScript)
// Implements the functions declared in remote.zero.md
// Server-side stub.

// @zero on (string channel) = connect to (string url)
function fn_connect_to__string(url: string): string {
    return "";
}

// @zero on (string result) = request (string command) on (string channel)
function fn_request__string_on__string(command: string, channel: string): string {
    return "";
}

// @zero on disconnect from (string channel)
function fn_disconnect_from__string(channel: string): void {
}

// @zero on (string result) = handle remote request (string command)
function fn_handle_remote_request__string(command: string): string {
    if (command === "ping") return "pong";
    if (command.startsWith("echo:")) return command.slice(5);
    return `error: unknown command: ${command}`;
}
