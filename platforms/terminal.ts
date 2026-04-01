// Platform implementation: terminal (TypeScript)
// Implements the streams declared in terminal.zero.md

// @zero string out$
// Subscription: each value pushed to out$ prints to stdout
function _push_terminal_out(value: string): void {
    console.log(value);
}

// @zero string in$
// Yields lines from stdin (not implemented for server context)
function* terminal_in(): Generator<string> {
    // stdin reading requires async in Node — stub for now
}
