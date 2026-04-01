// Platform implementation: http (TypeScript)
// Implements the streams and tasks declared in http.zero.md

import { createServer, IncomingMessage, ServerResponse } from 'http';

interface _PendingRequest {
    path: string;
    method: string;
    _send: (body: string) => void;
}

const _request_queue: _PendingRequest[] = [];
let _request_resolve: ((req: _PendingRequest) => void) | null = null;

function _enqueue_request(req: _PendingRequest): void {
    if (_request_resolve) {
        const resolve = _request_resolve;
        _request_resolve = null;
        resolve(req);
    } else {
        _request_queue.push(req);
    }
}

function _next_request(): Promise<_PendingRequest> {
    if (_request_queue.length > 0) {
        return Promise.resolve(_request_queue.shift()!);
    }
    return new Promise(resolve => { _request_resolve = resolve; });
}

// @zero on (http_request request$) <- serve http (int port)
async function* task_serve_http__int(port: number): AsyncGenerator<_PendingRequest> {
    createServer((req: IncomingMessage, res: ServerResponse) => {
        const pending: _PendingRequest = {
            path: req.url || "/",
            method: req.method || "GET",
            _send: (body: string) => {
                res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" });
                res.end(body);
            }
        };
        _enqueue_request(pending);
    }).listen(port);

    while (true) {
        yield await _next_request();
    }
}

// @zero http.response$
// Routes the response back to the correct client via the paired request
function _push_http_response(response: { request: any; body: string }): void {
    response.request._send(response.body);
}


// Platform implementation: io (TypeScript)
// Implements the functions declared in io.zero.md

import { readFileSync, writeFileSync } from 'fs';

// @zero on (string content) = read file (string path)
function fn_read_file__string(path: string): string {
    return readFileSync(path, 'utf8');
}

// @zero on write file (string path) (string content)
function fn_write_file__string__string(path: string, content: string): void {
    writeFileSync(path, content, 'utf8');
}

// @zero on print (string message)
function fn_print__string(message: string): void {
    console.log(message);
}


// Platform implementation: string (TypeScript)
// Implements the functions declared in string.zero.md


// @zero on (string result) = trim (string s)
function fn_trim__string(s: string): string {
    return s.trim();
}


// @zero on (char c) = char (int i) of (string s)
function fn_char__int_of__string(i: number, s: string): string {
    return s[i];
}


// @zero on (string result$) = split [string s] at [int positions$]
function fn_split_at(s: string, positions: readonly number[]): string[] {
    const parts: string[] = [];
    let start = 0;
    for (const pos of positions) {
        parts.push(s.slice(start, pos - 1));
        start = pos + 1;
    }
    const remainder = s.slice(start);
    if (remainder) parts.push(remainder);
    return parts;
}


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


interface http_request {
    readonly path: string;
    readonly method: string;
}

function http_request(args: Partial<http_request> = {}): http_request {
    return { path: args.path ?? "", method: args.method ?? "" };
}

interface http_response {
    readonly request: http_request;
    readonly body: string;
}

function http_response(args: Partial<http_response> = {}): http_response {
    return { request: args.request ?? http_request(), body: args.body ?? "" };
}

const logo: string = "ᕦ(ツ)ᕤ";

// @zero on main (string args$); website/website.zero.md:57
async function task_main__string(args_arr: readonly string[]): Promise<void> {
    _push_terminal_out(logo);
    const request_arr = task_serve_http__int(8084);
    for await (const request of request_arr) {
        _push_terminal_out(request.path);
        _push_http_response(http_response({ request: request, body: logo }));
    }
}


// Runtime harness: bridges OS to zero's main task
try {
    const result: unknown = task_main__string(process.argv.slice(2));
    if (result && typeof result === 'object') {
        if (Symbol.iterator in (result as object)) {
            for (const line of result as Iterable<string>) {
                console.log(line);
            }
        }
        if ('then' in (result as object)) {
            (result as Promise<void>).catch(() => {});
        }
    }
} catch (e) {
    // no main task defined
}

