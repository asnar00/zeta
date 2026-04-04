// Platform implementation: gui (TypeScript/web)
// Implements the functions declared in gui.zero.md

// @zero on (string result) = input (string prompt)
export function fn_input__string(prompt: string): string {
    // For now, use window.prompt — a proper implementation would
    // create a styled input element in the DOM and await submission
    const result = (globalThis as any).prompt?.(prompt) ?? "";
    return result;
}

// @zero on set cookie of (string name) to (string value)
export function fn_set_cookie_of__string_to__string(name: string, value: string): void {
    if (typeof document !== "undefined") {
        document.cookie = `${name}=${value}; path=/; SameSite=Strict`;
    }
}

// @zero on reload page ()
export function fn_reload_page(): void {
    if (typeof location !== "undefined") {
        location.reload();
    }
}


// Platform implementation: http (TypeScript)
// Implements the streams and tasks declared in http.zero.md

import { createServer, IncomingMessage, ServerResponse } from 'http';

interface _PendingRequest {
    path: string;
    method: string;
    token: string;
    _send: (body: string) => void;
}

const _request_queue: _PendingRequest[] = [];
let _request_resolve: ((req: _PendingRequest) => void) | null = null;

export function _enqueue_request(req: _PendingRequest): void {
    if (_request_resolve) {
        const resolve = _request_resolve;
        _request_resolve = null;
        resolve(req);
    } else {
        _request_queue.push(req);
    }
}

export function _next_request(): Promise<_PendingRequest> {
    if (_request_queue.length > 0) {
        return Promise.resolve(_request_queue.shift()!);
    }
    return new Promise(resolve => { _request_resolve = resolve; });
}

// @zero on (http_request request$) <- serve http (int port)
export async function* task_serve_http__int(port: number): AsyncGenerator<_PendingRequest> {
    createServer((req: IncomingMessage, res: ServerResponse) => {
        // extract session token from cookie
        let token = "";
        const cookie = req.headers.cookie || "";
        for (const part of cookie.split(";")) {
            const trimmed = part.trim();
            if (trimmed.startsWith("session=")) {
                token = trimmed.slice(8);
            }
        }
        const pending: _PendingRequest = {
            path: req.url || "/",
            method: req.method || "GET",
            token,
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
export function _push_http_response(response: { request: any; body: string }): void {
    response.request._send(response.body);
}


// Platform implementation: io (TypeScript)
// Implements the functions declared in io.zero.md

import { readFileSync, writeFileSync } from 'fs';

// @zero on (string content) = read file (string path)
export function fn_read_file__string(path: string): string {
    return readFileSync(path, 'utf8');
}

// @zero on write file (string path) (string content)
export function fn_write_file__string__string(path: string, content: string): void {
    writeFileSync(path, content, 'utf8');
}

// @zero on print (string message)
export function fn_print__string(message: string): void {
    console.log(message);
}


// Platform implementation: runtime (TypeScript)
// Implements the functions declared in runtime.zero.md

const _sessions: Map<string, any> = new Map();

// @zero on (string token) = create session ()
export function fn_create_session(): string {
    const token = Math.random().toString(36).slice(2, 10);
    // _Context will be available in the compiled output
    const ctx = new (globalThis as any)._Context();
    _sessions.set(token, ctx);
    return token;
}

// @zero on set session (string token)
export function fn_set_session__string(token: string): void {
    const ctx = _sessions.get(token);
    if (ctx && typeof (globalThis as any)._ctx_storage?.run === 'function') {
        // note: AsyncLocalStorage.run needs to wrap the request handler
        // for now, store as the default fallback
        (globalThis as any)._default_ctx = ctx;
    }
}

// @zero on exit process ()
export function fn_exit_process(): void {
    setTimeout(() => process.exit(0), 500);
}

// @zero on (string result) = rpc eval (string expr)
export function fn_rpc_eval__string(expr: string): string {
    return "error: rpc eval not implemented for TypeScript";
}


// Platform implementation: string (TypeScript)
// Implements the functions declared in string.zero.md


// @zero on (string result) = trim (string s)
export function fn_trim__string(s: string): string {
    return s.trim();
}


// @zero on (char c) = char (int i) of (string s)
export function fn_char__int_of__string(i: number, s: string): string {
    return s[i];
}


// @zero on (string result$) = split [string s] at [int positions$]
export function fn_split_at(s: string, positions: readonly number[]): string[] {
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


// @zero on (bool result) = (string s) starts with (string prefix)
export function fn__string_starts_with__string(s: string, prefix: string): boolean {
    return s.startsWith(prefix);
}


// @zero on (string result$) = split (string s) by (string delim)
export function fn_split__string_by__string(s: string, delim: string): string[] {
    return s.split(delim);
}


// @zero on (string result) = replace (string needle) in (string s) with (string replacement)
export function fn_replace__string_in__string_with__string(needle: string, s: string, replacement: string): string {
    return s.split(needle).join(replacement);
}


// @zero on (int n) = length of (string s)
export function fn_length_of__string(s: string): number {
    return s.length;
}


// @zero on (string sub) = substring of (string s) from (int start)
export function fn_substring_of__string_from__int(s: string, start: number): string {
    return s.slice(start);
}


// Platform implementation: terminal (TypeScript)
// Implements the streams declared in terminal.zero.md

// @zero string out$
// Subscription: each value pushed to out$ prints to stdout
export function _push_terminal_out(value: string): void {
    console.log(value);
}

// @zero string in$
// Yields lines from stdin (not implemented for server context)
export function* terminal_in(): Generator<string> {
    // stdin reading requires async in Node — stub for now
}


import { AsyncLocalStorage } from 'async_hooks';

class _Ctx_background {
    colour: string = "#34988b";
}

class _Ctx_landing_page {
    enabled: boolean = true;
}

class _Context {
    background = new _Ctx_background();
    landing_page = new _Ctx_landing_page();
}

const _ctx_storage = new AsyncLocalStorage<_Context>();
const _default_ctx = new _Context();

export function _get_ctx(): _Context {
    return _ctx_storage.getStore() ?? _default_ctx;
}


class _ZeroRaise extends Error {
    zeroName: string;
    argsList: any[];
    constructor(name: string, args: any[] = []) {
        super(`${name}(${args.join(', ')})`);
        this.zeroName = name;
        this.argsList = args;
    }
}

const users_arr: readonly User[] = [User({ name: "_alice", phone: "+440001", role: "admin" }), User({ name: "_bob", phone: "+440002", role: "user" })];
const pending_codes_arr: Map<string, string> = new Map();

interface Http_Request {
    readonly path: string;
    readonly method: string;
    readonly token: string;
}

export function Http_Request(args: Partial<Http_Request> = {}): Http_Request {
    return { path: args.path ?? "", method: args.method ?? "", token: args.token ?? "" };
}

interface Http_Response {
    readonly request: Http_Request;
    readonly body: string;
}

export function Http_Response(args: Partial<Http_Response> = {}): Http_Response {
    return { request: args.request ?? Http_Request(), body: args.body ?? "" };
}

interface User {
    readonly name: string;
    readonly phone: string;
    readonly role: string;
}

export function User(args: Partial<User> = {}): User {
    return { name: args.name ?? "", phone: args.phone ?? "", role: args.role ?? "" };
}

// @zero on login; website/login/login.zero.md:152
export function fn_login(): void {
    try {
        const name = fn_input__string("name");
        const code = fn_request_login__string(name);
        const entered = fn_input__string("code");
        const token = fn_complete_login__string__string(name, entered);
        fn_set_cookie_of__string_to__string("session", token);
        fn_reload_page();
    } catch (_e) {
        if (_e instanceof _ZeroRaise) {
            if (_e.zeroName === 'unknown user') {
                fn_unknown_user__string(_e.argsList[0]);
            } else if (_e.zeroName === 'invalid code') {
                fn_invalid_code__string(_e.argsList[0]);
            } else { throw _e; }
        } else { throw _e; }
    }
}

// @zero on unknown user (string name); website/login/login.zero.md:160
export function fn_unknown_user__string(name: string): void {
    fn_print__string("unknown user");
}

// @zero on invalid code (string code); website/login/login.zero.md:163
export function fn_invalid_code__string(code: string): void {
    fn_print__string("invalid code");
}

// @zero on (string code) = request login (string name); website/login/login.zero.md:166
export function fn_request_login__string(name: string): string {
    let code: string = undefined!;
    const found = users_arr.find(x => x.name == name)!;
    if (found.name != name) {
    throw new _ZeroRaise('unknown user', ['name']);
}
    code = fn_generate_code__User(found);
    /* TODO: send (code) to (found) */;
    pending_codes_arr.set(found.phone, code);
    return code;
}

// @zero on (User result) = verify login (string name) (string code); website/login/login.zero.md:174
export function fn_verify_login__string__string(name: string, code: string): User {
    let result: User = undefined!;
    const found = users_arr.find(x => x.name == name)!;
    const stored = pending_codes_arr.get(found.phone) ?? "";
    if (found.name != name || stored != code || stored == "") {
    throw new _ZeroRaise('invalid code', ['code']);
}
    pending_codes_arr.set(found.phone, "");
    result = found;
    return result;
}

// @zero on (string token) = complete login (string name) (string code); website/login/login.zero.md:182
export function fn_complete_login__string__string(name: string, code: string): string {
    const found = fn_verify_login__string__string(name, code);
    const token: string = fn_create_session();
    return token;
}

// @zero on (string code) = generate code (User u); website/login/login.zero.md:186
export function fn_generate_code__User(u: User): string {
    let code: string = undefined!;
    if (u.name == "_alice") {
    code = "1234";
} else if (u.name == "_bob") {
    code = "4321";
} else {
    code = "1234";
}
    return code;
}

// @zero on logo clicked; website/login/login.zero.md:194
export function fn_logo_clicked(): void {
    fn_login();
}
