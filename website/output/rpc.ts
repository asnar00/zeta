import { register_tests } from './_runtime.js';

// Platform implementation: blackbox (TypeScript)
// Implements the functions declared in blackbox.zero.md

const _recording_start: number = performance.now();
const _timers: Map<string, ReturnType<typeof setInterval>> = new Map();
let _timer_counter: number = 0;
const _store: Map<string, string> = new Map();
const _STORAGE_PREFIX: string = "blackbox:";


export function _is_browser(): boolean {
    return typeof window !== "undefined" && typeof localStorage !== "undefined";
}


export function _load_store(): void {
    if (!_is_browser()) return;
    for (let i = 0; i < localStorage.length; i++) {
        const raw_key = localStorage.key(i);
        if (raw_key && raw_key.startsWith(_STORAGE_PREFIX)) {
            const key = raw_key.slice(_STORAGE_PREFIX.length);
            _store.set(key, localStorage.getItem(raw_key) ?? "");
        }
    }
}


export function _save_key(key: string, value: string): void {
    if (_is_browser()) {
        localStorage.setItem(_STORAGE_PREFIX + key, value);
    }
}


export function _remove_key(key: string): void {
    if (_is_browser()) {
        localStorage.removeItem(_STORAGE_PREFIX + key);
    }
}


_load_store();


export function _bb_record_stream(stream_name: string, iterator: any): any {
    if (iterator && typeof iterator[Symbol.asyncIterator] === "function") {
        return (async function* () {
            for await (const value of iterator) {
                yield value;
            }
        })();
    }
    return (function* () {
        for (const value of iterator) {
            yield value;
        }
    })();
}


export function _bb_record_call(fn_name: string, result: any): any {
    // record the return value of a non-deterministic call (placeholder)
    return result;
}


// @zero on (number ms) = elapsed time ()
export function fn_elapsed_time(): number {
    return Math.round((performance.now() - _recording_start) * 10) / 10;
}


// @zero on (string timer) = every (number ms) do (string callback)
export function fn_every__number_do__string(ms: number, callback: string): string {
    _timer_counter++;
    const timer_id = `timer-${_timer_counter}`;
    const interval = setInterval(() => {
        _resolve_and_call(callback);
    }, ms);
    _timers.set(timer_id, interval);
    return timer_id;
}


export function _resolve_and_call(callback: string): void {
    const fn_name = "fn_" + callback.replace(/ /g, "_").replace(/-/g, "_");
    const fn = (globalThis as any)[fn_name];
    if (typeof fn === "function") {
        try {
            fn();
        } catch (_) {}
    }
}


// @zero on cancel timer (string timer)
export function fn_cancel_timer__string(timer_id: string): void {
    const interval = _timers.get(timer_id);
    if (interval !== undefined) {
        clearInterval(interval);
        _timers.delete(timer_id);
    }
}


// @zero on store locally (string key, string value)
export function fn_store_locally__string__string(key: string, value: string): void {
    _store.set(key, value);
    _save_key(key, value);
}


// @zero on (string value) = retrieve locally (string key)
export function fn_retrieve_locally__string(key: string): string {
    return _store.get(key) ?? "";
}


// @zero on (string result) = stored keys (string prefix)
export function fn_stored_keys__string(prefix: string): string {
    const matches: string[] = [];
    for (const k of _store.keys()) {
        if (k.startsWith(prefix)) {
            matches.push(k);
        }
    }
    matches.sort();
    return matches.join(",");
}


// @zero on remove locally (string key)
export function fn_remove_locally__string(key: string): void {
    _store.delete(key);
    _remove_key(key);
}


// @zero on (string fp) = build fingerprint ()
export function fn_build_fingerprint(): string {
    return JSON.stringify((globalThis as any)._BUILD_FINGERPRINT ?? {});
}


// @zero on upload pending faults ()
export function fn_upload_pending_faults(): void {
    // client-side implementation is in blackbox.client.js
}


// @zero on (string fault) = report fault (string comment)
export function fn_report_fault__string(comment: string): string {
    // server-side stub — real implementation is in blackbox.py
    // client-side implementation is in blackbox.client.js
    return "";
}


// @zero on (string result) = get fault (string fault-id)
export function fn_get_fault__string(fault_id: string): string {
    // server-side stub — real implementation is in blackbox.py
    return "";
}


// @zero on (string buffer) = freeze buffer (string fault-id)
export function fn_freeze_buffer__string(fault_id: string): string {
    // server-side stub — real implementation is in blackbox.client.js
    return "{}";
}


// Platform implementation: eval (TypeScript)
// Implements the functions declared in eval.zero.md
// Server-side stub — delegates to rpc eval

// @zero on (string result) = eval (string expr)
export function fn_eval__string(expr: string): string {
    return fn_rpc_eval__string(expr);
}


// Platform implementation: gui (TypeScript/web)
// Implements the functions declared in gui.zero.md

// @zero on (string result) = input (string prompt)
export function fn_input__string(prompt: string): string {
    return (globalThis as any).prompt?.(prompt) ?? "";
}

// @zero on show message (string text)
export function fn_show_message__string(text: string): void {
    if (typeof alert !== "undefined") {
        alert(text);
    } else {
        console.log(text);
    }
}

// @zero on (string value) = get cookie (string name)
export function fn_get_cookie__string(name: string): string {
    return "";
}

// @zero on clear cookie (string name)
export function fn_clear_cookie__string(name: string): void {
}

// @zero on (string choice) = choose (string option_a) or (string option_b)
export function fn_choose__string_or__string(option_a: string, option_b: string): string {
    return option_a;
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

// @zero on click on (string selector)
export function fn_click_on__string(selector: string): void {
}

// @zero on type (string text) into input box (string selector)
export function fn_type__string_into_input_box__string(text: string, selector: string): void {
}

// @zero on press (string key) on (string selector)
export function fn_press__string_on__string(key: string, selector: string): void {
}

// @zero on (string snapshot) = describe page ()
export function fn_describe_page(): string {
    return "no gui on server";
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

// @zero on write (string content) to file (string path)
export function fn_write__string_to_file__string(content: string, path: string): void {
    writeFileSync(path, content, 'utf8');
}

// @zero on print (string message)
export function fn_print__string(message: string): void {
    console.log(message);
}


// Platform implementation: remote (TypeScript)
// Implements the functions declared in remote.zero.md
// Server-side stub.

// @zero on (string channel) = connect to (string url)
export function fn_connect_to__string(url: string): string {
    return "";
}

// @zero on (string result) = request (string command) on (string channel)
export function fn_request__string_on__string(command: string, channel: string): string {
    return "";
}

// @zero on disconnect from (string channel)
export function fn_disconnect_from__string(channel: string): void {
}

// @zero on (string result) = handle remote request (string command)
export function fn_handle_remote_request__string(command: string): string {
    if (command === "ping") return "pong";
    if (command.startsWith("echo:")) return command.slice(5);
    return `error: unknown command: ${command}`;
}


// Platform implementation: runtime (TypeScript)
// Implements the functions declared in runtime.zero.md


const _sessions: Map<string, any> = new Map();
const _session_names: Map<string, string> = new Map();

// @zero on (string token) = create session (string name)
export function fn_create_session__string(name: string): string {
    const existing = _session_names.get(name);
    if (existing) return existing;
    const token = Math.random().toString(36).slice(2, 10);
    const ctx = new (globalThis as any)._Context();
    _sessions.set(token, ctx);
    _session_names.set(name, token);
    return token;
}

// @zero on (string result) = random digits (int n)
export function fn_random_digits__int(n: number): string {
    let result = "";
    for (let i = 0; i < n; i++) {
        result += Math.floor(Math.random() * 10).toString();
    }
    return result;
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


// Platform implementation: sms (TypeScript)
// Implements the functions declared in sms.zero.md
// Server-side only — not used in client bundle

// @zero on send sms (string message) to (string phone)
export function fn_send_sms__string_to__string(message: string, phone: string): void {
    console.log(`sms: would send to ${phone}: ${message}`);
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


// @zero on (bool result) = (string s) contains (string substring)
export function fn__string_contains__string(s: string, substring: string): boolean {
    return s.includes(substring);
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


// @zero on (int n) = to int (string s)
export function fn_to_int__string(s: string): number {
    const n = parseInt(s, 10);
    return isNaN(n) ? 0 : n;
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


// Platform implementation: websocket (TypeScript)
// Implements the functions declared in websocket.zero.md
// Server-side stub — the real client implementation lives in the client bundle.

// @zero on (string channel) = open channel (string path)
export function fn_open_channel__string(path: string): string {
    return "";
}

// @zero on send message (string data) on (string channel)
export function fn_send_message__string_on__string(data: string, channel: string): void {
}

// @zero on (string data) = receive message on (string channel)
export function fn_receive_message_on__string(channel: string): string {
    return "";
}

// @zero on close channel (string channel)
export function fn_close_channel__string(channel: string): void {
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

export function test_rpc_0(): void {
    // rpc eval ("port") => "8084"
    const _result = fn_rpc_eval__string("port");
    const _expected = "8084";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_rpc_1(): void {
    // rpc eval ("logo = hi") => "logo = hi"
    const _result = fn_rpc_eval__string("logo = hi");
    const _expected = "logo = hi";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_rpc_2(): void {
    // rpc eval ("not found ()") => "not found"
    const _result = fn_rpc_eval__string("not found ()");
    const _expected = "not found";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_rpc_3(): void {
    // rpc eval ("trim (\"  hello  \")") => "hello"
    const _result = fn_rpc_eval__string("trim (\"  hello  \")");
    const _expected = "hello";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_rpc_4(): void {
    // rpc eval ("length of (\"test\")") => "4"
    const _result = fn_rpc_eval__string("length of (\"test\")");
    const _expected = "4";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

register_tests('rpc', [[test_rpc_0, 'rpc eval ("port") => "8084"'], [test_rpc_1, 'rpc eval ("logo = hi") => "logo = hi"'], [test_rpc_2, 'rpc eval ("not found ()") => "not found"'], [test_rpc_3, 'rpc eval ("trim (\\"  hello  \\")") => "hello"'], [test_rpc_4, 'rpc eval ("length of (\\"test\\")") => "4"']]);

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
