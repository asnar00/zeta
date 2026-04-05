import { register_tests, run_tests } from './_runtime.js';
import './not_found.js';
import * as not_found from './not_found.js';
import './login.js';
import * as login from './login.js';
import './rpc.js';
import * as rpc from './rpc.js';
import './landing_page.js';
import * as landing_page from './landing_page.js';
import './background.js';
import * as background from './background.js';

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

export function test_website_0(): void {
    // trim ("  hello  ") => "hello"
    const _result = fn_trim__string("  hello  ");
    const _expected = "hello";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_1(): void {
    // trim ("already") => "already"
    const _result = fn_trim__string("already");
    const _expected = "already";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_2(): void {
    // char (0) of ("hello") => "h"
    const _result = fn_char__int_of__string(0, "hello");
    const _expected = "h";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_3(): void {
    // char (4) of ("hello") => "o"
    const _result = fn_char__int_of__string(4, "hello");
    const _expected = "o";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_4(): void {
    // ("hello world") starts with ("hello") => true
    const _result = fn__string_starts_with__string("hello world", "hello");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_5(): void {
    // ("hello world") starts with ("world") => false
    const _result = fn__string_starts_with__string("hello world", "world");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_6(): void {
    // split ("a/b/c") by ("/") => ["a", "b", "c"]
    const _result = fn_split__string_by__string("a/b/c", "/");
    const _expected = ["a", "b", "c"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_7(): void {
    // split ("hello") by ("/") => ["hello"]
    const _result = fn_split__string_by__string("hello", "/");
    const _expected = ["hello"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_8(): void {
    // length of ("hello") => 5
    const _result = fn_length_of__string("hello");
    const _expected = 5;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_9(): void {
    // length of ("") => 0
    const _result = fn_length_of__string("");
    const _expected = 0;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_10(): void {
    // replace ("world") in ("hello world") with ("zero") => "hello zero"
    const _result = fn_replace__string_in__string_with__string("world", "hello world", "zero");
    const _expected = "hello zero";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_11(): void {
    // substring of ("hello world") from (6) => "world"
    const _result = fn_substring_of__string_from__int("hello world", 6);
    const _expected = "world";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_12(): void {
    // substring of ("abc") from (0) => "abc"
    const _result = fn_substring_of__string_from__int("abc", 0);
    const _expected = "abc";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_13(): void {
    // trim ("") => ""
    const _result = fn_trim__string("");
    const _expected = "";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_14(): void {
    // trim ("  ") => ""
    const _result = fn_trim__string("  ");
    const _expected = "";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_15(): void {
    // trim ("no spaces") => "no spaces"
    const _result = fn_trim__string("no spaces");
    const _expected = "no spaces";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_16(): void {
    // trim ("  leading") => "leading"
    const _result = fn_trim__string("  leading");
    const _expected = "leading";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_17(): void {
    // trim ("trailing  ") => "trailing"
    const _result = fn_trim__string("trailing  ");
    const _expected = "trailing";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_18(): void {
    // char (0) of ("a") => "a"
    const _result = fn_char__int_of__string(0, "a");
    const _expected = "a";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_19(): void {
    // char (2) of ("abcde") => "c"
    const _result = fn_char__int_of__string(2, "abcde");
    const _expected = "c";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_20(): void {
    // ("") starts with ("") => true
    const _result = fn__string_starts_with__string("", "");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_21(): void {
    // ("hello") starts with ("") => true
    const _result = fn__string_starts_with__string("hello", "");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_22(): void {
    // ("") starts with ("x") => false
    const _result = fn__string_starts_with__string("", "x");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_23(): void {
    // ("abc") starts with ("abc") => true
    const _result = fn__string_starts_with__string("abc", "abc");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_24(): void {
    // ("abc") starts with ("abcd") => false
    const _result = fn__string_starts_with__string("abc", "abcd");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_25(): void {
    // split ("one") by (",") => ["one"]
    const _result = fn_split__string_by__string("one", ",");
    const _expected = ["one"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_26(): void {
    // split ("a,b") by (",") => ["a", "b"]
    const _result = fn_split__string_by__string("a,b", ",");
    const _expected = ["a", "b"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_27(): void {
    // split ("a,,b") by (",") => ["a", "", "b"]
    const _result = fn_split__string_by__string("a,,b", ",");
    const _expected = ["a", "", "b"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_28(): void {
    // length of ("") => 0
    const _result = fn_length_of__string("");
    const _expected = 0;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_29(): void {
    // length of ("a") => 1
    const _result = fn_length_of__string("a");
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_30(): void {
    // length of ("hello world") => 11
    const _result = fn_length_of__string("hello world");
    const _expected = 11;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_31(): void {
    // substring of ("hello") from (0) => "hello"
    const _result = fn_substring_of__string_from__int("hello", 0);
    const _expected = "hello";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_32(): void {
    // substring of ("hello") from (3) => "lo"
    const _result = fn_substring_of__string_from__int("hello", 3);
    const _expected = "lo";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_33(): void {
    // substring of ("hello") from (5) => ""
    const _result = fn_substring_of__string_from__int("hello", 5);
    const _expected = "";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_34(): void {
    // replace ("a") in ("aaa") with ("b") => "bbb"
    const _result = fn_replace__string_in__string_with__string("a", "aaa", "b");
    const _expected = "bbb";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_35(): void {
    // replace ("xy") in ("no match") with ("z") => "no match"
    const _result = fn_replace__string_in__string_with__string("xy", "no match", "z");
    const _expected = "no match";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_36(): void {
    // replace ("") in ("hello") with ("x") => "xhxexlxlxox"
    const _result = fn_replace__string_in__string_with__string("", "hello", "x");
    const _expected = "xhxexlxlxox";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_37(): void {
    // length of (random digits (1)) => 1
    const _result = fn_length_of__string(fn_random_digits__int(1));
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_38(): void {
    // length of (random digits (4)) => 4
    const _result = fn_length_of__string(fn_random_digits__int(4));
    const _expected = 4;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_39(): void {
    // length of (random digits (10)) => 10
    const _result = fn_length_of__string(fn_random_digits__int(10));
    const _expected = 10;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_40(): void {
    // length of (create session ("test")) => 8
    const _result = fn_length_of__string(fn_create_session__string("test"));
    const _expected = 8;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_41(): void {
    // handle request (Http-Request(path="/")) => "ᕦ(ツ)ᕤ"
    const _result = fn_handle_request__Http_Request(Http_Request({ path: "/" }));
    const _expected = "ᕦ(ツ)ᕤ";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_42(): void {
    // handle request (Http-Request(path="/nope")) => "ᕦ(ツ)ᕤ"
    const _result = fn_handle_request__Http_Request(Http_Request({ path: "/nope" }));
    const _expected = "ᕦ(ツ)ᕤ";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

register_tests('website', [[test_website_0, 'trim ("  hello  ") => "hello"'], [test_website_1, 'trim ("already") => "already"'], [test_website_2, 'char (0) of ("hello") => "h"'], [test_website_3, 'char (4) of ("hello") => "o"'], [test_website_4, '("hello world") starts with ("hello") => true'], [test_website_5, '("hello world") starts with ("world") => false'], [test_website_6, 'split ("a/b/c") by ("/") => ["a", "b", "c"]'], [test_website_7, 'split ("hello") by ("/") => ["hello"]'], [test_website_8, 'length of ("hello") => 5'], [test_website_9, 'length of ("") => 0'], [test_website_10, 'replace ("world") in ("hello world") with ("zero") => "hello zero"'], [test_website_11, 'substring of ("hello world") from (6) => "world"'], [test_website_12, 'substring of ("abc") from (0) => "abc"'], [test_website_13, 'trim ("") => ""'], [test_website_14, 'trim ("  ") => ""'], [test_website_15, 'trim ("no spaces") => "no spaces"'], [test_website_16, 'trim ("  leading") => "leading"'], [test_website_17, 'trim ("trailing  ") => "trailing"'], [test_website_18, 'char (0) of ("a") => "a"'], [test_website_19, 'char (2) of ("abcde") => "c"'], [test_website_20, '("") starts with ("") => true'], [test_website_21, '("hello") starts with ("") => true'], [test_website_22, '("") starts with ("x") => false'], [test_website_23, '("abc") starts with ("abc") => true'], [test_website_24, '("abc") starts with ("abcd") => false'], [test_website_25, 'split ("one") by (",") => ["one"]'], [test_website_26, 'split ("a,b") by (",") => ["a", "b"]'], [test_website_27, 'split ("a,,b") by (",") => ["a", "", "b"]'], [test_website_28, 'length of ("") => 0'], [test_website_29, 'length of ("a") => 1'], [test_website_30, 'length of ("hello world") => 11'], [test_website_31, 'substring of ("hello") from (0) => "hello"'], [test_website_32, 'substring of ("hello") from (3) => "lo"'], [test_website_33, 'substring of ("hello") from (5) => ""'], [test_website_34, 'replace ("a") in ("aaa") with ("b") => "bbb"'], [test_website_35, 'replace ("xy") in ("no match") with ("z") => "no match"'], [test_website_36, 'replace ("") in ("hello") with ("x") => "xhxexlxlxox"'], [test_website_37, 'length of (random digits (1)) => 1'], [test_website_38, 'length of (random digits (4)) => 4'], [test_website_39, 'length of (random digits (10)) => 10'], [test_website_40, 'length of (create session ("test")) => 8'], [test_website_41, 'handle request (Http-Request(path="/")) => "ᕦ(ツ)ᕤ"'], [test_website_42, 'handle request (Http-Request(path="/nope")) => "ᕦ(ツ)ᕤ"']]);

const port: number = 8084;
const logo: string = "ᕦ(ツ)ᕤ";

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

// @zero on main (string args$); website/website.zero.md:214
export async function task_main__string(args_arr: readonly string[]): Promise<void> {
    _push_terminal_out(logo);
    const request_arr = task_serve_http__int(port);
    for await (const request of request_arr) {
        _push_terminal_out(request.path);
        const body = fn_handle_request__Http_Request(request);
        _push_http_response(Http_Response({ request: request, body: body }));
    }
}

// @zero on (string body) = handle request (Http-Request request); website/website.zero.md:222
export function fn_handle_request__Http_Request(request: Http_Request): string {
    let body: string = undefined!;
    if (_get_ctx().landing_page.enabled && request.path == "/") {
    body = landing_page.fn_landing_page();
}
    if (body === undefined) {
        if (fn__string_starts_with__string(request.path, "/@rpc/")) {
    const expr = fn_substring_of__string_from__int(request.path, 6);
    body = fn_rpc_eval__string(expr);
}
    }
    if (body === undefined) {
        body = not_found.fn_not_found();
    }
    return body;
}

// @zero on stop; website/website.zero.md:230
export function fn_stop(): void {
    fn_print__string("stopping");
}


if (process.argv.includes('--test')) {
    const _names = process.argv.slice(2).filter(a => a !== '--test');
    process.exit(run_tests(_names.length ? _names : undefined) ? 1 : 0);
} else {
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
}

const _FEATURE_TREE: [string, string, string | null][] = [["website", "the nøøb website", null], ["not-found", "default 404 response", "website"], ["login", "SMS code authentication", "website"], ["rpc", "RPC endpoint for runtime evaluation", "website"], ["landing-page", "serves the noob landing page at root", "website"], ["background", "per-user background colour", "landing-page"]];
