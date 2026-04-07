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
import './test_blackbox.js';
import * as test_blackbox from './test_blackbox.js';

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


// Platform implementation: time (TypeScript)
// Implements the functions declared in time.zero.md


// @zero on (time t) = (number n) seconds
export function fn__number_seconds(n: number): number {
    return n;
}


// @zero on (time t) = (number n) ms
export function fn__number_ms(n: number): number {
    return n / 1000;
}


// @zero on (time t) = (number n) hz
export function fn__number_hz(n: number): number {
    return 1 / n;
}


// @zero on (time t) = (number n) bpm
export function fn__number_bpm(n: number): number {
    return 60 / n;
}


// @zero on (time t) = now ()
export function fn_now(): number {
    return Date.now() / 1000;
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
    // (1) seconds => 1
    const _result = fn__number_seconds(1);
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_1(): void {
    // (0.5) seconds => 0.5
    const _result = fn__number_seconds(0.5);
    const _expected = 0.5;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_2(): void {
    // (1000) ms => 1
    const _result = fn__number_ms(1000);
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_3(): void {
    // (500) ms => 0.5
    const _result = fn__number_ms(500);
    const _expected = 0.5;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_4(): void {
    // (1) hz => 1
    const _result = fn__number_hz(1);
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_5(): void {
    // (10) hz => 0.1
    const _result = fn__number_hz(10);
    const _expected = 0.1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_6(): void {
    // (60) bpm => 1
    const _result = fn__number_bpm(60);
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_7(): void {
    // (120) bpm => 0.5
    const _result = fn__number_bpm(120);
    const _expected = 0.5;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_8(): void {
    // trim ("  hello  ") => "hello"
    const _result = fn_trim__string("  hello  ");
    const _expected = "hello";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_9(): void {
    // trim ("already") => "already"
    const _result = fn_trim__string("already");
    const _expected = "already";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_10(): void {
    // char (0) of ("hello") => "h"
    const _result = fn_char__int_of__string(0, "hello");
    const _expected = "h";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_11(): void {
    // char (4) of ("hello") => "o"
    const _result = fn_char__int_of__string(4, "hello");
    const _expected = "o";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_12(): void {
    // ("hello world") starts with ("hello") => true
    const _result = fn__string_starts_with__string("hello world", "hello");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_13(): void {
    // ("hello world") starts with ("world") => false
    const _result = fn__string_starts_with__string("hello world", "world");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_14(): void {
    // ("hello world") contains ("world") => true
    const _result = fn__string_contains__string("hello world", "world");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_15(): void {
    // ("hello world") contains ("xyz") => false
    const _result = fn__string_contains__string("hello world", "xyz");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_16(): void {
    // ("hello") contains ("hello") => true
    const _result = fn__string_contains__string("hello", "hello");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_17(): void {
    // ("hello") contains ("") => true
    const _result = fn__string_contains__string("hello", "");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_18(): void {
    // split ("a/b/c") by ("/") => ["a", "b", "c"]
    const _result = fn_split__string_by__string("a/b/c", "/");
    const _expected = ["a", "b", "c"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_19(): void {
    // split ("hello") by ("/") => ["hello"]
    const _result = fn_split__string_by__string("hello", "/");
    const _expected = ["hello"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_20(): void {
    // length of ("hello") => 5
    const _result = fn_length_of__string("hello");
    const _expected = 5;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_21(): void {
    // length of ("") => 0
    const _result = fn_length_of__string("");
    const _expected = 0;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_22(): void {
    // replace ("world") in ("hello world") with ("zero") => "hello zero"
    const _result = fn_replace__string_in__string_with__string("world", "hello world", "zero");
    const _expected = "hello zero";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_23(): void {
    // substring of ("hello world") from (6) => "world"
    const _result = fn_substring_of__string_from__int("hello world", 6);
    const _expected = "world";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_24(): void {
    // substring of ("abc") from (0) => "abc"
    const _result = fn_substring_of__string_from__int("abc", 0);
    const _expected = "abc";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_25(): void {
    // to int ("42") => 42
    const _result = fn_to_int__string("42");
    const _expected = 42;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_26(): void {
    // to int ("0") => 0
    const _result = fn_to_int__string("0");
    const _expected = 0;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_27(): void {
    // trim ("") => ""
    const _result = fn_trim__string("");
    const _expected = "";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_28(): void {
    // trim ("  ") => ""
    const _result = fn_trim__string("  ");
    const _expected = "";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_29(): void {
    // trim ("no spaces") => "no spaces"
    const _result = fn_trim__string("no spaces");
    const _expected = "no spaces";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_30(): void {
    // trim ("  leading") => "leading"
    const _result = fn_trim__string("  leading");
    const _expected = "leading";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_31(): void {
    // trim ("trailing  ") => "trailing"
    const _result = fn_trim__string("trailing  ");
    const _expected = "trailing";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_32(): void {
    // char (0) of ("a") => "a"
    const _result = fn_char__int_of__string(0, "a");
    const _expected = "a";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_33(): void {
    // char (2) of ("abcde") => "c"
    const _result = fn_char__int_of__string(2, "abcde");
    const _expected = "c";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_34(): void {
    // ("") starts with ("") => true
    const _result = fn__string_starts_with__string("", "");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_35(): void {
    // ("hello") starts with ("") => true
    const _result = fn__string_starts_with__string("hello", "");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_36(): void {
    // ("") starts with ("x") => false
    const _result = fn__string_starts_with__string("", "x");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_37(): void {
    // ("abc") starts with ("abc") => true
    const _result = fn__string_starts_with__string("abc", "abc");
    const _expected = true;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_38(): void {
    // ("abc") starts with ("abcd") => false
    const _result = fn__string_starts_with__string("abc", "abcd");
    const _expected = false;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_39(): void {
    // split ("one") by (",") => ["one"]
    const _result = fn_split__string_by__string("one", ",");
    const _expected = ["one"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_40(): void {
    // split ("a,b") by (",") => ["a", "b"]
    const _result = fn_split__string_by__string("a,b", ",");
    const _expected = ["a", "b"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_41(): void {
    // split ("a,,b") by (",") => ["a", "", "b"]
    const _result = fn_split__string_by__string("a,,b", ",");
    const _expected = ["a", "", "b"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_42(): void {
    // length of ("") => 0
    const _result = fn_length_of__string("");
    const _expected = 0;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_43(): void {
    // length of ("a") => 1
    const _result = fn_length_of__string("a");
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_44(): void {
    // length of ("hello world") => 11
    const _result = fn_length_of__string("hello world");
    const _expected = 11;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_45(): void {
    // substring of ("hello") from (0) => "hello"
    const _result = fn_substring_of__string_from__int("hello", 0);
    const _expected = "hello";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_46(): void {
    // substring of ("hello") from (3) => "lo"
    const _result = fn_substring_of__string_from__int("hello", 3);
    const _expected = "lo";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_47(): void {
    // substring of ("hello") from (5) => ""
    const _result = fn_substring_of__string_from__int("hello", 5);
    const _expected = "";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_48(): void {
    // replace ("a") in ("aaa") with ("b") => "bbb"
    const _result = fn_replace__string_in__string_with__string("a", "aaa", "b");
    const _expected = "bbb";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_49(): void {
    // replace ("xy") in ("no match") with ("z") => "no match"
    const _result = fn_replace__string_in__string_with__string("xy", "no match", "z");
    const _expected = "no match";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_50(): void {
    // replace ("") in ("hello") with ("x") => "xhxexlxlxox"
    const _result = fn_replace__string_in__string_with__string("", "hello", "x");
    const _expected = "xhxexlxlxox";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_51(): void {
    // length of (random digits (1)) => 1
    const _result = fn_length_of__string(fn_random_digits__int(1));
    const _expected = 1;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_52(): void {
    // length of (random digits (4)) => 4
    const _result = fn_length_of__string(fn_random_digits__int(4));
    const _expected = 4;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_53(): void {
    // length of (random digits (10)) => 10
    const _result = fn_length_of__string(fn_random_digits__int(10));
    const _expected = 10;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_54(): void {
    // length of (create session ("test")) => 8
    const _result = fn_length_of__string(fn_create_session__string("test"));
    const _expected = 8;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_55(): void {
    // elapsed time () => 0
    const _result = fn_elapsed_time();
    const _expected = 0;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_56(): void {
    // length of (report fault ("test")) => 8
    const _result = fn_length_of__string(fn_report_fault__string("test"));
    const _expected = 8;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_57(): void {
    // handle request (Http-Request(path="/")) => "ᕦ(ツ)ᕤ"
    const _result = fn_handle_request__Http_Request(Http_Request({ path: "/" }));
    const _expected = "ᕦ(ツ)ᕤ";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_website_58(): void {
    // handle request (Http-Request(path="/nope")) => "ᕦ(ツ)ᕤ"
    const _result = fn_handle_request__Http_Request(Http_Request({ path: "/nope" }));
    const _expected = "ᕦ(ツ)ᕤ";
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

register_tests('website', [[test_website_0, '(1) seconds => 1'], [test_website_1, '(0.5) seconds => 0.5'], [test_website_2, '(1000) ms => 1'], [test_website_3, '(500) ms => 0.5'], [test_website_4, '(1) hz => 1'], [test_website_5, '(10) hz => 0.1'], [test_website_6, '(60) bpm => 1'], [test_website_7, '(120) bpm => 0.5'], [test_website_8, 'trim ("  hello  ") => "hello"'], [test_website_9, 'trim ("already") => "already"'], [test_website_10, 'char (0) of ("hello") => "h"'], [test_website_11, 'char (4) of ("hello") => "o"'], [test_website_12, '("hello world") starts with ("hello") => true'], [test_website_13, '("hello world") starts with ("world") => false'], [test_website_14, '("hello world") contains ("world") => true'], [test_website_15, '("hello world") contains ("xyz") => false'], [test_website_16, '("hello") contains ("hello") => true'], [test_website_17, '("hello") contains ("") => true'], [test_website_18, 'split ("a/b/c") by ("/") => ["a", "b", "c"]'], [test_website_19, 'split ("hello") by ("/") => ["hello"]'], [test_website_20, 'length of ("hello") => 5'], [test_website_21, 'length of ("") => 0'], [test_website_22, 'replace ("world") in ("hello world") with ("zero") => "hello zero"'], [test_website_23, 'substring of ("hello world") from (6) => "world"'], [test_website_24, 'substring of ("abc") from (0) => "abc"'], [test_website_25, 'to int ("42") => 42'], [test_website_26, 'to int ("0") => 0'], [test_website_27, 'trim ("") => ""'], [test_website_28, 'trim ("  ") => ""'], [test_website_29, 'trim ("no spaces") => "no spaces"'], [test_website_30, 'trim ("  leading") => "leading"'], [test_website_31, 'trim ("trailing  ") => "trailing"'], [test_website_32, 'char (0) of ("a") => "a"'], [test_website_33, 'char (2) of ("abcde") => "c"'], [test_website_34, '("") starts with ("") => true'], [test_website_35, '("hello") starts with ("") => true'], [test_website_36, '("") starts with ("x") => false'], [test_website_37, '("abc") starts with ("abc") => true'], [test_website_38, '("abc") starts with ("abcd") => false'], [test_website_39, 'split ("one") by (",") => ["one"]'], [test_website_40, 'split ("a,b") by (",") => ["a", "b"]'], [test_website_41, 'split ("a,,b") by (",") => ["a", "", "b"]'], [test_website_42, 'length of ("") => 0'], [test_website_43, 'length of ("a") => 1'], [test_website_44, 'length of ("hello world") => 11'], [test_website_45, 'substring of ("hello") from (0) => "hello"'], [test_website_46, 'substring of ("hello") from (3) => "lo"'], [test_website_47, 'substring of ("hello") from (5) => ""'], [test_website_48, 'replace ("a") in ("aaa") with ("b") => "bbb"'], [test_website_49, 'replace ("xy") in ("no match") with ("z") => "no match"'], [test_website_50, 'replace ("") in ("hello") with ("x") => "xhxexlxlxox"'], [test_website_51, 'length of (random digits (1)) => 1'], [test_website_52, 'length of (random digits (4)) => 4'], [test_website_53, 'length of (random digits (10)) => 10'], [test_website_54, 'length of (create session ("test")) => 8'], [test_website_55, 'elapsed time () => 0'], [test_website_56, 'length of (report fault ("test")) => 8'], [test_website_57, 'handle request (Http-Request(path="/")) => "ᕦ(ツ)ᕤ"'], [test_website_58, 'handle request (Http-Request(path="/nope")) => "ᕦ(ツ)ᕤ"']]);

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

// @zero on main (string args$); website/website.zero.md:336
export async function task_main__string(args_arr: readonly string[]): Promise<void> {
    _push_terminal_out(logo);
    const request_arr = task_serve_http__int(port);
    for await (const request of _bb_record_stream("request$", request_arr)) {
        _push_terminal_out(request.path);
        const body = fn_handle_request__Http_Request(request);
        _push_http_response(Http_Response({ request: request, body: body }));
    }
}

// @zero on (string body) = handle request (Http-Request request); website/website.zero.md:344
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

// @zero on stop; website/website.zero.md:352
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

const _FEATURE_TREE: [string, string, string | null][] = [["website", "the nøøb website", null], ["not-found", "default 404 response", "website"], ["login", "SMS code authentication", "website"], ["rpc", "RPC endpoint for runtime evaluation", "website"], ["landing-page", "serves the noob landing page at root", "website"], ["background", "per-user background colour", "landing-page"], ["test-blackbox", "integration tests for the flight recorder", "website"]];

const _BUILD_FINGERPRINT: {hash: string, git: string, features: string} = {"hash": "9866c00b2ed1618d", "git": "4ae6b7ee4c7e", "features": "website,not-found,login,rpc,landing-page,background,test-blackbox"};
