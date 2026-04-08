import * as blackbox from './blackbox.js';

// Platform implementation: blackbox (TypeScript)
// Thin OS primitives: elapsed time, timers, local key-value store.

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


// timed stream iteration — async generator with setTimeout for real-time playback
export function _timed_iterate(stream_name: string, iterator: any): any {
    const dt = iterator?.dt ?? 0;
    if (dt > 0) {
        return (async function* () {
            for (const value of iterator) {
                yield value;
                await new Promise(resolve => setTimeout(resolve, dt * 1000));
            }
        })();
    }
    return (function* () {
        for (const value of iterator) {
            yield value;
        }
    })();
}


// @zero input number elapsed$
export function _get_elapsed(): number {
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


// @zero on inject call (string name) with (string args) result (string result)
export function fn_inject_call__string_with__string_result__string(name: string, args: string, result: string): void {
    const Call = (globalThis as any).Call;
    const push = (globalThis as any)._push_runtime_input;
    if (Call && push) {
        push(new Call(name, args, result));
    }
}


// @zero on replay with timing [Action actions$]
export function fn_replay_with_timing(actions: any): void {
    const Call = (globalThis as any).Call;
    const push = (globalThis as any)._push_runtime_input;
    if (!Call || !push) return;
    const timestamps: number[] = actions?._timestamps ?? [];
    for (let i = 0; i < actions.length; i++) {
        const action = actions[i];
        const name = action?.name ?? String(action);
        const args = action?.args ?? "";
        const result = action?.result ?? "";
        push(new Call(name, args, result));
    }
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

// @zero on (string result$) <- input (string prompt)
export function* task_input__string(prompt: string): Generator<string> {
    yield (globalThis as any).prompt?.(prompt) ?? "";
}

// @zero on show message (string text)
export function fn_show_message__string(text: string): void {
    if (typeof alert !== "undefined") {
        alert(text);
    } else {
        console.log(text);
    }
}

// @zero input string cookie$[string]
const cookie_arr: Map<string, string> = new Map();

// @zero on (string value) = get cookie (string name)
export function fn_get_cookie__string(name: string): string {
    return cookie_arr.get(name) ?? "";
}

// @zero on clear cookie (string name)
export function fn_clear_cookie__string(name: string): void {
}

// @zero on (string choice$) <- choose (string option-a) or (string option-b)
export function* task_choose_or__string__string(option_a: string, option_b: string): Generator<string> {
    yield option_a;
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

// @zero on input (string channel$) <- connect to (string url)
export function* task_connect_to__string(url: string): Generator<string> {
    yield "";
}

// @zero on input (string result$) <- request (string command) on (string channel)
export function* task_request_on__string__string(command: string, channel: string): Generator<string> {
    yield fn_request__string_on__string(command, channel);
}

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

// @zero input uint random$
export function _get_random(): number {
    return Math.floor(Math.random() * 4294967296);
}


// @zero on (string result) = random digits (int n)
export function fn_random_digits__int(n: number): string {
    let result = "";
    for (let i = 0; i < n; i++) {
        result += (_get_random() % 10).toString();
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

// @zero Call input$
const input_arr: any[] = [];
const _input_subscribers: ((call: any) => void)[] = (globalThis as any)._input_subscribers ?? [];
(globalThis as any)._input_subscribers = _input_subscribers;

export function _push_runtime_input(call: any): void {
    input_arr.push(call);
    for (const sub of _input_subscribers) {
        sub(call);
    }
}

export function _subscribe_to_input(callback: (call: any) => void): void {
    const g = globalThis as any;
    if (!g._input_subscribers) {
        g._input_subscribers = [];
    }
    g._input_subscribers.push(callback);
}

const _input_instrumented: Set<string> = new Set();

export function _instrument_input(fn_name: string, _fn: Function): void {
    _input_instrumented.add(fn_name);
}

export function _record_input(fn_name: string, args: string, result: any): any {
    if (_input_instrumented.has(fn_name)) {
        _push_runtime_input({name: fn_name, args: args, result: String(result)});
    }
    return result;
}


// @zero on (string result) = rpc eval (string expr)
export function fn_rpc_eval__string(expr: string): string {
    return "error: rpc eval not implemented for TypeScript";
}


// @zero on (string json) = serialise [items$]
export function fn_serialise(items: any): string {
    const data: any = {
        values: [...items].map(v => {
            if (v === null || v === undefined) return null;
            if (typeof v === "object" && !Array.isArray(v)) {
                const obj: any = {};
                for (const [k, val] of Object.entries(v)) {
                    if (!k.startsWith("_")) obj[k] = val;
                }
                return obj;
            }
            return v;
        }),
    };
    if (items.dt) data.dt = items.dt;
    if (items.capacity) data.capacity = items.capacity;
    if (items.t0) data.t0 = items.t0;
    if (items._timestamps?.length) data.timestamps = [...items._timestamps];
    return JSON.stringify(data);
}


// @zero on (string result$) = deserialise (string json)
export function fn_deserialise__string(json_str: string): any {
    const data = JSON.parse(json_str);
    const values = data.values ?? [];
    // @ts-ignore — _Stream may or may not be defined depending on build
    const result: any = typeof _Stream !== "undefined" ? new (_Stream as any)(values) : [...values];
    if (data.dt) result.dt = data.dt;
    if (data.capacity) result.capacity = data.capacity;
    if (data.t0) result.t0 = data.t0;
    if (data.timestamps && result._timestamps) result._timestamps = data.timestamps;
    return result;
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


// @zero on (string sub) = substring of (string s) from (int start) to (int end)
export function fn_substring_of__string_from__int_to__int(s: string, start: number, end: number): string {
    return s.slice(start, end);
}


// @zero on (int pos) = index of (string needle) in (string s)
export function fn_index_of__string_in__string(needle: string, s: string): number {
    return s.indexOf(needle);
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


// @zero on (time t) = dt of [items$]
export function fn_dt_of(items: any): number {
    return items?.dt ?? 0;
}


// @zero on (time t) = capacity of [items$]
export function fn_capacity_of(items: any): number {
    return items?.capacity ?? 0;
}


// @zero on (time t) = t0 of [items$]
export function fn_t0_of(items: any): number {
    return items?.t0 ?? 0;
}


// @zero on (items$) = snapshot [items$]
export function fn_snapshot(items: any): any {
    const copy = [...items];
    if (items.dt !== undefined) (copy as any).dt = items.dt;
    if (items.capacity !== undefined) (copy as any).capacity = items.capacity;
    if (items.t0 !== undefined) (copy as any).t0 = items.t0;
    if (items._timestamps) (copy as any)._timestamps = [...items._timestamps];
    return copy;
}


// Platform implementation: websocket (TypeScript)
// Implements the functions declared in websocket.zero.md
// Server-side stub — the real client implementation lives in the client bundle.

// @zero on input (string channel$) <- open channel (string path)
export function* task_open_channel__string(path: string): Generator<string> {
    yield "";
}

// @zero on send message (string data) on (string channel)
export function fn_send_message__string_on__string(data: string, channel: string): void {
}

// @zero on input (string data$) <- receive message on (string channel)
export function* task_receive_message_on__string(channel: string): Generator<string> {
    yield "";
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

interface Call {
    readonly name: string;
    readonly args: string;
    readonly result: string;
}

export function Call(args: Partial<Call> = {}): Call {
    return { name: args.name ?? "", args: args.args ?? "", result: args.result ?? "" };
}

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

interface Action {
    readonly source: string;
    readonly name: string;
    readonly args: string;
    readonly result: string;
}

export function Action(args: Partial<Action> = {}): Action {
    return { source: args.source ?? "", name: args.name ?? "", args: args.args ?? "", result: args.result ?? "" };
}

// @zero on test blackbox; website/test-blackbox/test-blackbox.zero.md:515
export async function task_test_blackbox(): Promise<void> {
    fn_inject_call__string_with__string_result__string("click on", ".logo", "ok");
    fn_inject_call__string_with__string_result__string("press", "Escape", "ok");
    const report_arr: any = blackbox.task_report_fault__string("test: logo did something weird");
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "comment");
    }
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "trace");
    }
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "test: logo did something weird");
    }
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "click on");
    }
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "Escape");
    }
}

// @zero on test replay; website/test-blackbox/test-blackbox.zero.md:525
export async function task_test_replay(): Promise<void> {
    fn_inject_call__string_with__string_result__string("click on", ".logo", "ok");
    fn_inject_call__string_with__string_result__string("press", "Escape", "ok");
    const report1_arr: any = blackbox.task_report_fault__string("replay source");
    for (const _v of report1_arr) {
        blackbox.fn_replay_fault__string(_v);
    }
    const report2_arr: any = blackbox.task_report_fault__string("replay verify");
    for (const _v of report2_arr) {
        fn_bb_check__string_contains__string(_v, "click on");
    }
    for (const _v of report2_arr) {
        fn_bb_check__string_contains__string(_v, "Escape");
    }
}

// @zero on test blackbox auto; website/test-blackbox/test-blackbox.zero.md:534
export async function task_test_blackbox_auto(): Promise<void> {
    let token = fn_create_session__string("_bb_auto_test");
    const report_arr: any = blackbox.task_report_fault__string("auto-instrumentation test");
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "create_session");
    }
    for (const _v of report_arr) {
        fn_bb_check__string_contains__string(_v, "_bb_auto_test");
    }
}

// @zero on bb check (string actual) contains (string expected); website/test-blackbox/test-blackbox.zero.md:507
export function fn_bb_check__string_contains__string(actual: string, expected: string): void {
    const found = fn__string_contains__string(actual, expected);
    if (found == false) {
    throw new _ZeroRaise('bb check failed', ['expected']);
}
}

// @zero on bb check failed (string what); website/test-blackbox/test-blackbox.zero.md:512
export function fn_bb_check_failed__string(what: string): void {
    fn_print__string("FAIL: expected " + what);
}

_instrument_input('fn_create_session__string', fn_create_session__string);
