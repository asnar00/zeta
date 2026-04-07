// Platform implementation: blackbox (TypeScript)
// Implements the functions declared in blackbox.zero.md

const _recording_start: number = performance.now();
const _timers: Map<string, ReturnType<typeof setInterval>> = new Map();
let _timer_counter: number = 0;
const _store: Map<string, string> = new Map();
const _STORAGE_PREFIX: string = "blackbox:";


function _is_browser(): boolean {
    return typeof window !== "undefined" && typeof localStorage !== "undefined";
}


function _load_store(): void {
    if (!_is_browser()) return;
    for (let i = 0; i < localStorage.length; i++) {
        const raw_key = localStorage.key(i);
        if (raw_key && raw_key.startsWith(_STORAGE_PREFIX)) {
            const key = raw_key.slice(_STORAGE_PREFIX.length);
            _store.set(key, localStorage.getItem(raw_key) ?? "");
        }
    }
}


function _save_key(key: string, value: string): void {
    if (_is_browser()) {
        localStorage.setItem(_STORAGE_PREFIX + key, value);
    }
}


function _remove_key(key: string): void {
    if (_is_browser()) {
        localStorage.removeItem(_STORAGE_PREFIX + key);
    }
}


_load_store();


function _bb_record_stream(stream_name: string, iterator: any): any {
    const dt = iterator?.dt ?? 0;
    if (dt > 0 || (iterator && typeof iterator[Symbol.asyncIterator] === "function")) {
        return (async function* () {
            for (const value of iterator) {
                yield value;
                if (dt > 0) {
                    await new Promise(resolve => setTimeout(resolve, dt * 1000));
                }
            }
        })();
    }
    return (function* () {
        for (const value of iterator) {
            yield value;
        }
    })();
}


function _bb_record_call(fn_name: string, result: any): any {
    // record the return value of a non-deterministic call (placeholder)
    return result;
}


// @zero on (number ms) = elapsed time ()
function fn_elapsed_time(): number {
    return Math.round((performance.now() - _recording_start) * 10) / 10;
}


// @zero on (string timer) = every (number ms) do (string callback)
function fn_every__number_do__string(ms: number, callback: string): string {
    _timer_counter++;
    const timer_id = `timer-${_timer_counter}`;
    const interval = setInterval(() => {
        _resolve_and_call(callback);
    }, ms);
    _timers.set(timer_id, interval);
    return timer_id;
}


function _resolve_and_call(callback: string): void {
    const fn_name = "fn_" + callback.replace(/ /g, "_").replace(/-/g, "_");
    const fn = (globalThis as any)[fn_name];
    if (typeof fn === "function") {
        try {
            fn();
        } catch (_) {}
    }
}


// @zero on cancel timer (string timer)
function fn_cancel_timer__string(timer_id: string): void {
    const interval = _timers.get(timer_id);
    if (interval !== undefined) {
        clearInterval(interval);
        _timers.delete(timer_id);
    }
}


// @zero on store locally (string key, string value)
function fn_store_locally__string__string(key: string, value: string): void {
    _store.set(key, value);
    _save_key(key, value);
}


// @zero on (string value) = retrieve locally (string key)
function fn_retrieve_locally__string(key: string): string {
    return _store.get(key) ?? "";
}


// @zero on (string result) = stored keys (string prefix)
function fn_stored_keys__string(prefix: string): string {
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
function fn_remove_locally__string(key: string): void {
    _store.delete(key);
    _remove_key(key);
}


// @zero on (string fp) = build fingerprint ()
function fn_build_fingerprint(): string {
    return JSON.stringify((globalThis as any)._BUILD_FINGERPRINT ?? {});
}


// @zero on upload pending faults ()
function fn_upload_pending_faults(): void {
    // client-side implementation is in blackbox.client.js
}


// @zero on (string fault) = report fault (string comment)
function fn_report_fault__string(comment: string): string {
    // server-side stub — real implementation is in blackbox.py
    // client-side implementation is in blackbox.client.js
    return "";
}


// @zero on (string result) = get fault (string fault-id)
function fn_get_fault__string(fault_id: string): string {
    // server-side stub — real implementation is in blackbox.py
    return "";
}


// @zero on (string buffer) = freeze buffer (string fault-id)
function fn_freeze_buffer__string(fault_id: string): string {
    // server-side stub — real implementation is in blackbox.client.js
    return "{}";
}
