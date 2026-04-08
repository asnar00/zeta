// Platform implementation: blackbox (TypeScript)
// Thin OS primitives: elapsed time, timers, local key-value store.

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


// timed stream iteration — async generator with setTimeout for real-time playback
function _timed_iterate(stream_name: string, iterator: any): any {
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
function _get_elapsed(): number {
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
