# Platform implementation: blackbox (Python)
# Thin OS primitives: elapsed time, timers, local key-value store.

import time
import json
import os
import threading

_recording_start = time.monotonic()
_timers = {}
_timer_counter = 0
_timer_lock = threading.Lock()
_store = {}
_store_path = None


def _init_store_path():
    """Set the persistence path next to the running module."""
    global _store_path
    if _store_path is not None:
        return
    import sys
    main = sys.modules.get('__main__')
    if main and hasattr(main, '__file__'):
        _store_path = os.path.join(
            os.path.dirname(os.path.abspath(main.__file__)),
            "blackbox_store.json"
        )
    else:
        _store_path = "blackbox_store.json"
    _load_store()


def _load_store():
    """Load the key-value store from disk."""
    global _store
    if _store_path and os.path.exists(_store_path):
        try:
            with open(_store_path, 'r') as f:
                _store = json.load(f)
        except Exception:
            _store = {}


def _save_store():
    """Save the key-value store to disk."""
    if _store_path is None:
        return
    try:
        with open(_store_path, 'w') as f:
            json.dump(_store, f, indent=2)
    except Exception:
        pass


# timed stream iteration — sleeps dt between values for real-time playback
def _timed_iterate(stream_name, iterator):
    dt = getattr(iterator, 'dt', 0)
    for value in iterator:
        yield value
        if dt and dt > 0:
            time.sleep(dt)


# @zero input number elapsed$
def _get_elapsed() -> float:
    return round((time.monotonic() - _recording_start) * 1000, 1)


# @zero on (string timer) = every (number ms) do (string callback)
def fn_every__number_do__string(ms: float, callback: str) -> str:
    global _timer_counter
    interval = ms / 1000.0

    with _timer_lock:
        _timer_counter += 1
        timer_id = f"timer-{_timer_counter}"

    def tick():
        if timer_id not in _timers:
            return
        _resolve_and_call(callback)
        if timer_id in _timers:
            t = threading.Timer(interval, tick)
            t.daemon = True
            t.start()
            _timers[timer_id] = t

    t = threading.Timer(interval, tick)
    t.daemon = True
    t.start()
    _timers[timer_id] = t
    return timer_id


def _resolve_and_call(callback):
    """Find and call a zero function by name."""
    import sys
    main = sys.modules.get('__main__')
    if main is None:
        return
    fn_name = "fn_" + callback.replace(" ", "_").replace("-", "_")
    fn = getattr(main, fn_name, None)
    if fn and callable(fn):
        try:
            fn()
        except Exception:
            pass


# @zero on cancel timer (string timer)
def fn_cancel_timer__string(timer_id: str):
    t = _timers.pop(timer_id, None)
    if t is not None:
        t.cancel()


# @zero on store locally (string key, string value)
def fn_store_locally__string__string(key: str, value: str):
    _init_store_path()
    _store[key] = value
    _save_store()


# @zero on (string value) = retrieve locally (string key)
def fn_retrieve_locally__string(key: str) -> str:
    _init_store_path()
    return _store.get(key, "")


# @zero on (string result) = stored keys (string prefix)
def fn_stored_keys__string(prefix: str) -> str:
    _init_store_path()
    matches = sorted(k for k in _store if k.startswith(prefix))
    return ",".join(matches)


# @zero on remove locally (string key)
def fn_remove_locally__string(key: str):
    _init_store_path()
    _store.pop(key, None)
    _save_store()


# @zero on inject call (string name) with (string args) result (string result)
def fn_inject_call__string_with__string_result__string(name: str, args: str, result: str):
    """Inject a Call into the runtime input$ stream (flows to action$ via live pipe)."""
    import sys
    main = sys.modules.get('__main__')
    if main is None:
        return
    Call = getattr(main, 'Call', None)
    push = getattr(main, '_push_runtime_input', None)
    if Call and push:
        push(Call(name=name, args=args, result=result))


# @zero on replay with timing [Action actions$]
def fn_replay_with_timing(actions):
    """Replay actions into input$ with original timing gaps."""
    import sys
    main = sys.modules.get('__main__')
    if main is None:
        return
    Call = getattr(main, 'Call', None)
    push = getattr(main, '_push_runtime_input', None)
    if not (Call and push):
        return
    timestamps = getattr(actions, '_timestamps', [])
    for i, action in enumerate(actions):
        if i > 0 and i < len(timestamps) and (i - 1) < len(timestamps):
            delta = min(timestamps[i] - timestamps[i - 1], 1.0)
            if delta > 0:
                time.sleep(delta)
        name = action.get('name', str(action)) if isinstance(action, dict) else getattr(action, 'name', str(action))
        args = action.get('args', '') if isinstance(action, dict) else getattr(action, 'args', '')
        result = action.get('result', '') if isinstance(action, dict) else getattr(action, 'result', '')
        push(Call(name=name, args=args, result=result))
