# Platform implementation: blackbox (Python)
# Implements the functions declared in blackbox.zero.md

import time
import json
import os
import threading

_recording_start = None
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


# --- server-side flight recorder ---

_recording_start = time.monotonic()
_bb_recording = False
_bb_session_id = ""
_bb_moments = []
_bb_current_moment = None
_bb_max_moments = 6
_bb_moment_duration = 10  # seconds
_bb_tick_timer = None
_bb_correlation_counter = 0
_bb_lock = threading.Lock()
_bb_fault_reports = {}  # fault_id -> report dict (received from clients)


def _bb_elapsed():
    """Milliseconds since recording started."""
    return round((time.monotonic() - _recording_start) * 1000)


def _bb_record_stream(stream_name, iterator):
    """Wrap a stream iterator to record each yielded value."""
    for value in iterator:
        _bb_record_action("stream", stream_name, _bb_serialize_value(value), "", "", 0)
        yield value


def _bb_record_call(fn_name, result):
    """Record the return value of a non-deterministic platform call."""
    _bb_record_action("call", fn_name, "", "", _bb_serialize_value(result), 0)
    return result


def _bb_serialize_value(value):
    """Serialize a value for recording. Handles structs, primitives, and arrays."""
    if value is None:
        return ""
    if isinstance(value, (str, int, float, bool)):
        return str(value)
    if isinstance(value, list):
        return json.dumps([_bb_serialize_value(v) for v in value])
    if hasattr(value, '__dict__'):
        d = {k: _bb_serialize_value(v) for k, v in value.__dict__.items()
             if not k.startswith('_')}
        return json.dumps(d)
    return str(value)


def _bb_next_correlation():
    global _bb_correlation_counter
    _bb_correlation_counter += 1
    return f"s{_bb_correlation_counter}"


def _bb_capture_keyframe():
    """Serialize server context state as a keyframe."""
    import sys
    mod = sys.modules.get('__main__')
    if mod and hasattr(mod, '_serialize_ctx') and hasattr(mod, '_ctx_var'):
        try:
            ctx = mod._ctx_var.get()
            return json.dumps(mod._serialize_ctx(ctx))
        except Exception:
            pass
    return "{}"


def _bb_new_moment():
    return {
        "start_time": _bb_elapsed(),
        "keyframe": _bb_capture_keyframe(),
        "actions": []
    }


def _bb_rotate_moment():
    """Close current moment, start a new one."""
    global _bb_current_moment
    if not _bb_recording:
        return
    with _bb_lock:
        if _bb_current_moment:
            _bb_current_moment["end_time"] = _bb_elapsed()
            _bb_moments.append(_bb_current_moment)
            if len(_bb_moments) > _bb_max_moments:
                _bb_moments.pop(0)
        _bb_current_moment = _bb_new_moment()


def _bb_start_server_recording():
    """Start server-side recording with periodic moment rotation."""
    global _bb_recording, _bb_current_moment, _bb_tick_timer
    _bb_recording = True
    _bb_moments.clear()
    _bb_current_moment = _bb_new_moment()

    def tick():
        if not _bb_recording:
            return
        _bb_rotate_moment()
        if _bb_recording:
            t = threading.Timer(_bb_moment_duration, tick)
            t.daemon = True
            t.start()

    _bb_tick_timer = threading.Timer(_bb_moment_duration, tick)
    _bb_tick_timer.daemon = True
    _bb_tick_timer.start()
    print("[blackbox] server recording started")


def _bb_record_action(feature, name, args, correlation, result, elapsed):
    """Append an action to the current moment."""
    if not _bb_recording or not _bb_current_moment:
        return
    with _bb_lock:
        _bb_current_moment["actions"].append({
            "time": _bb_elapsed(),
            "feature": feature,
            "name": name,
            "args": args,
            "correlation": correlation,
            "result": result,
            "elapsed": elapsed,
            "kind": "call"
        })


_bb_start_server_recording()


def _bb_snapshot_server_moments():
    """Snapshot the server's current buffer."""
    with _bb_lock:
        moments = list(_bb_moments)
        if _bb_current_moment:
            snap = dict(_bb_current_moment)
            snap["end_time"] = _bb_elapsed()
            moments.append(snap)
    return moments


def _bb_collect_other_devices(fault_id, reporter_session):
    """Ask all other connected clients to freeze and send their buffers.
    Skips the reporter (identified by session) and uses a short timeout."""
    import sys
    import queue as _queue
    device_buffers = []
    mod = sys.modules.get('__main__')
    if mod is None:
        return device_buffers
    list_clients = getattr(mod, '_list_connected_clients', None)
    if not list_clients:
        return device_buffers
    try:
        clients = list_clients()
    except Exception:
        return device_buffers
    # resolve reporter's route name from session token
    reporter_name = None
    resolve_user = getattr(mod, '_resolve_session_user', None)
    if resolve_user and reporter_session:
        reporter_name = resolve_user(reporter_session)
    for client_name in clients:
        if client_name == reporter_name:
            continue
        try:
            result = _bb_request_with_timeout(
                f'freeze buffer ("{fault_id}")', client_name, timeout=3
            )
            if result and result.startswith("{"):
                device_buffers.append(json.loads(result))
        except Exception:
            pass
    return device_buffers


def _bb_request_with_timeout(command, client_name, timeout=3):
    """Send a command to a client with a short timeout. Returns result or empty."""
    import sys
    import queue as _queue
    mod = sys.modules.get('__main__')
    get_channel = getattr(mod, '_get_client_channel', None)
    get_ws = getattr(mod, '_get_ws_channel', None)
    if not get_channel or not get_ws:
        return ""
    channel_id = get_channel(client_name)
    if not channel_id:
        return ""
    ch = get_ws(channel_id)
    if ch is None:
        return ""
    import json as _json
    counter = getattr(mod, '_next_request_id', None)
    pending = getattr(mod, '_pending_responses', None)
    if not counter or pending is None:
        return ""
    req_id = counter()
    response_q = _queue.Queue()
    pending[req_id] = response_q
    ch.send(_json.dumps({"id": req_id, "cmd": command}))
    try:
        return response_q.get(timeout=timeout)
    except _queue.Empty:
        return ""
    finally:
        pending.pop(req_id, None)


def _bb_store_report(fault_id, report):
    """Persist a fault report to the in-memory cache and local store."""
    _bb_fault_reports[fault_id] = report
    _init_store_path()
    _store[f"fault:{fault_id}"] = json.dumps(report)
    _save_store()


# @zero on (string fault) = report fault (string comment)
def fn_report_fault__string(comment: str) -> str:
    """Receive a fault report (from client or server). Stores it for retrieval."""
    # if comment looks like a JSON fault report from the client, store it directly
    if comment.startswith("{") and "fault_id" in comment:
        try:
            report = json.loads(comment)
            fault_id = report.get("fault_id", "")
            if fault_id:
                # attach server moments from the same time window
                report["server_moments"] = _bb_snapshot_server_moments()
                # collect buffers from all other connected devices
                report["device_buffers"] = _bb_collect_other_devices(
                    fault_id, report.get("session", "")
                )
                _bb_store_report(fault_id, report)
                print(f"[blackbox] fault received: {fault_id} "
                      f"({len(report['device_buffers'])} other devices)")
                return fault_id
        except json.JSONDecodeError:
            pass

    # server-side fault report (not from client)
    import uuid
    fault_id = str(uuid.uuid4())[:8]
    report = {
        "fault_id": fault_id,
        "session": _bb_session_id,
        "comment": comment,
        "moments": _bb_snapshot_server_moments(),
        "reported_at": _bb_elapsed()
    }
    _bb_store_report(fault_id, report)
    print(f"[blackbox] server fault reported: {fault_id}")
    return fault_id


# @zero on (string result) = get fault (string fault-id)
def fn_get_fault__string(fault_id: str) -> str:
    """Retrieve a stored fault report by ID."""
    report = _bb_fault_reports.get(fault_id)
    if report:
        return json.dumps(report)
    _init_store_path()
    stored = _store.get(f"fault:{fault_id}", "")
    if stored:
        return stored
    return ""


# @zero on (number ms) = elapsed time ()
def fn_elapsed_time() -> float:
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
