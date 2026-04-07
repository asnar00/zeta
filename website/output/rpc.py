from _runtime import register_tests

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
    """Wrap a stream iterator to record each yielded value.
    If the iterator has a dt property, sleep between values for real-time playback."""
    dt = getattr(iterator, 'dt', 0)
    for value in iterator:
        _bb_record_action("stream", stream_name, _bb_serialize_value(value), "", "", 0)
        yield value
        if dt and dt > 0:
            time.sleep(dt)


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
_bb_start_server_recording()


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


def _bb_get_build_fingerprint():
    """Read _BUILD_FINGERPRINT from the root module."""
    import sys
    mod = sys.modules.get('__main__')
    if mod and hasattr(mod, '_BUILD_FINGERPRINT'):
        return mod._BUILD_FINGERPRINT
    return {}


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
                # attach build fingerprint and server moments
                report["build_fingerprint"] = _bb_get_build_fingerprint()
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
        "build_fingerprint": _bb_get_build_fingerprint(),
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


# @zero on (string fp) = build fingerprint ()
def fn_build_fingerprint() -> str:
    fp = _bb_get_build_fingerprint()
    if fp:
        return json.dumps(fp)
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


# Platform implementation: eval (Python)
# Implements the functions declared in eval.zero.md
# Server-side: delegates to the existing rpc eval machinery in runtime.py


# @zero on (string result) = eval (string expr)
def fn_eval__string(expr: str) -> str:
    # delegate to rpc eval which has the full implementation
    return fn_rpc_eval__string(expr)


# functions() and features() are already implemented in runtime.py
# and will be available in the compiled output


# Platform implementation: gui (Python)
# Implements the functions declared in gui.zero.md
# Server-side fallback — in production, these run on the client.


# @zero on (string result) = input (string prompt)
def fn_input__string(prompt: str) -> str:
    return input(f"{prompt}: ")


# @zero on show message (string text)
def fn_show_message__string(text: str):
    print(text)  # server fallback: print to terminal


# @zero on (string value) = get cookie (string name)
def fn_get_cookie__string(name: str) -> str:
    return ""  # server fallback: no cookies


# @zero on clear cookie (string name)
def fn_clear_cookie__string(name: str):
    pass  # server fallback: no-op


# @zero on (string choice) = choose (string option_a) or (string option_b)
def fn_choose__string_or__string(option_a: str, option_b: str) -> str:
    return option_a  # server fallback: return first option


# @zero on set cookie of (string name) to (string value)
def fn_set_cookie_of__string_to__string(name: str, value: str):
    pass  # no-op on server — cookies are set by the HTTP response


# @zero on reload page ()
def fn_reload_page():
    pass  # no-op on server


# @zero on click on (string selector)
def fn_click_on__string(selector: str):
    pass  # no-op on server


# @zero on type (string text) into input box (string selector)
def fn_type__string_into_input_box__string(text: str, selector: str):
    pass  # no-op on server


# @zero on press (string key) on (string selector)
def fn_press__string_on__string(key: str, selector: str):
    pass  # no-op on server


# @zero on (string snapshot) = describe page ()
def fn_describe_page() -> str:
    return "no gui on server"


# Platform implementation: http (Python)
# Implements the streams and tasks declared in http.zero.md

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import queue
import json


class _Request:
    """Wraps an HTTP request with a channel for the response."""
    def __init__(self, path, method, token=""):
        self.path = path
        self.method = method
        self.token = token
        self._response = queue.Queue()

    def _send(self, body):
        self._response.put(body)

    def _wait(self):
        return self._response.get()


# @zero on (http_request request$) <- serve http (int port)
def task_serve_http__int(port):
    q = queue.Queue()

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            # WebSocket upgrade
            if self.headers.get("Upgrade", "").lower() == "websocket":
                self._handle_websocket()
                return
            # serve client-side files from /@client/
            if self.path.startswith("/@client/"):
                self._serve_client_file()
                return
            # extract session token from cookie
            token = ""
            cookie_header = self.headers.get("Cookie", "")
            for part in cookie_header.split(";"):
                part = part.strip()
                if part.startswith("session="):
                    token = part[8:]
            req = _Request(self.path, "GET", token)
            q.put(req)
            body = req._wait()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Cache-Control", "no-cache, no-store, must-revalidate")
            self.end_headers()
            self.wfile.write(body.encode("utf-8"))

        def _handle_websocket(self):
            """Upgrade to WebSocket and keep the connection alive.
            Associates the channel with the user's session if a cookie is present."""
            result = websocket_handshake(self)
            if result is None:
                self.send_response(400)
                self.end_headers()
                return
            channel, channel_id = result
            # extract session cookie and associate channel with user
            token = ""
            cookie_header = self.headers.get("Cookie", "")
            for part in cookie_header.split(";"):
                part = part.strip()
                if part.startswith("session="):
                    token = part[8:]
            user_name = _resolve_session_user(token) if token else None
            route_name = user_name or _next_guest_name()
            _register_client_channel(route_name, channel_id)
            channel.send(json.dumps({"channel": channel_id, "route": route_name}))
            # process incoming messages via the remote platform
            while not channel._closed:
                try:
                    msg = channel.inbox.get(timeout=1)
                    _handle_incoming_ws_message(channel_id, msg)
                except Exception:
                    pass
            # clean up
            _unregister_client_channel(route_name, channel_id)
            self.close_connection = True

        def _serve_client_file(self):
            """Serve a static file from the output directory."""
            import os
            filename = self.path[len("/@client/"):]
            script_dir = os.path.dirname(os.path.abspath(__file__))
            filepath = os.path.join(script_dir, filename)
            if os.path.exists(filepath):
                self.send_response(200)
                ct = "application/javascript" if filename.endswith(".js") else "text/plain"
                self.send_header("Content-Type", ct)
                self.send_header("Cache-Control", "no-cache, no-store, must-revalidate")
                self.end_headers()
                with open(filepath, "rb") as f:
                    self.wfile.write(f.read())
            else:
                self.send_response(404)
                self.end_headers()

        def log_message(self, format, *args):
            pass  # silence default logging

    from http.server import ThreadingHTTPServer
    server = ThreadingHTTPServer(("", port), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()

    # keep a reference to the default context
    _default_ctx = None
    import sys as _sys
    _main_mod = _sys.modules.get('__main__')
    if _main_mod and hasattr(_main_mod, '_ctx_var'):
        _default_ctx = _main_mod._ctx_var.get()

    while True:
        req = q.get()
        # switch context: session-specific if token, default otherwise
        if _main_mod and hasattr(_main_mod, '_ctx_var'):
            if req.token:
                _ensure_sessions = getattr(_main_mod, '_get_sessions', None)
                sessions = _ensure_sessions() if _ensure_sessions else getattr(_main_mod, '_sessions', {})
                ctx = sessions.get(req.token, _default_ctx)
                _main_mod._ctx_var.set(ctx)
            else:
                _main_mod._ctx_var.set(_default_ctx)
        yield req


# @zero http.response$
# Routes the response back to the correct client via the paired request
def _push_http_response(response):
    response.request._send(response.body)


# Platform implementation: io (Python)
# Implements the functions declared in io.zero.md


# @zero on (string content) = read file (string path)
def fn_read_file__string(path: str) -> str:
    with open(path, 'r') as f:
        return f.read()


# @zero on write (string content) to file (string path)
def fn_write__string_to_file__string(content: str, path: str):
    with open(path, 'w') as f:
        f.write(content)


# @zero on print (string message)
def fn_print__string(message: str):
    print(message)


# Platform implementation: remote (Python)
# Implements the functions declared in remote.zero.md
# Server-side: handles incoming remote requests via WebSocket.

import json
import threading


# @zero on (string channel) = connect to (string url)
def fn_connect_to__string(url: str) -> str:
    # server-side: connections are initiated by clients, not by the server.
    # a server-to-client channel is obtained when the client connects.
    return ""


# @zero on (string result) = request (string command) on (string channel)
def fn_request__string_on__string(command: str, channel: str) -> str:
    """Send a command to a remote component and wait for the response.
    Channel can be a channel ID or a user name (routes to their browser)."""
    import queue as _queue
    # try as user name first
    client_channel = _get_client_channel(channel)
    if client_channel:
        channel = client_channel
    ch = _get_ws_channel(channel)
    if ch is None:
        return f"error: channel '{channel}' not found"
    # build request with unique ID
    req_id = _next_request_id()
    msg = json.dumps({"id": req_id, "cmd": command})
    # register a response queue before sending
    response_q = _queue.Queue()
    _pending_responses[req_id] = response_q
    ch.send(msg)
    try:
        result = response_q.get(timeout=10)
        return result
    except _queue.Empty:
        return "error: timeout"
    finally:
        _pending_responses.pop(req_id, None)


# @zero on disconnect from (string channel)
def fn_disconnect_from__string(channel: str):
    ch = _get_ws_channel(channel)
    if ch:
        ch.close()


# @zero on (string result) = handle remote request (string command)
def fn_handle_remote_request__string(command: str) -> str:
    """Handle incoming remote requests by evaluating as zero expressions."""
    if command == "ping":
        return "pong"
    if command.startswith("echo:"):
        return command[5:]
    if command == "connected clients ()":
        return ", ".join(_list_connected_clients()) or "none"
    # blackbox fault upload via WebSocket (avoids URL length limits)
    if command.startswith("bb:fault:"):
        return fn_report_fault__string(command[9:])
    # evaluate as zero expression
    return fn_rpc_eval__string(command)


# --- client channel routing ---

_client_channels = {}  # route_name -> set of channel_ids
_client_channels_lock = threading.Lock()
_guest_counter = 0


def _next_guest_name():
    """Generate a sequential guest name for anonymous connections."""
    global _guest_counter
    with _client_channels_lock:
        _guest_counter += 1
        return f"guest-{_guest_counter}"


def _register_client_channel(route_name, channel_id):
    """Associate a WebSocket channel with a route name."""
    with _client_channels_lock:
        if route_name not in _client_channels:
            _client_channels[route_name] = set()
        _client_channels[route_name].add(channel_id)


def _unregister_client_channel(user_name, channel_id):
    """Remove a WebSocket channel association."""
    with _client_channels_lock:
        if user_name in _client_channels:
            _client_channels[user_name].discard(channel_id)
            if not _client_channels[user_name]:
                del _client_channels[user_name]


def _get_client_channel(user_name):
    """Get a WebSocket channel ID for a user, or None."""
    with _client_channels_lock:
        channels = _client_channels.get(user_name, set())
        if channels:
            return next(iter(channels))
    return None


def _resolve_session_user(token):
    """Reverse-look up the user name from a session token."""
    import sys
    mod = sys.modules.get("__main__")
    if mod and hasattr(mod, "_session_names"):
        names = mod._session_names
        for name, tok in names.items():
            if tok == token:
                return name
    return None


def _list_connected_clients():
    """List connected client user names."""
    with _client_channels_lock:
        return list(_client_channels.keys())


# --- internal machinery ---

_request_counter = 0
_counter_lock = threading.Lock()
_pending_responses = {}


def _next_request_id():
    global _request_counter
    with _counter_lock:
        _request_counter += 1
        return str(_request_counter)


def _get_ws_channel(channel_id):
    """Look up a WebSocket channel by ID."""
    try:
        from websocket import _get_channel
        return _get_channel(channel_id)
    except ImportError:
        pass
    # fallback: look in the compiled module
    import sys
    mod = sys.modules.get("__main__")
    if mod and hasattr(mod, "_get_channel"):
        return mod._get_channel(channel_id)
    # try the websocket platform functions directly
    for name, m in sys.modules.items():
        if hasattr(m, "_get_channel"):
            return m._get_channel(channel_id)
    return None


def _handle_incoming_ws_message(channel_id, data):
    """Route an incoming WebSocket message — response, local request, or routed request."""
    try:
        msg = json.loads(data)
    except json.JSONDecodeError:
        return
    if "result" in msg and "id" in msg:
        # it's a response to a pending request
        req_id = msg["id"]
        q = _pending_responses.get(req_id)
        if q:
            q.put(msg["result"])
    elif "cmd" in msg and "id" in msg:
        if "to" in msg:
            # routed request — forward to target user's client channel
            result = fn_request__string_on__string(msg["cmd"], msg["to"])
            ch = _get_ws_channel(channel_id)
            if ch:
                ch.send(json.dumps({"id": msg["id"], "result": result}))
        else:
            # local request — handle on this component
            result = fn_handle_remote_request__string(msg["cmd"])
            ch = _get_ws_channel(channel_id)
            if ch:
                ch.send(json.dumps({"id": msg["id"], "result": result}))


# Platform implementation: runtime (Python)
# Implements the functions declared in runtime.zero.md

import re
import sys
import urllib.parse



def _find_root_module():
    """Find the root website module (the one with __main__ entry point)."""
    # check __main__ first (when run directly)
    main = sys.modules.get('__main__')
    if main and hasattr(main, 'task_main__string'):
        return main
    # fallback: search all modules
    for name, mod in sys.modules.items():
        if hasattr(mod, 'task_main__string') and name != __name__:
            return mod
    return None


def _try_get_ctx(mod, name):
    """Try to get a user-scoped variable from the context. Returns value or None."""
    try:
        ctx = mod._ctx_var.get() if hasattr(mod, '_ctx_var') else None
        if ctx is None:
            return None
        if '.' in name:
            feat_name, var_name = name.rsplit('.', 1)
            safe_feat = feat_name.replace("-", "_")
            safe_var = var_name.replace("-", "_")
            section = getattr(ctx, safe_feat, None)
            if section is not None:
                return getattr(section, safe_var, None)
        return None
    except Exception:
        return None


def _try_set_ctx(mod, name, value):
    """Try to set a user-scoped variable on the context. Returns True if successful."""
    try:
        ctx = mod._ctx_var.get() if hasattr(mod, '_ctx_var') else None
        if ctx is None:
            return False
        if '.' in name:
            feat_name, var_name = name.rsplit('.', 1)
            safe_feat = feat_name.replace("-", "_")
            safe_var = var_name.replace("-", "_")
            section = getattr(ctx, safe_feat, None)
            if section is not None and hasattr(section, safe_var):
                setattr(section, safe_var, _coerce_value(value))
                _save_sessions()
                return True
        return False
    except Exception:
        return False


def _find_module(name):
    """Find a module by zero name (hyphenated)."""
    safe = name.replace("-", "_")
    return sys.modules.get(safe)


def _coerce_value(s):
    """Convert a string value to the appropriate Python type for variable assignment."""
    if s in ("true", "false"):
        return s == "true"
    try:
        return int(s)
    except ValueError:
        try:
            return float(s)
        except ValueError:
            return s


def _format_value(val):
    """Format a Python value as a string for RPC response."""
    if val is None:
        return "ok"
    if isinstance(val, bool):
        return "true" if val else "false"
    return str(val)


def _extract_args(s):
    """Extract parenthesized arguments from a zero call string.
    'set feature var ("landing-page-enabled") ("false")' → ["landing-page-enabled", "false"]
    """
    args = []
    i = 0
    while i < len(s):
        if s[i] == '(':
            depth = 1
            start = i + 1
            i += 1
            while i < len(s) and depth > 0:
                if s[i] == '(':
                    depth += 1
                elif s[i] == ')':
                    depth -= 1
                i += 1
            inner = s[start:i - 1].strip()
            # skip empty parens (zero-arg calls)
            if inner == "":
                continue
            # strip quotes from string args
            if inner.startswith('"') and inner.endswith('"'):
                inner = inner[1:-1]
            args.append(inner)
        else:
            i += 1
    return args


def _extract_fn_words(s):
    """Extract the function name words (everything before the first parenthesized arg).
    'set feature var ("x") ("y")' → 'set feature var'
    'stop ()' → 'stop'
    """
    paren_pos = s.find('(')
    if paren_pos == -1:
        return s.strip()
    return s[:paren_pos].strip()


def _try_match_call(mod, expr):
    """Try to match expr against all known function signatures.
    Handles method-style calls like (s) contains (sub)."""
    sigs = _extract_zero_signatures(mod)
    # also check child modules
    for attr in dir(mod):
        child = getattr(mod, attr, None)
        if hasattr(child, '__name__') and hasattr(child, '__file__'):
            sigs.update(_extract_zero_signatures(child))
    for fn_attr, proto in sigs.items():
        fn, args = _try_match_proto(expr, proto, fn_attr, mod)
        if fn is not None:
            return fn, args
    return None, None


def _try_match_proto(expr, proto, fn_attr, mod):
    """Try to match an expression against a zero prototype string.
    Returns (fn, args) or (None, None)."""
    # parse prototype into word/param pattern
    # e.g. "on (bool result) = (string s) contains (string substring)"
    # becomes pattern: [("param",), ("word", "contains"), ("param",)]
    import re
    rhs = proto
    # strip "on " prefix and result declaration
    if rhs.startswith("on "):
        rhs = rhs[3:]
    result_match = re.match(r"\([^)]+\)\s*=\s*(.*)", rhs)
    if result_match:
        rhs = result_match.group(1).strip()
    elif rhs.startswith("(") and "=" in rhs:
        return None, None
    pattern = []
    remaining = rhs.strip()
    while remaining:
        remaining = remaining.strip()
        if not remaining:
            break
        param_match = re.match(r"\(\w[\w\s-]*\s+\w[\w-]*\$?\)", remaining)
        if param_match:
            pattern.append(("param",))
            remaining = remaining[param_match.end():]
            continue
        word_match = re.match(r"(\w[\w-]*)", remaining)
        if word_match:
            pattern.append(("word", word_match.group(1)))
            remaining = remaining[word_match.end():]
            continue
        break
    if not pattern:
        return None, None
    # try to match expr against this pattern
    args = []
    pos = 0
    s = expr.strip()
    for item in pattern:
        while pos < len(s) and s[pos] == " ":
            pos += 1
        if pos >= len(s) and item[0] == "word":
            return None, None
        if item[0] == "word":
            word = item[1]
            if not s[pos:].startswith(word):
                return None, None
            end = pos + len(word)
            if end < len(s) and s[end] not in " (":
                return None, None
            pos = end
        elif item[0] == "param":
            if pos >= len(s) or s[pos] != "(":
                return None, None
            depth = 1
            start = pos + 1
            pos += 1
            while pos < len(s) and depth > 0:
                if s[pos] == "(": depth += 1
                elif s[pos] == ")": depth -= 1
                pos += 1
            inner = s[start:pos - 1].strip()
            if inner.startswith('"') and inner.endswith('"'):
                inner = inner[1:-1]
            args.append(inner)
    # check we consumed the whole expression
    remaining = s[pos:].strip()
    if remaining and remaining != "()":
        return None, None
    # find the function
    fn = getattr(mod, fn_attr, None)
    if fn is None:
        for attr in dir(mod):
            child = getattr(mod, attr, None)
            if hasattr(child, '__name__') and hasattr(child, '__file__'):
                fn = getattr(child, fn_attr, None)
                if fn and callable(fn):
                    break
    if fn and callable(fn):
        return fn, args
    return None, None


def _find_function(mod, fn_words, arg_count=0):
    """Find a compiled function in a module by its zero name words and argument count."""
    safe_prefix = "fn_" + fn_words.replace(" ", "_").replace("-", "_")
    # count type separators to match arg count
    def _matches(attr_name):
        if attr_name == safe_prefix:
            return arg_count == 0
        if attr_name.startswith(safe_prefix + "__"):
            type_part = attr_name[len(safe_prefix) + 2:]
            n_types = type_part.count("__") + 1
            return n_types == arg_count
        return False
    # search root module
    for attr_name in dir(mod):
        if _matches(attr_name):
            fn = getattr(mod, attr_name, None)
            if callable(fn):
                return fn
    # also check imported child modules
    for attr_name in dir(mod):
        child = getattr(mod, attr_name, None)
        if hasattr(child, '__name__') and hasattr(child, '__file__'):
            for child_attr in dir(child):
                if _matches(child_attr):
                    fn = getattr(child, child_attr, None)
                    if callable(fn):
                        return fn
    return None


def _sessions_path():
    """Path to the sessions persistence file."""
    import os
    mod = _find_root_module()
    if mod and hasattr(mod, '__file__'):
        return os.path.join(os.path.dirname(os.path.abspath(mod.__file__)), "sessions.json")
    return "sessions.json"


def _serialize_ctx(ctx):
    """Serialize a _Context object to a dict."""
    data = {}
    for attr in dir(ctx):
        if attr.startswith("_"):
            continue
        feat = getattr(ctx, attr)
        if hasattr(feat, '__dict__'):
            data[attr] = {k: v for k, v in feat.__dict__.items() if not k.startswith("_")}
    return data


def _deserialize_ctx(data, ctx_class):
    """Deserialize a dict into a _Context object."""
    ctx = ctx_class()
    for feat_name, feat_data in data.items():
        feat = getattr(ctx, feat_name, None)
        if feat is None:
            continue
        for k, v in feat_data.items():
            if hasattr(feat, k):
                setattr(feat, k, v)
    return ctx


def _save_sessions():
    """Save all sessions and name mappings to disk."""
    import json
    sessions = _get_sessions()
    data = {
        "sessions": {token: _serialize_ctx(ctx) for token, ctx in sessions.items()},
        "names": _get_session_names(),
    }
    try:
        with open(_sessions_path(), "w") as f:
            json.dump(data, f, indent=2)
    except Exception as e:
        print(f"sessions: save error: {e}")


def _load_sessions():
    """Load sessions and name mappings from disk."""
    import json, os
    mod = _find_root_module()
    if mod is None:
        return
    path = _sessions_path()
    if not os.path.exists(path):
        return
    try:
        with open(path) as f:
            data = json.load(f)
        # handle both old format (flat) and new format (with names)
        if "sessions" in data:
            session_data = data["sessions"]
            name_data = data.get("names", {})
        else:
            session_data = data
            name_data = {}
        sessions = _get_sessions()
        for token, ctx_data in session_data.items():
            sessions[token] = _deserialize_ctx(ctx_data, mod._Context)
        name_map = _get_session_names()
        name_map.update(name_data)
        print(f"sessions: loaded {len(sessions)} ({len(name_map)} named)")
    except Exception as e:
        print(f"sessions: load error: {e}")


def _get_sessions():
    """Get the shared sessions dict from the root module."""
    mod = _find_root_module()
    if mod is None:
        return {}
    if not hasattr(mod, '_sessions'):
        mod._sessions = {}
        _load_sessions()
    return mod._sessions


# @zero on (string token) = create session (string name)
def fn_create_session__string(name: str) -> str:
    import uuid
    mod = _find_root_module()
    if mod is None:
        return ""
    sessions = _get_sessions()
    # check for existing session for this user
    name_map = _get_session_names()
    if name in name_map:
        return name_map[name]
    # create new session
    token = str(uuid.uuid4())[:8]
    ctx = mod._Context()
    sessions[token] = ctx
    name_map[name] = token
    _save_sessions()
    return token


def _get_session_names():
    """Get the shared name→token mapping from the root module."""
    mod = _find_root_module()
    if mod is None:
        return {}
    if not hasattr(mod, '_session_names'):
        mod._session_names = {}
    return mod._session_names


# @zero on (string result) = random digits (int n)
def fn_random_digits__int(n: int) -> str:
    import random
    return "".join(str(random.randint(0, 9)) for _ in range(n))


# @zero on set session (string token)
def fn_set_session__string(token: str):
    mod = _find_root_module()
    if mod is None:
        return
    ctx = _get_sessions().get(token)
    if ctx is not None:
        mod._ctx_var.set(ctx)


# @zero on (string result) = test ()
def fn_test() -> str:
    return _run_tests_captured(None)


# @zero on (string result) = test (string feature)
def fn_test__string(feature: str) -> str:
    return _run_tests_captured([feature])


def _run_tests_captured(names):
    """Run tests and capture output as a string."""
    import io
    from contextlib import redirect_stdout
    try:
        from _runtime import run_tests
        buf = io.StringIO()
        with redirect_stdout(buf):
            run_tests(names)
        return buf.getvalue().rstrip()
    except ImportError:
        return "no tests available"


# @zero on exit process ()
def fn_exit_process():
    import os, threading
    threading.Timer(0.5, lambda: os._exit(0)).start()



def _is_user_var(attr, val):
    """Check if a module attribute is a user-defined variable (not import/platform)."""
    if attr.startswith("_") or attr.startswith("fn_") or attr.startswith("task_"):
        return False
    if callable(val) or isinstance(val, type):
        return False
    if hasattr(val, '__file__'):  # module
        return False
    if isinstance(val, (str, int, float, bool)):
        return True
    return False


def _build_platform_map(mod):
    """Build a map of function attr_name -> platform name from source comments."""
    result = {}
    try:
        import inspect
        source = inspect.getsource(mod)
        lines = source.split("\n")
        current_platform = None
        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped.startswith("# Platform implementation:"):
                current_platform = stripped.split(":")[1].strip().split(" ")[0]
            elif stripped.startswith("# @zero "):
                # check for feature source location — means it's NOT a platform fn
                if "; " in stripped:
                    current_platform = None
                # find the def line
                for j in range(i + 1, min(i + 3, len(lines))):
                    defline = lines[j].strip()
                    if defline.startswith("def fn_") or defline.startswith("def task_"):
                        attr_name = defline.split("(")[0].replace("def ", "").strip()
                        if current_platform:
                            result[attr_name] = current_platform
                        break
    except (OSError, TypeError):
        pass
    return result


def _extract_zero_signatures(mod):
    """Extract @zero source comments from a module's source file."""
    sigs = {}
    try:
        import inspect
        source = inspect.getsource(mod)
        for line in source.split("\n"):
            stripped = line.strip()
            if stripped.startswith("# @zero ") or stripped.startswith("// @zero "):
                sig = stripped.split("@zero ", 1)[1]
                # strip source location suffix
                if "; " in sig:
                    sig = sig[:sig.index("; ")]
                # find the next function def to map sig -> attr name
                continue
        # simpler: scan pairs of comment + def lines
        lines = source.split("\n")
        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped.startswith("# @zero "):
                sig = stripped.split("@zero ", 1)[1]
                if "; " in sig:
                    sig = sig[:sig.index("; ")]
                # find the def on the next non-empty line
                for j in range(i + 1, min(i + 3, len(lines))):
                    defline = lines[j].strip()
                    if defline.startswith("def fn_") or defline.startswith("def task_"):
                        attr_name = defline.split("(")[0].replace("def ", "").strip()
                        sigs[attr_name] = sig
                        break
    except (OSError, TypeError):
        pass
    return sigs


def _build_directory(mod):
    """Build a directory of available features, variables, and functions."""
    lines = []

    # collect child feature modules
    feature_modules = []
    for attr in sorted(dir(mod)):
        child = getattr(mod, attr)
        if hasattr(child, '__file__') and hasattr(child, '__name__') and attr not in ('sys', 're', 'urllib', 'queue', 'threading', 'os'):
            feature_modules.append((attr.replace("_", "-"), child))

    # extract signatures and classify: platform vs user-defined
    root_fns = set()
    zero_sigs = _extract_zero_signatures(mod)
    platform_map = _build_platform_map(mod)
    platform_lines = {}  # platform_name -> [sig_lines]
    user_fn_lines = []
    user_var_lines = []

    for attr in sorted(dir(mod)):
        val = getattr(mod, attr)
        if _is_user_var(attr, val):
            user_var_lines.append(f"  {attr.replace('_', '-')} = {_format_value(val)}")

    for attr in sorted(dir(mod)):
        is_fn = attr.startswith("fn_") and callable(getattr(mod, attr))
        is_task = attr.startswith("task_") and callable(getattr(mod, attr))
        if not is_fn and not is_task:
            continue
        root_fns.add(attr)
        sig = zero_sigs.get(attr, attr)
        plat = platform_map.get(attr)
        if plat:
            platform_lines.setdefault(plat, []).append(f"  {sig}")
        else:
            user_fn_lines.append(f"  {sig}")

    # features first, in composition order (root then children)
    lines.append("website:")
    lines.extend(user_var_lines)
    lines.extend(user_fn_lines)

    for mod_name, m in feature_modules:
        child_sigs = _extract_zero_signatures(m)
        child_lines = []
        for attr in sorted(dir(m)):
            if attr.startswith("fn_") and attr not in root_fns and callable(getattr(m, attr)):
                child_lines.append(f"  {child_sigs.get(attr, attr)}")
        if child_lines:
            lines.append(f"{mod_name}:")
            lines.extend(child_lines)

    # platforms grouped together
    if platform_lines:
        lines.append("platform:")
        for plat_name in sorted(platform_lines):
            lines.append(f"  {plat_name}:")
            lines.extend(f"    {l.strip()}" for l in platform_lines[plat_name])

    return "\n".join(lines) if lines else "empty"


# @zero on (string result) = functions ()
def fn_functions() -> str:
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"
    return _build_directory(mod)


def _build_feature_tree(tree):
    """Build a feature tree with box-drawing characters from _FEATURE_TREE data."""
    # tree is [(name, summary, extends), ...]
    children = {}  # parent -> [child entries]
    root = None
    for name, summary, extends in tree:
        if extends is None:
            root = (name, summary)
        else:
            children.setdefault(extends, []).append((name, summary))
    if root is None:
        return "no root feature"
    lines = [f"{root[0]} \u2014 {root[1]}"]
    _render_children(lines, root[0], children, "")
    return "\n".join(lines)


def _render_children(lines, parent, children, prefix):
    """Recursively render child features with box-drawing connectors."""
    kids = children.get(parent, [])
    for i, (name, summary) in enumerate(kids):
        is_last = (i == len(kids) - 1)
        connector = "\u2514\u2500 " if is_last else "\u251c\u2500 "
        lines.append(f"{prefix}{connector}{name} \u2014 {summary}")
        child_prefix = prefix + ("   " if is_last else "\u2502  ")
        _render_children(lines, name, children, child_prefix)


# @zero on (string result) = features ()
def fn_features() -> str:
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"
    tree = getattr(mod, '_FEATURE_TREE', None)
    if not tree:
        return "no feature tree available"
    return _build_feature_tree(tree)


# @zero on (string result) = rpc eval (string expr)
def fn_rpc_eval__string(expr: str) -> str:
    expr = urllib.parse.unquote(expr).strip()
    mod = _find_root_module()
    if mod is None:
        return "error: no root module"

    # empty expression: return features + functions
    if not expr:
        return fn_features() + "\n\n" + fn_functions()

    # assignment: feature.var = value  or  var = value
    # brackets indicate a function call, not an assignment
    assign_match = re.match(r'^([\w.-]+)\s*=\s*(.+)$', expr)
    if assign_match and '(' not in expr and '[' not in expr:
        name = assign_match.group(1)
        value = assign_match.group(2).strip().strip('"')
        # try context first (for user-scoped vars)
        if '.' in name and _try_set_ctx(mod, name, value):
            return f"{name} = {value}"
        # fall back to module attribute
        if '.' in name:
            mod_name, var_name = name.rsplit('.', 1)
            target = _find_module(mod_name) or mod
        else:
            target = mod
            var_name = name
        attr_name = var_name.replace("-", "_")
        setattr(target, attr_name, _coerce_value(value))
        return f"{name} = {value}"

    # var get: no brackets and no = means it's a variable read
    if '(' not in expr and '[' not in expr and '=' not in expr:
        name = expr.strip()
        # try context first (for user-scoped vars)
        ctx_val = _try_get_ctx(mod, name)
        if ctx_val is not None:
            return _format_value(ctx_val)
        # fall back to module attribute
        if '.' in name:
            mod_name, var_name = name.rsplit('.', 1)
            target = _find_module(mod_name) or mod
        else:
            target = mod
            var_name = name
        attr_name = var_name.replace("-", "_")
        val = getattr(target, attr_name, None)
        if val is not None:
            return _format_value(val)
        return f"error: {name} not found"

    # function call — try signature matching first, then naive fn_words
    fn, args = _try_match_call(mod, expr)
    if fn is None:
        fn_words = _extract_fn_words(expr)
        args = _extract_args(expr)
        fn = _find_function(mod, fn_words, len(args))
    if fn is None:
        return f"error: function '{_extract_fn_words(expr)}' not found"
    try:
        result = fn(*args)
        return _format_value(result)
    except Exception as e:
        return f"error: {e}"


# Platform implementation: sms (Python)
# Implements the functions declared in sms.zero.md
# Uses Vonage SMS API. Credentials from env vars or ../fieldnote/.env

import os


def _load_vonage_credentials():
    """Load Vonage API credentials from environment or .env file."""
    key = os.environ.get("VONAGE_API_KEY")
    secret = os.environ.get("VONAGE_API_SECRET")
    if key and secret:
        return key, secret
    # try loading from .env — check script directory and parent directories
    script_dir = os.path.dirname(os.path.abspath(__file__))
    for d in [script_dir, os.path.dirname(script_dir), os.path.dirname(os.path.dirname(script_dir))]:
        env_path = os.path.join(d, "platforms", ".env")
        if os.path.exists(env_path):
            break
        env_path = os.path.join(d, ".env")
        if os.path.exists(env_path):
            break
    else:
        env_path = None
    if env_path:
        for line in open(env_path):
            line = line.strip()
            if line.startswith("VONAGE_API_KEY="):
                key = line.split("=", 1)[1]
            elif line.startswith("VONAGE_API_SECRET="):
                secret = line.split("=", 1)[1]
    return key, secret


# @zero on send sms (string message) to (string phone)
def fn_send_sms__string_to__string(message: str, phone: str):
    import urllib.request
    import urllib.parse
    import json
    key, secret = _load_vonage_credentials()
    if not key or not secret:
        print(f"sms: no credentials, would send to {phone}: {message}")
        return
    # strip + prefix for Vonage
    to = phone.lstrip("+")
    data = json.dumps({
        "from": "noob",
        "text": message,
        "to": phone,
        "api_key": key,
        "api_secret": secret,
    }).encode("utf-8")
    req = urllib.request.Request(
        "https://rest.nexmo.com/sms/json",
        data=data,
        headers={"Content-Type": "application/json"},
    )
    try:
        resp = urllib.request.urlopen(req, timeout=10)
        result = json.loads(resp.read().decode())
        status = result.get("messages", [{}])[0].get("status", "?")
        if status == "0":
            print(f"sms: sent to {to}")
        else:
            print(f"sms: failed to {to}: {result}")
    except Exception as e:
        print(f"sms: error sending to {to}: {e}")


# Platform implementation: string (Python)
# Implements the functions declared in string.zero.md


# @zero on (string result) = trim (string s)
def fn_trim__string(s: str) -> str:
    return s.strip()


# @zero on (char c) = char (int i) of (string s)
def fn_char__int_of__string(i: int, s: str) -> str:
    return s[i]


# @zero on (string result$) = split [string s] at [int positions$]
def fn_split_at(s: str, positions: list[int]) -> list[str]:
    parts = []
    start = 0
    for pos in positions:
        parts.append(s[start:pos - 1])
        start = pos + 1
    remainder = s[start:]
    if remainder:
        parts.append(remainder)
    return parts


# @zero on (bool result) = (string s) starts with (string prefix)
def fn__string_starts_with__string(s: str, prefix: str) -> bool:
    return s.startswith(prefix)


# @zero on (bool result) = (string s) contains (string substring)
def fn__string_contains__string(s: str, substring: str) -> bool:
    return substring in s


# @zero on (string result$) = split (string s) by (string delim)
def fn_split__string_by__string(s: str, delim: str) -> list[str]:
    return s.split(delim)


# @zero on (string result) = replace (string needle) in (string s) with (string replacement)
def fn_replace__string_in__string_with__string(needle: str, s: str, replacement: str) -> str:
    return s.replace(needle, replacement)


# @zero on (int n) = length of (string s)
def fn_length_of__string(s: str) -> int:
    return len(s)


# @zero on (string sub) = substring of (string s) from (int start)
def fn_substring_of__string_from__int(s: str, start: int) -> str:
    return s[start:]


# @zero on (int n) = to int (string s)
def fn_to_int__string(s: str) -> int:
    try:
        return int(s)
    except (ValueError, TypeError):
        return 0


# Platform implementation: terminal (Python)
# Implements the streams declared in terminal.zero.md

import sys


# @zero string out$
# Subscription: each value pushed to out$ prints to stdout
def _push_terminal_out(value: str):
    print(value)


# @zero string in$
# Yields lines from stdin
def terminal_in():
    for line in sys.stdin:
        yield line.rstrip('\n')


# Platform implementation: time (Python)
# Implements the functions declared in time.zero.md

import time as _time


# @zero on (time t) = (number n) seconds
def fn__number_seconds(n: float) -> float:
    return float(n)


# @zero on (time t) = (number n) ms
def fn__number_ms(n: float) -> float:
    return float(n) / 1000.0


# @zero on (time t) = (number n) hz
def fn__number_hz(n: float) -> float:
    return 1.0 / float(n)


# @zero on (time t) = (number n) bpm
def fn__number_bpm(n: float) -> float:
    return 60.0 / float(n)


# @zero on (time t) = now ()
def fn_now() -> float:
    return _time.time()


# @zero on (time t) = dt of [items$]
def fn_dt_of(items) -> float:
    return getattr(items, 'dt', 0.0)


# @zero on (time t) = capacity of [items$]
def fn_capacity_of(items) -> float:
    return getattr(items, 'capacity', 0.0)


# @zero on (time t) = t0 of [items$]
def fn_t0_of(items) -> float:
    return getattr(items, 't0', 0.0)


# Platform implementation: websocket (Python)
# Implements the functions declared in websocket.zero.md
# Server-side WebSocket using the standard library (no dependencies).

import hashlib
import base64
import struct
import threading
import queue


class _WebSocketChannel:
    """A single WebSocket connection with send/receive queues."""
    def __init__(self, socket, path):
        self.socket = socket
        self.path = path
        self.inbox = queue.Queue()
        self._closed = False
        self._reader = threading.Thread(target=self._read_loop, daemon=True)
        self._reader.start()

    def send(self, data):
        """Send a text message."""
        if self._closed:
            return
        payload = data.encode("utf-8")
        header = bytearray()
        header.append(0x81)  # text frame, fin
        length = len(payload)
        if length < 126:
            header.append(length)
        elif length < 65536:
            header.append(126)
            header.extend(struct.pack(">H", length))
        else:
            header.append(127)
            header.extend(struct.pack(">Q", length))
        try:
            self.socket.sendall(bytes(header) + payload)
        except Exception:
            self._closed = True

    def receive(self):
        """Wait for and return the next text message."""
        return self.inbox.get()

    def close(self):
        """Close the connection."""
        self._closed = True
        try:
            self.socket.close()
        except Exception:
            pass

    def _read_loop(self):
        """Read frames from the socket and put decoded messages in the inbox."""
        while not self._closed:
            try:
                data = self._read_frame()
                if data is None:
                    break
                self.inbox.put(data)
            except Exception:
                break
        self._closed = True

    def _read_frame(self):
        """Read a single WebSocket frame, return decoded text or None on close."""
        header = self._recv_exact(2)
        if header is None:
            return None
        opcode = header[0] & 0x0F
        if opcode == 0x8:  # close frame
            return None
        masked = bool(header[1] & 0x80)
        length = header[1] & 0x7F
        if length == 126:
            ext = self._recv_exact(2)
            if ext is None:
                return None
            length = struct.unpack(">H", ext)[0]
        elif length == 127:
            ext = self._recv_exact(8)
            if ext is None:
                return None
            length = struct.unpack(">Q", ext)[0]
        mask_key = self._recv_exact(4) if masked else None
        if masked and mask_key is None:
            return None
        payload = self._recv_exact(length)
        if payload is None:
            return None
        if masked:
            payload = bytes(b ^ mask_key[i % 4] for i, b in enumerate(payload))
        if opcode == 0x1:  # text
            return payload.decode("utf-8")
        return payload.decode("utf-8", errors="replace")

    def _recv_exact(self, n):
        """Read exactly n bytes from the socket."""
        data = bytearray()
        while len(data) < n:
            try:
                chunk = self.socket.recv(n - len(data))
                if not chunk:
                    return None
                data.extend(chunk)
            except Exception:
                return None
        return bytes(data)


# global channel registry
_channels = {}
_channels_lock = threading.Lock()


def _register_channel(channel_id, channel):
    with _channels_lock:
        _channels[channel_id] = channel


def _get_channel(channel_id):
    with _channels_lock:
        return _channels.get(channel_id)


def _remove_channel(channel_id):
    with _channels_lock:
        _channels.pop(channel_id, None)


def websocket_handshake(handler):
    """Perform the WebSocket upgrade handshake. Returns a _WebSocketChannel or None."""
    key = handler.headers.get("Sec-WebSocket-Key", "")
    if not key:
        return None
    accept = base64.b64encode(
        hashlib.sha1((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest()
    ).decode()
    # write directly to the socket — BaseHTTPRequestHandler's wfile is buffered
    # and defaults to HTTP/1.0 which WebSocket rejects
    response = (
        "HTTP/1.1 101 Switching Protocols\r\n"
        "Upgrade: websocket\r\n"
        "Connection: Upgrade\r\n"
        f"Sec-WebSocket-Accept: {accept}\r\n"
        "\r\n"
    )
    handler.request.sendall(response.encode())
    channel = _WebSocketChannel(handler.request, handler.path)
    channel_id = f"{id(channel)}"
    _register_channel(channel_id, channel)
    return channel, channel_id


# @zero on (string channel) = open channel (string path)
def fn_open_channel__string(path: str) -> str:
    # server-side: channels are opened by the client connecting,
    # not by the server. This returns "" on the server.
    return ""


# @zero on send message (string data) on (string channel)
def fn_send_message__string_on__string(data: str, channel: str):
    ch = _get_channel(channel)
    if ch:
        ch.send(data)


# @zero on (string data) = receive message on (string channel)
def fn_receive_message_on__string(channel: str) -> str:
    ch = _get_channel(channel)
    if ch:
        return ch.receive()
    return ""


# @zero on close channel (string channel)
def fn_close_channel__string(channel: str):
    ch = _get_channel(channel)
    if ch:
        ch.close()
        _remove_channel(channel)


import contextvars

class _Context:
    class background:
        colour: str = "#34988b"
    class landing_page:
        enabled: bool = True
    def __init__(self):
        self.background = _Context.background()
        self.landing_page = _Context.landing_page()

_ctx_var: contextvars.ContextVar['_Context'] = contextvars.ContextVar('_ctx', default=_Context())

def _get_ctx() -> '_Context':
    import sys
    _main = sys.modules.get('__main__')
    if _main and hasattr(_main, '_ctx_var'):
        return _main._ctx_var.get()
    return _ctx_var.get()


from typing import NamedTuple

class _ZeroRaise(Exception):
    def __init__(self, name, args=None):
        self.name = name
        self.args_list = args or []
        super().__init__(f"{name}({', '.join(str(a) for a in self.args_list)})")

def test_rpc_0():
    '''rpc eval ("port") => "8084"'''
    _result = fn_rpc_eval__string("port")
    _expected = "8084"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_rpc_1():
    '''rpc eval ("logo = hi") => "logo = hi"'''
    _result = fn_rpc_eval__string("logo = hi")
    _expected = "logo = hi"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_rpc_2():
    '''rpc eval ("not found ()") => "not found"'''
    _result = fn_rpc_eval__string("not found ()")
    _expected = "not found"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_rpc_3():
    '''rpc eval ("trim (\"  hello  \")") => "hello"'''
    _result = fn_rpc_eval__string("trim (\"  hello  \")")
    _expected = "hello"
    assert _result == _expected, f"expected {_expected}, got {_result}"

def test_rpc_4():
    '''rpc eval ("length of (\"test\")") => "4"'''
    _result = fn_rpc_eval__string("length of (\"test\")")
    _expected = "4"
    assert _result == _expected, f"expected {_expected}, got {_result}"

register_tests('rpc', [(test_rpc_0, 'rpc eval ("port") => "8084"'), (test_rpc_1, 'rpc eval ("logo = hi") => "logo = hi"'), (test_rpc_2, 'rpc eval ("not found ()") => "not found"'), (test_rpc_3, 'rpc eval ("trim (\\"  hello  \\")") => "hello"'), (test_rpc_4, 'rpc eval ("length of (\\"test\\")") => "4"')])

class Http_Request(NamedTuple):
    path: str = ""
    method: str = ""
    token: str = ""

class Http_Response(NamedTuple):
    request: Http_Request = 0
    body: str = ""

class User(NamedTuple):
    name: str = ""
    phone: str = ""
    role: str = ""
