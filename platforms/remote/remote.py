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
