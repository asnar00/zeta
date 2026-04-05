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
    """Send a command to a remote component and wait for the response."""
    import queue as _queue
    ch = _get_ws_channel(channel)
    if ch is None:
        return "error: channel not found"
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
    """Default handler for incoming remote requests. Supports ping and echo."""
    if command == "ping":
        return "pong"
    if command.startswith("echo:"):
        return command[5:]
    return f"error: unknown command: {command}"


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
    """Route an incoming WebSocket message — either a response or a request."""
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
        # it's an incoming request — handle it and send the response
        result = fn_handle_remote_request__string(msg["cmd"])
        ch = _get_ws_channel(channel_id)
        if ch:
            response = json.dumps({"id": msg["id"], "result": result})
            ch.send(response)
