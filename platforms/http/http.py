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
