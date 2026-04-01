# Platform implementation: http (Python)
# Implements the streams and tasks declared in http.zero.md

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import queue


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
            self.end_headers()
            self.wfile.write(body.encode("utf-8"))

        def log_message(self, format, *args):
            pass  # silence default logging

    server = HTTPServer(("", port), Handler)
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
                sessions = getattr(_main_mod, '_sessions', {}) if _main_mod else {}
                ctx = sessions.get(req.token, _default_ctx)
                _main_mod._ctx_var.set(ctx)
            else:
                _main_mod._ctx_var.set(_default_ctx)
        yield req


# @zero http.response$
# Routes the response back to the correct client via the paired request
def _push_http_response(response):
    response.request._send(response.body)
