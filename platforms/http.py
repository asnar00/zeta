# Platform implementation: http (Python)
# Implements the streams and tasks declared in http.zero.md

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import queue


class _Request:
    """Wraps an HTTP request with a channel for the response."""
    def __init__(self, path, method):
        self.path = path
        self.method = method
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
            req = _Request(self.path, "GET")
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

    while True:
        yield q.get()


# @zero http.response$
# Routes the response back to the correct client via the paired request
def _push_http_response(response):
    response.request._send(response.body)
