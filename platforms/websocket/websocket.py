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


# @zero on input (string channel$) <- open channel (string path)
def task_open_channel__string(path: str):
    yield ""  # server-side: channels are opened by the client


# @zero on send message (string data) on (string channel)
def fn_send_message__string_on__string(data: str, channel: str):
    ch = _get_channel(channel)
    if ch:
        ch.send(data)


# @zero on input (string data$) <- receive message on (string channel)
def task_receive_message_on__string(channel: str):
    ch = _get_channel(channel)
    if ch:
        yield ch.receive()
    else:
        yield ""


# @zero on close channel (string channel)
def fn_close_channel__string(channel: str):
    ch = _get_channel(channel)
    if ch:
        ch.close()
        _remove_channel(channel)
