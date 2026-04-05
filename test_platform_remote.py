"""Platform tests for remote.zero.md.

Tests the remote request/response protocol through Cloudflare HTTPS.

Usage: python3 test_platform_remote.py
"""

import subprocess
import sys
import time
import os
import signal
import json

BASE_URL = "wss://test.xn--nb-lkaa.org"


def build_and_start():
    subprocess.run(
        [sys.executable, "zeta.py", "website/features.md", "website/output/"],
        capture_output=True, text=True
    )
    try:
        pids = subprocess.check_output(["lsof", "-ti", ":8084"], text=True).strip()
        for pid in pids.split("\n"):
            if pid:
                os.kill(int(pid), signal.SIGTERM)
        time.sleep(1)
    except (subprocess.CalledProcessError, ValueError):
        pass
    proc = subprocess.Popen(
        [sys.executable, "website/output/website.py"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    import urllib.request
    for _ in range(20):
        try:
            urllib.request.urlopen("http://localhost:8084/", timeout=1)
            break
        except Exception:
            time.sleep(0.3)
    for _ in range(20):
        try:
            req = urllib.request.Request("https://test.xn--nb-lkaa.org/",
                headers={"User-Agent": "Mozilla/5.0 Chrome/120"})
            urllib.request.urlopen(req, timeout=3)
            return proc
        except Exception:
            time.sleep(0.5)
    print("SERVER NOT REACHABLE")
    proc.kill()
    return None


def request_on_channel(ws, command):
    """Send a remote request and wait for the response."""
    import websocket as _ws_mod
    req_id = str(time.monotonic_ns())
    ws.send(json.dumps({"id": req_id, "cmd": command}))
    # wait for matching response
    deadline = time.time() + 10
    while time.time() < deadline:
        ws.settimeout(5)
        try:
            data = ws.recv()
            msg = json.loads(data)
            if msg.get("id") == req_id:
                return msg.get("result", "")
        except Exception:
            break
    return "error: timeout"


def run_tests():
    import websocket

    results = []

    def check(name, actual, expected):
        ok = actual == expected
        results.append((name, ok))
        print(f"  {'PASS' if ok else 'FAIL'}: {name}" + (f" (got {actual!r}, expected {expected!r})" if not ok else ""))

    def check_true(name, value):
        results.append((name, bool(value)))
        print(f"  {'PASS' if value else 'FAIL'}: {name}" + ("" if value else f" (got {value!r})"))

    # --- ping ---
    print("ping/pong:")
    ws = websocket.create_connection(f"{BASE_URL}/@ws", timeout=5)
    ws.recv()  # channel ID
    result = request_on_channel(ws, "ping")
    check("ping returns pong", result, "pong")
    ws.close()

    # --- echo ---
    print("echo:")
    ws = websocket.create_connection(f"{BASE_URL}/@ws", timeout=5)
    ws.recv()
    check("echo hello", request_on_channel(ws, "echo:hello"), "hello")
    check("echo world", request_on_channel(ws, "echo:world"), "world")
    ws.close()

    # --- multiple requests same channel ---
    print("multiple requests:")
    ws = websocket.create_connection(f"{BASE_URL}/@ws", timeout=5)
    ws.recv()
    for i in range(5):
        check(f"echo msg{i}", request_on_channel(ws, f"echo:msg{i}"), f"msg{i}")
    ws.close()

    # --- independent channels ---
    print("independent channels:")
    ws1 = websocket.create_connection(f"{BASE_URL}/@ws", timeout=5)
    ws2 = websocket.create_connection(f"{BASE_URL}/@ws", timeout=5)
    ws1.recv(); ws2.recv()
    check("channel 1 echo", request_on_channel(ws1, "echo:one"), "one")
    check("channel 2 echo", request_on_channel(ws2, "echo:two"), "two")
    ws1.close(); ws2.close()

    # --- unknown command ---
    print("unknown command:")
    ws = websocket.create_connection(f"{BASE_URL}/@ws", timeout=5)
    ws.recv()
    result = request_on_channel(ws, "bogus")
    check_true("unknown command returns error", result.startswith("error:"))
    ws.close()

    passed = sum(1 for _, ok in results if ok)
    failed = sum(1 for _, ok in results if not ok)
    print(f"\n{passed} passed, {failed} failed")
    return failed == 0


if __name__ == "__main__":
    print("=== building and starting server ===")
    server = build_and_start()
    if not server:
        sys.exit(1)

    try:
        print("\n=== remote platform tests ===")
        success = run_tests()
    finally:
        print("\n=== stopping server ===")
        server.terminate()
        try:
            server.wait(timeout=3)
        except subprocess.TimeoutExpired:
            server.kill()

    sys.exit(0 if success else 1)
