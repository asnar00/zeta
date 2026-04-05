"""Platform tests for websocket.zero.md.

Tests WebSocket connectivity through the real Cloudflare HTTPS URL.

Usage: python3 test_platform_websocket.py
"""

import subprocess
import sys
import time
import os
import signal

BASE_URL = "wss://test.xn--nb-lkaa.org"


def build_and_start():
    """Build, kill existing server, start fresh."""
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
    # wait for server
    import urllib.request
    for _ in range(20):
        try:
            urllib.request.urlopen("http://localhost:8084/", timeout=1)
            break
        except Exception:
            time.sleep(0.3)
    # wait for Cloudflare
    for _ in range(20):
        try:
            req = urllib.request.Request(f"https://test.xn--nb-lkaa.org/",
                headers={"User-Agent": "Mozilla/5.0 (Macintosh) Chrome/120"})
            urllib.request.urlopen(req, timeout=3)
            return proc
        except Exception:
            time.sleep(0.5)
    print("SERVER NOT REACHABLE VIA CLOUDFLARE")
    proc.kill()
    return None


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

    # --- basic connection ---
    print("basic connection:")
    ws = websocket.create_connection(f"{BASE_URL}/@ws")
    channel_id = ws.recv()
    check_true("channel opens and receives ID", len(channel_id) > 0)
    ws.send("hello")
    # server doesn't echo, but connection should stay alive
    time.sleep(0.5)
    check_true("channel stays connected after send", ws.connected)
    ws.close()
    time.sleep(0.5)
    check_true("channel disconnects after close", not ws.connected)

    # --- multiple independent channels ---
    print("multiple channels:")
    ws_a = websocket.create_connection(f"{BASE_URL}/@ws")
    ws_b = websocket.create_connection(f"{BASE_URL}/@ws")
    id_a = ws_a.recv()
    id_b = ws_b.recv()
    check_true("two channels get different IDs", id_a != id_b)
    ws_a.send("for a")
    ws_b.send("for b")
    time.sleep(0.5)
    check_true("both channels still connected", ws_a.connected and ws_b.connected)
    ws_a.close()
    time.sleep(0.5)
    check_true("channel a closed, b still open", not ws_a.connected and ws_b.connected)
    ws_b.close()

    # --- multiple messages on one channel ---
    print("multiple messages:")
    ws = websocket.create_connection(f"{BASE_URL}/@ws")
    ws.recv()  # channel ID
    ws.send("one")
    time.sleep(0.3)
    check_true("connected after message 1", ws.connected)
    ws.send("two")
    time.sleep(0.3)
    check_true("connected after message 2", ws.connected)
    ws.send("three")
    time.sleep(0.3)
    check_true("connected after message 3", ws.connected)
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
        print("\n=== websocket platform tests ===")
        success = run_tests()
    finally:
        print("\n=== stopping server ===")
        server.terminate()
        try:
            server.wait(timeout=3)
        except subprocess.TimeoutExpired:
            server.kill()

    sys.exit(0 if success else 1)
