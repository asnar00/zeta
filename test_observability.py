"""Observability tests: route commands to browser clients via WebSocket.

Tests that the server can:
1. Associate WebSocket channels with user sessions
2. Route commands to a specific user's browser
3. Evaluate zero expressions on the client and return results

Uses Playwright to open a browser with a session cookie (simulating a logged-in user).
All network traffic goes through the real Cloudflare HTTPS URL.

Usage: python3 test_observability.py [--headed]
"""

import subprocess
import sys
import time
import os
import signal
import json
import urllib.request
import urllib.parse

BASE_URL = "https://test.xn--nb-lkaa.org"
WS_URL = "wss://test.xn--nb-lkaa.org/@ws"


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
    sessions_path = os.path.join("website", "output", "sessions.json")
    if os.path.exists(sessions_path):
        os.remove(sessions_path)
    proc = subprocess.Popen(
        [sys.executable, "website/output/website.py"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    for _ in range(20):
        try:
            urllib.request.urlopen("http://localhost:8084/", timeout=1)
            break
        except Exception:
            time.sleep(0.3)
    for _ in range(20):
        try:
            req = urllib.request.Request(f"{BASE_URL}/",
                headers={"User-Agent": "Mozilla/5.0 Chrome/120"})
            urllib.request.urlopen(req, timeout=3)
            return proc
        except Exception:
            time.sleep(0.5)
    print("SERVER NOT REACHABLE")
    proc.kill()
    return None


def rpc(path):
    """Make an RPC call via HTTP."""
    url = f"{BASE_URL}/@rpc/{urllib.parse.quote(path)}"
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 Chrome/120"})
    resp = urllib.request.urlopen(req, timeout=5)
    return resp.read().decode().strip()


def ws_request(ws, command, to=None, timeout=10):
    """Send a remote request via WebSocket."""
    req_id = str(time.monotonic_ns())
    msg = {"id": req_id, "cmd": command}
    if to:
        msg["to"] = to
    ws.send(json.dumps(msg))
    deadline = time.time() + timeout
    while time.time() < deadline:
        ws.settimeout(max(0.1, deadline - time.time()))
        try:
            data = ws.recv()
            resp = json.loads(data)
            if resp.get("id") == req_id:
                return resp.get("result", "")
        except Exception:
            break
    return "error: timeout"


def run_tests(headed=False):
    import websocket as ws_lib
    from playwright.sync_api import sync_playwright

    results = []

    def check(name, actual, expected):
        ok = actual == expected
        results.append((name, ok))
        print(f"  {'PASS' if ok else 'FAIL'}: {name}" + (f" (got {actual!r}, expected {expected!r})" if not ok else ""))

    def check_true(name, value):
        results.append((name, bool(value)))
        print(f"  {'PASS' if value else 'FAIL'}: {name}" + ("" if value else f" (got {value!r})"))

    def check_contains(name, haystack, needle):
        ok = needle in haystack
        results.append((name, ok))
        print(f"  {'PASS' if ok else 'FAIL'}: {name}" + (f" ({needle!r} not in {haystack[:80]!r}...)" if not ok else ""))

    # --- step 1: login _alice via RPC ---
    print("setup:")
    rpc('request login ("_alice")')
    token = rpc('complete login ("_alice") with code ("1234")')
    check_true("alice logged in", len(token) > 0 and token != "invalid")

    # --- step 2: open browser with alice's session cookie ---
    print("browser:")
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not headed)
        ctx = browser.new_context()
        ctx.add_cookies([{
            "name": "session",
            "value": token,
            "url": BASE_URL,
        }])
        page = ctx.new_page()
        page.goto(f"{BASE_URL}/")
        page.wait_for_load_state("networkidle")
        # wait for client.js to connect WebSocket
        time.sleep(3)
        check_true("browser loaded", page.title() != "")

        # --- step 3: connect test WebSocket ---
        print("routing:")
        ws = ws_lib.create_connection(WS_URL, timeout=5)
        ws.recv()  # our channel ID

        # check alice is listed as connected
        clients = ws_request(ws, "connected clients ()")
        check_contains("alice connected", clients, "_alice")

        # --- step 4: send command to alice's browser ---
        print("client eval:")
        result = ws_request(ws, 'get cookie ("session")', to="_alice")
        check("client eval: get cookie", result, token)

        result = ws_request(ws, "functions ()", to="_alice")
        check_contains("client functions: has login", result, "login")
        check_contains("client functions: has input", result, "input")

        # --- step 5: describe page ---
        print("describe page:")
        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("snapshot has viewport", snapshot, "viewport")
        check_contains("snapshot has body", snapshot, "body")
        check_contains("snapshot has logo", snapshot, "logo")
        check_contains("snapshot has teal bg", snapshot, "52, 152, 139")

        # --- step 6: clean up ---
        ws.close()
        browser.close()

    passed = sum(1 for _, ok in results if ok)
    failed = sum(1 for _, ok in results if not ok)
    print(f"\n{passed} passed, {failed} failed")
    return failed == 0


if __name__ == "__main__":
    headed = "--headed" in sys.argv

    print("=== building and starting server ===")
    server = build_and_start()
    if not server:
        sys.exit(1)

    try:
        print("\n=== observability tests ===")
        success = run_tests(headed)
    finally:
        print("\n=== stopping server ===")
        server.terminate()
        try:
            server.wait(timeout=3)
        except subprocess.TimeoutExpired:
            server.kill()

    sys.exit(0 if success else 1)
