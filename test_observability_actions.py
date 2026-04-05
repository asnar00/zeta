"""Observability action tests: control the browser via WebSocket commands.

Tests the full login flow using WebSocket commands to click, type, and
press keys. Playwright only opens the browser — all interaction goes
through the remote channel.

All tests go through the real Cloudflare HTTPS URL.

Usage: python3 test_observability_actions.py [--headed]
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


def ws_request(ws, cmd, to=None, timeout=10):
    rid = str(time.monotonic_ns())
    msg = {"id": rid, "cmd": cmd}
    if to:
        msg["to"] = to
    ws.send(json.dumps(msg))
    deadline = time.time() + timeout
    while time.time() < deadline:
        ws.settimeout(max(0.1, deadline - time.time()))
        try:
            data = ws.recv()
            resp = json.loads(data)
            if resp.get("id") == rid:
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
        print(f"  {'PASS' if ok else 'FAIL'}: {name}" + (f" ({needle!r} not in ...)" if not ok else ""))

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not headed)

        # --- open anonymous browser (no session) ---
        print("anonymous browser:")
        ctx = browser.new_context()
        page = ctx.new_page()
        page.goto(f"{BASE_URL}/")
        page.wait_for_load_state("networkidle")
        time.sleep(3)

        # connect test WebSocket
        ws = ws_lib.create_connection(WS_URL, timeout=5)
        ws.recv()

        # find the guest name assigned to the anonymous browser
        clients = ws_request(ws, "connected clients ()")
        guest_names = [c.strip() for c in clients.split(",") if c.strip().startswith("guest-")]
        guest = guest_names[0] if guest_names else "guest-1"
        check_true("guest connected", len(guest_names) > 0)

        # describe initial page
        snapshot = ws_request(ws, "describe page ()", to=guest)
        check_contains("initial: has logo", snapshot, "logo")
        check_contains("initial: teal bg", snapshot, "52, 152, 139")

        # --- click logo: should show login (no session) ---
        print("login flow via ws:")
        result = ws_request(ws, 'click on (".logo")', to=guest)
        check("click logo", result, "ok")
        time.sleep(1)

        snapshot = ws_request(ws, "describe page ()", to=guest)
        check_contains("name input appeared", snapshot, "input")

        # type name
        result = ws_request(ws, 'type ("_alice") into ("input")', to=guest)
        check("type name", result, "ok")

        # press Enter — triggers RPC to server
        result = ws_request(ws, 'press ("Enter") on ("input")', to=guest)
        check("press enter on name", result, "ok")
        time.sleep(3)

        # code input should appear
        snapshot = ws_request(ws, "describe page ()", to=guest)
        check_contains("code input appeared", snapshot, "input")

        # type code
        result = ws_request(ws, 'type ("1234") into ("input")', to=guest)
        check("type code", result, "ok")

        # press Enter — triggers login, cookie set, page reload
        result = ws_request(ws, 'press ("Enter") on ("input")', to=guest)
        check("press enter on code", result, "ok")
        time.sleep(5)

        # after reload, the client WS reconnects with the session cookie
        # the channel is now associated with _alice, not anonymous
        clients = ws_request(ws, "connected clients ()")
        check_contains("alice connected after login", clients, "_alice")

        # describe page as alice — should show logo, no input
        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("logged in: has logo", snapshot, "logo")

        # click logo as alice — should show logout buttons
        result = ws_request(ws, 'click on (".logo")', to="_alice")
        check("click logo (toggle)", result, "ok")
        time.sleep(1)

        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("logout buttons", snapshot, '"log out"')
        check_contains("cancel button", snapshot, '"cancel"')

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
        print("\n=== observability action tests ===")
        success = run_tests(headed)
    finally:
        print("\n=== stopping server ===")
        server.terminate()
        try:
            server.wait(timeout=3)
        except subprocess.TimeoutExpired:
            server.kill()

    sys.exit(0 if success else 1)
