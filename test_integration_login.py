"""Integration test for the login feature.

Implements the test spec from website/login/login.zero.md ## integration tests:
Three browser tabs, three users, three background colours.
Also tests the toggle login / logout workflow.

Playwright opens browser contexts. All interaction and assertions go
through the WebSocket remote channel.

All tests go through the real Cloudflare HTTPS URL.

Usage: python3 test_integration_login.py [--headed]
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
    url = f"{BASE_URL}/@rpc/{urllib.parse.quote(path)}"
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 Chrome/120"})
    return urllib.request.urlopen(req, timeout=5).read().decode().strip()


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


def get_bg(ws, route):
    """Get background colour as hex from a page snapshot."""
    import re
    snapshot = ws_request(ws, "describe page ()", to=route)
    m = re.search(r"bg:rgb\((\d+),\s*(\d+),\s*(\d+)\)", snapshot)
    if m:
        r, g, b = int(m.group(1)), int(m.group(2)), int(m.group(3))
        return f"#{r:02x}{g:02x}{b:02x}"
    return ""


def set_bg(colour, token):
    """Set background colour via RPC with session cookie."""
    expr = urllib.parse.quote(f"background.colour = {colour}")
    url = f"{BASE_URL}/@rpc/{expr}"
    req = urllib.request.Request(url, headers={
        "User-Agent": "Mozilla/5.0 Chrome/120",
        "Cookie": f"session={token}",
    })
    urllib.request.urlopen(req, timeout=5)


def find_new_guests(ws, known_guests):
    """Find guest names that weren't in known_guests."""
    clients = ws_request(ws, "connected clients ()")
    all_names = [c.strip() for c in clients.split(",")]
    return [n for n in all_names if n.startswith("guest-") and n not in known_guests]


def open_tab(browser, ws, known_guests):
    """Open a new browser tab and return (context, page, guest_name)."""
    ctx = browser.new_context()
    page = ctx.new_page()
    page.goto(f"{BASE_URL}/")
    page.wait_for_load_state("networkidle")
    time.sleep(3)
    new_guests = find_new_guests(ws, known_guests)
    name = new_guests[0] if new_guests else None
    if name:
        known_guests.add(name)
    return ctx, page, name


def login_via_ws(ws, route, name, code):
    """Login flow via WebSocket commands. Returns the user's route name after login."""
    ws_request(ws, 'click on (".logo")', to=route)
    time.sleep(1)
    ws_request(ws, f'type ("{name}") into ("input")', to=route)
    ws_request(ws, 'press ("Enter") on ("input")', to=route)
    time.sleep(3)
    ws_request(ws, f'type ("{code}") into ("input")', to=route)
    ws_request(ws, 'press ("Enter") on ("input")', to=route)
    time.sleep(5)
    # after login + reload, the route changes from guest-N to the user name
    return name


def run_tests(headed=False):
    import websocket as ws_lib
    from playwright.sync_api import sync_playwright

    results = []
    known_guests = set()

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

    ws = ws_lib.create_connection(WS_URL, timeout=5)
    first_msg = ws.recv()
    try:
        info = json.loads(first_msg)
        test_route = info.get("route", "")
    except Exception:
        test_route = ""
    known_guests.add(test_route)  # exclude our own connection from guest discovery

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not headed)

        # --- default tab: no login, teal ---
        print("default tab:")
        ctx_default, _, default_route = open_tab(browser, ws, known_guests)
        check_true("default: has route", default_route is not None)
        check("default: teal", get_bg(ws, default_route), "#34988b")

        # --- alice: login, set red ---
        print("alice tab:")
        ctx_alice, _, alice_guest = open_tab(browser, ws, known_guests)
        check_true("alice tab: has route", alice_guest is not None)
        login_via_ws(ws, alice_guest, "_alice", "1234")
        # alice's route should now be "_alice"
        clients = ws_request(ws, "connected clients ()")
        check_contains("alice connected", clients, "_alice")
        alice_token = ws_request(ws, 'get cookie ("session")', to="_alice")
        check_true("alice has token", len(alice_token) > 0 and alice_token != "error: timeout")
        set_bg("#ff0000", alice_token)
        ws_request(ws, "reload page ()", to="_alice")
        time.sleep(3)
        check("alice: red", get_bg(ws, "_alice"), "#ff0000")

        # --- alice toggle: logo shows logout ---
        print("alice toggle:")
        ws_request(ws, 'click on (".logo")', to="_alice")
        time.sleep(1)
        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("alice: logout buttons", snapshot, '"log out"')
        ws_request(ws, "reload page ()", to="_alice")
        time.sleep(3)

        # --- bob: login, set blue ---
        print("bob tab:")
        ctx_bob, _, bob_guest = open_tab(browser, ws, known_guests)
        check_true("bob tab: has route", bob_guest is not None)
        login_via_ws(ws, bob_guest, "_bob", "4321")
        clients = ws_request(ws, "connected clients ()")
        check_contains("bob connected", clients, "_bob")
        bob_token = ws_request(ws, 'get cookie ("session")', to="_bob")
        check_true("bob has token", len(bob_token) > 0 and bob_token != "error: timeout")
        set_bg("#0000ff", bob_token)
        ws_request(ws, "reload page ()", to="_bob")
        time.sleep(3)
        check("bob: blue", get_bg(ws, "_bob"), "#0000ff")

        # --- isolation ---
        print("isolation:")
        check("default: still teal", get_bg(ws, default_route), "#34988b")
        check("alice: still red", get_bg(ws, "_alice"), "#ff0000")
        check("bob: still blue", get_bg(ws, "_bob"), "#0000ff")

        # --- alice logout ---
        print("alice logout:")
        ws_request(ws, 'click on (".logo")', to="_alice")
        time.sleep(1)
        ws_request(ws, 'click on ("button")', to="_alice")
        time.sleep(3)
        # after logout + reload, alice's browser reconnects as a new guest
        time.sleep(3)
        new_guests = find_new_guests(ws, known_guests)
        alice_after = new_guests[0] if new_guests else None
        if alice_after:
            known_guests.add(alice_after)
        check_true("alice: got new guest route", alice_after is not None)
        if alice_after:
            check("alice: teal after logout", get_bg(ws, alice_after), "#34988b")

        # bob still blue
        check("bob: still blue", get_bg(ws, "_bob"), "#0000ff")

        browser.close()

    ws.close()

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
        print("\n=== running integration tests ===")
        success = run_tests(headed)
    finally:
        print("\n=== stopping server ===")
        try:
            pids = subprocess.check_output(["lsof", "-ti", ":8084"], text=True).strip()
            for pid in pids.split("\n"):
                if pid:
                    os.kill(int(pid), signal.SIGTERM)
        except Exception:
            pass

    sys.exit(0 if success else 1)
