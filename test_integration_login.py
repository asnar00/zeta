"""Integration test for the login feature.

Implements the test spec from website/login/login.zero.md ## integration tests:
Three browser tabs, three users, three background colours.
Also tests the toggle login / logout workflow.

No Playwright. Uses Chrome with temporary profiles for isolated cookie jars.
All interaction and assertions go through the WebSocket remote channel.
All tests go through the real Cloudflare HTTPS URL.

Usage: python3 test_integration_login.py [--headed]
"""

import subprocess
import sys
import time
import os
import signal
import json
import re
import tempfile
import shutil
import urllib.request
import urllib.parse

BASE_URL = "https://test.xn--nb-lkaa.org"
WS_URL = "wss://test.xn--nb-lkaa.org/@ws"
CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"


def kill_test_chromes():
    """Kill any Chrome instances from previous test runs."""
    try:
        subprocess.run(["pkill", "-f", "Chrome.*user-data-dir=/"], capture_output=True)
        time.sleep(1)
    except Exception:
        pass


def build_and_start():
    kill_test_chromes()
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


class Browser:
    """A Chrome instance with an isolated profile (separate cookie jar)."""
    def __init__(self, headed=False):
        self.tmpdir = tempfile.mkdtemp()
        args = [CHROME, f"--user-data-dir={self.tmpdir}",
                "--no-first-run", "--no-default-browser-check",
                "--window-size=800,600"]
        if not headed:
            args.append("--headless=new")
        args.append(f"{BASE_URL}/")
        self.proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.guest_name = None

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            self.proc.kill()
        shutil.rmtree(self.tmpdir, ignore_errors=True)


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


def find_new_guests(ws, known):
    """Find guest names not in known set."""
    clients = ws_request(ws, "connected clients ()")
    all_names = [c.strip() for c in clients.split(",") if c.strip()]
    return [n for n in all_names if n.startswith("guest-") and n not in known]


def wait_for_browser(ws, known, timeout=15):
    """Wait for a new browser to connect and return its guest name."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        new = find_new_guests(ws, known)
        if new:
            return new[0]
        time.sleep(0.5)
    return None


def get_bg(ws, route):
    """Get background colour as hex from a page snapshot."""
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


def login_via_ws(ws, route, name, code):
    """Login flow via WebSocket commands."""
    ws_request(ws, 'click on (".logo")', to=route)
    time.sleep(1)
    ws_request(ws, f'type ("{name}") into ("input")', to=route)
    ws_request(ws, 'press ("Enter") on ("input")', to=route)
    time.sleep(3)
    ws_request(ws, f'type ("{code}") into ("input")', to=route)
    ws_request(ws, 'press ("Enter") on ("input")', to=route)
    time.sleep(5)


def run_tests(headed=False):
    import websocket as ws_lib

    results = []
    browsers = []
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

    try:
        # connect test WebSocket
        ws = ws_lib.create_connection(WS_URL, timeout=5)
        first_msg = ws.recv()
        try:
            info = json.loads(first_msg)
            known_guests.add(info.get("route", ""))
        except Exception:
            pass

        # --- default tab ---
        print("default tab:")
        b_default = Browser(headed)
        browsers.append(b_default)
        default_route = wait_for_browser(ws, known_guests)
        known_guests.add(default_route or "")
        check_true("default: connected", default_route is not None)
        check("default: teal", get_bg(ws, default_route), "#34988b")

        # --- alice tab: login, set red ---
        print("alice tab:")
        b_alice = Browser(headed)
        browsers.append(b_alice)
        alice_guest = wait_for_browser(ws, known_guests)
        known_guests.add(alice_guest or "")
        check_true("alice: connected", alice_guest is not None)
        login_via_ws(ws, alice_guest, "_alice", "1234")
        clients = ws_request(ws, "connected clients ()")
        check_contains("alice: logged in", clients, "_alice")
        alice_token = ws_request(ws, 'get cookie ("session")', to="_alice")
        check_true("alice: has token", len(alice_token) > 0 and "error" not in alice_token)
        set_bg("#ff0000", alice_token)
        ws_request(ws, "reload page ()", to="_alice")
        time.sleep(3)
        check("alice: red", get_bg(ws, "_alice"), "#ff0000")

        # --- alice toggle ---
        print("alice toggle:")
        ws_request(ws, 'click on (".logo")', to="_alice")
        time.sleep(1)
        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("alice: logout buttons", snapshot, '"log out"')
        ws_request(ws, "reload page ()", to="_alice")
        time.sleep(3)

        # --- bob tab: login, set blue ---
        print("bob tab:")
        b_bob = Browser(headed)
        browsers.append(b_bob)
        bob_guest = wait_for_browser(ws, known_guests)
        known_guests.add(bob_guest or "")
        check_true("bob: connected", bob_guest is not None)
        login_via_ws(ws, bob_guest, "_bob", "4321")
        clients = ws_request(ws, "connected clients ()")
        check_contains("bob: logged in", clients, "_bob")
        bob_token = ws_request(ws, 'get cookie ("session")', to="_bob")
        check_true("bob: has token", len(bob_token) > 0 and "error" not in bob_token)
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
        time.sleep(5)
        new_guests = find_new_guests(ws, known_guests)
        alice_after = new_guests[0] if new_guests else None
        if alice_after:
            known_guests.add(alice_after)
        check_true("alice: new guest after logout", alice_after is not None)
        if alice_after:
            check("alice: teal after logout", get_bg(ws, alice_after), "#34988b")

        # bob still blue
        check("bob: still blue", get_bg(ws, "_bob"), "#0000ff")

        ws.close()

    finally:
        for b in browsers:
            b.close()

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
