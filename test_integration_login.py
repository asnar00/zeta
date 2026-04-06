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
        # always use visible Chrome — headless has issues with multiple WS connections
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
    """Wait for a new browser tab to connect and return its guest name.
    Verifies the guest has a visible logo element to skip Chrome's
    internal connections (service workers, extensions)."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        new = find_new_guests(ws, known)
        for name in new:
            snapshot = ws_request(ws, "describe page ()", to=name, timeout=3)
            # real page has logo that's NOT offscreen
            if "logo" in snapshot and "offscreen" not in snapshot.split("logo")[1][:50]:
                return name
            else:
                known.add(name)  # skip non-page connections
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
    ws_request(ws, f'type ("{name}") into input box ("input")', to=route)
    ws_request(ws, 'press ("Enter") on ("input")', to=route)
    time.sleep(3)
    ws_request(ws, f'type ("{code}") into input box ("input")', to=route)
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

        # --- alice: login, set red, logout ---
        print("alice first login:")
        b_alice = Browser(headed)
        browsers.append(b_alice)
        alice_guest = wait_for_browser(ws, known_guests)
        known_guests.add(alice_guest or "")
        check_true("alice: connected", alice_guest is not None)
        check("alice: teal before login", get_bg(ws, alice_guest), "#34988b")
        login_via_ws(ws, alice_guest, "_alice", "1234")
        clients = ws_request(ws, "connected clients ()")
        check_contains("alice: logged in", clients, "_alice")
        alice_token = ws_request(ws, 'get cookie ("session")', to="_alice")
        check_true("alice: has token", len(alice_token) > 0 and "error" not in alice_token)
        check("alice: teal on first login", get_bg(ws, "_alice"), "#34988b")
        set_bg("#ff0000", alice_token)
        ws_request(ws, "reload page ()", to="_alice")
        time.sleep(3)
        check("alice: red after set", get_bg(ws, "_alice"), "#ff0000")

        # alice logs out
        print("alice first logout:")
        ws_request(ws, 'click on (".logo")', to="_alice")
        time.sleep(1)
        ws_request(ws, 'click on ("button")', to="_alice")
        time.sleep(5)
        alice_guest2 = find_new_guests(ws, known_guests)
        alice_guest2 = alice_guest2[0] if alice_guest2 else None
        if alice_guest2:
            known_guests.add(alice_guest2)
        check_true("alice: guest after logout", alice_guest2 is not None)
        check("alice: teal after logout", get_bg(ws, alice_guest2), "#34988b")

        # alice logs back in — colour should be red (persisted)
        print("alice second login:")
        time.sleep(2)  # ensure page is fully loaded after logout
        login_via_ws(ws, alice_guest2, "_alice", "1234")
        clients = ws_request(ws, "connected clients ()")
        check_contains("alice: logged in again", clients, "_alice")
        check("alice: red on second login", get_bg(ws, "_alice"), "#ff0000")

        # alice logs out again — back to teal
        print("alice second logout:")
        ws_request(ws, 'click on (".logo")', to="_alice")
        time.sleep(1)
        ws_request(ws, 'click on ("button")', to="_alice")
        time.sleep(5)
        alice_guest3 = find_new_guests(ws, known_guests)
        alice_guest3 = alice_guest3[0] if alice_guest3 else None
        if alice_guest3:
            known_guests.add(alice_guest3)
        check_true("alice: guest after second logout", alice_guest3 is not None)
        check("alice: teal after second logout", get_bg(ws, alice_guest3), "#34988b")

        # --- bob: same flow ---
        print("bob first login:")
        b_bob = Browser(headed)
        browsers.append(b_bob)
        bob_guest = wait_for_browser(ws, known_guests)
        known_guests.add(bob_guest or "")
        check_true("bob: connected", bob_guest is not None)
        check("bob: teal before login", get_bg(ws, bob_guest), "#34988b")
        login_via_ws(ws, bob_guest, "_bob", "4321")
        clients = ws_request(ws, "connected clients ()")
        check_contains("bob: logged in", clients, "_bob")
        bob_token = ws_request(ws, 'get cookie ("session")', to="_bob")
        check_true("bob: has token", len(bob_token) > 0 and "error" not in bob_token)
        check("bob: teal on first login", get_bg(ws, "_bob"), "#34988b")
        set_bg("#0000ff", bob_token)
        ws_request(ws, "reload page ()", to="_bob")
        time.sleep(3)
        check("bob: blue after set", get_bg(ws, "_bob"), "#0000ff")

        # --- isolation: default still teal ---
        print("isolation:")
        check("default: still teal", get_bg(ws, default_route), "#34988b")

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
