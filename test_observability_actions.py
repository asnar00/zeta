"""Observability action tests: control the browser via WebSocket commands.

No Playwright. Uses Chrome with temporary profiles.
All tests go through the real Cloudflare HTTPS URL.

Usage: python3 test_observability_actions.py [--headed]
"""

import subprocess
import sys
import time
import os
import signal
import json
import tempfile
import shutil
import urllib.request

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
    def __init__(self, headed=False):
        self.tmpdir = tempfile.mkdtemp()
        args = [CHROME, f"--user-data-dir={self.tmpdir}",
                "--no-first-run", "--no-default-browser-check",
                "--window-size=800,600"]
        if not headed:
            args.append("--window-size=800,600")
        args.append(f"{BASE_URL}/")
        self.proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

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


def wait_for_guest(ws, known, timeout=15):
    """Wait for a real browser tab (with visible logo) to connect."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        clients = ws_request(ws, "connected clients ()")
        new = [c.strip() for c in clients.split(",")
               if c.strip().startswith("guest-") and c.strip() not in known]
        for name in new:
            snapshot = ws_request(ws, "describe page ()", to=name, timeout=3)
            if "logo" in snapshot and "offscreen" not in snapshot.split("logo")[1][:50]:
                return name
            else:
                known.add(name)
        time.sleep(0.5)
    return None


def run_tests(headed=False):
    import websocket as ws_lib

    results = []
    browsers = []
    known = set()

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
        ws = ws_lib.create_connection(WS_URL, timeout=5)
        first = ws.recv()
        try:
            known.add(json.loads(first).get("route", ""))
        except Exception:
            pass

        # --- open anonymous browser ---
        print("anonymous browser:")
        b = Browser(headed)
        browsers.append(b)
        guest = wait_for_guest(ws, known)
        known.add(guest or "")
        check_true("guest connected", guest is not None)

        snapshot = ws_request(ws, "describe page ()", to=guest)
        check_contains("initial: has logo", snapshot, "logo")
        check_contains("initial: teal bg", snapshot, "52, 152, 139")

        # --- login flow via WS ---
        print("login flow via ws:")
        result = ws_request(ws, 'click on (".logo")', to=guest)
        check("click logo", result, "ok")

        # wait for input to appear (async login flow)
        snapshot = ""
        for _ in range(10):
            time.sleep(1)
            snapshot = ws_request(ws, "describe page ()", to=guest)
            if "input" in snapshot:
                break
        check_contains("name input appeared", snapshot, "input")

        result = ws_request(ws, 'type ("_alice") into input box ("input")', to=guest)
        check("type name", result, "ok")

        result = ws_request(ws, 'press ("Enter") on ("input")', to=guest)
        check("press enter on name", result, "ok")

        # wait for code input (after RPC completes)
        snapshot = ""
        for _ in range(10):
            time.sleep(1)
            snapshot = ws_request(ws, "describe page ()", to=guest)
            if "input" in snapshot:
                break
        check_contains("code input appeared", snapshot, "input")

        result = ws_request(ws, 'type ("1234") into input box ("input")', to=guest)
        check("type code", result, "ok")

        result = ws_request(ws, 'press ("Enter") on ("input")', to=guest)
        check("press enter on code", result, "ok")
        time.sleep(5)

        clients = ws_request(ws, "connected clients ()")
        check_contains("alice connected after login", clients, "_alice")

        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("logged in: has logo", snapshot, "logo")

        result = ws_request(ws, 'click on (".logo")', to="_alice")
        check("click logo (toggle)", result, "ok")
        time.sleep(1)

        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("logout buttons", snapshot, '"log out"')
        check_contains("cancel button", snapshot, '"cancel"')

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
        print("\n=== observability action tests ===")
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
