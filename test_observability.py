"""Observability tests: route commands to browser clients via WebSocket.

No Playwright. Uses Chrome with temporary profiles.
All tests go through the real Cloudflare HTTPS URL.

Usage: python3 test_observability.py [--headed]
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
    def __init__(self, headed=False):
        self.tmpdir = tempfile.mkdtemp()
        args = [CHROME, f"--user-data-dir={self.tmpdir}",
                "--no-first-run", "--no-default-browser-check",
                "--window-size=800,600"]
        if not headed:
            args.append("--headless=new")
        args.append(f"{BASE_URL}/")
        self.proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            self.proc.kill()
        shutil.rmtree(self.tmpdir, ignore_errors=True)


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


def wait_for_guest(ws, known, timeout=15):
    deadline = time.time() + timeout
    while time.time() < deadline:
        clients = ws_request(ws, "connected clients ()")
        new = [c.strip() for c in clients.split(",")
               if c.strip().startswith("guest-") and c.strip() not in known]
        if new:
            return new[0]
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
        # login alice via RPC
        print("setup:")
        rpc('request login ("_alice")')
        token = rpc('complete login ("_alice") with code ("1234")')
        check_true("alice logged in", len(token) > 0 and token != "invalid")

        # connect test WebSocket
        ws = ws_lib.create_connection(WS_URL, timeout=5)
        first = ws.recv()
        try:
            known.add(json.loads(first).get("route", ""))
        except Exception:
            pass

        # open browser — need to set session cookie via a login URL redirect
        # simpler: open browser, it connects as guest, we login via WS
        print("browser:")
        b = Browser(headed)
        browsers.append(b)
        guest = wait_for_guest(ws, known)
        known.add(guest or "")
        check_true("browser connected", guest is not None)

        # login alice via WS on this browser
        ws_request(ws, 'click on (".logo")', to=guest)
        time.sleep(1)
        ws_request(ws, 'type ("_alice") into ("input")', to=guest)
        ws_request(ws, 'press ("Enter") on ("input")', to=guest)
        time.sleep(3)
        ws_request(ws, 'type ("1234") into ("input")', to=guest)
        ws_request(ws, 'press ("Enter") on ("input")', to=guest)
        time.sleep(5)

        clients = ws_request(ws, "connected clients ()")
        check_contains("alice connected", clients, "_alice")

        # --- test observability ---
        print("client eval:")
        result = ws_request(ws, 'get cookie ("session")', to="_alice")
        check("get cookie", result, token)

        result = ws_request(ws, "functions ()", to="_alice")
        check_contains("functions: has login", result, "login")
        check_contains("functions: has input", result, "input")

        print("describe page:")
        snapshot = ws_request(ws, "describe page ()", to="_alice")
        check_contains("snapshot has viewport", snapshot, "viewport")
        check_contains("snapshot has body", snapshot, "body")
        check_contains("snapshot has logo", snapshot, "logo")
        check_contains("snapshot has teal bg", snapshot, "52, 152, 139")

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
        print("\n=== observability tests ===")
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
