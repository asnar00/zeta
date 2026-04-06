"""Integration test for the blackbox flight recorder.

Tests the actual user experience: app runs, user does things, user reports
a fault, the server receives a fault report containing moments with
keyframes and actions that reflect what the user actually did.

All tests go through the real Cloudflare HTTPS URL.

Usage: python3 test_blackbox_integration.py [--headed]
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
    """Build the website with blackbox, start the server."""
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
    store_path = os.path.join("website", "output", "blackbox_store.json")
    if os.path.exists(store_path):
        os.remove(store_path)
    proc = subprocess.Popen(
        [sys.executable, "website/output/website.py"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    wait_for_server(proc)
    return proc


def wait_for_server(proc):
    """Wait for the server to be reachable through Cloudflare."""
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
            return
        except Exception:
            time.sleep(0.5)
    proc.kill()
    raise RuntimeError("SERVER NOT REACHABLE VIA CLOUDFLARE")


class Browser:
    """A Chrome instance with an isolated profile."""
    def __init__(self, headed=False):
        self.tmpdir = tempfile.mkdtemp()
        args = [CHROME, f"--user-data-dir={self.tmpdir}",
                "--no-first-run", "--no-default-browser-check",
                "--window-size=800,600", f"{BASE_URL}/"]
        self.proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            self.proc.kill()
        shutil.rmtree(self.tmpdir, ignore_errors=True)


def ws_request(ws, cmd, to=None, timeout=10):
    """Send a command over WebSocket and wait for the response."""
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
    """Wait for a new browser tab to connect and return its guest name."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        new = find_new_guests(ws, known)
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
    known_guests = set()

    def check(name, actual, expected):
        ok = actual == expected
        results.append((name, ok))
        status = "PASS" if ok else "FAIL"
        detail = "" if ok else f" (got {actual!r}, expected {expected!r})"
        print(f"  {status}: {name}{detail}")

    def check_true(name, value):
        results.append((name, bool(value)))
        status = "PASS" if value else "FAIL"
        detail = "" if value else f" (got {value!r})"
        print(f"  {status}: {name}{detail}")

    try:
        # connect test WebSocket
        ws = ws_lib.create_connection(WS_URL, timeout=5)
        first_msg = ws.recv()
        try:
            info = json.loads(first_msg)
            known_guests.add(info.get("route", ""))
        except Exception:
            pass

        # --- test 1: blackbox auto-starts and records RPC calls ---
        print("server-side recording:")

        # wait for blackbox auto-start (2 second delay)
        time.sleep(3)

        # make some RPC calls that the server blackbox should record
        ws_request(ws, "port")
        ws_request(ws, "features ()")
        ws_request(ws, "functions ()")

        # check that server-side blackbox is recording
        # (we query this via RPC eval — the function exists on the server)
        elapsed = ws_request(ws, "elapsed time ()")
        check_true("server elapsed time is a number", elapsed.replace(".", "").isdigit())
        check_true("server elapsed time > 0", float(elapsed) > 0)

        # --- test 2: open browser, do some actions, report fault ---
        print("client-side recording and fault report:")
        browser = Browser(headed)
        browsers.append(browser)
        guest = wait_for_browser(ws, known_guests)
        known_guests.add(guest or "")
        check_true("browser connected", guest is not None)

        # do some user actions via WebSocket commands
        ws_request(ws, 'click on (".logo")', to=guest)
        time.sleep(2)

        # close the login panel by pressing Escape (so we don't change state too much)
        ws_request(ws, 'press ("Escape") on ("body")', to=guest)
        time.sleep(1)

        # report a fault from the client
        fault_id = ws_request(ws, 'report fault ("the logo did something weird")', to=guest)
        check_true("fault ID returned", len(fault_id) == 8)
        check_true("fault ID is alphanumeric", fault_id.isalnum())

        # --- test 3: client uploads the fault, server receives it ---
        print("fault upload:")

        # trigger upload of pending faults from the client
        ws_request(ws, 'upload pending faults ()', to=guest)
        time.sleep(3)

        # check that the server has the fault report
        fault_data = ws_request(ws, f'get fault ("{fault_id}")')
        check_true("server has fault report", len(fault_data) > 10)

        if fault_data and not fault_data.startswith("error"):
            try:
                report = json.loads(fault_data)
                check_true("report has fault_id", report.get("fault_id") == fault_id)
                check_true("report has comment", "weird" in report.get("comment", ""))
                check_true("report has moments", len(report.get("moments", [])) > 0)

                # check that moments have the expected structure
                moments = report.get("moments", [])
                if moments:
                    first_moment = moments[0]
                    check_true("moment has keyframe", "keyframe" in first_moment)
                    check_true("moment has actions", "actions" in first_moment)
                    check_true("moment has start_time", "start_time" in first_moment)

                    # check that RPC actions were recorded
                    all_actions = []
                    for m in moments:
                        all_actions.extend(m.get("actions", []))
                    check_true("has recorded actions", len(all_actions) > 0)

                    if all_actions:
                        action = all_actions[0]
                        check_true("action has time", "time" in action)
                        check_true("action has kind", action.get("kind") == "call")
                        check_true("action has correlation", "correlation" in action)

                # check for server moments (attached when server receives the report)
                server_moments = report.get("server_moments", [])
                check_true("report has server moments", len(server_moments) > 0)

            except json.JSONDecodeError:
                results.append(("parse fault report JSON", False))
                print(f"  FAIL: parse fault report JSON (got {fault_data[:100]!r})")

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

    try:
        print("\n=== blackbox integration tests ===\n")
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
