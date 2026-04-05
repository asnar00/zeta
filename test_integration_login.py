"""Integration test for the login feature.

Implements the test spec from website/login/login.zero.md ## integration tests:
Three browser tabs, three users, three background colours.
Also tests the toggle login / logout workflow.

ALL tests run through the real Cloudflare HTTPS URL, not localhost.

Usage: python3 test_integration_login.py [--headed]
"""

import subprocess
import sys
import time
import os

BASE_URL = "https://test.xn--nb-lkaa.org"


def build_website():
    result = subprocess.run(
        [sys.executable, "zeta.py", "website/features.md", "website/output/"],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print("BUILD FAILED:")
        print(result.stderr)
        return False
    print(result.stdout.strip())
    return True


def start_server():
    # kill any existing server on port 8084
    import signal
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
    # wait for server to be reachable — first check localhost, then Cloudflare
    import urllib.request
    for _ in range(20):
        try:
            urllib.request.urlopen("http://localhost:8084/", timeout=1)
            break
        except Exception:
            time.sleep(0.3)
    else:
        print("SERVER FAILED TO START (not listening on 8084)")
        if proc.poll() is not None:
            print("STDERR:", proc.stderr.read().decode()[:500])
        proc.kill()
        return None
    # now wait for Cloudflare to route to it
    for _ in range(20):
        try:
            req = urllib.request.Request(f"{BASE_URL}/",
                headers={"User-Agent": "Mozilla/5.0 (Macintosh) Chrome/120"})
            urllib.request.urlopen(req, timeout=3)
            return proc
        except Exception:
            time.sleep(0.5)
    print("SERVER FAILED TO START (not reachable via Cloudflare)")
    if proc.poll() is not None:
        print("STDERR:", proc.stderr.read().decode()[:500])
    proc.kill()
    return None


def stop_server(proc):
    if proc:
        proc.terminate()
        try:
            proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            proc.kill()


def get_background_colour(page):
    return page.evaluate("""
        () => {
            const bg = window.getComputedStyle(document.body).backgroundColor;
            const match = bg.match(/rgb\\((\\d+),\\s*(\\d+),\\s*(\\d+)\\)/);
            if (!match) return bg;
            const r = parseInt(match[1]).toString(16).padStart(2, '0');
            const g = parseInt(match[2]).toString(16).padStart(2, '0');
            const b = parseInt(match[3]).toString(16).padStart(2, '0');
            return '#' + r + g + b;
        }
    """)


def set_background_via_rpc(page, colour):
    import urllib.parse
    expr = urllib.parse.quote(f'background.colour = {colour}')
    page.evaluate(f'() => fetch("{BASE_URL}/@rpc/{expr}", {{ credentials: "include" }})')
    page.reload()
    page.wait_for_load_state("networkidle")


def has_session_cookie(ctx):
    return any(c["name"] == "session" for c in ctx.cookies())


def login_via_browser(page, ctx, name, code):
    """Click logo, type name, type code. Returns True if login succeeded."""
    page.click(".logo")
    try:
        page.wait_for_selector("input", timeout=5000)
    except Exception:
        return False
    input_el = page.query_selector("input")
    input_el.fill(name)
    input_el.press("Enter")
    time.sleep(2)
    try:
        page.wait_for_selector("input", timeout=10000)
    except Exception:
        return False
    input_el = page.query_selector("input")
    input_el.fill(code)
    input_el.press("Enter")
    time.sleep(3)
    return has_session_cookie(ctx)


def logout_via_browser(page, ctx):
    """Click logo (should show logout dialog), click 'log out'."""
    page.click(".logo")
    try:
        page.wait_for_selector("button", timeout=5000)
    except Exception:
        return False
    buttons = page.query_selector_all("button")
    for b in buttons:
        if b.text_content() == "log out":
            b.click()
            break
    time.sleep(3)
    return not has_session_cookie(ctx)


def click_logo_shows_buttons(page):
    """Click logo and check if buttons appear (logout) rather than input (login)."""
    page.click(".logo")
    time.sleep(1)
    buttons = page.query_selector_all("button")
    inputs = page.query_selector_all("input")
    return len(buttons) > 0 and len(inputs) == 0


def run_tests(headed=False):
    from playwright.sync_api import sync_playwright

    results = []

    def check(name, actual, expected):
        ok = actual == expected
        results.append((name, ok))
        print(f"  {'PASS' if ok else 'FAIL'}: {name}" + (f" (got {actual!r}, expected {expected!r})" if not ok else ""))

    def check_true(name, value):
        results.append((name, bool(value)))
        print(f"  {'PASS' if value else 'FAIL'}: {name}" + ("" if value else f" (got {value!r})"))

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not headed)

        # --- default tab: no login, teal ---
        print("default tab:")
        ctx_default = browser.new_context()
        page_default = ctx_default.new_page()
        page_default.goto(f"{BASE_URL}/")
        page_default.wait_for_load_state("networkidle")
        check("default: teal background", get_background_colour(page_default), "#34988b")

        # --- alice: login, set red ---
        print("alice tab:")
        ctx_alice = browser.new_context()
        page_alice = ctx_alice.new_page()
        page_alice.goto(f"{BASE_URL}/")
        page_alice.wait_for_load_state("networkidle")
        logged_in = login_via_browser(page_alice, ctx_alice, "_alice", "1234")
        check_true("alice: logged in", logged_in)
        set_background_via_rpc(page_alice, "#ff0000")
        check("alice: red background", get_background_colour(page_alice), "#ff0000")

        # --- alice: click logo again shows logout dialog ---
        print("alice toggle:")
        shows_buttons = click_logo_shows_buttons(page_alice)
        check_true("alice: logo click shows logout buttons", shows_buttons)
        page_alice.reload()
        page_alice.wait_for_load_state("networkidle")

        # --- bob: login, set blue ---
        print("bob tab:")
        ctx_bob = browser.new_context()
        page_bob = ctx_bob.new_page()
        page_bob.goto(f"{BASE_URL}/")
        page_bob.wait_for_load_state("networkidle")
        logged_in = login_via_browser(page_bob, ctx_bob, "_bob", "4321")
        check_true("bob: logged in", logged_in)
        set_background_via_rpc(page_bob, "#0000ff")
        check("bob: blue background", get_background_colour(page_bob), "#0000ff")

        # --- isolation ---
        print("isolation:")
        page_default.reload()
        page_default.wait_for_load_state("networkidle")
        check("default: still teal", get_background_colour(page_default), "#34988b")

        page_alice.reload()
        page_alice.wait_for_load_state("networkidle")
        check("alice: still red", get_background_colour(page_alice), "#ff0000")

        page_bob.reload()
        page_bob.wait_for_load_state("networkidle")
        check("bob: still blue", get_background_colour(page_bob), "#0000ff")

        # --- alice: logout ---
        print("alice logout:")
        logged_out = logout_via_browser(page_alice, ctx_alice)
        check_true("alice: logged out (cookie cleared)", logged_out)
        page_alice.goto(f"{BASE_URL}/")
        page_alice.wait_for_load_state("networkidle")
        check("alice: back to teal after logout", get_background_colour(page_alice), "#34988b")

        # --- alice: logo after logout shows login ---
        print("alice after logout:")
        page_alice.click(".logo")
        time.sleep(1)
        has_input = page_alice.query_selector("input") is not None
        has_buttons = len(page_alice.query_selector_all("button")) > 0
        check_true("alice: logo shows login input after logout", has_input and not has_buttons)

        # --- bob: still logged in ---
        print("bob still active:")
        page_bob.reload()
        page_bob.wait_for_load_state("networkidle")
        check("bob: still blue after alice logout", get_background_colour(page_bob), "#0000ff")

        browser.close()

    passed = sum(1 for _, ok in results if ok)
    failed = sum(1 for _, ok in results if not ok)
    print(f"\n{passed} passed, {failed} failed")
    return failed == 0


if __name__ == "__main__":
    headed = "--headed" in sys.argv

    print("=== building website ===")
    if not build_website():
        sys.exit(1)

    print("\n=== starting server ===")
    server = start_server()
    if not server:
        sys.exit(1)

    try:
        print("\n=== running integration tests ===")
        success = run_tests(headed)
    finally:
        print("\n=== stopping server ===")
        stop_server(server)

    sys.exit(0 if success else 1)
