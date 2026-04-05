"""Integration test for the login feature.

Implements the test spec from website/login/login.zero.md ## integration tests:
Three browser tabs, three users, three background colours.
Also tests the toggle login / logout workflow.

Usage: python3 test_integration_login.py [--headed]
"""

import subprocess
import sys
import time
import os


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


def start_server(port=8084):
    # clear old sessions for clean test
    sessions_path = os.path.join("website", "output", "sessions.json")
    if os.path.exists(sessions_path):
        os.remove(sessions_path)
    proc = subprocess.Popen(
        [sys.executable, "website/output/website.py"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    for _ in range(30):
        try:
            import urllib.request
            urllib.request.urlopen(f"http://localhost:{port}/", timeout=1)
            return proc
        except Exception:
            time.sleep(0.2)
    print("SERVER FAILED TO START")
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


def set_background_via_rpc(page, colour, port=8084):
    import urllib.parse
    expr = urllib.parse.quote(f'background.colour = {colour}')
    page.evaluate(f'() => fetch("http://localhost:{port}/@rpc/{expr}", {{ credentials: "include" }})')
    page.reload()
    page.wait_for_load_state("networkidle")


def has_session_cookie(ctx):
    return any(c["name"] == "session" for c in ctx.cookies())


def login_via_browser(page, ctx, name, code, port=8084):
    """Click logo, type name, type code. Returns True if login succeeded (cookie set)."""
    page.click(".logo")
    try:
        page.wait_for_selector("input", timeout=5000)
    except Exception:
        return False
    input_el = page.query_selector("input")
    input_el.fill(name)
    input_el.press("Enter")
    time.sleep(1)
    try:
        page.wait_for_selector("input", timeout=10000)
    except Exception:
        return False
    input_el = page.query_selector("input")
    input_el.fill(code)
    input_el.press("Enter")
    time.sleep(2)
    return has_session_cookie(ctx)


def logout_via_browser(page, ctx):
    """Click logo (should show logout dialog), click 'log out'. Returns True if logged out."""
    page.click(".logo")
    try:
        page.wait_for_selector("button", timeout=5000)
    except Exception:
        return False
    buttons = page.query_selector_all("button")
    labels = [b.text_content() for b in buttons]
    if "log out" not in labels:
        return False
    for b in buttons:
        if b.text_content() == "log out":
            b.click()
            break
    time.sleep(2)
    return not has_session_cookie(ctx)


def click_logo_shows_buttons(page):
    """Click logo and check if buttons appear (logout dialog) rather than input (login)."""
    page.click(".logo")
    time.sleep(1)
    buttons = page.query_selector_all("button")
    inputs = page.query_selector_all("input")
    return len(buttons) > 0 and len(inputs) == 0


def run_tests(headed=False, port=8084):
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
        page_default.goto(f"http://localhost:{port}/")
        page_default.wait_for_load_state("networkidle")
        check("default: teal background", get_background_colour(page_default), "#34988b")

        # --- alice: login, set red ---
        print("alice tab:")
        ctx_alice = browser.new_context()
        page_alice = ctx_alice.new_page()
        page_alice.goto(f"http://localhost:{port}/")
        page_alice.wait_for_load_state("networkidle")
        logged_in = login_via_browser(page_alice, ctx_alice, "_alice", "1234", port)
        check_true("alice: logged in", logged_in)
        set_background_via_rpc(page_alice, "#ff0000", port)
        check("alice: red background", get_background_colour(page_alice), "#ff0000")

        # --- alice: click logo again shows logout dialog, not login ---
        print("alice toggle:")
        shows_buttons = click_logo_shows_buttons(page_alice)
        check_true("alice: logo click shows logout buttons", shows_buttons)
        # dismiss by reloading
        page_alice.reload()
        page_alice.wait_for_load_state("networkidle")

        # --- bob: login, set blue ---
        print("bob tab:")
        ctx_bob = browser.new_context()
        page_bob = ctx_bob.new_page()
        page_bob.goto(f"http://localhost:{port}/")
        page_bob.wait_for_load_state("networkidle")
        logged_in = login_via_browser(page_bob, ctx_bob, "_bob", "4321", port)
        check_true("bob: logged in", logged_in)
        set_background_via_rpc(page_bob, "#0000ff", port)
        check("bob: blue background", get_background_colour(page_bob), "#0000ff")

        # --- isolation checks ---
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
        page_alice.goto(f"http://localhost:{port}/")
        page_alice.wait_for_load_state("networkidle")
        check("alice: back to teal after logout", get_background_colour(page_alice), "#34988b")

        # --- alice: click logo after logout shows login, not logout ---
        print("alice after logout:")
        page_alice.click(".logo")
        time.sleep(1)
        has_input = page_alice.query_selector("input") is not None
        has_buttons = len(page_alice.query_selector_all("button")) > 0
        check_true("alice: logo click shows login input after logout", has_input and not has_buttons)

        # --- bob: still logged in with blue ---
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
