"""Integration test for the login feature.

Implements the test spec from website/login/login.zero.md ## integration tests:
Three browser tabs, three users, three background colours.

Usage: python3 test_integration_login.py [--headed]
"""

import subprocess
import sys
import time
import os


def build_website():
    """Build the website from features."""
    result = subprocess.run(
        [sys.executable, "zeta.py", "website/features.md", "website/output/"],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print("BUILD FAILED:")
        print(result.stdout)
        print(result.stderr)
        return False
    print(result.stdout.strip())
    return True


def start_server(port=8084):
    """Start the website server, return the process."""
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
    """Stop the server process."""
    if proc:
        proc.terminate()
        try:
            proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            proc.kill()


def get_background_colour(page):
    """Get the background colour of the page body as a hex string."""
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
    """Set background colour via RPC for the current session."""
    import urllib.parse
    expr = urllib.parse.quote(f'background.colour = {colour}')
    page.evaluate(f'() => fetch("http://localhost:{port}/@rpc/{expr}", {{ credentials: "include" }})')
    page.reload()
    page.wait_for_load_state("networkidle")


def login_via_browser(page, name, code, port=8084):
    """Log in by clicking the logo and typing into DOM input elements.
    The login flow: click logo -> input box appears (name) -> type + Enter ->
    RPC to server -> input box appears (code) -> type + Enter ->
    RPC to server -> cookie set -> page reloads."""
    initial_url = page.url
    # click the logo to start the login flow
    page.click(".logo")
    # wait for the name input to appear
    try:
        page.wait_for_selector("input", timeout=5000)
    except Exception:
        return False
    # type the name and press Enter
    input_el = page.query_selector("input")
    input_el.fill(name)
    input_el.press("Enter")
    # the first input disappears, then the RPC completes, then the code input appears
    # wait for the old input to be removed and a new one to appear
    time.sleep(1)
    try:
        page.wait_for_selector("input", timeout=10000)
    except Exception:
        return False
    # type the code and press Enter
    input_el = page.query_selector("input")
    input_el.fill(code)
    input_el.press("Enter")
    # wait for page reload (login success sets cookie and reloads)
    time.sleep(2)
    return True


def run_tests(headed=False, port=8084):
    """Run the integration tests from login.zero.md."""
    from playwright.sync_api import sync_playwright

    results = []

    def check(name, actual, expected):
        ok = actual == expected
        status = "PASS" if ok else "FAIL"
        results.append((name, ok))
        print(f"  {status}: {name} (got {actual}, expected {expected})")
        return ok

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not headed)

        # --- Tab 1: default (no login) ---
        print("default tab:")
        ctx_default = browser.new_context()
        page_default = ctx_default.new_page()
        page_default.goto(f"http://localhost:{port}/")
        page_default.wait_for_load_state("networkidle")
        check("default background is teal",
              get_background_colour(page_default), "#34988b")

        # --- Tab 2: alice ---
        print("alice tab:")
        ctx_alice = browser.new_context()
        page_alice = ctx_alice.new_page()
        page_alice.goto(f"http://localhost:{port}/")
        page_alice.wait_for_load_state("networkidle")
        logged_in = login_via_browser(page_alice, "_alice", "1234", port)
        print(f"  alice logged in: {logged_in}")
        # set alice's background to red
        set_background_via_rpc(page_alice, "#ff0000", port)
        check("alice background is red",
              get_background_colour(page_alice), "#ff0000")

        # --- Tab 3: bob ---
        print("bob tab:")
        ctx_bob = browser.new_context()
        page_bob = ctx_bob.new_page()
        page_bob.goto(f"http://localhost:{port}/")
        page_bob.wait_for_load_state("networkidle")
        logged_in = login_via_browser(page_bob, "_bob", "4321", port)
        print(f"  bob logged in: {logged_in}")
        # set bob's background to blue
        set_background_via_rpc(page_bob, "#0000ff", port)
        check("bob background is blue",
              get_background_colour(page_bob), "#0000ff")

        # --- verify isolation: switch back to each tab ---
        print("isolation checks:")
        page_default.reload()
        page_default.wait_for_load_state("networkidle")
        check("default still teal",
              get_background_colour(page_default), "#34988b")

        page_alice.reload()
        page_alice.wait_for_load_state("networkidle")
        check("alice still red",
              get_background_colour(page_alice), "#ff0000")

        page_bob.reload()
        page_bob.wait_for_load_state("networkidle")
        check("bob still blue",
              get_background_colour(page_bob), "#0000ff")

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

    print("\n=== starting server on port 8084 ===")
    server = start_server(8084)
    if not server:
        sys.exit(1)

    try:
        print("\n=== running integration tests ===")
        success = run_tests(headed)
    finally:
        print("\n=== stopping server ===")
        stop_server(server)

    sys.exit(0 if success else 1)
