"""Platform tests for gui.zero.md.

Implements the integration test spec from platforms/gui.zero.md.
Tests the browser-side gui platform functions using Playwright.

Usage: python3 test_platform_gui.py [--headed]
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
    return True


def start_server(port=8084):
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
    proc.kill()
    return None


def stop_server(proc):
    if proc:
        proc.terminate()
        try:
            proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            proc.kill()


def run_tests(headed=False, port=8084):
    from playwright.sync_api import sync_playwright

    results = []

    def check(name, actual, expected):
        ok = actual == expected
        status = "PASS" if ok else "FAIL"
        results.append((name, ok))
        print(f"  {status}: {name}" + (f" (got {actual!r}, expected {expected!r})" if not ok else ""))
        return ok

    def check_true(name, value):
        status = "PASS" if value else "FAIL"
        results.append((name, value))
        print(f"  {status}: {name}" + ("" if value else f" (got {value!r})"))
        return value

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not headed)
        url = f"http://localhost:{port}/"

        # --- input: shows labelled text field, returns typed value, removes itself ---
        print("input (basic):")
        ctx = browser.new_context()
        page = ctx.new_page()
        page.goto(url)
        page.wait_for_load_state("networkidle")

        # call input("name") by injecting JS
        page.evaluate("window._testResult = null; fn_input__string('name').then(r => window._testResult = r);")
        time.sleep(0.5)

        # check: div contains text "name"
        label = page.query_selector("div:last-child div")
        check_true("input: label contains 'name'",
                    label is not None and "name" in (label.text_content() or ""))

        # check: input element exists
        inp = page.query_selector("input")
        check_true("input: input element exists", inp is not None)

        # check: input is focused
        is_focused = page.evaluate("document.activeElement?.tagName === 'INPUT'")
        check_true("input: input is focused", is_focused)

        # type "alice" and press Enter
        inp.fill("alice")
        inp.press("Enter")
        time.sleep(0.5)

        # check: result is "alice"
        result = page.evaluate("window._testResult")
        check("input: returns typed value", result, "alice")

        # check: input element is removed
        inp_after = page.query_selector("input")
        check_true("input: input element removed after submit", inp_after is None)

        ctx.close()

        # --- input: empty submission ---
        print("input (empty):")
        ctx = browser.new_context()
        page = ctx.new_page()
        page.goto(url)
        page.wait_for_load_state("networkidle")

        page.evaluate("window._testResult = null; fn_input__string('code').then(r => window._testResult = r);")
        time.sleep(0.5)
        inp = page.query_selector("input")
        inp.press("Enter")
        time.sleep(0.5)
        result = page.evaluate("window._testResult")
        check("input: empty returns empty string", result, "")

        ctx.close()

        # --- show message: displays alert ---
        print("show message:")
        ctx = browser.new_context()
        page = ctx.new_page()
        page.goto(url)
        page.wait_for_load_state("networkidle")

        alert_text = []
        page.on("dialog", lambda d: (alert_text.append(d.message), d.accept()))
        page.evaluate("fn_show_message__string('hello')")
        time.sleep(0.5)
        check("show message: alert with correct text",
              alert_text[0] if alert_text else None, "hello")

        ctx.close()

        # --- set cookie: stores value ---
        print("set cookie:")
        ctx = browser.new_context()
        page = ctx.new_page()
        page.goto(url)
        page.wait_for_load_state("networkidle")

        page.evaluate("fn_set_cookie_of__string_to__string('test', 'abc')")
        cookies = {c["name"]: c["value"] for c in ctx.cookies()}
        check("set cookie: cookie stored", cookies.get("test"), "abc")

        ctx.close()

        # --- reload page: triggers navigation ---
        print("reload page:")
        ctx = browser.new_context()
        page = ctx.new_page()
        page.goto(url)
        page.wait_for_load_state("networkidle")

        # set a marker, reload, check marker is gone
        page.evaluate("window._marker = 'before'")
        before = page.evaluate("window._marker")
        check("reload page: marker set", before, "before")

        page.evaluate("fn_reload_page()")
        time.sleep(1)
        page.wait_for_load_state("networkidle")
        after = page.evaluate("window._marker")
        check_true("reload page: marker gone after reload", after is None or after != "before")

        ctx.close()
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
        print("\n=== gui platform tests ===")
        success = run_tests(headed)
    finally:
        print("\n=== stopping server ===")
        stop_server(server)

    sys.exit(0 if success else 1)
