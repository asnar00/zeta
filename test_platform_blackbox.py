"""Platform tests for blackbox.zero.md.

Tests the blackbox platform primitives: monotonic clock, periodic timers,
and local key-value persistence.

These are local-only tests — no server or network required.

Usage: python3 test_platform_blackbox.py
"""

import sys
import os
import time
import threading

# import the platform module directly
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "platforms", "blackbox"))
import blackbox  # noqa: E402


def run_tests():
    results = []

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

    # --- elapsed time ---
    print("elapsed time:")
    t0 = blackbox.fn_elapsed_time()
    check_true("elapsed time returns a number", isinstance(t0, float))
    check_true("elapsed time is non-negative", t0 >= 0)
    time.sleep(0.05)
    t1 = blackbox.fn_elapsed_time()
    check_true("elapsed time increases after sleep", t1 > t0)
    delta = t1 - t0
    check_true("elapsed time delta is roughly 50ms", 30 < delta < 200)

    # --- store / retrieve / remove ---
    print("store and retrieve:")
    blackbox.fn_store_locally__string__string("test:alpha", "hello")
    check("retrieve stored value", blackbox.fn_retrieve_locally__string("test:alpha"), "hello")

    blackbox.fn_store_locally__string__string("test:alpha", "updated")
    check("overwrite and retrieve", blackbox.fn_retrieve_locally__string("test:alpha"), "updated")

    check("retrieve missing key returns empty", blackbox.fn_retrieve_locally__string("test:nonexistent"), "")

    blackbox.fn_remove_locally__string("test:alpha")
    check("retrieve after remove returns empty", blackbox.fn_retrieve_locally__string("test:alpha"), "")

    # --- stored keys ---
    print("stored keys:")
    blackbox.fn_store_locally__string__string("bb:one", "1")
    blackbox.fn_store_locally__string__string("bb:two", "2")
    blackbox.fn_store_locally__string__string("bb:three", "3")
    blackbox.fn_store_locally__string__string("other:x", "x")

    keys = blackbox.fn_stored_keys__string("bb:")
    key_list = keys.split(",")
    check_true("stored keys finds 3 bb: keys", len(key_list) == 3)
    check_true("keys are sorted", key_list == sorted(key_list))
    check_true("bb:one in keys", "bb:one" in key_list)
    check_true("bb:two in keys", "bb:two" in key_list)
    check_true("bb:three in keys", "bb:three" in key_list)

    other_keys = blackbox.fn_stored_keys__string("other:")
    check("stored keys prefix filters correctly", other_keys, "other:x")

    empty_keys = blackbox.fn_stored_keys__string("nonexistent:")
    check("stored keys returns empty for no match", empty_keys, "")

    # cleanup
    blackbox.fn_remove_locally__string("bb:one")
    blackbox.fn_remove_locally__string("bb:two")
    blackbox.fn_remove_locally__string("bb:three")
    blackbox.fn_remove_locally__string("other:x")

    # --- timers ---
    print("timers:")
    counter = {"value": 0}
    counter_lock = threading.Lock()

    # register a callable that the timer can find
    def fn_test_tick():
        with counter_lock:
            counter["value"] += 1

    # patch it into the blackbox module's resolution path
    blackbox.fn_test_tick = fn_test_tick
    sys.modules['__main__'].fn_test_tick = fn_test_tick

    timer_id = blackbox.fn_every__number_do__string(50, "test tick")
    check_true("every returns a timer ID", timer_id.startswith("timer-"))

    time.sleep(0.35)
    blackbox.fn_cancel_timer__string(timer_id)
    time.sleep(0.1)  # let any in-flight tick finish

    with counter_lock:
        final_count = counter["value"]
    check_true("timer fired multiple times", final_count >= 3)
    check_true("timer fired reasonable number of times", final_count <= 10)

    # verify timer actually stopped
    snapshot = final_count
    time.sleep(0.2)
    with counter_lock:
        after_cancel = counter["value"]
    check("timer stops after cancel", after_cancel, snapshot)

    # cleanup
    if hasattr(blackbox, 'fn_test_tick'):
        delattr(blackbox, 'fn_test_tick')
    if hasattr(sys.modules['__main__'], 'fn_test_tick'):
        delattr(sys.modules['__main__'], 'fn_test_tick')

    # --- persistence across reload ---
    print("persistence:")
    blackbox.fn_store_locally__string__string("persist:check", "survives")
    # force a reload of the store
    blackbox._store = {}
    blackbox._load_store()
    check("value survives store reload", blackbox.fn_retrieve_locally__string("persist:check"), "survives")
    blackbox.fn_remove_locally__string("persist:check")

    # --- summary ---
    passed = sum(1 for _, ok in results if ok)
    failed = sum(1 for _, ok in results if not ok)
    print(f"\n{passed} passed, {failed} failed")
    return failed == 0


if __name__ == "__main__":
    print("=== blackbox platform tests ===\n")
    success = run_tests()

    # clean up the store file
    if blackbox._store_path and os.path.exists(blackbox._store_path):
        os.remove(blackbox._store_path)

    sys.exit(0 if success else 1)
