_test_registry = {}

def register_tests(feature, tests):
    _test_registry[feature] = tests

def run_tests(names=None):
    total = 0
    for feat, tests in _test_registry.items():
        if names and feat not in names:
            continue
        print(f"{feat}:")
        passed = failed = 0
        for fn, desc in tests:
            try:
                fn()
                passed += 1
                print(f"  PASS {desc}")
            except Exception as e:
                failed += 1
                print(f"  FAIL {desc}: {e}")
        print(f"{feat}: {passed} passed, {failed} failed")
        total += failed
    return total
