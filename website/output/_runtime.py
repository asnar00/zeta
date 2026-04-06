_test_registry = {}

def register_tests(feature, tests):
    _test_registry[feature] = tests

def _call_expr(desc):
    """Extract the call expression (LHS of =>) from a test description."""
    if " => " in desc:
        return desc[:desc.index(" => ")].strip()
    return desc

def run_tests(names=None):
    results = []
    for feat, tests in _test_registry.items():
        if names and feat not in names:
            continue
        for fn, desc in tests:
            try:
                fn()
                results.append((feat, desc, True, None))
            except Exception as e:
                results.append((feat, desc, False, str(e).split("\n")[0]))
    passing_calls = set()
    for feat, desc, passed, err in results:
        if passed:
            passing_calls.add(_call_expr(desc))
    total_fail = 0
    current_feat = None
    passed = failed = overridden = 0
    for feat, desc, ok, err in results:
        if feat != current_feat:
            if current_feat is not None:
                _print_feat_summary(current_feat, passed, failed, overridden)
            current_feat = feat
            passed = failed = overridden = 0
            print(f"{feat}:")
        if ok:
            passed += 1
            print(f"  PASS {desc}")
        elif _call_expr(desc) in passing_calls:
            overridden += 1
            print(f"  OVERRIDDEN {desc}")
        else:
            failed += 1
            total_fail += 1
            print(f"  FAIL {desc}: {err}")
    if current_feat is not None:
        _print_feat_summary(current_feat, passed, failed, overridden)
    return total_fail

def _print_feat_summary(feat, passed, failed, overridden):
    parts = [f"{passed} passed"]
    if failed:
        parts.append(f"{failed} failed")
    if overridden:
        parts.append(f"{overridden} overridden")
    print(f"{feat}: {', '.join(parts)}")
