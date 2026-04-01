# Platform implementation: runtime (Python)
# Implements the functions declared in runtime.zero.md

import sys


def _find_root_module():
    """Find the root website module (the one with __main__ entry point)."""
    for name, mod in sys.modules.items():
        if hasattr(mod, 'task_main__string') and name != __name__:
            return mod
    return None


# @zero on set feature var (string name) (string value)
def fn_set_feature_var__string__string(name: str, value: str):
    mod = _find_root_module()
    if mod is None:
        return
    # convert string value to appropriate type
    if value in ("true", "false"):
        setattr(mod, name, value == "true")
    elif value.isdigit():
        setattr(mod, name, int(value))
    else:
        setattr(mod, name, value)


# @zero on (string value) = get feature var (string name)
def fn_get_feature_var__string(name: str) -> str:
    mod = _find_root_module()
    if mod is None:
        return ""
    val = getattr(mod, name, None)
    if val is None:
        return ""
    if isinstance(val, bool):
        return "true" if val else "false"
    return str(val)
