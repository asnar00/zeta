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
    # zero uses hyphens, Python uses underscores
    attr_name = name.replace("-", "_")
    # convert string value to appropriate type
    if value in ("true", "false"):
        setattr(mod, attr_name, value == "true")
    elif value.isdigit():
        setattr(mod, attr_name, int(value))
    else:
        setattr(mod, attr_name, value)


# @zero on exit process ()
def fn_exit_process():
    import os, threading
    # exit after a short delay so the HTTP response can be sent
    threading.Timer(0.5, lambda: os._exit(0)).start()


# @zero on (string value) = get feature var (string name)
def fn_get_feature_var__string(name: str) -> str:
    mod = _find_root_module()
    if mod is None:
        return ""
    attr_name = name.replace("-", "_")
    val = getattr(mod, attr_name, None)
    if val is None:
        return ""
    if isinstance(val, bool):
        return "true" if val else "false"
    return str(val)
