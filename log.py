"""Structured logging for the zeta translator pipeline."""

import sys
import time

_enabled = False
_depth = 0
_start_time = None


def enable():
    """Enable verbose logging."""
    global _enabled, _start_time
    _enabled = True
    _start_time = time.time()


def is_enabled() -> bool:
    return _enabled


def _elapsed() -> str:
    if _start_time is None:
        return ""
    return f"[{time.time() - _start_time:.3f}s] "


def log(msg: str):
    """Log a message at the current depth."""
    if not _enabled:
        return
    indent = "  " * _depth
    print(f"{_elapsed()}{indent}{msg}", file=sys.stderr)


def section(name: str):
    """Return a context manager that logs entry/exit of a named section."""
    return _Section(name)


class _Section:
    def __init__(self, name: str):
        self.name = name

    def __enter__(self):
        global _depth
        log(self.name)
        _depth += 1
        self._enter_time = time.time()
        return self

    def __exit__(self, *_):
        global _depth
        _depth -= 1
        if _enabled:
            elapsed = time.time() - self._enter_time
            log(f"done ({elapsed:.3f}s)")
