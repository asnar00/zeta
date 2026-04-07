# Platform implementation: time (Python)
# Implements the functions declared in time.zero.md

import time as _time


# @zero on (time t) = (number n) seconds
def fn__number_seconds(n: float) -> float:
    return float(n)


# @zero on (time t) = (number n) ms
def fn__number_ms(n: float) -> float:
    return float(n) / 1000.0


# @zero on (time t) = (number n) hz
def fn__number_hz(n: float) -> float:
    return 1.0 / float(n)


# @zero on (time t) = (number n) bpm
def fn__number_bpm(n: float) -> float:
    return 60.0 / float(n)


# @zero on (time t) = now ()
def fn_now() -> float:
    return _time.time()


# @zero on (time t) = dt of [items$]
def fn_dt_of(items) -> float:
    return getattr(items, 'dt', 0.0)


# @zero on (time t) = capacity of [items$]
def fn_capacity_of(items) -> float:
    return getattr(items, 'capacity', 0.0)


# @zero on (time t) = t0 of [items$]
def fn_t0_of(items) -> float:
    return getattr(items, 't0', 0.0)
