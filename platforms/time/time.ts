// Platform implementation: time (TypeScript)
// Implements the functions declared in time.zero.md


// @zero on (time t) = (number n) seconds
function fn__number_seconds(n: number): number {
    return n;
}


// @zero on (time t) = (number n) ms
function fn__number_ms(n: number): number {
    return n / 1000;
}


// @zero on (time t) = (number n) hz
function fn__number_hz(n: number): number {
    return 1 / n;
}


// @zero on (time t) = (number n) bpm
function fn__number_bpm(n: number): number {
    return 60 / n;
}


// @zero on (time t) = now ()
function fn_now(): number {
    return Date.now() / 1000;
}


// @zero on (time t) = dt of [items$]
function fn_dt_of(items: any): number {
    return items?.dt ?? 0;
}


// @zero on (time t) = capacity of [items$]
function fn_capacity_of(items: any): number {
    return items?.capacity ?? 0;
}


// @zero on (time t) = t0 of [items$]
function fn_t0_of(items: any): number {
    return items?.t0 ?? 0;
}
