// Platform implementation: blackbox (browser/client)
// Flight recorder: circular buffer of moments, auto-records RPC calls.

// --- buffer state ---
const _bb_moments = [];
const _bb_max_moments = 6;
const _bb_moment_duration = 10000;
let _bb_current_moment = null;
let _bb_start_time = performance.now();
let _bb_recording = false;
let _bb_tick_timer = null;
let _bb_session_id = "";
let _bb_fingerprint = "";
let _bb_correlation_counter = 0;


// --- time ---

function _bb_elapsed() {
    return Math.round(performance.now() - _bb_start_time);
}


function _bb_next_correlation() {
    _bb_correlation_counter++;
    return "c" + _bb_correlation_counter.toString(36);
}


// --- moment management ---

function _bb_new_moment() {
    return {
        start_time: _bb_elapsed(),
        keyframe: _bb_capture_keyframe(),
        actions: []
    };
}


function _bb_rotate_moment() {
    if (!_bb_recording) return;
    if (_bb_current_moment) {
        _bb_current_moment.end_time = _bb_elapsed();
        _bb_moments.push(_bb_current_moment);
        if (_bb_moments.length > _bb_max_moments) {
            _bb_moments.shift();
        }
    }
    _bb_current_moment = _bb_new_moment();
}


function _bb_capture_keyframe() {
    // serialise all fn_ variables on window as a state snapshot
    const state = {};
    for (const name of Object.getOwnPropertyNames(window)) {
        if (typeof window[name] === "function") continue;
        if (name.startsWith("_")) continue;
        const val = window[name];
        if (typeof val === "string" || typeof val === "number" || typeof val === "boolean") {
            state[name] = val;
        }
    }
    return JSON.stringify(state);
}


// --- recording ---

function _bb_record(action) {
    if (!_bb_recording || !_bb_current_moment) return;
    _bb_current_moment.actions.push(action);
}


// --- boundary instrumentation ---
// wraps _rpc and _client_eval to record every boundary-crossing call

function _bb_install_hooks() {
    // hook outgoing RPC calls (client -> server)
    if (typeof _rpc === "function") {
        const _original_rpc = _rpc;
        window._rpc = async function(expr) {
            const correlation = _bb_next_correlation();
            const t0 = _bb_elapsed();
            let result, error;
            try {
                result = await _original_rpc(expr);
                return result;
            } catch (e) {
                error = e;
                throw e;
            } finally {
                const elapsed = _bb_elapsed() - t0;
                _bb_record({
                    time: t0,
                    feature: "rpc",
                    name: expr,
                    args: "",
                    correlation: correlation,
                    result: error ? "error: " + error.message : (result || ""),
                    elapsed: elapsed,
                    kind: "call"
                });
            }
        };
    }

    // hook incoming commands (server -> client via WebSocket)
    if (typeof _client_eval === "function") {
        const _original_client_eval = _client_eval;
        window._client_eval = async function(expr) {
            const correlation = _bb_next_correlation();
            const t0 = _bb_elapsed();
            let result, error;
            try {
                result = await _original_client_eval(expr);
                return result;
            } catch (e) {
                error = e;
                throw e;
            } finally {
                const elapsed = _bb_elapsed() - t0;
                _bb_record({
                    time: t0,
                    feature: "client-eval",
                    name: expr,
                    args: "",
                    correlation: correlation,
                    result: error ? "error: " + error.message : (result || ""),
                    elapsed: elapsed,
                    kind: "call"
                });
            }
        };
    }
}


// --- user-facing API ---

function _bb_start(session) {
    _bb_session_id = session;
    _bb_start_time = performance.now();
    _bb_recording = true;
    _bb_moments.length = 0;
    _bb_current_moment = _bb_new_moment();
    _bb_tick_timer = setInterval(_bb_rotate_moment, _bb_moment_duration);
    _bb_install_hooks();
    console.log("[blackbox] recording started for session: " + session);
}


function _bb_stop() {
    _bb_recording = false;
    if (_bb_tick_timer) {
        clearInterval(_bb_tick_timer);
        _bb_tick_timer = null;
    }
    _bb_current_moment = null;
    _bb_moments.length = 0;
    console.log("[blackbox] recording stopped");
}


function _bb_freeze(comment) {
    if (!_bb_recording) return null;
    // close current moment
    if (_bb_current_moment) {
        _bb_current_moment.end_time = _bb_elapsed();
        _bb_moments.push(_bb_current_moment);
        if (_bb_moments.length > _bb_max_moments) {
            _bb_moments.shift();
        }
    }
    // copy moments
    const frozen_moments = _bb_moments.map(m => ({
        start_time: m.start_time,
        end_time: m.end_time,
        keyframe: m.keyframe,
        actions: m.actions.slice()
    }));
    // generate fault ID
    const fault_id = Math.random().toString(36).slice(2, 10);
    const report = {
        fault_id: fault_id,
        session: _bb_session_id,
        comment: comment,
        fingerprint: _bb_fingerprint,
        moments: frozen_moments,
        reported_at: _bb_elapsed()
    };
    // persist locally
    try {
        localStorage.setItem("blackbox:fault:" + fault_id, JSON.stringify(report));
    } catch (e) {
        console.log("[blackbox] persist error: " + e.message);
    }
    // start fresh buffer
    _bb_moments.length = 0;
    _bb_current_moment = _bb_new_moment();
    console.log("[blackbox] fault reported: " + fault_id);
    return fault_id;
}


async function _bb_upload_pending() {
    for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i);
        if (!key || !key.startsWith("blackbox:fault:")) continue;
        const data = localStorage.getItem(key);
        try {
            const resp = await fetch("/@rpc/report%20fault%20(" +
                encodeURIComponent('"' + data + '"') + ")");
            if (resp.ok) {
                localStorage.removeItem(key);
                console.log("[blackbox] uploaded: " + key);
            }
        } catch (e) {
            console.log("[blackbox] upload failed, will retry: " + e.message);
        }
    }
}


// --- client-callable functions ---

// @zero on (string fault) = report fault (string comment)
function fn_report_fault__string(comment) {
    return _bb_freeze(comment) || "";
}

// @zero on start recording (string session)
function fn_start_recording__string(session) {
    _bb_start(session);
}

// @zero on stop recording ()
function fn_stop_recording() {
    _bb_stop();
}

// @zero on upload pending faults ()
function fn_upload_pending_faults() {
    _bb_upload_pending();
}


// --- auto-start on page load ---
document.addEventListener("DOMContentLoaded", () => {
    // auto-start recording with session from cookie, or a random session ID
    let session = "";
    const match = document.cookie.match(/(?:^|;\s*)session=([^;]+)/);
    if (match) session = match[1];
    if (!session) session = "anon-" + Math.random().toString(36).slice(2, 8);
    _bb_start(session);

    // try uploading any pending faults from previous sessions
    setTimeout(_bb_upload_pending, 5000);
});
