// Platform implementation: remote (browser/client)
// Provides RPC, expression evaluation, and WebSocket connection.

// --- RPC bridge ---
async function _rpc(expr) {
    const resp = await fetch("/@rpc/" + expr);
    return await resp.text();
}

// --- ZeroRaise for exception handling ---
class _ZeroRaise extends Error {
    constructor(name, args) {
        super(name + "(" + args.join(", ") + ")");
        this.zeroName = name;
        this.argsList = args || [];
    }
}

// --- expression evaluator ---
function _extract_fn_words(s) {
    const p = s.indexOf("(");
    return p === -1 ? s.trim() : s.slice(0, p).trim();
}

function _extract_args(s) {
    const args = [];
    let i = 0;
    while (i < s.length) {
        if (s[i] === "(") {
            let depth = 1, start = i + 1;
            i++;
            while (i < s.length && depth > 0) {
                if (s[i] === "(") depth++;
                else if (s[i] === ")") depth--;
                i++;
            }
            let inner = s.slice(start, i - 1).trim();
            if (inner === "") continue;
            if (inner.startsWith('"') && inner.endsWith('"')) inner = inner.slice(1, -1);
            args.push(inner);
        } else {
            i++;
        }
    }
    return args;
}

function _find_client_function(fn_words, arg_count) {
    const prefix = "fn_" + fn_words.replace(/ /g, "_").replace(/-/g, "_");
    for (const name of Object.getOwnPropertyNames(window)) {
        if (typeof window[name] !== "function") continue;
        if (name === prefix && arg_count === 0) return window[name];
        if (name.startsWith(prefix + "__")) {
            const typePart = name.slice(prefix.length + 2);
            const nTypes = (typePart.match(/__/g) || []).length + 1;
            if (nTypes === arg_count) return window[name];
        }
    }
    return null;
}

function _format_client_value(val) {
    if (val === null || val === undefined) return "ok";
    if (typeof val === "boolean") return val ? "true" : "false";
    return String(val);
}

function _list_client_functions() {
    const fns = [];
    for (const name of Object.getOwnPropertyNames(window)) {
        if (typeof window[name] === "function" && name.startsWith("fn_")) {
            fns.push(name.replace(/^fn_/, "").replace(/__/g, " (").replace(/_/g, " ") + (name.includes("__") ? ")" : " ()"));
        }
    }
    return fns.join("\n");
}

async function _client_eval(expr) {
    expr = expr.trim();
    if (!expr) return _list_client_functions();
    if (expr === "functions ()") return _list_client_functions();

    if (!expr.includes("(") && !expr.includes("=")) {
        const val = window[expr.replace(/-/g, "_")];
        return val !== undefined ? _format_client_value(val) : "error: " + expr + " not found";
    }

    const fn_words = _extract_fn_words(expr);
    const args = _extract_args(expr);
    const fn = _find_client_function(fn_words, args.length);
    if (!fn) return "error: function '" + fn_words + "' not found";
    try {
        const result = await fn(...args);
        return _format_client_value(result);
    } catch (e) {
        if (e instanceof _ZeroRaise) return "raise: " + e.zeroName;
        return "error: " + e.message;
    }
}

// --- WebSocket connection ---
let _ws = null;
let _ws_channel_id = null;

function _connect_ws() {
    const proto = location.protocol === "https:" ? "wss:" : "ws:";
    const url = proto + "//" + location.host + "/@ws";
    _ws = new WebSocket(url);
    _ws.onopen = () => console.log("[zero] ws connected");
    _ws.onmessage = async (event) => {
        const data = event.data;
        if (!_ws_channel_id) {
            try {
                const info = JSON.parse(data);
                _ws_channel_id = info.channel;
                console.log("[zero] ws connected as " + info.route + " (channel " + info.channel + ")");
            } catch (e) {
                _ws_channel_id = data;
                console.log("[zero] ws channel: " + data);
            }
            return;
        }
        try {
            const msg = JSON.parse(data);
            if (msg.cmd && msg.id) {
                const result = await _client_eval(msg.cmd);
                _ws.send(JSON.stringify({ id: msg.id, result: result }));
            }
        } catch (e) {
            console.log("[zero] ws message error: " + e);
        }
    };
    _ws.onclose = () => {
        console.log("[zero] ws disconnected, reconnecting...");
        _ws_channel_id = null;
        setTimeout(_connect_ws, 1000);
    };
}

// --- DOM wiring ---
document.addEventListener("DOMContentLoaded", () => {
    const logo = document.querySelector(".logo");
    if (logo) {
        logo.style.cursor = "pointer";
        logo.addEventListener("click", () => fn_logo_clicked());
    }
    _connect_ws();
});
