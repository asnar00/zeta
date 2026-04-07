// Platform implementation: runtime (TypeScript)
// Implements the functions declared in runtime.zero.md

// @shared-runtime-start
export const _test_registry: Map<string, [() => void, string][]> = new Map();

export function register_tests(feature: string, tests: [() => void, string][]): void {
    _test_registry.set(feature, tests);
}

export function run_tests(names?: string[]): number {
    let total = 0;
    for (const [feat, tests] of _test_registry) {
        if (names && names.length && !names.includes(feat)) continue;
        console.log(`${feat}:`);
        let passed = 0;
        let failed = 0;
        for (const [fn, desc] of tests) {
            try {
                fn();
                passed++;
                console.log(`  PASS ${desc}`);
            } catch (e: any) {
                failed++;
                console.log(`  FAIL ${desc}: ${e.message}`);
            }
        }
        console.log(`${feat}: ${passed} passed, ${failed} failed`);
        total += failed;
    }
    return total;
}
// @shared-runtime-end

const _sessions: Map<string, any> = new Map();
const _session_names: Map<string, string> = new Map();

// @zero on (string token) = create session (string name)
function fn_create_session__string(name: string): string {
    const existing = _session_names.get(name);
    if (existing) return existing;
    const token = Math.random().toString(36).slice(2, 10);
    const ctx = new (globalThis as any)._Context();
    _sessions.set(token, ctx);
    _session_names.set(name, token);
    return token;
}

// @zero on (string result) = random digits (int n)
function fn_random_digits__int(n: number): string {
    let result = "";
    for (let i = 0; i < n; i++) {
        result += Math.floor(Math.random() * 10).toString();
    }
    return result;
}

// @zero on set session (string token)
function fn_set_session__string(token: string): void {
    const ctx = _sessions.get(token);
    if (ctx && typeof (globalThis as any)._ctx_storage?.run === 'function') {
        // note: AsyncLocalStorage.run needs to wrap the request handler
        // for now, store as the default fallback
        (globalThis as any)._default_ctx = ctx;
    }
}

// @zero on exit process ()
function fn_exit_process(): void {
    setTimeout(() => process.exit(0), 500);
}

// @zero on (string result) = rpc eval (string expr)
function fn_rpc_eval__string(expr: string): string {
    return "error: rpc eval not implemented for TypeScript";
}


// @zero on (string json) = serialise [items$]
function fn_serialise(items: any): string {
    const data: any = {
        values: [...items].map(v => {
            if (v === null || v === undefined) return null;
            if (typeof v === "object" && !Array.isArray(v)) {
                const obj: any = {};
                for (const [k, val] of Object.entries(v)) {
                    if (!k.startsWith("_")) obj[k] = val;
                }
                return obj;
            }
            return v;
        }),
    };
    if (items.dt) data.dt = items.dt;
    if (items.capacity) data.capacity = items.capacity;
    if (items.t0) data.t0 = items.t0;
    if (items._timestamps?.length) data.timestamps = [...items._timestamps];
    return JSON.stringify(data);
}


// @zero on (string result$) = deserialise (string json)
function fn_deserialise__string(json_str: string): any {
    const data = JSON.parse(json_str);
    const values = data.values ?? [];
    // @ts-ignore — _Stream may or may not be defined depending on build
    const result: any = typeof _Stream !== "undefined" ? new (_Stream as any)(values) : [...values];
    if (data.dt) result.dt = data.dt;
    if (data.capacity) result.capacity = data.capacity;
    if (data.t0) result.t0 = data.t0;
    if (data.timestamps && result._timestamps) result._timestamps = data.timestamps;
    return result;
}
