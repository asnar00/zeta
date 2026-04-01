// Platform implementation: runtime (TypeScript)
// Implements the functions declared in runtime.zero.md

const _sessions: Map<string, any> = new Map();

// @zero on (string token) = create session ()
function fn_create_session(): string {
    const token = Math.random().toString(36).slice(2, 10);
    // _Context will be available in the compiled output
    const ctx = new (globalThis as any)._Context();
    _sessions.set(token, ctx);
    return token;
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
