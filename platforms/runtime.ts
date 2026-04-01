// Platform implementation: runtime (TypeScript)
// Implements the functions declared in runtime.zero.md

// @zero on exit process ()
function fn_exit_process(): void {
    setTimeout(() => process.exit(0), 500);
}

// @zero on (string result) = rpc eval (string expr)
function fn_rpc_eval__string(expr: string): string {
    return "error: rpc eval not implemented for TypeScript";
}
