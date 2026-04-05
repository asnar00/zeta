// Platform implementation: eval (TypeScript)
// Implements the functions declared in eval.zero.md
// Server-side stub — delegates to rpc eval

// @zero on (string result) = eval (string expr)
function fn_eval__string(expr: string): string {
    return fn_rpc_eval__string(expr);
}
