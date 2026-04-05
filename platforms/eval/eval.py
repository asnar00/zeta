# Platform implementation: eval (Python)
# Implements the functions declared in eval.zero.md
# Server-side: delegates to the existing rpc eval machinery in runtime.py


# @zero on (string result) = eval (string expr)
def fn_eval__string(expr: str) -> str:
    # delegate to rpc eval which has the full implementation
    return fn_rpc_eval__string(expr)


# functions() and features() are already implemented in runtime.py
# and will be available in the compiled output
