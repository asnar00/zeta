// Platform implementation: runtime (TypeScript)
// Implements the functions declared in runtime.zero.md

// @zero on set feature var (string name) (string value)
function fn_set_feature_var__string__string(name: string, value: string): void {
    // set the variable on the global scope
    if (value === "true" || value === "false") {
        (globalThis as any)[name] = value === "true";
    } else if (/^\d+$/.test(value)) {
        (globalThis as any)[name] = parseInt(value);
    } else {
        (globalThis as any)[name] = value;
    }
}

// @zero on exit process ()
function fn_exit_process(): void {
    setTimeout(() => process.exit(0), 500);
}

// @zero on (string value) = get feature var (string name)
function fn_get_feature_var__string(name: string): string {
    const val = (globalThis as any)[name];
    if (val === undefined) return "";
    if (typeof val === "boolean") return val ? "true" : "false";
    return String(val);
}
