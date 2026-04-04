// Platform implementation: gui (TypeScript/web)
// Implements the functions declared in gui.zero.md

// @zero on (string result) = input (string prompt)
function fn_input__string(prompt: string): string {
    // For now, use window.prompt — a proper implementation would
    // create a styled input element in the DOM and await submission
    const result = (globalThis as any).prompt?.(prompt) ?? "";
    return result;
}

// @zero on set cookie of (string name) to (string value)
function fn_set_cookie_of__string_to__string(name: string, value: string): void {
    if (typeof document !== "undefined") {
        document.cookie = `${name}=${value}; path=/; SameSite=Strict`;
    }
}

// @zero on reload page ()
function fn_reload_page(): void {
    if (typeof location !== "undefined") {
        location.reload();
    }
}
