// Platform implementation: gui (TypeScript/web)
// Implements the functions declared in gui.zero.md

// @zero on (string result) = input (string prompt)
function fn_input__string(prompt: string): string {
    return (globalThis as any).prompt?.(prompt) ?? "";
}

// @zero on show message (string text)
function fn_show_message__string(text: string): void {
    if (typeof alert !== "undefined") {
        alert(text);
    } else {
        console.log(text);
    }
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
