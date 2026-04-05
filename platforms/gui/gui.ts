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

// @zero on (string value) = get cookie (string name)
function fn_get_cookie__string(name: string): string {
    return "";
}

// @zero on clear cookie (string name)
function fn_clear_cookie__string(name: string): void {
}

// @zero on (string choice) = choose (string option_a) or (string option_b)
function fn_choose__string_or__string(option_a: string, option_b: string): string {
    return option_a;
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
