// Platform implementation: gui (TypeScript/web)
// Implements the functions declared in gui.zero.md

// @zero on (string result$) <- input (string prompt)
function* task_input__string(prompt: string): Generator<string> {
    yield (globalThis as any).prompt?.(prompt) ?? "";
}

// @zero on show message (string text)
function fn_show_message__string(text: string): void {
    if (typeof alert !== "undefined") {
        alert(text);
    } else {
        console.log(text);
    }
}

// @zero input string cookie$[string]
const cookie_arr: Map<string, string> = new Map();

// @zero on (string value) = get cookie (string name)
function fn_get_cookie__string(name: string): string {
    return cookie_arr.get(name) ?? "";
}

// @zero on clear cookie (string name)
function fn_clear_cookie__string(name: string): void {
}

// @zero on (string choice$) <- choose (string option-a) or (string option-b)
function* task_choose_or__string__string(option_a: string, option_b: string): Generator<string> {
    yield option_a;
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

// @zero on click on (string selector)
function fn_click_on__string(selector: string): void {
}

// @zero on type (string text) into input box (string selector)
function fn_type__string_into_input_box__string(text: string, selector: string): void {
}

// @zero on press (string key) on (string selector)
function fn_press__string_on__string(key: string, selector: string): void {
}

// @zero on (string snapshot) = describe page ()
function fn_describe_page(): string {
    return "no gui on server";
}
