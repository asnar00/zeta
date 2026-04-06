# Platform implementation: gui (Python)
# Implements the functions declared in gui.zero.md
# Server-side fallback — in production, these run on the client.


# @zero on (string result) = input (string prompt)
def fn_input__string(prompt: str) -> str:
    return input(f"{prompt}: ")


# @zero on show message (string text)
def fn_show_message__string(text: str):
    print(text)  # server fallback: print to terminal


# @zero on (string value) = get cookie (string name)
def fn_get_cookie__string(name: str) -> str:
    return ""  # server fallback: no cookies


# @zero on clear cookie (string name)
def fn_clear_cookie__string(name: str):
    pass  # server fallback: no-op


# @zero on (string choice) = choose (string option_a) or (string option_b)
def fn_choose__string_or__string(option_a: str, option_b: str) -> str:
    return option_a  # server fallback: return first option


# @zero on set cookie of (string name) to (string value)
def fn_set_cookie_of__string_to__string(name: str, value: str):
    pass  # no-op on server — cookies are set by the HTTP response


# @zero on reload page ()
def fn_reload_page():
    pass  # no-op on server


# @zero on click on (string selector)
def fn_click_on__string(selector: str):
    pass  # no-op on server


# @zero on type (string text) into input box (string selector)
def fn_type__string_into_input_box__string(text: str, selector: str):
    pass  # no-op on server


# @zero on press (string key) on (string selector)
def fn_press__string_on__string(key: str, selector: str):
    pass  # no-op on server


# @zero on (string snapshot) = describe page ()
def fn_describe_page() -> str:
    return "no gui on server"
