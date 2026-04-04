# Platform implementation: gui (Python)
# Implements the functions declared in gui.zero.md
# Server-side fallback — in production, these run on the client.


# @zero on (string result) = input (string prompt)
def fn_input__string(prompt: str) -> str:
    return input(f"{prompt}: ")


# @zero on set cookie of (string name) to (string value)
def fn_set_cookie_of__string_to__string(name: str, value: str):
    pass  # no-op on server — cookies are set by the HTTP response


# @zero on reload page ()
def fn_reload_page():
    pass  # no-op on server
