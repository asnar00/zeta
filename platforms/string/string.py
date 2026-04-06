# Platform implementation: string (Python)
# Implements the functions declared in string.zero.md


# @zero on (string result) = trim (string s)
def fn_trim__string(s: str) -> str:
    return s.strip()


# @zero on (char c) = char (int i) of (string s)
def fn_char__int_of__string(i: int, s: str) -> str:
    return s[i]


# @zero on (string result$) = split [string s] at [int positions$]
def fn_split_at(s: str, positions: list[int]) -> list[str]:
    parts = []
    start = 0
    for pos in positions:
        parts.append(s[start:pos - 1])
        start = pos + 1
    remainder = s[start:]
    if remainder:
        parts.append(remainder)
    return parts


# @zero on (bool result) = (string s) starts with (string prefix)
def fn__string_starts_with__string(s: str, prefix: str) -> bool:
    return s.startswith(prefix)


# @zero on (bool result) = (string s) contains (string substring)
def fn__string_contains__string(s: str, substring: str) -> bool:
    return substring in s


# @zero on (string result$) = split (string s) by (string delim)
def fn_split__string_by__string(s: str, delim: str) -> list[str]:
    return s.split(delim)


# @zero on (string result) = replace (string needle) in (string s) with (string replacement)
def fn_replace__string_in__string_with__string(needle: str, s: str, replacement: str) -> str:
    return s.replace(needle, replacement)


# @zero on (int n) = length of (string s)
def fn_length_of__string(s: str) -> int:
    return len(s)


# @zero on (string sub) = substring of (string s) from (int start)
def fn_substring_of__string_from__int(s: str, start: int) -> str:
    return s[start:]


# @zero on (int n) = to int (string s)
def fn_to_int__string(s: str) -> int:
    try:
        return int(s)
    except (ValueError, TypeError):
        return 0
