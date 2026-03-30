import { register_tests } from './_runtime.js';

export function test_parser_0(): void {
    // matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3
    const _result = fn_matching__string_in__string_after__int("()", "ᕦ(ツ)ᕤ", 1);
    const _expected = 3;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_parser_1(): void {
    // matching ("[]") in ("a[b[c]d]e") after (1) => 7
    const _result = fn_matching__string_in__string_after__int("[]", "a[b[c]d]e", 1);
    const _expected = 7;
    if (_result !== _expected) throw new Error(`expected ${_expected}, got ${_result}`);
}

register_tests('parser', [[test_parser_0, 'matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3'], [test_parser_1, 'matching ("[]") in ("a[b[c]d]e") after (1) => 7']]);

// @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/parser.zero.md:20
export function* task_bracket_depth_of_matching__char__string(c_arr: readonly string[], pair: string): Generator<number> {
    let d = 0;
    for (const c of c_arr) {
        if (c == pair[0]) {
            d = d + 1;
        } else if (c == pair[1]) {
            d = d - 1;
        }
        yield d;
    }
}

// @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/parser.zero.md:29
export function fn_matching__string_in__string_after__int(pair: string, s: string, start: number): number {
    const sub = s.slice(start);
    const depth_arr = [...task_bracket_depth_of_matching__char__string([...sub], pair)];
    const pos: number = start + depth_arr.findIndex(x => x == 0);
    return pos;
}
