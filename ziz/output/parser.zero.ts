// @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/parser.zero.md:22
function* task_bracket_depth_of_matching__char__string(c_arr: readonly string[], pair: string): Generator<number> {
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

// @zero on (string result$) <- test brackets; ziz/parser.zero.md:36
function* task_test_brackets(): Generator<string> {
    let logo = "ᕦ(ツ)ᕤ";
    let pos = fn_matching__string_in__string_after__int("()", logo, 1);
    if (pos == 3) {
        yield "PASS brackets: logo paren at 3";
    } else {
        yield "FAIL brackets: expected 3";
    }
}

// @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/parser.zero.md:31
function fn_matching__string_in__string_after__int(pair: string, s: string, start: number): number {
    const sub = s.slice(start);
    const depth_arr = [...task_bracket_depth_of_matching__char__string(sub, pair)];
    const pos: number = start + depth_arr.findIndex(x => x == 0);
    return pos;
}
