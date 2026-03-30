import { register_tests } from './_runtime.js';

// Platform implementation: io (TypeScript)
// Implements the functions declared in io.zero.md

import { readFileSync, writeFileSync } from 'fs';

// @zero on (string content) = read file (string path)
export function fn_read_file__string(path: string): string {
    return readFileSync(path, 'utf8');
}

// @zero on write file (string path) (string content)
export function fn_write_file__string__string(path: string, content: string): void {
    writeFileSync(path, content, 'utf8');
}

// @zero on print (string message)
export function fn_print__string(message: string): void {
    console.log(message);
}


// Platform implementation: string (TypeScript)
// Implements the functions declared in string.zero.md


// @zero on (string result) = trim (string s)
export function fn_trim__string(s: string): string {
    return s.trim();
}


// @zero on (string result$) = split [string s] at [int positions$]
export function fn_split_at(s: string, positions: readonly number[]): string[] {
    const parts: string[] = [];
    let start = 0;
    for (const pos of positions) {
        parts.push(s.slice(start, pos - 1));
        start = pos + 1;
    }
    const remainder = s.slice(start);
    if (remainder) parts.push(remainder);
    return parts;
}


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

export function test_parser_2(): void {
    // split stream parts ("1 <- 2 <- 3") => ["1", "2", "3"]
    const _result = fn_split_stream_parts__string("1 <- 2 <- 3");
    const _expected = ["1", "2", "3"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_parser_3(): void {
    // split stream parts ("1 <- (a <- b) <- 3") => ["1", "(a <- b)", "3"]
    const _result = fn_split_stream_parts__string("1 <- (a <- b) <- 3");
    const _expected = ["1", "(a <- b)", "3"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

export function test_parser_4(): void {
    // split stream parts ("hello") => ["hello"]
    const _result = fn_split_stream_parts__string("hello");
    const _expected = ["hello"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

register_tests('parser', [[test_parser_0, 'matching ("()") in ("ᕦ(ツ)ᕤ") after (1) => 3'], [test_parser_1, 'matching ("[]") in ("a[b[c]d]e") after (1) => 7'], [test_parser_2, 'split stream parts ("1 <- 2 <- 3") => ["1", "2", "3"]'], [test_parser_3, 'split stream parts ("1 <- (a <- b) <- 3") => ["1", "(a <- b)", "3"]'], [test_parser_4, 'split stream parts ("hello") => ["hello"]']]);

// @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/parser.zero.md:30
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

// @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/parser.zero.md:39
export function fn_matching__string_in__string_after__int(pair: string, s: string, start: number): number {
    const sub = s.slice(start);
    const depth_arr = [...task_bracket_depth_of_matching__char__string([...sub], pair)];
    const pos: number = start + depth_arr.findIndex(x => x == 0);
    return pos;
}

// @zero on (string part$) = split stream parts (string s); ziz/parser.zero.md:44
export function fn_split_stream_parts__string(s: string): string[] {
    const padded = s + "<-";
    const depth_arr = [...task_bracket_depth_of_matching__char__string([...padded], "()")];
    const lt_arr = Array.from({ length: [...padded].length }, (_, _i) => [...padded][_i] == "<");
    const is_sep_arr = Array.from({ length: depth_arr.length }, (_, _i) => (_i - 1 >= 0 ? lt_arr[_i - 1] : 0) && [...padded][_i] == "-" && depth_arr[_i] == 0);
    const pos_arr = is_sep_arr.map((x, i) => (x) ? i : -1).filter(i => i >= 0);
    const part_arr = fn_split_at(padded, pos_arr).map(x => fn_trim__string(x));
    return part_arr;
}
