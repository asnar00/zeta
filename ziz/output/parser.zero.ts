// Platform implementation: io (TypeScript)
// Implements the functions declared in io.zero.md

import { readFileSync, writeFileSync } from 'fs';

// @zero on (string content) = read file (string path)
function fn_read_file__string(path: string): string {
    return readFileSync(path, 'utf8');
}

// @zero on write file (string path) (string content)
function fn_write_file__string__string(path: string, content: string): void {
    writeFileSync(path, content, 'utf8');
}

// @zero on print (string message)
function fn_print__string(message: string): void {
    console.log(message);
}


const logo: string = "ᕦ(ツ)ᕤ";

// @zero on (string out$) <- main (string args$); ziz/zeta.zero.md + ziz/parser.zero.md:16
function* task_main__string(args_arr: readonly string[]): Generator<string> {
    yield logo;
    for (const r of task_test_brackets()) {
        yield r;
    }
}

// @zero on (int depth$) <- bracket depth of matching (char c$) (string pair); ziz/zeta.zero.md + ziz/parser.zero.md:21
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

// @zero on (string result$) <- test brackets; ziz/zeta.zero.md + ziz/parser.zero.md:35
function* task_test_brackets(): Generator<string> {
    let logo = "ᕦ(ツ)ᕤ";
    let pos = fn_matching__string_in__string_after__int("()", logo, 1);
    if (pos == 3) {
        yield "PASS brackets: logo paren at 3";
    } else {
        yield "FAIL brackets: expected 3";
    }
}

// @zero on (int pos) = matching (string pair) in (string s) after (int start); ziz/zeta.zero.md + ziz/parser.zero.md:30
function fn_matching__string_in__string_after__int(pair: string, s: string, start: number): number {
    const sub = s.slice(start);
    const depth_arr = [...task_bracket_depth_of_matching__char__string(sub, pair)];
    const pos: number = start + depth_arr.findIndex(x => x == 0);
    return pos;
}


// Runtime harness: bridges OS to zero's main task
try {
    for (const line of task_main__string(process.argv.slice(2))) {
        console.log(line);
    }
} catch (e) {
    // no main task defined
}
