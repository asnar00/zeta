import { register_tests, run_tests } from './_runtime.js';
import './parser.js';
import * as parser from './parser.js';

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


// Platform implementation: string (TypeScript)
// Implements the functions declared in string.zero.md


// @zero on (string result) = trim (string s)
function fn_trim__string(s: string): string {
    return s.trim();
}


// @zero on (string result$) = split [string s] at [int positions$]
function fn_split_at(s: string, positions: readonly number[]): string[] {
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


function test_zeta_0(): void {
    // main(["input.zero.md", "output.py"]) => "ᕦ(ツ)ᕤ"
    const _result = [...task_main__string(["input.zero.md", "output.py"])];
    const _expected = ["ᕦ(ツ)ᕤ"];
    if (JSON.stringify(_result) !== JSON.stringify(_expected)) throw new Error(`expected ${_expected}, got ${_result}`);
}

register_tests('zeta', [[test_zeta_0, 'main(["input.zero.md", "output.py"]) => "ᕦ(ツ)ᕤ"']]);

const logo: string = "ᕦ(ツ)ᕤ";

// @zero on (string out$) <- main (string args$); ziz/zeta.zero.md:27
function* task_main__string(args_arr: readonly string[]): Generator<string> {
    yield logo;
}


if (process.argv.includes('--test')) {
    const _names = process.argv.slice(2).filter(a => a !== '--test');
    process.exit(run_tests(_names.length ? _names : undefined) ? 1 : 0);
} else {
// Runtime harness: bridges OS to zero's main task
try {
    for (const line of task_main__string(process.argv.slice(2))) {
        console.log(line);
    }
} catch (e) {
    // no main task defined
}
}
