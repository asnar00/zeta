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

// @zero on (string out$) <- main (string args$); ziz/zeta.zero.md:37
function* fn_main__string(args_arr: readonly string[]): Generator<string> {
    yield logo;
}


// Runtime harness: bridges OS to zero's main task
try {
    for (const line of fn_main__string(process.argv.slice(2))) {
        console.log(line);
    }
} catch (e) {
    // no main task defined
}
