// Platform implementation: io (TypeScript)
// Implements the functions declared in io.zero.md

import { readFileSync, writeFileSync } from 'fs';

function fn_read_file__string(path: string): string {
    return readFileSync(path, 'utf8');
}

function fn_write_file__string__string(path: string, content: string): void {
    writeFileSync(path, content, 'utf8');
}

function fn_print__string(message: string): void {
    console.log(message);
}
