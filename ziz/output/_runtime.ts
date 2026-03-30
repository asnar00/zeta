export const _test_registry: Map<string, [() => void, string][]> = new Map();

export function register_tests(feature: string, tests: [() => void, string][]): void {
    _test_registry.set(feature, tests);
}

export function run_tests(names?: string[]): number {
    let total = 0;
    for (const [feat, tests] of _test_registry) {
        if (names && names.length && !names.includes(feat)) continue;
        console.log(`${feat}:`);
        let passed = 0;
        let failed = 0;
        for (const [fn, desc] of tests) {
            try {
                fn();
                passed++;
                console.log(`  PASS ${desc}`);
            } catch (e: any) {
                failed++;
                console.log(`  FAIL ${desc}: ${e.message}`);
            }
        }
        console.log(`${feat}: ${passed} passed, ${failed} failed`);
        total += failed;
    }
    return total;
}
