.global run
run:
    adr x0, spill               // load spill adr
    mov sp, x0                  // load spill adr
    adr x0, scratch             // load constant memory address
    ldr s0, [x0, #0x48]         // a_0.x <= f32(1)
    str s0, [x0]                // tap a_0.x
    ldr s1, [x0, #0x4c]         // b_1.x <= f32(4)
    str s1, [x0, #0x0c]         // tap b_1.x
    fsub s0, s0, s1             // v_3.x <= sub a_0.x, b_1.x
    str s0, [x0, #0x1c]         // tap v_3.x
    fmul s0, s0, s0             // n_9 <= mul v_3.x, v_3.x
    str s0, [x0, #0x3c]         // tap n_9
    ldr s1, [x0, #0x50]         // a_0.y <= f32(2)
    str s1, [x0, #0x04]         // tap a_0.y
    str s0, [sp]                // spill n_9 to make room for b_1.y
    ldr s0, [x0, #0x54]         // b_1.y <= f32(5)
    str s0, [x0, #0x10]         // tap b_1.y
    fsub s0, s1, s0             // v_3.y <= sub a_0.y, b_1.y
    str s0, [x0, #0x20]         // tap v_3.y
    fmul s0, s0, s0             // n_10 <= mul v_3.y, v_3.y
    str s0, [x0, #0x40]         // tap n_10
    ldr s1, [sp]                // unspill n_9
    fadd s0, s1, s0             // n_8 <= add n_9, n_10
    str s0, [x0, #0x38]         // tap n_8
    ldr s1, [x0, #0x58]         // a_0.z <= f32(3)
    str s1, [x0, #0x08]         // tap a_0.z
    str s0, [sp]                // spill n_8 to make room for b_1.z
    ldr s0, [x0, #0x5c]         // b_1.z <= f32(6)
    str s0, [x0, #0x14]         // tap b_1.z
    fsub s0, s1, s0             // v_3.z <= sub a_0.z, b_1.z
    str s0, [x0, #0x24]         // tap v_3.z
    fmul s0, s0, s0             // n_11 <= mul v_3.z, v_3.z
    str s0, [x0, #0x44]         // tap n_11
    ldr s1, [sp]                // unspill n_8
    fadd s0, s1, s0             // n_7 <= add n_8, n_11
    str s0, [x0, #0x34]         // tap n_7
    fsqrt s0, s0                // d_2 <= sqrt n_7
    str s0, [x0, #0x18]         // tap d_2
    mov w0, 0x18                // semihosting SYS_EXIT operation
    mov w1, 0x0                 // exit status
    hlt 0xf000                  // halt
scratch:
    .word 0x00000000            // tap a_0.x
    .word 0x00000000            // tap a_0.y
    .word 0x00000000            // tap a_0.z
    .word 0x00000000            // tap b_1.x
    .word 0x00000000            // tap b_1.y
    .word 0x00000000            // tap b_1.z
    .word 0x00000000            // tap d_2
    .word 0x00000000            // tap v_3.x
    .word 0x00000000            // tap v_3.y
    .word 0x00000000            // tap v_3.z
    .word 0x00000000            // tap n_4
    .word 0x00000000            // tap n_5
    .word 0x00000000            // tap n_6
    .word 0x00000000            // tap n_7
    .word 0x00000000            // tap n_8
    .word 0x00000000            // tap n_9
    .word 0x00000000            // tap n_10
    .word 0x00000000            // tap n_11
    .word 0x3f800000            // constant f32 1
    .word 0x40800000            // constant f32 4
    .word 0x40000000            // constant f32 2
    .word 0x40a00000            // constant f32 5
    .word 0x40400000            // constant f32 3
    .word 0x40c00000            // constant f32 6
spill:
    .word 0x00000000            // spill
    .word 0x00000000            // spill
    .word 0x00000000            // spill
    .word 0x00000000            // spill
