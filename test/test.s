.section .rodata
scratch:
    .word 0x00000000            @ tap a_0.x
    .word 0x00000000            @ tap a_0.y
    .word 0x00000000            @ tap a_0.z
    .word 0x00000000            @ tap b_1.x
    .word 0x00000000            @ tap b_1.y
    .word 0x00000000            @ tap b_1.z
    .word 0x00000000            @ tap d_2
    .word 0x00000000            @ tap v_3.x
    .word 0x00000000            @ tap v_3.y
    .word 0x00000000            @ tap v_3.z
    .word 0x00000000            @ tap n_4
    .word 0x00000000            @ tap n_5
    .word 0x00000000            @ tap n_6
    .word 0x00000000            @ tap n_7
    .word 0x00000000            @ tap n_8
    .word 0x00000000            @ tap n_9
    .word 0x00000000            @ tap n_10
    .word 0x00000000            @ tap n_11
    .word 0x3f800000            @ constant f32 1
    .word 0x40800000            @ constant f32 4
    .word 0x40000000            @ constant f32 2
    .word 0x40a00000            @ constant f32 5
    .word 0x40400000            @ constant f32 3
    .word 0x40c00000            @ constant f32 6
.global run
run:
    stp x29, x30, [sp, #-16]!   @ save frame pointer and return adr
    mov x29, sp                 @ set up frame pointer
    sub sp, sp, #4              @ allocate spill space
    adr x0, scratch             @ load constant memory address
    ldr s0, [x0, #72]           @ a_0.x <= f32(1)
    str s0, [x0, #0]            @ tap a_0.x
    ldr s1, [x0, #76]           @ b_1.x <= f32(4)
    str s1, [x0, #12]           @ tap b_1.x
    fsub s0, s0, s1             @ v_3.x <= sub a_0.x, b_1.x
    str s0, [x0, #28]           @ tap v_3.x
    fmul s0, s0, s0             @ n_9 <= mul v_3.x, v_3.x
    str s0, [x0, #60]           @ tap n_9
    ldr s1, [x0, #80]           @ a_0.y <= f32(2)
    str s1, [x0, #4]            @ tap a_0.y
    str s0, [sp, #0]            @ spill n_9 to make room for b_1.y
    ldr s0, [x0, #84]           @ b_1.y <= f32(5)
    str s0, [x0, #16]           @ tap b_1.y
    fsub s0, s1, s0             @ v_3.y <= sub a_0.y, b_1.y
    str s0, [x0, #32]           @ tap v_3.y
    fmul s0, s0, s0             @ n_10 <= mul v_3.y, v_3.y
    str s0, [x0, #64]           @ tap n_10
    ldr s1, [sp, #0]            @ unspill n_9
    fadd s0, s1, s0             @ n_8 <= add n_9, n_10
    str s0, [x0, #56]           @ tap n_8
    ldr s1, [x0, #88]           @ a_0.z <= f32(3)
    str s1, [x0, #8]            @ tap a_0.z
    str s0, [sp, #0]            @ spill n_8 to make room for b_1.z
    ldr s0, [x0, #92]           @ b_1.z <= f32(6)
    str s0, [x0, #20]           @ tap b_1.z
    fsub s0, s1, s0             @ v_3.z <= sub a_0.z, b_1.z
    str s0, [x0, #36]           @ tap v_3.z
    fmul s0, s0, s0             @ n_11 <= mul v_3.z, v_3.z
    str s0, [x0, #68]           @ tap n_11
    ldr s1, [sp, #0]            @ unspill n_8
    fadd s0, s1, s0             @ n_7 <= add n_8, n_11
    str s0, [x0, #52]           @ tap n_7
    fsqrt s0, s0                @ d_2 <= sqrt n_7
    str s0, [x0, #24]           @ tap d_2
    add sp, sp, #4              @ deallocate spill space
    ret                         @ return
