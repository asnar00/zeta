.section .rodata
scratch:
    .word 0x3f800000            @ f32(1)
    .word 0x40800000            @ f32(4)
    .word 0x40000000            @ f32(2)
    .word 0x40a00000            @ f32(5)
    .word 0x40400000            @ f32(3)
    .word 0x40c00000            @ f32(6)
.global run
run:
    stp x29, x30, [sp, #-16]!   @ save frame pointer and return address
    mov x29, sp                 @ set up frame pointer
    sub sp, sp, #4              @ allocate spill space
    adr x0, scratch             @ load constant memory address
    ldr s0, [x0, #0]            @ a_0.x <= f32(1)
    ldr s1, [x0, #4]            @ b_1.x <= f32(4)
    fsub s0, s0, s1             @ v_3.x <= sub a_0.x, b_1.x
    fmul s0, s0, s0             @ n_9 <= mul v_3.x, v_3.x
    ldr s1, [x0, #8]            @ a_0.y <= f32(2)
    str s0, [sp, #0]            @ spill n_9 to make room for b_1.y
    ldr s0, [x0, #12]           @ b_1.y <= f32(5)
    fsub s0, s1, s0             @ v_3.y <= sub a_0.y, b_1.y
    fmul s0, s0, s0             @ n_10 <= mul v_3.y, v_3.y
    ldr s1, [sp, #0]            @ unspill n_9
    fadd s0, s1, s0             @ n_8 <= add n_9, n_10
    ldr s1, [x0, #16]           @ a_0.z <= f32(3)
    str s0, [sp, #0]            @ spill n_8 to make room for b_1.z
    ldr s0, [x0, #20]           @ b_1.z <= f32(6)
    fsub s0, s1, s0             @ v_3.z <= sub a_0.z, b_1.z
    fmul s0, s0, s0             @ n_11 <= mul v_3.z, v_3.z
    ldr s1, [sp, #0]            @ unspill n_8
    fadd s0, s1, s0             @ n_7 <= add n_8, n_11
    fsqrt s0, s0                @ d_2 <= sqrt n_7
    add sp, sp, #4              @ deallocate spill space
    ret                         @ return
