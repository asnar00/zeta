
.section .rodata
.align 4
f32_constants:
    .word 0x3f800000 @ f32(1)
    .word 0x40800000 @ f32(4)
    .word 0x40000000 @ f32(2)
    .word 0x40a00000 @ f32(5)
    .word 0x40400000 @ f32(3)
    .word 0x40c00000 @ f32(6)

.global run
run:
    stp x29, x30, [sp, #-16]! 
    mov x29, sp
    sub sp, sp, #4
    adr x0, f32_constants
    ldr s0, [x0, #0]            [30;1m@ f32(1)[0m
    ldr s1, [x0, #4.0]          [30;1m@ f32(4)[0m
    fsub s0, s0, s1             [30;1m@ sub v_3.x [a_0.x, b_1.x][0m
    fmul s0, s0, s0             [30;1m@ mul n_9 [v_3.x, v_3.x][0m
    ldr s1, [x0, #8.0]          [30;1m@ f32(2)[0m
    str s0, [[sp], #0]          [30;1m@ spill n_9 to make room for b_1.y[0m
    ldr s0, [x0, #12.0]         [30;1m@ f32(5)[0m
    fsub s0, s1, s0             [30;1m@ sub v_3.y [a_0.y, b_1.y][0m
    fmul s0, s0, s0             [30;1m@ mul n_10 [v_3.y, v_3.y][0m
    ldr s1, [[sp], #0]          [30;1m@ unspill n_9[0m
    fadd s0, s1, s0             [30;1m@ add n_8 [n_9, n_10][0m
    ldr s1, [x0, #16.0]         [30;1m@ f32(3)[0m
    str s0, [[sp], #0]          [30;1m@ spill n_8 to make room for b_1.z[0m
    ldr s0, [x0, #20.0]         [30;1m@ f32(6)[0m
    fsub s0, s1, s0             [30;1m@ sub v_3.z [a_0.z, b_1.z][0m
    fmul s0, s0, s0             [30;1m@ mul n_11 [v_3.z, v_3.z][0m
    ldr s1, [[sp], #0]          [30;1m@ unspill n_8[0m
    fadd s0, s1, s0             [30;1m@ add n_7 [n_8, n_11][0m
    fsqrt s0, s0                [30;1m@ sqrt d_2 [n_7][0m
    add sp, sp, #4
    ret                         [30;1m@ return[0m
