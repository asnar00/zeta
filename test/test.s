
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
    adr x0, f32_constants
    ldr s0, [x0, #0]
    ldr s1, [x0, #4]
    fsub s0, s0, s1
    fmul s0, s0, s0
    ldr s1, [x0, #8]
    ldr s2, [x0, #12]
    fsub s1, s1, s2
    fmul s1, s1, s1
    fadd s0, s0, s1
    ldr s1, [x0, #16]
    ldr s2, [x0, #20]
    fsub s1, s1, s2
    fmul s1, s1, s1
    fadd s0, s0, s1
    fsqrt s0, s0
    ret
