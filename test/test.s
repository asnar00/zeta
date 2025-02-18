    .section .text
    .option norvc          # disable compressed instructions
    .globl _start          # entry point must be global
    .type _start, @function

_start:
    la x1, constants      # load address of constants
    la sp, stack_top      # load address of stack top
prologue:
    addi sp, sp, -4       # allocate stack
code:
    flw f0, 0(x1)         # a_0.x <= const 1
    fsw f0, 32(x1)        # dbg: store a_0.x at offset 0
    flw f1, 4(x1)         # b_1.x <= const 4
    fsw f1, 36(x1)        # dbg: store b_1.x at offset 1
    fsub.s f0, f0, f1     # v_3.x <= sub a_0.x, b_1.x
    fsw f0, 40(x1)        # dbg: store v_3.x at offset 2
    fmul.s f0, f0, f0     # n_9 <= mul v_3.x, v_3.x
    fsw f0, 44(x1)        # dbg: store n_9 at offset 3
    flw f1, 8(x1)         # a_0.y <= const 2
    fsw f1, 48(x1)        # dbg: store a_0.y at offset 4
    fsw f0, 0(sp)         # spill n_9 to slot 0
    flw f0, 12(x1)        # b_1.y <= const 5
    fsw f0, 52(x1)        # dbg: store b_1.y at offset 5
    fsub.s f0, f1, f0     # v_3.y <= sub a_0.y, b_1.y
    fsw f0, 56(x1)        # dbg: store v_3.y at offset 6
    fmul.s f0, f0, f0     # n_10 <= mul v_3.y, v_3.y
    fsw f0, 60(x1)        # dbg: store n_10 at offset 7
    flw f1, 0(sp)         # unspill n_9 from slot 0
    fadd.s f0, f1, f0     # n_8 <= add n_9, n_10
    fsw f0, 64(x1)        # dbg: store n_8 at offset 8
    flw f1, 16(x1)        # a_0.z <= const 3
    fsw f1, 68(x1)        # dbg: store a_0.z at offset 9
    fsw f0, 0(sp)         # spill n_8 to slot 0
    flw f0, 20(x1)        # b_1.z <= const 6
    fsw f0, 72(x1)        # dbg: store b_1.z at offset 10
    fsub.s f0, f1, f0     # v_3.z <= sub a_0.z, b_1.z
    fsw f0, 76(x1)        # dbg: store v_3.z at offset 11
    fmul.s f0, f0, f0     # n_11 <= mul v_3.z, v_3.z
    fsw f0, 80(x1)        # dbg: store n_11 at offset 12
    flw f1, 0(sp)         # unspill n_8 from slot 0
    fadd.s f0, f1, f0     # n_7 <= add n_8, n_11
    fsw f0, 84(x1)        # dbg: store n_7 at offset 13
    fsqrt.s f0, f0        # d_2 <= sqrt n_7
    fsw f0, 88(x1)        # dbg: store d_2 at offset 14
epilogue:
    addi sp, sp, 4        # deallocate stack
shutdown:
    wfi                   # wait for interrupt
    j shutdown            # loop back if we awaken

    .section .data        # data section
    .align 4              # align to 4-byte boundary

constants:
    .word 0x0000803f      # const_f32_1
    .word 0x00008040      # const_f32_4
    .word 0x00000040      # const_f32_2
    .word 0x0000a040      # const_f32_5
    .word 0x00004040      # const_f32_3
    .word 0x0000c040      # const_f32_6

    .section .bss         # uninitialized data section
    .align 16             # align to 16-byte boundary

dbg_vars:                # label for debug variables
    .zero 60              # allocate space for debug variables
    .align 16

stack_bottom:            # label for bottom of stack
    .zero 1024            # allocate 1KB for stack
stack_top:               # label for top of stack
