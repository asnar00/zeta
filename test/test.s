_start:
    000: auipc gp, 0x0        [30;1m# load address of constants[0m
    004: addi gp, gp, 0xb0    [30;1m# load address of constants[0m
    008: auipc sp, 0x0        [30;1m# load address of stack top[0m
    00c: addi sp, sp, 0x508   [30;1m# load address of stack top[0m
prologue:
    010: addi sp, sp, -4      [30;1m# allocate stack[0m
code:
    014: flw f0, 0(gp)        [30;1m# a_0.x <= const 1[0m
    018: fsw f0, 24(gp)       [30;1m# dbg: store a_0.x at offset 0[0m
    01c: flw f1, 4(gp)        [30;1m# b_1.x <= const 4[0m
    020: fsw f1, 28(gp)       [30;1m# dbg: store b_1.x at offset 1[0m
    024: fsub.s f0, f0, f1    [30;1m# v_3.x <= sub a_0.x, b_1.x[0m
    028: fsw f0, 32(gp)       [30;1m# dbg: store v_3.x at offset 2[0m
    02c: fmul.s f0, f0, f0    [30;1m# n_9 <= mul v_3.x, v_3.x[0m
    030: fsw f0, 36(gp)       [30;1m# dbg: store n_9 at offset 3[0m
    034: flw f1, 8(gp)        [30;1m# a_0.y <= const 2[0m
    038: fsw f1, 40(gp)       [30;1m# dbg: store a_0.y at offset 4[0m
    03c: fsw f0, 0(sp)        [30;1m# spill n_9 to slot 0[0m
    040: flw f0, 12(gp)       [30;1m# b_1.y <= const 5[0m
    044: fsw f0, 44(gp)       [30;1m# dbg: store b_1.y at offset 5[0m
    048: fsub.s f0, f1, f0    [30;1m# v_3.y <= sub a_0.y, b_1.y[0m
    04c: fsw f0, 48(gp)       [30;1m# dbg: store v_3.y at offset 6[0m
    050: fmul.s f0, f0, f0    [30;1m# n_10 <= mul v_3.y, v_3.y[0m
    054: fsw f0, 52(gp)       [30;1m# dbg: store n_10 at offset 7[0m
    058: flw f1, 0(sp)        [30;1m# unspill n_9 from slot 0[0m
    05c: fadd.s f0, f1, f0    [30;1m# n_8 <= add n_9, n_10[0m
    060: fsw f0, 56(gp)       [30;1m# dbg: store n_8 at offset 8[0m
    064: flw f1, 16(gp)       [30;1m# a_0.z <= const 3[0m
    068: fsw f1, 60(gp)       [30;1m# dbg: store a_0.z at offset 9[0m
    06c: fsw f0, 0(sp)        [30;1m# spill n_8 to slot 0[0m
    070: flw f0, 20(gp)       [30;1m# b_1.z <= const 6[0m
    074: fsw f0, 64(gp)       [30;1m# dbg: store b_1.z at offset 10[0m
    078: fsub.s f0, f1, f0    [30;1m# v_3.z <= sub a_0.z, b_1.z[0m
    07c: fsw f0, 68(gp)       [30;1m# dbg: store v_3.z at offset 11[0m
    080: fmul.s f0, f0, f0    [30;1m# n_11 <= mul v_3.z, v_3.z[0m
    084: fsw f0, 72(gp)       [30;1m# dbg: store n_11 at offset 12[0m
    088: flw f1, 0(sp)        [30;1m# unspill n_8 from slot 0[0m
    08c: fadd.s f0, f1, f0    [30;1m# n_7 <= add n_8, n_11[0m
    090: fsw f0, 76(gp)       [30;1m# dbg: store n_7 at offset 13[0m
    094: fsqrt.s f0, f0       [30;1m# d_2 <= sqrt n_7[0m
    098: fsw f0, 80(gp)       [30;1m# dbg: store d_2 at offset 14[0m
epilogue:
    09c: addi sp, sp, 4       [30;1m# deallocate stack[0m
shutdown:
    0a0: wfi                  [30;1m# wait for interrupt[0m
    0a4: j shutdown           [30;1m# loop back if we awaken[0m
constants:
    0b0: 0x0000803f           [30;1m# const_f32_1[0m
    0b4: 0x00008040           [30;1m# const_f32_4[0m
    0b8: 0x00000040           [30;1m# const_f32_2[0m
    0bc: 0x0000a040           [30;1m# const_f32_5[0m
    0c0: 0x00004040           [30;1m# const_f32_3[0m
    0c4: 0x0000c040           [30;1m# const_f32_6[0m
dbg:
    0c8: 0x00000000           [30;1m# a_0.x[0m
    0cc: 0x00000000           [30;1m# b_1.x[0m
    0d0: 0x00000000           [30;1m# v_3.x[0m
    0d4: 0x00000000           [30;1m# n_9[0m
    0d8: 0x00000000           [30;1m# a_0.y[0m
    0dc: 0x00000000           [30;1m# b_1.y[0m
    0e0: 0x00000000           [30;1m# v_3.y[0m
    0e4: 0x00000000           [30;1m# n_10[0m
    0e8: 0x00000000           [30;1m# n_8[0m
    0ec: 0x00000000           [30;1m# a_0.z[0m
    0f0: 0x00000000           [30;1m# b_1.z[0m
    0f4: 0x00000000           [30;1m# v_3.z[0m
    0f8: 0x00000000           [30;1m# n_11[0m
    0fc: 0x00000000           [30;1m# n_7[0m
    100: 0x00000000           [30;1m# d_2[0m
stack_bottom:
    110: 0x00000000           [30;1m# stack bottom[0m
stack_top:
    510: 0x00000000           [30;1m# stack top[0m
