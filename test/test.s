    ldr s0, [x0, #0]            [30;1m@ a_0.x <= f32(1)[0m
    ldr s1, [x0, #4]            [30;1m@ b_1.x <= f32(4)[0m
    fsub s0, s0, s1             [30;1m@ v_3.x <= sub a_0.x, b_1.x[0m
    fmul s0, s0, s0             [30;1m@ n_9 <= mul v_3.x, v_3.x[0m
    ldr s1, [x0, #8]            [30;1m@ a_0.y <= f32(2)[0m
    str s0, [sp, #0]            [30;1m@ spill n_9 to make room for b_1.y[0m
    ldr s0, [x0, #12]           [30;1m@ b_1.y <= f32(5)[0m
    fsub s0, s1, s0             [30;1m@ v_3.y <= sub a_0.y, b_1.y[0m
    fmul s0, s0, s0             [30;1m@ n_10 <= mul v_3.y, v_3.y[0m
    ldr s1, [sp, #0]            [30;1m@ unspill n_9[0m
    fadd s0, s1, s0             [30;1m@ n_8 <= add n_9, n_10[0m
    ldr s1, [x0, #16]           [30;1m@ a_0.z <= f32(3)[0m
    str s0, [sp, #0]            [30;1m@ spill n_8 to make room for b_1.z[0m
    ldr s0, [x0, #20]           [30;1m@ b_1.z <= f32(6)[0m
    fsub s0, s1, s0             [30;1m@ v_3.z <= sub a_0.z, b_1.z[0m
    fmul s0, s0, s0             [30;1m@ n_11 <= mul v_3.z, v_3.z[0m
    ldr s1, [sp, #0]            [30;1m@ unspill n_8[0m
    fadd s0, s1, s0             [30;1m@ n_7 <= add n_8, n_11[0m
    fsqrt s0, s0                [30;1m@ d_2 <= sqrt n_7[0m
