
test/riscv/test.bin:     file format binary


Disassembly of section .data:

00000000 <.data>:
   0:	00000197          	auipc	x3,0x0
   4:	06c18193          	addi	x3,x3,108 # 0x6c
   8:	00000117          	auipc	x2,0x0
   c:	48810113          	addi	x2,x2,1160 # 0x490
  10:	0001a007          	flw	f0,0(x3)
  14:	0041a087          	flw	f1,4(x3)
  18:	08100053          	fsub.s	f0,f0,f1,rne
  1c:	10000053          	fmul.s	f0,f0,f0,rne
  20:	0081a087          	flw	f1,8(x3)
  24:	00012027          	fsw	f0,0(x2)
  28:	00c1a007          	flw	f0,12(x3)
  2c:	08008053          	fsub.s	f0,f1,f0,rne
  30:	10000053          	fmul.s	f0,f0,f0,rne
  34:	00012087          	flw	f1,0(x2)
  38:	00008053          	fadd.s	f0,f1,f0,rne
  3c:	0101a087          	flw	f1,16(x3)
  40:	00012027          	fsw	f0,0(x2)
  44:	0141a007          	flw	f0,20(x3)
  48:	08008053          	fsub.s	f0,f1,f0,rne
  4c:	10000053          	fmul.s	f0,f0,f0,rne
  50:	00012087          	flw	f1,0(x2)
  54:	00008053          	fadd.s	f0,f1,f0,rne
  58:	58000053          	fsqrt.s	f0,f0,rne
  5c:	001005b7          	lui	x11,0x100
  60:	000052b7          	lui	x5,0x5
  64:	55528293          	addi	x5,x5,1365 # 0x5555
  68:	0055a023          	sw	x5,0(x11) # 0x100000
