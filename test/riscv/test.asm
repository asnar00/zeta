
test/riscv/test.bin:     file format binary


Disassembly of section .data:

00000000 <.data>:
   0:	00000197          	auipc	x3,0x0
   4:	04c18193          	addi	x3,x3,76 # 0x4c
   8:	00000117          	auipc	x2,0x0
   c:	46810113          	addi	x2,x2,1128 # 0x470
  10:	0001a007          	flw	f0,0(x3)
  14:	0041a087          	flw	f1,4(x3)
  18:	08100053          	fsub.s	f0,f0,f1,rne
  1c:	10000053          	fmul.s	f0,f0,f0,rne
  20:	0081a087          	flw	f1,8(x3)
  24:	00c1a107          	flw	f2,12(x3)
  28:	082080d3          	fsub.s	f1,f1,f2,rne
  2c:	101080d3          	fmul.s	f1,f1,f1,rne
  30:	00100053          	fadd.s	f0,f0,f1,rne
  34:	0101a087          	flw	f1,16(x3)
  38:	0141a107          	flw	f2,20(x3)
  3c:	001000b7          	lui	x1,0x100
  40:	00008093          	addi	x1,x1,0 # 0x100000
  44:	00005137          	lui	x2,0x5
  48:	55510113          	addi	x2,x2,1365 # 0x5555
  4c:	0020a023          	sw	x2,0(x1)
