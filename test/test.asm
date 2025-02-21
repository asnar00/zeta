
test/test.bin:     file format binary


Disassembly of section .data:

00000000 <.data>:
   0:	00000197          	auipc	x3,0x0
   4:	0b018193          	addi	x3,x3,176 # 0xb0
   8:	00000117          	auipc	x2,0x0
   c:	50810113          	addi	x2,x2,1288 # 0x510
  10:	ffc10113          	addi	x2,x2,-4
  14:	0001a007          	flw	f0,0(x3)
  18:	0001ac27          	fsw	f0,24(x3)
  1c:	0041a087          	flw	f1,4(x3)
  20:	0011ae27          	fsw	f1,28(x3)
  24:	08100053          	fsub.s	f0,f0,f1,rne
  28:	0201a027          	fsw	f0,32(x3)
  2c:	10000053          	fmul.s	f0,f0,f0,rne
  30:	0201a227          	fsw	f0,36(x3)
  34:	0081a087          	flw	f1,8(x3)
  38:	0211a427          	fsw	f1,40(x3)
  3c:	00012027          	fsw	f0,0(x2)
  40:	00c1a007          	flw	f0,12(x3)
  44:	0201a627          	fsw	f0,44(x3)
  48:	08008053          	fsub.s	f0,f1,f0,rne
  4c:	0201a827          	fsw	f0,48(x3)
  50:	10000053          	fmul.s	f0,f0,f0,rne
  54:	0201aa27          	fsw	f0,52(x3)
  58:	00012087          	flw	f1,0(x2)
  5c:	00008053          	fadd.s	f0,f1,f0,rne
  60:	0201ac27          	fsw	f0,56(x3)
  64:	0101a087          	flw	f1,16(x3)
  68:	0211ae27          	fsw	f1,60(x3)
  6c:	00012027          	fsw	f0,0(x2)
  70:	0141a007          	flw	f0,20(x3)
  74:	0401a027          	fsw	f0,64(x3)
  78:	08008053          	fsub.s	f0,f1,f0,rne
  7c:	0401a227          	fsw	f0,68(x3)
  80:	10000053          	fmul.s	f0,f0,f0,rne
  84:	0401a427          	fsw	f0,72(x3)
  88:	00012087          	flw	f1,0(x2)
  8c:	00008053          	fadd.s	f0,f1,f0,rne
  90:	0401a627          	fsw	f0,76(x3)
  94:	58000053          	fsqrt.s	f0,f0,rne
  98:	0401a827          	fsw	f0,80(x3)
  9c:	00410113          	addi	x2,x2,4
  a0:	10500073          	wfi
  a4:	ffdff06f          	jal	x0,0xa0
