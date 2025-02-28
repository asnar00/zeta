
test/arm/test.bin:     file format binary


Disassembly of section .data:

0000000000000000 <.data>:
   0:	1000023c 	adr	x28, 0x44
   4:	100022fd 	adr	x29, 0x460
   8:	bd400380 	ldr	s0, [x28]
   c:	bd401381 	ldr	s1, [x28, #16]
  10:	1e213800 	fsub	s0, s0, s1
  14:	1e200800 	fmul	s0, s0, s0
  18:	bd402381 	ldr	s1, [x28, #32]
  1c:	bd403382 	ldr	s2, [x28, #48]
  20:	1e223821 	fsub	s1, s1, s2
  24:	1e210821 	fmul	s1, s1, s1
  28:	1e212800 	fadd	s0, s0, s1
  2c:	bd404381 	ldr	s1, [x28, #64]
  30:	bd405382 	ldr	s2, [x28, #80]
  34:	1e223821 	fsub	s1, s1, s2
  38:	1e210821 	fmul	s1, s1, s1
  3c:	1e212800 	fadd	s0, s0, s1
  40:	1e21c000 	fsqrt	s0, s0
