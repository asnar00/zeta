
test/riscv/test.bin:     file format binary


Disassembly of section .data:

00000000 <.data>:
   0:	00000197          	auipc	x3,0x0
   4:	02018193          	addi	x3,x3,32 # 0x20
   8:	00000117          	auipc	x2,0x0
   c:	41810113          	addi	x2,x2,1048 # 0x420
  10:	001005b7          	lui	x11,0x100
  14:	000052b7          	lui	x5,0x5
  18:	55528293          	addi	x5,x5,1365 # 0x5555
  1c:	0055a023          	sw	x5,0(x11) # 0x100000
