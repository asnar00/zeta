
test/test.o:	file format elf32-littleriscv

Disassembly of section .text:

00000000 <_start>:
       0: 00000097     	auipc	ra, 0x0
       4: 00008093     	mv	ra, ra

00000008 <.Lpcrel_hi1>:
       8: 00000117     	auipc	sp, 0x0
       c: 00010113     	mv	sp, sp

00000010 <prologue>:
      10: ffc10113     	addi	sp, sp, -0x4

00000014 <code>:
      14: 0000a007     	flw	ft0, 0x0(ra)
      18: 0200a027     	fsw	ft0, 0x20(ra)
      1c: 0040a087     	flw	ft1, 0x4(ra)
      20: 0210a227     	fsw	ft1, 0x24(ra)
      24: 08107053     	fsub.s	ft0, ft0, ft1
      28: 0200a427     	fsw	ft0, 0x28(ra)
      2c: 10007053     	fmul.s	ft0, ft0, ft0
      30: 0200a627     	fsw	ft0, 0x2c(ra)
      34: 0080a087     	flw	ft1, 0x8(ra)
      38: 0210a827     	fsw	ft1, 0x30(ra)
      3c: 00012027     	fsw	ft0, 0x0(sp)
      40: 00c0a007     	flw	ft0, 0xc(ra)
      44: 0200aa27     	fsw	ft0, 0x34(ra)
      48: 0800f053     	fsub.s	ft0, ft1, ft0
      4c: 0200ac27     	fsw	ft0, 0x38(ra)
      50: 10007053     	fmul.s	ft0, ft0, ft0
      54: 0200ae27     	fsw	ft0, 0x3c(ra)
      58: 00012087     	flw	ft1, 0x0(sp)
      5c: 0000f053     	fadd.s	ft0, ft1, ft0
      60: 0400a027     	fsw	ft0, 0x40(ra)
      64: 0100a087     	flw	ft1, 0x10(ra)
      68: 0410a227     	fsw	ft1, 0x44(ra)
      6c: 00012027     	fsw	ft0, 0x0(sp)
      70: 0140a007     	flw	ft0, 0x14(ra)
      74: 0400a427     	fsw	ft0, 0x48(ra)
      78: 0800f053     	fsub.s	ft0, ft1, ft0
      7c: 0400a627     	fsw	ft0, 0x4c(ra)
      80: 10007053     	fmul.s	ft0, ft0, ft0
      84: 0400a827     	fsw	ft0, 0x50(ra)
      88: 00012087     	flw	ft1, 0x0(sp)
      8c: 0000f053     	fadd.s	ft0, ft1, ft0
      90: 0400aa27     	fsw	ft0, 0x54(ra)
      94: 58007053     	fsqrt.s	ft0, ft0
      98: 0400ac27     	fsw	ft0, 0x58(ra)

0000009c <epilogue>:
      9c: 00410113     	addi	sp, sp, 0x4

000000a0 <shutdown>:
      a0: 10500073     	wfi
      a4: 0000006f     	j	0xa4 <shutdown+0x4>
