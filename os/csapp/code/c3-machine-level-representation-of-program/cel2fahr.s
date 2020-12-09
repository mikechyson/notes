	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function cel2fahr
LCPI0_0:
	.quad	4610785298501913805     ## double 1.8
LCPI0_1:
	.quad	4629700416936869888     ## double 32
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_cel2fahr
	.p2align	4, 0x90
_cel2fahr:                              ## @cel2fahr
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	vmulsd	LCPI0_0(%rip), %xmm0, %xmm0
	vaddsd	LCPI0_1(%rip), %xmm0, %xmm0
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
