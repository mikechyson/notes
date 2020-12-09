	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15
	.globl	_float_mov              ## -- Begin function float_mov
	.p2align	4, 0x90
_float_mov:                             ## @float_mov
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movss	(%rdi), %xmm1           ## xmm1 = mem[0],zero,zero,zero
	movss	%xmm0, (%rsi)
	movaps	%xmm1, %xmm0
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
