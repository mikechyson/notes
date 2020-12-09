	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15, 4
	.globl	_write_read             ## -- Begin function write_read
	.p2align	4, 0x90
_write_read:                            ## @write_read
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	testq	%rdx, %rdx
	je	LBB0_3
## %bb.1:
	xorl	%eax, %eax
	.p2align	4, 0x90
LBB0_2:                                 ## =>This Inner Loop Header: Depth=1
	movq	%rax, (%rsi)
	movq	(%rdi), %rax
	incq	%rax
	decq	%rdx
	jne	LBB0_2
LBB0_3:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
