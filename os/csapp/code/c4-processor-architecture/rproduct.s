	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15
	.globl	_rproduct               ## -- Begin function rproduct
	.p2align	4, 0x90
_rproduct:                              ## @rproduct
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movl	$1, %eax
	cmpq	$2, %rsi
	jl	LBB0_3
## %bb.1:
	incq	%rsi
	movl	$1, %eax
	.p2align	4, 0x90
LBB0_2:                                 ## =>This Inner Loop Header: Depth=1
	imulq	(%rdi), %rax
	addq	$8, %rdi
	decq	%rsi
	cmpq	$2, %rsi
	jg	LBB0_2
LBB0_3:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
