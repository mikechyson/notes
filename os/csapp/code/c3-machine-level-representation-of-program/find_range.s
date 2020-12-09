	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15
	.globl	_find_range             ## -- Begin function find_range
	.p2align	4, 0x90
_find_range:                            ## @find_range
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	xorl	%eax, %eax
	vxorps	%xmm1, %xmm1, %xmm1
	vucomiss	%xmm0, %xmm1
	ja	LBB0_3
## %bb.1:
	movl	$1, %eax
	vucomiss	%xmm1, %xmm0
	jne	LBB0_2
	jp	LBB0_2
LBB0_3:
	popq	%rbp
	retq
LBB0_2:
	vxorps	%xmm1, %xmm1, %xmm1
	xorl	%eax, %eax
	vucomiss	%xmm1, %xmm0
	seta	%al
	xorl	$3, %eax
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
