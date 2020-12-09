	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15, 4
	.globl	_poly                   ## -- Begin function poly
	.p2align	4, 0x90
_poly:                                  ## @poly
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	vmovapd	%xmm0, %xmm1
	vmovsd	(%rdi), %xmm0           ## xmm0 = mem[0],zero
	testq	%rsi, %rsi
	jle	LBB0_3
## %bb.1:
	xorl	%eax, %eax
	vmovapd	%xmm1, %xmm2
	.p2align	4, 0x90
LBB0_2:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	8(%rdi,%rax,8), %xmm2, %xmm3
	vaddsd	%xmm3, %xmm0, %xmm0
	vmulsd	%xmm1, %xmm2, %xmm2
	incq	%rax
	cmpq	%rax, %rsi
	jne	LBB0_2
LBB0_3:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
