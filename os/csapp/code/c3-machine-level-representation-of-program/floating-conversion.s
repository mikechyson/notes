	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15
	.globl	_fcvt                   ## -- Begin function fcvt
	.p2align	4, 0x90
_fcvt:                                  ## @fcvt
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	vcvttsd2si	(%rdx), %rax
	vcvtsi2sdq	(%rcx), %xmm0, %xmm1
	vmovss	(%rsi), %xmm0           ## xmm0 = mem[0],zero,zero,zero
	vcvtsi2ssl	%edi, %xmm2, %xmm2
	vcvtss2sd	%xmm0, %xmm0, %xmm0
	movq	%rax, (%rcx)
	vmovss	%xmm2, (%rsi)
	vmovsd	%xmm1, (%rdx)
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
