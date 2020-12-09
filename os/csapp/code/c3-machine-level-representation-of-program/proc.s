	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15
	.globl	_proc                   ## -- Begin function proc
	.p2align	4, 0x90
_proc:                                  ## @proc
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	addq	%rdi, (%rsi)
	addl	%edx, (%rcx)
	movb	16(%rbp), %al
	addw	%r8w, (%r9)
	movq	24(%rbp), %rcx
	addb	%al, (%rcx)
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
