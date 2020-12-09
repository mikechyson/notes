	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15, 4
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	leaq	L_.str(%rip), %rsi
	xorl	%eax, %eax
	movb	%al, %cl
	movl	$1, %edi
	movl	$13, %edx
	movl	%eax, -8(%rbp)          ## 4-byte Spill
	movb	%cl, %al
	callq	_write
	movl	-8(%rbp), %edi          ## 4-byte Reload
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	callq	__exit
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"hello, world\n"


.subsections_via_symbols
