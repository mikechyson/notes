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
	pushq	%r14
	pushq	%rbx
	subq	$32, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	cmpl	$2, %edi
	jne	LBB0_1
## %bb.2:
	movq	8(%rsi), %rdi
	callq	_atoi
	movl	%eax, -20(%rbp)
	leaq	_thread(%rip), %r14
	leaq	-40(%rbp), %rdi
	leaq	-20(%rbp), %rbx
	xorl	%esi, %esi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	_Pthread_create
	leaq	-32(%rbp), %rdi
	xorl	%esi, %esi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	_Pthread_create
	movq	-40(%rbp), %rdi
	xorl	%esi, %esi
	callq	_Pthread_join
	movq	-32(%rbp), %rdi
	xorl	%esi, %esi
	callq	_Pthread_join
	movl	_cnt(%rip), %eax
	movl	-20(%rbp), %ecx
	addl	%ecx, %ecx
	cmpl	%ecx, %eax
	movl	_cnt(%rip), %esi
	leaq	L_.str.2(%rip), %rax
	leaq	L_.str.1(%rip), %rdi
	cmoveq	%rax, %rdi
	xorl	%eax, %eax
	callq	_printf
	xorl	%edi, %edi
	callq	_exit
LBB0_1:
	movq	(%rsi), %rsi
	leaq	L_.str(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	xorl	%edi, %edi
	callq	_exit
	.cfi_endproc
                                        ## -- End function
	.globl	_thread                 ## -- Begin function thread
	.p2align	4, 0x90
_thread:                                ## @thread
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movl	(%rdi), %eax
	testl	%eax, %eax
	jle	LBB1_2
	.p2align	4, 0x90
LBB1_1:                                 ## =>This Inner Loop Header: Depth=1
	incl	_cnt(%rip)
	decl	%eax
	jne	LBB1_1
LBB1_2:
	xorl	%eax, %eax
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_cnt                    ## @cnt
.zerofill __DATA,__common,_cnt,4,2
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"usage: %s <niters>\n"

L_.str.1:                               ## @.str.1
	.asciz	"BOOM! cnt=%d\n"

L_.str.2:                               ## @.str.2
	.asciz	"OK cnt=%d\n"


.subsections_via_symbols
