	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15	sdk_version 10, 15, 4
	.globl	_new_vec                ## -- Begin function new_vec
	.p2align	4, 0x90
_new_vec:                               ## @new_vec
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movq	%rdi, %r14
	movl	$16, %edi
	callq	_malloc
	testq	%rax, %rax
	je	LBB0_6
## %bb.1:
	movq	%rax, %rbx
	movq	%r14, (%rax)
	testq	%r14, %r14
	jle	LBB0_2
## %bb.4:
	movl	$8, %esi
	movq	%r14, %rdi
	callq	_calloc
	testq	%rax, %rax
	jne	LBB0_3
## %bb.5:
	movq	%rbx, %rdi
	callq	_free
LBB0_6:
	xorl	%ebx, %ebx
	jmp	LBB0_7
LBB0_2:
	xorl	%eax, %eax
LBB0_3:
	movq	%rax, 8(%rbx)
LBB0_7:
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_get_vec_element        ## -- Begin function get_vec_element
	.p2align	4, 0x90
_get_vec_element:                       ## @get_vec_element
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	xorl	%eax, %eax
	testq	%rsi, %rsi
	js	LBB1_3
## %bb.1:
	cmpq	%rsi, (%rdi)
	jle	LBB1_3
## %bb.2:
	movq	8(%rdi), %rax
	movq	(%rax,%rsi,8), %rax
	movq	%rax, (%rdx)
	movl	$1, %eax
LBB1_3:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_vec_length             ## -- Begin function vec_length
	.p2align	4, 0x90
_vec_length:                            ## @vec_length
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	(%rdi), %rax
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_get_vec_start          ## -- Begin function get_vec_start
	.p2align	4, 0x90
_get_vec_start:                         ## @get_vec_start
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	8(%rdi), %rax
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function combine1
LCPI4_0:
	.quad	4607182418800017408     ## double 1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_combine1
	.p2align	4, 0x90
_combine1:                              ## @combine1
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movabsq	$4607182418800017408, %rax ## imm = 0x3FF0000000000000
	movq	%rax, (%rsi)
	movq	(%rdi), %rdx
	testq	%rdx, %rdx
	jle	LBB4_8
## %bb.1:
	movq	8(%rdi), %rcx
	leaq	-1(%rdx), %rdi
	movl	%edx, %eax
	andl	$3, %eax
	cmpq	$3, %rdi
	jae	LBB4_3
## %bb.2:
	vmovsd	LCPI4_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	testq	%rax, %rax
	jne	LBB4_6
	jmp	LBB4_8
LBB4_3:
	subq	%rax, %rdx
	vmovsd	LCPI4_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	.p2align	4, 0x90
LBB4_4:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	8(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	16(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	24(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	addq	$4, %rdi
	cmpq	%rdi, %rdx
	jne	LBB4_4
## %bb.5:
	testq	%rax, %rax
	je	LBB4_8
LBB4_6:
	leaq	(%rcx,%rdi,8), %rcx
	xorl	%edx, %edx
	.p2align	4, 0x90
LBB4_7:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdx,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	incq	%rdx
	cmpq	%rdx, %rax
	jne	LBB4_7
LBB4_8:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function combine2
LCPI5_0:
	.quad	4607182418800017408     ## double 1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_combine2
	.p2align	4, 0x90
_combine2:                              ## @combine2
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	(%rdi), %rdx
	movabsq	$4607182418800017408, %rax ## imm = 0x3FF0000000000000
	movq	%rax, (%rsi)
	testq	%rdx, %rdx
	jle	LBB5_8
## %bb.1:
	movq	8(%rdi), %rcx
	leaq	-1(%rdx), %rdi
	movl	%edx, %eax
	andl	$3, %eax
	cmpq	$3, %rdi
	jae	LBB5_3
## %bb.2:
	vmovsd	LCPI5_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	testq	%rax, %rax
	jne	LBB5_6
	jmp	LBB5_8
LBB5_3:
	subq	%rax, %rdx
	vmovsd	LCPI5_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	.p2align	4, 0x90
LBB5_4:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	8(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	16(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	24(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	addq	$4, %rdi
	cmpq	%rdi, %rdx
	jne	LBB5_4
## %bb.5:
	testq	%rax, %rax
	je	LBB5_8
LBB5_6:
	leaq	(%rcx,%rdi,8), %rcx
	xorl	%edx, %edx
	.p2align	4, 0x90
LBB5_7:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdx,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	incq	%rdx
	cmpq	%rdx, %rax
	jne	LBB5_7
LBB5_8:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function combine3
LCPI6_0:
	.quad	4607182418800017408     ## double 1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_combine3
	.p2align	4, 0x90
_combine3:                              ## @combine3
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	(%rdi), %rdx
	movq	8(%rdi), %rcx
	movabsq	$4607182418800017408, %rax ## imm = 0x3FF0000000000000
	movq	%rax, (%rsi)
	testq	%rdx, %rdx
	jle	LBB6_8
## %bb.1:
	leaq	-1(%rdx), %rdi
	movl	%edx, %eax
	andl	$3, %eax
	cmpq	$3, %rdi
	jae	LBB6_3
## %bb.2:
	vmovsd	LCPI6_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	testq	%rax, %rax
	jne	LBB6_6
	jmp	LBB6_8
LBB6_3:
	subq	%rax, %rdx
	vmovsd	LCPI6_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	.p2align	4, 0x90
LBB6_4:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	8(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	16(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	vmulsd	24(%rcx,%rdi,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	addq	$4, %rdi
	cmpq	%rdi, %rdx
	jne	LBB6_4
## %bb.5:
	testq	%rax, %rax
	je	LBB6_8
LBB6_6:
	leaq	(%rcx,%rdi,8), %rcx
	xorl	%edx, %edx
	.p2align	4, 0x90
LBB6_7:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdx,8), %xmm0, %xmm0
	vmovsd	%xmm0, (%rsi)
	incq	%rdx
	cmpq	%rdx, %rax
	jne	LBB6_7
LBB6_8:
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function combine4
LCPI7_0:
	.quad	4607182418800017408     ## double 1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_combine4
	.p2align	4, 0x90
_combine4:                              ## @combine4
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	(%rdi), %rdx
	testq	%rdx, %rdx
	jle	LBB7_1
## %bb.2:
	movq	8(%rdi), %rcx
	leaq	-1(%rdx), %rdi
	movl	%edx, %eax
	andl	$7, %eax
	cmpq	$7, %rdi
	jae	LBB7_4
## %bb.3:
	vmovsd	LCPI7_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	testq	%rax, %rax
	jne	LBB7_7
	jmp	LBB7_9
LBB7_1:
	vmovsd	LCPI7_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	jmp	LBB7_9
LBB7_4:
	subq	%rax, %rdx
	vmovsd	LCPI7_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	xorl	%edi, %edi
	.p2align	4, 0x90
LBB7_5:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	8(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	16(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	24(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	32(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	40(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	48(%rcx,%rdi,8), %xmm0, %xmm0
	vmulsd	56(%rcx,%rdi,8), %xmm0, %xmm0
	addq	$8, %rdi
	cmpq	%rdi, %rdx
	jne	LBB7_5
## %bb.6:
	testq	%rax, %rax
	je	LBB7_9
LBB7_7:
	leaq	(%rcx,%rdi,8), %rcx
	xorl	%edx, %edx
	.p2align	4, 0x90
LBB7_8:                                 ## =>This Inner Loop Header: Depth=1
	vmulsd	(%rcx,%rdx,8), %xmm0, %xmm0
	incq	%rdx
	cmpq	%rdx, %rax
	jne	LBB7_8
LBB7_9:
	vmovsd	%xmm0, (%rsi)
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
