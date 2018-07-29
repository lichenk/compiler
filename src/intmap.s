	.globl	_c0_main

_c0_main:
	pushq	%rbx
	pushq	%rbp
	subq	$0, %rsp
	movl	$4, %eax
	movl	$1, %ebx
	pushq	$0
	movl	%ebx, %esi
	movl	%eax, %edi
	call	calloc
	addq	$8, %rsp
	movq	%rax, %rbx
	movl	$12, %eax
	movl	$1, %ecx
	pushq	$0
	movl	%ecx, %esi
	movl	%eax, %edi
	call	calloc
	addq	$8, %rsp
	movq	%rax, %rbp
	movq	%rbp, %rax
	addq	$4, %rax
	movq	%rax, %r14
	movq	$0, (%r14)
	movq	%rbp, %rax
	movq	%rbp, %rcx
	addq	$4, %rcx
	movq	%rcx, %rsi
	movl	$2, %edx
	movl	$3, %ecx
	pushq	$0
	movq	%rax, %rdi
	call	_c0_insert
	addq	$8, %rsp
	movq	%rbp, %rax
	movq	%rbp, %rcx
	addq	$4, %rcx
	movq	%rcx, %rsi
	movl	$6, %edx
	movl	$7, %ecx
	pushq	$0
	movq	%rax, %rdi
	call	_c0_insert
	addq	$8, %rsp
	movq	%rbp, %rax
	movq	%rbp, %rcx
	addq	$4, %rcx
	movq	%rcx, %rsi
	movl	$4, %edx
	movl	$5, %ecx
	pushq	$0
	movq	%rax, %rdi
	call	_c0_insert
	addq	$8, %rsp
	movq	%rbp, %rax
	movq	%rbp, %rcx
	addq	$4, %rcx
	movq	%rcx, %rsi
	movl	$8, %edx
	movl	$9, %ecx
	pushq	$0
	movq	%rax, %rdi
	call	_c0_insert
	addq	$8, %rsp
	movq	%rbp, %rax
	addq	$4, %rax
	movl	$6, %ecx
	movq	%rbx, %rdx
	pushq	$0
	movl	%ecx, %esi
	movq	%rax, %rdi
	call	_c0_lookup
	addq	$8, %rsp
	movq	%rbx, %rax
	movq	%rax, %r13
	movl	(%r13), %eax
	addq	$0, %rsp
	popq	%rbp
	popq	%rbx
	ret
	.globl	_c0_insert

_c0_insert:
	pushq	%rbx
	pushq	%rbp
	subq	$0, %rsp
	movq	%rdi, %rbp
	movq	%rsi, %rbx
	movl	%edx, %r10d
	movl	%ecx, %r11d
	movq	%rbx, %rcx
	movq	$0, %rdx
	cmpl	%edx, %ecx
	jne	.L26
	movl	$0, %ecx
	jmp	.L27
.L26:
	movl	$1, %ecx
.L27:
	movl	%ecx, %r15d
	cmpl	$0, %r15d
	je	.L18
	movq	%rbx, %rax
	addq	$0, %rax
	movq	%rax, %r13
	movl	(%r13), %edi
	movl	%r10d, %ecx
	movl	%edi, %edx
	cmpl	%ecx, %edx
	jle	.L28
	movl	$0, %edx
	jmp	.L29
.L28:
	movl	$1, %edx
.L29:
	movq	%rdx, %r8
	jmp	.L19
.L18:
	movl	$0, %r8d
	movq	%rdx, %rdi
.L19:
.L20:
	movl	%r8d, %r15d
	cmpl	$0, %r15d
	je	.L21
	movq	%rbx, %rcx
	addq	$0, %rcx
	movq	%rcx, %r13
	movl	(%r13), %esi
	movl	%r10d, %edx
	movl	%esi, %edi
	cmpl	%edx, %edi
	je	.L30
	movl	$0, %edi
	jmp	.L31
.L30:
	movl	$1, %edi
.L31:
	movl	%edi, %r15d
	cmpl	$0, %r15d
	je	.L22
	movq	%rbx, %rdx
	movq	%rdx, %rcx
	addq	$4, %rcx
	movq	%rcx, %r13
	movq	(%r13), %rdx
	movq	%rdx, %rsi
	addq	$0, %rsi
	movq	%rsi, %r14
	movl	%r11d, (%r14)
	movl	$0, %eax
	addq	$0, %rsp
	popq	%rbp
	popq	%rbx
	ret
	jmp	.L23
.L22:
	movq	%rax, %rdx
.L23:
	movq	%rbx, %rax
	addq	$4, %rax
	movq	%rax, %rbx
	movq	%rbx, %rax
	movq	%rax, %rsi
	addq	$4, %rsi
	movq	%rsi, %r8
	movq	%r8, %rax
	movq	$0, %rdi
	movl	%eax, %ecx
	cmpl	%edi, %ecx
	jne	.L32
	movl	$0, %ecx
	jmp	.L33
.L32:
	movl	$1, %ecx
.L33:
	movl	%ecx, %r15d
	cmpl	$0, %r15d
	je	.L24
	movq	%r8, %rdx
	addq	$0, %rdx
	movq	%rdx, %r13
	movl	(%r13), %edi
	movl	%r10d, %esi
	movl	%edi, %ebp
	cmpl	%esi, %ebp
	jle	.L34
	movl	$0, %ebp
	jmp	.L35
.L34:
	movl	$1, %ebp
.L35:
	movq	%rbp, %r9
	jmp	.L25
.L24:
	movl	$0, %r9d
.L25:
	movq	%rbx, %rbp
	movq	%r8, %rbx
	movq	%r9, %r8
	movq	%rax, %rcx
	movq	%rsi, %rcx
	movq	%rdx, %rax
	jmp	.L20
.L21:
	movl	$12, %eax
	movl	$1, %ecx
	pushq	%r10
	pushq	%r11
	pushq	$0
	movl	%ecx, %esi
	movl	%eax, %edi
	call	calloc
	addq	$8, %rsp
	popq	%r11
	popq	%r10
	movq	%rax, %rcx
	movq	%rcx, %rax
	addq	$0, %rax
	movq	%rax, %r14
	movl	%r11d, (%r14)
	movq	%rcx, %rax
	addq	$4, %rax
	movq	%rax, %r14
	movq	%rbx, (%r14)
	movl	$12, %eax
	movl	$1, %ebx
	pushq	%rcx
	pushq	%r10
	pushq	$0
	movl	%ebx, %esi
	movl	%eax, %edi
	call	calloc
	addq	$8, %rsp
	popq	%r10
	popq	%rcx
	movq	%rax, %rbx
	addq	$0, %rbx
	movq	%rbx, %r14
	movl	%r10d, (%r14)
	movq	%rax, %rbx
	addq	$4, %rbx
	movq	%rbx, %r14
	movq	%rcx, (%r14)
	movq	%rbp, %rbx
	addq	$4, %rbx
	movq	%rbx, %r14
	movq	%rax, (%r14)
	movl	$1, %eax
	addq	$0, %rsp
	popq	%rbp
	popq	%rbx
	ret
	.globl	_c0_lookup

_c0_lookup:
	pushq	%rbx
	pushq	%rbp
	subq	$0, %rsp
	movq	%rdi, %rax
	movl	%esi, %r8d
	movq	%rdx, %r9
	movq	%rax, %rcx
	movq	$0, %rdx
	cmpl	%edx, %ecx
	jne	.L8
	movl	$0, %ecx
	jmp	.L9
.L8:
	movl	$1, %ecx
.L9:
	movl	%ecx, %r15d
	cmpl	$0, %r15d
	je	.L0
	movq	%rax, %rbx
	addq	$0, %rbx
	movq	%rbx, %r13
	movl	(%r13), %esi
	movl	%r8d, %ecx
	movl	%esi, %edx
	cmpl	%ecx, %edx
	jle	.L10
	movl	$0, %edx
	jmp	.L11
.L10:
	movl	$1, %edx
.L11:
	movq	%rdx, %rbp
	jmp	.L1
.L0:
	movl	$0, %ebp
	movq	%rdx, %rsi
.L1:
.L2:
	movl	%ebp, %r15d
	cmpl	$0, %r15d
	je	.L3
	movq	%rax, %rcx
	movq	%rcx, %rdx
	addq	$0, %rdx
	movq	%rdx, %r13
	movl	(%r13), %ecx
	movl	%r8d, %edi
	movl	%ecx, %esi
	cmpl	%edi, %esi
	je	.L12
	movl	$0, %esi
	jmp	.L13
.L12:
	movl	$1, %esi
.L13:
	movl	%esi, %r15d
	cmpl	$0, %r15d
	je	.L4
	movq	%r9, %rcx
	addq	$4, %rax
	movq	%rax, %rdx
	movq	%rdx, %rbx
	addq	$0, %rbx
	movq	%rcx, %r14
	movq	%rbx, %r13
	movq	(%r13), %r15
	movl	%r15d, (%r14)
	movl	$0, %eax
	addq	$0, %rsp
	popq	%rbp
	popq	%rbx
	ret
	jmp	.L5
.L4:
.L5:
	movq	%rax, %rdi
	movq	%rdi, %rax
	addq	$4, %rax
	movq	%rax, %rdx
	addq	$4, %rdx
	movq	%rdx, %rax
	movq	%rax, %rbx
	movq	$0, %rsi
	movl	%ebx, %ecx
	cmpl	%esi, %ecx
	jne	.L14
	movl	$0, %ecx
	jmp	.L15
.L14:
	movl	$1, %ecx
.L15:
	movl	%ecx, %r15d
	cmpl	$0, %r15d
	je	.L6
	movq	%rax, %rdx
	movq	%rdx, %rdi
	addq	$0, %rdi
	movq	%rdi, %r13
	movl	(%r13), %esi
	movl	%r8d, %edx
	movl	%esi, %ebp
	cmpl	%edx, %ebp
	jle	.L16
	movl	$0, %ebp
	jmp	.L17
.L16:
	movl	$1, %ebp
.L17:
	jmp	.L7
.L6:
	movl	$0, %ebp
.L7:
	movq	%rbx, %rcx
	movq	%rdx, %rcx
	movq	%rdi, %rbx
	jmp	.L2
.L3:
	movl	$-1, %eax
	addq	$0, %rsp
	popq	%rbp
	popq	%rbx
	ret
