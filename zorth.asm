; nasm -felf64 zorth.asm && ld zorth.o -o zorth && ./zorth

	global	_start

	section	.data
msg:	db	"hello world"
	dd	10
len:	equ	12
s_error:	db	"error: "
l_error:	equ	7
ok:	db	"ok"
	dd	10

	section	.bss
inlen:	equ	100
input:	resb	inlen

	section	.text

%macro	read_	2
	mov	rax, 0
	mov	rdi, 0
	mov	rsi, %1
	mov	rdx, %2
	syscall
%endmacro

%macro	write_	2
	mov	rax, 1
	mov	rdi, 1
	mov	rsi, %1
	mov	rdx, %2
	syscall
%endmacro

_start:
	jmp	repl
exit:
	mov	rax, 60
	mov	rdi, 0
	syscall

repl:
	read_	input, inlen
	write_	ok, 3
	jmp	run
	; jmp	repl
run:
	mov	r8, 0
	mov	r9, input[r8]
	; cmp	r9, ' '
	inc	r8
	cmp	r8, rax
	jge	repl
	jmp	run
	jmp	exit
; token:
; is_num:
; 	mov	ecx, input[r8]
; 	cmp	edx, 4
; 	je	run
; 	jmp	error
error:
	write_	s_error, l_error
	jmp	exit





