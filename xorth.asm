;; nasm -felf64 xorth.asm && ld xorth.o -o xorth && ./xorth
;;
;; constants: integers
;; words: [a-z][A-Z]
;;
;; words are stored thus:
;;
;; | WORD_NAME\0 | NEXT WORD ADDR | CODE | NEXT_WORD\0 | ...
;;   32 bytes      8 bytes
;;
;; NOTE: not working


	global	_start

	section	.data
msg:	db	"hello world"
	dd	10
len:	equ	12
s_error:	db	"error: "
l_error:	equ	7
ok:	db	"ok"
	dd	10
inlen:	equ	100
next_dict:	equ	0		;; where to put the next word
barf:	db	"barf"
err_token:	db	"invalid token"

	section	.bss
input:	resb	inlen
dict:	resb	3200

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

%macro	error_	1
	mov	rax, 1
	mov	rdi, 1
	mov	rsi, %1,
	mov	rdx, 100		; TEMP
	syscall
	jmp	exit
%endmacro

_start:
	mov	rsi, "hello\0"
	mov	rdi, dict
	cld
	mov	rcx, 6
	mov	rdi, 999		; next addr TODO

	jmp	repl
exit:
	mov	rax, 60
	mov	rdi, 0
	syscall

repl:
	read_	input, inlen
	mov	r15, rax
	xor	rcx, rcx
	xor	r10, r10

;; read input until we reach whitespace
;; type check token and perform appropriate action
;; if it's valid, loop

;; r15 - total input len
;; r10 - start of current token
;; rcx - position in input

run:
	cmp	byte input[rcx], ' '
	inc	rcx
	jge	token
	jmp	run

token:
t_dot:
	cmp	byte input[r10], '.'
	jne	t_num
	inc	r10
	cmp	r10, rcx
	dec	r10
	je	print
t_num:
	xor	rax, rax
	mov	r8, rcx
	dec	r8
	cmp	byte input[r8], 48
	jge	t_num_2
	jmp	t_word
t_num_2:
	cmp	byte input[r8], 57
	jle	t_num_3
	jmp	t_word
t_num_3:
	add	rax, input[r8]
	push	rax		;; the value of the number
	jmp	tokendone
t_word:
	mov	r8, r10
	cmp	byte input[r8], 97
	jge	t_word_2
	jmp	badtoken
t_word_2:
	cmp	byte input[r8], 122
	jle	t_word_3
	jmp	badtoken
t_word_3:
	cmp	byte input[r8], ' '
	je	t_word
tokendone:
	write_	ok, 3
	mov	r10, rcx
	cmp	rcx, r15
	jge	repl
	jmp	run
badtoken:
	error_	err_token
print:
	write_	barf, 4
	jmp	tokendone

