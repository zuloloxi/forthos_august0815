; program: kernel
; by José Dinuncio <jdinunci@uc.edu.veZ
; 12/2009

%include "forth.h"
global main
extern main_test

[BITS 32]
section .text

; function: main
;   Initialize the forth machinery.
;
global main
main:
			mov [var_S0], esp 			; Save the initial data stack pointer in FORTH variable S0.
            mov ebp, return_stack_top   ; init the return stack
            mov eax,point_HERE			; init HERE
            mov dword [var_HERE],eax	;
            mov esi, cold_start         ; fist foth word to exec
            next

section .rodata

; Bridge to the forth's word main
cold_start:
            dd main_kernel

; function: main
;   Firts foth word to be executed by the kernel
extern pit_init
extern irq_init
extern idt_init
: main_kernel, main_kernel, 0
    idt_init
    100 pit_init
    irq_init
    main_test
    stop
;


; stacks
section   .bss
align 4096
            RETURN_STACK_SIZE equ 8192
return_stack:
            resb RETURN_STACK_SIZE
global return_stack_top
return_stack_top:

align 4096
BUFFER_SIZE equ 4096
buffer:
	resb BUFFER_SIZE

align 4096
pad: 
	resb 1024
align 1024	
point_HERE: resb 2048

point: resb 363748
global top
top: resb 0
