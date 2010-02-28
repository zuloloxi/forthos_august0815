; program: forth_words
; The basic words of the forth language.

; This file is a translation of jonesforth 
; (http://www.annexia.org/_file/jonesforth.s.txt) for being compiled with nasm.

; License: GPL
; José Dinuncio <jdinunci@uc.edu.ve>, 12/2009.
; This file is based on Bran's kernel development tutorial file start.asm

%include "forth_macros.s"
%include "forth_core.h"

extern name_exit
%undef OLDLINK
%xdefine LINK name_exit

[BITS 32]
; forthword ptrs contains the basic words of a forth interpreter. The escential
; routines and word ptrs are in forthcore.

; function: stop
;   Endless loop.
defcode stop, stop, 0
        jmp $


; function: lit 
;   Takes the next word (a literal value) in a word definition and stores it in 
;   the stack.
;
; Stack:
;   -- n
defcode lit, lit, 0
        lodsd               ; Load the next word in the current definition
        push eax            ; pushes it on the stack
        next                ; and executes the following word
            
; function: drop 
; Stack:
;   n --
defcode drop, drop,0
        pop eax       
        next

; function: swap
; Stack:
;   a b -- b a
defcode swap, swap,0
        pop eax       
        pop ebx
        push eax
        push ebx
        next

; function: dup
; Stack:
;   a -- a a
defcode dup, dup, 0
        mov eax, [esp]    
        push eax
        next

; function: over
; Stack:
;   a b -- a b a
defcode over, over, 0
        mov eax, [esp + 4]   
        push eax      
        next

; function: rot
; Stack:
;   a b c -- b c a
defcode rot, rot, 0
        pop eax
        pop ebx
        pop ecx
        push ebx
        push eax
        push ecx
        next

; function: -rot (nrot)
; Stack:
;   a b c -- c a b
defcode -rot, nrot, 0
        pop eax
        pop ebx
        pop ecx
        push eax
        push ecx
        push ebx
        next

; function: 2drop (twodrop)
; Stack:
;   a b --
defcode 2drop, twodrop, 0
        pop eax
        pop eax
        next

; function: 2dup (twodup)
; Stack:
;   a b -- a b a b
defcode 2dup, twodup, 0
        mov eax, [esp]
        mov ebx, [esp + 4]
        push ebx
        push eax
        next

; function: 2swap (twoswap)
; Stack:
;   a b c d -- c d a b
defcode 2swap, twoswap, 0
        pop eax
        pop ebx
        pop ecx
        pop edx
        push ebx
        push eax
        push edx
        push ecx
        next

; function: ?dup (qdup)
;   Consume if the top of the stack is zero.
;
; Stack:
;   0 --
;   n -- n
defcode ?dup, qdup, 0
        mov eax, [esp]
        test eax, eax
        jz .1
        push eax
.1: next

; function: 1+ (incr)
; Stack:
;   n -- n+1
defcode 1+, incr, 0
        inc dword [esp]    
        next

; function: 1- (decr)
; Stack:
;   n -- n-1
defcode 1-, decr, 0
        dec dword [esp]    
        next

; function: 4+ (incr4)
; Stack:
;   n -- n+4
defcode 4+, incr4, 0
        add dword [esp], 4     
        next

; function: 4- (decr4)
; Stack:
;   n -- n-4
defcode 4-, decr4, 0
        sub dword [esp], 4     
        next

; function: + (add)
; Stack:
;   a b -- a+b
defcode +, add, 0
        pop eax       
        add [esp], eax   
        next

; function: - (sub)                                                             
; Stack:
;   a b -- b-a
defcode -, sub, 0
        pop eax       
        sub [esp], eax   
        next

; function: * (mul)
; Stack:
;   a b -- a*b
defcode *, mul, 0
        pop eax
        pop ebx
        imul eax, ebx
        push eax      
        next


;  In this FORTH, only /mod is primitive.  Later we will define the / 
;  and mod word ptrs in terms of the primitive /mod.  The design of the i386 
;  assembly instruction idiv which leaves both quotient and remainder makes 
;  this the obvious choice.

; function: /mod (divmod)
; Stack:
;   a b -- a%b a/b
defcode /mod, divmod, 0
        xor edx, edx
        pop ebx
        pop eax
        idiv ebx
        push edx      
        push eax      
        next

; function: / (div)
; Stack:
;   a b -- a/b
defword /, div, 0
        dd divmod
        dd swap
        dd drop
        dd exit

; function: mod
; Stack:
;   a b -- a%b
defword mod, mod, 0
        dd divmod
        dd drop
        dd exit

; Comparisons
; function: = (equ)
; Stack:
;  n -- bool
defcode =, equ, 0
        pop eax
        pop ebx
        cmp eax, ebx
        sete al
        movzx eax, al
        push eax
        next

; function: <> (nequ)
; Stack:
;  n -- bool
defcode <>, nequ, 0
        pop eax
        pop ebx
        cmp eax, ebx
        setne al
        movzx eax, al
        push eax
        next

; function: < (lt)
; Stack:
;   n -- bool
defcode <, lt, 0
        pop eax
        pop ebx
        cmp ebx, eax
        setl al
        movzx eax, al
        push eax
        next

; function: > (gt)
; Stack:
;   n -- bool
defcode >, gt, 0
        pop eax
        pop ebx
        cmp ebx, eax
        setg al
        movzx eax, al
        push eax
        next

; function: <= (le)
; Stack:
;   n -- bool
defcode <=, le, 0
        pop eax
        pop ebx
        cmp ebx, eax
        setle al
        movzx eax, al
        push eax
        next

; function: >= (ge)
; Stack:
;   n -- bool
defcode >=, ge, 0
        pop eax
        pop ebx
        cmp ebx, eax
        setge al
        movzx eax, al
        push eax
        next

; function: 0= (zequ)
; Stack:
;   n -- bool
defcode 0=, zequ, 0
        pop eax
        test eax, eax
        setz al
        movzx eax, al
        push eax
        next

; function: 0<> (znequ)
; Stack:
;  n -- bool
defcode 0<>, znequ, 0
        pop eax
        test eax, eax
        setnz al
        movzx eax, al
        push eax
        next

; function: 0< (zlt)
; Stack:
;   n -- bool
defcode 0<, zlt, 0
        pop eax
        test eax, eax
        setl al
        movzx eax, al
        push eax
        next

; function: 0> (zgt)
; Stack:
;   n -- bool
defcode 0>, zgt, 0
        pop eax
        test eax, eax
        setg al
        movzx eax, al
        push eax
        next

; function: 0<= (zle)
; Stack:
;   n -- bool
defcode 0<=, zle, 0
        pop eax
        test eax, eax
        setle al
        movzx eax, al
        push eax
        next

; function: 0>= (zge)
; Stack:
;   n -- bool
defcode 0>=, zge, 0
        pop eax
        test eax, eax
        setge al
        movzx eax, al
        push eax
        next

; function: and
; Stack:
;   a b -- a&b
defcode and, and, 0   
        pop eax
        and [esp], eax
        next

; function: or
; Stack:
;   a b -- a|b
defcode or, or, 0 
        pop eax
        or [esp], eax
        next

; function: xor
; Stack:
;   a b -- (a xor b)
defcode xor, xor, 0   
        pop eax
        xor [esp], eax
        next

; function: invert
; Stack:
;   a -- !a
defcode invert, invert, 0
        not dword [esp]
        next

; function: ! (store)
;   Stores a value in an address.
;
; Stack:
;   n addr --
defcode !, store, 0
        pop ebx       
        pop eax       
        mov [ebx], eax    
        next

; function: @ (fetch)
;   Gets the value in an address
;
; Stack:
;   addr -- v
defcode @, fetch, 0
        pop ebx       
        mov eax, [ebx]    
        push eax      
        next

; function: +! (addstore)
;   Add a value to the content of an address.
;
; Stack:
;   v addr --
defcode +!, addstore, 0
        pop ebx       
        pop eax       
        add [ebx], eax   
        next

; function: -! (substore)
;   Substract a value to the content of an address.
;
; Stack:
;   v addr --
defcode -!, substore, 0
        pop ebx       
        pop eax       
        sub [ebx], eax   
        next

; function: c! (storebyte)
;   Store a byte in an address.
;
; Stack:
;   b addr --
defcode c!, storebyte, 0
        pop ebx       
        pop eax       
        mov [ebx], al    
        next

; function: c@ (fetchbyte)
;   Fetchs a byte from an address.
;
; Stack:
;   addr -- b
defcode c@, fetchbyte, 0
        pop ebx       
        xor eax, eax
        mov al, [ebx]    
        push eax      
        next

; function: w! (storeword)
;   Store a word in an address.
;
; Stack:
;   w addr --
defcode w!, storeword, 0
        pop ebx       
        pop eax       
        mov [ebx], ax    
        next

; function: w@ (fetchword)
;   Fetchs a word form an address.
;
; Stack:
;   addr -- w
defcode w@, fetchword, 0
        pop ebx       
        xor eax, eax
        mov ax, [ebx]    
        push eax      
        next

; function: c@c! (ccopy)
;   Copy a byte from an address to another and increments both addresses.
;
; Stack:
;   &src &dst -- (&src+1) (&dst+1)
defcode c@c!, ccopy, 0
        mov ebx, [esp + 4]  	;movl 4(%esp),%ebx	// source address
        mov al, [ebx]    		;movb (%ebx),%al		// get source character
        pop edi       			;pop %edi		// destination address
        stosb          			;stosb			// copy to destination
        push edi    			;push %edi		// increment destination address
        inc dword [esp + 4]     ;incl 4(%esp)		// increment source address  
        next

; function: cmove
;   Block copy.
;
; Stack:
;   &s &d n --
; 
; Params:
;   &s - Source Address
;   &d - Destination Address
;   n  - Number of bytes to copy
defcode cmove, cmove, 0
        mov edx, esi      
        pop ecx       
        pop edi       
        pop esi       
        rep movsb      
        mov esi, edx      
        next

; function:  >R (tor)
;
; Stack:
;   --
defcode >r, tor, 0
        pop eax       
        pushrsp eax       
        next

; function: R> (fromr)
;
; Stack:
;   --
defcode r>, fromr, 0
        poprsp eax    
        push eax      
        next

; function: rsp@ (rspfetch)
;
; Stack:
;   --
defcode rsp@, rspfetch, 0
        push ebp
        next

; function: rsp! (rspstore)
;   Pops the return stack and trow away.
;
; Stack:
;   --
defcode rsp!, rspstore, 0
        pop ebp
        next

; function: rdrop
;
; Stack:
;   --
defcode rdrop, rdrop, 0
        add ebp, 4       
        next

; Branching

; function: branch
;   Unconditional relative branch.
;
; The next codeword is a literal which indicate how many bytes (positive
; or negative) it is going to jump. So, to jump four word ahead, the literal
; must be 8*4=32.
;
; Stack:
;   --
defcode branch, branch, 0
        add esi, [esi]
        next

; function: 0branch (zbranch)
;   branch if zero.
;
; The next codeword is a literal which indicate how many bytes (positive
; or negative) it is going to jump. So, to jump four word ahead, the literal
; must be 8*4=32.
;
; Stack:
;   n --
defcode 0branch, zbranch, 0
        pop eax
        test eax, eax
        jz code_branch 
        lodsd           
        next

; Data stack manipulation

; function: dsp@ (dspfetch)
;
; Stack:
;   --
defcode dsp@, dspfetch, 0
        mov eax, esp
        push eax
        next

; function: dsp! (dspstore)
;
; Stack:
;   --
defcode dsp!, dspstore, 0
        pop esp
        next

; function: shl
;   Shift to the left
;
; Stack:
;   n1 n2 -- n1 << n2
defcode shl, shl, 0
        pop ecx
        pop eax
        shl eax, cl
        push eax
        next

; function: shr
;   Shift to the right
;
; Stack:
;   n1 n2 -- n1 >> n2
defcode shr, shr, 0
        pop ecx
        pop eax
        shr eax, cl
        push eax
        next

; function: n_byte
;   Gives the n-th byte of a cell
;
; Stack:
;   b3b2b1b0 n -- bn
defword n_byte, n_byte, 0
        litn 8
        dd mul
        dd shr
        litn 0xff
        dd and
        dd exit

;-------------This Code is taken from bb4wforth.f---see--README-------
       
defcode d+,  dadd, 0 ; added by rtr
        pop edx                 ; ms 32-bits
        pop eax                 ; ls 32-bits (forth is big-endian)
        add dword [esp+4],eax
        adc dword [esp],edx
        next
         
defcode d-, dsub,0 ; added by rtr
        pop edx                 ; ms 32-bits
        pop eax                 ; ls 32-bits (forth is big-endian)
        sub dword [esp+4],eax
        sbb dword [esp],edx
        next

defcode um/mod, umdivmod , 0 ; added by rtr (unsigned)
        pop ebx
        pop edx
        pop eax
        div ebx
        push edx                ; push remainder
        push eax                ; push quotient
        next
        
defcode um*,  umul64 ,0  ; added by rtr
        pop eax
        pop ebx
        mul ebx                 ; unsigned multiply
        push eax                ; ls 32 bits (forth is big-endian)
        push edx                ; ms 32 bits
        next 
        
defcode r@, rfetch, 0 ; added by rtr
        mov eax,[ebp]
        push eax
        next  
              
defcode m*, mul64 ,0 ; added by rtr
        pop eax
        pop ebx
        imul ebx                ; signed multiply
        push eax                ; ls 32 bits (forth is big-endian)
        push edx                ; ms 32 bits
        next
        
defcode sm/rem, smdivrem, 0 ; added by rtr (signed)
        pop ebx
        pop edx
        pop eax
        idiv ebx
        push edx                ; push remainder
        push eax                ; push quotient
        next
        
defcode fm/mod, fmdivmod,0 ; added by rtr (floored)
        pop ebx
        pop edx
        pop eax
        idiv ebx
        or edx,edx
        jz _fmmodx              ; no remainder
        or eax,eax
        jns _fmmodx             ; quotient is positive
        dec eax
        add edx,ebx
_fmmodx:
        push edx                ; push remainder
        push eax                ; push quotient
        next
        
defcode */, muldiv,0 ; added by rtr
        pop ecx
        pop ebx
        pop eax
        imul ebx
        idiv ecx
        push eax
        next
        
defcode */mod, muldivmod ,0 ; added by rtr
        pop ecx
        pop ebx
        pop eax
        imul ebx
        idiv ecx
        push edx                ; push remainder
        push eax                ; push quotient
        next
        
defcode 2*, twomul, 0 ; added by rtr
        shl dword [esp],1
        next
        
defcode 2/, twodiv, 0 ; added by rtr
        sar dword [esp],1
        next
        
defcode u2/, utwodiv,0 ; added by rtr
        shr dword [esp],1
        next

defcode u<, ult,0 ; added by rtr
        pop eax
        pop ebx
        cmp ebx,eax
        setb al
        movzx eax,al
        neg eax
        push eax
       next
  
 defcode u>, ugt,0 ; added by rtr
        pop eax
        pop ebx
        cmp ebx,eax
        seta al
        movzx eax,al
        neg eax
        push eax
       next

defcode s>d, stod,0   ; added by rtr
        pop eax
        cdq
        push eax                ; ls 32 bits (forth is big-endian)
        push edx                ; ms 32 bits
       next
        
defcode roll, roll,0 ; added by rtr
        pop ecx
        jecxz _roll_next
        lea edi,[esp+ecx*4]
        lea ebx,[edi-4]
        mov eax,[edi]
        std
        xchg esi,ebx
        rep movsd
        xchg esi,ebx
        cld
        mov [esp],eax
_roll_next:
       next  
       
defcode (does), does2, 0
        lea ebp, [ebp-4]
        mov [ebp],esi
        pop esi
        add eax,4
        push eax
        next
        

defcode leave, LEAVE, 0  ; added by rtr
        lea ebp,[ebp+12]        ; pop return stack
        jmp _leave
        
        
defcode ?do, qdo, 0 ; added by rtr
        pop ecx                 ; initial index
        pop edx                 ; limit
        cmp ecx,edx
        jne _dogo
_leave:
        mov ecx,1
        xor ebx,ebx
_qdo_loop:
        lodsd
        cmp eax,DO              ; nested loop ?
        setz bl
        add ecx,ebx
        cmp eax,qdo
        setz bl
        add ecx,ebx
        cmp eax,LOOP
        setz bl
        sub ecx,ebx
        cmp eax,ploop
        setz bl
        sub ecx,ebx
        or ecx,ecx
        jnz _qdo_loop
        next
        
defcode do,  DO, 0 ; added by rtr
        pop ecx                 ; initial index
        pop edx                 ; limit
_dogo:
        lea ebp,[ebp-12]        ; make room on return stack
        mov [ebp+8],esi
        mov [ebp+4],edx
        mov [ebp],ecx
        next
        
defcode +loop, ploop, 0 ; added by rtr
        pop eax                 ; step
        jmp _loop_step
        
defcode loop, LOOP, 0 ; added by rtr
        mov eax,1               ; default step
_loop_step:
        ;test byte [bflags],&81
        ;jnz near _escape        ; escape key pressed or close
        mov ebx,[ebp]           ; index
        sub ebx,[ebp+4]         ; subtract limit
        btc ebx,31              ; invert msb
        add ebx,eax             ; step
        jo _unloop              ; overflow signals loop end
        btc ebx,31              ; invert msb again
        add ebx,[ebp+4]         ; add limit back
        mov [ebp],ebx           ; new index
        mov esi,[ebp+8]
        next       ; continue looping
        
defcode unloop, unloop, 0  ; added by rtr
_unloop:
        lea ebp,[ebp+12]        ; pop return stack
        next       ; exit loop
        
defcode I, I , 0 ; added by rtr
        push dword [ebp]        ; index
        next       ; exit loop
        
defcode J, J, 0 ; added by rtr
        push dword [ebp+12]     ; outer index
        next       ; exit loop

;-------------this code is taken from bb4wforth.f---see--readme-------

; function: execute
;   Executes the word which address in in the stack
;
; stack:
;   addr -- ??
defcode execute, execute, 0
        pop eax
        jmp [eax]
        
global name_execute
