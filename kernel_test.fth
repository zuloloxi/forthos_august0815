; program: kernel_test
; Test for the kernel.

; License: GPL
; José Dinuncio <jdinunci@uc.edu.ve>, 12/2009.

%include "forth.h"
%include "kernel_words.h"
%include "kernel_video.h"
%include "kernel_kbd.h"
%include "irq.h"

extern name_getchar
%undef OLDLINK
%xdefine LINK name_getchar

[BITS 32]
: print_scancode, print_scancode, 0
    kbd_scancode dup intprint spc sc>c spc  emit cr
;

defvar tic_count, tic_count, 0, 0
: print_tic, print_tic, 0
    1 tic_count +!
    tic_count @ 100 mod 0= if
      tic_msg printcstring cr
    then
;

: print_scancodes, print_scancodes, 0
    begin print_scancode 0 until
;

: print_interrupt, print_interrupt, 0
    fault printcstring cr 
;

; prints an idt entry
: print_idtentry, print_idtentry, 0
    dup 4 + @   swap @              ; wh wl
    dup hi hexprint spc             ; sel
        lo hexprint spc             ; base lo
    dup hi hexprint spc             ; base hi
        lo 8 shr hexprint cr        ; flags
;

; test irq
defcode test_irq, test_irq, 0
    int 33
    next

; divide by zero
: div_by_zero, div_by_zero, 0
    2 0 / drop
;

; Print hello word
: print_hello, print_hello, 0
    hello printcstring cr
;

%define _invoke_addr print_hello
: test_invoke, test_invoke, 0
    _invoke_addr execute
;

; function: ZEILE  ; einlesen einer Zeile bis CR   TESTED_OK
;
; Stack:
; address_of_text_buffer  -- 
; zeile_buffer:  ist 1024 byte lang
; ( char text_buffer text_buffer -- text_buffer )
: store_in_buffer, store_in_buffer,0
	swap dup 1+ -rot c!
;
: zeile, zeile, 0
        1
        begin
        while
        getchar dup  0x09 	;TAB
        =
        if 
        	drop tab ;branch repn
        then
        dup  0x0D =
        if
        	drop 0x20 store_in_buffer
                 0	  store_in_buffer
            drop  
        	exit
        then 
        dup  0x08 	;BS backspace
        =
        if 
        	drop
       		cursor_back		; del the char
       		0x20 emit  cursor_back   ; the position on back !
       		1-					   	; position of text_buffer(input) on back 
       		;branch repn
		else 
			dup emit store_in_buffer 
        then        
 		1
	    repeat
;
text_buffer: times 1024 db 0
  
: tstout, tstout, 0
	text_buffer dup
	cr printcstring cr
	'>' emit
;

defvar END_OF_LINE, END_OF_LINE, 0 , 0
defvar PARS_ERROR, PARS_ERROR, 0 , 0

; function:  NUMBER  TESTED_OK
;
; IN : ecx 	 length of string
;
;     edi 	 start address of string
;
; OUT:eax parsed number
;
;     ecx number of unparsed characters (0 = no error)
defcode number, number, 0
	pop ecx		; length of string
	pop edi		; start address of string
	call _NUMBER
	push eax		; parsed number
	push ecx		; number of unparsed characters (0 = no error)
	next
_NUMBER:
	xor eax,eax
	xor ebx,ebx
	test ecx,ecx		; trying to parse a zero-length string is an error, but will return 0.
	jz .5
	mov edx,[var_BASE]	; get BASE (in %dl)
	; Check if first character is '-'.
	mov bl,[edi]		; %bl = first character in string
	inc edi
	push eax		; push 0 on stack
	cmp bl,'-'		; negative number?
	jnz .2
	pop eax
	push ebx		; push <> 0 on stack, indicating negative
	dec ecx
	jnz .1
	pop ebx		; error: string is only '-'.
	mov ecx, $1
	ret
	; Loop reading digits.
.1:	imul eax,edx		; %eax *= BASE
	mov bl,[edi]		; %bl = next character in string
	inc edi
	; Convert 0-9, A-Z to a number 0-35.
.2:	sub bl,'0'		; < '0'?
	jb .4
	cmp bl,$10		; <= '9'?
	jb .3
	sub bl,$17		; < 'A'? (17 is 'A'-'0')
	jb .4
	add bl,$10
.3:	cmp bl,dl		; >= BASE?
	jge .4
	; OK, so add it to %eax and loop.
	add eax,ebx
	dec ecx
	jnz .1
	; Negate the result if first character was '-' (saved on the stack).
.4:	pop ebx
	test ebx,ebx
	jz .5
	neg eax
.5:	ret


; function: FIND   TESTED_OK
;
; IN: ecx = length
; edi = address
;
;OUT: ; eax = address of dictionary entry (or NULL)
defcode find, find, 0
	pop ecx		; ecx = length
	pop edi		; edi = address
	call _FIND
	
	push eax		; eax = address of dictionary entry (or NULL)
	next
_FIND:
    push esi		; Save esi so we can use it in string comparison.
	; Now we start searching backwards through the dictionary for this word.
	mov edx,[var_LATEST]	; LATEST points to name header of the latest word in the dictionary
.1:	test edx,edx		; NULL pointer?  (end of the linked list)
	je .4
	; Compare the length expected and the length of the word.
	; Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
	; this won't pick the word (the length will appear to be wrong).
	xor eax,eax
	mov al,[edx+4]		; %al = flags+length field
	and al,(0x20| 0x1f)	;F_HIDDEN|F_LENMASK) ; %al = name length
	cmp byte al,cl		; Length is the same?
	jne .2
	; Compare the strings in detail.
	push ecx		; Save the length
	push edi		; Save the address (repe cmpsb will move this pointer)
	lea esi,[edx+5]	; Dictionary string we are checking against.
	repe cmpsb		; Compare the strings.
	pop edi
	pop ecx
	jne .2			; Not the same.
	; The strings are the same - return the header pointer in %eax
	pop esi
	mov eax,edx
	ret
.2:	mov edx,[edx]		; Move back through the link field to the previous word
	jmp .1			; .. and loop.
.4:	; Not found.
	pop esi
	xor eax,eax		; Return zero to indicate not found.
	ret



; function: ">CFA"  TESTED_OK
defcode >CFA, TCFA, 0
	pop edi
	call _TCFA
	push edi
	next
_TCFA:
	xor eax,eax
	add edi,4		; Skip link pointer.
	mov al,[edi]		; Load flags+len into %al.
	inc edi		; Skip flags+len byte.
	and al,0x1f 	;F_LENMASK	; Just the length, not the flags.
	add edi,eax		; Skip the name.
	add edi,3		; The codeword is 4-byte aligned.
	and edi,~3
	ret



; function: >DFA
: >DFA, TDFA, 0
	 >CFA	;	(get code field address)
	 4+		;   (add 4 to it to get to next word)
;
	
; function: HEADER ; TESTED_OK
defcode header, header, 0
	    pop     ecx             ; rcx = length
        pop     edx             ; rdx = address of name
        mov     edi,    [var_HERE]
        mov     eax,    [var_LATEST]
        stosd                   ; link を設定
        ;xor     eax,    eax
        ;stosb                   ; flags を設定
        mov     al,     cl
        stosb                   ; length を設定
        push    esi             ; rsi 退避
        mov     esi,    edx     ; address of name
        rep     movsb           ; name を設定
        pop     esi             ; rsi 復元
       lea edi,[edi+3]  ; add     edi,    3      ; align 8
        and     edi,    ~3
        mov     eax,    [var_HERE]
        mov     [var_LATEST],   eax
        mov     [var_HERE],     edi
        next
     
; defcode; "," TESTED_OK
	defcode comma, comma, 0
	pop eax		; Code pointer to store.
	call _COMMA
	next
_COMMA:
  	mov edi,[var_HERE]	; HERE
	stosd			; Store it.
	mov dword [var_HERE],edi	; HERE
	ret

; function: [   TESTED_OK
defcode [, LBRAC, 0x80 ;;F_IMMED,LBRAC,0
	mov dword [var_STATE],0	; Set STATE to 0.
	next
; defcode ]	   TESTED_OK
defcode ], RBRAC, 0
	mov dword [var_STATE],1	; Set STATE to 1.
	next

; function: :   
: COL, COLON  ,0
	wort			; Get the name of the new word
    header			; HEADER the dictionary entry / header
	lit  DOCOL  comma	; Append DOCOL  (the codeword).
	LATEST @  hidden ; Make the word hidden (see below for definition).
	]		; Go into compile mode.
;

; function: ;
: sk,semicolon, 0x80 ;F_IMMED 
	STATE @
	if
	lit exit  comma			; Append EXIT (so the word will return).
	LATEST @  hidden	 	; Toggle hidden flag -- unhide the word (see below for definition).
	[					; Go back to IMMEDIATE mode.
	then
;

; function: IMMEDIATE  TESTED_OK
defcode immediate, immediate, 0x80 ; F_IMMED
	mov edi,[var_LATEST]	; LATEST word.
	add edi,4		; Point to name/flags byte.
	xor	byte [edi],0x20 ; F_IMMED	; Toggle the IMMED bit.
	next

; function: HIDDEN 
defcode hidden, hidden, 0
	pop edi		; Dictionary entry.
	add edi,4		; Point to name/flags byte.
	xor byte [edi],0x20   ;F_HIDDEN	; Toggle the HIDDEN bit.
	next
	
; function: HIDE	
: hide, hide, 0
	wort		; Get the word (after HIDE).
	find		; Look up in the dictionary.
	hidden		; Set F_HIDDEN flag.
;

; function: "'"  TESTED_OK
defcode tt, tick, 0 
	lodsd			; Get the address of the next word and skip it.
	push eax		; Push it on the stack.
	next
	
; TODO Branching??

; function: LITSTRING
defcode litstring, litstring, 0
	lodsd			; get the length of the string
	push esi		; push the address of the start of the string
	push eax		; push it on the stack
	add esi,eax		; skip past the string
 	add esi,3		; but round up to next 4 byte boundary
	and esi,~3
	next

	

; function: TEILWORT  rename later to WORD ; TESTED_OK 
;
; gibt den pointer des strings aus zeilenbuffer bis zum Leerzeichen
; zurück , PPTR zeigt danach auf das nächste Wort
; edi  		; push base address
; ecx		; push length

defcode wort, wort , 0
	call _word
	push edi		; push base address
	push ecx		; push length
	next

_word:
	;/* Search for first non-blank character.  Also skip \ comments. */
    mov ebx,[var_PPTR]
.1:
	mov al,[ebx] ;_KEY		; get next key, returned in %eax
	test al,al
	jnz .5
	mov dword [var_END_OF_LINE],0xffff
	ret
.5:	inc ebx
	cmp al,'\'		; start of a comment?
	je .3			; if so, skip the comment
	cmp al,' '
	jbe .1			; if so, keep looking
	;/* Search for the end of the word, storing chars as we go. */
	mov edi,ptr_buff	; pointer to return buffer
.2:
	stosb			; add character to return buffer
	mov al,[ebx] ;_KEY		; get next key, returned in %eax
	inc ebx; _KEY		; get next key, returned in %al
	cmp al,' '		; is blank?
	ja .2			; if not, keep looping
	
	;/* Return the word (well, the static buffer) and length. */
	sub edi,ptr_buff
	mov ecx,edi		; return length of the word
	mov edi,ptr_buff	; return address of the word
	mov dword [var_PPTR],ebx
	ret
.4:	
	;/* Code to skip \ comments to end of the current line. */
.3:
	mov al,[ebx] ;_KEY		; get next key, returned in %eax
	inc ebx ;_KEY
	cmp al,$13	; end of line yet?
	jne .3
	jmp .1
section .data			; NB: easier to fit in the .data section
	; A static buffer where WORD returns.  Subsequent calls
	; overwrite this buffer.  Maximum word length is 256 chars.
ptr_buff: times 256 db 0
		
section .text

: quit, quit, 0
  R0  rsp! 
 1 begin while ZEIL  qstack -1 repeat ; loops forever
;
 
; function: TELL   rewrite it !!!! still for linux
: tell, tell, 0
	drop printcstring ;printt
;

; function: echooff   TESTED_OK
: echooff, echooff, 0
			0 NOECHO !
;	
; function: echoon   TESTED_OK
: echoon, echoon, 0
			0 NOECHO !
;
; function: PRESSKEY   TESTED_OK
: presskey, presskey, 0
      		key_press printcstring tab '!' emit getchar drop clear
;
  		   
;defcode: INTERPRET    better now 
defcode interpret, interpret, 0  
	mov	dword [var_PARS_ERROR],0	
	call _word ; Returns %ecx = length, %edi = pointer to word.
	; Is it in the dictionary?
	xor eax,eax
	mov dword [interpret_is_lit],eax ; Not a literal number (not yet anyway ...)
	call _FIND		; Returns %eax = pointer to header or 0 if not found.
	test eax,eax		; Found?
	jz .1
	
	; In the dictionary.  Is it an IMMEDIATE codeword?
	mov edi,eax		; %edi = dictionary entry
	mov al,[edi+4]	; Get name+flags.
	push ax		; Just save it for now.
	call _TCFA		; Convert dictionary entry (in %edi) to codeword pointer.
	pop ax
	and al,0x80     ;F_IMMED	; Is IMMED flag set?
	mov eax,edi
	
	jnz .4 			; If IMMED, jump straight to executing.
    
	jmp .2

.1:	; Not in the dictionary (not a word) so assume it's a literal number.
    ;
	inc dword [interpret_is_lit]
	call _NUMBER		; Returns the parsed number in %eax, %ecx > 0 if error
	test ecx,ecx
	jnz .6
	mov ebx,eax
	mov eax,lit		; The word is LIT

.2:	; Are we compiling or executing?
	;--------------NOW COMPILING !!-----------------------------
	mov	dword edx, [var_STATE]
	test	edx, edx
	jz	.4			; Jump if executing.

	; Compiling - just append the word to the current dictionary definition.
	call	 _COMMA
	mov	ecx, [interpret_is_lit] ; Was it a literal?
	test	ecx, ecx
	jz	.3
	mov eax,ebx		; Yes, so LIT is followed by a number. 
	call	 _COMMA
.3:	next

.4:	; Executing - run it!
	mov ecx,[interpret_is_lit] ; Literal?
	test ecx,ecx		; Literal?
	jnz .5
    ; Not a literal, execute it now.  This never returns, but the codeword will
	; eventually call next which will reenter the loop in QUIT.
	jmp [eax]

.5:	; Executing a literal, which means push it on the stack.
	push ebx
	next

.6:	; Parse error (not a known word or a number in the current BASE).
	; Print an error message followed by up to 40 characters of context.
	;mov ebx,2		; 1st param: stderr
	mov	dword [var_PARS_ERROR] ,0xffff
	next

defcode char, char, 0
	call _word
	xor eax,eax
	mov al,[edi]
	push eax	
	next
	
; funktion: printt 
; prints an string of len , pointer to string
: printt, printt, 0
 1- 0   do
 	  	rot  dup @ emit 1+ -rot
 	  loop
 	  drop
; 

: u., udot, 0
	BASE @ /mod	?dup
	if 				;( if quotient <> 0 then )
	 	 udot
	else
	then
		dup 10 <
		if
	 		 '0'  ;(decimal digits 0..9 )
		else
			10 - 'A'
	 	then
	 	+ emit	
;

: .s, dots, 0
	'>' emit dsp@
	begin
		dup S0 @ <
	while
		dup @ udot spc 4+
	repeat
	drop '<' emit
;

; function: inter
; ( -- )    
;| the interpreter loop
;| tests for  'INTERPRET' errors and shows the result of interpret/compile   
: inter, inter,0	
 			0 END_OF_LINE !
			NOECHO @ 0<>
			if
				cr
			then
			1
			begin
			while 
			interpret
			END_OF_LINE @ 0<>   ; endof line Interprt was OK
 			if
				NOECHO @ 0<>
				if
					cr ok printcstring cr  	
			 	then
			 	0  dup END_OF_LINE ! PARS_ERROR !
				 ;clear Error_flag
           		 ;clear End_of_Line fla
            	exit
			then	 
			PARS_ERROR @ 0<>     ; error in einput stream
 			if  	
				cr 10 ink text_buffer printcstring	
				cr 12 ink errmsg      printcstring 
			 	PPTR_LAST @ 10  printt cr
			 	15 ink presskey
				0  dup END_OF_LINE ! PARS_ERROR ! exit
           	then	 
			PPTR @  PPTR_LAST !
			1
			repeat
;			

; function: linecopy 
; ( -- )
;| reads from source until ';' char is found in stream
;| replace in the stream
;| 'lf'  with SPACE
;| 'tab' with SPACE
;| if ';' is found then 'CR' an 0 is added (to text_buffer) 
;| this simulates an keyboard input with 'CR' , so the interpreter will
;| execute  the line  
: ->PPTR, tPPRT, 0
	PPTR @ c! 1 PPTR +! 1+
;
: endln, endln, 0
	0x3b ->PPTR FILP ! 0xd ->PPTR FILP ! 0x0 PPTR @ c!
; 
: linecopy, linecopy, 0
	dup c@ 	 ; IF LF is the first char
	0x0a =
	if 
	;branch lf			; goto lf	
	then
	
	begin
	dup c@ dup 0x3b <>
	while
	 dup 0x0a =  ; wenn LF dann SPACE
	 if 
	  drop 0x20
	 then
	 dup 0x09 =   ; wenn TAB dann SPACE
	 if 
	  drop 0x20
	 then
	 ->PPTR
	repeat
	endln ; CR and 0 -> ENDING 0 for PRINTSTRING
;
	
; function: interforth
; ( -- )
;| executes the loaded ( via GRUB) file
: interforth, interforth, 0
	echooff
	SRC @    	; source
	FILP !		; file_position_pointer
	text_buffer	; 
	PPTR !		; input_line_source_pointer
1	
	begin	
	while
    FILP @	
    linecopy
	text_buffer PPTR !
 	NOECHO @ 0<>
	if
	 cr cr text_buffer printcstring
	then
	
	inter
    text_buffer dup PPTR_LAST ! ; remember the last word witch compiled without error
	PPTR !
    
	1
	FILP +1 
	FILP @		; is next char = 0
	c@ 		; then it is  EOF
	?dup
	if 
	 	-1
		FILP		; no , go for next line_input
		+!
	 	;dd DROP
	else
	 ;dd DROP
	 exit		; yes , EOF 
	then
	;dd DROP
	1
	repeat
;
: zeilemit, zeilemit, 0
  	cr 10 0 do '-'emit loop cr '>'emit text_buffer printcstring '<' emit 
;
: teilemit, teilemit, 0
  	cr 10 0 do '_'emit loop cr '>'emit ptr_buff printcstring '<' emit 
; 			
; function: ZEIL
; ( -- )
;| reads stream of char to text_buffer
;| until 'CR' is hit 
: ZEIL, ZEIL, 0
       	text_buffer dup  TEXT_BUFF ! zeile 
        inter
        text_buffer dup PPTR_LAST ! PPTR !
        ;drop ;  clsstack drop
;

: depth, depth, 0
S0 @ dsp@ - 4-
;
: ?stack, qstack, 0
		depth 0>
		if 
			drop
		else
		    stackerr printcstring
			S0  @ dsp!
		then
;

: compile, compile, 0
 cr cr  
 GRUB  @   0x14 +  @ 
 GRUB  @   0x18 +  @ 
 dup @ swap 4+ @   swap
 2dup -  -rot SRC_END ! 0  SRC_END c! ; Store 0 (EOF ) TO  SRC_END
 swap dup SRC ! swap 2drop drop ;interforth    
 text_buffer TEXT_BUFF ! 1 TEXT_BUFF @ c! ; init
 S0 @ dsp! 
 text_buffer dup  PPTR_LAST ! PPTR !
;


extern module
; function: main
;   The first forth word executed by the kernel.
: main_test, main_test, 0
    clear module @ GRUB !
    0x101006 print_idtentry
    0x10100E print_idtentry
    0x101016 print_idtentry
    ;[`] print_scancode 33 register_isr_handler
    ;[`] print_tic      32 register_isr_handler
    compile
	quit
 	stop
;
global last_word
last_word:
: tst, tst,0
 cr 10 0 do '-'emit loop cr
; 
section .rodata
hello:      db "hello, world", 0
fault:      db "A fault happened", 0
tic_msg:    db "The clock tics", 0
ok: 			db '  OK ... ' ,0
key_press: 		db '   PRESS ANY KEY  .... ' , 0
outputmes: 		db 'Words of forth' , 0
inputloop:		db 'Enter  words' , 0
errmsg: 		db 'PARSE ERROR: AT ->' ,0
gef: 			db 'GEFUNDEN' , 0
ngef: 			db 'NICHT IN TABELLE' , 0
stackmes:		db 'STACK> ', 0

stackerr:		db ' STACK undeflow .. reset STACK !' ,0

interpret_is_lit: db 0     
