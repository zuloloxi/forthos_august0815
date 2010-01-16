; program: kernel_test
; Debug words helper for the kernel.

; License: GPL
; José Dinuncio <jdinunci@uc.edu.ve>, 12/2009.

%include "forth.h"
%include "kernel_words.h"
%include "kernel_video.h"

%include "irq.h"

[BITS 32]

; function: space
; same as spc
: space, space, 0
	32 emit
;

; function: negate 
: negate, negate, 0
	0 swap sub
;

; function: true   
: true, true, 0
	1
;

; function: false 
: false, false, 0 
	0
;

; function: nip
;
: nip, nip, 0
    swap drop
;

; function: tuck	
;
: tuck, tuck, 0
	swap over
;
	
; function:pick
: pick, pick, 0
	1+ 4 * dsp@ add @
;
; function:  spaces( n -- ) 
: spaces, spaces, 0
	begin dup 0> while space 1-	repeat drop
;

; function:  decimal ( -- ) 
: decimal, decimal, 0
	10 BASE !
;

; function: :hex
: hex, hex, 0
	0x10 BASE !
;	

; function: U. 
;	Displays an unsigned number
: U., Udot, 0
    BASE @ /mod
    dup 0<> if  U.  else  drop  then
    '0' + emit 
;	 
; function: .S  
;	FORTH word .S prints the contents of the stack.  It doesn't alter the stack.
: .S, dotS, 0
	'>' emit dsp@ 
	begin dup S0 @ < while dup @ intprint space 4+ repeat
	drop '<' emit
;

; function: uwidth 
: uwidth, uwidth, 0
    BASE @ / ?dup if uwidth 1+ else 1 then
;

; function: u.r  
: u.r, udotr, 0
	swap dup uwidth rot swap - spaces
;

; function: .R 
: .R, dotR, 0
	swap dup 0< 
	if negate 1 swap rot 
	else 0 swap rot
	then
	swap drop znequ if '-' emit else then
	U.
;

; function: .  
: ., DOT, 0
	0 .R space
;

; function: ?
: ?, qq, 0
	@ .
;

