: bl   32 ;
: '\n' 10 ;
: true  1 ;
: false 0 ;
: not   0= ;
: negate 0 swap - ;
: space bl emit ;
: literal immediate
	' lit ,
	,
	;
	
: ':'
	[
	char :
	]
	literal	
;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'a' [ char a ] literal ;
: '0' [ char 0 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;
: [compile] immediate
	wort
	find
	>cfa
	,
;
: recurse immediate
	LATEST @
	>cfa
	,
;
: if immediate
	' 0branch ,
	HERE @
	0 ,
;

: then immediate
	dup
	HERE @ swap -
	swap !
;

: else immediate
	' branch ,
	HERE @
	0 ,
	swap
	dup
	HERE @ swap -
	swap !
;

: begin immediate
	HERE @
;

: until immediate
	' 0branch ,
	HERE @ -
	,
;


: again immediate
	' branch ,
	HERE @ -
	,
;

: while immediate
	' 0branch ,
	HERE @
	0 ,
;

: repeat immediate
	' branch ,
	swap
	HERE @ - ,
	dup
	HERE @ swap -
	swap !
;

: unless immediate
	' not ,
	[compile] if
;
: ( immediate
	1	begin
		key1
		dup '(' = if
			drop
			1+
		else
			')' = if
				1-
			then
		then
	dup 0= until
	drop
;
( from now on we can use ( ... ) for comments.) ;

( some more complicated stack examples, showing the stack notation. ) ;
: nip ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	dsp@ +		( add to the stack pointer )
	@    		( and fetch )
;
( standard words for manipulating base. )
: decimal ( -- ) 10 BASE ! ;
: hex ( -- ) 16 BASE ! ;


( with the looping constructs, we can now write spaces, which writes n spaces to stdout. )
: spaces	( n -- )
	begin
		dup 0>		( while n > 0 )
	while
		space		( print a space )
		1-		( until we count down to 0 )
	repeat
	drop
;

: case immediate
	0
;

: of immediate
	' over ,
	' = ,
	[compile] if
	' drop ,
;

: endof immediate
	[compile] else
;

: endcase immediate
	' drop ,
	begin
		?dup
	while
		[compile] then
	repeat
;

( this is the underlying recursive definition of u. )
: u.		( u -- )
	BASE @ /mod	( width rem quot )
	?dup if			( if quotient <> 0 then )
		recurse		( print the quotient )
	then

	( print the remainder )
	dup 10 < if
		'0'		( decimal digits 0..9 )
	else
		10 -		( hex and beyond digits a..z )
		'a'
	then
	+
	emit
;

(
	forth word .s prints the contents of the stack.  it doesn't alter the stack.
	very useful for debugging.
)
: .s		( -- )
	dsp@		( get current stack pointer )
	begin
		dup S0 @ <
	while
		dup @ u.	( print the stack element )
		space
		4+		( move up )
	repeat
	drop
;

( this word returns the width (in characters) of an unsigned number in the current base )
: uwidth	( u -- width )
	BASE @ /	( rem quot )
	?dup if		( if quotient <> 0 then )
		recurse 1+	( return 1+recursive call )
	else
		1		( return 1 )
	then
;

: u.r		( u width -- )
	swap		( width u )
	dup		( width u u )
	uwidth		( width u uwidth )
	rot		( u uwidth width )
	swap -		( u width-uwidth )
	( at this point if the requested width is narrower, we'll have a negative number on the stack.
	  otherwise the number on the stack is the number of spaces to print.  but spaces won't print
	  a negative number of spaces anyway, so it's now safe to call spaces ... )
	spaces
	( ... and then call the underlying implementation of u. )
	u.
;

(
	.r prints a signed number, padded to a certain width.  we can't just print the sign
	and call u.r because we want the sign to be next to the number ('-123' instead of '-  123').
)
: .r		( n width -- )
	swap		( width n )
	dup 0< if
		negate		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		swap		( width 1 u )
		rot		( 1 u width )
		1-		( 1 u width-1 )
	else
		0		( width u 0 )
		swap		( width 0 u )
		rot		( 0 u width )
	then
	swap		( flag width u )
	dup		( flag width u u )
	uwidth		( flag width u uwidth )
	rot		( flag u uwidth width )
	swap -		( flag u width-uwidth )

	spaces		( flag u )
	swap		( u flag )

	if			( was it negative? print the - character )
		'-' emit
	then

	u.
;	

( finally we can define word . in terms of .r, with a trailing space. )
: . 0 .r space ;

( the real u., note the trailing space. )
: u. u. space ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

: ascii  127 and dup 32 < if  drop 46 then emit ;
( some more complicated stack examples, showing the stack notation. )
: nip ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	dsp@ +		( add to the stack pointer )
	@    		( and fetch )
;



( c a b within returns true if a <= c and c < b )
(  or define without ifs: over - >r - r>  u<  )
: within
	rot		( b c a )
	over		( b c a c )
	<= if
		> if		( b c -- )
			true
		else
			false
		then
	else
		2drop		( b c -- )
		false
	then
;

: aligned	( addr -- addr )
	3 + 3 invert and	( (addr+3) & ~3 )
;


: align HERE @ aligned HERE ! ;
: c,
	HERE @ c!
	1 HERE +!
;

: s" immediate		( -- addr len )
	STATE @ if	( compiling? )
		' litstring ,	( compile litstring )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		begin
			key1 		( get next character of the string )
			dup '"' <>
		while
			c,		( copy character )
		repeat
		drop		( drop the double quote character at the end )
		dup		( get the saved address of the length word )
		HERE @ swap -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		swap !		( and back-fill the length location )
		align		( round up to next multiple of 4 bytes for the remaining code )
	else		( immediate mode )
		HERE @		( get the start address of the temporary space )
		begin
			key1
			dup '"' <>
		while
			over c!		( save next character )
			1+		( increment address )
		repeat
		drop		( drop the final " character )
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
		swap 		( addr len )
	then
;
: ." immediate		( -- )
	STATE @ if	( compiling? )
		[compile] s"	( read the string, and compile litstring, etc. )
		' tell ,	( compile the final tell )
	else
		( in immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		begin
			key1
			dup '"' = if
				drop	( drop the double quote character )
				exit	( return from this function )
			then
			emit
		again
	then
;

: constant
	wort
	header	
	DOCOL ,	
	' lit ,	
	,
	' exit ,
;
: allot		( n -- addr )
	HERE @ swap	
	HERE +!	
;
: cells ( n -- n ) 4 * ;
: variable
	1 cells allot
	wort header	
	DOCOL ,	
	' lit ,	
	,
	' exit ,
;
: value		( n -- )
	wort header	
	DOCOL ,
	' lit ,
	,
	' exit ,
;

: to immediate	( n -- )
	wort
	find
	>dfa
	4+
	STATE @ if
		' lit ,	
		,
		' ! ,
	else
		!
	then
;

( x +to val adds x to val )
: +to immediate
	wort
	find
	>dfa
	4+
	STATE @ if	
		' lit ,	
		,
		' +! ,	
	else
		+!	
	then
;
: id.
	4+		( skip over the link pointer )
	dup c@		( get the flags/length byte )
	F_LENMASK and	( mask out the flags - just want the length )

	begin
		dup 0>		( length > 0? )
	while
		swap 1+		( addr len -- len addr+1 )
		dup c@		( len addr -- len addr char | get the next character)
		emit		( len addr char -- len addr | and print it)
		swap 1-		( len addr -- addr len-1    | subtract one from length )
	repeat
	2drop		( len addr -- )
;

(
	'word word find ?hidden' returns true if 'word' is flagged as hidden.

	'word word find ?immediate' returns true if 'word' is flagged as immediate.
)
: ?hidden
	4+		( skip over the link pointer )
	c@		( get the flags/length byte )
	F_HIDDEN and	( mask the f_hidden flag and return it (as a truth value) )
;
: ?immediate
	4+		( skip over the link pointer )
	c@		( get the flags/length byte )
	F_IMMED and	( mask the f_immed flag and return it (as a truth value) )
;

(
	words prints all the words defined in the dictionary, starting with the word defined most recently.
	however it doesn't print hidden words.

	the implementation simply iterates backwards from latest using the link pointers.
)
: words
	LATEST @	( start at latest dictionary entry )
	begin
		?dup		( while link pointer is not null )
	while
		dup ?hidden not if	( ignore hidden words )
			dup id.		( but if not hidden, print the word )
			space
		then
		@		( dereference the link pointer - go to previous word )
	repeat
	cr
;


: forget
	wort find	( find the word, gets the dictionary entry address )
	dup @ LATEST !	( set latest to point to the previous word )
	HERE !		( and store here with the dictionary address )
;

: cfa>
	LATEST @	( start at latest dictionary entry )
	begin
		?dup		( while link pointer is not null )
	while
		2dup swap	( cfa curr curr cfa )
		< if		( current dictionary entry < cfa? )
			nip		( leave curr dictionary entry on the stack )
			exit
		then
		@		( follow link pointer back )
	repeat
	drop		( restore stack )
	0		( sorry, nothing found )
;

: see
	cr
	wort
	find
	HERE @	
	LATEST @	
	begin
		2 pick	
		over
		<>	
	while
		nip	
		dup @
	repeat

	drop
	swap	
	':' emit space dup id. space
	dup ?immediate if ." immediate " then

	>dfa		
	begin	
		2dup >
	while
		dup @

		case
		' lit of
			4 + dup @	
			.
		endof
		' litstring of	
			[ char s ] literal emit '"' emit space
			4 + dup @	
			swap 4 + swap	
			2dup tell	
			'"' emit space	
			+ aligned	
			4 -	
		endof
		' 0branch of
			." 0branch >"
			4 + dup @		
			.
			." < "
		endof
		' branch of		
			." branch >"
			4 + dup @
			.
			." < "
		endof
		' ' of		
			[ char ' ] literal emit space
			4 + dup @		
			cfa>	
			id. space
		endof
		' exit of	
				2dup	
			4 +	
			<> if	
				." exit "
			then
		endof
					
			dup		
			cfa>	
			id. space		
		endcase

		4 +		
	repeat

	59 emit cr

	2drop		( restore stack )
;

: noname
	0 0 header	
	HERE @	
	DOCOL ,	
	]
;


: ['] immediate
	' lit ,		
;
: exception-marker
	rdrop
	0
;

: catch	
	dsp@ 4+ >r		
	' exception-marker 4+
	>r
	execute
;

: throw		( n -- )
	?dup if			
		rsp@ 			( get return stack pointer )
		begin
			dup R0 4- <		
		while
			dup @			( get the return stack entry )
			' exception-marker 4+ = if	( found the exception-marker on the return stack )
				4+			( skip the exception-marker on the return stack )
				rsp!			( restore the return stack pointer )

				( restore the parameter stack. )
				dup dup dup		( reserve some working space so the stack for this word
							  doesn't coincide with the part of the stack being restored )
				r>			
				4-			( reserve space on the stack to store n )
				swap over		( dsp n dsp )
				!			( write n on the stack )
				dsp! exit		( restore the parameter stack pointer, immediately exit )
			then
			4+
		repeat

		
		drop

		case
		0 1- of	( abort )
			." aborted" cr
		endof
			( default case )
			." uncaught throw "
			dup . cr
		endcase
		quit
	then
;

: abort	
	0 1- throw
;

: dump
	BASE @ rot		( save the current base at the bottom of the stack )
	hex			( and switch to hexadecimal mode )

	begin
		?dup		( while len > 0 )
	while
		over 8 .r	( print the address )
		space

		2dup	
		1- 15 and 1+
		begin
			?dup	
		while
			swap
			dup c@	
			2 .r space	( print the byte )
			1+ swap 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		repeat
		drop		( addr len )

		( print the ascii equivalents )
		2dup 1- 15 and 1+ ( addr len addr linelen )
		begin
			?dup		( while linelen > 0)
		while
			swap		( addr len linelen addr )
			dup c@		( addr len linelen addr byte )
			dup 32 128 within if	( 32 <= c < 128? )
				emit
			else
				drop '.' emit
			then
			1+ swap 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		repeat
		drop		( addr len )
		cr

		dup 1- 15 and 1+ ( addr len linelen )
		tuck		( addr linelen len linelen )
		-		( addr linelen len-linelen )
		>r + r>		( addr+linelen len-linelen )
	repeat

	drop			( restore stack )
	BASE !			( restore saved base )
;

: strlen 	( str -- len )
	dup		( save start address )
	begin
		dup c@ 0<>	( zero byte found? )
	while
		1+
	repeat

	swap -		( calculate the length )
;

: cstring	( addr len -- c-addr )
	swap over	( len saddr len )
	HERE @ swap	( len saddr daddr len )
	cmove		( len )

	HERE @ +	( daddr+len )
	0 swap c!	( store terminating nul char )

	HERE @ 		( push start address )
;


: unused	( -- n )
	TOPMEM @		( get end of data segment according to the kernel )
	HERE @		( get current position in data segment )
	-
	4 /		( returns number of cells )
;


: wel
	clear cr ." my-forth version 0." 1 . 
	."  adapted from jonesforth version 47"  cr
	." corrections and additions by richard russell, 19-oct-2009 " cr
	." adapted by august0815, 01-feb-2010" cr
	unused . ." cells remaining" cr cr
	." Type 'words' ... " cr cr
	
;

immediate ; 
echoon ;
wel ;
