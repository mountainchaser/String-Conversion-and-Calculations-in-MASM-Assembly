TITLE String Primitives and Macros    (Proj6_LANDAL.asm)

; Author: Allison Land
; Last Modified: 12/5/21
; OSU email address: LANDAL@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 12/5/21
; Description: This program uses macros to take decimal number inputs as strings, and then converts them to 
;		decimal numbers and stores them in an array. It then performs calculations and converts the stored 
;		numbers back into strings, and displays those strings. 
;-----------------------------------------------------------------------------------------------------

INCLUDE Irvine32.inc

; MACRO DEFINITIONS _____________________________________________________________________________
; ---------------------------------------------------------------------------------------------------
; Name:			mGetString
; 
; Desc:			Displays a string. 
;
; Preconditions: Called in ReadVal - arguments are pushed onto the stack. 
;
; Postconditions: EDX, ECX, EAX changed
;
; Receives: {parameters: StringAddress (reference), bytesReadAddress (reference), 
;			promptAddress (reference)}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------

	mGetString		MACRO	stringAddress, bytesReadAddress, promptAddress

		;prompt user for string input, store using ReadString
		MOV		EDX, promptAddress
		CALL	WriteString

		MOV		EDX, stringAddress
		MOV		ECX, MAXBYTES
		CALL	ReadString
		MOV		[bytesReadAddress], EAX

	ENDM


	
; ---------------------------------------------------------------------------------------------------
; Name:			mDisplayString
; 
; Desc:			Displays string by reversing it (due to reverse storage)	
;
; Preconditions: Called in WriteVal - arguments are pushed onto the stack. 
;
; Postconditions: EDX, ECX, ESI, EDI, ESI, EAX changed
;
; Receives: {parameters: StringAddress (reference), revStringAddress (reference), 
;			stringLength (value)}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------


	mDisplayString	MACRO	stringAddress, revStringAddress, stringLength
		;print stored string using WriteString

		CMP		stringLength, 1
		JNE		_setup
		MOV    EDX, stringAddress
		CALL   WriteString
		JMP	   _end

		_setup:
		CMP	   stringLength, 0
		JE	   _end
		MOV    ECX, stringLength
		MOV    ESI, stringAddress
		MOV    EDI, revStringAddress
		ADD    ESI, ECX
		DEC    ESI
  
		;REVERSE STRING
		_reverseLoop:
		STD
		LODSB
		CLD
		STOSB
		LOOP   _reverseLoop

		;PRINT STRING
		MOV    EDX, revStringAddress
		CALL   WriteString
		_end:
	
	ENDM

;-----------------------------------------------------------------------------------------------------
	
	;CONSTANTS
	MAXVAL = 2147483647  ;negated  and incremented in code
	MAXASCII = 57
	MAXBYTES = 12
	ARRAYELEMENTS = 10

.data

	progTitle	BYTE	"			String Primitives and Macros in MASM Assembly",13,10,0
	programmer	BYTE	"					by Allison Land",13,10,13,10,0
	instruction	BYTE	"This program requires that you enter ten integers between -2,147,483,648 and 2,147,483,647.",13,10,13,10,0
	prompt		BYTE	"Please enter a number: ",0
	error		BYTE	"ERROR: This is either not a valid number, or your integer has too many digits.",0
	ec1			BYTE	"**EC: Numbers each line of user input with running total of user's valid numbers using WriteVal.",13,10,13,10,0
	displayList BYTE	"You entered the following numbers: ",13,10,0
	sumString	BYTE	"Their sum is: ",0
	avgString	BYTE	"Their rounded average is: ",0
	farewell	BYTE	"That was fun - cya later!",0
	ASCIIstring	BYTE	MAXBYTES DUP(?)
	emptyString	BYTE	MAXBYTES DUP(?)
	revString	BYTE	MAXBYTES DUP(?)
	maxValue	BYTE	"2147483648",0			
	space		BYTE	" ",0
	comma		BYTE	", ",0
	numArray	SDWORD	ARRAYELEMENTS DUP(?)
	arrayCount	SDWORD	1	
	sum			SDWORD	?
	average		SDWORD	?
	storedDec	SDWORD	?
	bytesRead	SDWORD	?
	numCount	SDWORD	0
	boolie		SDWORD	0				; 0 = TRUE 1 = FALSE

;-----------------------------------------------------------------------------------------------------
.code
main PROC

	;INTRODUCTION
	MOV		EDX, OFFSET progTitle
	CALL	WriteString
	MOV		EDX, OFFSET programmer
	CALL	Writestring
	MOV		EDX, OFFSET ec1
	CALL	WriteString
	MOV		EDX, OFFSET instruction
	CALL	WriteString

	;GET 10 VALID INTEGERS FROM USER
	;set up:
	MOV		EDI, OFFSET storedDec
	MOV		ESI, OFFSET numArray
	MOV		ECX, ARRAYELEMENTS
	MOV		EBX, 0

	_loop:
	;EXTRA CREDIT 1
	CDQ
	MOV		EAX, 10
	IDIV	arrayCount
	INC		EDX
	MOV		bytesRead, EDX
	PUSH	OFFSET revString
	PUSH	OFFSET bytesRead
	PUSH	OFFSET revString
	PUSH	arrayCount
	PUSH	OFFSET ASCIIstring
	CALL	WriteVal
	MOV		EDX, OFFSET space
	CALL	WriteString

	PUSH	OFFSET boolie
	PUSH	OFFSET maxValue
	PUSH	OFFSET error
	PUSH	OFFSET storedDec
	PUSH	OFFSET ASCIIstring
	PUSH	OFFSET bytesRead
	PUSH	OFFSET prompt
	CALL	ReadVal
	CALL	CrLF
	CMP		boolie, 1
	JNE		_loop
	INC		arrayCount
	MOV		boolie, 0

	;STORE IN ARRAY (using register indirect addressing)
	MOV		ESI, storedDec	; num going into array
	MOV		EDI, OFFSET	numArray
	ADD		EDI, EBX
	MOV		[EDI], ESI				; move value into array
	ADD		EBX, TYPE numArray				
	
	; clear ASCIIstring
	PUSH	ECX
	CLD
	MOV    ECX, MAXBYTES
	MOV    ESI, OFFSET emptyString
	MOV    EDI, OFFSET ASCIIstring
	REP    MOVSB
	POP		ECX
	DEC		ECX
	CMP		ECX, 0
	JNE	   _loop

	;loop through array, display ints
	MOV		EDX, OFFSET displayList
	CALL	WriteString

	CLD
	MOV		ECX, arrayCount
	MOV		EBX, 0

	_displayInts:
	MOV		ESI, OFFSET numarray
	PUSH	OFFSET	revString
	PUSH	OFFSET bytesRead
	PUSH	OFFSET revString
	PUSH	[ESI + EBX]
	PUSH	OFFSET ASCIIstring
	CALL	WriteVal
	ADD		EBX, TYPE numArray
	CMP		ECX, 2
	JE		_break
	MOV		EDX, OFFSET comma
	CALL	WriteString

	; clear ASCIIstring
	_clear:
	PUSH	ECX
	CLD
	MOV    ECX, MAXBYTES
	MOV    ESI, OFFSET emptyString
	MOV    EDI, OFFSET ASCIIstring
	REP    MOVSB
	POP		ECX

	; clear revString
	_clear2:
	PUSH	ECX
	CLD
	MOV    ECX, MAXBYTES
	MOV    ESI, OFFSET emptyString
	MOV    EDI, OFFSET revString
	REP    MOVSB
	POP	   ECX	

	LOOP	_displayInts
	_break:

	; clear ASCIIstring
	PUSH	ECX
	CLD
	MOV    ECX, MAXBYTES
	MOV    ESI, OFFSET emptyString
	MOV    EDI, OFFSET ASCIIstring
	REP    MOVSB
	POP		ECX

	; clear revString
	PUSH	ECX
	CLD
	MOV    ECX, MAXBYTES
	MOV    ESI, OFFSET emptyString
	MOV    EDI, OFFSET revString
	REP    MOVSB
	POP	   ECX	


	;CALCULATE AND DISPLAY SUM
	CALL	CrLf
	CALL	CrLF
	MOV		EDX, OFFSET sumString
	CALL	WriteString

	MOV		ECX, arrayCount
	SUB		ECX, 2
	MOV		ESI, OFFSET numArray
	MOV		EDI, OFFSET sum
	MOV		EBX, 0
	MOV		EAX, [ESI+EBX]
	_summation:
	ADD		EBX, 4
	ADD		EAX, [ESI+EBX]
	LOOP	_summation
	MOV		[EDI], EAX

	PUSH	OFFSET revString
	PUSH	OFFSET bytesRead
	PUSH	OFFSET revString
	PUSH	[EDI]
	PUSH	OFFSET ASCIIstring
	CALL	WriteVal
	CALL	CrLf
	CALL	CrLf

	;CALCULATE AND DISPLAY AVERAGE
	MOV		EDX, OFFSET avgString
	CALL	WriteString
	
	CDQ
	MOV		EAX, sum
	MOV		ECX, arrayCount
	SUB		ECX, 1
	IDIV	ECX
	MOV		average, EAX

	PUSH	OFFSET revString
	PUSH	OFFSET bytesRead
	PUSH	OFFSET revString
	PUSH	EAX
	PUSH	OFFSET ASCIIstring
	CALL	WriteVal
	
	CALL	CrLF
	CALL	CrLF
	MOV		EDX, OFFSET farewell
	CALL	WriteString

	CALL	CrLf
	CALL	CrlF
	
	Invoke ExitProcess,0	; exit to operating system
main ENDP


; PROCEDURE DEFINITIONS _____________________________________________________________________________
; ---------------------------------------------------------------------------------------------------
; Name:			ReadVal
; 
; Desc:			Gets a string from the user and validates that it contains a valid integer that will 
;				fit in a 32 bit register (11 characters total, including sign +/-)		
;
; Preconditions: 	PUSHED TO STACK:	OFFSET boolie, OFFSET maxValue, OFFSET error, OFFSET storedDec
;				OFFSET ASCIIstring, OFFSET bytesRead, OFFSET prompt

;
; Postconditions: Registers preserved on stack and not changed.
;
; Receives: {parameters: OFFSET error, OFFSET storedDec, OFFSET ASCIIstring, OFFSET bytesRead, OFFSET prompt}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------

	readVal PROC	USES ECX EDX ESI EDI

	;invoke mGetString
	PUSH	EBP
	MOV		EBP, ESP
	mGetString		[EBP + 32], [EBP + 28], [EBP + 24]  ;stringAddress, bytesReadAddress, promptAddress

	; string primitive setup
	_setup:
	CLD
	MOV		EDX, [EBP + 28]
	MOV		ECX, EDX				; bytesread
	MOV		ESI, [EBP + 32]			; input array ASCIIstring (from mGetString)
	MOV		EDI, 0					; for conversion algorithim
	MOV		EDX, [EBP + 44]			; track iteration through maxvalue chars

	;CHECK FOR SIGNS
	_stringLoop:
	XOR		EAX, EAX
	LODSB	
	CMP		AL, 45			; -
	JE		_skipSign
	CMP		AL, 43			; +
	JE		_skipSign
	CMP		ECX, [EBP + 28]
	JNE		_numsOnly

	;VALIDATE LENGTH FIRST ITERATION
	CMP		ECX, LENGTHOF MAXVALUE-1	; if more chars than maxvalue (no sign)
	JG		_error
	CMP		ECX, LENGTHOF MAXVALUE-1
	JE		_validateLoop1
	JMP		_numsOnly

	_signLength:
	CMP		ECX, LENGTHOF MAXVALUE
	JG		_error	
	CMP		ECX, LENGTHOF MAXVALUE 
	JL		_numsOnly

	_validateLoop2:					; if same length as max value, check if value is greater than maxvalue
	XOR		EAX, EAX
	LODSB
	_validateLoop1:
	MOV		EDX, [EBP+28]			;bytes read/ counter
	SUB		EDX, ECX
	CMP		AL, maxValue[EDX]		
	JG		_error
	LOOP	_validateLoop2

	_numsOnly:
	CLD								; check for invalid chars
	MOV		EDX, [EBP + 28]
	MOV		ECX, EDX
	MOV		ESI, [EBP + 32]			; input array ASCIIstring (from mGetString)
	XOR		EAX, EAX
	LODSB	
	CMP		AL, 45			; -
	JE		_loopNumsOnly
	CMP		AL, 43			; +	
	JE		_loopNumsOnly
	_checknum:
	CMP		AL, 48
	JL		_error
	CMP		AL, 57
	JG		_error
	_loopNumsOnly:
	XOR		EAX, EAX
	LODSB
	LOOP	_checkNum
	
	_convert2:                      ;setup conversion
	CLD
	MOV		EDX, [EBP + 28]
	MOV		ECX, EDX				; bytesread
	MOV		ESI, [EBP + 32]			; input array ASCIIstring (from mGetString)
	MOV		EDI, 0					; for conversion algorithim

	_convert:
	XOR		EAX, EAX
	LODSB
	CMP		AL, 45
	JE		_convertLoop
	CMP		AL, 43
	JE		_convertLoop
	SUB		EAX, 48							;convert ASCII to SDWORD
	IMUL	EDI, 10
	ADD		EAX, EDI
	MOV		EDI, EAX
	CMP		EDI, 0
	JGE		_convertLoop
	NEG		EDI

	_convertLoop:
	LOOP	_convert
	MOV		EAX, [EBP+36]
	MOV		[EAX], EDI
	JMP		_validateNeg

	_skipSign:
	JMP		_signLength ; jump

	_error:
	MOV		EDX, [EBP + 40]    
	CALL	WriteString
	JMP		_errorExit

	; NEGATE IF NEGATIVE
	_validateNeg:
	MOV		ECX, 1	
	MOV		ESI, [EBP + 32]     ;input array ASCIIstring (from mGetString)
	LODSB
	CMP		AL, 45				;check if first char is -
	JNE		_exit
	NEG		EDI
	MOV		EAX, [EBP+36]
	CMP		EDI, -MAXVAL-1
	JL		_error
	MOV		[EAX], EDI

	_exit:
	MOV		ECX, 1	
	MOV		ESI, [EBP + 32]     ;input array ASCIIstring (from mGetString)
	LODSB
	CMP		AL, 45
	JNE		_exit2
	CMP		EDI, MAXVAL
	JG		_error

	_exit2:
	XOR		ECX, ECX
	MOV		EDX, [EBP + 48]
	MOV		ECX, 1
	MOV		[EDX], ECX			; boolie return TRUE (1) 
	MOV		[EBP + 36], EDI		; output storedDec
	MOV		[EBP + 42], EDX
	_errorExit:
	POP EBP
	RET 
	ReadVal ENDP

; ---------------------------------------------------------------------------------------------------
; Name:			WriteVal
; 
; Desc:			Converts a SDWORD to ASCII and displays the number value as a string. 	
;
; Preconditions: 	PUSHED TO STACK:	OFFSET revString, OFFSET bytesRead, OFFSET revString
;				value to display, OFFSET ASCIIstring
;
; Postconditions: Registers preserved on stack and not changed. 
;				
;
; Receives: {parameters:  revString (reference), bytesRead (reference), revString (reference)
;				value to display (value), ASCIIstring (reference)}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------
	
WriteVal PROC USES EAX EDX EBX ECX

	PUSH	EBP
	MOV		EBP, ESP

	MOV		EBX, 0					;initialize digit count

	XOR		EDX, EDX
	MOV		EAX, [EBP + 28]			;move value into EAX
	_loop:
	CDQ
	MOV		ECX, 10
	IDIV	ECX
	MOV		ECX, 0
	MOV		EDI, EDX
	ADD		EDI, 48					;else add 48 to remainder to convert to ASCII
	MOV		ECX, [EBP + 24]			;move into ASCIIstring
	ADD		ECX, EBX
	MOV		[ECX], EDI				
	INC		EBX
	CMP		EAX, 0					;if quotient is 0, then last digit
	JLE		_isNegative
	JMP		_loop

	_isNegative:
	MOV		EDX, [EBP + 28]
	CMP		EDX, 0
	JGE		_displaystring
 	INC		EBX
	MOV		EDI, [EBP + 24 + EBX]
	MOV		EDX, 45
	MOV		[EDI], EDX				;move into ASCIIstring

	_displayString:
	MOV		EDI, [EBP + 36]
	MOV		[EDI], EBX
	XOR		ECX, ECX
	MOV		ECX, EBX
	mDisplayString	[EBP + 24], [EBP + 40], ECX  ;stringAddress, revStringAddress, stringLength 

	POP EBP

	RET	
	WriteVal ENDP

END main

