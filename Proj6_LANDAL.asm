TITLE String Primitives and Macros    (Proj6_LANDAL.asm)

; Author: Allison Land
; Last Modified: 11/29/21
; OSU email address: LANDAL@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 12/4/21
; Description: This program uses macros to take decimal number inputs as strings, and then converts them to 
;		decimal numbers....
;-----------------------------------------------------------------------------------------------------

INCLUDE Irvine32.inc

; MACRO DEFINITIONS _____________________________________________________________________________
; ---------------------------------------------------------------------------------------------------
; Name:			mGetString
; 
; Desc:			Gets a string from the user and validates that it contains a valid integer that will 
;				fit in a 32 bit register (11 characters total, including sign +/-)
;
; Preconditions: 
;
; Postconditions: 
;
; Receives: {parameters: progTitle (reference), programmer (reference), ec1 (reference), ec2 (reference),
;			progDesc(reference)}
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
; Desc:			
;
; Preconditions: 
;
; Postconditions: 
;
; Receives: {parameters: progTitle (reference), programmer (reference), ec1 (reference), ec2 (reference),
;			progDesc(reference)}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------

	mDisplayString	MACRO	stringAddress
		;print stored string using WriteString
		MOV		EDX, stringAddress
		CALL	WriteString
	ENDM

;-----------------------------------------------------------------------------------------------------
	
	;CONSTANTS
	MINVALUE = -2147483648    ;negated in code
	MAXVALUE = 2147483647
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
	ASCIIstring	BYTE	?

	numArray	SDWORD	ARRAYELEMENTS DUP(?)
	arrayCount	SDWORD	1				
	space		BYTE	" ",0
	comma		BYTE	", ",0
	decArr		SDWORD	10 DUP(?)
	storedDec	SDWORD	?
	bytesRead	SDWORD	?
	numCount	SDWORD	0


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
	;PUSH	OFFSET arrayCount
	;PUSH	OFFSET ASCIIstring
	;CALL	WriteVal
	;MOV	EDX, OFFSET space
	;CALL	WriteString

	PUSH	OFFSET error
	PUSH	OFFSET storedDec
	PUSH	OFFSET ASCIIstring
	PUSH	OFFSET bytesRead
	PUSH	OFFSET prompt
	CALL	ReadVal
	INC		arrayCount
	CALL	CrLF

	;STORE IN ARRAY (using register indirect addressing)

	MOV		ESI, OFFSET storedDec	; num going into array
	MOV		EDX, [ESI]
	MOV		numArray[EBX], EDX				; move value into array
	ADD		EBX, TYPE numArray					
	LOOP	_loop					; LOOP x's ARRAYSIZE


	;loop through array, display ints
;	MOV		ESI

	;calculate sum and display

	;calvulate average and display

	Invoke ExitProcess,0	; exit to operating system
main ENDP


; PROCEDURE DEFINITIONS _____________________________________________________________________________
; ---------------------------------------------------------------------------------------------------
; Name:			ReadVal
; 
; Desc:			
;
; Preconditions: 
;
; Postconditions: 
;
; Receives: {parameters: OFFSET error, OFFSET storedDec, OFFSET ASCIIstring, OFFSET bytesRead, OFFSET prompt}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------

	readVal PROC	USES ECX EBX ESI EDI
	;invoke mGetString
	PUSH	EBP
	MOV		EBP, ESP
	mGetString		[EBP + 32], [EBP + 28], [EBP + 24]  ;stringAddress, bytesReadAddress, promptAddress

	; string primitive setup
	MOV		ECX, [EBP + 28]		; bytesread
	MOV		ESI, [EBP + 32]     ;input array ASCIIstring (from mGetString)
	MOV		EDI, [EBP + 36]		;output storedDec
	MOV		EBX, 0				;for conversion algorithim

	;validate that characters are valid
	_stringLoop:
	XOR		EAX, EAX
	LODSB	
	CMP		AL, 45			; -
	JE		_skip

	_cont:
	CMP		AL, 48
	JL		_error
	CMP		AL, 57
	JG		_error
	JMP		_convert

	_sign:
	CMP		ECX, 1
	MOV		EAX, [EBP + 36]
	NEG		EAX
	MOV		[EBP + 36], EAX
	JMP		_validate
	JNE		_error

	; convert ASCII to SDWORD using LODSB/STOSB (convert each digit)
	_convert:	
	SUB		EAX, 48
	IMUL	EBX, 10
	ADD		EAX, EBX
	MOV		EBX, EAX
	LOOP	_stringLoop
	MOV		[EBP + 36], EBX
	JMP		_validateNeg

	_skip:
	DEC		ECX
	JMP		_stringLoop
	_error:
	MOV		EDX, [EBP + 40]    
	CALL	WriteString
	JMP		_exit

	;determine if negative
	_validateNeg:
	;negate if negative
	MOV		ESI, [EBP + 32]
	MOV		ECX, 1
	LODSB
	CMP		AL, 45
	JNE		_validate
	MOV		EDI, [EBP + 36]
	CMP		EDI, MINVALUE
	JG		_error
	MOV		EAX, EDI
	JMP		_exit
	
	;verify fits in 32bit register
	_validate:
	MOV		EDX, MAXVALUE
	CMP		EDX, EBX
	JG		_error
	MOV		[EBP + 36], EBX

	_exit:	
	POP EBP
	RET 
	ReadVal ENDP

; ---------------------------------------------------------------------------------------------------
; Name:			WriteVal
; 
; Desc:			
;
; Preconditions: 
;
; Postconditions: 
;
; Receives: {parameters: string (reference)}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------
	
	WriteVal PROC USES EAX EDX EBX ECX

	PUSH	EBP
	MOV		EBP, ESP

	MOV		EBX, 0			;initialize digit count

	;convert SDWORD to ASCII
	_loop:
	CDQ
	MOV		EAX, [EBP + 28]			;move storedDec SDWORD into EDX
	INC		EBX	
	MOV		ECX, 10
	IDIV	ECX
	MOV		ECX, 0
	CMP		EAX, ECX				;if quotient is 0, then last digit
	JLE		_lastDigit
	ADD		EDX, 48					;else add 48 to remainder to convert to ASCII
	MOV		[EBP + 24 + EBX], EDX	;move into ASCIIstring
	JMP		_loop

	_lastDigit:
	CMP		EDX, 0
	JL		_negative
	JMP		_displayString

	_negative:
	NEG		EDX
	ADD		EDX, 48
	MOV		[EBP + 8 + EBX], EDX
	INC		EBX
	MOV		[EBP + 8 + EBX], 45

	;invoke mDisplayString
	_displayString:
	mDisplayString	[EBP+24]

	POP EBP
	RET	20
	WriteVal ENDP

END main
