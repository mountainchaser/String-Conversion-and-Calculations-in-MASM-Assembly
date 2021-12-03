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
	MINVALUE = 2147483648  ;negated in code
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
	ASCIIstring	BYTE	MAXBYTES DUP(?)
	maxValue	BYTE	"2147483647",0
	numArray	SDWORD	ARRAYELEMENTS DUP(?)
	arrayCount	SDWORD	1				
	space		BYTE	" ",0
	comma		BYTE	", ",0
	storedDec	SDWORD	?
	bytesRead	SDWORD	?
	numCount	SDWORD	0
	boolie		SDWORD	0				; 0 = TRUE 1 = FALSE


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
	PUSH	OFFSET arrayCount
	PUSH	OFFSET ASCIIstring
	CALL	WriteVal
	MOV		EDX, OFFSET space
	CALL	WriteString

	PUSH	OFFSET	maxValue
	PUSH	OFFSET error
	PUSH	OFFSET storedDec
	PUSH	OFFSET ASCIIstring
	PUSH	OFFSET bytesRead
	PUSH	OFFSET prompt
	CALL	ReadVal
	CALL	CrLF
	MOV		EDX, boolie
	CMP		EDX, 1
	JNE		_loop
	INC		arrayCount
	MOV		EDX, 0
	MOV		boolie, EDX

	;STORE IN ARRAY (using register indirect addressing)

	MOV		ESI, OFFSET storedDec	; num going into array
	MOV		EDX, [ESI]
	MOV		numArray[EBX], EDX				; move value into array
	ADD		EBX, TYPE numArray					
	LOOP	_loop					; LOOP x's ARRAYSIZE

	;loop through array, display ints
	_displayInts:
	MOV		ECX, LENGTHOF numArray
	MOV		ESI, OFFSET numArray
	CALL	WriteVal
	MOV		EDX, OFFSET comma
	CALL	WriteString
	LOOP	_displayInts


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
	_setup:
	MOV		EDX, [EBP + 28]
	MOV		ECX, EDX				; bytesread
	MOV		ESI, [EBP + 32]			; input array ASCIIstring (from mGetString)
	MOV		EDI, 0					; for conversion algorithim
	MOV		EDX, [EBP+44]			; track iteration through maxvalue chars

	;CHECK FOR SIGNS
	_stringLoop:
	XOR		EAX, EAX
	LODSB	
	CMP		AL, 45			; -
	JE		_skipSign
	CMP		AL, 43			; +
	JE		_skipSign
	CMP		ECX, [EBP + 28]
	JNE		_signLength

	;VALIDATE LENGTH FIRST ITERATION
	CMP		ECX, LENGTHOF MAXVALUE	; if more chars than maxvalue (no sign)
	JG		_error
	CMP		ECX, LENGTHOF MAXVALUE
	JE		_validateLoop
	JMP		_convert

	_signLength:
	CMP		ECX, LENGTHOF MAXVALUE
	JG		_error	
	CMP		ECX, LENGTHOF MAXVALUE
	JL		_convert

	_validateLoop:
	MOV		EDX, [EBP+28]			;bytes read/ counter
	SUB		EDX, ECX
	ADD		EDX, [EBP + 44]
	XOR		EBX, EBX
	MOV		EBX, EDX
	CMP		EAX, EBX		
	JG		_error
	SUB		EAX, 48							;convert ASCII to SDWORD
	IMUL	EDI, 10
	ADD		EAX, EBX
	MOV		EDI, EAX
	LOOP	_stringLoop	
	MOV		EAX, [EBP+36]
	MOV		[EAX], EDI
	LOOP	_validateLoop
	JMP		_validateNeg
	
	_convert:
	SUB		EAX, 48							;convert ASCII to SDWORD
	IMUL	EDI, 10
	ADD		EAX, EBX
	MOV		EDI, EAX
	LOOP	_stringLoop	
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
	MOV		EAX, [EBP + 36]
	NEG		EAX
	CMP		EAX, MINVALUE
	JNE		_exit
	INC		EAX		

	_exit:
	MOV		EDX, [EBP + 48]
	INC		EDX
	MOV		[EBP + 48], EDX
	MOV		[EBP + 36], EAX		; output storedDec
	MOV		EDX, 1				; boolie return TRUE
	MOV		[EBP + 42], EDX
	_errorExit:
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
	
	MOV		EAX, [EBP + 28]			;move val SDWORD into EDX
	MOV		EDX, [EAX]
	MOV		EAX, EDX
	
	CDQ
	MOV		ECX, 10
	IDIV	ECX
	MOV		ECX, 0
	ADD		EDX, 48					;else add 48 to remainder to convert to ASCII
	MOV		ECX, [EBP + 24 + EBX]
	MOV		[ECX], EDX				;move into ASCIIstring
	INC		EBX	
	CMP		EAX, ECX				;if quotient is 0, then last digit
	JLE		_displayString
	JMP		_loop
					

	_lastDigit:
	;CMP		EDX, 0
	;JL		_negative
	;JMP		_displayString

	;_negative:
	;NEG		EDX
	;ADD		EDX, 48
	;MOV		[EBP + 8 + EBX], EDX
	;INC		EBX
	;MOV		[EBP + 8 + EBX], 45

	;invoke mDisplayString
	_displayString:
	mDisplayString	[EBP+24]

	POP EBP

	RET
	WriteVal ENDP

END main
