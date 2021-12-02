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

	mGetString		MACRO	promptAddress, ASCIIarrAddress, ASCIIcountAddress, bytesReadAddress

		;prompt user for string input string input, store using ReadString
		MOV		EDX, promptAddress
		CALL	WriteString
		MOV		EDX, ASCIIarrAddress
		MOV		ECX, MAXBYTES
		CALL	ReadString
		MOV		[bytesReadAddress], EAX
		CALL	CrLF

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
	MINVALUE = -2147483648
	MAXVALUE = 2147483647
	MAXASCII = 57
	MAXBYTES = 12


.data

	progTitle	BYTE	"			String Primitives and Macros in MASM Assembly",13,10,0
	programmer	BYTE	"					by Allison Land",13,10,13,10,0
	instruction	BYTE	"This program requires that you enter ten numbers between -2,147,483,648 and 2,147,483,647.",13,10,13,10,0
	prompt		BYTE	"Please enter a number: ",0
	error		BYTE	"ERROR: This is either not a valid number, or your integer has too many digits.",0
	ASCIIarr	BYTE	maxBytes DUP(?)
	ASCIIcount	SDWORD	1				
	;space		BYTE	" ",0
	decArr		SDWORD	10 DUP(?)
	storedDec	SDWORD	?
	bytesRead	SDWORD	?
	numCount	SDWORD	0
	space		BYTE	" ",0

.code
main PROC

	;get 10 valid integers from the user, introduction
	MOV		EDX, OFFSET progTitle
	CALL	WriteString
	MOV		EDX, OFFSET programmer
	CALL	Writestring
	MOV		EDX, OFFSET instruction
	CALL	WriteString

	MOV		EDI, OFFSET storedDec
	MOV		ESI, OFFSET ASCIIarr
	MOV		ECX, 10
	MOV		EBX, 0
	_loop:
	PUSH	OFFSET ASCIIcount
	CALL	WriteVal
	MOV		EDX, OFFSET space
	CALL	WriteString

	; user inputs 10 numbers
	PUSH	OFFSET prompt
	PUSH	OFFSET ASCIIarr
	PUSH	OFFSET ASCIIcount
	PUSH	OFFSET bytesRead
	CALL	ReadVal

	;store in array using register indirect addressing
	ADD		ESI, EBX
	MOV		EAX, ESI
	MOV		EAX, [EDI]
	MOV		[ESI], EAX
	ADD		EBX, 4
	LOOP	_loop
	;loop through array, display ints

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
; Receives: {parameters: progTitle (reference), programmer (reference), ec1 (reference), ec2 (reference),
;			progDesc(reference)}
;
; Returns: None
; ---------------------------------------------------------------------------------------------------

	readVal PROC
	;invoke mGetString
	PUSH	EBP
	MOV		EBP, ESP
	mGetString		[EBP + 20] , [EBP + 16], [EBP + 12], [EBP + 8]

	; convert ASCII to SDWORD using LODSB/STOSB (convert each digit)

	;validate that characters are valid and can fit 32 bit sdword

	POP EBP
	RET 20
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
	
	WriteVal PROC USES EAX

	PUSH	EBP
	MOV		EBP, ESP

	;convert SDword to ASCII
	MOV		EAX, [EBP + 8]	
	ADD		EAX, 48 
	MOV		storedDec, EAX

	;invoke mDisplayString
	mDisplayString	OFFSET storedDec

	POP EBP
	RET	8
	WriteVal ENDP

END main
