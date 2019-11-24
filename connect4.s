

PINSEL0	EQU	0xE002C000
U0RBR	EQU	0xE000C000
U0THR	EQU	0xE000C000
U0LCR	EQU	0xE000C00C
U0LSR	EQU	0xE000C014


	AREA	globals, DATA, READWRITE

BOARD	DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0

	AREA	RESET, CODE, READONLY
	ENTRY

	; initialise SP to top of RAM
	LDR	R13, =0x40010000	; initialse SP

	; initialise the console
	BL	inithw

	;
	; your program goes here
	;
	;BL computer
reset		BL intBoardSub
			LDR R0, =str_go
			BL puts
			LDR R0, =str_against
			BL puts
			
			BL get
			CMP R0,#0x70
			BEQ player
			CMP R0,#0x63
			BEQ vsComp
			BNE reset
			
player			
			LDR R3, =0x59
			LDR R12, =0
			
main
			
			BL currentPiece
			BL printBoardSub	
			BL puts
invalidMove	
			BL get
			BL validMove
			CMP R1,#1
			BEQ invalidMove
			BL makeMove
			LDR R6, =4
			BL winnerRow
			CMP R12, #1
			BEQ winner
			
			BL winnerCol
			CMP R12, #1
			BEQ winner
			
			BL winnerDiag
			CMP R12, #1
			BEQ winner
			
			BL winnerDiag2
			CMP R12, #1
			BEQ winner
			
			
	
			B main
winner
			BL printBoardSub
			LDR R0,=str_winner
			BL puts
			LDR R0,=str_gameOver
			BL puts
keepAsking	BL get
			CMP R0,#0x71
			BEQ reset
			BNE keepAsking
			LDR R0, =str_gameOver
			BL puts
			
vsComp
			LDR R3, =0x52
			LDR R12, =0
mainComp
			
			BL currentPieceComp
			BL printBoardSub	
			BL puts
invalidMoveComp	
			BL get
			BL validMove
			CMP R1,#1
			BEQ invalidMoveComp
			BL makeMove
			LDR R6, =4
			BL winnerRow
			CMP R12, #1
			BEQ winnerComp
			
			BL winnerCol
			CMP R12, #1
			BEQ winnerComp
			
			BL winnerDiag
			CMP R12, #1
			BEQ winnerComp
			
			BL winnerDiag2
			CMP R12, #1
			BEQ winnerComp
			
			BL computer
	
			B vsComp
winnerComp
			BL printBoardSub
			LDR R0,=str_winner
			BL puts
			LDR R0,=str_gameOver
			BL puts
keepAskingC	BL get
			CMP R0,#0x71
			BEQ reset
			BNE keepAskingC
			LDR R0, =str_gameOver
			BL puts

stop	B	stop


;
; your subroutines go here
;
	;current piece
currentPiece	
	CMP R3, #0x59
	BEQ yellow
	BNE red
red
	LDR R3,=0x59
	LDR R0, =str_yellowTurn
	bx lr
yellow
	LDR R3,=0x52
	LDR R0, =str_redTurn
	bx lr
	
	
currentPieceComp	
	CMP R3, #0x59
	BEQ yellowC
	BNE redC
redC
	LDR R0, =str_yellowTurn
	bx lr
yellowC
	LDR R0, =str_redTurn
	bx lr
	;int board
	
intBoardSub
			PUSH{R4,R5,R8,lr}
			LDR R4, =0x40000000
			LDR R5, =0x30
			LDR R8, =0
intBoard	STR R5, [R4, R8, LSL #2]
			ADD R8, R8, #1
			CMP R8, #42
			BNE intBoard
			POP{R4,R5,R8,PC}
			
	;print board

printBoardSub	
			PUSH {R0,R4,R5,R8,lr}
			LDR R0,=str_column
			BL puts
					
			LDR R4, =0x40000000 ;location of board
			LDR R5, =0
			LDR R6, =0
printBoard	LDR R0, [R4]
			BL put
			LDR R0, =0x20
			BL put
			BL put
			ADD R4, R4, #4
			ADD R5, R5, #1
			ADD R6, R6, #1
			CMP R6, #7
			BEQ downACol
			CMP R5, #42
			BLT printBoard
			B endPrint

downACol
			LDR R0, =str_newLine
			BL puts
			LDR R6, =0
			B printBoard
endPrint

			POP{R0,R4,R5,R8,PC}
			
	;make move
	
validMove
			PUSH{R4,lr}
			MOV R4, R0
			CMP R4,#0x71
			BEQ reset
			CMP R4,#0x31
			BLT nope
			CMP R4,#0x37
			BGT nope
			
			POP{R4,PC}
nope
			LDR R0, =str_invalid
			BL puts
			LDR R1, =1
			
			POP{R4,PC}
makeMove
			PUSH{R4,R5,R6,R0,lr}
			MOV R7, R0	;moves character from console into R0
			SUB R7, R7, #0x31 ;sub 0x31 to go from charcter to col number
			LDR R6, =4 ;element size
			MOV R10, #7 ;row size
			MOV R11, #5 ;starting row
			
loop		MUL R8, R11, R10 ;  starting row * row size	
			ADD R8, R8, R7 ; + coloumn = index			
			MUL R8, R6, R8 ; byte offset = index * element size
			
			LDR R4, =0x40000000 ; location of board		
			ADD R4, R4, R8 ;address = array base address + byte offset
			LDR R5, [R4] ; LOAD VALUE FROM BOARD
			CMP R5, #0x30 ;IS THE VALUE = 0?
			BNE down ;If it is 0 branch to down
			
			MOV R5, R3 ; load in Y
			STR R5, [R4]	;store piece in memory
			B done
down 
			CMP R11,#0 
			BEQ nope
	
			SUB R11, R11, #1 ;moves up one row
			B loop	;loops
			
done		
			MOV R1, R11 ;current row
			MOV R2, R7 ; current col
			
			POP{R4,R5,R6,R0,PC}
			
						;winner;
				
				;Passed in parameters;
				;R1 = Row of the piece placed
				;R3 = Current piece
				
				;Return;
				;R0 if there is a winner
				
	
winnerRow
			PUSH{R4,R5,R7-R11,lr}
			MOV R11, R1 ;piece row
			LDR R9, =0 ;counter = 0
			LDR R7, =0; first column
			MOV R10, #7 ;row size	
						
loopRow		
			LDR R4, =0x40000000 ;R4 points to the board
			CMP R7, #7 ;have we hit the end of the row?
			BEQ endWin
			MUL R8, R11, R10 ;  starting row * row size	
			ADD R8, R8, R7 ; + coloumn = index
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			ADD R4, R4, R8 ;address = array base address + byte offset		
			LDR R5, [R4] ;load in the piece
			CMP R5, R3 ;is it equal to the current piece?
			BEQ addCol   ; if it is add to the counter
			BNE backRow   ; if its not we move to the next column
backRow		
			LDR R9, =0 ; reset the counter
			ADD R7, R7, #1 ; move over a column
			B loopRow
addCol	
			ADD R9,R9,#1 ;counter + 1
			CMP R9, R6 ; have we got 4 in a row?
			BEQ putWinner
			ADD R7,R7,#1 ; move over a column
			B loopRow
putWinner	
			
			MOV R1, R11
			MOV R2, R7
			LDR R12,=1;
			LDR R0, =1;
			B endRow
endWin
			LDR R0, =0;
			B endRow
endRow		
			POP{R4,R5,R7-R11,PC}
			
winnerCol
			PUSH{R4,R5,R7-R11,lr}
			MOV R7, R2
			LDR R9, =0 ;counter = 0
			MOV R10, #7 ;row size
			MOV R11, #5 ;starting row
			;LDR R6, =4
loopCol		
			CMP R11, #0
			BEQ endWinCol
			LDR R4, =0x40000000
			MUL R8, R11, R10 ;  starting row * row size	
			ADD R8, R8, R7 ; + coloumn = index
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			ADD R4, R4, R8 ;address = array base address + byte offset			
			LDR R5, [R4] ;load in the piece
			CMP R5, R3	;is it equal to the current piece?
			BEQ addRow	; if it is add to the counter		
			BNE backCol	; if its not we move to the next column
backCol		
			LDR R9,=0 ;reset the counter
			SUB R11, R11, #1 ;move up a row
			B loopCol
addRow	
			ADD R9,R9,#1 ;counter + 1
			CMP R9, R6 ;have we got 4 in a row?
			BEQ putWinnerCol
			SUB R11,R11,#1 ;move up a row
			B loopCol
putWinnerCol	
			
			LDR R12,=1
			MOV R1, R11
			MOV R2, R7
			LDR R0,=1
			B endCol
			
endWinCol
			LDR R0,=0
			B endCol
endCol	
			POP{R4,R5,R7-R11,PC}
			
			;winner diag bottom corner
winnerDiag
			PUSH{R1,R2,R3,R4,R5,R7-R11,R0,lr}
			MOV R7, R2 ;col
			MOV R11, R1 ; row
			LDR R9, =0 ; counter = 1
			MOV R10, #7; row size
			
			
			B findStart
findStart

loopFind	CMP R11,#5 ;have we hit bottom row
			BEQ loopDiag
			CMP R7,#0 ;have we hit left most column
			BEQ	loopDiag
			SUB R7, R7, #1 ;one row to the left
			ADD R11,R11,#1 ;one column down
			
			B loopFind
			
loopDiag	LDR R4,=0x40000000 ;location of board
			CMP R7, #7
			BEQ endDiag
			CMP R11,#0
			BLT	endDiag
			MUL R8, R11,R10 ;  starting row * row size
			ADD R8,R8, R7  ; + coloumn = index
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			ADD R4, R4, R8 ;address = array base address + byte offset	
			LDR R5, [R4] ;load in the piece
			CMP R5, R3 ;is it equal to the current piece?
			BEQ addDiag ; if it is add to the counter
			B backDiag ; if not move to next piece
backDiag
			LDR R9, =0 ;reset counter
			ADD R7, R7, #1 ;move over a column
			SUB R11, R11, #1 ;move up a row
			B loopDiag
addDiag
			ADD R9, R9, #1 ;counter + 1
			CMP R9, R6 ;have we found 4 in a row?
			BEQ winDiag		
			ADD R7, R7, #1 ;move over a column
			SUB R11, R11, #1 ; up a row
			B loopDiag
			
winDiag
			
			LDR R12,=1
			MOV R1, R11
			MOV R2, R7
			LDR R0,=1
			B endDiagSub			
endDiag		
			LDR R0,=0
endDiagSub
			
			POP{R1,R2,R3,R4,R5,R7-R11,R0,PC}
						
			;Diag top corner
winnerDiag2
			PUSH{R1,R2,R3,R4,R5,R7-R11,R0,lr}
			MOV R7, R2 ;col
			MOV R11, R1 ; row
			LDR R9, =0 ; counter = 1
			MOV R10, #7; row size
			LDR R6, =4
			B findStart2
findStart2

loopFind2	CMP R11,#0 ;have we hit bottom row
			BEQ loopDiag2
			CMP R7,#0 ;have we hit left most column
			BEQ	loopDiag2
			SUB R7, R7, #1
			SUB R11,R11,#1			
			B loopFind2
			
loopDiag2	LDR R4,=0x40000000
			CMP R7, #7
			BEQ endDiag
			CMP R11,#0
			BLT	endDiag
			MUL R8, R11,R10 ;  starting row * row size
			ADD R8,R8, R7 ; + coloumn = index
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			ADD R4, R4, R8 ;address = array base address + byte offset	
			LDR R5, [R4] ;load in the piece
			CMP R5, R3 ;is it equal to the current piece?
			BEQ addDiag2 ; if it is add to the counter
			B backDiag2 ; if not move to next piece
backDiag2
			LDR R9,=0 ;reset counter
			ADD R7,R7,#1 ;next col
			ADD R11,R11,#1 ;next row
			B loopDiag2
addDiag2
			ADD R9, R9, #1 ;counter + 1
			CMP R9,R6 ;have we got 4 in a row?
			BEQ winDiag2	
			ADD R7,R7,#1 ;next col
			ADD R11,R11,#1 ;next row 
			B loopDiag2
winDiag2
			
			MOV R1, R11
			MOV R2, R7
			LDR R12, =1;
			B endDiag2			
endDiag2			
			POP{R1,R2,R3,R4,R5,R7-R11,R0,PC}
			
			
			
			
			
			;Computer
computer
			PUSH{R1,R2,R4-R11,R0,lr}
			MOV R10, #7 ;row size	
				; Variable used to determine if the next move will be a win
			
			 
			BL winnerDiag
			CMP R0,#1
			BEQ makeMoveComp
			CMP R0,#0
			B  check2
			
check2		BL winnerDiag2
			CMP R0,#1
			BEQ makeMoveComp
			CMP R0,#0
			B blockWinningMoves
			

					
			
blockWinningMoves			
			
			LDR R3,=0x52
			LDR R6,=3
			
upTopR		
			;Check if the player is about to win via row
			BL winnerRow   
			CMP R0,#0     ;hasnt found a move
			BEQ checkCol ;
			CMP R0,#1	; found a move
			BEQ gottemR
			
		
			
upTopC		
			;Check if the player is about to win via col
checkCol	BL winnerCol  
			CMP R0,#0   ;hasnt found a move
			BEQ foundNoneC
			CMP R0,#1  ;found a move
			BEQ gottemC			
			
loopCompR
			SUB R1,R1,#1
			B upTopR
			
loopCompC	
			ADD R2,R2,#1
			B upTopC
gottem		;make the move
			LDR R12,=0
			BL makeMoveComp
			B turnOver


foundNoneC

			B checkThreeInARow
			
turnOver
			;Checks if the computer has won
			LDR R6,=4
			BL winnerRow
			CMP R12, #1
			BEQ winner
			
			BL winnerCol
			CMP R12, #1
			BEQ winner
			
			BL winnerDiag
			CMP R12, #1
			BEQ winner
			
			BL winnerDiag2
			CMP R12, #1
			BEQ winner
			POP{R1,R2,R4-R11,R0,PC}

checkThreeInARow
			LDR R3,=0x59
			LDR R6,=3
			BL winnerRow
			CMP R0,#1
			BEQ gottem
			
			BL winnerCol
			CMP R0,#1
			BEQ gottem

			B checkTwoInARow
checkTwoInARow	;if no move need to be blocked check if the computer has a good move to make	
			LDR R3,=0x59
			LDR R6,=2
			BL winnerRow
			CMP R0,#1
			BEQ gottem
			
			BL winnerCol
			CMP R0,#1
			BEQ gottem
random		;no move so just do whatever
			LDR R1,=2

			BL makeMoveComp

			B turnOver
gottemR
			;Here we have found a 3 in a row, but we dont know if the next move is valid so we index into the next coloumn and check if it is empty
			LDR R12, =0
			MOV R7, R2
			ADD R7,R7,#1
			MOV R11,R1
			LDR R4, =0x40000000
			MUL R8, R11, R10 ;  starting row * row size	
			ADD R8, R8, R7 ; + coloumn = index
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			ADD R4, R4, R8 ;address = array base address + byte offset			
			LDR R5, [R4] ;load in the piece
			CMP R5, #0x30	;is it equal to zero?
			BNE checkCol
		
			BL makeMoveComp
			B turnOver
gottemC	
			;Here we have found a 3 in a row, but we dont know if the next move is valid so we index into the next row and check if it is empty
			LDR R12, =0
			MOV R11, R1
			SUB R11,R11,#1
			MOV R11,R1
			LDR R4, =0x40000000
			MUL R8, R11, R10 ;  starting row * row size	
			ADD R8, R8, R7 ; + coloumn = index
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			ADD R4, R4, R8 ;address = array base address + byte offset			
			LDR R5, [R4] ;load in the piece
			CMP R5, #0x30	;is it equal to zero?
			BNE checkThreeInARow
		
			BL makeMoveComp
			B turnOver

makeMoveComp ;Same make move function but for the computer to use
			PUSH{R4,R5,R7-R11,R0,lr}
			LDR R6, =4 ;element size
			MOV R10, #7 ;row size
			MOV R11, #5 ;starting row
			LDR R3,=0x59
			
loopC		MUL R8, R11, R10 ;  starting row * row size	
			ADD R8, R8, R7 ; + coloumn = index			
			MOV R8, R8, LSL #2 ; byte offset = index * element size
			
			LDR R4, =0x40000000 ; location of board		
			ADD R4, R4, R8 ;address = array base address + byte offset
			LDR R5, [R4] ; LOAD VALUE FROM BOARD
			CMP R5, #0x30 ;IS THE VALUE = 0?
			BNE downC ;If it is 0 branch to down
			
			MOV R5, R3 ; load in Y
			STR R5, [R4]	;store piece in memory
			B doneC
downC
			CMP R11,#0 
			BEQ nope
	
			SUB R11, R11, #1 ;moves up one row
			B loopC	;loops
			
doneC		
			MOV R1, R11 ;current row
			MOV R2, R7 ; current col
			
			
			POP{R4,R5,R7-R11,R0,PC}
;
; inithw subroutines
; performs hardware initialisation, including console
; parameters:
;	none
; return value:
;	none
;
inithw
	LDR	R0, =PINSEL0		; enable UART0 TxD and RxD signals
	MOV	R1, #0x50
	STRB	R1, [R0]
	LDR	R0, =U0LCR		; 7 data bits + parity
	LDR	R1, =0x02
	STRB	R1, [R0]
	BX	LR

;
; get subroutine
; returns the ASCII code of the next character read on the console
; parameters:
;	none
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
get	LDR	R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
get0	LDR	R0, [R1]		; wait until
	ANDS	R0, #0x01		; receiver data
	BEQ	get0			; ready
	LDR	R1, =U0RBR		; R1 -> U0RBR (Receiver Buffer Register)
	LDRB	R0, [R1]		; get received data
	BX	LR			; return

;
; put subroutine
; writes a character to the console
; parameters:
;	R0 - ASCII code of the character to write
; return value:
;	none
;
put	LDR	R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
	LDRB	R1, [R1]		; wait until transmit
	ANDS	R1, R1, #0x20		; holding register
	BEQ	put			; empty
	LDR	R1, =U0THR		; R1 -> U0THR
	STRB	R0, [R1]		; output charcter
put0	LDR	R1, =U0LSR		; R1 -> U0LSR
	LDRB	R1, [R1]		; wait until
	ANDS	R1, R1, #0x40		; transmitter
	BEQ	put0			; empty (data flushed)
	BX	LR			; return

;
; puts subroutine
; writes the sequence of characters in a NULL-terminated string to the console
; parameters:
;	R0 - address of NULL-terminated ASCII string
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
puts	STMFD	SP!, {R4, LR} 		; push R4 and LR
	MOV	R4, R0			; copy R0
puts0	LDRB	R0, [R4], #1		; get character + increment R4
	CMP	R0, #0			; 0?
	BEQ	puts1			; return
	BL	put			; put character
	B	puts0			; next character
puts1	LDMFD	SP!, {R4, PC} 		; pop R4 and PC


;
; hint! put the strings used by your program here ...
;

str_go
	DCB	"Let's play Connect4!!",0xA, 0xD, 0xA, 0xD, 0
	
str_newl
	DCB	0xA, 0xD, 0x0

str_newLine
	DCB "   ",0xA, 0xD, 0xA, 0xD, 0

str_redTurn
	DCB "Its reds turn",0xA, 0xD, 0xA, 0xD, 0

str_yellowTurn
	DCB "Its yellows turn",0xA, 0xD, 0xA, 0xD, 0

str_column
	DCB "1  2  3  4  5  6  7 ",0xA, 0xD, 0xA, 0xD, 0
	
str_winner
	DCB "Winner Winner" ,0xA, 0xD, 0xA, 0xD, 0

str_invalid
	DCB "Invalid move, try again" ,0xA, 0xD, 0xA, 0xD, 0

str_gameOver
	DCB "Press q to restart", 0xA, 0xD, 0xA, 0xD, 0

str_against
	DCB "Enter 'p' to play against a player, or c to play vs the computer" , 0xA, 0xD, 0xA, 0xD, 0
	END
