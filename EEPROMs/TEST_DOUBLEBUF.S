ORG $4000


INCLUDE MYOS_2_0000_HARD.SYM

JP STARTTHIS




VGABUF		EQU $FFF0


MSG1A 		DEFM "THIS IS BUFFER 2 TWO"
		DB 10,13,0

MSG2A 		DEFM "THIS IS BUFFER 1 ONE"
		DB 10,13,0

MSG1 		DEFM "T23456789012345678901234567890123456789T123456789012345678901234567890123456789T"
		DB 10,13,0

MSG2 		DEFM "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
		DB 10,13,0

	


STARTTHIS0:	LD B,4	;$8000
		LD A, 12 							;PAGE 12 IS BUFFER 1
		CALL PUTPGINBANK
		LD HL,MSG1
		LD DE,$8000
		LD BC,16
		LDIR	
		CALL RS_RX	
		LD B,4	;$8000
		LD A, 8 							;PAGE 8 IS BUFFER  0
		CALL PUTPGINBANK		
		LD HL,MSG2
		LD DE,$8000
		LD BC,16
		LDIR	
		CALL RS_RX
		JR DOLOOP	


PRINTALL:	LD B,1
PAAGN:		LD A,B
		OR A
		RET Z
		PUSH BC
		CALL VGA_PRCH
		POP BC
		INC B
		JR PAAGN
		

STARTTHIS:	LD A,1
		CALL VGA_SETWRKBUFF
		CALL VGA_TXTCLR
		CALL PRINTALL
		LD A,0
		CALL VGA_SETWRKBUFF
		CALL VGA_TXTCLR
		JR DOCONT

STARTTHIS1:	LD A,1
		CALL VGA_SETWRKBUFF
		CALL VGA_TXTCLR
		LD B,10
NXMSG1:		LD HL,MSG1
		PUSH BC
		CALL VGA_PRINT
		POP BC
		DJNZ NXMSG1
		CALL RS_RX
		LD A,0
		CALL VGA_SETWRKBUFF
		CALL VGA_TXTCLR
DOCONT:		LD B,10
NXMSG2:		LD HL,MSG2
		PUSH BC
		CALL VGA_PRINT		;PRINTS AT WORKING BUFFER
		POP BC
		DJNZ NXMSG2

DOLOOP:		CALL RS_RX		;WAIT FOR A KEY		
		CALL VGA_SWVISBUFF	;SWITCH BUFFER
		JR DOLOOP