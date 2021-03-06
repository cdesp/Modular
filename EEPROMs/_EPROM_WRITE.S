ORG $A000

MNU:	LD HL,MSG1
	CALL RS_TXT

	CALL RS_RX
	CP 'U'
	CALL Z,MYUPLOD
	CP 'P'
	CALL Z,PERMSAVE 
	CP 'M'
	JP Z,4 ;Menu
	JR MNU


MSG1:	DEFM "EEPROM WRITE MODE U,P,M"
	DB 10,13,0

MSG2:	DEFM "EEPROM PERMANENT SAVED"
	DB 10,13,0
	
MSG3:	DEFM "SAVING... WAIT"	
	DB 10,13,0

MSG4:	DEFM "PRESS P TO PERMANENT SAVE"	
	DB 10,13,0

PERMSAVE:  DI
           LD HL,MSG2
	   CALL RS_TXT
	   LD A,(0x0E38)
           LD A,(0x31C7)
	   LD A,(0x03E0)
	   LD A,(0x3C1F)
	   LD A,(0x303F)
	   LD A,(0x0FC0)
	   RET

MYUPLOD: ;LD HL,MSG3
	 ;CALL RS_TXT
	 CALL UPLOD
	 LD HL,MSG4
	 CALL RS_TXT
	 RET
;UPLOAD (SEND TO Z80) BYTES THROUGH RS232
;RECEIVING LOW BYTE FIRST
;HL ADDRESS IN RAM BYTE 1&2
;BC BYTES TO RECEIVE BYTE 3&4 

UPLOD: DI	
	;LD HL,DATST
	;CALL LCDLN2
	;CALL LCDTXT

	CALL RS_RX 	;HL HAS RAM ADDRESS
	LD L,A
	CALL RS_RX
	LD H,A
	CALL RS_RX	;BC HAS TOTAL DATA TO BE SEND
	LD C,A
	CALL RS_RX
	LD B,A
	
	LD DE,PRGEX 	;SAVE TO PRGEX THE PROG EXEC ADDR=HL
	LD A,L
	LD (DE),A
	INC DE
	LD A,H
	LD (DE),A
	
	CP $80
	CALL C,EPROMON  ;C ROM IN 0-8000 NC ROM IN 8000-FFFF ;SET ROM WRITABLE = '0'

NORWR:	PUSH HL
NXB:	CALL RS_RX
	LD (HL),A
	INC HL
	DEC BC
	LD A,B
	OR C
	JR NZ,NXB	;LOOP ALL BYTES	
  	CALL EPROMOFF

	;LD HL,DATTR
	;CALL LCDCLS
	;CALL LCDTXT
	LD A,5	
	CALL SCPRN			
	CALL DELAY
	
	POP HL
	;CALL (HL)
	
  EI
	RET


INCLUDE MYOS_2_0000.SYM