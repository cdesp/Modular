ORG $4000

INCLUDE MYOS_2_0000_HARD.SYM

JP TSTSTART






BUFFER  DEFS 60 ;MAX 20 BYTES TO SEND
BUFCNT	DEFW 0  ;BYTE COUNT



TSTSTART:	NOP

AGN:    LD HL,MS3
    CALL RS_TXT
	
 CALL GET_CHAR	;TAKE A CHAR FROM RS232 OR PS2 KEYB
CP 'M'
JP Z,MENU
CP 'I'
CALL Z,I2C_INIT
CP 'O'
CALL Z,I2CTEST
CP 'A'
CALL Z,I2CADJUST
CP 'S'
CALL Z,I2CSETSQ
CP 'F'
CALL Z,LCD16_INIT
CP 'G'
CALL Z,LCD16_TEST


JR AGN

MS3:DEFM "M:MENU, I,A,O,S:I2C INIT-ADJ-NOW-SQOUT"
    DB 10,13,0







TIMMSG  	DEFM '10/10/2020 12:43:45'
		DB 0

CONVHL:		;PUSH DE
		;CALL B2Bcd 
		;POP DE
	        CALL Bcd2ASC
		LD H,0
		RET


PRINTTIME:	LD IX,BUFFER
		LD DE,TIMMSG		
		LD H,0
		LD L,(IX+4) ;DAY
		CALL CONVHL
		INC DE
		LD L,(IX+5) ;MONTH
		CALL CONVHL
		INC DE
		INC DE
		INC DE
		LD L,(IX+6) ;YEAR
		CALL CONVHL
		INC DE
		LD L,(IX+2) ;HOUR
		CALL CONVHL
		INC DE
		LD L,(IX+1) ;MIN
		CALL CONVHL
		INC DE
		LD L,(IX)   ;SECS
		CALL CONVHL
		LD DE,COL_GREEN   ; SET FCOLOR
		LD HL,FCOLOR
            	LD (HL),E
            	INC HL
            	LD (HL),D		
		LD A,60
  		LD (CURSORX),A
  		LD A,1
		LD (CURSORY),A  
  		LD HL,TIMMSG   
		CALL LCD_MSG
		LD A,(IX+3)  ;DAY OF WEEK
		;TODO PRINT DAY
		;PRINT ON LCD
		LD D,0 ;COL
		LD E,1 ;ROW
		CALL LCD16_SETCURS
		LD HL,TIMMSG+3
		CALL LCD16_PRINT
		RET


RST_BUF:	XOR A
		LD B,50
		LD HL,BUFFER
LP1: 		LD (HL),A
		INC HL
		DJNZ LP1
		RET


ERRCNT DB 0

CHKERROR:	LD B,7
		LD HL,BUFFER
CEAGN:		LD A,(HL)
		CP $FF
		JR Z,STER
		INC HL
		DJNZ CEAGN
		XOR A
		RET
STER:		LD HL,ERRCNT
		INC (HL)
		SCF
		RET



I2CTEST:	LD HL,ERRCNT
		XOR A
		LD (HL),A
		CALL RST_BUF
		LD HL,MES0
		CALL RS_TXT
		LD B,60 ;10 TIMES EVERY SECOND
MYLP1: 		PUSH BC
		CALL DS1307_GETNOW
		JR C,ERRREAD  ;IF ERROR DONT PRINT
		CALL CHKERROR		
		JR C,ERRREAD  ;IF ERROR DONT PRINT	
		CALL PRINTTIME
		LD B,7	
		CALL BUFBCD2BIN ; CONVERT BUFFER FROM BCD TO BINARY
		CALL SHOWOUT
ERRREAD:	LD	BC,FFFFh ;8000H FOR 4MHZ
		CALL	PAUSE_LOOP	; 0.5 second delay 
		LD	BC,FFFFh ;8000H FOR 4MHZ
		CALL	PAUSE_LOOP	; 0.5 second delay 
		POP BC
		DJNZ MYLP1
		LD A,10
		CALL RS_TX
		LD A,13
		CALL RS_TX
		LD HL,ERRCNT
		LD A,(HL)
		CALL SOUTAS
		RET


I2CADJUST:	CALL DS1307_ADJUST
		CALL RST_BUF
		RET

BUFBCD2BIN: 	LD HL,BUFFER+1  	 
LL1:     	LD A,(HL)
         	PUSH BC  
	 	CALL BCD2BN
	 	POP BC
	 	LD (HL),A
	 	INC HL
	 	DJNZ LL1 
         	RET  


SHOWOUT:	LD HL,MES1
		CALL RS_TXT

		LD HL,BUFFER
		LD B,10
LP2: 		LD A,(HL)
		PUSH BC
		PUSH HL
		CALL SOUTAS
		POP HL
		POP BC
		INC HL
		DJNZ LP2
		RET


;FREQUENCIES
; D=16           1HZ
; D= 16.OR.1     4096HZ
; D= 16.OR.2     8192HZ
; D= 16.OR.3     32768HZ

I2CSETSQ:	LD D,16
		CALL DS1307_SETSQ
		RET



LCD16_TEST:	LD HL,LMES0
		CALL LCD16_PRINT
		LD D,0 ;COL
		LD E,1 ;ROW
		CALL LCD16_SETCURS
		LD HL,LMES1
		CALL LCD16_PRINT
		RET






LMES0 DEFM "HELLO WORLD!"
      DB 0

LMES1 DEFM "SECOND ROW.."
      DB 0

MES0 DEFM "RETREIVING CURRENT TIME FROM DS1307"
     DEFB 10,13,0

MES1 DEFM "BYTES RECEIVED:"
     DEFB 10,13,0

MES2 DEFM "GETTING BYTES FOR NOW()..."
     DEFB 10,13,0

MES3 DEFM "SENDING COMMAND FOR NOW()..."
     DEFB 10,13,0

MES4 DEFM "SENDING COMMAND FOR ADJUST()..."
     DEFB 10,13,0

MES5 DEFM "SENDING COMMAND FOR setSqwOutSignal()..."
     DEFB 10,13,0


MESERR DEFM "ERROR ON RECEIVING"
     DEFB 10,13,0

MESERR0 DEFM "ERROR ON SENDING"
     DEFB 10,13,0

