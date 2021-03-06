ORG $9000


;8 BIT COLORS

RED8 	EQU 11100000B
GREEN8 	EQU 00011100B
BLUE8	EQU 00000011B
WHITE8	EQU 11111111B
BLACK8  EQU 00000000B
MAGENTA8 EQU 11100011B
YELLOW8 EQU 11111100B
CYAN8	EQU 00011111B

LRED8 	EQU 01100000B
LGREEN8 EQU 00001100B
LBLUE8	EQU 00000001B
LWHITE8	EQU 01001101B
LBLACK8 EQU 10010010B
LMAGENTA8 EQU 01100001B
LYELLOW8 EQU 01101100B
LCYAN8	EQU 00001101B


DGREEN8 EQU 00011000B	;DARK
DBLUE8	EQU 00000010B	;DARK
CREAM8	EQU 11011010B  
GREY8	EQU 00100101B	;DARK
PINK8 	EQU 11000010B
ORANGE8 EQU 11011000B
DCYAN8	EQU 00011010B	;DARK

DDGREEN8 EQU 00001100B  ;DARK
DDBLUE8	EQU 00000001B   ;NAVY
LGREY8	EQU 01001101B
LPINK8  EQU 10010010B


INCLUDE MYOS_2_0000.SYM

AGN:
LD HL,MS3
    CALL RS_TXT
 CALL GET_CHAR	;TAKE A CHAR FROM RS232 OR PS2 KEYB
CP 'M'
JP Z,MENU
CP 'L'
CALL Z,LCD_TEST
CP '8'
CALL Z,TEST8BIT
CP 'D'
CALL Z,DO8BITCOLOR


CP 'R'
RET Z
JR AGN

MS3 DEFM 'L:TEST; K:LETTERS; P:LINES; M:MENU '
    DEFB 10,13,0


LCD_WR_DAT:        
LCD_WR_BUS:  ;ONE PARAM VL
             ;PASS BY REGISTER D=VL 
             ;DESTROYS A            
    LD A,D  
    OUT (LCD_CS),A
    RET


LCD_WR_COM:  ;ONE PARAM VL
             ;PASS BY REGISTER D=VL 
             ;DESTROYS A            
    LD A,D  
    OUT (LCD_RS),A
    RET


@LCD_SETXY:    ;SET WINDOW ON LCD TO FILL WITH BYTES
              ;4 PARAMS PASS BY STACK X1,Y1,X2,Y2
              ;2 BYTES EACH PARAM TOTAL 8 BYTES
              ;X1,X2,Y1,Y2   X1,X2 SHOULD SWAP Y1,Y2 CAUSE OF THE LCD ORIENTATION
              ;WE SEND HERE
              ;X1 = IX+6_7  Y1=IX+4_5
              ;X2=  IX+2_3  Y2=IX+0_1
              
              ;WE SEND ON LCD
              ;Y1=IX+6_7 Y2=IX+2_3
              ;X1=IX+4_5 X2=IX+0_1
              ;HIGH BYTE FIRST SO +8 IS H AND +7 IS L
              ;SHOULD ADD SP,8 AFTER RET
              
    LD IX,2
    ADD IX,SP         
    LD D,0x2A         ;SEND IX PARAMS
    CALL LCD_WR_COM    
    LD D,(IX+7)
    CALL LCD_WR_DAT
    LD D,(IX+6)
    CALL LCD_WR_DAT
    LD D,(IX+3)
    CALL LCD_WR_DAT
    LD D,(IX+2)
    CALL LCD_WR_DAT       
    LD D,0x2B         ;SEND Y PARAMS
    CALL LCD_WR_COM   
    LD D,(IX+5)
    CALL LCD_WR_DAT
    LD D,(IX+4)
    CALL LCD_WR_DAT
    LD D,(IX+1)
    CALL LCD_WR_DAT
    LD D,(IX)
    CALL LCD_WR_DAT       
    LD D,0x2C         ;SEND DATA
    CALL LCD_WR_COM   
    POP DE
    LD HL,4*2       ;REMOVE 4 PARAMS FROM STACK
    ADD HL,SP
    LD SP,HL
    EX DE,HL
    JP (HL)        


;GETS AN 8BIT COLOR INFO 3,3,2 AND RETURNS IT IN 16BIT
;INPUT A OUTPUT HL
DECCOLOR8       LD D,A ;SAVE COLOR8
SPLTRED		AND 11100000B
		LD H,A
		SLA A
		SLA A
		SLA A  	;HERE A IS ZERO AND BIT 5 IS ON CARRY
		JR NC,SPLTGRN		
		LD A,H
		OR 00011000B ;SET R0,R1
SPLTGRN:	CCF ;RED IS OK HERE	ON H
		LD A,D
		AND 00011100B		
		RRA
		RRA
		OR H
		LD H,A	;H HAS THE HIGH BYTE INFO
		RRA
		LD L,11000000B  ;SET G1,G0	
		JR C,SPLTBLU		
		LD L,0		;RESET G1,G0	
SPLTBLU:	CCF  ;GREEN IS OK HERE
		LD A,D
		AND 00000011B
		SLA A
		SLA A
		SLA A
		SLA A
		OR L
		LD L,A
		LD A,D
		RRA
		JR NC,COLDONE
		LD A,L
		OR 00001111
		LD L,A	
COLDONE:	;HL HAS THE COLOR	
		EX DE,HL  ;TEMP SAVE TO RAM
		LD HL,CBHI
		LD (HL),D
		LD HL,CBLO
		LD (HL),E
		EX DE,HL  ;HL HAS THE COLOR
		RET

;GETS A 16 BIT COLOR AND ENCODES IT AS 8 BIT 3,3,2
;INPUT HL COLOR 16 BIT OUTPUT A COLOR 8BIT
ENCOLOR8	LD A,H
		AND 11100000  ;REMOVE GREEN AND R0,R1
		LD D,A	;SAVE TO D
		;RED IS OK HERE
		LD A,H
		AND 00000111  ;REMOVE RED
		CCF
		SLA A
		SLA A
		OR D
		LD D,A	;SAVE TO D
		;GREEN IS OK HERE
		LD A,L  ;GET LOW BYTE
		AND 00111111B  ;REMOVE GREEN
		SLA A
		SLA A
		SLA A
		SLA A
		OR D
		;BLUE IS OK
		RET



MAKERECT8 	PUSH HL
		PUSH DE
		CALL DECCOLOR8	
		POP DE	
		POP HL	
		CALL MAKERECT
		RET


DO8BITCOLOR  	LD DE,30
		LD A,RED8
		LD HL,200		
		CALL MAKERECT8
		LD A,GREEN8
		LD HL,230		
		CALL MAKERECT8
		LD A,BLUE8
		LD HL,260		
		CALL MAKERECT8
		LD A,WHITE8
		LD HL,290		
		CALL MAKERECT8
		LD A,BLACK8
		LD HL,320		
		CALL MAKERECT8
		LD A,MAGENTA8
		LD HL,350		
		CALL MAKERECT8
		LD A,YELLOW8
		LD HL,380		
		CALL MAKERECT8
		LD A,CYAN8
		LD HL,410		
		CALL MAKERECT8
		
		CALL DO8BITLCOLOR  
		RET
    

DO8BITLCOLOR  	LD DE,60
		LD A,LRED8
		LD HL,200		
		CALL MAKERECT8
		LD A,LGREEN8
		LD HL,230		
		CALL MAKERECT8
		LD A,LBLUE8
		LD HL,260		
		CALL MAKERECT8
		LD A,LWHITE8
		LD HL,290		
		CALL MAKERECT8
		LD A,LBLACK8
		LD HL,320		
		CALL MAKERECT8
		LD A,LMAGENTA8
		LD HL,350		
		CALL MAKERECT8
		LD A,LYELLOW8
		LD HL,380		
		CALL MAKERECT8
		LD A,LCYAN8
		LD HL,410		
		CALL MAKERECT8
		RET



    
TESTCOLORH EQU 10000000B
TESTCOLORL EQU 00000000B    

DEVTFT  EQU $10
LCD_RS EQU DEVTFT;  
LCD_CS EQU DEVTFT+1;
CBHI     DB TESTCOLORH  
CBLO     DB TESTCOLORL 






TEST8BIT:	CALL DORED
		CALL DOGREEN
		CALL DOBLUE
		RED


DORED:  	LD HL,CBLO
		LD A,00000000B
		LD (HL),A
		LD HL,CBHI
		LD A,11100000B
		LD (HL),A
		LD HL,30
                LD DE,30			
		CALL MAKERECT
		LD HL,CBHI
		LD A,11000000B
		LD (HL),A
		LD HL,60
		CALL MAKERECT 
		LD HL,CBHI
		LD A,10100000B
		LD (HL),A
		LD HL,90
		CALL MAKERECT   
		LD HL,CBHI
		LD A,10000000B
		LD (HL),A
		LD HL,120
		CALL MAKERECT 
		LD HL,CBHI
		LD A,01100000B
		LD (HL),A
		LD HL,150
		CALL MAKERECT 
		LD HL,CBHI
		LD A,01000000B
		LD (HL),A
		LD HL,180
		CALL MAKERECT 

		RET




DOGREEN         LD HL,CBLO
		LD A,00000000B
		LD (HL),A
		LD HL,CBHI
		LD A,00000111B
		LD (HL),A
		LD HL,30	
                LD DE,60		
		CALL MAKERECT
		LD HL,CBHI
		LD A,00000110B
		LD (HL),A
		LD HL,60
		CALL MAKERECT 
		LD HL,CBHI
		LD A,00000101B
		LD (HL),A
		LD HL,90
		CALL MAKERECT   
		LD HL,CBHI
		LD A,00000100B
		LD (HL),A
		LD HL,120
		CALL MAKERECT 
		LD HL,CBHI
		LD A,00000011B
		LD (HL),A
		LD HL,150
		CALL MAKERECT 
		LD HL,CBHI
		LD A,00000010B
		LD (HL),A
		LD HL,180
		CALL MAKERECT 

		RET

DOBLUE:  	LD HL,CBHI
		LD A,00000000B
		LD (HL),A
		LD HL,CBLO
		LD A,00110000B
		LD (HL),A
		LD HL,30
                LD DE,90			
		CALL MAKERECT
		LD HL,CBLO
		LD A,00100000B
		LD (HL),A
		LD HL,60
		CALL MAKERECT 
		LD HL,CBLO
		LD A,00010000B
		LD (HL),A
		LD HL,90
		CALL MAKERECT   
		LD HL,CBLO
		LD A,00000000B
		LD (HL),A
		LD HL,120
		CALL MAKERECT 
		LD HL,CBLO
		LD A,00000000B
		LD (HL),A
		LD HL,150
		CALL MAKERECT 
		LD HL,CBLO
		LD A,00000000B
		LD (HL),A
		LD HL,180
		CALL MAKERECT 

		RET






MAKERECT	PUSH DE
		LD BC,30
		PUSH HL  ;X1
		PUSH DE  ;Y1		
		ADD HL,BC
		PUSH HL  ;X2
                EX DE,HL
                ADD HL,BC
  		PUSH HL  ;Y2
		CALL LCD_SETXY
		LD BC,30*30   
		
TBAGN:		LD A,(CBHI)
		OUT (LCD_CS),A 
		LD A,(CBLO)
		OUT (LCD_CS),A                
		DEC BC
		LD A,B
		OR C
		JR NZ,TBAGN

		POP DE
		RET
		

