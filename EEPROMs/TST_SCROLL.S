ORG $9000


INCLUDE MYOS_2_0000.SYM
LCD_WR_COM EQU 0X05E1
LCD_WR_DAT EQU 0X05DD
SOUTAS EQU 0X040B
FONT_WIDTH EQU 8
FONT_HEIGHT EQU 10
TOTLINES EQU 480/10

  LD A,10
  LD (CURSORX),A
  LD A,47
  LD (CURSORY),A  
  LD HL,STR0   
  CALL LCD_MSG
  CALL LCD_SETSCROLL

AGAIN:  CALL GET_CHAR
  CP 'S'
  CALL Z,LCD_DOSCROLL
  CP 'E'
  CALL Z,SHOWMSG
  CP 'M'
  JP Z,MENU
  CP 'D'
  CALL Z,DEMO
  CP 'T'
  CALL Z,PRIN47
  JP AGAIN


PRIN47:
  LD A,10
  LD (CURSORX),A
  LD A,47
  LD (CURSORY),A  
  LD HL,STR0   
  CALL LCD_MSG
  RET

DEMO: CALL LCD_CLRSCR
      LD B,48
DM_AGN:PUSH BC
       LD A,48
       SUB B
       LD (CURSORX),A
       CALL SHOWMSG
       CALL LCD_DOSCROLL
       POP BC
       DJNZ DM_AGN
       RET


SHOWMSG:
  LD A,0
  LD (CURSORY),A   
  PUSH AF
  LD A,(SCRLN)
  CALL SOUTAS
  CALL LCD_CALCLINE
  LD (CURSORY),A   
  CALL SOUTAS
  LD HL,STR1
  CALL LCD_MSG  
  POP AF
  LD (CURSORY),A   
  RET

SCRLN DB 0;

STR0 DEFM "SCROLLING TEST.. S=SCROLL,E=TYPE MSG,M=MENU"
     DEFB 0
STR1 DEFM "TEST MESSAGE"
     DEFB 0

;CALCULATE VLINE
LCD_CALCLINE:   LD A,(SCRLN)
                LD D,A 
                LD A,(CURSORY)  ;480/8 MAKES 60 LINES                
                ADD A,D
                CP TOTLINES
                JR C,CL_EXIT
                ;WE ARE OVER 60 LINES
                SUB TOTLINES
CL_EXIT:        RET             
  


;SETS HORIZONTAL SCROLL (LANDSCAPE MODE) 480 LINES TO SCROLL
LCD_SETSCROLL:  LD D,0x33
                CALL LCD_WR_COM
		LD D,0X00 ;TOP FIXED NO OF LINES 2 BYTES HIGH (0)
                CALL LCD_WR_DAT
		LD D,0X00 ;TOP FIXED NO OF LINES 2 BYTES LOW
                CALL LCD_WR_DAT
		LD D,0X01 ;SCROLL LINES NO OF LINES 2 BYTES HIGH (480)
                CALL LCD_WR_DAT
		LD D,0XE0 ;SCROLL LINES NO OF LINES 2 BYTES LOW
                CALL LCD_WR_DAT
		LD D,0X00 ;BOTTOM FIXED NO OF LINES 2 BYTES HIGH (0)
                CALL LCD_WR_DAT
		LD D,0X00 ;BOTTOM FIXED NO OF LINES 2 BYTES LOW
                CALL LCD_WR_DAT
                XOR A
                LD (SCRLN),A
		RET

;SCROLLS SCRLN*8 LINES 
LCD_DOSCROLL:   LD A,(SCRLN)
                INC A
                CP TOTLINES
                JR C, DS_NXT
                SUB TOTLINES
DS_NXT:         LD (SCRLN),A
                LD L,A
                LD H,0
                ADD HL,HL ;*10=FONT_HEIGHT
                ADD HL,HL
                ADD HL,HL  
                LD E,A
                LD D,0
                ADD HL,DE
                ADD HL,DE
                LD D,0x37
                CALL LCD_WR_COM
		LD D,H ;NEW TOP SCROLL LINE BYTES HIGH (1-480) 
                CALL LCD_WR_DAT
		LD D,L ;NEW TOP SCROLL LINE BYTES LOW
                CALL LCD_WR_DAT
                RET

