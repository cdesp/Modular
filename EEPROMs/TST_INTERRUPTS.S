ORG $9000

DEVINT  EQU $40 ;Y8 
DEVINP  EQU $18 ;Y3

LCD_MSG EQU 2613
CURSORX EQU 33541
CURSORY EQU 33542
MENU EQU 237

INCLUDE MYOS_2_0000.SYM


LD A,40
LD (CURSORX),A
LD A,20
LD (CURSORY),A
LD HL,MYMSG1
CALL LCD_MSG


AGN:    LD HL,MYMSG1
    CALL RS_TXT
 CALL GET_CHAR	;TAKE A CHAR FROM RS232 OR PS2 KEYB

CP '1'
CALL Z,CLRINT1
CP '2'
CALL Z,CLRINT2
CP '3'
CALL Z,CLRINT3
CP '4'
CALL Z,CLRINT4

CP 'T'
CALL Z,TSTINT
CP 'M'
JP Z,MENU
JR AGN

MYMSG1 DEFM "1,2:CLEAR INT | M=MENU"
    DB 10,13,0


CLRINT1:LD A,0 
        OUT (DEVINT),A
        RET

CLRINT2:LD A,1
        OUT (DEVINT),A
        RET

CLRINT3:LD A,2 
        OUT (DEVINT),A
        RET

CLRINT4:LD A,3
        OUT (DEVINT),A
        RET


TSTINT: IN A,(DEVINP)
        CALL SOUTAS
        IN A,(DEVINP)
        CPL        
        AND 0X07        
        CALL SOUTAS
        RET




