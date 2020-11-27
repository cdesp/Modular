ORG $9000

JP AGN

INCLUDE MYOS_2_0000.SYM


AGN:    LD HL,MS3
    CALL RS_TXT
 CALL GET_CHAR	;TAKE A CHAR FROM RS232 OR PS2 KEYB
CP 'M'
JP Z,MENU
CP 'T'
CALL Z,CHAR_TEST
CP 'P'
CALL Z,TEST2
JR AGN

MS3 DEFM 'T:TEST; M:MENU '
    DEFB 10,13,0


TEST2:  LD A,0
        CALL PS2LOK 
	CALL SOUTAS 
        LD A,0
        CALL PS2LOK 
	CALL SOUTAS 
        LD A,1
        CALL PS2LOK 
	CALL SOUTAS 
        LD A,2
        CALL PS2LOK 
	CALL SOUTAS 
        LD A,3
        CALL PS2LOK
	CALL SOUTAS 
        LD A,4
        CALL PS2LOK
	CALL SOUTAS 

	RET

CHAR_TEST:
      	LD B,10
NXT:	PUSH BC
	CALL READCH
        PUSH AF
        PUSH DE
        CALL SOUTAS          
        POP DE
	LD A,D
        CALL SOUTAS          
        LD A,32
        CALL RS_TX
        POP AF
        CALL PS2LOK2  
        CP 0
        JR Z,NXT2
        CALL RS_TX
	LD A,10
        CALL RS_TX
	LD A,13
        CALL RS_TX
NXT2:	POP BC
	DJNZ NXT
	RET
         
        


READCH:
       CALL GTKEY
       LD D,A
       PUSH DE
       CALL PS2LOK
       CP 0
       POP DE 
       JR Z,READCH

       RET






PS2LOK2	
	LD HL,KEYCTR
	LD B,(HL)
        BIT 6,B			;BIT 6 IS SHIFT
        LD HL,PS2TAB1	;UNSHIFTED
	JR Z,PLK1
	LD HL,PS2TAB2	;SHIFTED
PLK1:   LD B,0
	LD C,A
	ADD HL,BC        
	LD A,(HL)	;GET THE CHAR
	RET

PS2TAB1	DB 0,0,0,0,0,0,0,0,0,0
	DB 0,0,0,0,'`',0,0,0,0,0
	DB 0,'q','1',0,0,0,'z','s','a','w'
	DB '2',0,0,'c','x','d','e','4','3',0
	DB 0,' ','v','f','t','r','5',0,0,'n'	  ;40-49
	DB 'b','h','g','m','6',0,0,44,'m','j'	  ;50-59
	DB 'u','7','8',0,0,',','k','i','o','0'	  ;60-69
	DB '9',0,0,'.','/','l',59,'p','-',0       ;70-79
	DB 0,0,39,0,'[','=',0,0,0,0               ;80-89
	DB 13,']',0,'\',0,0,0,0,0,0               ;90-99
	DB 0,0,6,0,0,8,0,4,9,0                    ;100-109   
	DB 0,0,1,7,10,0,3,11,0,0                  ;110-119 
	DB 0,0,'+','-','*','9',0,0,0,0              ;120-129
	DB 0,0,0,0,0,0,0,0,0,0			  ;130-139
	DB 0,0,0,0,0,0,0,0,0,0                    ;140-149
        DB 0,0,0,0,0,0,0,0,0,0		          ;150-159
	DB 0,0,0,0,0,0,0,0,0,0                    ;160-169


PS2TAB2	DB 0,0,0,0,0,0,0,0,0,0
	DB 0,0,0,0,'~',0,0,0,0,0
	DB 0,'Q','!',0,0,0,'Z','S','A','W'
	DB '@',0,0,'C','X','D','E','$','#',0
	DB 0,' ','V','F','T','R','%',0,0,'N'	  ;40-49
	DB 'B','H','G','Y','^',0,0,'"','M','J'	  ;50-59
	DB 'U','^','*',0,0,'<','K','I','O',')'	  ;60-69
	DB '(',0,0,'>','?','L',':','P','_',0      ;70-79
	DB 0,0,34,0,'{','+',0,0,0,0               ;80-89
	DB 13,'}',0,'|',0,0,0,0,0,0               ;90-99
	DB 0,0,6,0,0,8,0,4,9,0                    ;100-109   
	DB 0,0,1,7,10,0,3,11,0,0                  ;110-119 
	DB 0,0,'+','-','*','9',0,0,0,0              ;120-129
	DB 0,0,0,0,0,0,0,0,0,0			  ;130-139
	DB 0,0,0,0,0,0,0,0,0,0                    ;140-149
	DB 0,0,0,0,0,0,0,0,0,0		          ;150-159
	DB 0,0,0,0,0,0,0,0,0,0                    ;160-169

