ORG $9000


;INCLUDE \..\MYOS_2_0000.SYM
LCD_WR_COM EQU $0625
LCD_WR_DAT EQU $0621

		LD D,0x35         ;tear on  0X34 TEAR OFF
		CALL LCD_WR_COM     
		LD D,0x00         ;ONLY V-BLNK=0 , V + H =1
		CALL LCD_WR_DAT   
		RET