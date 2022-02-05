ORG $4000

INCLUDE MYOS_2_0000_HARD.SYM

		LCDADDR EQU $27							;$3F
		
		LCD_NOBACKLIGHT EQU 0x00
		LCD_BACKLIGHT EQU 0xFF

;    @abstract   Backlight off constant declaration
;    @discussion Used in combination with the setBacklight to swith off the
; LCD backlight. @set setBacklight
		
		BACKLIGHT_OFF EQU 0
		
; @abstract   Backlight on constant declaration
; @discussion Used in combination with the setBacklight to swith on the
; LCD backlight. @set setBacklight
		
		BACKLIGHT_ON EQU 255

		JP MYSTART
MSG1		DEFM "N:LCD ON, F:LCD OFF"
		DB 10,13,0
MSG2		DEFM "HELLO"
		DB 0
	

MYSTART:	LD HL, MSG1
		CALL RS_TXT

LP:		CALL RS_RX
		CP 'M'
		JP Z, 4
		CP 'N'
		CALL Z, DOON
		CP 'F'
		CALL Z, DOOF
		CP '0'
		CALL Z,ZERO
		CP '1'
		CALL Z,ONE
		CP '2'
		CALL Z,TWO
		JR MYSTART

ZERO:		CALL LCD16_INIT
		RET


ONE:		
		CALL LCD16_CLR
		LD HL,MSG2
		CALL LCD16_PRINT
 		;CALL LCD16_BLINKON
		CALL LCD16_CURSON
		RET


TWO:		;CALL LCD16_BLINKOFF
		CALL LCD16_CURSOFF
		RET

		

DOON:		LD A,(LCD_DSPCTRL)
		OR 128
		LD (LCD_DSPCTRL),A
		CALL LCD16_BKLTON
		RET



DOOF:		CALL LCD16_BKLTOFF
		RET
		