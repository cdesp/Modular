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
		CP '1'
		CALL Z,ONE
		CP '2'
		CALL Z,TWO
		CP '3'
		CALL Z,THREE
		CP '4'
		CALL Z,FOUR
		JR MYSTART

;----COPY FROM ISC

		EN EQU 2							;6  // Enable bit
		RW EQU 1							;5  // Read/Write bit
		RS EQU 0							;4  // Register select bit
		D4 EQU 4							;0
		D5 EQU 5							;1
		D6 EQU 6							;2
		D7 EQU 7							;3

;// Define COMMAND and DATA LCD Rs (used by send method).
;// ---------------------------------------------------------------------------
		COMMAND EQU 0
		LCD_DATA EQU 1
		FOUR_BITS EQU 2

		
		LCD_DISPLAYCONTROL EQU 0x08

;// flags for display on/off and cursor control
;// ---------------------------------------------------------------------------
		LCD_DISPLAYON EQU 2;0x04
		LCD_DISPLAYOFF EQU 0x00
		LCD_CURSORON EQU 1;0x02
		LCD_CURSOROFF EQU 0x00
		LCD_BLINKON EQU 0;0x01
		LCD_BLINKOFF EQU 0x00

		BACKLIGHTPIN EQU 3
		BACKLIGHTPINMASK EQU 1.SHL.BACKLIGHTPIN 
		STSMASK DB 0

;A HAS THE VALUE FOR THE 4 BITS
;E HAS THE COMMAND OR DATA SIGNAL E=1 MEANS DATA
LCD16_WR4BITS:	LD D, 0 							;PIN MAP VALUES FRO 8574 BIT 0-7
; Map the value ON A to LCD pin mapping
		RRCA 								;D4 = D0,D5=D1,D6=D2,D7=D3
		JR NC, LWNXT1 							;BIT 0 NOT SET JR
		SET D4, D
LWNXT1:		RRCA
		JR NC, LWNXT2 							;BIT 1 NOT SET JR
		SET D5, D
LWNXT2:		RRCA
		JR NC, LWNXT3 							;BIT 2 NOT SET JR
		SET D6, D
LWNXT3:		RRCA
		JR NC, LWNXT4 							;BIT 3 NOT SET JR
		SET D7, D
LWNXT4:		LD A, E
		RRCA
		JR NC, LWNXT5
		SET RS, D
LWNXT5:										;HERE WE HAVE ALL BITS SET TO DRIVE 8574 PINS
		;ADD THE BACKLIGHT MASKS 
		LD A,(STSMASK)							;8 BIT 3
		OR D
		LD D,A
;WE SEND A PULSE TO WRITE THE 4BITS TO LCD
		SET EN, D 							;EN=HIGH
		LD A, D
		CALL LCD16_I2CSEND
		RES EN, D 							;EN=LOW
		LD A, D
		CALL LCD16_I2CSEND
		RET
		
		

;A HAS THE BYTE TO SEND
LCD16_I2CSEND:	PUSH DE
		PUSH HL
		LD HL, BUFFER
		LD (HL), A
		LD D, LCDADDR
		LD BC, 1
		CALL I2C_WRITE
		POP HL
		POP DE
		RET
		
;A HAS THE COMMAND TO SEND
LCD16_COMMD:	LD E, COMMAND
		JR LCD16_SEND
		
;A HAS THE DATA TO SEND
LCD16_DATA:	LD E, LCD_DATA
		;JP LCD16_SEND
;A HAS THE VALUE FOR THE 4 BITS
;E HAS THE COMMAND OR DATA SIGNAL E=1 MEANS DATA E=0 MEANS COMMAND E=2 IS FOUR_BITS
LCD16_SEND:	LD D, A 							;SAVE A FOR LATER USE
		LD A, E
		RRCA
		JR C, ISCMDDATA
		RRCA
		JR NC, ISCMDDATA
;FOUR BITS
		LD E, COMMAND 							;SEND 4 BITS AS COMMAND E=0
		JR LCSD_4BIT
ISCMDDATA:	LD A, D
		SRL A
		SRL A
		SRL A
		SRL A
		PUSH DE
		CALL LCD16_WR4BITS						;HIGH 4 BITS FIRST
		POP DE
LCSD_4BIT:	LD A, D
		AND $0F
		CALL LCD16_WR4BITS						;LOW 4 BITS NEXT
		RET
		
		
		



LCD16_CURSOFF:	LD A, (LCD_DSPCTRL)
		RES LCD_CURSORON, A
		LD (LCD_DSPCTRL), A
		OR LCD_DISPLAYCONTROL
		CALL LCD16_COMMD
		RET
		
		
LCD16_CURSON:	LD A, (LCD_DSPCTRL)
		SET LCD_CURSORON, A
		LD (LCD_DSPCTRL), A
		OR LCD_DISPLAYCONTROL
		CALL LCD16_COMMD
		RET
		
LCD16_BLINKOFF:	LD A, (LCD_DSPCTRL)
		RES LCD_BLINKON, A
		LD (LCD_DSPCTRL), A
		OR LCD_DISPLAYCONTROL
		CALL LCD16_COMMD
		RET
		
		
LCD16_BLINKON:	LD A, (LCD_DSPCTRL)
		SET LCD_BLINKON, A
		LD (LCD_DSPCTRL), A
		OR LCD_DISPLAYCONTROL
		CALL LCD16_COMMD
		RET

;----------------


ONE:		
 		CALL LCD16_BLINKON
		CALL LCD16_CURSON
		RET


TWO:		CALL LCD16_BLINKOFF
		CALL LCD16_CURSOFF
		RET

THREE:		CALL LCD16_CLR
		LD HL,MSG2
		CALL LCD16_PRINT
		RET

FOUR:		CALL LCD16_SCRLEFT
		CALL LCD16_SCRLEFT
		RET

; void LiquidCrystal_I2C::setBacklight( uint8_t value ) 
; {
;    // Check if backlight is available
;    // ----------------------------------------------------
;    if ( _backlightPinMask != 0x0 )
;    {
;       // Check for polarity to configure mask accordingly
;       // ----------------------------------------------------------
;       if  (((_polarity == POSITIVE) && (value > 0)) || 
;            ((_polarity == NEGATIVE ) && ( value == 0 )))
;       {
;          _backlightStsMask = _backlightPinMask & LCD_BACKLIGHT;
;       }
;       else 
;       {
;          _backlightStsMask = _backlightPinMask & LCD_NOBACKLIGHT;
;       }
;       _i2cio.write( _backlightStsMask );
;    }
; }

;_backlightPinMask = ( 1 << value );

		
		

DOON:		LD A, BACKLIGHTPINMASK AND LCD_BACKLIGHT
		LD (STSMASK),A
		CALL LCD16_I2CSEND
		RET



DOOF:		LD A, BACKLIGHTPINMASK AND LCD_NOBACKLIGHT
		LD HL,STSMASK
		LD (HL),A
		CALL LCD16_I2CSEND
		RET
		