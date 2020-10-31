ORG $9000

DEVI2C  EQU $30 ;Y6    A0=LOW
DEVI2C_S1  EQU $31 ;Y6 A0=HIGH
            ;TEMP

GET_CHAR EQU $02D2;
SOUTAS   EQU $0432
MENU     EQU $0003;
RS_TXT   EQU $08AF;
RS_TX    EQU $08A0;
LCD_MSG  EQU $0ECB
DELAYMICRO EQU $12E1


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


JR AGN

MS3:DEFM "M:MENU, I,A,O,S:I2C INIT-ADJ-NOW-SQOUT"
    DB 10,13,0


;------------------------------------------------------------------------------				 

; Binary to BCD
; Convert one byte of binary data to two bytes of BCD data
; INPUT
; Binary data in A
; OUTPUT
; Hundreds digit in H
; Tens and ones digits in L
; 
; REGISTERS : AF,C,HL
;------------------------------------------------------------------------------				 

BN2BCD: LD H,$FF
D100LP: INC H
	SUB 100
	JR NC,D100LP
	ADD A,100
	LD L,$FF
D10LP:  INC L
        SUB 10
        JR NC, D10LP
        ADD A,10
        LD C,A
        LD A,L
        RLCA
        RLCA        
	RLCA
        RLCA
	OR C
	LD L,A
	RET

;------------------------------------------------------------------------------				 

; BCD to Binary 
; Convert one byte BCD to one byte binary data
; INPUT
; BCD data in A
; OUTPUT
; Binary data in A
; 
; REGISTERS : A,B,C,F
;------------------------------------------------------------------------------				 

BCD2BN: OR A
        LD B,A
	AND $F0
	RRCA
	LD C,A
	RRCA
	RRCA
	ADD A,C
	LD C,A
	LD A,B
	AND $0F
	ADD A,C
	RET


;------------------------------------------------------------------------------				 
; PAUSE_LOOP
;
; Timer function
;
; 16-bit (BC) decrement counter, performing 4xNEG loop until BC
; reaches zero.
;
; 61 T-states in loop = 15.25uS per loop @ 4 MHz - near enough
; a second delay for 65,535 iterations.
;
; Set iteration count in BC before calling this function.
; Destroys: BC
;------------------------------------------------------------------------------
PAUSE_LOOP:
	PUSH	AF							; 11 T-states
pau_lp:
	NEG									; 8 T-states
	NEG									; 8 T-states
	NEG									; 8 T-states
	NEG									; 8 T-states
	DEC		BC							; 6 T-states
	LD		A,C							; 9 T-states
	OR		B							; 4 T-states
	JP		NZ,pau_lp					; 10 T-states
	POP		AF							; 10 T-states
	RET									; Pause complete, RETurn




;=========================
;I2C ROUTINES
;------------------------

S1_BB  EQU 0
S1_LRB  EQU 3
S1_PIN EQU 7



I2C_OS1:
   OUT (DEVI2C_S1),A
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG 
RET


I2C_IS1:   
   IN A,(DEVI2C_S1)
   NEG
   NEG   
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
RET

I2C_OUT:
   OUT (DEVI2C),A
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
RET

I2C_IN:
   IN A,(DEVI2C)
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
   NEG
RET


I2C_INIT:
LD A,$80
CALL I2C_OS1  ;A0=H Loads byte 80H into register S1 i.e. next byte will be loaded into register S0 (own address register); serial interface off.
LD A,$55
CALL I2C_OUT  ;A0=L Loads byte 55H into register S0';effective own address becomes AAH.
LD A,$A0
CALL I2C_OS1  ;A0=H Loads byte A0H into register S1, i.e. next byte will be loaded into the clock control register S2.
LD A,$18 ;$1C=12MHZ  ;18H = 8MHZ ; 14=6MHZ 90KHZ ; 15=6MHZ 45KHZ
CALL I2C_OUT  ;A0=L Loads byte 1CH into register S2;system clock is 12 MHz; SCL = 90 kHz
LD A,$C1
CALL I2C_OS1  ;A0=H ; register enable serial interface, set I2C-bus into idle mode; SDA and SCL are HIGH
LD	BC,FFFFh ;8000H FOR 4MHZ
CALL	PAUSE_LOOP	; 0.5 second delay to synchronise BB-bit
RET


;B=NUMBER OF BYTES TO SEND
;HL=BUFFER ADDRESS OF BYTES
;D=SLAVE ADDRESS OF AN I2C DEVICE

I2C_SEND: CALL I2C_IS1  ;CHECK BUSY
BIT S1_BB,A ;CHECK BB STATUS BIT
JR Z,I2C_SEND
LD A,D
SLA A
CALL I2C_OUT;SET SLAVE ADDRESS
LD A,$C5
CALL I2C_OS1 ;PCF8584 generates 'START' condition.
LP_SEND1:CALL I2C_IS1  ;CHECK PREVIOUS TRANSMISSION ENDED
BIT S1_PIN,A  ;CHECK PIN STATUS BIT
JR NZ,LP_SEND1
BIT S1_LRB,A  ;CHECK LRB STATUS BIT
JR NZ,SENDERR
LD A,(HL)
CALL I2C_OUT ; SEND BYTE TO DEVICE
INC HL
DJNZ LP_SEND1
SND_EOT: LD A,$C3  ;END OF TRANSMISSION
CALL I2C_OS1; PCF8584 generates 'STOP' condition.
XOR A
RET

SENDERR:
LD A,$C3  ;END OF TRANSMISSION
CALL I2C_OS1 ; PCF8584 generates 'STOP' condition.
SCF
RET

;B=NUMBER OF BYTES TO RECEIVE 
;HL=BUFFER ADDRESS OF BYTES + 1
;D=SLAVE ADDRESS OF AN I2C DEVICE
I2C_GET: 
LD A,D
SLA A         ;7 BITS FOR ADDRESS SHIFT LEFT
SET 0,A	      ;SET READ FLAG ON BIT 0=1
CALL I2C_OUT  ;SET SLAVE ADDRESS
LP_GET1: CALL I2C_IS1  ;STATUS REGISTER
BIT S1_BB,A ;CHECK BB STATUS BIT
JR Z,LP_GET1 
LD A,$C5  ;
CALL I2C_OS1 ; PCF8584 generates 'START' condition.
LP_GET2: CALL I2C_IS1  ;STATUS REGISTER
BIT S1_PIN,A  ;CHECK PIN STATUS BIT
JR NZ,LP_GET2
BIT S1_LRB,A  ;CHECK LRB STATUS BIT
JR NZ,RECVERR
CALL I2C_IN
LD (HL),A
INC HL
DJNZ LP_GET2
LD A,$40  ;
CALL I2C_OS1  ; Set ACK bit S1 to 0
CALL I2C_IN ;READ FINAL BYTE AND PUT IT ON S0, Neg. ACK is also sent

LP_GET3: CALL I2C_IS1  ;STATUS REGISTER
BIT S1_PIN,A  ;CHECK PIN STATUS BIT
JR NZ,LP_GET3
GET_EOT: LD A,$C3  ;
CALL I2C_OS1  ;PCF8584 generates'STOP' condition
CALL I2C_IN ; This command transfers the final data byte from the data buffer to accumulator.
	      ; Because the STOP condition was previously executed, no I2C-bus activity takes place. 
LD (HL),A
 XOR A
RET


RECVERR:
LD A,$C3  ;
CALL I2C_OS1 ;PCF8584 generates'STOP' condition
CALL I2C_IN ; This command transfers the final data byte from the data buffer to accumulator.
	      ; Because the STOP condition was previously executed, no I2C-bus activity takes place. 
SCF
RET


;The first read of the S0 register is a ‘dummy read’ of the slave address which should be discarded. The first read of the S0 register simultaneously
;reads the current value of S0 and then transfers the first valid data byte from the I2C-bus to S0.

;-----------------------


;=========================
; DS1307
;-----------------------

DS1307_ADDRESS        EQU  0x68

DS1307_CONTROL_REGISTER EQU 0x07

DS1307_RAM_REGISTER   EQU  0x08
; DS1307 Control register bits.

RTC_DS1307__RS0       EQU  0x00

RTC_DS1307__RS1       EQU  0x01

RTC_DS1307__SQWE      EQU  0x04

RTC_DS1307__OUT       EQU  0x07


SHERR0:
LD HL,MESERR0
CALL RS_TXT
RET


SHERR:
LD A,B
CALL SOUTAS
LD HL,MESERR
CALL RS_TXT
RET


DS1307_GETNOW:

;SEND COMMAND 0

LD HL,MES3
CALL RS_TXT

LD HL,BUFFER
XOR A
LD (HL),A
LD D,DS1307_ADDRESS
LD B,1
CALL I2C_SEND
JP C,SHERR0


LD HL,MES2
CALL RS_TXT

;READ 7 BYTES
LD HL,BUFFER
LD D,DS1307_ADDRESS
LD B,7
CALL I2C_GET
JP C,SHERR
;LD A,$C3  ;END OF TRANSMISSION
;CALL I2C_OS1; PCF8584 generates 'STOP' condition.

RET


DS1307_ADJUST:
      ;Wire.write((byte) 0);

      ;Wire.write(bin2bcd(dt.second()));

      ;Wire.write(bin2bcd(dt.minute()));

      ;Wire.write(bin2bcd(dt.hour()));

      ;Wire.write(bin2bcd(0));

      ;Wire.write(bin2bcd(dt.day()));

      ;Wire.write(bin2bcd(dt.month()));

      ;Wire.write(bin2bcd(dt.year() - 2000));

      ;Wire.write((byte) 0);


;SEND COMMAND ADHUST TIME

LD HL,MES4
CALL RS_TXT

;SET PARAMS
LD DE,BUFFER
XOR A
LD (DE),A
INC DE
LD A,10 ; 10 SECS
CALL BN2BCD
LD A,L
LD (DE),A
INC DE
LD A,29 ; 28 MINS
CALL BN2BCD
LD A,L
LD (DE),A
INC DE
LD A,19 ; 19 HOURS
CALL BN2BCD
LD A,L
LD (DE),A
INC DE
XOR A
LD (DE),A
INC DE
LD A,8 ; 8 DAY
CALL BN2BCD
LD A,L
LD (DE),A
INC DE
LD A,10 ; 10 MONTH
CALL BN2BCD
LD A,L
LD (DE),A
INC DE
LD A,20 ; 20 YEAR (2020)
CALL BN2BCD
LD A,L
LD (DE),A
INC DE
XOR A
LD (DE),A
;END OF 9 PARAMS
LD HL,BUFFER
LD D,DS1307_ADDRESS
LD B,9
CALL I2C_SEND
JP C,SHERR0
LD	BC,FFFFh ;8000H FOR 4MHZ
CALL	PAUSE_LOOP	; 0.5 second delay 
LD	BC,FFFFh ;8000H FOR 4MHZ
CALL	PAUSE_LOOP	; 0.5 second delay 
RET


; D HAS THE FREQUENCY PARAM
DS1307_SETSQ:
LD HL,MES5
CALL RS_TXT
LD HL,BUFFER
PUSH HL
LD A,DS1307_CONTROL_REGISTER
LD (HL),A
INC HL
LD A,D
LD (HL),A
POP HL
LD D,DS1307_ADDRESS
LD B,2
CALL I2C_SEND
JP C,SHERR0

RET
;=======================


;----------------------
; TEST CODE


RST_BUF:
XOR A
LD B,50
LD HL,BUFFER
LP1: LD (HL),A
INC HL
DJNZ LP1
RET

I2CTEST:CALL RST_BUF
LD HL,MES0
CALL RS_TXT
LD B,10 ;10 TIMES EVERY SECOND
MYLP1: PUSH BC
CALL DS1307_GETNOW
LD B,7
CALL BUFBCD2BIN ; CONVERT BUFFER FROM BCD TO BINARY
CALL SHOWOUT
LD	BC,FFFFh ;8000H FOR 4MHZ
CALL	PAUSE_LOOP	; 0.5 second delay 
 LD A,$C3  ;END OF TRANSMISSION
 CALL I2C_OS1; PCF8584 generates 'STOP' condition.
LD	BC,FFFFh ;8000H FOR 4MHZ
CALL	PAUSE_LOOP	; 0.5 second delay 
 LD A,$C3  ;END OF TRANSMISSION
 CALL I2C_OS1; PCF8584 generates 'STOP' condition.
POP BC
DJNZ MYLP1
RET


I2CADJUST:
 CALL DS1307_ADJUST
 LD A,$C3  ;END OF TRANSMISSION
 CALL I2C_OS1; PCF8584 generates 'STOP' condition.

 CALL RST_BUF
RET

BUFBCD2BIN: LD HL,BUFFER+1  	 
LL1:     LD A,(HL)
         PUSH BC  
	 CALL BCD2BN
	 POP BC
	 LD (HL),A
	 INC HL
	 DJNZ LL1 
         RET  


SHOWOUT:
LD HL,MES1
CALL RS_TXT

LD HL,BUFFER
LD B,10
LP2: LD A,(HL)
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

I2CSETSQ:
LD D,16
CALL DS1307_SETSQ
LD A,$C3  ;END OF TRANSMISSION
CALL I2C_OS1; PCF8584 generates 'STOP' condition.

RET

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


BUFFER DS 255