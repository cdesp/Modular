	NAME UTILS
		
	GLOBAL DELAY
	GLOBAL DEL2
	GLOBAL DELAYMICRO
	GLOBAL BCD2BN     ;CONVERT ONE BYTE BCD TO BINARY
	GLOBAL BN2BCD	  ;CONVERT ONE BYTE BINARY  TO BCD
	GLOBAL B2BCD      ;CONVERT 16BIT ON HL TO 6 DIGIT BCD ON C:HL
	GLOBAL BCD2HA	  ;Converts a 6-digit BCD number to a hex ASCII string
	GLOBAL OUTASC	  CONVERTS A 16BIT BINARY NUMBER ON HL TO 6 DIGIT ASCII
	GLOBAL Bcd2ASC    ;Converts a 2-digit BCD number to an ASCII string
	GLOBAL PAUSE_LOOP


@SCCLS:
@SCPRN:

      RET	

;THIS DELAYS 1MICROSECOND ON 8MHZ CPU
DELAYMICROSEC:	PUSH AF
	PUSH DE
  DEC DE
	POP DE
	POP AF
	RET

DELAYMICRO:	
S21:	CALL DELAYMICROSEC
	DJNZ S21	
	RET

;THIS DELAYS 1MILISECOND ON 8MHZ CPU
DELAY:
DELAYMILISEC:	PUSH AF
	PUSH DE
	LD DE,300	;2000 1 MILISEC =300 ON 8MHZ SO 30= 0.1 MILISECS = 100 MICROSECS
DEL1:	DEC DE
	LD A,D            ;TEST FOR DE=00
	OR E
	JR NZ,DEL1 	
	POP DE
	POP AF
	RET

DEL2:LD B,80
DELAYMILI:	
S1:	CALL DELAYMILISEC
	DJNZ S1	
	RET
	
	
; B HAS THE SECONDS TO DELAY
; A,C IS DESTROYED
@DELAYSEC:PUSH BC 
         LD BC,1000 
MYDLAGN: PUSH BC
         CALL DELAY  ;1MS ON 8 MHZ
      	 POP BC
	       DEC BC
         LD A,B
         OR C
         JR NZ,MYDLAGN
         POP BC
         DJNZ DELAYSEC
         RET
	
	
; DE =THE TXT BUFFER	
; HL  =THE NUMBER
OUTASC	PUSH AF
	PUSH DE
	CALL B2Bcd
	POP DE
	CALL BCD2HA
	POP AF
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
	
	
;;--------------------------------------------------
;; Binary to BCD conversion
;;
;; Converts a 16-bit unsigned integer into a 6-digit
;; BCD number. 1181 Tcycles
;;
;; input: HL = unsigned integer to convert
;; output: C:HL = 6-digit BCD number
;; destroys: A,F,B,C,D,E,H,L
;;--------------------------------------------------
B2Bcd:
LD BC, 16*256+0 ; handle 16 bits, one bit per iteration
LD DE, 0
cvtLp:
ADD HL, HL
LD A, E
ADC A, A
DAA
LD E, A
LD A, D
ADC A, A
DAA
LD D, A
LD A, C
ADC A, A
DAA
LD C, A
DJNZ cvtLp
EX DE,HL
RET

;;----------------------------------------------------
;; Converts a 6-digit BCD number to a hex ASCII string
;;
;; input: DE = pointer to start of ASCII string
;; C:HL number to be converted
;; output: DE = pointer past end of ASCII string
;; destroys: A,F,D,E
;;-----------------------------------------------------
Bcd2HA:
LD A, C
CALL cvtUN
LD A, C
CALL cvtLN
LD A, H
CALL cvtUN
LD A, H
CALL cvtLN
LD A, L
CALL cvtUN
LD A, L
JR cvtLN
cvtUN:
RRA ; move upper nibble into lower nibble
RRA
RRA
RRA
cvtLN:
AND  0Fh ; isolate lower nibble
ADD A, 90h ; old trick
DAA ; for converting
ADC A, 40h ; one nibble
DAA ; to hex ASCII
LD (DE), A
INC DE
RET


;;----------------------------------------------------
;; Converts a 2-digit BCD number to a hex ASCII string
;;
;; input: DE = pointer to start of ASCII string
;; L number to be converted
;; output: DE = pointer past end of ASCII string
;; destroys: A,F,D,E
;;-----------------------------------------------------
Bcd2ASC:
LD A, L
CALL cvtUN
LD A, L
JR cvtLN
cvtUN:
RRA ; move upper nibble into lower nibble
RRA
RRA
RRA
cvtLN:
AND  0Fh ; isolate lower nibble
ADD A, 90h ; old trick
DAA ; for converting
ADC A, 40h ; one nibble
DAA ; to hex ASCII
LD (DE), A
INC DE
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


	


	END