	NAME UTILS
		
	GLOBAL DELAY
	GLOBAL DEL2
	GLOBAL DELAYMICRO
	GLOBAL B2BCD
	GLOBAL BCD2HA
	GLOBAL OUTASC


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




	


	END