;******************************************************************************
;
;                   Copyright (c) 2003, O'ksi'D
;                       All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
;       Redistributions of source code must retain the above copyright
;	notice, this list of conditions and the following disclaimer.
;
;       Redistributions in binary form must reproduce the above copyright
;	notice, this list of conditions and the following disclaimer in the
;	documentation and/or other materials provided with the distribution.
;
;	Neither the name of O'ksi'D nor the names of its contributors
;	may be used to endorse or promote products derived from this software
;	without specific prior written permission.
;
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;******************************************************************************
;                                                                             *
;    Filename: 		slave2dmx.asm                                               *
;    Date: 			30.7.2003                                                 *
;    File Version:  1.0                                                       *
;                                                                             *
;    Author:  Jean-Marc Lienher                                               *
;******************************************************************************
;                                                                             *
;    Files required:         P18F452.INC                                      *
;                                                                             *
;******************************************************************************

	LIST P=18F452		;directive to define processor
	#include <P18F452.INC>	;processor specific variable definitions

;******************************************************************************
;Configuration bits
;
; OSCEN = 1 (Osc switch disabled)
; Oscillator = 101 (EC OSC2 as Port pin)
; BOREN = 1 (BO Reset enabled)
; BO Voltage = 01 ( Vbor 4.2V)
; WDTEN = 0 ( WDog disabled)
; WD-Prescale = 111 (1:128)
; PWRTEN = 1 (PWRT disabled)
; LVP = 0 (LVP diabled, RB5 is I/O pin)
; CCP2MX = 1 (CCP2 mux RC1)
; STVREN = 1 (Stack O/U reset)
; DEBUG = 1 (In-Circuit debbugger disabled)
;

;******************************************************************************
;Macro definitions
;

TestMacro macro
		
		endm

;******************************************************************************
;Variable definitions
; These variables are only needed if low priority interrupts are used. 
; More variables may be needed to store other special function registers used
; in the interrupt routines.

		CBLOCK	0x080 ; block 0x0 high (128 byte)
		WREG_TEMP	;variable used for context saving 
		STATUS_TEMP	;variable used for context saving
		BSR_TEMP	;variable used for context saving
		ENDC

		CBLOCK	0x000 ; access RAM (128 bytes) = block 0x0 low
		; *** common variables ***
		BSR_SAVE	 
		STATUS_SAVE
		WREG_SAVE
		TMP1			; generic tmp
		TMP2			; generic tmp
		ITMP1			; generic tmp in interrupt routine
		ITMP2			; generic tmp in interrupt routine

		; *** Synchronous Serial Port Recieve ***
		COMMAND			; recieved command 
						; 0x4? = set current block
						; 0x1? = write data
						; 0x80 = Reset
		INBYTE			; recieved byte
		CHIP			; chip selected 0 = this, 1 = slave0, 2 = slave1
		BLOCK			; current selected block
		NIBBLE_CURRENT	; current nibble increment counter	
		DATA_BYTE		; tmp memory for the 2 nibbles 

		; *** DMX 512 TX ***
		SND_STATE		; sender status
						; bit	state
						;  0	send byte 256-511
						;  1    byte 511 has been loaded in TX buffer
						;  2    
						;  3	break sequence : TX is/will be low
						;  4	must send the start code
		SND_COUNT		; current sender byte
		BREAK			; count for sender break time
		
		ENDC

		CBLOCK 0x100 ; block 0x1
		; ...
		ENDC 

		CBLOCK 0x200 ; block 0x2
		OUT0
		; ...
		OUT255
		ENDC 

		CBLOCK 0x300 ; block 0x3
		OUT256
		; ...
		OUT511
		ENDC 

		CBLOCK 0x400 ; block 0x4
		; ...
		ENDC 

		CBLOCK 0x500 ; block 0x5
		; ...
		ENDC 

;******************************************************************************
;EEPROM data
; Data to be programmed into the Data EEPROM is defined here

		ORG	0xf00000

;		DE	"Test Data",0,1,2,3,4,5

;******************************************************************************
;Reset vector
; This code will start executing when a reset occurs.

		ORG	0x0000

		goto	Init		;go to start of main code

;******************************************************************************
;High priority interrupt vector
; This code will start executing when a high priority interrupt occurs or
; when any interrupt occurs if interrupt priorities are not enabled.

		ORG	0x0008

		goto	HighInt		;go to high priority interrupt routine

;******************************************************************************
;Low priority interrupt vector and routine
; This code will start executing when a low priority interrupt occurs.
; This code can be removed if low priority interrupts are not used.

		ORG	0x0018

		movff	STATUS,STATUS_TEMP	;save STATUS register
		movff	WREG,WREG_TEMP		;save working register
		movff	BSR,BSR_TEMP		;save BSR register

;	*** low priority interrupt code goes here ***
		; BANKED or ACCESS

		movff	BSR_TEMP,BSR		;restore BSR register
		movff	WREG_TEMP,WREG		;restore working register
		movff	STATUS_TEMP,STATUS	;restore STATUS register
		retfie

;******************************************************************************
;High priority interrupt routine
; The high priority interrupt code is placed here to avoid conflicting with
; the low priority interrupt vector.

HighInt:
	; *** Synchronous serial port interrupt ***

		btfss SSPSTAT, BF, ACCESS	; do we have recieved a byte ?
		goto SspifEnd				; no		

		movff SSPBUF, INBYTE		; read the data

		bcf PIR1, SSPIF, ACCESS		; reset interrupt flag

		btfsc PORTA, 4, ACCESS		; is SS low ?
		goto SspifEnd				; no, we are not selected
		
		movlw 0x80
		cpfseq INBYTE, ACCESS		; reset command ?
		goto ResetCmdEnd			; no
		call ResetDevice
		goto SspifEnd
ResetCmdEnd:

		movff INBYTE, TMP1			; copy data to tmp memory
		movlw 0xF0
		andwf TMP1, 1, ACCESS		; mask command nibble
		
		movlw 0x40
		cpfseq TMP1, ACCESS			; set block command ?
		goto SetBlockCmdEnd			; no
		
		movlw 0x0F
		andwf INBYTE, 0, ACCESS		; mask data
		movff WREG, BLOCK			; save block value

		clrf NIBBLE_CURRENT, ACCESS	; clean counter
		
		goto SspifEnd

SetBlockCmdEnd:

		movlw 0x10
		cpfseq TMP1, ACCESS			; write command
		goto WriteCmdEnd			; no

		btfss NIBBLE_CURRENT, 0, ACCESS	; is byte complete ?
		goto WriteCmda					; no

	; *** low nibble ***
		movlw 0x0F					
		andwf INBYTE, 0, ACCESS		; mask data
		iorwf DATA_BYTE, 1, ACCESS	; OR it with high nibble	

		call WriteFlush				; write it in memory
 		goto WriteCmdb

WriteCmda:
	; *** high nibble ***
		movff INBYTE, DATA_BYTE		; move recieved byte in data buffer
		swapf DATA_BYTE, 1, ACCESS	; swap nibbles
		movlw 0xF0
		andwf DATA_BYTE, 1, ACCESS  ; mask valid data
		
WriteCmdb:
		incf NIBBLE_CURRENT, 1, ACCESS 	; incremment current nibble
		movlw 0x40						; 64
		cpfslt NIBBLE_CURRENT, ACCESS	; last nibble
		clrf NIBBLE_CURRENT, ACCESS		; yes, re-start from 0

WriteCmdEnd:
SspifEnd:

HighIntEnd:

		retfie	FAST				; return with cached registers enabled

;******************************************************************************
;Start of Initialization
;

Init:
	
		call CleanMem

		bsf SND_STATE, 3, ACCESS 	; initilize sender

	
		clrf TRISA, ACCESS			; unused port A as output
		bsf TRISA, 4, ACCESS 		; port A 4 as input
		clrf TRISE, ACCESS			; unused port E as output
		clrf TRISD, ACCESS 			; unused port D as output

		clrf TRISB, ACCESS 			; Port B as output

		bcf INTCON, RBIE, ACCESS 	; disable port B interrupts
		bcf INTCON, INT0IE, ACCESS 	; disable int0 interrupt

		bcf INTCON3, INT2IE, ACCESS ; disable int2 inter
		bcf INTCON3, INT1IE, ACCESS ; disable int1 interrupt 
		
		movlw 0x00
		movff WREG, TRISC			; port C as ouput 
		bcf TRISC, 6, ACCESS		; TX pin as output
		bsf TXSTA, TX9, ACCESS 		; 9 bits sender
		bsf TXSTA, TX9D, ACCESS 	; 9th bit is a '1'
		bcf TXSTA, TXEN, ACCESS 	; tx disabled
		bcf TXSTA, SYNC, ACCESS 	; asynchronous
		bcf TXSTA, BRGH, ACCESS 	; low speed
		movlw 0x1
		movwf SPBRG, ACCESS 		; 250 kBauds at 32 MHz
		bcf PIE1, TXIE, ACCESS 		; TX interrupt disabled
		bcf PIE1, RCIE, ACCESS 		; RX interrupt disabled
		bcf PORTC, 6, ACCESS		; TX default low
		bsf RCSTA, SPEN, ACCESS 	; USART enabled

		bsf TRISC, 4, ACCESS 		; SDI as input
		bsf TRISC, 5, ACCESS 		; SDO as input 
	 	bsf TRISA, 5, ACCESS 		; SS as input 
									; the SS pin is totaly buggy in slave mode
									; don't use it !
		bsf PORTA, 5, ACCESS		; set SS data latch to high
		bsf TRISC, 3, ACCESS 		; SCK as input
		bcf SSPSTAT, SMP, ACCESS 	; input sampled at middle	
		bcf SSPCON1, CKP, ACCESS 	; Idle clock state = low level
		bcf SSPSTAT, CKE, ACCESS 	; data transmited on falling edge
		bcf SSPCON1, WCOL, ACCESS 	; No colision		
		bcf SSPCON1, SSPM3, ACCESS 	; Slave, SS pin disabled
		bsf SSPCON1, SSPM2, ACCESS
		bcf SSPCON1, SSPM1, ACCESS
		bsf SSPCON1, SSPM0, ACCESS
		bsf SSPCON1, SSPEN, ACCESS 	; serial port enabled	
		bsf PIE1, SSPIE, ACCESS 	; Synchonous RX/TX interrupt enabled

		movlw 0x0 					; random delay
		movwf TMR0L, ACCESS			; load timer 0 counter register
		bsf T0CON, TMR0ON, ACCESS 	; timer on
		bsf T0CON, T08BIT, ACCESS 	; 8 bit counter
		bcf T0CON, T0CS, ACCESS 	; internal clock
		bsf T0CON, PSA, ACCESS 		; prescaller off
		bcf T0CON, T0PS2, ACCESS 	; Fosc / 4 ( 0.125 uS)
		bcf T0CON, T0PS1, ACCESS
		bcf T0CON, T0PS0, ACCESS
		bcf INTCON, TMR0IE, ACCESS 	; disable interrupt   

		bcf RCON, IPEN, ACCESS 		; disable interrupts priority
		bsf INTCON, GIE, ACCESS 	; enable all unmasked interrupts 
		bsf INTCON, PEIE, ACCESS 	; enable all unmasked peripheral 
									; interrupts
	

		goto Main
;******************************************************************************
; Sub routines
;


CleanMem:
	; *** clear OUT memory *** 
		lfsr FSR0, 0x200			; destination base address
CleanMema:
		clrf INDF0, ACCESS			; clear memory
		incfsz FSR0L, 1, ACCESS 	; increment indirect pointer
		goto CleanMema

		lfsr FSR0, 0x300			; destination base address
CleanMemb:
		clrf INDF0, ACCESS			; clear memory
		incfsz FSR0L, 1, ACCESS 	; increment indirect pointer
		goto CleanMemb

		return


ResetDevice:
	; *** Reset the device  ***
		clrf NIBBLE_CURRENT, ACCESS
		clrf COMMAND, ACCESS
		call CleanMem
		return

WriteFlush:
	;*** write recieved byte on this chip ***

		lfsr FSR1, 0x200			; destination base address
		btfsc BLOCK, 3, ACCESS  	; high block address ?
		lfsr FSR1, 0x300			; (256-511) base address

		movff NIBBLE_CURRENT, TMP2	; compute byte offset
		rrncf TMP2, 1, ACCESS		; divide per 2
		bcf TMP2, 7, ACCESS			; clear shifted low bit
		movff TMP2, FSR1L			; load o

		btfsc BLOCK, 2, ACCESS		; set block offset
		bsf FSR1L, 7, ACCESS
		btfsc BLOCK, 1, ACCESS
		bsf FSR1L, 6, ACCESS
		btfsc BLOCK, 0, ACCESS
		bsf FSR1L, 5, ACCESS
			
		movff DATA_BYTE, INDF1 		; write data in indirect memory

		return 


;******************************************************************************
;Start of main program

Main:
HiTmr0:
		btfss INTCON, TMR0IF, ACCESS
		bra HiTmr0End

; *** timer0 overflow ***
		movlw 0x62 				; 20 uS (the timer will not be incremented 
								; during 2 instruction cycles + we lost one 
								; cycle during addwf ?)
		addwf TMR0L,1, ACCESS		; add constant to timer0
		bcf INTCON, TMR0IF, ACCESS	; clear interrupt flag

HiTmr0a:
; *** DMX 512 TX break time delay ***
		btfsc SND_STATE, 4, ACCESS	; must we send the start code ?
		goto HiTmr0b				; yes !
	
		btfss SND_STATE, 3, ACCESS	; have all bytes been sent ?
		goto HiTmr0c				; no !

		incf BREAK, 1, ACCESS		; increment break time counter
		movlw 0x3
		cpfslt BREAK, ACCESS 		; leave time (40-60uS) for the last 
									; dimmer bits to be fully sent
		bsf TRISC, 6, ACCESS 		; TX pin as input for 88uS 
							 		;(pull-down by an external resistor)

		cpfslt BREAK, ACCESS 		; (40-60uS) after byte 511
		bcf TXSTA, TXEN, ACCESS 	; disable DMX 512 send
	
		movlw 0x07
		cpfsgt BREAK, ACCESS 		; is break time passed ? 
									; 100uS (88 uS in DMX512 spec)	
		goto HiTmr0c				; no !

	; *** <DEBUG> Uncomment for Debug, it gives a synchro pulse for 
	;     the oscilloscope. 
	;	bsf PORTB, 1, ACCESS
	;	nop
	;	nop
	;	bcf PORTB, 1, ACCESS
	;	nop
	;	nop
	;	nop
	;	bsf PORTB, 1, ACCESS
	; *** </DEBUG> ***
	
		; *** after break time we must set TX to high for at least 8 uS ***
		bsf SND_STATE, 4, ACCESS	; enable send of start code
		bcf TRISC, 6, ACCESS 		; TX pin as output
		bsf TXSTA, TXEN, ACCESS 	; enable UART send
		goto HiTmr0c

HiTmr0b:
		; *** TX was high for 20uS -> send start code ***
		clrf BREAK, ACCESS			; clear break time counter
		clrf SND_STATE, ACCESS		; enable sending bytes
		clrf SND_COUNT, ACCESS		; start from dimmer 0		
		movlw 0x0
		movwf TXREG, ACCESS 		; send start code (0)
		goto HiTmr0c

HiTmr0c:

HiTmr0End:

HiTx:
		btfss PIR1, TXIF, ACCESS 
		bra HiTxEnd

;	*** TX register is empty ***
		btfss SND_STATE, 1, ACCESS 	; should we send data ?
		goto HiTxb					; yes
		
		btfsc SND_STATE, 3, ACCESS 	; are we in break sequence ?
		goto HiTxEnd 				; yes, ignore interrupt (the TXIF
									; flag is not cleared because we
									; don't load data in TXREG). 
	
		bsf  SND_STATE, 3, ACCESS   ; no, but go in break sequence
		clrf BREAK, ACCESS			; sequence at 0
		goto HiTxEnd ; 

HiTxb:
	; *** send dimmer bytes ***
		lfsr FSR2, 0x200			; base address of dimmer 0-255
		btfsc SND_STATE, 0, ACCESS	; high dimmer ?
		lfsr FSR2, 0x300			; yes dimmer 256-511

		movf SND_COUNT, 0, ACCESS 	; read dimmer offset
		addwf FSR2L, 1, ACCESS		; add offset to indirect pointer
		movff INDF2, TXREG			; send DMX 512 data	

		btfsc SND_STATE, 0, ACCESS 	; high dimmer ?
		goto HiTxc					; yes
		infsnz SND_COUNT, 1, ACCESS	; increment dimmer offset
		bsf SND_STATE, 0, ACCESS	; 255->0 => set high dimmer
		goto HiTxEnd

HiTxc:
	; *** high dimmer (256-511) offset increment ***
		incfsz SND_COUNT, 1, ACCESS	; increment dimmer offset
		goto HiTxEnd				; no overflow		
		bsf SND_STATE, 1, ACCESS 	; end of trame (next is break)
		goto HiTxEnd

HiTxEnd:
		goto Main
;******************************************************************************
;End of program

		END
