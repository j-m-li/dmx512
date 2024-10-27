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
;    Filename: 		spp2dmx.asm                                               *
;    Date: 			29.7.2003                                                 *
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

		; *** Parallel Port Recieve/Transmit ***
		COMMAND			; recieved command 
						; bit	command
						;  0	write block
						;  1	read block
						;  2	set current block
						;  3	read status
		INBYTE			; recieved byte
		CHIP			; chip selected 0 = this, 1 = slave0, 2 = slave1
		BLOCK			; current selected block
		NIBBLE_COUNT 	; 1/2 byte (nibble)reciever decrement counter
		NIBBLE_CURRENT	; current nibble increment counter

		; *** Parallel Port Recieve ***	
		DATA_BYTE		; tmp memory for the 2 nibbles 

		; *** Parallel Port Transmit / DMX 512 RX ***
		STATUS0			; input block status low
		STATUS1			; input block status high
		CLR_STAT		; if 1 must clear status during next interrupt

		; *** DMX 512 RX ***
		REC_STATE		; reciever status
						; bit	state
						;  0	recieve byte 256-511
						;  1	valid start byte recieved
						;  2	ignore recieved bytes
		REC_COUNT		; current recieved byte
		REC_TIME0		; 0-5mS reset timer
		REC_TIME1		; 0-1S reset timer

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
		IN0
		; ...
		IN255
		ENDC 

		CBLOCK 0x500 ; block 0x5
		IN256
		; ...
		IN511
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
	; *** reset timer for DMX RX no activity of more than 1S ***
		incf REC_TIME0, 1, ACCESS	; increment 20uS resloution timer
		movlw 0xFA					; 250
		cpfseq REC_TIME0, ACCESS	; low timer = 250 ?
		goto HiTmr0d				; no
		
		clrf REC_TIME0, ACCESS		; low timer = 0
		incf REC_TIME1, 1, ACCESS	; increment 5mS resolution timer
		movlw 0xC8					; 200
		cpfsgt REC_TIME1, ACCESS	; more than 1S ?		
		goto HiTmr0d				; no !

		clrf REC_TIME1, ACCESS		; high timer = 0
		call ResetDmxIn				; DMX reset !
		movlw 0xff
		movff WREG, STATUS0			; all input have changed
		movff WREG, STATUS1			; all input have changed

HiTmr0d:

HiTmr0End:

HiRx:
		btfss PIR1, RCIF, ACCESS 
		bra HiRxEnd

; *** DMX 512 RX data ***

		btfsc CLR_STAT, 0, ACCESS	; need clearing status
		clrf STATUS0, ACCESS		; yes
		btfsc CLR_STAT, 0, ACCESS	; need clearing status
		clrf STATUS1, ACCESS		; yes
		clrf CLR_STAT, ACCESS		; re-enable clean trigger

		btfsc RCSTA, FERR, ACCESS 	; recieve error ?
		goto HiRxg					; yes
		btfss RCSTA, RX9D, ACCESS 	; stop bit recieve error ?
		goto HiRxg					; yes
		
		bcf REC_STATE, 3, ACCESS	; clear invalid byte recieved

		movff RCREG, ITMP1			; read DMX 512 data

		clrf REC_TIME0, ACCESS		; clear reset timer
		clrf REC_TIME1, ACCESS		;

		btfsc REC_STATE, 2, ACCESS  ; should we read the data ?
		goto HiRxEnd				; no !
		
		btfss REC_STATE, 1, ACCESS	; do we have recieved the start code ?
		goto HiRxf					; no !


	; *** valid data byte recieved ***
		lfsr FSR2, 0x400			; base address 0-255
		btfsc REC_STATE, 0, ACCESS	; high dimmer ?
		lfsr FSR2, 0x500			; yes, base address 256-511

		movf REC_COUNT, 0, ACCESS 	; load offset
		addwf FSR2L, 1, ACCESS		; add offset to indirect pointer
		movff INDF2, ITMP2 			; read memory
		movff ITMP1, INDF2			; copy recieved data to memory

		movf ITMP1, 0, ACCESS		; load data in working register	
		cpfseq ITMP2, ACCESS		; is recieved data equal to previous ?
		goto HiRxa					; no !
		goto HiRxd					; yes

HiRxa:
	; *** dimmer value has change => set status bit ***
		movlw 0xE0
		andwf REC_COUNT, 0, ACCESS 	; get the 3 high bits of the offset
		movwf ITMP1, ACCESS			; put the result in tmp memory
		rrncf ITMP1, 1, ACCESS		; divide it per 2
		swapf ITMP1, 1, ACCESS		; swap low and high nibble (divide per 16)
									; 0 >= ITMP1 && ITMP1 < 8
		movlw 0x01
		movwf ITMP2, ACCESS 		; prepare a bit mask
		movlw 0x0					; put 0 in the working register

HiRxb:
		; *** loop to find the bit mask position ***
		cpfsgt ITMP1, ACCESS		; is data greater than 0 ?
		goto HiRxc					; no, we found it
		decf ITMP1, 1, ACCESS		; decrement tmp
		rlncf ITMP2, 1, ACCESS 		; shift bit mask
		goto HiRxb
		
HiRxc:
		; *** update status byte ***
		movff ITMP2, WREG			; load bit mask in working register
		btfsc REC_STATE, 0, ACCESS 	; is it a 256-511 dimmer ?
		iorwf STATUS1, 1, ACCESS	; OR the high status byte with bit mask
		btfss REC_STATE, 0, ACCESS	; is it a 0-255 dimmer ?
		iorwf STATUS0, 1, ACCESS	; OR the low status byte with bit mask

HiRxd:
		; *** increment dimmer counter ***
		btfsc REC_STATE, 0, ACCESS	; are we in 256-511 dimmer ? 
		goto HiRxe					; yes
		infsnz REC_COUNT, 1, ACCESS	; increment dimmer reciever count
		bsf REC_STATE, 0, ACCESS	; overflow => set high dimmer bit
		goto HiRxEnd

HiRxe:
		; *** increment dimmer counter 256-511 ***
		incfsz REC_COUNT, 1, ACCESS	; increment dimmer reciever count
		goto HiRxEnd				; no overflow
		bsf REC_STATE, 2, ACCESS	; this is the end, ignore next bytes
		goto HiRxEnd

HiRxf:
	; *** get the DMX 512 start code ***
		bsf REC_STATE, 1, ACCESS 	; recieve data
		movlw 0x0
		cpfseq ITMP1, ACCESS		; is start code equal zero ?
		bsf REC_STATE, 2, ACCESS 	; no, ignore next data
		goto HiRxEnd
HiRxg:
		; *** recieve Error, it's a break synchronize ***
		clrf REC_STATE, ACCESS		; enable byte recieving
		clrf REC_COUNT, ACCESS		; start from byte 0
		movff RCREG, ITMP1			; read bad DMX 512 data

HiRxEnd:

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

		goto HighIntEnd	; skip all other interrupts ...
				
HiSsp:
		btfss PIR1, SSPIF, ACCESS 
		bra HiSspEnd

;	*** TX/RX in synchonous serial port ***
		;movf SSPBUF, W, ACCESS 	; read data

		;movwf SSPBUF, ACCESS 		; send data
		;bcf SSPCON1, WCOL, ACCESS 	; clear colision flag
HiSspEnd:

HiInt0:
		btfss INTCON, INT0IF, ACCESS
		bra HiInt0End
HiInt0End:

HiRb:
		btfss INTCON, RBIF, ACCESS
		bra HiRbEnd
HiRbEnd:

HiInt2:
		btfss INTCON3, INT2IF, ACCESS
		bra HiInt2End
HiInt2End:

HiInt1:
		btfss INTCON3, INT1IF, ACCESS
		bra HiInt1End
HiInt1End:


HiPsp:
		btfss PIR1, PSPIF, ACCESS
		bra HiPspEnd
HiPspEnd:

HiAd:
		btfss PIR1, ADIF, ACCESS
		bra HiAdEnd
HiAdEnd:
 
HiCcp1:
		btfss PIR1, CCP1IF, ACCESS
		bra HiCcp1End
HiCcp1End:

HiTmr2:
		btfss PIR1, TMR2IF, ACCESS
		bra HiTmr2End
HiTmr2End:

HiTmr1:
		btfss PIR1, TMR1IF, ACCESS
		bra HiTmr1End
HiTmr1End:

HiEe:
		btfss PIR2, EEIF, ACCESS
		bra HiEeEnd
HiEeEnd:

HiBcl:
		btfss PIR2, BCLIF, ACCESS
		bra HiBclEnd
HiBclEnd:

HiLvd:
		btfss PIR2, LVDIF, ACCESS
		bra HiLvdEnd
HiLvdEnd:

HiTmr3:
		btfss PIR2, TMR3IF, ACCESS
		bra HiTmr3End
HiTmr3End:

HiCcp2:
		btfss PIR2, CCP2IF, ACCESS
		bra HiCcp2End
HiCcp2End:

HighIntEnd:

		retfie	FAST				; return with cached registers enabled

;******************************************************************************
;Start of Initialization
;

Init:
	
		call CleanMem

		bsf SND_STATE, 3, ACCESS 	; initilize sender
		clrf REC_STATE, ACCESS		; clear reciever state
		bsf REC_STATE, 2, ACCESS	; don't recieve DMX 512 data
		clrf REC_TIME0, ACCESS		; clear reset timer
		clrf REC_TIME1, ACCESS		;
	
		clrf TRISA, ACCESS			; unused port A as output
		clrf TRISE, ACCESS			; unused port E as output

		movlw 0xFF
		movwf TRISD, ACCESS 		; Port D as input

		movlw 0x0
		movwf TRISB, ACCESS 		; Port B as output

		bcf INTCON, RBIE, ACCESS 	; disable port B interrupts
		bcf INTCON, INT0IE, ACCESS 	; disable int0 interrupt

		bcf INTCON3, INT2IE, ACCESS ; disable int2 inter
		bcf INTCON3, INT1IE, ACCESS ; disable int1 interrupt 
		
		movlw 0x80
		movff WREG, TRISC			; port C as ouput except RX pin
		bcf TRISC, 6, ACCESS		; TX pin as output
		bsf TRISC, 7, ACCESS		; RX pin as input
		bsf TXSTA, TX9, ACCESS 		; 9 bits sender
		bsf TXSTA, TX9D, ACCESS 	; 9th bit is a '1'
		;bsf TXSTA, TXEN, ACCESS 	; tx enabled
		bcf TXSTA, TXEN, ACCESS 	; tx disabled
		bcf TXSTA, SYNC, ACCESS 	; asynchronous
		bcf TXSTA, BRGH, ACCESS 	; low speed
		movlw 0x1
		movwf SPBRG, ACCESS 		; 250 kBauds at 32 MHz
		bsf RCSTA, RX9, ACCESS 		; 9 bits reciever
		bsf RCSTA, CREN, ACCESS 	; reciever enabled
		bsf PIE1, TXIE, ACCESS 		; TX interrupt enabled
		bsf PIE1, RCIE, ACCESS 		; RX interrupt enabled
		bcf PORTC, 6, ACCESS		; TX default low
		bsf RCSTA, SPEN, ACCESS 	; USART enabled

		bcf TRISC, 4, ACCESS 		; SDI as output (unused)
		bcf TRISC, 5, ACCESS 		; SDO as output
		bcf TRISC, 3, ACCESS 		; SCK as output
		; bsf TRISA, 5, ACCESS 		; SS as input
									; the SS pin is totaly buggy in slave mode
									; don't use it !
		; bsf TRISC, 3, ACCESS 		; SCK as input
		bcf SSPSTAT, SMP, ACCESS 	; input sampled at middle	
		bcf SSPCON1, CKP, ACCESS 	; Idle clock state = low level
		bcf SSPSTAT, CKE, ACCESS 	; data transmited on falling edge
		bcf SSPCON1, WCOL, ACCESS 	; No colision
		bsf SSPCON1, SSPEN, ACCESS 	; serial port enabled
		bcf SSPCON1, SSPM3, ACCESS 	; Master clock = Fosc / 4
		bcf SSPCON1, SSPM2, ACCESS
		bcf SSPCON1, SSPM1, ACCESS
		bcf SSPCON1, SSPM0, ACCESS
		; bsf SSPCON1, SSPM2, ACCESS ; Slave mode SS enabled
		bsf PIR1, SSPIF, ACCESS		; set ssp iterrupt flag
		bcf PIE1, SSPIE, ACCESS 	; Synchonous RX/TX interrupt disabled

		movlw 0x0 					; random delay
		movwf TMR0L, ACCESS			; load timer 0 counter register
		bsf T0CON, TMR0ON, ACCESS 	; timer on
		bsf T0CON, T08BIT, ACCESS 	; 8 bit counter
		bcf T0CON, T0CS, ACCESS 	; internal clock
		bsf T0CON, PSA, ACCESS 		; prescaller off
		bcf T0CON, T0PS2, ACCESS 	; Fosc / 4 ( 0.125 uS)
		bcf T0CON, T0PS1, ACCESS
		bcf T0CON, T0PS0, ACCESS
		bsf INTCON, TMR0IE, ACCESS 	; enable interrupt   

		bcf RCON, IPEN, ACCESS 		; disable interrupts priority
		bsf INTCON, GIE, ACCESS 	; enable all unmasked interrupts 
		;bsf INTCON, PEIE, ACCESS 	; (don't forget to clear PIR1!) enable 
									; all unmasked peripheral interrupts
	

		goto Main
;******************************************************************************
; Sub routines
;

WaitForSspif:
	; *** wait for ssp to complete transfere ***
		btfss PIR1, SSPIF, ACCESS
		goto WaitForSspif
		bcf PIR1, SSPIF, ACCESS		; clear flag to enable next detection
		return
	
		
Feedback:
		;*** set data to port B according to low nibble of INBYTE ***
		bcf PORTB, 7, ACCESS
		btfsc INBYTE, 3, ACCESS
		bsf PORTB, 7, ACCESS
		bcf PORTB, 6, ACCESS	
		btfsc INBYTE, 2, ACCESS
		bsf PORTB, 6, ACCESS		
		bcf PORTB, 5, ACCESS	
		btfsc INBYTE, 1, ACCESS
		bsf PORTB, 5, ACCESS
		bcf PORTB, 4, ACCESS	
		btfsc INBYTE, 0, ACCESS
		bsf PORTB, 4, ACCESS

		return

ResetDmxIn:
	; *** DMX RESET : clear DMX IN memory *** 
		lfsr FSR2, 0x400			; destination base address
ResetDmxIna:
		clrf INDF2, ACCESS			; clear memory
		incfsz FSR2L, 1, ACCESS 	; increment indirect pointer
		goto ResetDmxIna

		lfsr FSR2, 0x500			; destination base address
ResetDmxInb:
		clrf INDF2, ACCESS			; clear memory
		incfsz FSR2L, 1, ACCESS 	; increment indirect pointer
		goto ResetDmxInb

		return

CleanMem:
	; *** clear IN and OUT memory *** 
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

		lfsr FSR0, 0x400			; destination base address
CleanMemc:
		clrf INDF0, ACCESS			; clear memory
		incfsz FSR0L, 1, ACCESS 	; increment indirect pointer
		goto CleanMemc

		lfsr FSR0, 0x500			; destination base address
CleanMemd:
		clrf INDF0, ACCESS			; clear memory
		incfsz FSR0L, 1, ACCESS 	; increment indirect pointer
		goto CleanMemd	

		return


ResetDevice:
	; *** Reset the device and slaves ***
		bcf PORTB, 1, ACCESS		; select slave1
		bsf PORTB, 2, ACCESS		;
		movlw 0x80					; reset command
		movwf SSPBUF, ACCESS 		; send reset command

		clrf NIBBLE_COUNT, ACCESS
		clrf NIBBLE_CURRENT, ACCESS
		clrf COMMAND, ACCESS
		clrf CLR_STAT, ACCESS

		call CleanMem

		bcf PORTB, 2, ACCESS		; select slave0
		bsf PORTB, 1, ACCESS		;
		movlw 0x80					; reset command 
		movwf SSPBUF, ACCESS 		; send reset command
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		bsf PIR1, SSPIF, ACCESS		; set ssp iterrupt flag
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


SendNibble:
	; *** send data nibble to slave chip ***
		btfss CHIP, 0, ACCESS		; slave0 ?
		goto SendNibblea			; no
		bcf PORTB, 1, ACCESS		; select slave 0
		bsf PORTB, 2, ACCESS
		goto SendNibbleb
SendNibblea:
		; *** select salve1 ***
		bcf PORTB, 2, ACCESS
		bsf PORTB, 1, ACCESS
SendNibbleb:
		; *** send data to slave with Synchonous serial port ***
		call WaitForSspif			; wait for transfere complete
		movlw 0x10					; write command
		iorwf INBYTE, 0, ACCESS		; OR command with data
		movwf SSPBUF, ACCESS 		; send nibble	

		return

SendBlock:
	; *** send block to slave chip *** 
		btfss CHIP, 0, ACCESS		; slave0 ?
		goto SendBlocka				; no
		bcf PORTB, 1, ACCESS		; select slave 0
		bsf PORTB, 2, ACCESS
		goto SendBlockb
SendBlocka:
		; *** select slave1 ***
		bcf PORTB, 2, ACCESS
		bsf PORTB, 1, ACCESS
SendBlockb:
		; *** send current block using the SSP ***
		call WaitForSspif			; wait for transfere complete
		movlw 0x40					; set current block 
		iorwf BLOCK, 0, ACCESS
		movwf SSPBUF, ACCESS 		; send set current block command

		return

GetNibble:
	; *** process the recieved nibble according to the current COMMAND ***
		movff	STATUS,STATUS_SAVE	;save STATUS register
		movff	WREG,WREG_SAVE		;save working register
		movff	BSR,BSR_SAVE		;save BSR register

		decf NIBBLE_COUNT, 1, ACCESS

GetNibbleWrite:
		btfss COMMAND, 0, ACCESS	; write command ?
		goto GetNibbleWriteEnd		; no !

	; *** write block ***
		call Feedback				; copy INBYTE to output

GetNibbleWritea:
		movlw 0x0f
		andwf INBYTE, 1, ACCESS  	; mask data

		movlw 0x0
		cpfseq CHIP, ACCESS			; is this the master chip ?
		call SendNibble				; no, send nibble to slave chip

		movlw 0x0
		cpfseq CHIP, ACCESS			; is this the master chip ?
		goto GetNibbleWriteEnd		; if slave chip then the job is done
		
		movlw 0x0
		cpfsgt NIBBLE_COUNT, ACCESS	; is this the last nibble
		goto GetNibbleWriteEnd		; yes, ignore last dummy nibble 
		
		btfss NIBBLE_CURRENT, 0, ACCESS	; is this the low nibble ?
		goto GetNibbleWriteb		; no !

	;*** low nibble ***
		movf INBYTE, 0, ACCESS 		; load low nibble
		iorwf DATA_BYTE, 1, ACCESS	; OR high and low nibble
		call WriteFlush				; write byte in memory
	
		goto GetNibbleWriteEnd

GetNibbleWriteb:
	;*** high nibble ***
		swapf INBYTE, 1, ACCESS 	; swap nibble
		movff INBYTE, DATA_BYTE 	; store it

GetNibbleWriteEnd:

GetNibbleRead:
		btfss COMMAND, 1, ACCESS
		goto GetNibbleReadEnd

	; *** read block ***
		lfsr FSR0, 0x400 			; indirect base address
		btfsc BLOCK, 3, ACCESS		; is it a high dimmer ?
		lfsr FSR0, 0x500 			; yes, (256-511) indirect base address
	
		; *** compute dimmer offset ***
		rrncf NIBBLE_CURRENT, 0, ACCESS ; dimmer offset, divide per 2
		movff WREG, FSR0L			; load it in indirect register pointer
		bcf FSR0L, 7, ACCESS		; clear shifted low bit
		btfsc BLOCK, 2, ACCESS		; set block offset
		bsf FSR0L, 7, ACCESS
		btfsc BLOCK, 1, ACCESS
		bsf FSR0L, 6, ACCESS
		btfsc BLOCK, 0, ACCESS
		bsf FSR0L, 5, ACCESS

		movff INDF0, TMP1			; read indirect data
		
		btfsc NIBBLE_CURRENT, 0, ACCESS	; is it a low nibble ?
		swapf TMP1, 1, ACCESS		; yes, swap it

		bcf PORTB, 7, ACCESS		; set data to port
		btfsc TMP1, 7, ACCESS
		bsf PORTB, 7, ACCESS
		bcf PORTB, 6, ACCESS	
		btfsc TMP1, 6, ACCESS
		bsf PORTB, 6, ACCESS		
		bcf PORTB, 5, ACCESS	
		btfsc TMP1, 5, ACCESS
		bsf PORTB, 5, ACCESS
		bcf PORTB, 4, ACCESS	
		btfsc TMP1, 4, ACCESS
		bsf PORTB, 4, ACCESS
		nop							; wait a while
		nop
		nop
		nop

GetNibbleReadEnd:


GetNibbleBlock:
		btfss COMMAND, 2, ACCESS
		goto GetNibbleBlockEnd

	; *** set current block ***
		movlw 0x0
		cpfseq NIBBLE_CURRENT, ACCESS	; is this nibble 0 ?
		goto GetNibbleBlockEnd				; no, ignore it !
		
		call Feedback					; copy INBYTE to output
		
		clrf BLOCK, ACCESS				; clear block
		btfsc INBYTE, 3, ACCESS			; set block bit according to INBYTE
		bsf BLOCK, 3, ACCESS
		btfsc INBYTE, 2, ACCESS
		bsf BLOCK, 2, ACCESS
		btfsc INBYTE, 1, ACCESS
		bsf BLOCK, 1, ACCESS
		btfsc INBYTE, 0, ACCESS
		bsf BLOCK, 0, ACCESS

		movlw 0x0
		cpfseq CHIP, ACCESS				; is this a slave chip ?
		call SendBlock					; yes, send it

GetNibbleBlockEnd:


GetNibbleStatus:
		btfss COMMAND, 3, ACCESS
		goto GetNibbleEnd

	; *** read status ***
		movff STATUS0, TMP1				; move low status to tmp memory
		btfsc NIBBLE_CURRENT, 1, ACCESS ; is this the high status ?
		movff STATUS1, TMP1				; yes, move it to tmp

		movlw 0x3
		cpfslt NIBBLE_CURRENT, ACCESS 	; last nibble ?
		bsf CLR_STAT, 0, ACCESS 		; yes, enable clean status!

		btfsc NIBBLE_CURRENT, 0, ACCESS ; low or high nibble ?
		swapf TMP1, 1, ACCESS 			; low, swap it !

		bcf PORTB, 7, ACCESS		; set data to port according to high 							
		btfsc TMP1, 7, ACCESS		; nibble of TMP1
		bsf PORTB, 7, ACCESS
		bcf PORTB, 6, ACCESS	
		btfsc TMP1, 6, ACCESS
		bsf PORTB, 6, ACCESS		
		bcf PORTB, 5, ACCESS	
		btfsc TMP1, 5, ACCESS
		bsf PORTB, 5, ACCESS
		bcf PORTB, 4, ACCESS	
		btfsc TMP1, 4, ACCESS
		bsf PORTB, 4, ACCESS

GetNibbleStatusEnd:
	
GetNibbleEnd:
		incf NIBBLE_CURRENT, 1, ACCESS	; increment current nibble
		movff	BSR_SAVE,BSR		;restore BSR register
		movff	WREG_SAVE,WREG		;restore working register
		movff	STATUS_SAVE,STATUS	;restore STATUS register	
		return

;******************************************************************************
;Start of main program

Main:								; D4 = 1, IN3 = 1
		btfsc PORTD, 4, ACCESS		; is D4 low ?
		goto Main					; no !
		nop							; debounce, wait a while.
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		movff PORTD, INBYTE 		; read port B in INBYTE
		btfsc INBYTE, 4, ACCESS		; be sure that D4 is still low
		goto Main					; NO ! try again !
		
		movlw 0x60
		cpfseq INBYTE, ACCESS		; is it command 0x60 (Reset) ?
		goto NormalByte				; No.

;	*** Reset  ***
		bcf PORTB, 7, ACCESS		; clear port B 
		bcf PORTB, 6, ACCESS
		bcf PORTB, 5, ACCESS
		bcf PORTB, 4, ACCESS
		bcf PORTB, 3, ACCESS 		; strobe

ResetCmd1:							; D4=0, IN3=0 
		btfss PORTD, 4, ACCESS		; is D4 high ?
		goto ResetCmd1				; no
		nop							; debounce
		nop
		nop
		nop
		movff PORTD, INBYTE			; read data
		btfss INBYTE, 4, ACCESS		; is D4 still high ?
		goto ResetCmd1				; NO, try again !

		bsf PORTB, 3, ACCESS 		; strobe
		call ResetDevice			; Reset !
		goto Main		
				
NormalByte:							;D4 = 0 IN3 = 1
		btfsc INBYTE, 7, ACCESS		; is this a command byte ?
		goto CommandByte			; Yes !

; *** Data byte **
		call GetNibble				; get nibble according to COMMAND
		bcf PORTB, 3, ACCESS 		; strobe
		goto Nibble2				; go read next nibble

CommandByte: ; D4 = 0 IN3 = 1
; *** Command Byte ***	
		clrf NIBBLE_CURRENT, ACCESS	; clear nibble count

		clrf CHIP, ACCESS			; clear chip select
		btfsc INBYTE, 6, ACCESS		; set chip
		bsf CHIP, 1, ACCESS
		btfsc INBYTE, 5, ACCESS
		bsf CHIP, 0, ACCESS

		movlw 0x0f
		andwf INBYTE, 1, ACCESS		; mask low bits of INBYTE
		
		clrf COMMAND, ACCESS		; clear command

Cmd0:
		movlw 0x0
		cpfseq INBYTE, ACCESS		; command 0000 ?
		goto Cmd1
	; ** No Operation ***
		movlw 0x00
		movwf NIBBLE_COUNT, ACCESS	; no nibble will come	 
		goto CmdEnd

Cmd1:
		movlw 0x01
		cpfseq INBYTE, ACCESS		; command 0001 ?
		goto Cmd2
	; *** write ***
		bsf COMMAND, 0, ACCESS
		movlw 0x41
		movwf NIBBLE_COUNT, ACCESS	; 65 nibble will folow
		goto CmdEnd

Cmd2:
		movlw 0x02
		cpfseq INBYTE, ACCESS		; command 0010 ?
		goto Cmd3
	; *** read ***
		bsf COMMAND, 1, ACCESS
		movlw 0x41
		movwf NIBBLE_COUNT, ACCESS	; 65 nibble will folow
		goto CmdEnd
Cmd3:
		movlw 0x03
		cpfseq INBYTE, ACCESS		; command 0011
		goto Cmd4
	; *** read / write ***
		bsf COMMAND, 1, ACCESS
		bsf COMMAND, 0, ACCESS
		movlw 0x41
		movwf NIBBLE_COUNT, ACCESS	; 65 nibble will folow
		goto CmdEnd

Cmd4:
		movlw 0x04
		cpfseq INBYTE, ACCESS		; command 0100
		goto Cmd5
	; *** set current block ***
		bsf COMMAND, 2, ACCESS
		movlw 0x01
		movwf NIBBLE_COUNT, ACCESS	; 1 nibble will folow
		call Feedback
		goto CmdEnd

Cmd5:
		movlw 0x05
		cpfseq INBYTE, ACCESS		; command 0101
		goto Cmd8
	; *** read status ***
		bsf COMMAND, 3, ACCESS
		movlw 0x05
		movwf NIBBLE_COUNT, ACCESS	; 5 nibble will folow
		goto CmdEnd

Cmd8:
		movlw 0x08
		cpfseq INBYTE, ACCESS		; command 1000
		goto CmdX
	; *** reset ***
		call ResetDevice
		movlw 0x00
		movwf NIBBLE_COUNT, ACCESS	; no nibble will folow
		goto CmdEnd

CmdX:
		; *** unknown command ***
		clrf COMMAND, ACCESS
		goto CmdEnd

CmdEnd:
		bcf PORTB, 3, ACCESS 		; strobe
		goto Nibble2				; get next nibble

; *** second nibble ***
Nibble2: 	; D4 = 0 IN3 = 0
		btfss PORTD, 4, ACCESS		; is D4 high ?
		goto Nibble2				; No
		nop							; Yes, wait for a while
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		movff PORTD, INBYTE 		; read port D
		btfss INBYTE, 4, ACCESS		; is D4 still high
		goto Nibble2				; no, try again !
			
		btfsc INBYTE, 7, ACCESS		; is this a command byte ?
		goto CommandByte2			; yes

; *** Data byte ** 	; D4 = 1 IN3 = 0
		call GetNibble				; get the nibble according to COMMAND
		bsf PORTB, 3, ACCESS 		; strobe
		goto Main					; get next nibble

CommandByte2: ; D4 = 1 IN3 = 0
; *** Command Byte = only reset can come here ***
		bcf PORTB, 7, ACCESS		; clear port B
		bcf PORTB, 6, ACCESS
		bcf PORTB, 5, ACCESS
		bcf PORTB, 4, ACCESS
		call ResetDevice			; clean memory
		bsf PORTB, 3, ACCESS 		; strobe
		goto Main					; get next nibble

;******************************************************************************
;End of program

		END
