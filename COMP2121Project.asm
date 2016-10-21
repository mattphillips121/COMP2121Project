.include "m2560def.inc"

.def status = r29   ; 000 D MM PP 
;D = Direction of rotation (0 clockwise 1 counterclockwise) 
;P = Power (1-3)  
;M = Menu 
;Menu:
;* 00 running
;* 01 Entry
;* 10 Paused
;* 11 Finished
.def minutes = r27
.def seconds = r28
.def LCDtime = r26

.dseg
Turntable:
	.db "-/|``|/-"

.cseg

.org 0x0000 ; Reset interrupt
jmp Reset
.org 0x001E ; Timer2 overflow
jmp timer2Int
.org 0x002E ; Timer0 overflow
jmp timer0Int


Reset:

	; Initialise stack for interrupts
	ldi r16, high(RAMEND)
	out SPH, r16
	ldi r16, low(RAMEND)
	out SPL, r16
	
	; Initialise timer 0 with prescalar 8
	; Interrupt on overflow
	clr r16
	out TCCR0A, r16
	ldi r16, 0b00000010
	out TCCR0B, r16
	ldi r16, 1<<TOIE0 	
	sts TIMSK0, r16 ; Interrupt on overflow
	
	; Initialise the LCD Ports
	ser r16
	out DDRF, r16
	out DDRA, r16
	clr r16
	out PORTF, r16
	out PORTA, r16

	; Initialise the Push buttons
	clr r16
	out DDRD, r16 ; Input
	ser r16
	out PORTD, r16 ; Pull up resistors

	;Initialise the LEDs for testing
	ser r16
	out DDRC, r16 ; output
	clr r16
	out PORTD, r16 ; Turn all off

	;Initialise timer1
	clr r16
	sts TCCR2A, r16
	ldi r16, 0b00000010
	sts TCCR2B, r16
	ldi r16, 1<<TOIE2 ; Interrupt on overflow (is this 255 or whatever a 16bit counter overflows at?)	
	sts TIMSK2, r16

	sei

halt:
	rjmp halt

timer2Int:
	reti

timer0Int:
	reti
