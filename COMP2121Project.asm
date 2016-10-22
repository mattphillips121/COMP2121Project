.include "m2560def.inc"

.def status = r29   ; 00 D MMM PP 
;D = Direction of rotation (0 clockwise 1 counterclockwise) 
;P = Power (1-3)  
;M = Menu 
;Menu:
;* 000 running
;* 001 Entry
;* 010 Paused
;* 100 Finished
;* 111 Door Open
.def minutes = r27
.def seconds = r28
.def LCDtime = r26



.macro clear
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr r16
	st Y+, r16
	st Y, r16
.endmacro

.dseg
Turntable:
	.db "-/|``|/-"

Timer2Counter:
	.byte 2

;--------- START PUSH BUTTON ---------;
Timer2Milli: ; How many ms have passed for timer 2 interrupt (PB) handling
	.byte 1
voltagesSeen0: ; The voltages seen from PB0
	.byte 1
voltagesSeen1: ; The voltages seen from PB1
	.byte 1
;--------- END PUSH BUTTON ----------;

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

	;Initialise the LEDs for testing/Power level
	ser r16
	out DDRC, r16 ; output
	ldi r16, 0b00000011
	out PORTD, r16 ; Turn Set LEDS to display power level 1

	;Initialise timer1
	clr r16
	sts TCCR2A, r16
	ldi r16, 0b00000010
	sts TCCR2B, r16
	ldi r16, 1<<TOIE2 ; Interrupt on overflow

	sei

	ldi status, 0b00000101 ; Set to entry moode, power setting 1 and clockwise rotation

	ldi ZH, high(Turntable)
	ldi ZL, low(Turntable)

halt:
	rjmp halt

timer2Int:

;--------------- START Push Buttons ---------------;
	lds r24, Timer2Counter
	lds r25, Timer2Counter + 1
	adiw r25:r24,1
	ldi r16, high(100)
	cpi r24, low(100)
	cpc r25, r16
	breq continue_int
	rjmp not_milli
continue_int:
	clear Timer2Counter
	lds r24, Timer2Milli
	inc r24
	in r25, PIND
	cpi r25, 0xFF
	brne check_others
	rjmp neither_button
check_others:	
	cpi r25, 0xFE
	breq push0

	cpi r25, 0xFD
	breq push1
	rjmp do_nothing
push0:

	lds r16, voltagesSeen1
	lsl r16
	ori r16, 1
	sts voltagesSeen1, r16

	
	lds r16, voltagesSeen0
	lsl r16
	sts voltagesSeen0, r16

	cpi r16, 0xFE
	; Push button 0 is pressed if equal (Open door)
	breq open_door
	rjmp do_nothing

open_door:
	; If door is not open
	ori status, 0b00011100 ; Set the door to open
	; Move into open door state
		
push1:
	lds r16, voltagesSeen1
	lsl r16
	sts voltagesSeen1, r16
	lds r25, voltagesSeen0
	lsl r25
	ori r25, 1
	sts voltagesSeen0, r25
	cpi r16, 0xFE
	brne do_nothing
	; Push button 1 has been pressed here (Close door)
	mov r16, status
	ori r16, 0b00011100
	cpi r16, 28
	brne door_already_closed
	andi status, 0b11100011 ; Clear the status
	ori status, 0b00001000 ; Set the status to paused
	; Set all of the paused state requirements?? 
door_already_closed:
	rjmp do_nothing

neither_button:
	
	lsl r16
	ori r16, 1
	sts voltagesSeen0, r16
	lds r16, voltagesSeen1
	lsl r16
	ori r16, 1
	sts voltagesSeen1, r16
	
do_nothing:
	reti

not_milli:
	sts Timer2Counter, r24
	sts Timer2Counter+1, r25
	reti

;--------------- END Push Buttons ---------------;

timer0Int:
	reti
