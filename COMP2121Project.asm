.include "m2560def.inc"

.def status = r29   ; 0 K D MMM PP 
;K = Keypadpress flag (0 means it is pressed, 1 ready for new press)
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
.def colnum = r25

.set keypadMask = 0b11110000

.macro clear
	push YH
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr r16
	st Y+, r16
	st Y, r16
	pop YH
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
	out PORTC, r16 ; Turn Set LEDS to display power level 1

	;Initialise timer2
	clr r16
	sts TCCR2A, r16
	ldi r16, 0b00000010
	sts TCCR2B, r16
	ldi r16, 1<<TOIE2 ; Interrupt on overflow
	sts TIMSK2, r16
	
	;Initialise the keypad as 7-4 output and 3-0 input
	ldi r16, keypadMask
	sts DDRL, r16
	ser r16
	sts PORTL, r16
	clr colnum
	

	sei

	ldi status, 0b00000101 ; Set to entry moode, power setting 1 and clockwise rotation

	ldi ZH, high(Turntable)
	ldi ZL, low(Turntable)

halt:
	rjmp halt

timer2Int:

;--------------- START Push Buttons ---------------;
	push r25
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
push1:
	lds r16, voltagesSeen1
	lsl r16
	ori r16, 1
	sts voltagesSeen1, r16

	
	lds r16, voltagesSeen0
	lsl r16
	sts voltagesSeen0, r16

	cpi r16, 0xFE
	; Push button 1 is pressed if equal (Open door)
	breq open_door
	rjmp do_nothing

open_door:
	
	; If door is not open
	ori status, 0b00011100 ; Set the door to open
	;out PORTC, status
	ldi r16, 1
	out PORTA, r16
	rjmp do_nothing
	; Move into open door state
		
push0:
	lds r16, voltagesSeen1
	lsl r16
	sts voltagesSeen1, r16
	lds r25, voltagesSeen0
	lsl r25
	ori r25, 1
	sts voltagesSeen0, r25
	cpi r16, 0xFE
	brne do_nothing
	; Push button 0 has been pressed here (Close door)
	mov r16, status
	;out PORTC, r16
	andi r16, 0b00011100
	
	cpi r16, 28
	brne door_already_closed
	
	andi status, 0b11100011 ; Clear the status
	ori status, 0b00001000 ; Set the status to paused
	in r16, PORTA
	andi r16, 0b11111110
	out PORTA, r16
	rjmp do_nothing
	; Set all of the paused state requirements?? 
door_already_closed:
	 
	;out PORTC, r16
	rjmp do_nothing

neither_button:
	lds r16, voltagesSeen0
	lsl r16
	ori r16, 1
	sts voltagesSeen0, r16
	lds r16, voltagesSeen1
	lsl r16
	ori r16, 1
	sts voltagesSeen1, r16
	
do_nothing:
	pop r25
	reti

not_milli:
	sts Timer2Counter, r24
	sts Timer2Counter+1, r25
	pop r25
	reti

;--------------- END Push Buttons ---------------;


timer0Int:

;--------------- START Keypad --------------;
	;out PORTC, colnum
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0b00011100
	brne keypad_continue
	reti

keypad_continue:
	;ldi colnum, 0
	ldi r17, 0b11101111
	mov r16, colnum
setcolmask:
	cpi r16, 0
	breq colloop
	lsl r17
	ori r17, 1
	dec r16
	rjmp setcolmask

colloop:
	;out PORTC, r17
	cpi colnum, 4 ;
	brne collnext ;If all keys are not scanned yet, keep scanning!
	andi status, 0b10111111 ;turn off debounceFlag if no keys are currently pressed
	clr colnum
	reti
collnext:
	sts PORTL, r17; 
	;out PORTC, r17
	nop
	nop
	lds r17, PINL ; read Port L (r17 is temp1)
	andi r17, 0b00001111 ; Get the input value 
	cpi r17, 0b00001111 ; Check if any row is low
	breq nextcol
	ldi r18, 0b00000001 ; initialise for row check
	clr r19 ; clear the row number

rowloop:
	cpi r19, 4
	breq nextcol ; The row scan is over
	mov r20, r17 ; r20 is temp2
	and r20, r18 ; Check unmasked bit
	breq convert ; If bit is clear, the key is pressed
	inc r19
	lsl r18
	rjmp rowloop

nextcol:
	inc colnum ; increase column value
	reti
	;jmp colloop ; go to next column

convert:
	; Check the debounce flag in status
	mov r17, status
	andi r17, 0b01000000
	cpi r17, 0
	breq dont_stop
	;ser r17
	;out PORTC, r17
	reti

dont_stop:
	ori status, 0b01000000 
	;out PORTC, colnum
	cpi colnum, 3
	breq letters

	cpi r19, 3
	breq symbols

	mov r17, r19
	lsl r17
	add r17, r19
	add r17, colnum
	subi r17, -1
	out PORTC, r17
	rjmp convert_end

letters:
	cpi r19, 0;a
	breq addition
	cpi r19, 1;b
	breq subtraction
	cpi r19, 2;c
	breq multiplication
	cpi r19, 3;e (just joking it's d)
	breq division

addition:
	;add acc, input
	;clr input
	rjmp show
subtraction:
	;sub acc, input
	;clr input
	rjmp show
multiplication:
	;mul acc, input
	;mov acc, r0
	;clr input
	rjmp show
division: ;why is division the only operation that ends in 'sion'
	;cpi input, 0
	;breq show
	;clr temp2
	;divstart:
	;	cp acc, input
	;	brlo divdone
	;	sub acc, input
	;	inc temp2
	;	rjmp divstart
divdone:
	;mov acc, temp2
	;clr input
	rjmp show

symbols:
	;ldi temp1, 0
	;cpi col, 0 ; Check if we have a star
	;breq star
	;cpi col, 1 ; or if we have zero
	;breq zero
	;jmp convert_end
star: ;star empties accumulator and clears out bottom line
	;clr input 
	;clr acc 

	;do_lcd_command 0b00000001 ; clear display	
	;do_lcd_command 0x80  	; cursor on top line
	;do_lcd_data '0' ;show accumulator is empty

	;rcall sleep_5ms ;this is here because it is good luck
	;ldi debounceFlag, 1	;because we have received input we now set r24 to 0

	;jmp screen_end ;we would rjmp to 'show' but the spec requires an empty bottom line which show can't do (since it would display 0 instead), so we just do it here
	rjmp show
zero: ;very exciting
	;ldi temp1, 0
	

convert_end:
	;cpi debounceFlag, 0 ;if the debounceFlag is off then we continue but otherwise we just ignore all inputs
	;breq convert_continue
	;rjmp screen_end ;screen_end is too far away for a breq jump
convert_continue:
	;out PORTC, temp1
	;cpi input, 1 ;if input is 0 then we just add temp1 to it
	;brge addtens
	;add input, temp1
	;rjmp show
	;addtens:			;else we add 10*temp1 to it (since now we're going from 1 digit to 2 digit number)
	;ldi temp2, 10
	;mul input, temp2
	;mov input, temp1
	;add input, r0

show:

	;do_lcd_command 0b00000001 ; clear display	

	;do_lcd_command 0x80  	; cursor on top line
	;bin_to_txt acc

	;do_lcd_command 0xC0		; cursor on bottom line
	;bin_to_txt input

	;rcall sleep_5ms ;5ms of good luck
	;ldi debounceFlag, 1	;because we have received input we now set r24 to 0
	;andi status, 0b10111111
screen_end:
	reti

;--------------- END Keypad ----------------;
