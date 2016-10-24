.include "m2560def.inc"

.def status = r29   ; O K D MMM PP 
;O = old flag (1 if door was opened in entry mode)
;K = Keypadpress flag (0 means it is pressed, 1 ready for new press)
;D = Direction of rotation (0 clockwise 1 counterclockwise) 
;P = Power (1-3)  
;M = Menu 
;Menu:
;* 000 running
;* 001 Entry
;* 010 Paused
;* 100 Finished
;* 110 Power Entry
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

.macro changeMode
	andi status, 0b11100011
	.if @0 == 'E'
	ori status, 0b00000100
	.elif @0 == 'P'
	ori status, 0b00001000
	.elif @0 == 'F'
	ori status, 0b00010000
	.elif @0 == 'R'
	ori status, 0b00000000
	.elif @0 == 'D'
	ori status, 0b00011100
	.elif @0 == 'X'
	ori status, 0b00011000
	.endif
.endmacro

;----------------- LCD Macros ---------------
.macro mov_lcd_data ;need this for when I want to pass a reference to the lcd
	mov r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro do_lcd_command
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro
.macro do_lcd_data
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro bin_to_txt ;normally I'd loop over powers of 10 but I only wanted to use 2 registers so we simply have predefined powers of 10	
	mov r18, @0 ;im safe to use r18/19 because they're only used for rmask/cmask -- otherwise i'd have to do some stack (or lds/sts) stuff and thats no fun
	clr r19
	huns: ;we count the number of hundreds in our number
		cpi r18, 100
		brlo hundnext
		inc r19
		subi r18, 100
		rjmp huns
	hundnext:
		cpi r19, 1 ;if there's a hundreds digit to be written 
		brlo tens
		subi r19, -'0'
		mov_lcd_data r19
		ldi r19, 10 ;we do this to signal that a hundreds digit has been written (in the event of the tens digit being 0)
	tens:
		cpi r18, 10
		brlo tensnext
		inc r19
		subi r18, 10
		rjmp tens
	tensnext:
		cpi r19, 1  ;if there's a tens digit to be written (even if the tens digit is 0 the character '0' will be written if there was already a hundreds digit)
		brlo ones	;consider 109 vs 009 (where you would want 9 instead of 009)
		cpi r19,10
		brlo tenskip
		subi r19, 10;meed to -10 if r19 was =>10 because the 10 is just the signal from huns
		tenskip:
		subi r19, -'0'
		mov_lcd_data r19
		clr r19
	ones:
		subi r18, -'0' ;don't need any logic here because we are guaranteed a 1 digit number at this point to print
		mov_lcd_data r18
		clr r18

.endmacro

.macro mov_lcd_data_a ;need this for when I want to pass a reference to the lcd
	push YH
	push YL
	push XH
	push XL
	mov r16, @0

	lds YH, high(array_end)
	lds YL, low(array_end)
	ld XL, y+
	ld XH, y
	
	clr r0
	st x+, r16
	st x, r0

	st y+, XL
	st y, XH

	pop XL
	pop XH
	pop YL
	pop YH	
.endmacro
.macro do_lcd_command_a
	push YH
	push YL
	push XH
	push XL
	ldi r16, @0

	lds YH, high(array_end)
	lds YL, low(array_end)
	ld XL, y+
	ld XH, y
	
	st x+, r16
	ldi r16, 1
	st x, r16

	st y+, XL
	st y, XH

	pop XL
	pop XH
	pop YL
	pop YH	
.endmacro
.macro do_lcd_data_a
	push YH
	push YL
	push XH
	push XL
	ldi r16, @0

	lds YH, high(array_end)
	lds YL, low(array_end)
	ld XL, y+
	ld XH, y
	
	st x+, r16
	ldi r16, 0
	st x, r16

	st y+, XL
	st y, XH

	pop XL
	pop XH
	pop YL
	pop YH	
.endmacro
.macro queue_pop ;@0 is the start of the array 
	push YH
	push YL
	push XH
	push XL
	push ZH
	push ZL

	lds YL, low(LCD_array)
	lds YH, high(LCD_array)
	lds XH, high(array_end)
	lds XL, low(array_end)
	ld ZL, x+
	ld ZH, x
	adiw y, 2
	cp YL, ZL
	cpc YH, ZH
	brlo more_than_one
	breq one_in_array
	rjmp end_macro
one_in_array:
	sbiw y, 2
	sts array_end, YL
	sts array_end+1, YH
	rjmp end_macro
	 	
more_than_one:
	;while y < z
	cp YL, ZL
	cpc YH, ZH
	breq end_macro
	ld XL, y+
	ld XH, y
	sbiw y, 3
	st y+, XL
	st y+, XH
	adiw y, 2
	rjmp one_in_array

end_macro:	
	pop ZL
	pop ZH
	pop XL
	pop XH
	pop YL
	pop YH	
.endmacro

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro
;--------------- END LCD Macros -------------

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

;--------- LCD variables ------------
LCD_command_var:
	.byte 1
LCD_timer_var:
	.byte 1
LCD_type_var:
	.byte 1

LCD_array:
	.byte 80
array_end:
	.byte 2
;--------- END LCD variables --------

;--------- MOTOR variables ----------
num_overflow_int:
	.byte 1
num_needed:
	.byte 1
;--------- END MOTOR variables ------
.cseg

.org 0x0000 ; Reset interrupt
jmp Reset
.org 0x001E ; Timer2 overflow
jmp timer2Int
.org 0x0028
jmp timer1OVF
.org 0x002E ; Timer0 overflow
jmp timer0Int



Reset:
	ldi status, 0b00000101 ; Set to entry moode, power setting 1 and clockwise rotation
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
	
	; Initialise the motor port
	ser r16
	out DDRB, r16
	clr r16
	out PORTB, r16

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

	;Initialise timer1
	clr r16
	sts TCCR1A, r16
	ldi r16, 0b00000010 ;Set a 8 prescalar
	sts TCCR1B, r16
	ldi r16, (1<<TOIE1) ;Interrupt on compare match and overflow
	sts TIMSK1, r16
	clr r16
	sts num_overflow_int, r16
	ldi r16, 8
	sts num_needed, r16
	

	
	;Initialise the keypad as 7-4 output and 3-0 input
	ldi r16, keypadMask
	sts DDRL, r16
	ser r16
	sts PORTL, r16
	clr colnum

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink
	do_lcd_data 'D'
	
	
	sei
	;out PORTC, status
	

	ldi ZH, high(Turntable)
	ldi ZL, low(Turntable)
	;out PORTC, status

	clr seconds
	clr minutes

	;do_lcd_data_a 'F'

halt:
	rjmp halt

timer2Int:

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
;--------------- START LCD pipeline ---------------;
	lds r16, LCD_timer_var
	lds r18, LCD_type_var
	cpi r18, 2
	breq waiting_start
	cpi r16, 0
	brne continue_checking_lcd
	rjmp push_button_continue
continue_checking_lcd:
	cpi r16, 4
	brne command_executed
	lds r17, LCD_command_var
	out PORTF, r17
	cpi r18, 1
	breq command_end
	lcd_set LCD_RS
	rjmp command_end
command_executed:
	cpi r16, 3
	brne lcdE_set
	lcd_set LCD_E
	rjmp command_end
lcdE_set:
	lcd_clr LCD_E
	cpi r16, 1
	brne command_end
	inc r16
	sts LCD_timer_var,r16
	ldi r18, 2
	sts LCD_type_var, r18
    clr r17
	out DDRF, r17
	out PORTF, r17
	lcd_set LCD_RW
	cpi r18, 1
	brne skip_jump_to_push_button
	rjmp push_button_continue
skip_jump_to_push_button:
	lcd_clr LCD_RS
	rjmp push_button_continue
command_end:
	dec r16
	sts LCD_timer_var, r16

		






waiting_start:
	cpi r16, 2
	breq mili_wait2
	cpi r16,1
	breq mili_wait1
	in r17, PINF
	lcd_clr LCD_E
	sbrc r17, 7
	rjmp reset_timer
	lcd_clr LCD_RW
	ser r17
	out DDRF, r17 
	; Move the queue forward one here
	queue_pop
	; If it is empty, clr the timer
	; If it is not empty, set the timer to 4 again
	rjmp push_button_continue
mili_wait2:
	lcd_set LCD_E
	rjmp timer_decrement_wait
mili_wait1:
	lcd_clr LCD_E
timer_decrement_wait:
	dec r16
	sts LCD_timer_var, r16
	rjmp push_button_continue
reset_timer:		
	ldi r16,2
	sts LCD_timer_var, r16
;--------------- END LCD pipeline -----------------;
;--------------- START Push Buttons ---------------;
push_button_continue:
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
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0 ;If door was opened in running mode
	breq opened_when_running ; 
	cpi r16, 0b00001000 ;If door was opened in paused mode
	breq opened_when_running
	rjmp opened_in_entry ; Works for finished as well
	; If door is not open

opened_when_running:
	ori status, 0b00011100 ; Set the door to open
	;out PORTC, status
	rjmp set_led
opened_in_entry:
	ori status, 0b10011100
set_led:
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
	
	andi status, 0b11100011
	mov r16, status
	andi r16, 0b10000000
	cpi r16, 0
	brne to_entry_mode
	 ; Clear the status
	ori status, 0b00001000 ; Set the status to paused
	rjmp turn_off_door_led
to_entry_mode:
	andi status, 0b01111111
	ori status, 0b00000100 ; Set the status to entry mode
turn_off_door_led:
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
	mov r16, status ; Check to see whether to door is open
	andi r16, 0b00011100
	cpi r16, 0b00011100
	brne keypad_continue
	reti ; Handle no input if it is open

keypad_continue:
	;ldi colnum, 0
	ldi r17, 0b11101111 ;Load the column mask
	mov r16, colnum
setcolmask: ;Loop to position the column mask. Depends on colnum
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
	; Debouncing is still suss I think?
	clr colnum
	reti
collnext:
	sts PORTL, r17 ;Write the coolumn mask to keypad port
	;Wait for change to be made
	nop
	nop
	nop
	lds r17, PINL ; read Port L (r17 is temp1)
	andi r17, 0b00001111 ; Get the input value 
	cpi r17, 0b00001111 ; Check if any row is low
	breq nextcol ;If no button pressed, move to next
	ldi r18, 0b00000001 ; initialise for row check
	clr r19 ; clear the row number

rowloop:
	cpi r19, 4
	breq nextcol ; The row scan is over
	mov r20, r17 ; r20 is temp2, copy the input from keypad
	and r20, r18 ; Check unmasked bit
	breq convert ; If bit is clear, the key is pressed
	inc r19
	lsl r18 ;Update the row mask to scan along
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
	breq letters ;Handle the letters 

	cpi r19, 3
	brne numbers ;Handle the bottom symbols
	rjmp symbols
numbers:
	mov r16, status
	andi r16, 0b00011100 
	cpi r16, 0b00000100 ;Check if it is in entry mode
	brne change_power ;If not, check if it is in change power mode
	rjmp in_entry_mode

change_power:
	cpi r16, 0b00011000
	brne ignore_number ;If it is in neither change power or entry, ignore the press

in_entry_mode:
	;Algorithm to get num from keypad position
	mov r17, r19
	lsl r17
	add r17, r19
	add r17, colnum
	subi r17, -1
	;End Algorithm
	
	mov r16, status
	andi r16, 0b00011000
	cpi r16, 0b00011000 ;Check whether it was in power change mode
	brne not_power
	changeMode 'E' ;Set it back to entry
	cpi r17, 4 ;If the number is greater than 3 ignore it
	brge ignore_number
	andi status, 0b11111100
	or status, r17 ;Set the power level in the status reg
	;out PORTC, status ;Debug display on leds
	clr r16
	;sts TCNT1H, r16
	;sts TCNT1L, r16
	sts num_overflow_int, r16
	cpi r17, 1
	breq set_powerlvl_1
	cpi r17, 2
	breq set_powerlvl_2
	ldi r16, 31
	sts num_needed, r16
	rjmp show
set_powerlvl_1:
	ldi r16, 8
	sts num_needed, r16
	rjmp show
set_powerlvl_2:
	ldi r16, 16
	sts num_needed, r16
	rjmp show
not_power:
	;out PORTC, r17 ;Debug display on leds
ignore_number:	
	rjmp convert_end

letters:
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0b00011000
	brne interpret_letters
	changeMode 'E'
interpret_letters:
	cpi r19, 0;a
	breq change_powerlvl
	cpi r19, 1;b
	breq no_change

	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0 ; Check in running mode
	brne no_change

	cpi r19, 2;c
	breq add_30
	cpi r19, 3;e (just joking it's d)
	breq sub_30

change_powerlvl:
	;Set mode to power lvl change if in entry mode only
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0b00000100
	brne no_change
	changeMode 'X'
	;add acc, input
	;clr input
no_change:
	rjmp show
add_30:
	cpi seconds, 70
	brge no_change
	subi seconds, -30 ; Should it add to minutes?
	;mul acc, input
	;mov acc, r0
	;clr input
	rjmp show
sub_30: ;why is division the only operation that ends in 'sion'
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
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0b00011000
	brne interpret_symbols
	changeMode 'E'
interpret_symbols:
	ldi r16, 0
	cpi colnum, 0 ; Check if we have a star
	breq start_button
	cpi colnum, 1 ; or if we have zero
	breq zero
	brne stop_button
start_button: ;start_button 
; Entry Mode  - Change to running mode
;	Run the time entered. If no time entered, run for 1 min
; Running Mode - Add 1 min
; Pause Mode - Resume running
	mov r16, status
	andi r16, 0b00011100 ;Mask to get the current operating mode
	cpi r16, 0 ;Check if in running mode
	breq add_one_min
	cpi r16, 0b00010000 ;Check if in finished mode
	breq s_button_does_nothing
	;Otherwise, in entry mode and handle that
	clr r0
	cpi seconds, 0
	cpc minutes, r0
	breq add_one_min
	rjmp max_time

add_one_min: ; Issue with whether or not it should add to seconds?
	cpi minutes, 99
	brge max_time
	inc minutes
max_time:
	changeMode 'R' ;Set the status to running mode
	
s_button_does_nothing:
	;jmp screen_end ;we would rjmp to 'show' but the spec requires an empty bottom line which show can't do (since it would display 0 instead), so we just do it here
	rjmp show
zero: ;very exciting
	;ldi temp1, 0
stop_button:
; Entry mode - Clear all currently entered time
; Running Mode - Enter Pause mode
; Pause/Finished Mode - Enter entry mode and clear time etc (reset)		
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0b00000100 ; Entry mode
	breq clear_time
	cpi r16, 0b00000000 ; Running mode
	breq pause_button

clear_time:
	;out PORTC, status
	clr seconds
	clr minutes
	changeMode 'E'
	rjmp show

pause_button:
	changeMode 'P'
	clr r16
	out PORTB, r16
	rjmp show

convert_end:
	;cpi debounceFlag, 0 ;if the debounceFlag is off then we continue but otherwise we just ignore all inputs
	;breq convert_continue
	;rjmp screen_end ;screen_end is too far away for a breq jump
convert_continue:
	;out PORTC, r17
	cpi seconds, 1 ;if input is 0 then we just add temp1 to it
	brge addtens
	add seconds, r17
	rjmp show
	addtens:
	cpi seconds, 10
	brge addmins			;else we add 10*temp1 to it (since now we're going from 1 digit to 2 digit number)
	ldi r18, 10
	mul seconds, r18
	mov seconds, r17
	add seconds, r0
	add seconds, r17
	addmins:
	cpi minutes, 1
	brge addtenmins
	add minutes, r17
	addtenmins:
	ldi r18,10
	mul minutes, r18
	mov minutes, r17 
	add minutes, r0
	add minutes, r17



show:

	;do_lcd_command 0b00000001 ; clear display	

	;do_lcd_command 0x80  	; cursor on top line
	;bin_to_txt acc

	;do_lcd_command 0xC0		; cursor on bottom line
	;bin_to_txt input

	;rcall sleep_5ms ;5ms of good luck
	;ldi debounceFlag, 1	;because we have received input we now set r24 to 0
	;andi status, 0b10111111
	;out PORTC, status
screen_end:
	reti

;--------------- END Keypad ----------------;

timer1OVF:
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0
	breq continue_motor_overflow
	clr r16
	out PORTB, r16
	reti
continue_motor_overflow:
	lds r16, num_overflow_int
	cpi r16, 0
	brne already_started
	ser r18
	out PORTB, r18
already_started:
	inc r16
	lds r17, num_needed
	cpi r17, 31
	cp r16, r17
	brne dont_turn_off
	clr r17
	out PORTB, r17
dont_turn_off:
	cpi r16, 31
	brne dont_turn_on_motor
	ser r16
	out PORTB, r16
	clr r16
	
	;Turn on motor here
dont_turn_on_motor:
	sts num_overflow_int, r16
	reti

	
	

;---------------lcd subroutines used for timing during lcd commands (aka shit I didn't write) ------------------




;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, r16
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

