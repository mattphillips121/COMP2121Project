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
.def minutes = r27 ; Stores the current number of minutes to show
.def seconds = r26 ; Stores the current number of seconds to show
.def colnum = r25 ; Global variable for the column number that the keypad scan is up to

.set keypadMask = 0b11110000 ; Mask used to set the pins 7-4 to output and 3-0 input



;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START GENERAL MACROS						;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;

.macro clear ; Clears a two byte variable in SRAM (sets it to 0)
	push YH ; Push YH to avoid conflict 
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr r16
	st Y+, r16
	st Y, r16	 
	pop YH
.endmacro

.macro changeMode ; Changes the status register modes 
	andi status, 0b11100011 ;Clears the mode bits in status register
	.if @0 == 'E' ; Entry Mode
	ori status, 0b00000100
	.elif @0 == 'P' ; Pause Mode
	ori status, 0b00001000
	.elif @0 == 'F' ; Finished Mode
	ori status, 0b00010000
	.elif @0 == 'R' ; Running Mode
	ori status, 0b00000000
	.elif @0 == 'D' ; Door open Mode
	ori status, 0b00011100
	.elif @0 == 'X' ; Power level change Mode
	ori status, 0b00011000
	.endif
.endmacro

.macro if_in_mode ; Check whether a certain mode is the current mode
; Used before a breq or a brne only. Does not say what the current mode is, only whether or not it is the queried
	push r23
	mov r23, status
	andi r23, 0b00011100 ; Get only the mode bits of the status register
	.if @0 == 'E' 
	cpi r23, 0b00000100
	.elif @0 == 'P'
	cpi r23, 0b00001000
	.elif @0 == 'F'
	cpi r23, 0b00010000
	.elif @0 == 'R'
	cpi r23, 0b00000000
	.elif @0 == 'D'
	cpi r23, 0b00011100
	.elif @0 == 'X'
	cpi r23, 0b00011000
	.endif
	pop r23
.endmacro

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END GENERAL MACROS							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;


;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START LCD MACROS							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;

.macro mov_lcd_data ;need this for when I want to pass a reference to the lcd 
; OLD MACRO DELETE
	mov r16, @0
	call lcd_data
	call lcd_wait
.endmacro
.macro do_lcd_command
; OLD MACRO DELETE
	ldi r16, @0
	call lcd_command
	call lcd_wait
.endmacro
.macro do_lcd_data
; OLD MACRO DELETE
	ldi r16, @0
	call lcd_data
	call lcd_wait
.endmacro

.macro bin_to_txt ; Given a number in a register, will convert the value to ascii code and write out to current position on LCD
	push r18
	push r19
	mov r18, @0 
	clr r19
	tens: ; Continue subtracting 10 from the number and incrementing another register until the input number is less than 10
		cpi r18, 10
		brlo tensnext
		inc r19
		subi r18, 10
		rjmp tens
	tensnext: ; Convert the number of 10s (stored in r19) to the ascii for that number
		subi r19, -'0'
		mov_lcd_data_a r19 ; Append it to the LCD Queue
		clr r19
	ones: ; Convert the remaining 1s digit to the ascii
		subi r18, -'0'
		mov_lcd_data_a r18 ; Append to the LCD Queue
		clr r18
	pop r19
	pop r18
.endmacro

.macro mov_lcd_data_a ; Write an ascii value in a REGISTER to the LCD queue and update queue
	push YH
	push YL
	push XH
	push XL
	mov r16, @0 ; Store the input in r16

	ldi YH, high(array_end) ; Make Y a pointer to the pointer to the end of the queue
	ldi YL, low(array_end)
	ld XL, y+ ; Make X a pointer to the end of the queue (Note: XL is first since AVR is little endian)
	ld XH, y ; Increment y and store again because the pointer is a 2 byte variable
	
	st x+, r16 ; Store the new value into the end of the queue
	ldi r16, 0 ; Store the type of the new value as data (data is 0, command is 1) Important when pipelining
	st x+, r16 ; Increment x twice so it can be stored back as the pointer to the new end of the queue

	sbiw y, 1 ; Set y back to the start of the pointer to the end of the queue
	st y+, XL ; Update the pointer value in array_end to point to the new end of the queue
	st y, XH 


	pop XL
	pop XH
	pop YL
	pop YH		
.endmacro
.macro do_lcd_command_a ; Append a command to the LCD queue
	push YH
	push YL
	push XH
	push XL
	ldi r16, @0 ; Load the value of the command into r16

	ldi YH, high(array_end) ; Make Y a pointer to the pointer to the end of the queue
	ldi YL, low(array_end)
	ld XL, y+ ; Make X a pointer to the end of the queue (Note: XL is first since AVR is little endian)
	ld XH, y ; Increment y and store again because the pointer is a 2 byte variable
	
	st x+, r16 ; Store the new value into the end of the queue
	ldi r16, 1 ; Store the type of the new value as a command (data is 0, command is 1) Important when pipelining
	st x+, r16 ; Increment x twice so it can be stored back as the pointer to the new end of the queue

	sbiw y, 1 ; Set y back to the start of the pointer to the end of the queue
	st y+, XL ; Update the pointer value in array_end to point to the new end of the queue
	st y, XH

	pop XL
	pop XH
	pop YL
	pop YH	
.endmacro

.macro do_lcd_data_a ; Essentially the same as mov_lcd_data but gets the data as an ascii coode, not in a register
	push YH
	push YL
	push XH
	push XL
	ldi r16, @0

	ldi YH, high(array_end)
	ldi YL, low(array_end)
	ld XL, y+
	ld XH, y

	st x+, r16
	ldi r16, 0
	st x+, r16

	sbiw y, 1
	st y+, XL
	st y, XH


	pop XL
	pop XH
	pop YL
	pop YH	
.endmacro
.macro queue_pop ;Takes no Args
; Removes the first element in the queue and shifts the entire queue forwards
	push YH
	push YL
	push XH
	push XL
	push ZH
	push ZL

	ldi YL, low(LCD_array) ; Set y to point to the start of the queue
	ldi YH, high(LCD_array)
	ldi XH, high(array_end) ; Set x to be a pointer to the pointer to the end of the queue
	ldi XL, low(array_end)
	ld ZL, x+ ; Set z to point to the end of the queue
	ld ZH, x
	
loop_again:
	adiw y, 2 ; Add two to get the next element in the queue (It is a queue with item size 2 bytes)
	cp YL, ZL ; Compare the start+2 and the end addresses
	cpc YH, ZH
	brlo more_than_one ; If start+2 is still lower than the end address, then there is more than one element in the queue
	breq one_in_array ; If they are equal, then there is only one element in the queue
	rjmp end_macro ; Otherwise, the queue is empty so the macro is finished
one_in_array:
	sbiw y, 2 ; Bring the y value back to the new end of queue
	sts array_end, YL ; Update the array_end pointer to point to the end of the updated array
	sts array_end+1, YH ; Little endian order
	rjmp end_macro ; End the macro
	 	
more_than_one:

	cp YL, ZL ; Compare the y (pointer to item to move forward in the queue) and the end of queue pointer
	cpc YH, ZH
	breq end_macro ; When these two equal, all elements have been moved forward and the queue operation is complete
	ld XL, y+ ; Otherwise, load the elements of the item y points to into x (x can be used again now since z is already initialised)
	ld XH, y
	sbiw y, 3 ; Get y back to the position that the next item will be moved to
	st y+, XL ; Store the item in x (item to be moved) into y 
	st y+, XH
	rjmp loop_again

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

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END LCD MACROS								;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;

.dseg
;Turntable2:
	;.db "-/|``|/-" ; String defined in data memory that is looped through for turntable rotation
	;.db "1","2","3","4","4","3","2","1";test string
;	.byte 4

Timer2Counter:
	.byte 2 ; The counter used to add up interrupts (that handle lcd and push buttons) until a sufficient number has passed

;--------- START PUSH BUTTON ---------;
Timer2Milli: ; How many ms have passed for timer 2 interrupt (PB) handling
	.byte 1
voltagesSeen0: ; The voltages seen from PB0
	.byte 1
voltagesSeen1: ; The voltages seen from PB1
	.byte 1
;--------- END PUSH BUTTON ----------;




;--------- LCD variables ------------

LCD_timer_var: ; Stores the timer that is used to determine which section of the LCD handling to run
	.byte 1
LCD_array: ; Stores the queue of commands, data and type identifiers. Overestimated due to large memory space
	.byte 5000
array_end: ; Stores a pointer to the last location in the queue
	.byte 2
;--------- END LCD variables --------




;--------- MOTOR variables ----------
num_overflow_int:
	.byte 1 ; Counts the number of overflow interrupts
num_needed:
	.byte 1 ; Stores the number of overflows that are needed before turning off the motor
;--------- END MOTOR variables ------

;--------- BEEPING variables --------
beepnum:
	.byte 1
beep_overflow:
	.byte 1
untouched:
	.byte 1
;--------- Spinner variables --------
turntable_seconds:
	.byte 1
;--------- END Spinner variables ----



;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START CODE SEGMENT							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;

.cseg 

.org 0x0000 ; Reset interrupt - Used for initialisation and on start up
jmp Reset
.org 0x001E ; Timer2 overflow - Used for LCD pipeline and Push buttons
jmp timer2Int
.org 0x0028 ; Timer1 overflow - Used for Motor and turntable
jmp timer1OVF
.org 0x002E ; Timer0 overflow - Used for pipelined keypad
jmp timer0Int
.org 0x0046 ; Timer3 overflow - Used for backlight and key press beep 
jmp timer3Int

Turntable:
	.db "-/|``|/-" ; String defined in data memory that is looped through for turntable rotation

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START INITIALISATION						;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;

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
	
	; Initialise the LCD Ports and top LED port to output
	ser r16
	out DDRF, r16
	out DDRA, r16
	clr r16
	out PORTF, r16
	out PORTA, r16
	
	; Initialise the motor port to output
	ser r16
	out DDRB, r16 ; Output
	clr r16
	out PORTB, r16 ; Low voltage

	; Initialise the Push buttons to input
	clr r16
	out DDRD, r16 ; Input
	ser r16
	out PORTD, r16 ; Pull up resistors

	;Initialise the LEDs for testing/Power level
	ser r16
	out DDRC, r16 ; output
	ldi r16, 0b00000011
	out PORTC, r16 ; Turn Set LEDS to display power level 1
	
	;Would be changed for the speaker
	ser r16
	out DDRG, r16
	clr r16
	out PORTG, r16
	

	
	;Initialise the keypad as 7-4 output and 3-0 input
	ldi r16, keypadMask
	sts DDRL, r16
	ser r16
	sts PORTL, r16 ; Set all output (columns) to high and set pull up resistors on input
	clr colnum ; Clear the column counter used for creating mask in keypad interrupt

	;Initialise the LCD timer to 0 (because queue is empty)
	clr r16
	sts LCD_timer_var, r16

	; Initialise all of the LCD stuff
	; Don't use pipeline stuff here since time is not an issue in start up phase
	do_lcd_command 0b00111000 ; 2x5x7
	call sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	call sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00000110 ; Cursor on, bar, no blink
	;do_lcd_data 'A'

	; Initialise all of the queue variables to empty queue
	ldi XH, high(LCD_array) ; Load the address of the start of the queue into x
	ldi XL, low(LCD_array)
	sts array_end, XL ; Store the pointer to the start into the pointer to the end variable
	sts array_end+1, XH ; Sets up an empty queue

	; Initialise the minutes and seconds registers to 0
	clr seconds 
	clr minutes
	

	;Initialise timer2 (LCD and Push Buttons)
	clr r16
	sts TCCR2A, r16
	ldi r16, 0b00000010
	sts TCCR2B, r16
	ldi r16, 1<<TOIE2 ; Interrupt on overflow
	sts TIMSK2, r16

	;Initialise timer1 (Motor and turntable)
	clr r16
	sts TCCR1A, r16
	ldi r16, 0b00000010 ;Set a 8 prescalar
	sts TCCR1B, r16
	ldi r16, (1<<TOIE1) ;Interrupt on overflow
	sts TIMSK1, r16
	
	; Initialise timer3 (beep and backlight)
	clr r16
	sts TCCR3A, r16
	ldi r16, 0b00000011 ; 64 prescalar
	sts TCCR3B, r16
	ldi r16, 1<<TOIE3 ; Interrupt on overflow
	sts TIMSK3, r16
	clr r16
	sts beep_overflow, r16
	sts untouched, r16
	

	; Initialise the number of overflows to 0 and the num needed before turning off to 8
	; 8 represents the power level. 
	; There are approx. 31 interrupts a second
	; Therefore level 1 (25% duty cycle) turns off after 8
	; 			level 2 (50% duty cycle) turns off after 16
	;			level 3 (100% duty cycle) turns off after 31
	clr r16
	sts num_overflow_int, r16
	sts beepnum, r16
	ldi r16, 8
	sts num_needed, r16
	
	
	sei
	;out PORTC, status
	
	; Initialise the number of seconds for the turntable to 0
	; When it is 5, it will update the displayed image (5 means 1 cycle per 20s = 3 cycles per min)
	clr r16
	sts turntable_seconds, r16
	; Initialise the z pointer to the start of the turntable string in SRAM
	ldi ZH, high(Turntable<<1)
	ldi ZL, low(Turntable<<1)



	; Pipeline the initial set up for entry mode lcd screen display
	do_lcd_command_a 0b00000001 ; clear display	

	do_lcd_command_a 0x80  	; cursor on top line
	bin_to_txt minutes		; Will be 00

	do_lcd_data_a ':'		; Colon to separate minutes and seconds
	bin_to_txt seconds		; Will be 00
	do_lcd_command_a 0xC0	; Set cursor to bottom
	do_lcd_data_a 'A'		; Write out group num
	do_lcd_data_a '1'

	do_lcd_command_a 0xCF 	; Bottom right corner
	do_lcd_data_a 'C'		; Door is closed initially


halt:
	rjmp halt				; Empty loop. Now the initialisation phase is finished and the code is purely timer interrupts

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END INITIALISATON							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;





;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START TIMER2 SEGMENT						;
;						LCD & PUSH BUTTONS							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;
timer2Int:

	push r25
	lds r24, Timer2Counter ; Load the counter for timer 2 to check whether to properly enter the interrupt
	lds r25, Timer2Counter + 1
	adiw r25:r24,1
	ldi r16, high(20) ; If it has been 20 interrupts, enter the handler
	; The lower value the faster the LCD updates howeever run into issues with wastefulness and push button debouncing
	cpi r24, low(20)
	cpc r25, r16
	breq continue_int
	rjmp not_milli
continue_int:
	
	
	clear Timer2Counter ; Reset the counter to 0 

	lds r24, Timer2Milli ; Do we need this?
	inc r24




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START LCD PIPELINE							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;
	
	
	lds r16, LCD_timer_var ; Load the LCD timer to check what section of the pipeline to jump to
	cpi r16, 0 ; If the timer is 0, check whether or not the queue is empty
	breq check_queue
	rjmp handle_array ; Otherwise, handle the item at the start of the queue

; Check whether the Queue is empty
check_queue:
	push YH ; Push registers that may cause conflicts
	push YL
	push XH
	push XL
	push ZH
	push ZL
	ldi YL, low(LCD_array) ; Load the address of the start of the queue into y
	ldi YH, high(LCD_array)
	ldi XH, high(array_end) ; Load the address of the pointer to the end of the queue 
	ldi XL, low(array_end)

	ld ZL, x+ ; Load the pointer to the end of the queue into z
	ld ZH, x
	cp YL, ZL ; Compare whether the start (y) equals the end (z)
	cpc YH, ZH
	pop ZL ; Pop here since it has no impact on the comparison flags in the SREG and need the registers restored
	pop ZH
	pop XL
	pop XH
	pop YL
	pop YH
	brne array_not_empty ; If the start address and the end address were not equal, the queue is not empty
	rjmp push_button_continue ; Otherwise jump to handling the push buttons

; If the array is not empty run this label
array_not_empty:
	lds r18, LCD_array+1 ; Check what type the element at the start of the array is (0 for data, 1 for command)
	cpi r18, 2 ; It is possible to be set to 2 when waiting for the LCD busy flag to be clear
	breq waiting_start ; If it is two, then the process is already begun so jump to the appropriate handling (waiting for busy flag to be clear)
	ldi r16, 4 ; If it is not of type 2, then set the timer to 4 to start hanndling the item at the start of the queue
	sts LCD_timer_var, r16 ; Store the 4 into the timer variable

; This is run every time, used to allow the timer to not be set to 4 (in the case when the type is 2)
handle_array:
	lds r18, LCD_array + 1 ; Need to reload the type variable in case the timer is not 0 
	cpi r18, 2
	breq waiting_start ; Break to the waiting loop if it is of type 2
	;cpi r16, 0 ; Check whether the timer is 0 (should never be 0?) I think we can delete these 3 lines
	;brne continue_checking_lcd
	;rjmp push_button_continue
continue_checking_lcd:
	cpi r16, 4 ; Check the timer to determine which stage to jump to
	brne command_executed ; If the timer is not 4, then the first step has been completed and the command put out on the LCD port F
	lds r17, LCD_array ; Otherwise, load the command/data from the start of the queue
	
	out PORTF, r17 ; Out the data/command from the first element to the LCD port
	
	cpi r18, 1 ; Check whether the type is data or a command (will never be 2 at this point)
	breq command_end ; If it is a command, do not set the RS bit
	lcd_set LCD_RS ; If it is data, set the RS bit
	rjmp command_end ; Jump to command end which ends handling the LCD for this cycle

; This is run after the timer has been decremented once. ie the second stage of the pipeline
command_executed:
	cpi r16, 3 ; Check whether the timer is 3
	brne lcdE_set ; If it isn't then LCDE has already been set (which is the purpose of the second step of the pipieline)
	lcd_set LCD_E ; Otherwuse, set the LCD_E bit and end handling this section of the pipeline
	rjmp command_end

; This will be run if the timer is 2 or 1
lcdE_set:
	lcd_clr LCD_E ; Clear the LCD_E bit regardless of the type
	cpi r16, 1 ; Check whether the ccounter is 1
	brne command_end ; If not, the timer was 2 and jump to the end. Stop handling this section
	inc r16 ; Otherwise, set the timer back to 2
	sts LCD_timer_var, r16 ; And out it to the global variable
	ldi r17, 2 ; Set the type to 2 (2 indicates time to start the waiting loop)

	
	sts LCD_array+1, r17 ; Change the current type of the element to 2
    clr r17
	out DDRF, r17 ; Set PORTF to input and enable pull up resistors
	out PORTF, r17
	lcd_set LCD_RW ; Set the RW bit
	cpi r18, 1 ; Check if, before being set to 2, the type was data (0) or a command (1)
	brne skip_jump_to_push_button ; If it was data, need to clear the RS bit before jumping to push button handling
	rjmp push_button_continue ; Otherwise, jump straight to push button handling
skip_jump_to_push_button:
	lcd_clr LCD_RS
	rjmp push_button_continue
command_end:
	dec r16 ; Decrement the timer variable for the current start item of the queue
	sts LCD_timer_var, r16
	rjmp push_button_continue ; Jump to the push button handler
		





; Run after the type has been set to 2
waiting_start:
	
	cpi r16, 2 ; Check the timer
	breq mili_wait2 ; If it is 2, set the LCD_E flag
	cpi r16,1
	breq timer_decrement_wait ; If it is 1, simply continue waiting
	in r17, PINF ; Otherwise (when the timer is 0), read in from PORTF. Need a delay here to allow the change from output to input 
	lcd_clr LCD_E ; Clear the E flag
	sbrc r17, 7 ; Check whether the busy flag is set, if not (not busy) skip the reset
	rjmp reset_timer
	lcd_clr LCD_RW ; If it is not busy, clear the RW flag
	ser r17 ; And set the LCD PORTF back to output
	out DDRF, r17
	; Move the queue forward one here. The item at the start has been completely handled
	queue_pop
	rjmp push_button_continue ; Jump to handling the push buttons
mili_wait2:
	lcd_set LCD_E ; Set the E flag

; Decrement the timer used for waiting foro the LCD busy flag
timer_decrement_wait:
	dec r16 
	sts LCD_timer_var, r16
	rjmp push_button_continue

; Reset the timer if the busy flag is still set
reset_timer:		
	ldi r16,2
	sts LCD_timer_var, r16



;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END LCD PIPELINE							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START PUSH BUTTON							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;


; Used to jump to from the LCD handling when it finishes
push_button_continue:
	in r25, PIND ; Read in the voltages from PORTD. One bit will be low if a push button is pressed
	cpi r25, 0xFF ; Check whether either button is pressed
	brne check_others ; If one is pressed, check which one
	rjmp neither_button

; Will run this when one button has been pressed
check_others:	
	cpi r25, 0xFE ; If the bit 0 is low, PB0 has been pressed
	breq push0

	cpi r25, 0xFD ; If the bit 1 is low, PB1 has been pressed
	breq push1
	rjmp do_nothing ; Check to make sure it is only one of those two bits that can be 0

; Handle if PB1 has been pressed (Open the door)
push1:
	lds r16, voltagesSeen0 ; Load the voltage history of PB0
	lsl r16 
	ori r16, 1 ; Add another high voltage tick as the most recent entry
	sts voltagesSeen0, r16

	
	lds r16, voltagesSeen1 ; Load the voltage history of PB1
	lsl r16
	sts voltagesSeen1, r16 ; Add a low voltage tick as the most recent entry

	cpi r16, 0xFE ; Check whether it has only just been pressed (only the latest entry is low voltage)
	; Push button 1 is pressed if equal (Open door) 
	breq open_door
	rjmp do_nothing

open_door:
	mov r16, status
	andi r16, 0b00011100
	cpi r16, 0 ;If door was opened in running mode
	breq opened_when_running 
	cpi r16, 0b00001000 ;If door was opened in paused mode
	breq opened_when_running
	rjmp opened_in_entry ; Works for finished as well

opened_when_running:
	ori status, 0b00011100 ; Set the door to open and the when opened bit to 0
	; Setting the when opened bit to 0 means that when it is closed, it will enter pause mode
	rjmp set_led
opened_in_entry:
	ori status, 0b10011100 ; Set the mode to open and the when opened bit to 1
	; When the door is closed from this state, it will enter entry mode
set_led: ; Set the top LED when the door is opened
	ldi r16, 1
	out PORTA, r16
	pop r25
	rjmp show ; Jump to the show section of the keypad handler to update the LCD
	; Works since show is followed immediately by reti and this handler now only need to reti
	
; If PB0 has been pressed		
push0:
	lds r16, voltagesSeen0 ; Load the voltage history of PB0
	lsl r16
	sts voltagesSeen0, r16 ; Store the latest tick as a low voltage
	lds r25, voltagesSeen1 ; Load the voltage history of PB1
	lsl r25
	ori r25, 1
	sts voltagesSeen1, r25 ; Set the latest tick to a high voltage
	cpi r16, 0xFE ; Only interpret as a button press if the latest bit is the only low voltage tick
	brne do_nothing
	; Push button 0 has been pressed here (Close door)
	if_in_mode 'D' ; Only do anything if the door is opened 
	brne door_already_closed
	
	andi status, 0b11100011
	mov r16, status
	andi r16, 0b10000000 ; Check what mode the door was opened in
	cpi r16, 0 ; If the opened in entry/finish mode bit is set, return to entry mode
	brne to_entry_mode
	 ; Clear the status
	ori status, 0b00001000 ; Otherwise set the status to paused
	rjmp turn_off_door_led
to_entry_mode:
	changeMode 'E' ; Set the status to entry mode

; Turn off the top LED
turn_off_door_led:
	in r16, PORTA
	andi r16, 0b11111110
	out PORTA, r16
	pop r25
	; Jump to the show section of the keypad handler to update the LCD
	; Works since show is followed immediately by reti and this handler now only need to reti
	rjmp show

; Runs if the close door button is pressed when the door is closed 
door_already_closed:
	rjmp do_nothing

; Runs if the input is all high voltage
neither_button:
	; Set the latest tick to a high voltage for both PB1 and PB0
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
	; Increment the counter if it has not reached the necessary value (20)
	sts Timer2Counter, r24
	sts Timer2Counter+1, r25
	pop r25
	reti

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END PUSH BUTTON								;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END TIMER2 SEGMENT							;
;						LCD & PUSH BUTTONS							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START TIMER0 SEGMENT						;
;							KEYPAD									;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;


timer0Int:

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START KEYPAD PIPELINE						;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;


	if_in_mode 'D' ; Check to see if the door is open. If so, do nothing
	breq ignore_input
	rjmp keypad_continue
ignore_input:
	reti ; Handle no input if it is open

keypad_continue:
	ldi r17, 0b11101111 ;Load the initial column mask
	mov r16, colnum
setcolmask: ;Loop to position the column mask. Depends on colnum
	cpi r16, 0
	breq colloop
	lsl r17
	ori r17, 1
	dec r16
	rjmp setcolmask

colloop:
	cpi colnum, 4 
	brne collnext ; If all keys are not scanned yet, keep scanning!
	andi status, 0b10111111 ; Turn off debounceFlag if no keys are currently pressed

	clr colnum ; Reset colnum
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
	

convert:
	ser r17
	sts untouched, r17
	clr r17
	sts beep_overflow, r17
	; Check the debounce flag in status
	mov r17, status
	andi r17, 0b01000000
	cpi r17, 0
	breq dont_stop
	
	reti

dont_stop:
	ori status, 0b01000000 ; Set the keypad debounce flag
	cpi colnum, 3
	breq letters ;Handle the letters 

	cpi r19, 3
	brne numbers ;Handle the bottom symbols
	rjmp symbols

; If the column/row isn't 3, the button is a number
numbers:
	if_in_mode 'E' ;Check if it is in entry mode
	brne change_power ;If not, check if it is in change power mode
	rjmp in_entry_mode

change_power:
	if_in_mode 'X'
	brne ignore_number ;If it is in neither change power or entry, ignore the press

in_entry_mode:
	;Algorithm to get num from keypad position
	mov r17, r19
	lsl r17
	add r17, r19
	add r17, colnum
	subi r17, -1
	;End Algorithm
	
	if_in_mode 'X' ;Check whether it was in power change mode
	brne not_power
	changeMode 'E' ;Set it back to entry
	cpi r17, 4 ;If the number is greater than 3 ignore it
	brge ignore_number
	andi status, 0b11111100
	or status, r17 ;Set the power level in the status reg
	;out PORTC, status ;Debug display on leds
	;clr r16
	
	;sts num_overflow_int, r16 ; If a number has been pressed,
	cpi r17, 1 ; Check what the button pressed was and set the power level appropriately
	breq set_powerlvl_1
	cpi r17, 2
	breq set_powerlvl_2
	ldi r16, 31 ; 31 will result in a 100% duty cycle
	sts num_needed, r16
	ldi r16, 0b11111111
	out PORTC, r16
	rjmp show
set_powerlvl_1:
	ldi r16, 8 ; 8 will result in 25% duty cycle
	sts num_needed, r16
	ldi r16, 0b00000011
	out PORTC, r16
	rjmp show
set_powerlvl_2:
	ldi r16, 16 ; 16 will result in 50% duty cycle
	sts num_needed, r16
	ldi r16, 0b00001111
	out PORTC, r16
	rjmp show
not_power:
	rjmp convert_continue ; If it was not in power mode, jump to converting the number normally
ignore_number:	
	rjmp screen_end

letters:
	
	if_in_mode 'X' ; If in power change mode and a letter is pressed, set it to Entry mode before handling the press
	brne interpret_letters ; Otherwise, handle the letter press
	changeMode 'E'
interpret_letters:
	if_in_mode 'P' ; If in paused mode, ignore all letter presses
	brne check_finished_mode
	rjmp screen_end
check_finished_mode:
	if_in_mode 'F' ; If in finished mode, ignore all letter presses
	brne continue_with_letter
	rjmp screen_end

; Will be entered if not in paused, finished or door open mode
continue_with_letter:
	cpi r19, 0;A - Change the power level
	breq change_powerlvl
	cpi r19, 1;B - No behaviour for B
	breq no_change

	if_in_mode 'R' ; Check in running mode. If not, don't handle more/less buttons
	brne no_change

	cpi r19, 2;C  - Add 30 seconds
	breq add_30
	cpi r19, 3;D - Subtract 30 seconds
	breq sub_30

change_powerlvl:
	;Set mode to power lvl change if in entry mode only
	if_in_mode 'E'
	brne no_change
	changeMode 'X'
	rjmp show
no_change:
	rjmp screen_end

; Adds 30 seconds
add_30:
	cpi seconds, 30 ; If the sconds counter is less than 30, just add 30
	brlo easyadd
	cpi minutes, 99
	breq no_time_update
	subi seconds, 30 ; Otherwise, subtract 30 and add one to minutes
	inc minutes
	rjmp show
no_time_update:
	ldi seconds, 99
	rjmp show
; Run if 30 wants to be added to seconds which is less than 30
easyadd:
	subi seconds, -30 ; Just add 30 to seconds
	rjmp show

; Subtracts 30 seconds
sub_30: 
	cpi seconds, 30 ; If seconds is greater than 30 seconds, simply subtract 30
	brge easysub
	cpi minutes, 0 ; Otherwise, if there is a minute to subtract from, decrement that and add 30 seconds
	breq easiersub
	dec minutes
	subi seconds, -30
	rjmp show

; Subtract 30 from seconds >= 30
easysub:
	subi seconds, 30
	rjmp show
; Stop the microwave if 30 seconds is subtracted from a time less than 30 seconds
easiersub:
	ldi seconds, 0
	rjmp stopMicrowave ; Jumps to section in motor interrupt but is fine since all handling is finished and a reti is all that is needed

; If the button pressed is in the bottom line of symbols
symbols:
	if_in_mode 'X' ; If in power entry mode, change to entry mode before interpretting symbols
	brne interpret_symbols
	changeMode 'E'
interpret_symbols:
	ldi r16, 0
	cpi colnum, 0 ; Check if we have a star
	breq start_button
	cpi colnum, 1 ; or if we have zero
	breq zero
	brne stop_button ; Otherwise, it is a hash
zero:
	if_in_mode 'E' ; Only handle a 0 press if in entry mode
	breq handle_zero
	rjmp screen_end
handle_zero:
	clr r17 ; Set r17(the number pressed) to 0
	rjmp convert_continue ; Handle as a normal number
stop_button:
; Entry mode - Clear all currently entered time
; Running Mode - Enter Pause mode
; Pause/Finished Mode - Enter entry mode and clear time etc (reset)		
	if_in_mode 'E' ; Entry mode
	breq clear_time
	if_in_mode 'R' ; Running mode
	breq pause_button

clear_time:
	;out PORTC, status
	clr seconds
	clr minutes
	if_in_mode 'P'
	brne continue_clear_time

	ldi r23, low(Turntable<<1)+4
	sub r23, ZL
	subi r23, -low(Turntable<<1)

	mov ZL, r23

	ldi r23, 0b00100000
	eor status, r23
continue_clear_time:
	changeMode 'E' ; Have this in case it is in finished mode or some mode that is not entry
	rjmp show

pause_button:
	changeMode 'P' ; Change the mode to pause if in running mode
	clr r16
	out PORTB, r16 ; Stop the motor spinning
	rjmp show
start_button: ;start_button 
; Entry Mode  - Change to running mode
;	Run the time entered. If no time entered, run for 1 min
; Running Mode - Add 1 min
; Pause Mode - Resume running
	if_in_mode 'R'
	;Check if in running mode
	breq add_one_min
	if_in_mode 'F' ;Check if in finished mode and if so do nothing
	breq s_button_does_nothing
	;Otherwise, in entry mode and handle that
	clr r0
	cpi seconds, 0
	cpc minutes, r0 ; Check whether there is any time input already.
	breq add_one_min ; If not then add one minute and start. Otherwise, just run
	rjmp start_running

s_button_does_nothing:
	rjmp screen_end

add_one_min: ; Add one minute if it is not already 99
	cpi minutes, 99
	brge set_seconds_ninenine
	inc minutes
	rjmp start_running
set_seconds_ninenine:
	ldi seconds, 99
start_running:
	if_in_mode 'E'
	brne no_variable_update
	clr r23
	sts num_overflow_int, r23 ; Clear the number of motor interrupts so the motor starts a soon as start is pressed
	ldi r23,78
	sts turntable_seconds, r23
no_variable_update:
	changeMode 'R' ;Set the status to running mode
	do_lcd_command_a 0b00000001
	do_lcd_command_a 0xCF		; Cursor to bottom right
	do_lcd_data_a 'C'			; Write closed. Must always be closed to run this section
	do_lcd_command_a 0x80  		; Cursor on top line
	bin_to_txt minutes			; Write the minutes
	do_lcd_data_a ':'			; Write the colon
	bin_to_txt seconds			; Write the seconds
	sbrs status, 5 ;will play the second half of the string if we're spinning the other way
	adiw z, 4

	lpm r22, z
		
	do_lcd_command_a 0x8F ; Get to top right position on LCD

	mov_lcd_data_a r22 ; Write out current turntable position
	;do_lcd_data_a 0
	
	sbrs status,5
	sbiw z, 4

	rjmp screen_end



convert_continue:
	;out PORTC, r17
	ldi r23, 10 ; Used for the mul command
		cpi seconds, 1 ; If there is already a single second digit, check 10s of seconds
		brge tensPlace
		cpi minutes, 1
		brge minutesOnes
		add seconds, r17 ; Otherwise, just add the pressed digit to seconds
		rjmp show
	tensPlace:
		cpi seconds, 10 ; If there is already a 10s second digit, check minutes
		brge minutesOnes
		cpi minutes, 1 ; Needed for a 0 where the time is 0m:0s where m and s != 0
		brge minutesOnes
		mul seconds, r23 ; Otherwise, multiply seconds by 10 and add the new pressed number
		mov seconds, r0
		add seconds, r17
		rjmp show
	minutesOnes:
		cpi minutes, 10 ; If there is already a 10s of minutes, don't handle the press
		brlo continueMinutes ; Relative branch too far away, need an rjmp
		rjmp screen_end
		continueMinutes:
		mul minutes, r23 ; Otherwise, multiply minutes by 10
		mov minutes, r0
		
		; Get the number of 10s in the seconds and make it the first digit of minutes
		secondsShift:
		cpi seconds, 10 
		brlo updateSeconds
		inc minutes
		subi seconds, 10
		rjmp secondsShift
		; Need to multiply seconds by 10 and add the new key press
		updateSeconds:
		mul seconds, r23
		mov seconds, r0
		add seconds, r17

		
show:

	do_lcd_command_a 0b00000001 ; clear display	

	do_lcd_command_a 0x80  	; cursor on top line
	if_in_mode 'X'
	breq showpower_txt
	rjmp showtime_txt
showpower_txt:
	do_lcd_data_a 'S'
	do_lcd_data_a 'e'
	do_lcd_data_a 't'
	do_lcd_data_a ' '
	do_lcd_data_a 'P'
	do_lcd_data_a 'o'
	do_lcd_data_a 'w'
	do_lcd_data_a 'e'
	do_lcd_data_a 'r'
	do_lcd_data_a ' '
	do_lcd_data_a '1'
	do_lcd_data_a '/'
	do_lcd_data_a '2'
	do_lcd_data_a '/'
	do_lcd_data_a '3'

	rjmp show_bottom_line

showtime_txt:
	bin_to_txt minutes		; Write out the minutes register

	do_lcd_data_a ':'		; Write out colon
	bin_to_txt seconds		; Write out the seconds register
	

show_bottom_line:
	if_in_mode 'E'			; If not in entry mode, print group number on bottom line
	brne check_power_level
	rjmp show_group
check_power_level: 
	if_in_mode 'X'			; Power entry mode counts as entry mode
	brne no_groupe
show_group:
	do_lcd_command_a 0xC0
	do_lcd_data_a 'A'
	do_lcd_data_a '1'

no_groupe:
	do_lcd_command_a 0xCF 	; Get cursor to bottom right
	if_in_mode 'D'			; Show O or C depending on door state
	breq door_is_open_and_I_want_to_show_this
	do_lcd_data_a 'C'
door_is_open_and_I_want_to_show_this:
	do_lcd_data_a 'O'

screen_end:
		sbrs status, 5 ;will play the second half of the string if we're spinning the other way
	adiw z, 4

	lpm r22, z
	
	do_lcd_command_a 0x8F ; Get to top right position on LCD

	mov_lcd_data_a r22 ; Write out current turntable position
	;do_lcd_data_a 0
	
	sbrs status,5
	sbiw z, 4
	reti

;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END KEYPAD PIPELINE							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START TIMER1 SEGMENT						;
;						MOTOR & TURNTABLE							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						START MOTOR/TURNTABLE						;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;


timer1OVF:
	if_in_mode 'R' ; Check whether in running mode, otherwise ignore the interrupt
	breq continue_motor_overflow
	clr r22 ; If not in running mode, make sure motor is off
	out PORTB, r22

	if_in_mode 'F'
	breq handle_beep
	reti

handle_beep:
	lds r22, num_overflow_int
	cpi r22, 30
	breq check_beep
	inc r22
	sts num_overflow_int, r22
	reti
check_beep:
	clr r22
	sts num_overflow_int, r22
	lds r22, beepnum
	cpi r22, 6
	breq no_beep
	inc r22
	sts beepnum, r22
	clr r23
	sbrc r22, 0
	ser r23
	out PORTG, r23
no_beep:
	reti
; If it is in running mode, start from this label
continue_motor_overflow:
	lds r22, turntable_seconds
	cpi r22, 78 ; If there have been 78 interrupts, rotate the table
	breq turntable_update ; Otherwise, skip to motor handling
	rjmp no_turntable_update
turntable_update:	
	ldi r22, high(Turntable+2<<1) ;we check if Z has incremented 4 times
	cpi ZL, low(Turntable+2<<1) ;if so, we must reset its position in the string
	cpc ZH, r22
	brne no_reset_needed
	ldi ZL, low(Turntable<<1)
	ldi ZH, high(Turntable<<1)
no_reset_needed:
	sbrs status, 5 ;will play the second half of the string if we're spinning the other way
	adiw z, 4
	;sbrs status,5
	;sbiw z, 4
	lpm r22, z
	adiw z,1
	
	do_lcd_command_a 0x8F ; Get to top right position on LCD
	;bin_to_txt ZL
	mov_lcd_data_a r22 ; Write out current turntable position 
	;do_lcd_data_a 0
	
	sbrs status,5
	sbiw z, 4


spin_clockwise:
	;brne no_turntable_handling ; Otherwise do nothing except for increment the pointer
	;clr r23 ; Reset the second counter to 0

	; Best way will be to do it every 77 or 78 interrupts (because it needs 8 cycles every 20 seconds) 
	; TODO - Handle the turntable rotation using th Z pointer

	clr r22 ; Reset the interrupt timer for the turntable
	;no_turntable_handling:
	;sts turntable_seconds, r23 ; Store the new value of turntable seconds (will be 0 just after a turntable update)
no_turntable_update:
	inc r22
	sts turntable_seconds, r22 
	lds r22, num_overflow_int ; Check the number of interrupts so far
	cpi r22, 0 ; If it is 0, start the motor and remove the group name from bottom
	breq update_motor_state ; Otherwise, leave the motor doing what it is
	rjmp already_started
update_motor_state:
	ser r23
	out PORTB, r23
	;do_lcd_command_a 0b00000001 ; Clear the screen
	do_lcd_command_a 0x80  		; Cursor on top line
	bin_to_txt minutes			; Write the minutes
	do_lcd_data_a ':'			; Write the colon
	bin_to_txt seconds			; Write the seconds
	
	
already_started:
	inc r22	; Increment the number of interrupt count
	lds r17, num_needed
	cp r22, r17 ; Check whether the number of interrupts needed to turn off the motor equals the number of interrupts received
	brne dont_turn_off
	clr r17 ; If so, clr r17, which will be written to PORTB (turn off motor)
	out PORTB, r17
	
; If there have not been enough interrupts to turn off the motor, skip to here	
dont_turn_off:

	cpi r22, 30 ; Check whether it is time to turn the motor on next interrupt
	brne dont_turn_on_motor
	

	clr r22 ; clr r22 to be written to the number of interrupts received
	
	cpi seconds, 1 ;If there is one second left, the time is up or need to decrement minutes
	brne not_finished 
	cpi minutes, 0 ; If the minutes is 0, the time is up
	breq stopMicrowave
not_finished:
	cpi seconds, 0 ; If the seconds is 0, need to decrement minutes
	breq changeMins
	dec seconds ; Otherwise go down by 1 second and update screen
	rjmp update_screen

; If there are no more seconds to decrement, change mins
changeMins:
	dec minutes 
	ldi seconds, 59 ; Go from a whole minute to 59 seconds
	rjmp update_screen
stopMicrowave:
	clr r23
	sts num_overflow_int, r23
	inc r23
	sts beepnum, r23
	ser r23
	out PORTG, r23
	dec seconds ; Will set seconds to o0
	changemode 'F' ; Set the microwave to finished mode
	rjmp done_screen ; Display the finished screen	

; If there have not been 30 interrupts jump here. This will be run in every case
dont_turn_on_motor:
	sts num_overflow_int, r22 ; Store the number of interrupts passed here
	reti
update_screen:
	; Do not need to clear the screen because it is cleared at the start of running mode
	;do_lcd_command_a 0x80  	; Cursor on top line
	;bin_to_txt minutes		; Write out the minutes
	;do_lcd_data_a ':'		; Write out the colon
	;bin_to_txt seconds		; Write out the seconds
	;do_lcd_data_a 'G'
	rjmp dont_turn_on_motor



done_screen: ; Print out the necessary to the screen
	do_lcd_command_a 0b00000001 ; Clear the screen	
	do_lcd_command_a 0x80 		; Cursor on the top line
	do_lcd_data_a 'D'
	do_lcd_data_a 'o'
	do_lcd_data_a 'n'
	do_lcd_data_a 'e'
	do_lcd_command_a 0xC0		; Cursor on the bottom line
	do_lcd_data_a 'R'
	do_lcd_data_a 'e'
	do_lcd_data_a 'm'
	do_lcd_data_a 'o'
	do_lcd_data_a 'v'
	do_lcd_data_a 'e'
	do_lcd_data_a ' '
	do_lcd_data_a 'f'
	do_lcd_data_a 'o'
	do_lcd_data_a 'o'
	do_lcd_data_a 'd'

	sbrs status, 5 ;will play the second half of the string if we're spinning the other way
	adiw z, 4

	lpm r23, z
	
	do_lcd_command_a 0x8F ; Get to top right position on LCD

	mov_lcd_data_a r23 ; Write out current turntable position
	;do_lcd_data_a 0
	
	sbrs status,5
	sbiw z, 4

	do_lcd_command_a 0xCF	; Cursor to boottom right corner
	do_lcd_data_a 'C'		; Door will always be closed here
	

	ldi r23, low(Turntable<<1)+4
	sub r23, ZL
	subi r23, -low(Turntable<<1)

	mov ZL, r23

	ldi r23, 0b00100000
	eor status, r23
	rjmp dont_turn_on_motor
		


;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END MOTOR/TURNTABLE						;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;




;-------------------------------------------------------------------;
;																	;
;																	;
;																	;
;						END TIMER1 SEGMENT							;
;						MOTOR & TURNTABLE							;
;																	;
;																	;
;																	;
;-------------------------------------------------------------------;


timer3Int:
	lds r16, untouched
	sbrs r16, 0
	reti
	ser r16
	out PORTG, r16
	lds r16, beep_overflow
	cpi r16, 2
	breq turn_off
	inc r16
	sts beep_overflow, r16
	reti
turn_off:
	clr r17
	out PORTG, r17
	sts untouched, r17
	sts beep_overflow, r17
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

