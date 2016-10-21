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

Reset:
	

halt:
	rjmp halt
