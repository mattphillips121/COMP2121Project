@ECHO OFF
"C:\Program Files (x86)\Atmel\AVR Tools\AvrAssembler2\avrasm2.exe" -S "C:\Users\Matthew\Documents\GitHub\COMP2121Project\labels.tmp" -fI -W+ie -C V3 -o "C:\Users\Matthew\Documents\GitHub\COMP2121Project\mySpeaker.hex" -d "C:\Users\Matthew\Documents\GitHub\COMP2121Project\mySpeaker.obj" -e "C:\Users\Matthew\Documents\GitHub\COMP2121Project\mySpeaker.eep" -m "C:\Users\Matthew\Documents\GitHub\COMP2121Project\mySpeaker.map" "C:\Users\Matthew\Documents\GitHub\COMP2121Project\mySpeaker.asm"
