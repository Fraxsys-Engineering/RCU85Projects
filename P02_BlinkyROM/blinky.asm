; Test program for 8085 SBC.  Blinks an LED on at PIO0-PA0 forever.
; ROM Memory Map Version
; Note the first jump that seems to do nothing.  This is for the power-on jump.
; The ROM is normally at 8000 and the RAM at 0000.  At reset, the ROM maps to
; both 0000 and 8000.  This condition persists until the A15 line goes high.
; The first instruction is located at 8000, but the PC will actually be 0000
; when it executes.  The jump to 8003 will cause the power-on flip flop to
; clear and the normal addressing scheme will be activated.  At this point,
; the RAM and ROM use their normal addressing.

	ORG 0000h
	
	JMP INIT	; System Init

INIT:   MVI A, 0Fh  ; Set DDR on PIO0-A3..A0 to OUT
        OUT 80h

START:  MVI A, 01h	; LED on
        OUT 81h

;        CALL DELAY1
        
        MVI A, 0ffh	; Delay
        MOV B, A
D0PT1:  DCR A
D0PT2:  DCR B
        JNZ D0PT2
        CPI 00h
        JNZ D0PT1

        MVI A, 00h	; LED off:q
        OUT 81h

        MVI A, 0FFh	; Delay
        MOV B, A
D1PT1:  DCR A
D1PT2:  DCR B
        JNZ D1PT2
        CPI 00h
        JNZ D1PT1

        JMP START	; Loop forever

;DELAY1: MVI A, 0ffh	; Delay
;        MOV B, A
;DPT1:   DCR A
;DPT2:   DCR B
;        JNZ DPT2
;        CPI 00h
;        JNZ DPT1
;        RET
