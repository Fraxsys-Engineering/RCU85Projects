; Test Peripheral Card - Steering & User I/O Board
; Hardware Version : 1.0c (Aug 2022)
; 
; Basic Function API
; ---
; iob_configure - TESTED OK
; call args:    none.
; call ret:     none.
; desc:         Setup PIO controller for this I/O board
;               Note: currently set to hard IO address: 0x20 .. 0x2F
;                   PORT-A - INPUT
;                   PORT-B - OUTPUT/0x02
;                   PORT-C - OUTPUT/0x20 (only 6 bits)
;
; -
; iob_user_inp - TESTED OK
; call args:    none
; call ret:     user octet value in 'A' reg
; desc:         Read the 8 user input switches 
;
; -
; iob_disp_refresh - TESTED OK
; call args:    none
; call ret:     none
; desc:         refresh the 4 7-seg. nibble registers from the 4 shadow
;               registers. call this after modifying the shadow registers
;               to update a 7-seg ASCII-NIBBLE indicator or a decimal-
;               point indicator state.
;               Part of the low level display controller, Version 2.0
;               Ver. 2.0
;               *** Refreshes the Displays ***
;
; - 
; iob_user_disp_raw
; call args:    'A'     data nibble, only uses d0..d3
;               'B'     nibble addr, 0..3
; call ret:     none.
; desc.         Perform a low level nibble update to one of the 4 7-Seg.
;               nibble shadow registers, DOES NOT update the displays on
;               the I/O Steering Controller Board! Call iob_disp_refresh()
;               to update the 7-seg. displays on the board.
;               Ver. 2.0
;
; -
; iob_user_disp_dpl (left)
; iob_user_disp_dpr (right)
; call args:    A   true := 'enable dp', false := 'disable dp'
; call ret:     none.
; desc.         enable or disable 7-seg LED decimal point lights w/o
;               affecting the remaining digits on the display.
;               *** Refreshes the Displays ***
;
; -
; r_iob_user_disp_dpl (left)
; r_iob_user_disp_dpr (right)
; call args:    none.
; call ret:     A := bool state of the decimal-point. true := On
; desc.         Read back the current state of the decimal point, from
;               the shadow registers.
;
; - 
; iob_user_disp_hexnib
; call args:    'A'     data nibble to display, 0..9,A..F
;               'B'     1 := right disp, 0:= left disp
; call ret:     none.
; desc.         Write an ASCII-HEX nibble value to the left or right 7-Seg.
;               LED display. Uses LUTs to convert the value in 'A' into
;               raw control codes and uses the raw display call to 
;               write the values.
;
; - 
; iob_user_disp_hexbyt
; call args:    'A' holds BYTE to write out. Both left and right 7-Seg LEDs are updated!
; call ret:     none.
; desc.         Uses the above calls to create the highest abstraction 
;               level of use in the 7Seg displays in order to generate 
;               a display showing a full byte, "00" to "FF" representing
;               0x00 to 0xff in value (reg-A).
;               Left and right decimal points are controlled by a 
;               separate call.
;
; -
; iob_reset
; call args:    none
; call ret:     none
; desc.         reset the I/O Steering module board to a known good
;               starting state. The displays are cleared (all segments off)



; 256 Byte RAM Bank locations - Basic mode, no addl. RAM
NVM_BANK0   EQU     0000H           ; FRAM Memory Board (NV RAM)
RAM_BNK0    EQU     8000H           ; 8155 Bank-0
RAM_BNK1    EQU     9000H           ; 8155 Bank-1
RAM_BNK2    EQU     0A000H          ; 8155 Bank-2
RAM_EXPA    EQU     7000H           ; 4K RAM - Expansion Board Rev 1.0
STACK       EQU     RAM_BNK0+0100H  ; use all 256 bytes in PIO-2 (Bank-0)
STCKBOT     EQU     RAM_BNK0+00FFH  ; first addr used in the stack
STCKTOP     EQU     00H             ; LSB byte addr for top-of-stack (0xxx00) MSB same as 'STCKBOT'
STCKVCHK    EQU     RAM_BNK0        ; Address to check for stack overflow, should remain watermarked.

IOSDC_BANK  EQU     0A0H    ; Location of the I/O Steering Drive Controll Board (0xA000)

REG_CSR     EQU     IOSDC_BANK+0
CSR_VAL     EQU     01001110b
                    ; 01 TIMER         HALT
                    ;  0 PortB INTR    OFF
                    ;  0 PortA INTR    OFF
                    ; 11 PortC MODE    3 (All Outputs)
                    ;  1 PortB DIR     Output
                    ;  0 PortA DIR     Input
REG_A       EQU     IOSDC_BANK+1 ; INPUT  1-of-2 bytes values selected by PortB[0]
REG_B       EQU     IOSDC_BANK+2 ; OUTPUT 7-seg LED display + PortA read bank select
REG_C       EQU     IOSDC_BANK+3 ; OUTPUT steering and driver control
PB_VAL      EQU     02H     ; PortB Init. : bit-1 should stay at '1' for the 7-seg latch LOAD pulse. select Bank-1 input (SI1)
PC_VAL      EQU     20H     ; PortC Init.


; =====================================================================
; --- Testing code ----------------------------------------------------

DELAYL      EQU 0FFh        ; loop "heartbeat"
DELAYH      EQU 001h
USRINP      EQU 9000H       ; RAM addr to cache the last userinput (tested read)
LOOPHB      EQU 9001H       ; Heatbeat LED state (bool) [0,1]
LOOPDL      EQU 9002H       ; Cached 16-bit delay value. (Little Endian)
LOOPDH      EQU 9003H

            ORG NVM_BANK0
	
            JMP MAIN	; System Init

            ;ORG RAM_EXPA    ; Jump to run in the 4K RAM

MAIN:       LXI SP, STACK   ; Setup the stack (!) Stack pre-decrements on a PUSH
                            ; so the SP is initialized to 1 byte addr higher 
                            ; than the first PUSH address location (0x20FF)
                            ; Watermarking has to start at the next lower byte
                            ; (0x20FF)
            LXI H, STACK    ; watermark the stack (H,L) <--> M 0x2000..0x20FF
SWMARK:     DCX H           ; (!) pre-decrement HL to get 1st writable address
            MVI A, 055H
            MOV M,A
	        MVI A, STCKTOP
            CMP L           ; check for L == 0x00 (0x2000)
            JNZ SWMARK      

OPTTEST:    ; --- Tests (optional)
            ;JMP TSTIOSDC ; uncomment to skip basic tests
            MVI A, 00H      ; Test Stack push (address decoding test + stack ops.)
            PUSH PSW
            LXI H, 1122H
            PUSH H
            INX H
            PUSH H
            ; restore SP
            POP H
            POP H
            POP H
            MVI A, 01H
            PUSH PSW
            POP PSW

TSTIOSDC:   ; --- Test the I/O Steering Drive Control Board
            CALL iob_configure  ; initialize the board via the 8155 PIA
            MVI A,00H
            STA LOOPHB
            MVI A,DELAYH
            STA LOOPDH
            MVI A,DELAYL
            STA LOOPDL
            ; test read of the 8-bit user switch --> USRINP (RAM)
LOOPRIN:    CALL iob_user_inp
            STA USRINP
            ; loop math
            LHLD LOOPDL  ; HL <- (LOOPDL/16-bit)
            DCX H
            SHLD LOOPDL
            MOV A,L
            ORA H
            JNZ looppr0
            ; 0x0000 : flip the left Dp bit
            LDA LOOPHB
            XRI 08H
            STA LOOPHB
            CALL iob_user_disp_dpl ; update diplay cache, dp status only
            ; reset the delay counter
            MVI A,DELAYH
            STA LOOPDH
            MVI A,DELAYL
            STA LOOPDL
            ;; --- save user-byte into raw 7-seg shadow regs. to test them
            ;STA disp_shd_l_abcd ; low-nibble
            ;STA disp_shd_r_abcd ; low-nibble
            ;RRC
            ;RRC
            ;RRC
            ;RRC
            ;STA disp_shd_l_efgp ; high-nibble
            ;STA disp_shd_r_efgp ; high-nibble
            ;CALL iob_disp_refresh ; update 7-Seg. LED 'bits' from the raw 4-bit shadow registers.
looppr0:    LDA USRINP   ; get the user input back into Reg-A before the next call.
            ; --- decode user input as a full byte and update dual 7-seg diplays with the ASCII-HEX value.
            CALL iob_user_disp_hexbyt ; this also updates the display
            JMP LOOPRIN


TESTEND:    JMP TESTEND            




; =====================================================================

; memory for shadow register copies of PortB,C on the 8155 PIO
shdw_regb:  db  PB_VAL ; bit-1 should stay at '1' for the 7-seg latch LOAD pulse.
shdw_regc:  db  PC_VAL
; PortA is an input and does not need a shadow, but this value is the
; last read one and allows re-use without having to re-read the port.
shdw_rega:  db  00h

; Tested - OK
iob_configure:
            ; Initialize the 8155 to starting values and setup port DDRs.
            PUSH PSW        ; save Reg A
            MVI A, CSR_VAL
            OUT REG_CSR
            MVI A, PB_VAL
            OUT REG_B
            MVI A, PC_VAL
            OUT REG_C
            POP PSW         ; restore Reg A
            RET


; Read the 8-bit User Input, from SW1 on the board
; Tested - OK
iob_user_inp:
            ; set input to Bank-B, saving the current state
            LDA shdw_regb
            ORI 01H         ; set Bank-Select to Bank-1
            OUT REG_B
            ; Read PortA - cache the value as well.
            IN REG_A
            STA shdw_rega   ; local cache
            ; restore PortB settings
            LDA shdw_regb
            OUT REG_B
            LDA shdw_rega   ; return w/ value in Reg-A
            RET
            
            
            ; --- 7-seg led addressing --------
            ; Reg-B : LS[1,0]
            ; Reg-A : LD[3-0]
            ; +---+---+-----------+-----------+
            ; |LS1|LS0| LEFT DISP |RIGHT DISP |
            ; +---+---+-----+-----+-----+-----+
            ; | 0 | 0 |dcba |     |     |     | 
            ; +---+---+-----+-----+-----+-----+
            ; | 0 | 1 |     |pgfe |     |     | 
            ; +---+---+-----+-----+-----+-----+
            ; | 1 | 0 |     |     |dcba |     | 
            ; +---+---+-----+-----+-----+-----+
            ; | 1 | 1 |     |     |     |pgfe | 
            ; +---+---+-----+-----+-----+-----+
            ; |LD[3-0]|3210 |3210 |3210 |3210 |
            ; +-------+-----+-----+-----+-----+
            ; (p := decimal-point)
            ; -
            ; --- 7 -Segment Identification ---
            ;     b
            ;  +-----+
            ;  |     |
            ; a|  g  |c
            ;  +-----+
            ;  |     |
            ; f|     |d
            ;  +-----+   . p (d.p.)
            ;     e
            ;
            ; ---------------------------------
            
            
            ; Ver 2 of the low level display controller. The workflow
            ; for this version is to:
            ; a) update the shadow registers as needed (raw values & d.p. settings)
            ; b) call iob_disp_refresh() to write back all 4 of the shadow
            ;    registers to the I/O Steering Control board in one 
            ;    "refresh" operation.
            ; In this way, raw segment values and d.p status changes can
            ; be independantly performed without interferring with one
            ; another and be 'orthoganol' in operation.
            ; (!) Shadow Registers hold the LD[3-0] nibble contents only!
            ;     The registers do not represent a raw value for PortB.
            ; +---+---+---+---+---+---+---+---+
            ; | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
            ; +---+---+---+---+---+---+---+---+
            ; | 0 | 0 | 0 | 0 |LD3|LD2|LD1|LD0|
            ; +---+---+---+---+---+---+---+---+
disp_shd_l_abcd: db 00h
disp_shd_l_efgp: db 00h     ; has a d.p. (left)
disp_shd_r_abcd: db 00h
disp_shd_r_efgp: db 00h     ; has a d.p. (right)

; Refresh both 7-Seg LED displays from local nibble cache: disp_shd_*_*
; Tested - OK
iob_disp_refresh:            
            ; Low level operation to refresh the 2 7-segment displays by
            ; copying all 4 shadow regsters to the nibble latches on the 
            ; board.
            ; input: none.
            ; returns: none.
            ; affects: A,B,C,D,H,L
            ; Note: 7-seg nibble registers are updated in reverse, starting at '11'b
            ;       to make the loop more efficient.
            ; Note: All operations on the 7-seg display are done through PortB.
            ; -
            ; PortB
            ; +---+---+---+---+---+---+---+---+
            ; | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
            ; +---+---+---+---+---+---+---+---+
            ; |LD3|LD2|LD1|LD0|LS1|LS0|/LD|xxx|
            ; +---+---+---+---+---+---+---+---+
            ; Write back bit-0 as found in the shadow reg, used for read reg. select.
            ; LD[3..0] raw LED nibble value (reg-A)
            ; LS[1,0]  LED register select  (reg-B)
            ; /LD      pulse LOW to write the new value
            ; =================================
            PUSH PSW        ; PUSH A,[B,C],[D,E],[H,L] + STATUS onto stack
            PUSH B
            PUSH D
            PUSH H
            MVI D,00h       ; setup jump table... DE <- 0x0003
            MVI E,03h       ; use E to count the loop (for B = 3 ; B >= 0 ; B--) part of 16-bit DE reg.
idrf0:      LXI H,disp_shd_l_abcd
            DAD D           ; ...math. HL <- HL + DE, see for-loop in notes above
            MOV A,M         ; Read shadow-reg nibble from addr. 'M' (HL reg) -> A
            RLC
            RLC
            RLC
            RLC             ; rot A left by 4 bits. sets up LD[3-0] value
            ANI 0F0h        ; clear out the lower 4 bits, not used.
            MOV C,A         ; C <- A stash A. Need A for futher logic operations, data will be OR'd into C as a new working reg.
            MOV A,E         ; E -> A : LED reg. select bits
            RLC
            RLC             ; shift left 2 bits
            ORA C           ; A <- A OR C : combined LED raw value and the register address
            MOV C,A         ; A -> C : stash into 'C' now has shifted LD[3-0] | LS[1,0]
            LDA shdw_regb   ; load shadow_reg-PortB -> A
            ANI 01h         ; only save the SI1/SI2 input selector current state. set /LOAD to '0' !
            ORA C           ; A <- A OR C : combines the LED_DATA, LED_SEL and SI1/SI2 with /LOAD set to '0'b
            ; Perform the PortB write sequence for latching value into 4-bit nibble latch
            XRI 02h         ; A : flip /LD bit to '1' before write
            OUT REG_B       ; and write out to PortB (/LD de-asserted)
            XRI 02h         ; clear /LD bit to '0' by flipping it again, to latch the value
            OUT REG_B       ; and write out to PortB w/ /LD set
            XRI 02h         ; flip /LD bit to '1' before write
            OUT REG_B       ; one final write to de-assert /LD again
            STA shdw_regb   ; (shdw_regb) <- A - save latest PortB write back to the shadow reg.
            DCR E           ; E <- E - 1
            JP  idrf0       ; loop if E >= 0. When E=0 and is decremented, value becomes 
                            ; negative (FFh) so Sign 'S' bit in status is set (negative 
                            ; result). JP := Jump-if-positive-result
            POP H           ; restore saved registers.
            POP D
            POP B
            POP PSW
            RET             ; return()

            
; common code used with D.P Manipulation functions
iob_user_dpm_common:
            CPI 00h         ; A == 0? if not then set A to 0x08 and write either value.
            JZ  iobudpc0       ; A = False  (clear D.P.)
            MOV A, M        ; A = True   (set D.P)
            ORI 08H         ; set bit
            JMP iobudpc1
iobudpc0:   MOV A, M
            ANI 0F7H        ; clear bit-3
iobudpc1:   MOV M, A        ; save shadow reg.
            POP H
            ;CALL iob_disp_refresh ; update the displays
            RET


; disp_shd_l_efgp, disp_shd_r_efgp : bit-7 (0x80)
; Tested - OK
iob_user_disp_dpl:
            ; affect the left decimal-point "dot" on the 7-seg display.
            ; does not change any other data on the display.
            ; turn on or off, based on binary state of reg-A, true:=On
            PUSH H
            LXI H, disp_shd_l_efgp
            JMP iob_user_dpm_common ; remaining ops done in common code
            
; Tested - OK
iob_user_disp_dpr:
            ; affect the right decimal-point "dot" on the 7-seg display.
            ; does not change any other data on the display.
            ; turn on or off, based on binary state of reg-A, true:=On
            PUSH H
            LXI H, disp_shd_r_efgp
            JMP iob_user_dpm_common ; remaining ops done in common code


; common code used with D.P Read functions
riob_user_dpm_common:
            MOV A, M
            ANI 08H         ; clear all but bit 3
            CPI 00h         ; A == 0? 
            JZ  riobudpc0   ; Yes.
            LDA 01H         ; No.
riobudpc0:  POP H
            RET
            
; Tested - OK
r_iob_user_disp_dpl:
            PUSH H
            LXI H, disp_shd_l_efgp
            JMP riob_user_dpm_common

; Tested - OK
r_iob_user_disp_dpr:
            PUSH H
            LXI H, disp_shd_r_efgp
            JMP riob_user_dpm_common


led_7seg_lut:
            ; 7-seg ASCII-HEX to Raw conversion Look up Table
            ;  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
            db 3Fh,0Ch,76h,5Eh,4Dh,5Bh,79h,0Eh,7Fh,4Fh,6Fh,79h,33h,7Ch,73h,63h
            ; note: DOES NOT turn on dp segments, that needs to be adjusted
            ;       when appropriate.
            ; b[7..4] segment order: dp,g,f,e   (LS0 := 1)
            ; b[3..0] segment order: d,c,b,a    (LS1 := 0)
; Tested - OK
iob_user_disp_hexnib:
            ; write a hex-nibble value to the left or right 7-seg LED.
            ; A contains the ASCII-HEX value to write in b[3..0], b[7..4] is IGNORED
            ; B contains the selected display, 1 := right disp., 0 := left disp.
            ; actual raw value writes are performed by another subroutine.
            ; AFFECTS: A,B,C,H,L
            PUSH PSW        ; PUSH A,B,C,H,L + STATUS onto stack
            PUSH B          
            PUSH H
            LXI H,led_7seg_lut ; setup the LUT
            ANI 0Fh         ; only low nibble needed
            MOV E,A
            MVI D,00h       ; setup jump table...
            DAD D           ; HL <- HL + DE
            MOV A,M         ; A <- (HL) grab the high.low raw segment values -> A
            MOV C,A         ; cache LUT value
            ANI 0FH
            MOV L,A
            MOV A,C
            ANI 0F0H
            RRC
            RRC
            RRC
            RRC
            MOV H,A         ; HL := [0000pgfc][0000dcba] (!) get the cached d.p!
            ; decide which raw register to save to...
            MOV A,B
            CPI 00H         ; B == 0 ?
            JZ  udhn0      ; Yes. (Left)
            LDA disp_shd_r_efgp ; No. (Right) - get the D.P
            ANI 08H
            ORA H           ; A <- H || ('0000p000'b) D.P from cache
            MOV H,A
            SHLD disp_shd_r_abcd  ; right (16-bit) <- HL (!) writes 2 bytes, only b[3..0] used in each byte.
            JMP udhn1
udhn0:      LDA disp_shd_l_efgp ; get the D.P
            ANI 08H
            ORA H           ; A <- H || ('0000p000'b) D.P from cache
            MOV H,A
            SHLD disp_shd_l_abcd
udhn1:      POP H
            POP B
            POP PSW         ; Restore A,B,C,H,L + STATUS
            RET

; Tested - OK
iob_user_disp_hexbyt:
            ; Uses the above calls to create the highest abstraction 
            ; level of use in the 7Seg displays in order to generate 
            ; a display showing a full byte, "00" to "FF" representing
            ; 0x00 to 0xff in value (reg-A).
            ; Left and right decimal points are controlled by a 
            ; separate call.
            MOV C,A         ; cache Reg-A
            MVI B,01H       ; setup for the right disp, LSB
            CALL iob_user_disp_hexnib
            MOV A,C
            RRC
            RRC
            RRC
            RRC
            MVI B,00H       ; setup for the left disp, MSB
            CALL iob_user_disp_hexnib
            ; Update the display
            CALL iob_disp_refresh
            RET
            

