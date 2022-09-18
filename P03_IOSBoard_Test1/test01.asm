; Test Peripheral Card - Steering & User I/O Board
; Hardware Version : 1.0c (Aug 2022)
; 
; Basic Function API
; ---
; iob_configure
; call args:    none.
; call ret:     none.
; desc:         Setup PIO controller for this I/O board
;               Note: currently set to hard IO address: 0x20 .. 0x2F
;                   PORT-A - INPUT
;                   PORT-B - OUTPUT/0x02
;                   PORT-C - OUTPUT/0x20 (only 6 bits)
;
; -
; iob_user_inp
; call args:    none
; call ret:     user octet value in 'A' reg
; desc:         Read the 8 user input switches 
;
; - 
; iob_user_disp_raw
; call args:    'A'     data nibble, only uses d0..d3
;               'B'     nibble addr, 0..3
; call ret:     none.
; desc.         Perform a low level nibble write to one of the 4 7-Seg.
;               LED latches, latch identified in 'B'. D[0..3] of 'A'
;               will be written to the latch. Uses no LUTs.
;
; -
; iob_user_disp_dpl (left)
; iob_user_disp_dpr (right)
; call args:    A   true := 'enable dp', false := 'disable dp'
; call ret:     none.
; desc.         enable or disable 7-seg LED decimal point lights w/o
;               affecting the remaining digits on the display.
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
RAM_BNK0    EQU     0000H
RAM_BNK1    EQU     1000H
RAM_BNK2    EQU     2000H

REG_CSR     EQU     20H
CSR_VAL     EQU     01001110b
                    ; 01 TIMER         HALT
                    ;  0 PortB INTR    OFF
                    ;  0 PortA INTR    OFF
                    ; 11 PortC MODE    3 (All Outputs)
                    ;  1 PortB DIR     Output
                    ;  0 PortA DIR     Input
REG_A       EQU     21H
REG_B       EQU     22H
REG_C       EQU     23H
PB_VAL      EQU     02H
PC_VAL      EQU     20H

; memory for shadow register copies of PortB,C on the 8155 PIO
shdw_regb:  db  00h
shdw_regc:  db  00h
; PortA is an input and does not need a shadow, but this value is the
; last read one and allows re-use without having to re-read the port.
shdw_rega:  db  00h

iob_configure:
            PUSH PSW        ; save Reg A
            MVI A, CSR_VAL
            OUT REG_CSR
            MVI A, PB_VAL
            OUT REG_B
            MVI A, PC_VAL
            OUT REG_C
            POP PSW         ; restore Reg A
            RET
            
iob_user_inp:
            IN REG_A
            RET
            
            ; shadow registers for the higher level information being 
            ; sent to each display. decimal-point state is boolean,
            ; true := On.
            ; Note: bit-3 is set or cleared which reduces rotate operations 
            ;       when the bit is being used in higher level calls
disp_dp_r:  db 00h          ; dp - right display [bool]
disp_dp_l:  db 00h          ; dp - left display [bool]

iob_user_disp_raw:
            ; low level nibble operations on the 7-seg. display registers.
            ; all operations on the 7-seg display are done through PortB.
            ; reg-A - b[3..0] raw nibble (LD[3..0]) - high nibble ignored.
            ; reg-B - b[1,0] 2-bit nibble register address (LS[1,0]). b[7..2] ignored.
            ; AFFECTS: A,B,C,H,L
            ; (!) THIS WILL ALSO WRITE THE dp (decimal-place) digit!
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
            ; -
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
            ; ==================================
            ; do some initial bit testing to see if a dp shadow register
            ; needs to be updated.
            PUSH PSW        ; PUSH A,B,C,H,L + STATUS onto stack
            PUSH B          
            PUSH H
            ANI 0Fh         ; only preserve lower nibble in Reg A
            MOV C,A         ; copy A -> C : stash raw-reg into 'C' for now
            MOV A,B         ; copy B -> A
            ANI 01h         ; look at LS0
            CPI 01h         ; A == 0x01? check Z-flag, it is SET when equal
            JNZ udr2        ; skip if the raw value does not hold a dp bit. (Reg-B : '00'b or '10'b)
            MOV A,B         ; copy B -> A
            ANI 02h         ; look at LS1 (determine which dp to update)
            CPI 02h         ; A == 0x02? check Z-flag, it is SET when equal
            JNZ udr0        ; jump if not equal... LS1 = '0'b so it is the left dp that is to be updated.
            ; updating the right dp state
            LXI H,disp_dp_r ; will change this RAM "shadow register" (right led dp state)
            JMP udr1        ; do the operation...
udr0:       ; updating the left dp state
            LXI H,disp_dp_l ; will change this RAM "shadow register" (left led dp state)
udr1:       ; perform the dp update operation for the selected side (save reg addr already in H)
            MOV A,C         ; copy C -> A - reload the raw binay value
            ANI 08h         ; mask out all except the dp bit, b[3]
            MOV M,A         ; save it back to the shadow register, leave the bit shifted for convenience.
            ; -- Main Operation - save nibble to the correct LED register --------------
udr2:       MOV A,C         ; copy C -> A : restore Acc value (raw nibble) for further work.
            RLC
            RLC
            RLC
            RLC             ; rot A left by 4 bits
            ANI 0F0h        ; clear out the lower 4 bits, not used.
            MOV C,A         ; A -> C : stash into 'C' for now
            MOV A,B         ; B -> A : LED reg. select bits
            ANI 03h         ; clear all but lowest 2 bits
            RLC
            RLC             ; shift left 2 bits
            ORA C           ; A <- A OR C : combined LED raw value and the register address
            MOV C,A         ; A -> C : stash into 'C' for now
            LXI H,shdw_regc ; get 16-bit address of PortC shadow reg. Use 'M' for copy operations on this address
            MOV A,M         ; A <- (shdw_regc) : load shadow value -> 'A'
            ANI 01h         ; clear out all but leave bit 0 preserved
            ORA C           ; A <- A OR C : all is combined now with b0 preserved and b1 "/LD" is '0'!
            MOV M,A         ; (shdw_regc) <- A : save the shadow value, with /LD at '0' (does not matter)
            XRI 02h         ; flip /LD bit to '1' before write
            OUT REG_C       ; and write out to PortC (/LD de-asserted)
            XRI 02h         ; clear /LD bit to '0' by flipping it again, to latch the value
            OUT REG_C       ; and write out to PortC w/ /LD set
            XRI 02h         ; flip /LD bit to '1' before write
            OUT REG_C       ; one final write to de-assert /LD again
            POP H
            POP B
            POP PSW         ; Restore A,B,C,H,L + STATUS
            RET             ; Done.

iob_user_disp_dpl:
            ; affect the left decimal-point "dot" on the 7-seg display.
            ; does not change any other data on the display.
            ; turn on or off, based on binary state of reg-A, true:=On
            CPI 00h         ; A == 0? if not then set A to 0x08 and write either value.
            JZ  uddl0
            MVI A,08h       ; set b3 corresponding to the correct bit-lane for the dp bit in the (high) 4-bit register
uddl0:      ; write the result to the shadow register
            STA disp_dp_l
            RET
            
iob_user_disp_dpr:
            ; affect the right decimal-point "dot" on the 7-seg display.
            ; does not change any other data on the display.
            ; turn on or off, based on binary state of reg-A, true:=On
            CPI 00h         ; A == 0? if not then set A to 0x08 and write either value.
            JZ  uddr0
            MVI A,08h       ; set b3 corresponding to the correct bit-lane for the dp bit in the (high) 4-bit register
uddr0:      ; write the result to the shadow register
            STA disp_dp_r
            RET

r_iob_user_disp_dpl:
            ; read back the right 7-seg display decimal-point. reg-A
            ; is true if the d.p is currently On (lit).
            LDA disp_dp_l
            RET
            
r_iob_user_disp_dpr:
            ; read back the left 7-seg display decimal-point. reg-A
            ; is true if the d.p is currently On (lit).
            LDA disp_dp_r
            RET

led_7seg_lut:
            ; 7-seg ASCII-HEX to Raw conversion Look up Table
            ;  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
            db 3Fh,0Ch,76h,5Eh,4Dh,5Bh,79h,0Eh,7Fh,4Fh,6Fh,79h,33h,7Ch,73h,63h
            ; note: DOES NOT turn on dp segments, that needs to be adjusted
            ;       when appropriate.
            ; b[7..4] segment order: dp,g,f,e   (LS0 := 1)
            ; b[3..0] segment order: d,c,b,a    (LS1 := 0)

iob_user_disp_hexnib:
            ; write a hex-nibble value to the left or right 7-seg LED.
            ; A contains the ASCII-HEX value to write in b[3..0], b[7..4] is IGNORED
            ; B contains the selected display, 1 := right disp., 0 := left disp.
            ; actual raw value writes are performed by another subroutine.
            ; AFFECTS: A,B,C,H,L
            PUSH PSW        ; PUSH A,B,C,H,L + STATUS onto stack
            PUSH B          
            PUSH H
            LXI M,led_7seg_lut ; setup the LUT
            ANI 0Fh         ; only low nibble needed
            MOV E,A
            MOV D,00h       ; setup jump table...
            DAD D           ; ...math
            MOV A,M         ; grab the high.low raw segment values -> A
            MOV C,A         ; cache value into C
            ; left or right display?
            MOV A,B         ; copy B -> A
            CPI 00h         ; B == 0 ?  (0 := left disp.)
            JZ udhn0
            ; right disp.
            LDA disp_dp_r   ; A <- right disp. dp state (shadowed) (0 | 0x8)
            JMP udhn1
            ; left disp.
udhn0:      LDA disp_dp_l   ; A <- left disp. dp state (shadowed) (0 | 0x8)
udhn1:      RLC             ; common combo math for integrating dp
            RLC
            RLC
            RLC             ; rot A left by 4 bits, get dp bit up to msb
            ANI 80h         ; make sure only msb can be set
            ANA C           ; A <- A AND C
            ; Reg A now contains the upper and lower 7-seg nibbles with 
            ; the correct current state of the dp on that display.
            MOV C,A         ; cache value into C
            MOV A,B         ; copy B -> A
            RLC
            ANI 02h         ; adjust display selector to become LS1 bit. Will now be '00'b or '10'b
            MOV B,A         ; put back into B
            MOV A,C         ; Restore Reg A
            ; write the low nibble
            CALL iob_user_disp_raw
            ; Note: Above call saves any registers that it will modify during operation.
            ;       As the registers are pop'd back, prior to return, their contents will
            ;       match the values in them when the subroutine was first called.
            MOV C,A         ; cache value into C
            MOV A,B         ; copy B -> A
            ORI 01h         ; set low bit to chenge to the high order nibble
            MOV B,A         ; put back into B
            MOV A,C         ; Restore Reg A
            RRC
            RRC
            RRC
            RRC             ; shift down the upper nibble
            ; write the high nibble
            CALL iob_user_disp_raw
            ; we are done, restore registers that we messed with.
            POP H
            POP B
            POP PSW         ; Restore A,B,C,H,L + STATUS
            RET

iob_user_disp_hexbyt:
            ; Uses the above calls to create the highest abstraction 
            ; level of use in the 7Seg displays in order to generate 
            ; a display showing a full byte, "00" to "FF" representing
            ; 0x00 to 0xff in value (reg-A).
            ; Left and right decimal points are controlled by a 
            ; separate call.

