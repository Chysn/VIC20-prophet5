;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                               Ed for Prophet 5
;                            (c)2023, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARTRIDGE LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* = $6000
Vectors:    .word Start         ; Start
            .word NMISR         ; NMI Address
            .byte $41,$30,$c3,$c2,$cd  ; Uncomment for production
           ; .byte $ff,$ff,$ff,$ff,$ff  ; Uncomment for development

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Application Memory
; In addition, zero page usage by
; MIDI KERNAL uses $9b - $9f
; SEQ PACKING uses $f9 - $ff
FIELD       = $00               ; Pointer to field memory location (2 bytes)
SYIN_IX     = $02               ; Sysex In position index
PTR         = $03               ; General use pointer (2 bytes)
READY       = $033c             ; New sysex is ready
PAGE        = $033d             ; Current page number
FIELD_IX    = $033e             ; Current field index
LISTEN      = $033f             ; Sysex listen flag
TOGGLE      = $0340             ; General use toggle value
REPEAT      = $0341             ; Repeat speed
KEYBUFF     = $0342             ; Last key pressed
IX          = $0343             ; General use index
NRPN_TX     = $0344             ; NRPN Transmit toggle
PRGLOC      = $0345             ; Program location screen codes (3 bytes)
LIB_IX      = $0348             ; Last library index
OUTSYSEX    = $1200             ; Outgoing sysex stage
INSYSEX     = $1300             ; Incoming sysex stage
CURRPRG     = $1400             ; Current program indexed buffer (128 bytes)
LIBRARY     = $2000             ; Storage for 40 programs (160x40=6400 bytes)

; Packing memory, for forward references
; Otherwise XA generates absolute mode rather than ZP
P_START     = $f9               ; Start address, 2 bytes
P_END       = $fb               ; End address, 2 bytes
P_RESULT    = $fd               ; Result address, 2 bytes
P_SIZE      = $ff               ; Size of packet
P_SRC       = $60               ; Source packet, 8 bytes
P_DEST      = $68               ; Destination packet, 8 bytes

; Status Message Indices
SM_FAIL     = 0
SM_RECV     = 1
SM_SENT     = 2
SM_WELCOME  = 3

; Character Constants
CR          = $0d               ; Carriage return PETSCII
RT          = $1d               ; Cursor right 
W_TRI       = $ce               ; Triangle waveform
W_TRI2      = $cd               ; ,,
W_SAW       = $ce               ; Saw waveform
W_SAW2      = $a5               ; ,,
W_SQU       = $a7               ; Square waveform
W_SQU2      = $d0               ; ,,
SW_ON       = $51               ; Switch on screen code
SW_OFF      = $57               ; Switch off screen code
CURSOR      = $6c               ; Cursor screen code PETSCII
TL          = $a3               ; Top line PETSCII
TXTCURSOR   = $6f               ; Text edit cursor

; Display Constants
SCREEN      = $1000             ; Screen character memory (expanded)
COLOR       = $9400             ; Screen color memory (expanded)
PARCOL      = 3                 ; Parameter color (cyan)
SELCOL      = 7                 ; Selected field color (yellow)
STACOL      = 2                 ; Status line color (red)
STATUSDISP  = SCREEN+484        ; Status line starting location

; Key Constants
F1          = 39
F3          = 47
F5          = 55
F7          = 63
PREV        = 31                ; CRSR up/down
NEXT        = 23                ; CRSR left/right
INCR        = 37                ; >
DECR        = 29                ; <
SEND2BUFF   = 35                ; B
EDIT        = 15                ; Edit parameter
BACKSP      = 7                 ; Backspace

; Field Types
F_VALUE     = 0                 ; Value field 0-120
F_XVALUE    = 1                 ; Extended value field 0-127
F_SWITCH    = 2                 ; Switch 0-1
F_TRACK     = 3                 ; Keyboard tracking (OFF, HALF, FULL)
F_DETUNE    = 4                 ; Detune 0-7
F_WHEEL     = 5                 ; Wheel Range 0-12
F_FILTER    = 6                 ; Filter type (1/2, 3)
F_NAME      = 7                 ; Program name
F_COUNT     = 8                 ; Voice count 0-10
F_RETRIG    = 9                 ; Unison retrigger (LO, LOR, LAS, LAR)
F_FREQ      = 10                ; Frequency (C0 ~ C4)

; System Resources
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
IRQ         = $eb12             ; System ISR return point
VIC         = $9000             ; VIC starting address
CHROUT      = $ffd2             ; Character out
PRSTR       = $cb1e             ; Print string at A/Y
CASECT      = $0291             ; Disable Commodore case
SYSNMI      = $feb2             ; System NMI
RFI         = $ff56             ; Return from interrupt
KEY         = $c5               ; Pressed key
HOME        = $e581             ; Home cursor
CLSR        = $e55f             ; Clear screen
KEYMAP      = $ec5e             ; Key to character map

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
Start:      jsr $fd8d           ; Test RAM, initialize VIC chip
            jsr $fd52           ; Restore default I/O vectors
            jsr $fdf9           ; Initialize I/O registers
            jsr $e518           ; Initialize hardware

            sei                 ; IRQ unneeded for this application
            jsr CLSR            ; Clear screen
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,            
            lda #13             ; Screen color
            sta VIC+$0f         ; ,,
            lda #30             ; Text color
            jsr CHROUT          ; ,,
            lda #<NMISR
            sta NMINV
            lda #>NMISR
            sta NMINV+1    
            jsr MIDIINIT  
            lda INSYSEX         ; Is there sysex in the starting position?
            cmp #ST_SYSEX       ; ,,
            bne clearprg        ; ,,
            lda #<INSYSEX       ; Unpack systex into the program buffer
            ldy #>INSYSEX       ; ,,            
            jsr UnpBuff         ; ,,
            jmp startpage       ; ,,
clearprg:   ldy #$80            ; If no sysex was in memory, clear out
            lda #0              ;   the current program memory
-loop:      sta CURRPRG,y       ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
startpage:  lda #0              ; Initialize
            sta READY           ;   * Sysex ready flag
            sta LISTEN          ;   * Sysex listen flag
            sta PAGE            ;   * Edit screen number
            sta NRPN_TX         ;   * NRPN Transmit
            sta LIB_IX          ;   * Last library entry index
            jsr SwitchPage      ; Generate the edit screen
            ldx #SM_WELCOME     ; Show welcome status
            jsr Status
            ; Fall through to MainLoop

; Main Program Loop
; Wait for a key, then act on valid commands            
MainLoop:   lda KEY             ; Debounce the key press
            cmp #$40            ; ,,
            bne MainLoop        ; ,,
waitkey:    bit READY           ; If Sysex is ready, handle it
            bmi SysexReady      ; ,,
            lda KEY             ; Get key press
            sta KEYBUFF
            cmp #$40            ; If no key down, wait
            bne keydown         ; ,,
            ldx #$28            ; Reset key repeat rate
            stx REPEAT          ; ,,
            jmp waitkey
keydown:    tay                 ; Preserve key pressed
            ldx #0              ; Look through Key Code table for a valid
-loop:      lda KeyCode,x       ;   command key press
            beq MainLoop        ;   ,, 0 delimits command list
            cmp KEYBUFF
            beq dispatch
            inx
            bne loop
dispatch:   lda CommandH,x      ; Set dispatch address on the stack
            pha                 ; ,,
            lda CommandL,x      ; ,,
            pha                 ; ,,
            tya                 ; Put key pressed back in A
            rts                 ; Dispatch command with key in A

; Handle Incoming SysEx
SysexReady: clc                 ; Clear the sysex ready flag
            ror READY           ; ,,
            lda SYIN_IX
            cmp #$9e
            bne SysexFail
            ldx #SM_RECV        ; Show received success status
            jsr Status          ; ,,
            jsr ProgLoc         ; Get program location from in buffer
            ldy #2              ; Show received program number
-loop:      lda PRGLOC,y        ; ,,
            sta STATUSDISP+9,y  ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            lda #<INSYSEX       ; Unpack systex into the program buffer
            ldy #>INSYSEX       ; ,,
            jsr UnpBuff         ; ,,
            ldx LIB_IX          ; Move in buffer to library, then advance
            jsr MoveToLib       ;   library index
            cpx #39             ;   ,,
            beq lib_done        ;   ,,
            inc LIB_IX          ;   ,,
lib_done:   jsr SwitchPage
            jmp MainLoop
SysexFail:  ldx #SM_FAIL
            jsr Status
            jmp MainLoop
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                        
; Select Page           
PageSel:    sec
            sbc #39
            lsr 
            lsr 
            lsr 
            cmp PAGE
            beq read_r
            sta PAGE
            jsr SwitchPage
read_r:     jmp MainLoop

; Move Cursor to Previous Field            
PrevField:  ldy FIELD_IX        ; If the current index is 0, stay here
            beq pf_r            ; ,,
            dey
            lda FPage,y
            cmp PAGE            ; If the field change would cross pages,
            bne pf_r            ;   stay here
            jsr ClrCursor
            dey
            sty FIELD_IX
            jsr DrawCursor
pf_r:       jmp MainLoop

; Move Cursor to Next Field
NextField:  ldy FIELD_IX
            cpy #LFIELD - FPage
            beq nf_r
            iny
            lda FPage,y
            cmp PAGE
            bne nf_r
            jsr ClrCursor
            iny
            sty FIELD_IX
            jsr DrawCursor
nf_r:       jmp MainLoop

; Increment Field by 1
IncValue:   jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeH,y
            beq id_r            ; Already at maximum, so do nothing
            inc CURRPRG,x
id_draw:    ldy FIELD_IX
            jsr DrawField
            ldy FIELD_IX
            lda FType,y         ; Some field types should not debounce the
            cmp #F_VALUE        ;   key, so check those types here
            beq no_deb          ;   ,,
            cmp #F_XVALUE       ;   ,,
            beq no_deb          ;   ,,
            cmp #F_FREQ         ;   ,,
            beq no_deb          ;   ,,
id_r:       jmp MainLoop
no_deb:     ldx REPEAT
-loop:      ldy #$ff
-loop1:     dey
            bne loop1
            dex 
            bpl loop
            lda REPEAT
            cmp #$08
            bcc topspeed
            dec REPEAT
            dec REPEAT
topspeed:   jmp waitkey

; Decrement Field by 1
DecValue:   jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeL,y
            beq id_r            ; Already at minimum, so do nothing
            dec CURRPRG,x
            jmp id_draw

; Single-Key Edit
; * Toggles Switches
; * Edits Name
; * Advances Others
EditField:  ldy FIELD_IX        ; Check the field's type for this edit     
            lda FType,y         ;   behavior.
            cmp #F_SWITCH       ; If it's a switch, it's going to toggle
            beq toggle_sw       ;   between set and unset
            cmp #F_NAME         ; If it's a name, it will be edited
            beq edit_name       ; ,,
            lda FNRPN,y         ; Anything else will advance to its max
            tax                 ;   and then roll back to 0
            lda FType,y         ;   ,,
            tay                 ;   ,,
            lda CURRPRG,x       ;   ,,
            cmp TRangeH,y       ;   ,,
            bcc adv_f           ;   ,,
            lda #$ff            ;   ,,
            sta CURRPRG,x       ;   ,,
adv_f:      inc CURRPRG,x       ;   ,,
            ldy FIELD_IX        ;   ,,
            jsr DrawField       ;   ,,
            jmp MainLoop        ;   ,,
toggle_sw:  lda FNRPN,y         ; Toggle the switch
            tay                 ; ,,
            lda CURRPRG,y       ; ,,
            eor #$01            ; ,,
            sta CURRPRG,y       ; ,,
            ldy FIELD_IX        ; ,,
            jsr DrawField       ; ,,
            jmp MainLoop        ; ,,
edit_name:  jsr ClrCursor       ; Clear cursor during name edit
            jsr FieldLoc        ; Get actual starting position of Name field
pos_cur:    jsr find_end        ; Set IX to the character after the last one
            lda #TXTCURSOR      ; Show a cursor in that place
            sta (FIELD),y       ; ,,
getkey:     ldx #$40            ; Debounce key, wait for any key to be lifted
-debounce:  cpx KEY             ; ,,
            bne debounce        ; ,,
-wait:      ldy KEY             ; Wait for any key to be pressed
            cpy #$40            ; ,,
            beq wait            ; ,,
            cpy #BACKSP         ; Has backspace been pressed?
            beq backsp          ; ,,
            cpy #EDIT           ; Has return been pressed?
            beq entername       ; ,,
            lda KEYMAP,y        ; Map keycode to a PETSCII value
            cmp #" "            ; Constrain values for character
            bcc getkey          ; ,,
            cmp #"Z"+1          ; ,,
            bcs getkey          ; ,,
            ldy IX              ; Put this character into the NRPN buffer
            sta CURRPRG+65,y    ; ,,
            jsr PETtoScr        ; Convert to screen code for display
            ldy IX              ; ,,
            sta (FIELD),y       ; ,,
            cpy #18             ; If at maximum size, do not advance cursor
            bcc pos_cur         ; ,,
            inc IX              ; Advance the cursor
            jmp pos_cur         ; ,,
backsp:     ldy IX              ; Backspace
            lda #" "            ; Clear the old cursor
            sta (FIELD),y       ; ,,
            beq getkey          ; If at the beginning already, do not backspace
            dec IX              ; Backspace by moving index back
            ldy IX              ; ,,
            lda #0              ; And adding a 0 in the NRPN buffer
            sta CURRPRG+65,y    ; ,,
            beq pos_cur
entername:  jsr find_end        ; RETURN has been pressed, so remove the cursor
            lda #" "            ; ,,
            sta (FIELD),y       ; ,,
            lda #0              ; Always make sure that this last name location
            sta CURRPRG+85      ;   is a 0
            jsr DrawCursor      ; Replace removed field-level cursor
            jmp MainLoop        ; And go back to Main
find_end:   ldy #0              ; Starting NRPN index of Name
-loop:      lda CURRPRG+65,y    ; Find the end of the current name, where the
            beq fc_r            ;   cursor should go
            iny                 ;   ,,
            cpy #20             ;   ,,
            bne loop            ;   ,,
fc_r:       sty IX              ;   ,,
            rts

; Send Edit Buffer
; From the current program        
BufferSend: lda #<EditBuffer
            ldy #>EditBuffer
            jsr SysexMsg
            lda PTR
            sta P_RESULT
            lda PTR+1
            sta P_RESULT+1
            lda #<CURRPRG
            sta P_START
            clc
            adc #$88
            sta P_END
            lda #>CURRPRG
            sta P_START+1
            sta P_END+1
            jsr Pack
            lda #ST_ENDSYSEX
            sta OUTSYSEX+$9c
            jsr SendSysex
            bcs bsend_r
            ldx #SM_SENT
            jsr Status
bsend_r:    jmp MainLoop
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERFACE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Prepare Field
; for increment or decrement
PrepField:  ldy FIELD_IX
            lda FNRPN,y
            tax
            lda FType,y
            cmp #F_NAME
            beq no_decinc
            tay
            lda CURRPRG,x
            sec
            rts
no_decinc:  clc
            rts

; Draw Edit Page
; at PAGE
SwitchPage: jsr ClrScr
            ldx PAGE            ; Acquire current page index
            lda EditH,x         ; Draw that page's field labels
            tay                 ; ,,
            lda EditL,x         ; ,,
            jsr PRSTR           ; ,,
            ; Fall through to PopFields

; Populate Fields
; at PAGE        
PopFields:  ldx PAGE            ; Get top parameter number for this page
            lda TopParamIX,x    ; ,,
            sta FIELD_IX        ; ,,
            tay                 ; Y is the field index
-loop:      lda FPage,y         ; Get the page number of the field
            cmp PAGE            ; Is the next field on the current page? 
            bne DrawCursor      ; If not, then fields are done
            tya                 ;   (Preserve Y against whatever happens in the
            pha                 ;   field drawing routine)
            jsr DrawField       ; Draw the field
            pla                 ;   (Bring back Y iterator
            tay                 ;   ,,)
            iny                 ; Increment the field number
            bpl loop            ; Move to the next field
            ; Fall through to DrawCursor

; Draw Cursor
; At field index in FIELD_IX
DrawCursor: ldy FIELD_IX 
            lda FRow,y
            jsr FieldRow
            lda #CURSOR
            ldx #0
            sta (FIELD,x)
            lda FType,y         ; Color the field the selected color only
            cmp #F_SWITCH       ;   if it's a switch
            bne dc_r            ;   ,,
            jsr FieldColor      ;   ,,
            lda #SELCOL         ;   ,,
            ldx #0              ;   ,,
            sta (FIELD,x)       ;   ,,
dc_r:       rts
          
; Clear Previous Cursor 
ClrCursor:  ldy FIELD_IX        ; Remove the previous cursor
            lda FRow,y          ; ,,
            jsr FieldRow        ; ,,
            lda #" "            ; ,,
            ldx #0              ; ,,
            sta (FIELD,x)       ; ,,
            jsr FieldColor
            lda #PARCOL
            ldx #0
            sta (FIELD,x)
            rts
            
; Draw Field
; at index Y
DrawField:  jsr FieldLoc        ; Set the field's physical screen location
            lda FType,y         ; Get the field's type index
            tax                 ;   in X
            lda TSubH,x         ; Get the field's draw address-1 and put it
            pha                 ;   on the stack for dispatch
            lda TSubL,x         ;   ,,
            pha                 ;   ,,
            lda FNRPN,y         ; Get this field's NRPN, which is also the
            tay                 ;   index within the program data
            lda CURRPRG,y       ;   and put the current value in A
            rts                 ; Pull the draw address off the stack, dispatch
         
; Set Field Location   
; Field index is in Y  
FieldLoc:   lda FRow,y
            jsr FieldRow
pluscol:    lda FCol,y
            clc 
            adc FIELD
            sta FIELD
            bcc f_nc2
            inc FIELD+1
f_nc2:      rts

; Set Field Color Location
; Field index is in Y
FieldColor: ldx #<COLOR
            stx FIELD
            ldx #>COLOR
            stx FIELD+1
            lda FRow,y
            jsr moffset
            jmp pluscol
            
; Set Field Row
; Multiply A by 22 and add to SCREEN+22
; Store result in FIELD pointer
FieldRow:   ldx #<SCREEN
            stx FIELD
            ldx #>SCREEN
            stx FIELD+1
moffset:    tax
-loop:      clc
            lda #22
            adc FIELD
            sta FIELD
            bcc r_nc
            inc FIELD+1
r_nc:       dex 
            bpl loop
r_r:        rts

; Unpack to Buffer
; A = low byte / Y = high byte of $9f-byte sysex message ($f0 - $f7)
UnpBuff:    sta P_START
            sta P_END
            sty P_START+1
            sty P_END+1          
            lda #6              ; Add offset to packed data beginning
            clc
            adc P_START
            sta P_START
            bcc un_ncs
            inc P_START+1
un_ncs:     lda #$9f
            clc
            adc P_END
            sta P_END
            bcc un_nce
            inc P_END+1
un_nce:     lda #<CURRPRG       ; Set program buffer as result
            sta P_RESULT
            lda #>CURRPRG
            sta P_RESULT+1
            jmp Unpack
         
; Write Text
; From A=low / Y=high  
WriteText:  sta PTR
            sty PTR+1
            ldy #0
-loop:      lda (PTR),y
            beq wr_r
            jsr PETtoScr
            sta (FIELD),y
            iny
            cpy #21             ; Max size, for the NAME field
            bcc loop
wr_r:       rts
            
; Convert PETSCII to Screen Code
; In A
PETtoScr:   cmp #123            ; Make the Name field case-insensitive
            bcs ch_pet          ;   by subtracting 32 for uppercase
            cmp #97             ;   ,,
            bcc ch_pet          ;   ,,
            ;sec                ;   ,, (carry already known clear)
            sbc #$20            ;   ,, 
ch_pet:     cmp #$ff            ; Is pi, which is an odd duck
            beq pi              ; ,,
            cmp #$c0
            bcs b_c0
            cmp #$a0
            bcs b_a0
            cmp #" "
            bcc pet_r
            cmp #$60
            bcc s_60
            and #$df
            bne pet_r
s_60:       and #$3f
pet_r:      rts
b_a0:       sbc #$40
b_c0:       and #$7f
            rts
pi:         lda #$5E
            rts   
            
; Clear Screen
; And color appropriately
ClrScr:     ldx #242            ; Clear the entire screen, except for the
-loop:      lda #$20            ;   bottom row, which is used for MIDI status
            sta SCREEN,x        ;   ,,
            sta SCREEN+241,x    ;   ,,
            lda #PARCOL         ;   ,, (for parameters)
            sta COLOR,x         ;   ,,
            sta COLOR+241,x     ;   ,,
            dex                 ;   ,,
            cpx #$ff            ;   ,,
            bne loop            ;   ,,
            lda #<COLOR         ; Set margin cursor to the select color
            sta PTR             ; ,,
            lda #>COLOR         ; ,,
            sta PTR+1           ; ,,
            ldy #22             ; ,,
            ldx #0              ; ,,
-loop:      lda #SELCOL         ; ,,
            sta (PTR,x)         ; ,,
            lda PTR             ; ,,
            clc                 ; ,,
            adc #22             ; ,,
            sta PTR             ; ,,
            bcc nc_cl           ; ,,
            inc PTR+1           ; ,,
nc_cl:      dey                 ; ,,
            bne loop            ; ,,
            lda #STACOL         ; Status line color
            ldx #22             ; ,,
-loop:      sta COLOR+484,x     ; ,,
            dex                 ; ,,
            bpl loop            ; ,,
            jmp HOME
            
; Display Status Message
; in X            
Status:     pha
            lda #<STATUSDISP
            sta FIELD
            lda #>STATUSDISP
            sta FIELD+1
            lda StatusH,x
            tay 
            lda StatusL,x
            jsr WriteText
            pla
            rts
            
; Show MIDI Monitor
; Of a byte in A
; Last three bytes
Monitor:    ldx SCREEN+501      ; Unrolled loop to show the most recent three
            stx SCREEN+498      ; MIDI bytes
            ldx SCREEN+502      ; ,,
            stx SCREEN+499      ; ,,
            ldx SCREEN+504      ; ,,
            stx SCREEN+501      ; ,,
            ldx SCREEN+505      ; ,,
            stx SCREEN+502      ; ,,
            pha                 ; Show the high nybble onscreen as hex
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            jsr hexb            ; ,,
            sta SCREEN+504      ; ,,
            pla                 ; Show the low nybble onscreen as hex
            pha                 ; ,,
            and #$0f            ; ,,
            jsr hexb            ; ,,
            sta SCREEN+505      ; ,,
            pla                 ; ,,
            rts
hexb:       cmp #$0a 
            bcs hexl 
            ora #$30
            rts
hexl:       sbc #$09
            rts
            
; Get Program Location
; For display. Sets 3 PRGLOC locations with group, bank, and program numbers
ProgLoc:    lda INSYSEX+4       ; Get bank number
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta PRGLOC          ;   Set it to first digit
            lda INSYSEX+5       ; Get group/program number
            pha
            and #$07            ; Isolate the program number
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta PRGLOC+2        ;   Set it to third digit
            pla
            lsr                 ; Isolate the group number
            lsr                 ;   ,,
            lsr                 ;   ,,
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta PRGLOC+1        ;   Set it to second digit
            rts
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O AND DATA SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Construct Sysex Message
; from A=high/Y=low to OUTSYSEX
; PTR points to the next byte in the sysex output stage
SysexMsg:   sta PTR
            sty PTR+1
            ldy #$ff
-loop:      iny
            lda (PTR),y
            cmp #$ff
            beq msg_done
            sta OUTSYSEX,y
            bne loop
msg_done:   tya
            clc
            adc #<OUTSYSEX
            sta PTR
            lda #>OUTSYSEX
            sta PTR+1
sm_r:       rts
            
; Send Sysex
; Returns with carry set if error
SendSysex:  ldy #0
-loop:      lda OUTSYSEX,y
            jsr Monitor
            jsr MIDIOUT
            bcs send_err
            cmp #ST_ENDSYSEX
            beq send_r
            iny
            bne loop
send_err:   ldx #SM_FAIL
            jsr Status
            sec 
            rts
send_r:     clc
            rts
            
; Move In Stage to Library
; Library index in X (0-39)
MoveToLib:  lda LibraryL,x      ; Set pointer to library start
            sta PTR             ; ,,  
            lda LibraryH,x      ; ,,
            sta PTR+1           ; ,,
            ldy #$9f            ; Move 160 bytes from the sysex input buffer
-loop:      lda INSYSEX,y       ;   into the library entry
            sta (PTR),y         ;   ,,
            dey                 ;   ,,
            cpy #$ff            ;   ,,
            bne loop            ;   ,,
            rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERRUPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NMI watches for incoming system exclusive data
NMISR:      pha                 ; NMI does not automatically save registers like
            txa                 ;   IRQ does, so that needs to be done
            pha                 ;   ,,
            tya                 ;   ,,
            pha                 ;   ,,
            jsr CHKMIDI         ; Is this a MIDI-based interrupt?
            bne midi            ;   If so, handle MIDI input
            jmp SYSNMI          ; Back to normal NMI, after register saves
midi:       jsr MIDIIN
            jsr Monitor         ; Show MIDI byte indicator
            cmp #ST_SYSEX       ; If sysex, 
            bne sy_catch        ;   initialize storage index
            sec                 ;   and set sysex listen flag
            ror LISTEN          ;   ,,
            ldx #0              ;   ,,
            stx SYIN_IX         ;   ,,
sy_catch:   bit LISTEN          ; If sysex listen flag is on, store the byte to
            bpl r_isr           ;   specified memory
sy_store:   ldx SYIN_IX         ; Get the index and store the byte there
            sta INSYSEX,x       ; ,,
            cmp #ST_ENDSYSEX    ; If the sysex has ended, perform end
            beq sydone          ; ,,
            inc SYIN_IX         ; Increment storage index. If it exceeds 255,
            bne r_isr           ;   end the sysex, for a likely error status
sydone:     clc                 ;   ,,
            ror LISTEN          ;   ,,
            sec                 ;   ,,
            ror READY           ;   ,,
r_isr:      jmp RFI             ; Restore registers and return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA FIELDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Value Bar
; at FIELD position, with value in A
ValBar:     and #$7f            ; Constrain A to 0-127
            ldy #0              ; Y is the position offset
            lsr                 ; Divide A by 2
            sec                 ; For each 8 units, add a full bar
-loop:      sbc #8              ; ,,
            bcc rem             ; ,,
            pha                 ; ,,
            lda #$a0            ; ,, (Reverse space)
            sta (FIELD),y       ; ,,
            iny                 ; ,,
            pla                 ; ,,
            bcs loop            ; ,,
rem:        eor #$ff            ; Handle the remainder by looking up 255-R
            tax                 ;   in the BarPartial table
            lda BarPartial,x    ;   ,,
-loop:      sta (FIELD),y       ;   ,,
            lda #$20            ; Clear out the rightmost unused characters
            iny                 ; ,,
            cpy #8              ; ,,
            bne loop            ; ,,
            rts

; Draw Switch
; At field location            
Switch:     cmp #1
            bne s_off
            lda #SW_ON
            .byte $3c 
s_off:      lda #SW_OFF 
            ldx #0
            sta (FIELD,x)
            rts

; Draw Enum Field  - Tracking          
Track:      cmp #2
            bne t_ch_1
            lda #<FullTrack
            ldy #>FullTrack
            jmp WriteText
t_ch_1:     cmp #1
            bne t_0
            lda #<HalfTrack
            ldy #>HalfTrack
            jmp WriteText
t_0:        lda #<NoTrack
            ldy #>NoTrack
            jmp WriteText
                   
; Draw Enum Field - Filter Revision       
FiltRev:    cmp #1
            bne f_0
            lda #<Rev3
            ldy #>Rev3
            jmp WriteText
f_0:        lda #<Rev1
            ldy #>Rev1
            jmp WriteText

; Draw Name Field           
Name:       lda #65             ; Offset for name location
            ldy #>CURRPRG       ; Current program location
            jmp WriteText
            
; Draw Enum Field - Retrigger
Retrigger:  cmp #3
            bne r_ch_2
            lda #<LAR
            ldy #>LAR
            jmp WriteText
r_ch_2:     cmp #2
            bne r_ch_1
            lda #<LAS
            ldy #>LAS
            jmp WriteText
r_ch_1:     cmp #1
            bne r_0:
            lda #<LOR
            ldy #>LOR
            jmp WriteText
r_0:        lda #<LO
            ldy #>LO
            jmp WriteText 
            
; Draw Numeric Field       
Num:        ldy #0              ; Y is the field index
            cmp #10             ; If less than 10, just show one digit
            bcc small           ; ,,
            pha                 ; If it's a two-digit number (no bigger than 19)
            lda #$31            ;   show a 1
            sta (FIELD),y       ;   ,,
            pla                 ;   ,,
            sec                 ;   then subtract 10 for the ones digit
            sbc #10             ;   ,,
            iny
small:      ora #$30            ; Add #$30 to make a screen code
            sta (FIELD),y       ; ,,
            iny                 ; Draw space afterward to clear right
            lda #" "            ; ,,
            sta (FIELD),y       ; ,,
            rts

; Draw Frequency (C 0 - C 4)            
Freq:       cmp #$61            ; Maximum value (for display purposes) is C4
            bcc getoct          ; ,, even though the P5 OS allows the full range
            lda #$60            ; ,,
getoct:     lsr                 ; Divide the value by 2 (2 increments per note)
            ldx #0              ; Get the octave number by subtracting 12
-loop:      cmp #12             ;   for each octave. The octave number will be 
            bcc haveoct         ;   X, and the note is the remainder, A
            ;sec                ;   ,, (we know carry is already set)
            sbc #12             ;   ,,
            inx
            bne loop            ;   ,,
haveoct:    pha                 ; Save the note number for later lookup
            txa                 ; A is now the octave number
            ora #$30            ; A is now the octave number digit character
            ldy #2              ; Y is the offset for the field location
            sta (FIELD),y       ; ,,
            pla                 ; Get note number back and use it as an index
            tax                 ;   to the note name and accidental tables
            lda Accidental,x    ;   ,,
            dey                 ;   ,,
            sta (FIELD),y       ;   ,,
            lda NoteName,x      ;   ,,
            dey                 ;   ,,
            sta (FIELD),y       ;   ,,
            rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Status messages
Failed:     .asc "FAILED!     ",0
Received:   .asc "RECEIVED 000",0
Sent:       .asc "SENT OK     ",0
Welcome:    .asc "ED PROPHET 5",0
NRPNOn:     .asc "NRPN ON     ",0
NRPNOff:    .asc "NRPN OFF    ",0
StatusL:    .byte <Failed,<Received,<Sent,<Welcome
StatusH:    .byte >Failed,>Received,>Sent,>Welcome

; MIDI Messages and Headers
EditBuffer: .byte $f0, $01, $32, $03, $ff

; Value Bar Partials
BarPartial: .byte $e7, $ea, $f6, $61, $75, $74, $65, $20

; Edit Page Data
EditL:      .byte <Edit0, <Edit1, <Edit2, <Edit3
EditH:      .byte >Edit0, >Edit1, >Edit2, >Edit3
TopParamIX: .byte 0,      16,     29,     45

; Key command subtroutine addresses
KeyCode:    .byte F1,F3,F5,F7,PREV,NEXT,INCR,DECR,SEND2BUFF,EDIT,0
CommandL:   .byte <PageSel-1,<PageSel-1,<PageSel-1,<PageSel-1
            .byte <PrevField-1,<NextField-1,<IncValue-1,<DecValue-1
            .byte <BufferSend-1,<EditField-1
CommandH:   .byte >PageSel-1,>PageSel-1,>PageSel-1,>PageSel-1
            .byte >PrevField-1,>NextField-1,>IncValue-1,>DecValue-1
            .byte >BufferSend-1,>EditField-1

; Field type subroutine addresses
; 0=Value Bar, 1=Ext Value, 2=Switch, 3=Tracking, 4=Detune, 5=Wheel, 6=Filter
; 7=Name, 8=Unison Voice Count, 9=Unison Retrigger, 10=Frequency
TSubL:      .byte <ValBar-1,<ValBar-1,<Switch-1,<Track-1
            .byte <Num-1,<Num-1,<FiltRev-1,<Name-1,<Num-1,<Retrigger-1,<Freq-1
TSubH:      .byte >ValBar-1,>ValBar-1,>Switch-1,>Track-1
            .byte >Num-1,>Num-1,>FiltRev-1,>Name-1,>Num-1,>Retrigger-1,>Freq-1
TRangeL:    .byte 0,  0,  0,0,0, 0,0,48, 0, 0,0
TRangeH:    .byte 120,127,1,2,7,11,1,90,10, 3,120

; Enum field values
NoTrack:    .asc "NONE",0
HalfTrack:  .asc "HALF",0
FullTrack:  .asc "FULL",0
Rev1:       .asc "1/2",0
Rev3:       .asc "3  ",0
LO:         .asc "LO ",0
LOR:        .asc "LOR",0
LAS:        .asc "LAS",0
LAR:        .asc "LAR",0

; Note Name Tables
; Flats are constructed of two screen code characters, Commodre-M and
; Commodore-S
NoteName:   .asc   3,  3,  4,  4,  5,  6,  6,  7,  7,  1,  1,  2
Accidental: .asc ' ','#',' ','#',' ',' ','#',' ','#',' ','#',' '

; Field data
; Field page number (0-3)
FPage:      .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
LFIELD:     .byte $80 ; Delimiter, and LFIELD - FPage = field count

; Field row
FRow:       .byte 1,1,2,3,4,7,7,7,8,9,10,11,12,15,16,17
            .byte 1,2,3,4,5,7,8,9,10,13,14,15,16
            .byte 1,2,3,4,5,8,9,10,10,10,13,14,15,16,17,18
            .byte 1,4,5,6,7,10,11,12,13,14,15,16,17,18,19

; Field column
FCol:       .byte 3,8,14,14,10,3,8,12,14,14,14,10,10,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14  
            .byte 14,14,8,8,8,14,14,3,8,12,14,8,8,8,8,8
            .byte 1,14,14,14,14,14,14,14,14,14,14,14,14,14,14

; Field type
FType:      .byte F_SWITCH,F_SWITCH,F_FREQ,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_FREQ,F_XVALUE,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_VALUE,F_VALUE,F_VALUE
            
            .byte F_VALUE,F_VALUE,F_VALUE,F_TRACK,F_FILTER,F_VALUE,F_VALUE
            .byte F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE
            
            .byte F_XVALUE,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_VALUE
            .byte F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_SWITCH
            
            .byte F_NAME,F_SWITCH,F_RETRIG,F_COUNT,F_DETUNE,F_XVALUE,F_VALUE
            .byte F_WHEEL,F_SWITCH,F_SWITCH,F_XVALUE
            .byte F_SWITCH,F_SWITCH,F_XVALUE,F_SWITCH

; Field NRPN number
FNRPN:      .byte 3,4,0,8,10,5,6,7,1,2,9,11,12,14,15,16
            .byte 17,18,40,19,20,43,45,47,49,44,46,48,50
            .byte 32,33,34,35,36,22,21,23,24,25,26,27,28,29,30,31
            .byte 88,52,87,53,54,13,37,86,41,42,97,38,39,98,51

; Edit Page Fields
Edit0:      .asc CR,"OSCILLATOR A",CR
            .asc RT,W_SAW,W_SAW2,RT,RT,W_SQU,W_SQU2,CR
            .asc RT,"FREQUENCY",CR
            .asc RT,"PULSE WIDTH",CR
            .asc RT,"SYNC",CR
            .asc CR,"OSCILLATOR B",CR
            .asc RT,W_SAW,W_SAW2,RT,RT,W_TRI,W_TRI2,RT,RT,W_SQU,W_SQU2,CR
            .asc RT,"FREQUENCY",CR
            .asc RT,"FINE",CR
            .asc RT,"PULSE WIDTH",CR
            .asc RT,"LO FREQ",CR
            .asc RT,"KEYBOARD",CR
            .asc CR,"MIXER",CR
            .asc RT,"OSC A",CR
            .asc RT,"OSC B",CR
            .asc RT,"NOISE"
            .asc 00
                      
Edit1:      .asc CR,"FILTER",CR
            .asc RT,"CUTOFF",CR
            .asc RT,"RESONANCE",CR
            .asc RT,"ENV AMOUNT",CR
            .asc RT,"KEYBOARD",CR
            .asc RT,"REV",CR
            .asc CR,RT,"ATTACK",CR
            .asc RT,"DECAY",CR
            .asc RT,"SUSTAIN",CR
            .asc RT,"RELEASE",CR
            .asc CR,"AMPLIFIER",CR
            .asc RT,"ATTACK",CR
            .asc RT,"DECAY",CR
            .asc RT,"SUSTAIN",CR
            .asc RT,"RELEASE"
            .asc 00
            
Edit2:      .asc CR,"POLY-MOD",CR
            .asc RT,"FILT ENV AMT",CR
            .asc RT,"OSC B AMT",CR
            .asc RT,"FREQ A",CR
            .asc RT,"PW A",CR
            .asc RT,"FILTER",CR
            .asc CR,"LFO",CR
            .asc RT,"INIT AMT",CR
            .asc RT,"FREQUENCY",CR
            .asc RT,W_SAW,W_SAW2,RT,RT,W_TRI,W_TRI2,RT,RT,W_SQU,W_SQU2,CR
            .asc CR,"WHEEL-MOD",CR
            .asc RT,"SOURCE MIX",CR
            .asc RT,"FREQ A",CR,
            .asc RT,"FREQ B",CR
            .asc RT,"PW A",CR,
            .asc RT,"PW B",CR
            .asc RT,"FILTER"
            .asc 00
                        
Edit3:      .asc CR,"NAME",CR,CR
            .asc CR,"UNISON",CR
            .asc RT,"ON",CR
            .asc RT,"RETRIGGER",CR
            .asc RT,"VOICE COUNT",CR
            .asc RT,"DETUNE",CR
            .asc CR,"OTHER",CR
            .asc RT,"GLIDE RATE",CR
            .asc RT,"VINTAGE",CR
            .asc RT,"WHEEL RANGE",CR
            .asc RT,"VEL  >FILTER",CR
            .asc RT,TL,TL,TL,"  >AMP",CR
            .asc RT,"     AMT",CR
            .asc RT,"AFT  >FILTER",CR
            .asc RT,TL,TL,TL,"  >LFO",CR
            .asc RT,"     AMT",CR
            .asc RT,"RELEASE/HOLD"
            .asc 00 
            
Menu:       .asc "   ED FOR PROPHET 5",CR
            .asc "  ",TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc "SETTINGS",CR
            .asc RT,"NRPN",CR
            .asc 00
             
; Library Sysex Pointers
; Indexed             
LibraryL:   .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60            
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60            
LibraryH:   .byte $20,$20,$21,$21,$22,$23,$23,$24
            .byte $25,$25,$26,$26,$27,$28,$28,$29
            .byte $2a,$2a,$2b,$2b,$2c,$2d,$2d,$2e
            .byte $2f,$2f,$30,$30,$31,$32,$32,$33
            .byte $34,$34,$35,$35,$36,$37,$37,$38
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBMODULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
#include "./submodules/MIDI-KERNAL/src/midikernal.asm"
#include "./submodules/sequential_lib/6502/sequential_packing.asm"

            