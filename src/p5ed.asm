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
SYIN        = $02               ; Sysex In pointer (2 bytes)
SYIN_IX     = $04               ; Sysex In position index
PTR         = $05               ; General use pointer (2 bytes)
READY       = $033c             ; New sysex is ready
PAGE        = $033d             ; Current page number
FIELD_IX    = $033e             ; Current field index
LISTEN      = $033f             ; Sysex listen flag
TOGGLE      = $0340             ; General use toggle value
REPEAT      = $0341             ; Repeat speed
KEYBUFF     = $0342             ; Last key pressed
IX          = $0343             ; General use index
PRGLOC      = $0344             ; Program location screen codes (3 bytes)
TGTLIB_IX   = $0347             ; Target library index
CURLIB_IX   = $0348             ; Current in-use library index
P_RAND      = $0349             ; Random number seed (2 bytes)
MASK        = $034b             ; Program mask for dump
MIDI_CH     = CURPRG+$a0        ; MIDI channel
NRPN_TX     = CURPRG+$a1        ; NRPN Transmit toggle
DEVICE_NUM  = CURPRG+$a2        ; Storage Device number
SEQ_STEPS   = CURPRG+$a3        ; Sequencer Steps
SEQ_TEMPO   = CURPRG+$a4        ; Sequencer Tempo
SEED1_PRG   = CURPRG+$a5        ; Generator Seed 1
SEED2_PRG   = CURPRG+$a6        ; Generator Seed 2
OUTSYSEX    = $1200             ; Outgoing sysex stage
CURPRG      = $1300             ; Current program indexed buffer (128 bytes)
SEED1       = $1400             ; Seed 1 program for generator (128 bytes)
SEED2       = $1480             ; Seed 2 program for generator (128 bytes)
SEQUENCE    = $1500             ; Sequence data (up to 64 steps)
LIBRARY     = $1600             ; Storage for 64 programs (160x64=10240 bytes)
LIB_TOP     = LibraryH-LibraryL ; Number of library entries

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
SM_LIB      = 3
SM_WELCOME  = 4
SM_GEN      = 5
SM_BLANK    = 6

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
P_TL        = 213               ; POPUP WINDOW - top left
P_T         = 195               ;                top
P_B         = 198               ;                bottom
P_TR        = 201               ;                top right
P_L         = 194               ;                left
P_R         = 200               ;                rifght
P_BL        = 202               ;                bottom left
P_BR        = 203               ;                bottom right
UP          = 145               ; Cursor up
RVON        = 18                ; Reverse on
RVOF        = 146               ;         off

; Display Constants
SCREEN      = $1000             ; Screen character memory (expanded)
COLOR       = $9400             ; Screen color memory (expanded)
PARCOL      = 3                 ; Parameter color (cyan)
SELCOL      = 7                 ; Selected field color (yellow)
STACOL      = 2                 ; Status line color (red)
LIBCOL      = 5                 ; Library display color (green)
STATUSDISP  = SCREEN+484        ; Status line starting location
WINDOW_ED   = SCREEN+248        ; Window editor location

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
NEXTLIB     = 5                 ; +
PREVLIB     = 61                ; -
OPENSETUP   = 8                 ; Back arrow
OPENHELP    = 43                ; H
GENERATE    = 19                ; G
SAVE        = 41                ; S
LOAD        = 21                ; L
SETPRG      = 13                ; P
CLEAR       = 62                ; CLR (+Commodore)
VOICEDUMP   = 18                ; D
DPROG       = 13                ; P
DBANK       = 35                ; B 
DGROUP      = 19                ; G
YES         = 11                ; Y

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
F_MIDICH    = 11                ; MIDI Channel (1-16)
F_DEVICE    = 12                ; Storage Device Number (8-11)
F_64        = 13                ; Program number, step count
F_NONE      = 14                ; Blank field

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
SHIFT       = $028d             ; SHIFT key status
VIAT        = $9114             ; VIA Timer (2 bytes)

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
            sta VIAT            ; Start VIA timer
            lda #13             ; Screen color
            sta VIC+$0f         ; ,,
            lda #30             ; Text color
            jsr CHROUT          ; ,,
            lda #<NMISR
            sta NMINV
            lda #>NMISR
            sta NMINV+1    
            jsr MIDIINIT
            ldy #0              ; If the first library location is a valid
            jsr Validate        ;   voice dump, unpack it to the
            bne clearprg        ;   current program buffer
            lda PTR             ;   ,,
            ldy PTR+1           ;   ,,
            jsr UnpBuff         ;   ,,
            jmp startpage       ;   ,,
clearprg:   ldy #$80            ; If no sysex was in memory, clear out
            lda #0              ;   the current program memory
-loop:      sta CURPRG,y        ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
startpage:  lda #0              ; Initialize
            sta READY           ;   * Sysex ready flag
            sta LISTEN          ;   * Sysex listen flag
            sta PAGE            ;   * Edit screen number
            sta NRPN_TX         ;   * NRPN Transmit
            sta TGTLIB_IX       ;   * Target library entry index
            sta CURLIB_IX       ;   * Current library entry index
            lda #1              ;   * MIDI channel
            sta MIDI_CH         ;     ,,
            sta SEED1_PRG       ;   * Generator seed 1
            lda #8              ;   * Device Number
            sta DEVICE_NUM      ;     ,,
            lda #2              ;   * Generator seed 2
            sta SEED2_PRG       ;     ,,
            lda #16             ;   * Sequencer steps
            sta SEQ_STEPS       ;     ,,
            lda #64             ;   * Sequencer tempo
            sta SEQ_TEMPO       ;     ,,
no_sysex:   jsr SwitchPage      ; Generate the edit screen
            ldx #SM_WELCOME     ; Show welcome message in status bar
            jsr Status          ; ,,
            jsr SelLib          ; ,,
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
            ldx #$40            ; Reset key repeat rate
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
            ldy CURLIB_IX       ; Is this a valid Prophet 5 voice dump?
            jsr Validate        ; ,,
            bne SysexFail       ; ,,
            ldx #SM_RECV        ; Show received success status
            jsr Status          ; ,,
            ldy CURLIB_IX       ; Get program location from library Y
            jsr ShowPrgNum      ; Show Program Number
            lda PTR             ; PTR was set above by Validate. Use it to
            ldy PTR+1           ;   unpack the sysex to the buffer
            jsr UnpBuff         ;   ,,
            jsr PopFields       ;   ,,            
lib_end:    jmp MainLoop
SysexFail:  ldx #SM_FAIL
            jsr Status
            jmp MainLoop
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; Previous Library Entry
PrevLib:    ldy CURLIB_IX
            jsr PackLib
            lda CURLIB_IX
            beq chlib_r
            dec CURLIB_IX
switchlib:  jsr ClrCursor
            ldy CURLIB_IX
            sty TGTLIB_IX
            jsr SelLib
            ldx #SM_LIB
            jsr Status
            ldy CURLIB_IX
            jsr ShowPrgNum
            jsr PopFields
chlib_r:    jmp MainLoop

; Next Library Entry
NextLib:    ldy CURLIB_IX
            jsr PackLib
            inc CURLIB_IX
            lda #LIB_TOP
            cmp CURLIB_IX
            bne switchlib
            dec CURLIB_IX
            jmp MainLoop
                      
; Select Page           
PageSel:    sec
            sbc #39             ; Determines which F key was pressed
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            ldy SHIFT           ; If SHIFT is held down, jump to
            bne LibSec          ;   ,,
            cmp PAGE
            beq read_r          
            sta PAGE
            jsr SwitchPage
read_r:     jmp MainLoop

; Go to Setup or Help Screen
GoHelp:     lda #5
            .byte $3c           ; Skip word (SKW)
GoSetup:    lda #4
            cmp PAGE
            beq setup_r
            sta PAGE
            jsr SwitchPage
            lda #0              ; Hide random cursor on help screen
            sta COLOR+440       ; ,,
setup_r:    jmp MainLoop
            
; Select Library Section
; The function key code minus 39 happen to be exactly half of
; where we want to place the library when the SHIFT key is held
; down.
LibSec:     asl                 ; Multiply FN key by 16
            asl                 ; ,,
            asl                 ; ,,
            asl                 ; ,,
            sta CURLIB_IX       ; Set current library index
            jmp switchlib       ; And go there
            
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
            inc CURPRG,x
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
            cmp #F_64           ;   ,,
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
            dec REPEAT
topspeed:   jmp waitkey

; Decrement Field by 1
DecValue:   jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeL,y
            beq id_r            ; Already at minimum, so do nothing
            dec CURPRG,x
            jmp id_draw

; Single-Key Edit
; * Toggles Switches
; * Edits Name
; * Advances Others
EditName:   ldy FIELD_IX        ; Check the field's type for this edit     
            lda FType,y         ;   behavior.
            cmp #F_NAME         ; If it's a name, it will be edited
            beq edit_name       ; ,,
            lda FNRPN,y         ; Anything else will advance to its max
            tax                 ;   and then roll back to 0
            lda FType,y         ;   ,,
            tay                 ;   ,,
            lda CURPRG,x        ;   ,,
            cmp TRangeH,y       ;   ,,
            bcc adv_f           ;   ,,
            lda TRangeL,y       ;   ,, If above high range, set to low
            sta CURPRG,x        ;   ,,   and save that
            jmp val_ch          ;   ,,
adv_f:      inc CURPRG,x        ;   ,,
val_ch:     ldy FIELD_IX        ;   ,,
            jsr DrawField       ;   ,,
            jmp MainLoop        ;   ,,
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
            sta CURPRG+65,y     ; ,,
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
            sta CURPRG+65,y     ; ,,
            beq pos_cur
entername:  jsr find_end        ; RETURN has been pressed, so remove the cursor
            lda #" "            ; ,,
            sta (FIELD),y       ; ,,
            lda #0              ; Always make sure that this last name location
            sta CURPRG+85       ;   is a 0
            jsr DrawCursor      ; Replace removed field-level cursor
            jmp MainLoop        ; And go back to Main
find_end:   ldy #0              ; Starting NRPN index of Name
-loop:      lda CURPRG+65,y     ; Find the end of the current name, where the
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
            lda #<CURPRG
            sta P_START
            clc
            adc #$88
            sta P_END
            lda #>CURPRG
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

; Generate Program
; From two spefcified parent programs in the library
Generate:   lda PAGE            ; Generate only works on edit screens
            cmp #4              ; ,,
            bcs gen_r           ; ,,
            ldy CURLIB_IX       ; First, make sure that the current library
            jsr Validate        ;   entry is either invalid, or lacks a
            bne gen_ok          ;   program number. We don't want to overwrite
            ldy #4              ;   something that's already in the library.
            lda (PTR),y         ;   ,,
            bmi gen_ok          ;   ,,
            ldx #SM_FAIL        ;   ,, Show failure message if not allowed
            jsr Status          ;   ,,
            jmp MainLoop
gen_ok:     lda $9114           ; Seed the random number shift register
            ora #$80            ;   with the VIA timer
            sta P_RAND          ;   ,,
            lda $9115           ;   ,,
            ora #$01            ;   ,,
            sta P_RAND+1        ;   ,,
            ldx SEED1_PRG       ; Get seed 1 program library number
            dex                 ;   -1 because SEED1_PRG is 1-indexed
            lda #<SEED1         ; Unpack seed 1 from library
            sta P_RESULT        ; ,,
            lda #>SEED1         ; ,,
            sta P_RESULT+1      ; ,,
            lda LibraryH,x      ; ,,
            tay                 ; ,,
            lda LibraryL,x      ; ,,
            jsr UnpSeed         ; ,,
            ldx SEED2_PRG       ; Get seed 2 program library number
            dex                 ;   -1 because SEED1_PRG is 1-indexed
            lda #<SEED2         ; Unpack seed 2 from library
            sta P_RESULT        ; ,,
            lda #>SEED2         ; ,,
            sta P_RESULT+1      ; ,,
            lda LibraryH,x      ; ,,
            tay                 ; ,,
            lda LibraryL,x      ; ,,
            jsr UnpSeed         ; ,,
            ldy #$80            ; For each unpacked memory location, randomly
-loop:      lda SEED1,y         ;   choose one from each seed program and
            tax                 ;   write it to the current program buffer.
            jsr PRand           ;   ,,
            bcs s1              ;   ,, (if pseudo-random carry, use seed 1,
            lda SEED2,y         ;   ,,  otherwise use seed 2)
            tax                 ;   ,,
s1:         txa                 ;   ,,
            sta CURPRG,y        ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            ldx #SM_GEN         ; Write status message when done
            jsr Status          ; ,,
            jsr PopFields       ; Update the buffer fields in the interface
gen_r:      jmp MainLoop
             
; Set Program Number
; for current program buffer                    
SetPrg:     lda PAGE            ; Set Program only works on edit screens
            cmp #4              ; ,,
            bcs gen_r           ; ,, (using gen_r above for range reasons)
            jsr Popup
            lda #<PrgLabel
            ldy #>PrgLabel
            jsr PRSTR
            ldy CURLIB_IX       ; Get program location to PRGLOC
            jsr PrgLoc          ; ,,
            ldy #3              ; Set up editor with current program
            sty IX              ; ,, Set current cursor position
            lda #TXTCURSOR      ; ,, Add cursor at end
            sta WINDOW_ED,y     ; ,,
            dey                 ; ,,
-loop:      lda PRGLOC,y        ; ,,
            sta WINDOW_ED,y     ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
pgetkey:    ldx #$40            ; Debounce key, wait for any key to be lifted
-debounce:  cpx KEY             ; ,,
            bne debounce        ; ,,
-wait:      ldy KEY             ; Wait for any key to be pressed
            cpy #$40            ; ,,
            beq wait            ; ,,
            cpy #BACKSP         ; Has backspace been pressed?
            beq pbacksp          ; ,,
            cpy #EDIT           ; Has return been pressed?
            beq pdone           ; ,,
            lda KEYMAP,y        ; Map keycode to a PETSCII value
            cmp #"1"            ; Constrain values for character
            bcc pgetkey         ; ,,
            ldy IX              ; If the current index is 2 (the third number)
            cpy #2              ;   then the maximum entry is 8
            bne max5            ;   otherwise it's 5
            cmp #"8"+1          ;   ,,
            .byte $3c           ;   ,, Skip word (SKW)
max5:       cmp #"5"+1          ;   ,,
            bcs pgetkey         ;   ,,
            ldy IX              ; Put this character into the program location
            cpy #3              ;   unless it's the 4th position
            bcs pgetkey         ;   ,,
            sta PRGLOC,y        ;   ,,
            jsr PETtoScr        ; Convert to screen code for display
            ldy IX              ; ,,
            sta WINDOW_ED,y     ; ,,
            inc IX              ; Advance the cursor
ppos_cur:   lda #TXTCURSOR      ; ,, Add cursor at end
            ldy IX              ; ,,
            sta WINDOW_ED,y     ; ,,
            bne pgetkey         ; ,,
pbacksp:    ldy IX
            beq pgetkey
            lda #" "
            sta WINDOW_ED,y
            sta PRGLOC,y
            dec IX
            jmp ppos_cur
pdone:      ldy IX              ; If edit isn't complete, then do nothing
            cpy #3              ; ,,
            bne pn_inval        ; ,,
            ldy CURLIB_IX       ; Editing is done, so update the sysex in 
            jsr SetLibPtr       ;   the library.
            lda PRGLOC          ; First character
            cmp #"-"            ;   If it's still "-" then it's invalid
            beq pn_inval        ;   ,,
            sec                 ; Subtract 1, because group is zero-indexed
            sbc #1              ; ,,
            and #$07            ; Constrain to actual group number
            ldy #4              ;   and store it in the library
            sta (PTR),y         ;   ,,
            lda PRGLOC+1        ; Here's the bank number
            cmp #"-"            ; Again, if it's "-" then invalid
            beq pn_inval        ; ,,
            sec                 ; Subtract 1, because bank is zero-indexed
            sbc #1              ; ,,
            and #$07            ; Constrain to a bank number
            asl                 ; Multiply by 8, since it's going along
            asl                 ;   with the program number
            asl                 ;   ,,
            sta IX              ;   and store it temporarily
            lda PRGLOC+2        ; Now the program number 
            cmp #"-"            ; You know the drill...
            beq pn_inval        ; ,,
            sec                 ; Same stuff as above, yadda yadda yadda
            sbc #1              ; ,,
            and #$07            ; ,,
            ora IX              ; Combine with the previously-stored bank number
            ldy #5              ; Store in the bank/program sysex location
            sta (PTR),y         ;   and that's it!
pn_inval:   jsr SwitchPage      ; Housekeeping. Redraw the page to get rid of
            ldx #SM_LIB         ;   the popup window, then draw status with
            jsr Status          ;   the new program number.
            ldy CURLIB_IX       ;   ,,
            jsr ShowPrgNum      ;   ,,
            jmp MainLoop
            
; System Exclusive Voice Dump
; of program, bank, or group
VoiceDump:  lda PAGE            ; Voice Dump only works on edit screens
            cmp #4              ; ,,
            bcs dump_r2         ; ,,
            ldy CURLIB_IX       ; If this is not a valid program, cannot
            jsr Validate        ;   do dump
            bne dump_r          ;   ,,
            ldy #4              ; If there's a valid program, but no program
            lda (PTR),y         ;   number is set, then treat this as
            bpl vdump           ;   an edit buffer dump
            jmp BufferSend      ;   ,,
vdump:      jsr Popup           ; Put the dump selection menu 
            lda #<DumpMenu      ;   into a popup window
            ldy #>DumpMenu      ;   ,,
            jsr PRSTR           ;   ,,
            lda #$40            ; Debounce the key, then wait for a key
-debounce:  cmp KEY             ;   to be pressed
            bne debounce        ;   ,,
-wait:      lda KEY             ;   ,,
            cmp #$40            ;   ,,
            beq wait
            pha                 ;   ,,
            jsr SwitchPage      ; Redraw the page to get rid of window
            pla
            cmp #DPROG          ; Dumping this program?
            bne ch_bank         ; ,,
            jmp DumpPrg         ; ,,
ch_bank:    cmp #DBANK          ; Dumping this program's bank?
            bne ch_group        ; ,,
            jmp DumpBank        ; ,,
ch_group:   cmp #DGROUP         ; Dumping this program's group?
            bne dump_r          ; ,,
            jmp DumpGroup       ; ,,
dump_r:     ldx #SM_BLANK       ; No dump done, clear status
            jsr Status          ; ,,
dump_r2:    jmp MainLoop

; Erase the current program      
Erase:      lda PAGE            ; Erase only works on edit screens
            cmp #4              ; ,,
            bcs erase_r2        ; ,,
            jsr Popup
            lda #<EraseConf
            ldy #>EraseConf
            jsr PRSTR
            lda #$40
-debounce:  cmp KEY
            bne debounce
-wait:      lda KEY
            cmp #$40
            beq wait
            cmp #YES
            bne erase_r
            ldy CURLIB_IX
            jsr SetLibPtr
            jsr ClearLib
erase_r:    jsr SwitchPage
            ldx #SM_BLANK
            jsr Status
erase_r2:   jmp MainLoop
                                           
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
            lda CURPRG,x
            sec
            rts
no_decinc:  clc
            rts

; Draw Edit Page
; at PAGE
SwitchPage: jsr ClrScr
            jsr ClrCursor
            ldx PAGE            ; Acquire current page index
            lda EditH,x         ; Draw that page's field labels
            tay                 ; ,,
            lda EditL,x         ; ,,
            jsr PRSTR           ; ,,
            lda #1              ; Use param #1 if page 1
            ldx PAGE            ; Get top parameter number for this page
            beq page1           ; ,,
            lda TopParamIX,x    ; ,,
page1:      sta FIELD_IX        ; ,,            
            ; Fall through to PopFields

; Populate Fields
; at PAGE        
PopFields:  ldy CURLIB_IX       ; Get current library entry 
            iny                 ; Library entries are 1-indexed for display
            jsr TwoDigNum       ; Get the number
            dey                 ; Return library to 0-indexed
            ora #$80            ; Make it reverse
            pha                 ; Save it to handle tens place
            txa                 ; ,,
            ora #$80            ; Make that reverse
            tax                 ; Get tens place back
            pla                 ; ,,
            stx STATUSDISP      ; Show the tens place as a numeral
            sta STATUSDISP+1    ; Show the ones place as a numeral
            lda #$6f            ; Show a little header for the library number
            sta STATUSDISP-22   ;   so it's easier to read
            sta STATUSDISP-21   ;   ,,
            ldx PAGE
            ldy TopParamIX,x
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
            lda CURPRG,y        ;   and put the current value in A
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
ClrScr:     ldx #230            ; Clear the entire screen, except for the
-loop:      lda #$20            ;   bottom 2 rows, which are used for status,
            sta SCREEN,x        ;   ,,
            sta SCREEN+230,x    ;   ,,
            lda #PARCOL         ;   ,, (for parameters)
            sta COLOR,x         ;   ,,
            sta COLOR+230,x     ;   ,,
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
            ldx #20             ; ,,
-loop:      sta COLOR+486,x     ; ,,
            dex                 ; ,,
            bpl loop            ; ,,
            lda #LIBCOL         ; Library number color (match screen border)
            sta COLOR+484       ; ,,
            sta COLOR+485       ; ,,
            sta COLOR+462       ; ,,
            sta COLOR+463       ; ,,
            jmp HOME
            
; Display Status Message
; in X            
Status:     pha
            lda #<(STATUSDISP+2)
            sta FIELD
            lda #>(STATUSDISP+2)
            sta FIELD+1
            lda StatusH,x
            tay 
            lda StatusL,x
            jsr WriteText
            pla
            rts

; Show Program Number
; In status bar, for library entry in Y      
ShowPrgNum: jsr PrgLoc          ; ,,
            ldy #2              ; Show received program number
-loop:      lda PRGLOC,y        ; ,,
            sta STATUSDISP+9,y ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            rts

; Get two-digit number
; In Y
; X is tens place, A is ones place PETSCII/screen code
TwoDigNum:  tya                 ; Transfer to A for maths
            ldx #0              ; Count tens places
-loop:      cmp #10             ; Is it 10 or more?
            bcc tensp           ; If lower, ones place in remainder
            sec                 ; Subtract 10
            sbc #10             ; ..
            inx                 ; And count how many 10s
            bne loop
tensp:      ora #$30            ; A is the ones place at this point
            pha                 ; X is the tens place, convert it to numeral
            txa                 ; ,,
            ora #$30            ; ,,
            tax                 ; ,,
            pla                 ; Get A back for return
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
; For library entry in Y
; For display. Sets 3 PRGLOC locations with group, bank, and program numbers
PrgLoc:     jsr Validate        ; Set library pointer and validate
            bne unset           ; Show unset location if not valid sysex
            ldy #4              ; Get group number
            lda (PTR),y         ;   ,,
            bmi unset           ;   ,, ($80 means unset)
            cmp #5              ; If this is a Factory group number,
            bcc usergr          ;   subtract 5 for display purposes
            ;sec                ;   ,, (carry known set here)
            sbc #5              ;   ,,
usergr:     clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta PRGLOC          ;   Set it to first digit
            iny
            lda (PTR),y         ; Get bank/program number
            pha
            and #$07            ; Isolate the program number
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta PRGLOC+2        ;   Set it to third digit
            pla
            lsr                 ; Isolate the bank number
            lsr                 ;   ,,
            lsr                 ;   ,,
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta PRGLOC+1        ;   Set it to second digit
            rts
unset:      lda #"-"
            sta PRGLOC
            sta PRGLOC+1
            sta PRGLOC+2
            rts
            
; Draw Popup Window
Popup:      jsr HOME
            lda #<Window
            ldy #>Window
            jsr PRSTR
            rts
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O AND DATA SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dump a Library Entry
; Set PTR before the call with SetLibPtr or Validate            
DumpLib:    ldy #0
-loop:      lda (PTR),y
            jsr Monitor
            jsr MIDIOUT
            bcs dump_err
            cmp #ST_ENDSYSEX
            beq dump_ok
            iny
            bne loop
dump_err:   ldx #SM_FAIL
            jmp Status
dump_ok:    ldx #SM_SENT
            jsr Status
            ldy IX
            iny
            jsr TwoDigNum
            stx STATUSDISP+7
            sta STATUSDISP+8
            rts
            
; Dump Single Program
; At the current library
; To dump another library, set PTR and call the DumpLib endpoint instead
DumpPrg:    ldy CURLIB_IX
            sty IX              ; Store in temporary index for status message
            jsr SetLibPtr
            jsr DumpLib
            jmp MainLoop

; Dump Bank
; Find all members of the current program's bank and dump them            
DumpBank:   ldy CURLIB_IX
            jsr SetLibPtr
            ldy #5              ; Get the current program's bank
            lda (PTR),y         ;   and set it as the bank mask
            and #$f8            ;   ,,
            sta MASK            ;   ,,
            ldy #0              ; IX is going to be the index of the search
            sty IX              ; ,,
-loop:      jsr Validate        ; Set the pointer to this library entry
            bne db_nomatch      ; ,,
            ldy #4              ; If the group number is $80, it means there's
            lda (PTR),y         ;   no program number set
            bmi db_nomatch      ;   ,,
            iny                 ; Get the bank number of this program
            lda (PTR),y         ; ,,
            and #$f8            ; ,,
            cmp MASK            ; Does it match the mask?
            bne db_nomatch      ; ,,
            jsr DumpLib         ; If it does, dump the library entry
db_nomatch: inc IX              ; Move to the next library entry
            ldy IX              ; Check the search index for the end
            cpy #LIB_TOP        ; ,,
            bne loop
            jmp MainLoop

; Dump Group
; Find all members of the current program's group and dump them
DumpGroup:  ldy CURLIB_IX
            jsr SetLibPtr
            ldy #4              ; Get the current program's group
            lda (PTR),y         ;   and set it as the group mask
            sta MASK            ;   ,,
            ldy #0              ; IX is going to be the index of the search
            sty IX              ; ,,
-loop:      jsr Validate        ; Set the pointer to this library entry
            bne dg_nomatch      ; ,,
            ldy #4              ; Get the group number of this program
            lda (PTR),y         ; ,,
            cmp MASK            ; Does it match the mask?
            bne dg_nomatch      ; ,,
            jsr DumpLib         ; If it does, dump the library entry
dg_nomatch: inc IX              ; Move to the next library entry
            ldy IX              ; Check the search index for the end 
            cpy #LIB_TOP        ; ,,
            bne loop
            jmp MainLoop
            
; Unpack to Buffer
; A = low byte / Y = high byte of $9f-byte sysex message ($f0 - $f7)
; For the UnpSeed endpoint, prepare by setting P_RESULT
UnpBuff:    ldx #<CURPRG        ; Set program buffer as result
            stx P_RESULT
            ldx #>CURPRG
            stx P_RESULT+1
UnpSeed:    sta P_START
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
un_nce:     jmp Unpack

; Pack Buffer to Library
; Library index in Y
; Generates system exclusive
PackLib:    jsr Validate        ; Validate the existing library entry, which
            beq hdr_ok          ;   sets PTR. If OK, continue
            ldy #03             ; Otherwise, generate a sysex header in the
-loop:      lda PrgDump,y       ;   library, with a group number byte of
            sta (PTR),y         ;   $80, which indicates that no group is
            dey                 ;   set.
            bpl loop            ;   ,,
            lda #$80            ;   ,,
            ldy #4                 ;   ,,
            sta (PTR),y         ;   ,,
            lda #$00            ;   ,,
            iny                 ;   ,,
            sta (PTR),y         ;   ,,
hdr_ok:     lda PTR
            clc
            adc #$06
            sta P_RESULT
            lda PTR+1
            sta P_RESULT+1
            lda #<CURPRG
            sta P_START
            clc
            adc #$80
            sta P_END
            lda #>CURPRG
            sta P_START+1
            sta P_END+1
            jsr Pack
            lda #ST_ENDSYSEX
            ldy #$9e 
            sta (PTR),y
            rts
                        
; Construct Sysex Message
; from A=low/Y=high to OUTSYSEX
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
       
; Select Library
; Unpack specified library index (in Y) to the current program location
SelLib:     jsr Validate
            beq lib_good
            jsr ClearLib
lib_good:   lda PTR
            ldy PTR+1
            jmp UnpBuff
            
; Set Library Pointer
; to entry index in Y
SetLibPtr:  lda LibraryL,y      ; In case of soft reset, advance library
            sta PTR             ;   pointer to the last library entry
            lda LibraryH,y      ;   ,,
            sta PTR+1           ;   ,,
            rts

; Validate Library
; Check sysex for Program Dump message, and #$7f in the right place
; Library index in Y
; Valid if zero flag is set
Validate:   jsr SetLibPtr
            ldy #3
-loop:      lda (PTR),y
            cmp PrgDump,y
            bne invalid 
            dey
            bpl loop
            ldy #$9e
            lda (PTR),y 
            cmp #ST_ENDSYSEX
invalid:    rts

; Clear Data
; With data pointer already in PTR
ClearLib:   ldy #$9f
            lda #0
-loop:      sta (PTR),y
            dey
            cpy #$ff
            bne loop
            ldx #$ff
            ldy #$50            ; Location of name in sysex
-loop:      inx
            iny
            lda Init,x
            sta (PTR),y
            bne loop
            lda PTR
            ldy PTR+1
            jsr UnpBuff
            rts

; Pseudo-Random
; One bit            
PRand:      lsr P_RAND
            ror P_RAND+1
            bcc rnd_r
            lda P_RAND
            eor #$aa
            sta P_RAND
            lda P_RAND+1
            eor #$2b
            sta P_RAND+1
rnd_r:      rts          
                     
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
            bne sy_catch        ;   ,,
            sec                 ;   set sysex listen flag
            ror LISTEN          ;   ,,
            ldy TGTLIB_IX       ; Get target library index
            ldx LibraryL,y      ; Set library memory from index
            stx SYIN            ; ,,
            ldx LibraryH,y      ; ,,
            stx SYIN+1          ; ,,
            ldx #0              ; Initialize library location index
            stx SYIN_IX         ; ,,
sy_catch:   bit LISTEN          ; If sysex listen flag is on, store the byte to
            bpl r_isr           ;   specified memory
sy_store:   ldy SYIN_IX         ; Get the index and store the byte there
            sta (SYIN),y        ; ,,
            cmp #ST_ENDSYSEX    ; If the sysex has ended, perform end
            beq sydone          ; ,,
            inc SYIN_IX         ; Increment storage index. If it exceeds 255,
            bne r_isr           ;   end the sysex, for a likely error status
sydone:     clc                 ;   ,,
            ror LISTEN          ;   ,,
            sec                 ; Set the ready flag to display status
            ror READY           ; ,,
            lda TGTLIB_IX       ; Copy library index to current library index 
            sta CURLIB_IX       ; ,,
            cmp #LIB_TOP-1      ; If not at the top entry yet, advance target
            beq r_isr           ;   library index
            inc TGTLIB_IX       ;   ,,
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
Blank:      rts                 ; Alias for the F_NONE field

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
Name:       ldy #21
            lda #" "
-loop:      sta SCREEN+22,y
            dey 
            bne loop
            lda #65             ; Offset for name location
            ldy #>CURPRG        ; Current program location
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
Num:        tay 
            jsr TwoDigNum
            ldy #0
            pha
            txa
            cmp #$30
            beq one_dig 
            sta (FIELD),y
            iny
one_dig:    pla
            sta (FIELD),y
            iny
            lda #" "
            sta (FIELD),y
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
Failed:     .asc "FAILED!   ",0
Received:   .asc "REC'D  000",0
Sent:       .asc "SENT OK   ",0
ChLib:      .asc "PROG   000",0
Welcome:    .asc "H FOR HELP",0
Generated:  .asc "GENERATED ",0
ClrStatus:  .asc "          ",0
StatusL:    .byte <Failed,<Received,<Sent,<ChLib,<Welcome,<Generated
            .byte <ClrStatus
StatusH:    .byte >Failed,>Received,>Sent,>ChLib,>Welcome,>Generated
            .byte >ClrStatus

; MIDI Messages and Headers
EditBuffer: .byte $f0, $01, $32, $03, $ff
PrgDump:    .byte $f0, $01, $32, $02, $ff

; Value Bar Partials
BarPartial: .byte $e7, $ea, $f6, $61, $75, $74, $65, $20

; Key command subtroutine addresses
KeyCode:    .byte F1,F3,F5,F7,PREV,NEXT,INCR,DECR,SEND2BUFF,EDIT
            .byte PREVLIB,NEXTLIB,OPENSETUP,OPENHELP,GENERATE,SETPRG
            .byte VOICEDUMP,CLEAR,0
CommandL:   .byte <PageSel-1,<PageSel-1,<PageSel-1,<PageSel-1
            .byte <PrevField-1,<NextField-1,<IncValue-1,<DecValue-1
            .byte <BufferSend-1,<EditName-1,<PrevLib-1,<NextLib-1
            .byte <GoSetup-1,<GoHelp-1,<Generate-1,<SetPrg-1,<VoiceDump-1
            .byte <Erase-1
CommandH:   .byte >PageSel-1,>PageSel-1,>PageSel-1,>PageSel-1
            .byte >PrevField-1,>NextField-1,>IncValue-1,>DecValue-1
            .byte >BufferSend-1,>EditName-1,>PrevLib-1,>NextLib-1
            .byte >GoSetup-1,>GoHelp-1,>Generate-1,>SetPrg-1,>VoiceDump-1
            .byte >Erase-1

; Field type subroutine addresses
; 0=Value Bar, 1=Ext Value, 2=Switch, 3=Tracking, 4=Detune, 5=Wheel, 6=Filter
; 7=Name, 8=Unison Voice Count, 9=Unison Retrigger, 10=Frequency
; 11=MIDI Ch,12=Device#, 13=SixtyFour, 14=No Field
TSubL:      .byte <ValBar-1,<ValBar-1,<Switch-1,<Track-1
            .byte <Num-1,<Num-1,<FiltRev-1,<Name-1,<Num-1,<Retrigger-1,<Freq-1
            .byte <Num-1,<Num-1,<Num-1,<Blank-1
TSubH:      .byte >ValBar-1,>ValBar-1,>Switch-1,>Track-1
            .byte >Num-1,>Num-1,>FiltRev-1,>Name-1,>Num-1,>Retrigger-1,>Freq-1
            .byte >Num-1,>Num-1,>Num-1,>Blank-1
TRangeL:    .byte 0,  0,  0,0,0, 0,0,48, 0, 0,0,  1 , 8, 1,0
TRangeH:    .byte 120,127,1,2,7,11,1,90,10, 3,120,16,11,64,0

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

; Initialized Program
Init:       .asc "INIT",0

; Edit Page Data
EditL:      .byte <Edit0, <Edit1, <Edit2, <Edit3, <Setup, <Help
EditH:      .byte >Edit0, >Edit1, >Edit2, >Edit3, >Setup, >Help
TopParamIX: .byte 0,      17,     30,     46,     60,     67

; Field data
; Field page number (0-3)
FPage:      .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3
            .byte 4,4,4,4,4,4,4
            .byte 5
LFIELD:     .byte $80 ; Delimiter, and LFIELD - FPage = field count

; Field row
FRow:       .byte 0,3,3,4,5,6,9,9,9,10,11,12,13,14,17,18,19
            .byte 1,2,3,4,5,7,8,9,10,13,14,15,16
            .byte 1,2,3,4,5,8,9,10,10,10,13,14,15,16,17,18
            .byte 1,2,3,4,7,8,9,10,11,12,13,14,15,16
            .byte 5,6,7,10,11,14,15
            .byte 19

; Field column
FCol:       .byte 1,3,8,14,14,10,3,8,12,14,14,14,10,10,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14  
            .byte 14,14,8,8,8,14,14,3,8,12,14,8,8,8,8,8
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14,14
            .byte 14,14,15,14,14,14,14
            .byte 1

; Field type
FType:      .byte F_NAME,F_SWITCH,F_SWITCH,F_FREQ,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_FREQ,F_XVALUE,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_VALUE,F_VALUE,F_VALUE
            
            .byte F_VALUE,F_VALUE,F_VALUE,F_TRACK,F_FILTER,F_VALUE,F_VALUE
            .byte F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE
            
            .byte F_XVALUE,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_VALUE
            .byte F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_SWITCH
            
            .byte F_SWITCH,F_RETRIG,F_COUNT,F_DETUNE,F_XVALUE,F_VALUE
            .byte F_WHEEL,F_SWITCH,F_SWITCH,F_XVALUE
            .byte F_SWITCH,F_SWITCH,F_XVALUE,F_SWITCH
            
            .byte F_MIDICH,F_SWITCH,F_DEVICE,F_64,F_XVALUE,F_64,F_64
            
            .byte F_NONE

; Field NRPN number
FNRPN:      .byte 88,3,4,0,8,10,5,6,7,1,2,9,11,12,14,15,16
            .byte 17,18,40,19,20,43,45,47,49,44,46,48,50
            .byte 32,33,34,35,36,22,21,23,24,25,26,27,28,29,30,31
            .byte 52,87,53,54,13,37,86,41,42,97,38,39,98,51
            ; These are not really NRPN numbers, but use the CURPRG storage
            ; for menu settings
            .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6
            .byte $ff

; Edit Page Fields
Edit0:      .asc CR,CR
            .asc CR,"OSCILLATOR A",CR
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
                        
Edit3:      .asc CR,"UNISON",CR
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
            
Setup:      .asc CR,"   ED FOR PROPHET 5",CR
            .asc "   ",TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc "  2023 JASON JUSTIAN",CR,CR
            .asc "SETTINGS",CR
            .asc RT,"MIDI CHANNEL",CR
            .asc RT,"NRPN SEND",CR
            .asc RT,"DEVICE       #",CR
            .asc CR,"SEQUENCER",CR
            .asc RT,"STEPS",CR
            .asc RT,"TEMPO",CR
            .asc CR,"GENERATION",CR
            .asc RT,"SEED",CR
            .asc RT,"SEED",CR
            .asc 00
            
Help:       .asc CR," ",$5f,"     SETUP",CR
            .asc " F1-F7 EDIT SCREEN",CR
            .asc " CRSR  PARAM",CR
            .asc " < >   EDIT VALUE",CR
            .asc " + -   LIBRARY",CR
            .asc " B     EDIT BUFFER",CR
            .asc " D     VOICE DUMP",CR
            .asc " G     GENERATE",CR
            .asc " P     SET PROG #",CR
            .asc " S     SAVE",CR
            .asc " L     LOAD",CR
            .asc " RUN   SEQUENCE",CR,CR
            .asc " MORE @",CR
            .asc " WWW.BEIGEMAZE.COM/ED"
            .asc 00

; Popup Window            
Window:     .asc 5 ; White
            .asc CR,CR,CR,CR,CR,CR,CR,CR
            .asc RT,RT,RT,RT,RT
            .asc P_TL,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_TR,CR
            .asc RT,RT,RT,RT,RT,P_L,"          ",P_R,CR
            .asc RT,RT,RT,RT,RT,P_L,"          ",P_R,CR
            .asc RT,RT,RT,RT,RT,P_L,"          ",P_R,CR
            .asc RT,RT,RT,RT,RT
            .asc P_BL,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_BR,CR
            .asc RT,RT,RT,RT,RT,RT,UP,UP,UP,UP ; Position for label
            .asc 00
            
PrgLabel:   .asc "PROGRAM #",30,0
DumpMenu:   .asc "VOICE DUMP",CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"P",RVOF,"ROG  ",RVON,"B",RVOF,"ANK",CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"G",RVOF,"ROUP E",RVON,"X",RVOF,"IT"
            .asc 30,0
            
EraseConf:  .asc "ERASE:",CR
            .asc RT,RT,RT,RT,RT,RT,"ARE YOU",CR
            .asc RT,RT,RT,RT,RT,RT,"SURE?(Y/N)",30,0
            
                         
; Library Sysex Pointers
; Indexed             
LibraryL:   .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60            
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60            
            .byte $00,$a0,$40,$e0,$80,$20,$c0,$60          
LibraryH:   .byte $16,$16,$17,$17,$18,$19,$19,$1a
            .byte $1b,$1b,$1c,$1c,$1d,$1e,$1e,$1f
            .byte $20,$20,$21,$21,$22,$23,$23,$24
            .byte $25,$25,$26,$26,$27,$28,$28,$29
            .byte $2a,$2a,$2b,$2b,$2c,$2d,$2d,$2e
            .byte $2f,$2f,$30,$30,$31,$32,$32,$33
            .byte $34,$34,$35,$35,$36,$37,$37,$38
            .byte $39,$39,$3a,$3a,$3b,$3c,$3c,$3d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBMODULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
#include "./submodules/MIDI-KERNAL/src/midikernal.asm"
#include "./submodules/sequential_lib/6502/sequential_packing.asm"
