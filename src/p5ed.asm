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
PTR         = $05               ; Library pointer (2 bytes)
PTRD        = $07               ; Destination pointer (2 bytes)

READY       = $033c             ; New sysex is ready
PAGE        = $033d             ; Current page number
FIELD_IX    = $033e             ; Current field index
LISTEN      = $033f             ; Sysex listen flag
N_LISTEN    = $0340             ; Note listen flag
REPEAT      = $0341             ; Repeat speed
KEYBUFF     = $0342             ; Last key pressed
IX          = $0343             ; General use index
PRGLOC      = $0344             ; Program location screen codes (3 bytes)
TGTLIB_IX   = $0347             ; Target library index
CURLIB_IX   = $0348             ; Current in-use library index
P_RAND      = $0349             ; Random number seed (2 bytes)
S_GROUP     = $034b             ; Group search
S_BANK      = $034c             ; Bank search
DUMPTYPE    = $034d             ; Bit 7 set = Group, clear = Bank
DEST10      = $034e             ; Tens digit of copy destination
DEST1       = $034f             ; Ones digit of copy destination

SEQ_XPORT   = $0350             ; Sequence transport (bit0=rec, 7=play, 0=stop)
SEQ_PLAY_IX = $0351             ; Sequence play note index
SEQ_REC_IX  = $0352             ; Sequence record index
SEQ_COUNT   = $0353             ; Sequence note countdown
SEQ_LAST    = $0354             ; Sequence last note played

MIDI_CH     = CURPRG+$a0        ; MIDI channel
NRPN_TX     = CURPRG+$a1        ; NRPN Transmit toggle
DEVICE_NUM  = CURPRG+$a2        ; Storage Device number
SEQ_STEPS   = CURPRG+$a3        ; Sequencer Steps
SEQ_TEMPO   = CURPRG+$a4        ; Sequencer Tempo
SEED1_PRG   = CURPRG+$a5        ; Generator Seed 1
SEED2_PRG   = CURPRG+$a6        ; Generator Seed 2
MUTATE      = CURPRG+$a7        ; Mutate flag

; Application Data Storage
OUTSYSEX    = $1200             ; Outgoing sysex stage
CURPRG      = $1300             ; Current program indexed buffer (128 bytes)
SEED1       = $1400             ; Seed 1 program for generator (128 bytes)
SEED2       = $1480             ; Seed 2 program for generator (128 bytes)
SEQUENCE    = $1500             ; Sequence note data (up to 64 steps)
VELOCITY    = $1580             ; Sequence velocity data (up to 64 steps)
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
SM_COPIED   = 7

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
P_R         = 200               ;                right
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
SEQCOL      = 1                 ; Sequencer transport color (white)
STATUSDISP  = SCREEN+484        ; Status line starting location
WINDOW_ED   = SCREEN+248        ; Window editor location
PROGRESSBAR = SCREEN+227        ; Progress bar location

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
OPENSETUP   = 32                ; Space
CANCEL      = 8                 ; Back arrow
OPENHELP    = 43                ; H
GENERATE    = 19                ; G
DISK        = 18                ; S
SETPRG      = 13                ; P
CLEAR       = 62                ; CLR
VOICEDUMP   = 27                ; V
COPY        = 34                ; C
REST        = 10                ; R
RUN         = 24                ; RUN/STOP

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
            sei                 ; Set IRQ for sequencer playback
            lda #<IRQSR
            sta CINV
            lda #>IRQSR
            sta CINV+1
            cli            
            lda #<NMISR         ; Set NMI for MIDI input listening
            sta NMINV           ; ,,
            lda #>NMISR         ; ,,
            sta NMINV+1         ; ,,
            jsr MIDIINIT        ; Initialize the MIDI interface
            ldy #0              ; If the first library location is a valid
            jsr Validate        ;   voice dump, unpack it to the
            bne clearprg        ;   current program buffer
            lda PTR             ;   ,,
            ldy PTR+1           ;   ,,
            jsr UnpBuff         ;   ,,
            jmp init_data       ;   ,,
clearprg:   jsr NewLib          ; New library entry if invalid
init_data:  lda #0              ; Initialize
            sta READY           ;   * Sysex ready flag
            sta LISTEN          ;   * Sysex listen flag
            sta N_LISTEN        ;   * Note listen flag
            sta PAGE            ;   * Edit screen number
            sta NRPN_TX         ;   * NRPN Transmit
            sta TGTLIB_IX       ;   * Target library entry index
            sta CURLIB_IX       ;   * Current library entry index
            sta SEQ_XPORT       ;   * Sequencer transport
            sta SEQ_PLAY_IX     ;   * Sequencer play index
            sta SEQ_REC_IX      ;   * Sequencer record index
            sta MUTATE          ;   * Generator mutation enable
            lda #1              ;   * MIDI channel
            sta MIDI_CH         ;     ,,
            sta SEED1_PRG       ;   * Generator seed 1
            lda #8              ;   * Device Number
            sta DEVICE_NUM      ;     ,,
            lda #2              ;   * Generator seed 2
            sta SEED2_PRG       ;     ,,
            lda #8              ;   * Sequencer steps
            sta SEQ_STEPS       ;     ,,
            lda #64             ;   * Sequencer tempo
            sta SEQ_TEMPO       ;     ,,
            jsr ClearSeq        ; Clear the sequence and velocity data
            jsr SwitchPage      ; Generate the edit screen
            ldx #SM_WELCOME     ; Show welcome message in status bar
            lda #$a2            ; Set IRQ to 240 per second
            sta $9124           ; ,,
            lda #$10            ; ,,
            sta $9125           ; ,,
            jsr Status          ; ,,
            ; Fall through to MainLoop

; Main Program Loop
; Wait for a key, then act on valid commands            
MainLoop:   lda #$40            ; Debounce the key press
-debounce:  cmp KEY             ; ,,
            bne debounce        ; ,,
waitkey:    bit READY           ; If Sysex is ready, handle it
            bmi SysexReady      ; ,,
            lda KEY             ; Get key press
            sta KEYBUFF         ; Store pressed key to avoid race conditions
            cmp #$40            ; If no key down, wait
            bne keydown         ; ,,
            ldx #$40            ; Reset key repeat rate
            stx REPEAT          ; ,,
            bne waitkey
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
            bne LibSec          ;   Library selection
            cmp PAGE            ; If already on this page, don't redraw
            beq read_r          ; ,,
            sta PAGE            ; Set the page and draw it
            jsr SwitchPage      ; ,,
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
LibSec:     tay                 ; Get library division for this key
            lda LibDiv,y        ; ,,
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
nrpn_msg:   jsr NRPNOut         ; Send NRPN message, if specified
            ldy FIELD_IX
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
            jmp nrpn_msg

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
getkey:     jsr Keypress        ; Get key code in Y, PETSCII in A
            cpy #BACKSP         ; Has backspace been pressed?
            beq backsp          ; ,,
            cpy #EDIT           ; Has return been pressed?
            beq entername       ; ,,
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
pgetkey:    jsr Keypress        ; Keycode in Y, PETSCII in A
            cpy #CANCEL         ; Cancel
            bne pch_bk          ; ,,
            jmp pn_inval        ; ,,
pch_bk:     cpy #BACKSP         ; Has backspace been pressed?
            beq pbacksp          ; ,,
            cpy #EDIT           ; Has return been pressed?
            beq pdone           ; ,,
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
vgetkey:    jsr Keypress        ; Get pressed key
            cpy #CANCEL         ; Check for cancel key
            beq dump_r          ; ,,
            cmp #"P"            ; Dumping this program?
            bne ch_bank         ; ,,
            jmp DumpPrg         ; ,,
ch_bank:    cmp #"B"            ; Dumping this program's bank?
            bne ch_group        ; ,,
            jmp DumpBank        ; ,,
ch_group:   cmp #"G"            ; Dumping this program's group?
            bne vgetkey         ; ,,
            jmp DumpGroup       ; ,,
dump_r:     ldx #SM_BLANK       ; No dump done, clear status
            jsr Status          ; ,,
            jsr SwitchPage      ; Get rid of window
dump_r2:    jmp MainLoop

; Erase the current program      
Erase:      lda PAGE            ; Erase only works on edit screens
            cmp #4              ; ,,
            bcs erase_r2        ; ,,
            jsr Popup
            lda #<EraseConf
            ldy #>EraseConf
            jsr PRSTR
            jsr Keypress
            cmp #"Y"
            bne erase_r
            ldy CURLIB_IX
            jsr SetLibPtr
            jsr NewLib
erase_r:    jsr SwitchPage
            ldx #SM_BLANK
            jsr Status
erase_r2:   jmp MainLoop

; Copy the current program
; to another location
CopyLib:    lda PAGE            ; Copy only works on edit screens
            cmp #4              ; ,,
            bcs erase_r2        ; Go to above MainLoop jump
cpage_ok:   jsr Popup
            lda #<CopyLabel
            ldy #>CopyLabel
            jsr PRSTR
            ldy #0              ; Set up editor for library number entry
            sty IX              ; ,, Set current cursor position
cpos_cur:   lda #TXTCURSOR      ; ,, Add cursor at beginning
            sta WINDOW_ED,y     ; ,,
cgetkey:    jsr Keypress        ; Get key
            cpy #CANCEL         ; Cancel
            bne cch_bk          ; ,,
            jmp copy_r
cch_bk:     cpy #BACKSP         ; Backspace
            beq cbacksp         ; ,,
            cpy #EDIT           ; Return
            beq cdone           ; ,,
            cmp #"0"            ; Test for valid tens-place numerals (1-6)
            bcc cgetkey         ; ,,
            ldy IX              ; If the index is past the first location (0)
            bne ones            ;   then it's the ones place
            cmp #"6"+1          ;   ,, The tens place allows 0-6
            bcs cgetkey         ;   ,,
            sta DEST10          ; If it's a valid numeral, add it to the string
            sta WINDOW_ED,y     ;   and to the display
            inc IX              ; Increment the index and redraw the cursor
            ldy IX              ; ,,
            jmp cpos_cur        ; ,,
ones:       ldx "9"+1           ; By default, maximum is 9
            stx DEST1           ;   Use DEST1 to temporarily store the maximum
            ldx DEST10          ;   ,,
            cpx #"6"            ;   ,, If tens is 6, change max to 4
            bne allow9          ;   ,,
            ldx #"4"+1          ;   ,,
            stx DEST1           ;   ,,
allow9:     cmp DEST1           ; Test against the determined maximum
            bcs cgetkey         ; ,,
            ldx IX              ; Do not advance past two digits
            cpx #2              ; ,,
            bcs cgetkey         ; ,,
            sta DEST1           ; Save in ones place location
            sta WINDOW_ED,y     ; Display in the popup
            inc IX              ; Advance the index and redraw
            ldy IX
            jmp cpos_cur        ;   the cursor
cbacksp:    ldy IX              ; Move back and clear the destination byte
            beq cgetkey         ; ,,
            lda #" "            ; ,,
            sta WINDOW_ED,y     ; ,,
            sta DEST10,y        ; ,,
            dec IX              ; ,,
            ldy IX
            jmp cpos_cur        ; ,,
cdone:      ldx IX              ; If the number isn't finished, go back for
            cpx #2              ;   more
            bne cgetkey         ;   ,,
            lda DEST10          ; Get the tens place
            asl                 ; Shift out everything but the numeral
            asl                 ; ,,
            asl                 ; ,,
            asl                 ; ,,
            sta IX              ; Tens place is in high nybble
            lda DEST1           ; Get ones place and remove the #$3x
            and #$0f            ; ,,
            ora IX              ; Set the high nybble of A to tens place
            ldy #0              ; And then use decimal mode to subtract
            sed                 ;   one, and count how many times it's done
-loop:      iny                 ;   which leaves the actual value in Y
            sec                 ;   ,,
            sbc #1              ;   ,,
            bne loop            ;   ,,
            cld                 ;   ,,
            dey                 ; Re-zero-index the library entry number
            jsr SetLibPtr       ; Set PTR to this library entry
            lda PTR             ; Move PTR to the destination pointer for
            sta PTRD            ;   copying
            lda PTR+1           ;   ,,
            sta PTRD+1          ;   ,,
            ldy CURLIB_IX       ; Now set PTR to the current entry
            jsr SetLibPtr       ; ,,
            jsr PackLib         ; Pack program data to library prior to copy
            ldy #$9f            ; Perform the copy operation
-loop:      lda (PTR),y         ; ,,
            sta (PTRD),y        ; ,,
            dey                 ; ,,
            cpy #$ff            ; ,,
            bne loop            ; ,,
            lda #$80            ; Set the copy's program number as unset
            ldy #4              ; ,,
            sta (PTRD),y        ; ,,
            ldx #SM_COPIED      ; Indicate copy success
            jsr Status          ; ,,
copy_r:     jsr SwitchPage
            jmp MainLoop

; Sequencer control
Sequencer:  ldx SEQ_LAST        ; Turn off last note whenever a transport
            ldy #0              ;   control is activated
            jsr NOTEOFF         ;   ,,
            lda SEQ_XPORT       ; Is transport currently on?
            beq start           ;   If not, start the sequencer
            lda #0              ; Stop the sequencer
            sta SEQ_XPORT       ; ,,
            beq annunciate      ; ,,
start:      lda SHIFT           ; Is Commodore key held down?
            and #$02            ; ,,
            beq startplay       ; If not, start the playback
            lda #$01            ; Turn on the record transport bit
            sta SEQ_XPORT       ; ,,
            lda SEQ_REC_IX      ; Show step number and name
            jsr ShowStep        ; ,,
            beq annunciate      ; ,,
startplay:  lda #" "            ; Clear the note number display
            sta SCREEN+15       ; ,,
            sta SCREEN+16       ; ,,
            sta SCREEN+18       ; ,, And the note name display
            sta SCREEN+19       ; ,,
            lda #$02            ; Turn on the record playback bit
            sta SEQ_XPORT       ; ,,
            lda MIDI_CH         ; Set MIDI channel
            sec                 ; ,,
            sbc #1              ; ,,
            jsr SETCH           ; ,,
            ldx #$ff            ; Reset the play index
            stx SEQ_PLAY_IX     ; ,,
            jsr PlayNote        ; Play the first note
annunciate: ldx SEQ_XPORT       ; Get the sequencer transport graphic
            lda XportAnn,x      ;   from the annunicator table and
            sta SCREEN+21       ;   put it in the top right corner.
seq_r:      jmp MainLoop

; Delete Last Sequencer Note
DelNote:    lda SEQ_XPORT       ; Is the sequencer in record status?
            cmp #1              ; ,,
            bne del_r           ; If not, do nothing
            lda SEQ_REC_IX      ; Is the record head at the beginning?
            beq del_r            ;   If so, do nothing
            dec SEQ_REC_IX      ; Remove the last note
            lda #0              ; ,,
            ldy SEQ_REC_IX      ; ,,
            sta SEQUENCE,y      ; ,,
            lda SEQ_REC_IX      ; Show new number of steps
UpdateDisp: jsr TwoDigNum       ; ,,
            stx SCREEN+15       ; ,,
            sta SCREEN+16       ; ,,
            lda #" "            ; Clear out the note name area
            sta SCREEN+18       ; ,,
            sta SCREEN+19       ; ,,
del_r:      jmp MainLoop

; Add Rest to Sequencer
AddRest:    lda SEQ_XPORT       ; Is the sequencer in record status?
            cmp #1              ; ,,
            bne del_r           ; If not, do nothing
            lda SEQ_REC_IX      ; Is the record head at the end?
            cmp #64             ;   If so, do nothing
            beq del_r           ;   ,,
            ldy SEQ_REC_IX      ; Set 0 for next note and velocity
            lda #0              ; ,,
            sta SEQUENCE,y      ; ,,
            sta VELOCITY,y      ; ,,
            iny                 ; Increment the record step
            sty SEQ_REC_IX      ; ,,
            tya                 ; ,,
            jmp UpdateDisp      ; Update display, show num but remove name
                        
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
            sta SCREEN+22,x     ;   and the top row, used for the sequencer.
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
            lda #SEQCOL         ; Add sequence transport color
            sta COLOR+21        ; ,,
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
            
; Configure Progress Bar
; within a popup       
; Once set up
;   LDA value
;   JSR ProgPopup
ProgPopup:  ldx #<PROGRESSBAR   ; Set FIELD pointer, which is used by
            stx FIELD           ;   VarBar
            ldx #>PROGRESSBAR   ;   ,,
            stx FIELD+1         ;   ,
            jsr ValBar
            rts
            
; Draw Popup Window
Popup:      jsr HOME
            lda #<Window
            ldy #>Window
            jsr PRSTR
            rts
            
; Get Key Press
; Return PETSCII value of key in A 
; and key code in Y
Keypress:   lda #$40            ; Debounce keyboard by waiting for keys to be
-debounce:  cmp KEY             ;   released
            bne debounce        ;   ,,
-wait:      ldy KEY             ; Wait for a keypress
            cpy #$40            ; ,,
            beq wait            ; ,,
            lda SHIFT           ; Get shift state
            and #$01            ; ,,
            beq unshift         ; Get location in ROM of keyboard map
            lda #$9f            ; ,,
            .byte $3c           ; Skip word (SKW)
unshift:    lda #$5e            ; ,,
            sta $f5             ; ,,
            lda #$ec            ; ,, high byte of key map in ROM
            sta $f6             ; ,,
            lda ($f5),y         ; Get PETSCII from table
            rts
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O AND DATA SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dump Single Program
; At the current library
; To dump another library, set PTR and call the DumpLib endpoint instead
DumpPrg:    ldy CURLIB_IX
            sty IX              ; Store in temporary index for status message
            jsr SetLibPtr
            jsr DumpLib
            jsr SwitchPage
            jmp MainLoop

; Dump Bank or Group
; Find all members of the current program's bank and/or group and dump them     
DumpGroup:  sec                 ; Set DUMPTYPE flag to indicate group dump
            .byte $34           ; Skip byte (SKB)
DumpBank:   clc                 ; Clear DUMPTYPE flag to indicate bank dump
            ror DUMPTYPE        ; ,,
            jsr Popup           ; Set up progress bar popup
            ldy CURLIB_IX       ; Get the current program's bank number
            jsr SetLibPtr       ;   ,,
            ldy #4              ;   Get the group
            lda (PTR),y         ;   ,,
            sta S_GROUP         ;   and set it as the search group
            iny                 ;   Get the bank
            lda (PTR),y         ;   ,,
            and #$f8            ;   ,, (isolate the bank bits)
            sta S_BANK          ;   and set it as the search bank
            ldy #0              ; IX is going to be the index of the search
            sty IX              ; ,,
-loop:      jsr Validate        ; Set the pointer to this library entry
            bne d_nomatch       ; ,,
            ldy #4              ; If the group number is $80, it means there's
            lda (PTR),y         ;   no program number set
            bmi d_nomatch       ;   ,,
            cmp S_GROUP         ; A program must match the group, always, to be
            bne d_nomatch       ;   dumped
            bit DUMPTYPE        ; If the DUMPTYPE flag is set, only look at
            bmi d_match         ;   the group. Otherwise, match the bank too
            iny                 ; Get the bank number of this program
            lda (PTR),y         ; ,,
            and #$f8            ; ,,
            cmp S_BANK          ; Does it match the bank?
            bne d_nomatch       ; ,,
d_match:    jsr DumpLib         ; If it does, dump the library entry
d_nomatch:  inc IX              ; Move to the next library entry
            lda IX              ; Draw the value bar based on index
            asl                 ;   ,, (twice the index, actually)
            jsr ProgPopup       ;   ,,
            ldy IX              ; Check the search index for the end
            cpy #LIB_TOP        ; ,,
            bne loop
            jsr SwitchPage
            jmp MainLoop
            
; Dump a Library Entry
; Set PTR before the call with SetLibPtr or Validate            
DumpLib:    ldy #0              ; Set the output index
-loop:      lda (PTR),y         ; Get the next byte to output
            jsr MIDIOUT         ; Send it to the Beige Maze MIDI KERNAL
            bcs dump_err        ; Show error if timeout
            cmp #ST_ENDSYSEX    ; Was this an end-of-sysex status?
            beq dump_ok         ; If so, dump is done with success
            iny                 ; Go to the next index
            bne loop            ; If we get all the way to 0, something's wrong
dump_err:   ldx #SM_FAIL        ; Fail by either (1) timing out at the
            jmp Status          ;   interface, or (2) invalid sysex
dump_ok:    ldx #SM_SENT        ; Success!
            jsr Status          ; Show the status and the library entry
            ldy IX              ;   number
            iny                 ;   ,, (which is 1-indexed)
            jsr TwoDigNum       ;   ,,
            stx STATUSDISP+7    ;   ,,
            sta STATUSDISP+8    ;   ,,
            rts
            
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
            jsr NewLib
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

; New Library Entry
; With data pointer already in PTR
NewLib:     ldy #$9f
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

; Send NRPN, if enabled
; NRPN index is in X
NRPNOut:    lda NRPN_TX         ; Skip the whole thing is NRPN is disabled
            beq nrpn_r          ; ,,
            cpx #$90            ; If this is one of the settings
            bcs nrpn_r          ;   parameters for Ed, do not send to P5
            stx IX              ; Temporarily store the NRPN number in IX
            ldy MIDI_CH         ; Get MIDI channel
            dey                 ; ,, zero-index it
            tya                 ; ,,
            and #$0f            ; Low nybble is MIDI channel
            ora #%10110000      ; Control Change
            jsr MIDIOUT         ; ,,
            lda #%01100011      ; NRPN parameter number MSB CC
            jsr MIDIOUT         ; ,,
            lda #%00000000      ; Parameter Number MSB
            jsr MIDIOUT         ; ,,
            lda #%01100010      ; NRPN parameter number LSB CC
            jsr MIDIOUT         ; ,,
            lda IX              ; Parameter number LSB
            jsr MIDIOUT         ; ,,
            lda #%00000110      ; NRPN parameter value MSB CC
            jsr MIDIOUT         ; ,,
            lda #%00000000      ; Parameter value MSB
            jsr MIDIOUT         ; ,,
            lda #%00100110      ; NRPN parameter value LSB CC
            jsr MIDIOUT         ; ,,
            ldx IX              ; Get the NRPN number
            lda CURPRG,x        ; Get the value
            and #$7f            ; Constrain for CC
            jsr MIDIOUT         ; ,,
nrpn_r:     rts

; Play Next Note
; and set the countdown timer for the IRQ
PlayNote:   ldx SEQ_PLAY_IX     ; Is this the last sequencer step?
            inx                 ; ,,
            cpx SEQ_STEPS       ; ,,
            bcc pl              ; If not, play the next step
            ldx #0              ; Otherwise reset the sequencer
pl:         stx SEQ_PLAY_IX     ; Store incremented (or reset) index
            lda VELOCITY,x      ; Get the velocity
            beq rest            ; Rest if zero velocity
            tay                 ; ,,
            lda SEQUENCE,x      ; Get the sequence note number
            sta SEQ_LAST        ; Store note for next note off
            tax                 ; ,,
rest:       jsr NOTEON          ; Send Note On command
            lda #150            ; Reset the countdown
            sec                 ; ,,
            sbc SEQ_TEMPO       ; ,,
            sta SEQ_COUNT       ; ,,
            rts

; Clear the sequence            
ClearSeq:   ldy #64             ; Fill 64 bytes
            lda #0              ; With rests
-loop:      sta SEQUENCE,y      ; ,,
            sta VELOCITY,y      ; ,,
            dey                 ; ,,
            bne loop            ; ,,
            rts
            
; Show Step
; Step number in Y
ShowStep:   cpy #0              ; If step is 0, do not show
            beq shstep_r        ; ,,
            lda VELOCITY,y      ; Is this step a rest?
            bne show_both       ; ,,
            ldx #" "            ; If so, clear the note name and show only
            sta SCREEN+18       ;   the step number
            sta SCREEN+19       ;   ,,
            bne only_step       ;   ,,
show_both:  lda SEQUENCE,y      ; Get the note number
-loop:      cmp #12             ; Show the note name
            bcc notef           ; ,,
            sbc #12             ; ,,
            bcs loop            ; ,,
notef:      tax                 ; X is now the remainder
            lda NoteName,x      ; Get the note name
            sta SCREEN+18       ;   ,,
            lda Accidental,x    ;   and accidental
            sta SCREEN+19       ;   ,,   
only_step:  lda SEQUENCE,y      ; Show note number         
            jsr TwoDigNum       ; ,,
            stx SCREEN+15       ; ,,
            sta SCREEN+16       ; ,, 
shstep_r:   rts 
                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERRUPT HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IRQ handles sequencer playback
IRQSR:      lda SEQ_XPORT       ; Is Play enabled?
            cmp #$02            ; ,,
            beq playback        ; ,,
            cmp #$01            ; Is Record enabled?
            bne irq_r           ; If not, return
            jsr GETMSG          ; Has a complete MIDI message been received?
            bcc irq_r           ; ,,
            cmp #ST_NOTEON      ; And is it a note on message?
            bne irq_r           ; ,,
            tya                 ; If so, move Velocity to A
            ldy SEQ_REC_IX      ;   Get index to the current record step
            sta VELOCITY,y      ;   and store velocity
            txa                 ; Now move note number to A
            sta SEQUENCE,y      ;   and store note number
            pha
            iny                 ; Increment the record index
            sty SEQ_REC_IX      ; ,,
            jsr ShowStep        ; Show step number and note name
            cpy #64             ; Has it reached 64 notes (maximum)?
            bcc irq_r           ; If not, just return
            lda #0              ; Otherwise, reset the record index
            sta SEQ_REC_IX      ; ,,
            beq irq_r
playback:   dec SEQ_COUNT       ; If play is enabled, do the countdown
            beq adv             ; Advance sequencer if count is 0
            lda SEQ_COUNT       ; Just before count finishes, 
            cmp #16             ;   ,,
            bne irq_r           ;   ,,
            ldx SEQ_LAST        ;   turn off previous note
            ldy #0              ;   ,,
            jsr NOTEOFF         ;   ,,
            jmp $eb12
adv:        lda SCREEN+21       ; Flash annunciator at tempo
            eor #$1e            ; ,,
            sta SCREEN+21       ; ,,
            jsr PlayNote        ; Play the next note
irq_r:      jmp $eb12           ; Scan keyboard and RTI


; NMI watches for incoming system exclusive data
NMISR:      pha                 ; NMI does not automatically save registers like
            txa                 ;   IRQ does, so that needs to be done
            pha                 ;   ,,
            tya                 ;   ,,
            pha                 ;   ,,
            jsr CHKMIDI         ; Is this a MIDI-based interrupt?
            bne midi            ;   If so, handle MIDI input
            jmp SYSNMI          ; Back to normal NMI, after register saves
midi:       ldy SEQ_XPORT       ; If in note record mode, ignore sysex
            cpy #$01            ; ,,
            bne sysexwait       ; ,,
            jsr MAKEMSG         ; Build MIDI message
            jsr Monitor         ; Show MIDI byte indicator
            jmp RFI             ; Restore registers and return from interrupt
sysexwait:  jsr MIDIIN
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

;notes:      bit N_LISTEN        ; Are we waiting for a note?
;            bpl note_on 
;            clc 
;            ror N_LISTEN
;            ldx SEQ_REC_IX 
;            sta SEQUENCE,x
;            cpx #63
;            beq r_isr
;            inc SEQ_REC_IX
;            bne r_isr
;note_on:    sta SYIN_IX         ; Temporary storage of incoming MIDI
;            lda #$90            ; Create a note-on template consisting of
;            clc                 ;   status and MIDI channel
;            adc MIDI_CH         ;   ,,
;            sbc #0              ;   ,, (1 indexed in memory)
;            cmp SYIN_IX         ; Is this equal to the incoming byte?
;            bne r_isr           ; If no, return from interrupt
;            ror N_LISTEN
;            jmp RFI
            
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
Copied:     .asc "COPIED    ",0
StatusL:    .byte <Failed,<Received,<Sent,<ChLib,<Welcome,<Generated
            .byte <ClrStatus,<Copied
StatusH:    .byte >Failed,>Received,>Sent,>ChLib,>Welcome,>Generated
            .byte >ClrStatus,>Copied

; MIDI Messages and Headers
EditBuffer: .byte $f0, $01, $32, $03, $ff
PrgDump:    .byte $f0, $01, $32, $02, $ff

; Value Bar Partials
BarPartial: .byte $e7, $ea, $f6, $61, $75, $74, $65, $20

; Library Divisions
LibDiv:     .byte 0,19,39,59

; Key command subtroutine addresses
KeyCode:    .byte F1,F3,F5,F7,PREV,NEXT,INCR,DECR,SEND2BUFF,EDIT
            .byte PREVLIB,NEXTLIB,OPENSETUP,OPENHELP,GENERATE,SETPRG
            .byte VOICEDUMP,CLEAR,COPY,RUN,REST,BACKSP,0
CommandL:   .byte <PageSel-1,<PageSel-1,<PageSel-1,<PageSel-1
            .byte <PrevField-1,<NextField-1,<IncValue-1,<DecValue-1
            .byte <BufferSend-1,<EditName-1,<PrevLib-1,<NextLib-1
            .byte <GoSetup-1,<GoHelp-1,<Generate-1,<SetPrg-1,<VoiceDump-1
            .byte <Erase-1,<CopyLib-1,<Sequencer-1,<AddRest-1,<DelNote-1
CommandH:   .byte >PageSel-1,>PageSel-1,>PageSel-1,>PageSel-1
            .byte >PrevField-1,>NextField-1,>IncValue-1,>DecValue-1
            .byte >BufferSend-1,>EditName-1,>PrevLib-1,>NextLib-1
            .byte >GoSetup-1,>GoHelp-1,>Generate-1,>SetPrg-1,>VoiceDump-1
            .byte >Erase-1,>CopyLib-1,>Sequencer-1,>AddRest+1,>DelNote-1

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


; Transport Annunciators for Sequencer
XportAnn:   .asc $20, $51, $3e

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
            .byte 4,4,4,4,4,4,4,4
            .byte 5
LFIELD:     .byte $80 ; Delimiter, and LFIELD - FPage = field count

; Field row
FRow:       .byte 0,3,3,4,5,6,9,9,9,10,11,12,13,14,17,18,19
            .byte 1,2,3,4,5,7,8,9,10,13,14,15,16
            .byte 1,2,3,4,5,8,9,10,10,10,13,14,15,16,17,18
            .byte 1,2,3,4,7,8,9,10,11,12,13,14,15,16
            .byte 5,6,7,10,11,14,15,16
            .byte 19

; Field column
FCol:       .byte 1,3,8,14,14,14,3,8,12,14,14,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14  
            .byte 14,14,14,14,14,14,14,3,8,12,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14
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
            
            .byte F_MIDICH,F_SWITCH,F_DEVICE,F_64,F_VALUE,F_64,F_64,F_SWITCH
            
            .byte F_NONE

; Field NRPN number
FNRPN:      .byte 88,3,4,0,8,10,5,6,7,1,2,9,11,12,14,15,16
            .byte 17,18,40,19,20,43,45,47,49,44,46,48,50
            .byte 32,33,34,35,36,22,21,23,24,25,26,27,28,29,30,31
            .byte 52,87,53,54,13,37,86,41,42,97,38,39,98,51
            ; These are not really NRPN numbers, but use the CURPRG storage
            ; for menu settings
            .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
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
            .asc RT,"DEVICE      #",CR
            .asc CR,"SEQUENCER",CR
            .asc RT,"STEPS",CR
            .asc RT,"TEMPO",CR
            .asc CR,"GENERATION",CR
            .asc RT,"SEED",CR
            .asc RT,"SEED",CR
            .asc RT,"MUTATE"
            .asc 00
            
Help:       .asc CR," WWW.BEIGEMAZE.COM/ED",CR,CR,CR
            .asc " SPACE SETUP",CR
            .asc " F1-F7 EDIT SCREEN",CR
            .asc " CRSR  PARAM",CR
            .asc " < >   EDIT VALUE",CR
            .asc " - +   LIBRARY",CR
            .asc " B     EDIT BUFFER",CR
            .asc " V     VOICE DUMP",CR
            .asc " G     GENERATE",CR
            .asc " P     SET PROG #",CR
            .asc " CLR   ERASE",CR
            .asc " C     COPY",CR
            .asc " D     DISK",CR
            .asc " RUN   PLAY/STOP",CR
            .asc " C=RUN REC",CR
            .asc 00

; Popup Window            
Window:     .asc 5 ; White
            .asc CR,CR,CR,CR,CR,CR
            .asc RT,RT,RT,RT,RT
            .asc P_TL,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_TR,CR
            .asc RT,RT,RT,RT,RT,P_L,$5f,"   CANCEL",P_R,CR
            .asc RT,RT,RT,RT,RT
            .asc P_L,30,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,5,P_R,CR
            .asc RT,RT,RT,RT,RT,P_L,"          ",P_R,CR
            .asc RT,RT,RT,RT,RT,P_L,"          ",P_R,CR
            .asc RT,RT,RT,RT,RT,P_L,"          ",P_R,CR
            .asc RT,RT,RT,RT,RT
            .asc P_BL,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_BR,CR
            .asc RT,RT,RT,RT,RT,RT,UP,UP,UP,UP ; Position for label
            .asc 30,00
            
PrgLabel:   .asc 5,"PROGRAM #",30,0
DumpMenu:   .asc 5,"VOICE DUMP",CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"P",RVOF,"ROGRAM   ",CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"B",RVOF,"ANK"," ",RVON,"G",RVOF,"ROUP"
            .asc 30,0
            
EraseConf:  .asc 5,"ERASE:",CR
            .asc RT,RT,RT,RT,RT,RT,"ARE YOU",CR
            .asc RT,RT,RT,RT,RT,RT,"SURE?(Y/N)",30,0

CopyLabel:  .asc 5,"COPY TO",30,0
            
                         
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
