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
FPS         = 120               ; IRQs per second
PROCSP      = 1020000           ; Processor speed in cycles per second
IRQ_C       = PROCSP / FPS      ; IRQ countdown value

; Application Memory
; In addition, zero page usage by
; MIDI KERNAL uses $9b - $9f
; SEQ PACKING uses $f9 - $ff
FIELD       = $00               ; Pointer to field memory location (2 bytes)
SYIN        = $02               ; Sysex In pointer (2 bytes)
SYIN_IX     = $04               ; Sysex In position index
PTR         = $05               ; Library pointer (2 bytes)
PTRD        = $07               ; Destination pointer (2 bytes)
LISTEN      = $a0               ; Sysex listen flag (jiffy clock not used)
READY       = $a1               ; Sysex ready flag (jiffy clock not used)
ANYWHERE    = $a2               ; Temporary iterator (jiffy click not used)
TAB         = $c1               ; Save start pointer (2 bytes)

PAGE        = $033c             ; Current page number
VIEW_START  = $033d             ; Library view start entry
FIELD_IX    = $033e             ; Current field index
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
SEQ_COUNT   = $0353             ; Sequence note countdown
SEQ_LAST    = $0354             ; Sequence last note played
RANDOM      = $0355             ; Random number for mutation
FILENAME    = $0356             ; KERNAL filename ($0356-$035f)
PROGNAME    = $0360             ; Unpacked program name ($0360-$0375)
DRAW_IX     = $0376             ; Drawn field index
VIEW_IX     = $0377             ; Field index in Library View
LAST_LIB_IX = $0378             ; Last index in Library View
UNDO_LEV    = $0379             ; Current undo level
LAST_NRPN   = $037a             ; Last NRPN, used for keeping track of Undo
STRIPE      = $037b             ; Mod 2 state for reverse ($80 when reversed)
HALF_TEMPO  = $037c             ; Time left before note off

DISKSETTING = $3e00             ; Memory for these settings on disk
MIDI_CH     = CURPRG+$a0        ; MIDI channel
NRPN_TX     = CURPRG+$a1        ; NRPN Transmit toggle
DEVICE_NUM  = CURPRG+$a2        ; Storage Device number
SEQ_STEPS   = CURPRG+$a3        ; Sequencer Steps
SEQ_TEMPO   = CURPRG+$a4        ; Sequencer Tempo
SEED1_PRG   = CURPRG+$a5        ; Generator Seed 1
SEED2_PRG   = CURPRG+$a6        ; Generator Seed 2
MUTATE      = CURPRG+$a7        ; Mutate flag
SEQ_REC_IX  = CURPRG+$a8        ; Sequence record index

; Application Data Storage
OUTSYSEX    = $1200             ; Outgoing sysex stage
CURPRG      = $1300             ; Current program indexed buffer (128 bytes)
UNDO_NRPN   = $1400             ; NRPN for undo level (64 bytes)
UNDO_VAL    = $1440             ; Value for undo level (64 bytes)
SEED1       = $1480             ; Seed 1 program for generator (128 bytes)
SEED2       = $1500             ; Seed 2 program for generator (128 bytes)
SEQUENCE    = $1580             ; Sequence note data (up to 64 steps)
VELOCITY    = $15c0             ; Sequence velocity data (up to 64 steps)
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
SM_FAIL     = 0                 ; The operation failed
SM_RECV     = 1                 ; Data has been received and is valid
SM_SENT     = 2                 ; Data has been sent
SM_NOTEMPTY = 3                 ; The target program cannot be overwritten
SM_WELCOME  = 4                 ; Welcome message
SM_GEN      = 5                 ; A program has been generated
SM_BLANK    = 6                 ; (Clear status)
SM_COPIED   = 7                 ; A program has been copied
SM_SAVING   = 8                 ; Save is in progress
SM_LOADING  = 9                 ; Load is in progress
SM_OK       = 10                ; The operation was successful
SM_UNDONE   = 11                ; Undo operation completed
SM_UNAVAIL  = 12                ; Trying operation on a non-edit page

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
BL          = $d2               ; Bottom line PETSCII
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
ARROW       = 94                ; Up arrow

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
EDIT        = 15                ; Edit parameter
BACKSP      = 7                 ; Backspace
NEXTLIB     = 5                 ; +
PREVLIB     = 61                ; -
OPENSETUP   = 32                ; Space
CANCEL      = 8                 ; Back arrow
OPENHELP    = 43                ; H
GENERATE    = 19                ; G
SETPRG      = 13                ; P
CLEAR       = 62                ; CLR
VOICESEND   = 54                ; Up Arrow
COPY        = 34                ; C
REST        = 10                ; R
RUN         = 24                ; RUN/STOP
DSAVE       = 41                ; S 
DLOAD       = 21                ; L
PRGREQ      = 48                ; Q
UNDO        = 33                ; Z
HEX         = 26                ; X

; Field Types
F_VALUE     = 0                 ; Value field 0-120
F_PRG       = 1                 ; Library view program information
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
F_MUTATIONS = 15                ; Number of mutations
F_HEX       = 16                ; Full-page hex view
F_TEMPO     = 17                ; Tempo in BPM
F_QCOMP     = 18                ; Q Compensation

; System Resources
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
IRQ         = $eb12             ; System ISR return point
VIC         = $9000             ; VIC starting address
CHROUT      = $ffd2             ; Character out
CASECT      = $0291             ; Disable Commodore case
SYSNMI      = $feb2             ; System NMI
RFI         = $ff56             ; Return from interrupt
KEY         = $c5               ; Pressed key
HOME        = $e581             ; Home cursor
CLSR        = $e55f             ; Clear screen
SHIFT       = $028d             ; SHIFT key status
VIAT        = $9114             ; VIA Timer (2 bytes)
MSGFLG      = $9d               ; KERNAL message mode flag,

; Disk KERNAL Calls
SETLFS      = $ffba             ; Setup logical file
SETNAM      = $ffbd             ; Setup file name
SAVE        = $ffd8             ; Save
LOAD        = $ffd5             ; Load
OPEN        = $ffc0             ; Open logical file
CLOSE       = $ffc3             ; Close logical file
CLALL       = $ffe7             ; Close all files
CHKIN       = $ffc6             ; Define file as input
CHRIN       = $ffcf             ; Get input
CLRCHN      = $ffcc             ; Close channel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
Start:      jsr $fd8d           ; Test RAM, initialize VIC chip
            jsr $fd52           ; Restore default I/O vectors
            jsr $fdf9           ; Initialize I/O registers
            jsr $e518           ; Initialize hardware

            ; Some hardware settings
            sei                 ; Disable interrupt; re-enabled at end of Start
            jsr CLSR            ; Clear screen
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,            
            sta VIAT            ; Start VIA timer
            lda #13             ; Screen color
            sta VIC+$0f         ; ,,
            lda #$40            ; Set aux color to purple for MIDI indicator
            sta VIC+$04         ; ,,
            
            ; Initialize the library
            ldy #64             ; For all 64 locations
            sty ANYWHERE        ; ,,
-loop:      ldy ANYWHERE        ; ,,
            jsr Validate        ; ,, If it's valid, leave it alone (i.e,, from
            beq lib_ok          ; ,, a reset button or something)
            ldy ANYWHERE        ; ,,
            jsr NewLib          ; ,, If it's invalid, create a new entry
lib_ok:     dec ANYWHERE
            bpl loop
            
            ; Set Interupts
            lda #<IRQSR         ; Set IRQ for sequencer playback
            sta CINV            ; ,,
            lda #>IRQSR         ; ,,
            sta CINV+1          ; ,,
            lda #<IRQ_C         ; Set IRQ countdown
            sta $9124           ; ,,
            lda #>IRQ_C         ; ,,
            sta $9125           ; ,,
            lda #<NMISR         ; Set NMI for MIDI input listening
            sta NMINV           ; ,,
            lda #>NMISR         ; ,,
            sta NMINV+1         ; ,,
            jsr MIDIINIT        ; Initialize the MIDI interface

            ; Initialize Application Data
            lda #0              ; Initialize
            sta READY           ;   * Sysex ready flag
            sta LISTEN          ;   * Sysex listen flag
            sta PAGE            ;   * Edit page number
            sta NRPN_TX         ;   * NRPN Transmit
            sta TGTLIB_IX       ;   * Target library entry index
            sta CURLIB_IX       ;   * Current library entry index
            sta SEQ_XPORT       ;   * Sequencer transport
            sta SEQ_PLAY_IX     ;   * Sequencer play index
            sta SEQ_REC_IX      ;   * Sequencer record index
            sta MUTATE          ;   * Generator mutation enable
            sta MSGFLG          ;   * Disable KERNAL messages
            lda #1              ;   * MIDI channel
            sta MIDI_CH         ;     ,,
            sta SEED1_PRG       ;   * Generator seed 1
            lda #8              ;   * Device Number
            sta DEVICE_NUM      ;     ,,
            lda #2              ;   * Generator seed 2
            sta SEED2_PRG       ;     ,,
            lda #8              ;   * Sequencer steps
            sta SEQ_STEPS       ;     ,,
            lda #30             ;   * Sequencer tempo to 120 BPM
            sta SEQ_TEMPO       ;     ,,
            jsr ClearSeq        ; Clear the sequence and velocity data
            
            ; Initialize user interface
            ldy CURLIB_IX       ; Select first library entry
            jsr SelLib          ; ,,
            ldx #SM_WELCOME     ; Show welcome message in status bar
            jsr Status          ; ,,
            jsr ShowPrgNum      ; Show program number
            jsr SwitchPage      ; Generate the edit page
            cli                 ; Start interrupt            
            ; Fall through to MainLoop

; Main Program Loop
; Wait for a key, then act on valid commands            
MainLoop:   lda #$40            ; Debounce the key press
-debounce:  cmp KEY             ; ,,
            bne debounce        ; ,,
waitkey:    ldx READY           ; If Sysex is ready, handle it
            bne SysexReady      ; ,,
            lda KEY             ; Get key press
            sta KEYBUFF         ; Store pressed key to avoid race conditions
            cmp #$40            ; If no key down, wait
            bne keydown         ; ,,
            ldx #$30            ; Reset key repeat rate
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
SysexReady: lda #0              ; Clear the sysex ready flag
            sta READY           ; ,,
            lda #" "            ; Disappear the MIDI indicator
            sta SCREEN          ; ,,            
            ldy CURLIB_IX       ; Is this a valid Prophet 5 voice dump?
            jsr Validate        ; ,,
            bne SysexFail       ; ,,
            ldx #SM_RECV        ; Show received success status
            jsr Status          ; ,,
            jsr ShowPrgNum      ; Add Program Number to the status
            lda PTR             ; PTR was set above by Validate. Use it to
            ldy PTR+1           ;   unpack the sysex to the buffer
            jsr UnpBuff         ;   ,,
            jsr PopFields       ;   ,,            
lib_end:    jmp MainLoop
SysexFail:  lda #$83            ; This has failed, so make it an unset program,
            ldy #4              ;   so that it's not accidentally seen as
            sta (PTR),y         ;   an actual program by some routines/
            ldx #SM_FAIL
            jsr Status
            jmp MainLoop
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; Previous Library Entry
PrevLib:    lda PAGE            ; If on the Library View, treat this keypress
            cmp #4              ;   as a Previous Field instead.
            beq PrevField       ;   ,,
            ldy CURLIB_IX
            jsr PackLib
            lda #1              ; Default value to substract, one
            sta IX              ; ,,
            ldy SHIFT           ; If shift is held, decrement by 10 instead
            beq pl_def          ; ,,
            lda #10             ; ,,
            sta IX              ; ,,
pl_def:     lda CURLIB_IX       ; Subtract the specified number from the
            sec                 ;   library number
            sbc IX              ;   ,,
            sta CURLIB_IX       ;   ,,
            bcs switchlib       ; If it's below 0, set it back to 0
            lda #0              ; ,,
            sta CURLIB_IX       ; ,,
switchlib:  jsr ClrCursor
            ldy CURLIB_IX
            sty TGTLIB_IX
            jsr SelLib
            jsr ShowPrgNum
            jsr PopFields
            ldx #SM_BLANK
            jsr Status
chlib_r:    jmp MainLoop

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
            jsr LibViewF        ; Handle library change if in Library View
            jsr DrawCursor
pf_r:       jmp MainLoop

; Next Library Entry
NextLib:    lda PAGE            ; If on the Library View, treat this keypress
            cmp #4              ;   as a Next Field instead.
            beq NextField       ;   ,,
            ldy CURLIB_IX
            jsr PackLib
            lda #1              ; Default value to add, one
            sta IX              ; ,,
            ldy SHIFT           ; If shift is held, increment by 10 instead
            beq nl_def          ; ,,
            lda #10             ; ,,
            sta IX              ; ,,
nl_def:     lda CURLIB_IX       ; Add the specified number to the
            clc                 ;   library number
            adc IX              ;   ,,
            cmp #LIB_TOP        ; If it overflows, set back to 64
            bcc nl_set          ; ,,
            lda #LIB_TOP-1      ; ,,
nl_set:     sta CURLIB_IX       ; Store the new index
            jmp switchlib       ; Swith library from PrevLib
                      
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
            jsr LibViewF        ; Handle library change if in Library View
            jsr DrawCursor
nf_r:       jmp MainLoop
                                            
; Select Page           
PageSel:    sec
            sbc #39             ; Determines which F key was pressed
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            ldy SHIFT           ; If C= or SHIFT is held down, jump to
            bne LibView         ;   Library selection
            cmp PAGE            ; If already on this page, don't redraw
            beq read_r          ; ,,
            sta PAGE            ; Set the page and draw it
            jsr SwitchPage      ; ,,
read_r:     jmp MainLoop
            
; Select Library Section
LibView:    tay                 ; Get library division for this key
            lda LibDiv,y        ; Set view starting point
            sta VIEW_START      ; ,,
            lda #4              ; Set page number
            sta PAGE            ; ,,
            lda TopParamIX+4    ; Set last library parameter index
            sta LAST_LIB_IX     ; ,,
            ldy VIEW_START      ; Select the view start entry on view load
            sty CURLIB_IX       ; ,,
            jsr SelLib          ; ,,
            ldy CURLIB_IX       ; Show program number
            jsr ShowPrgNum      ; ,,
            jsr PopFields       ; ,,            
            jsr SwitchPage      ; Draw the page header
            jmp MainLoop

; Increment Field by 1
IncValue:   lda FIELD_IX        ; For the tempo field type, increment means
            cmp #(TEMPO_FLD-FType)
            beq dec_bypass      ;   decrement!
inc_bypass: jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeH,y
            bcs id_r            ; Already at maximum, so do nothing
            jsr NRPNpre         ; Pre-change (Undo)
            inc CURPRG,x
nrpn_msg:   jsr NRPNpost        ; Send NRPN message, handle Undo
            ldy FIELD_IX
            jsr DrawField
            ldy FIELD_IX
            lda FType,y         ; Some field types should not debounce the
            cmp #F_VALUE        ;   key, so check those types here
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
            cmp #$07
            bcc topspeed
            dec REPEAT
            dec REPEAT
            dec REPEAT
topspeed:   jmp waitkey

; Decrement Field by 1
DecValue:   lda FIELD_IX        ; For the tempo field type, decrement means
            cmp #(TEMPO_FLD-FType)
            beq inc_bypass      ;   increment!
dec_bypass: jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeL,y
            beq id_r            ; Already at minimum, so do nothing
            jsr NRPNpre         ; Pre-change (Undo)
            dec CURPRG,x
            jmp nrpn_msg

; Single-Key Edit
; * Toggles Switches
; * Edits Name
; * Selects Programs in Library
; * Advances Others
EditName:   ldy FIELD_IX        ; Check the field's type for this edit     
            lda FType,y         ;   behavior.
            cmp #F_NAME         ; If it's a name, it will be edited
            beq edit_name       ; ,,
            cmp #F_PRG          ; If it's a program, it will be selected
            beq sel_prog        ; ,,
            tya                 ; Save Y for after undo level
            pha                 ; ,,
            ldx FNRPN,y         ; Pass NRPN number to NRPNpre to set undo
            jsr NRPNpre         ;   ,,
            pla                 ;   ,,
            tay                 ;   ,,
            ldx FNRPN,y         ; Anything else will advance to its max
            lda FType,y         ;   and then roll back to 0
            tay                 ;   ,,
            lda CURPRG,x        ;   ,,
            cmp TRangeH,y       ;   ,,
            bcc adv_f           ;   ,,
            lda TRangeL,y       ;   ,, If above high range, set to low
            sta CURPRG,x        ;   ,,   and save that
            jmp val_ch          ;   ,,
adv_f:      inc CURPRG,x        ;   ,,
val_ch:     ldx FIELD_IX        ;   Send NRPN, if enabled, handle Undo
            jsr NRPNpost        ;   ,,
            ldy FIELD_IX        ;   Draw the new field value
            jsr DrawField       ;   ,,
            jmp MainLoop        ;   ,,
sel_prog:   tya                 ; Field index
            clc                 ;   ,,
            adc VIEW_START      ;   plus start-of-view
            sec                 ;   ,,
            sbc TopParamIX+4    ;   minus first page parameter...
            sta CURLIB_IX       ; ...Equals the new current program index
            tay                 ; Select this library entry
            jsr SelLib          ; ,,
            ldy CURLIB_IX       ; Show program number
            jsr ShowPrgNum      ; ,,
            lda #0              ; Drill down to edit page
            sta PAGE            ; ,,
            jsr SwitchPage      ; ,,
            jmp MainLoop
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

; Generate Program
; From two spefcified parent programs in the library
Generate:   ldy CURLIB_IX       ; First, make sure that the current library
            jsr Validate        ;   entry is either invalid, or lacks a
            bne gen_ok          ;   program number. We don't want to overwrite
            ldy #4              ;   something that's already in the library.
            lda (PTR),y         ;   ,,
            bmi gen_ok          ;   ,,
            ldx #SM_NOTEMPTY    ;   ,, Show failure message if not allowed
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
            lda MUTATE          ; Mutate generated program if enabled
            beq no_mutate       ; ,,
            sta IX              ; Store mutation count
mutate:     jsr Rand31          ; Get five-bit pseudorandom number
            cmp #22             ; Find a mutable parameter index
            bcs mutate          ; ,,
            tax                 ; Get the NRPN index of a mutable parameter
            lda Mutable,x       ;   ,,
            tax                 ;   which is in X
-loop:      jsr Rand127         ; Get a value between 0-120
            cmp #121            ; ,,
            bcs loop            ; ,,
            sta CURPRG,x        ; Store it in the current program
            dec IX              ; Decrement the mutation count
            bne mutate          ; Go back for more
no_mutate:  jsr SetLibPtr       ; ,,
            jsr PackLib         ; Pack program data to library
            ldx #SM_GEN         ; Write status message when done
            jsr Status          ; ,,
            jsr PopFields       ; Update the buffer fields in the interface
gen_r:      jmp MainLoop
             
; Set Program Number
; for current program buffer                    
SetPrg:     jsr Popup
            lda #<PrgLabel
            ldy #>PrgLabel
            jsr PrintStr
            ldy CURLIB_IX       ; Get program location to PRGLOC
            jsr PrgLoc          ; ,,
            lda PTR             ; Unpack buffer prior to program # change
            ldy PTR+1           ; ,,
            jsr UnpBuff         ; ,,
            ldy #2              ; Set up editor with current program
-loop:      lda PRGLOC,y        ; ,,
            sta WINDOW_ED,y     ; ,,
            dey                 ; ,,
            bpl loop            ; ,,    
            ldy #3              ; Cursor position in edit field        
            jsr SetPrgNum       ; Get program number from user
            bcs setp_r          ; Return if cancel or error
            ldy #4              ; Get the user input from PTRD and update the
            lda PTRD            ;   actual sysex in the library with the
            sta (PTR),y         ;   new program number
            iny                 ;   ,,
            lda PTRD+1          ;   ,,
            sta (PTR),y         ;   ,,
            jsr ShowPrgNum      ;   ,,
setp_r:     jsr SwitchPage      ; Housekeeping. Redraw the page.
            jmp MainLoop
            
; System Exclusive Voice Dump
; of program, bank, or group
VoiceSend:  ldy CURLIB_IX       ; If this is not a valid program, cannot
            jsr Validate        ;   do dump
            bne dump_r          ;   ,,
            jsr Popup           ; Put the dump selection menu 
            ldy #4              ; Does this entry have a program number?
            lda (PTR),y         ; ,,
            bpl all_opt         ; ,, If so, show all options
            lda #<SendMenu2     ; ,, If no prog number, show only edit buffer
            ldy #>SendMenu2     ; ,,   option
            jmp voice_menu
all_opt:    lda #<SendMenu      ; Put options into popup window
            ldy #>SendMenu      ;   ,,
voice_menu: jsr PrintStr        ;   ,,
vgetkey:    jsr Keypress        ; Get pressed key
            cpy #CANCEL         ; Check for cancel key
            beq dump_r          ; ,,
            cmp #"E"            ; Dumping edit buffer?
            bne ch_prg          ; ,,
            jmp BufferSend      ; ,,
ch_prg:     cmp #"P"            ; Dumping this program?
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
Erase:      jsr Popup
            lda #<EraseConf
            ldy #>EraseConf
            jsr PrintStr
            jsr Keypress
            cmp #"Y"
            bne erase_r
            ldy CURLIB_IX
            jsr SetLibPtr
            jsr NewLib
            lda PTR 
            ldy PTR+1
            jsr UnpBuff 
            jsr ShowPrgNum
erase_r:    jsr SwitchPage
            ldx #SM_BLANK
            jsr Status
erase_r2:   jmp MainLoop

; Copy the current program
; to another location
CopyLib:    jsr Popup
            lda #<CopyLabel
            ldy #>CopyLabel
            jsr PrintStr
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
            lda #$83            ; Set the copy's program number as unset
            ldy #4              ; ,,
            sta (PTRD),y        ; ,,
            ldx #SM_COPIED      ; Indicate copy success
            jsr Status          ; ,,
            jsr ShowPrgNum      ; Show program number, in case copy to same lib
copy_r:     jsr SwitchPage
            jmp MainLoop

; Sequencer control
Sequencer:  ldx SEQ_LAST        ; Turn off last note whenever a transport
            ldy #0              ;   control is activated
            jsr NOTEOFF         ;   ,,
            lda MIDI_CH         ; Set MIDI channel
            sec                 ; ,,
            sbc #1              ; ,,
            jsr SETCH           ; ,,
            lda SEQ_XPORT       ; Is transport currently on?
            beq start           ;   If not, start the sequencer
            lda #0              ; Stop the sequencer
            sta SEQ_XPORT       ; ,,
            jmp cl_notedis      ; Clear note display and annunciator
start:      lda SHIFT           ; Is Commodore key held down?
            and #$02            ; ,,
            beq startplay       ; If not, start the playback
            lda #$01            ; Turn on the record transport bit
            sta SEQ_XPORT       ; ,,
            ldy SEQ_REC_IX      ; Show step number and name
            jsr ShowStep        ; ,,
            jmp annunciate      ; ,,
startplay:  lda #$02            ; Turn on the record playback bit
            sta SEQ_XPORT       ; ,,
            ldx #$ff            ; Reset the play index
            stx SEQ_PLAY_IX     ; ,,
            jsr PlayNote        ; Play the first note
cl_notedis: lda #" "            ; Clear the note number display
            sta SCREEN+15       ; ,,
            sta SCREEN+16       ; ,,
            sta SCREEN+18       ; ,, And the note name display
            sta SCREEN+19       ; ,,            
annunciate: ldx SEQ_XPORT       ; Get the sequencer transport graphic
            lda XportAnn,x      ;   from the annunicator table and
            sta SCREEN+21       ;   put it in the top right corner.
seq_r:      jmp MainLoop

; Delete Last Sequencer Note
DelNote:    lda SEQ_XPORT       ; Is the sequencer in record status?
            cmp #1              ; ,,
            bne del_r           ; If not, do nothing
            ldy SEQ_REC_IX      ; Is the record head at the beginning?
            beq del_r           ;   If so, do nothing
            dey                 ; Decrement the note index
            sty SEQ_REC_IX      ; ,,
            ldy SEQ_REC_IX      ; Clear out the note
            lda #0              ; ,,
            sta SEQUENCE,y      ; ,,
            sta VELOCITY,y      ; ,, Make it a rest, too
            jsr ShowStep
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
            cpy #64
            beq rest_r
            jsr ShowStep        ; Update display, show num but remove name
rest_r:     jmp MainLoop

; Go to Setup, Help, or Hex View
GoHex:      lda #5
            .byte $3c           ; Skip word (SKW)
GoSetup:    lda #6
            .byte $3c           ; Skip word (SKW)
GoHelp:     lda #7
            cmp PAGE
            beq setup_r
            sta PAGE
            jsr SwitchPage
setup_r:    jmp MainLoop

; Disk Save
GoSave:     jsr Popup
            lda #<SaveLabel
            ldy #>SaveLabel
            jsr PrintStr
            jsr SetName         ; SetName calls SETNAM
            bcs disk_canc       ; Cancel, so return
            jsr StopSeq         ; Stop sequence if name OK
            ldy #$10            ; Move settings to saved area
-loop:      lda CURPRG+$a0,y    ; ,,
            sta DISKSETTING,y   ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            ldx DEVICE_NUM      ; Device number
            ldy #0              ; Command (none)
            jsr SETLFS          ; ,,                  
            ldx #SM_SAVING      ; Show "SAVING..."
            jsr Status          ; ,,
            lda #<SEQUENCE      ; Set up KERNAL SAVE
            sta TAB             ; ,,
            lda #>SEQUENCE      ; ,,
            sta TAB+1           ; ,,
            ldx #$10            ; ,, (top of save, low)
            ldy #$3e            ; ,, (top of save, high)
            lda #$c1            ; ,, (location of start)
            jsr SAVE
            bcc disk_ok
            ; Fall through to disk_error
            
; Disk Error Message
disk_error: ldx #SM_FAIL
            jsr Status
            jsr SwitchPage
            jmp MainLoop

; Disk Load
GoLoad:     jsr Popup
            lda #<LoadLabel
            ldy #>LoadLabel
            jsr PrintStr    
            jsr SetName         ; SetName calls SETNAM
            bcs disk_canc       ; Cancel, so return
            jsr StopSeq         ; Stop sequence if name OK            
            ldx DEVICE_NUM      ; Device number
            ldy #1              ; Load to header location
            jsr SETLFS          ; ,,
            ldx #SM_LOADING
            jsr Status            
            lda #0              ; Perform load
            jsr LOAD            ; ,,      
            bcc load_ok
            jmp disk_error
load_ok:    ldy CURLIB_IX       ; After successful load, unpack the current
            jsr SetLibPtr       ;   (probably new) library entry to the
            lda PTR             ;   program buffer
            ldy PTR+1           ;   ,,
            jsr UnpBuff         ;   ,,
            ldy #$10
-loop:      lda DISKSETTING,y   ; Get settings from disk save area
            sta CURPRG+$a0,y    ; Put it in setting memory
            dey                 ; ,,
            bpl loop            ; ,,            
            ; Fall through to disk_ok
            
            ; Shared exit points for both save and load
disk_ok:    ldx #SM_OK          ; Show success message
            jsr Status          ; ,,
disk_canc:  jsr SwitchPage      ; Back to main
            jmp MainLoop        ; ,,
            
; Request Program
Request:    ldy CURLIB_IX
            sty TGTLIB_IX       ; Want the requested program to go HERE
            jsr SetLibPtr       ; Get pointer to sysex in library
            ldy #4              ; Cannot use the request for a library entry
            lda (PTR),y         ;   that already contains a program
            bmi req_c           ;   ,,
            ldx #SM_NOTEMPTY    ;   ,,
            jsr Status
req_r1:     jmp MainLoop        ; Not necessary to switch page here
req_c:      jsr Popup
            lda #<ReqLabel
            ldy #>ReqLabel
            jsr PrintStr
            ldy #0              ; Set up editor with current program
            jsr SetPrgNum       ; Get program number from user
            bcs req_r2          ; Return if cancel or error
            lda #<PrgRequest    ; Generate a Program Request message
            ldy #>PrgRequest    ;   from the message table
            jsr SysexMsg        ; Add it to the outgoing sysex buffer
            lda PTRD            ; Add the group number to the sysex
            sta OUTSYSEX+4      ; ,,
            lda PTRD+1          ; Add the bank/program number to the sysex
            sta OUTSYSEX+5      ; ,,
            lda #ST_ENDSYSEX    ; Add the end-of-sysex message
            sta OUTSYSEX+6      ; ,,
            jsr SendSysex       ; Send the request message
            ldx #SM_SENT        ; Show status and await dump from Prophet 5
req_s:      jsr Status          ; ,,
req_r2:     jsr SwitchPage      ; Redraw the page
            jmp MainLoop
         
; Undo
; If undo level > 0, then get last NRPN and set it to its last value           
Undo:       lda SHIFT           ; Commodore must be held for Undo
            cmp #$02            ; ,,
            bne undo_r          ; ,,
            ldy UNDO_LEV        ; At least one level must be available
            beq undo_r          ; ,,
            lda UNDO_NRPN,y     ; Get the NRPN number for the level
            pha                 ; ,, (store for DrawByNRPN, below)
            tax                 ; ,, (store in X)
            lda UNDO_VAL,y      ; Get the value for the level
            sta CURPRG,x        ; Restore the program value
            dec UNDO_LEV        ; Go to the next level
            lda #$ff            ; Reset the last NRPN number
            sta LAST_NRPN       ; ,,
            jsr NRPNpost        ; Transmit NRPN CCs, if enabled (passing X)
            ldx #SM_UNDONE      ; Show undo message
            jsr Status          ; ,,
            pla
            jsr DrawByNRPN      ; ,,
            ldy UNDO_LEV
            jsr TwoDigNum
            stx STATUSDISP+20
            sta STATUSDISP+21
undo_r:     jmp MainLoop          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERFACE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Filename Field
; If canceled, return with carry set
; If OK, return with carry clear and call SETNAM
SetName:    lda #"."            ; Add file extension .P5
            sta WINDOW_ED+7     ; ,,
            lda #16             ; ,, (P)
            sta WINDOW_ED+8     ; ,,
            lda #"5"            ; ,,
            sta WINDOW_ED+9     ; ,,
            ldy #0              ; Set up editor with cursor position set
            sty IX              ;   at the beginning            
            lda #TXTCURSOR      ;   ,,
            sta WINDOW_ED,y     ;   ,,
fgetkey:    jsr Keypress        ; Keycode in Y, PETSCII in A
            cpy #CANCEL         ; Cancel
            bne fch_bk          ; ,,
            sec 
            rts
fch_bk:     cpy #BACKSP         ; Has backspace been pressed?
            beq fbacksp          ; ,,
            cpy #EDIT           ; Has return been pressed?
            beq fdone           ; ,,
            cmp #"0"            ; Constrain values for character
            bcc fgetkey         ; ,,
            cmp #"Z"+1          ; ,,
            bcs fgetkey         ; ,,
            ldy IX              ; ,,
            cpy #6              ; Limit size of filename
            bcs fgetkey         ; ,,
            sta FILENAME,y      ; Store PETSCII in FILENAME storage
            jsr PETtoScr        ; Convert to screen code for display
            ldy IX              ; ,,
            sta WINDOW_ED,y     ; ,,
            inc IX              ; Advance the cursor
fpos_cur:   lda #TXTCURSOR      ; ,, Add cursor at end
            ldy IX              ; ,,
            sta WINDOW_ED,y     ; ,,
            bne fgetkey         ; ,,
fbacksp:    ldy IX
            beq fgetkey
            lda #" "
            sta WINDOW_ED,y
            sta FILENAME,y
            dec IX
            jmp fpos_cur
fdone:      ldy IX
            cpy #0              ; Do not allow RETURN if there's no name
            beq fgetkey         ; ,,
            lda #"."            ; Add file extension .P5
            sta FILENAME,y      ; ,,
            iny                 ; ,,
            lda #"P"            ; ,,
            sta FILENAME,y      ; ,,
            iny                 ; ,,
            lda #"5"            ; ,,
            sta FILENAME,y      ; ,,
            iny                 ; ,,
            tya                 ; Length of name for SETNAM call
            ldx #<FILENAME      ; Pointer to name (low)
            ldy #>FILENAME      ; Pointer to name (high)
            jsr SETNAM          ; Call SETNAM
            clc                 ; Carry clear for return
            rts

; Program Number Input
; Cursor position in Y
SetPrgNum:  sty IX              ; ,, Set current cursor position
            lda #TXTCURSOR      ; ,, Add cursor at end
            sta WINDOW_ED,y     ; ,,
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
            bne pgetkey         ; ,,
            lda PRGLOC          ; First character
            cmp #"-"            ;   If it's still "-" then it's invalid
            beq pn_inval        ;   ,,
            sec                 ; Subtract 1, because group is zero-indexed
            sbc #1              ; ,,
            and #$07            ; Constrain to actual group number
            sta PTRD            ; Store in destination location
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
            ldy #5              ; Store in the destination location
            sta PTRD+1          ;   and that's it!
            clc                 ; Clear carry to indicate everything's good
pn_inval:   rts                 ; Return with carry set if invalid

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
            jsr PrintStr           ; ,,
            lda #1              ; Use param #1 if page 1
            ldx PAGE            ; Get top parameter number for this page
            beq page1           ; ,,
            lda TopParamIX,x    ; ,,
page1:      sta FIELD_IX        ; ,,            
            ; Fall through to PopFields

; Populate Fields
; at PAGE        
PopFields:  lda PAGE            ; If on the Library View, recall the last
            cmp #4              ;   index used
            bne pf_prg          ;   ,,
            lda LAST_LIB_IX     ;   ,,
            sta FIELD_IX        ;   ,,
pf_prg:     ldy CURLIB_IX       ; Get current library entry 
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
            cpx #7              ; If Help page, do not populate any fields
            bne params          ; ,,
            rts                 ; ,,
params:     ldy TopParamIX,x
-loop:      lda FPage,y         ; Get the page number of the field
            cmp PAGE            ; Is the next field on the current page? 
            bne DrawCursor      ; If not, then fields are done
            sty DRAW_IX         ; Drawn field index
            jsr DrawField       ; Draw the field
            ldy DRAW_IX         ; Bring back Y as iterator
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
; Alias as the Blank field type
Blank:
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
   
; Draw Field by NRPN
; With NRPN supplied in A, if it's on the same edit page        
DrawByNRPN: ldy #0
-loop:      cmp FNRPN,y
            beq nrpn_found
            iny
            cpy #(LFIELD-FPage)
            bne loop
d_nrpn_r:   rts
nrpn_found: lda PAGE
            cmp FPage,y
            bne d_nrpn_r
            ; Fall through to DrawField
            
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
draw_r:     rts                 ; Pull the draw address off the stack, dispatch
         
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
WriteText:  sta PTRD
            sty PTRD+1
            ldy #0
-loop:      lda (PTRD),y
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
-loop:      lda #" "            ;   bottom 2 rows, which are used for status,
            sta SCREEN+22,x     ;   and the top row, used for the sequencer.
            sta SCREEN+230,x    ;   ,,
            lda #PARCOL         ;   ,, (for parameters)
            sta COLOR,x         ;   ,,
            sta COLOR+230,x     ;   ,,
            dex                 ;   ,,
            cpx #$ff            ;   ,,
            bne loop            ;   ,,
            lda #<COLOR         ; Set margin cursor to the select color
            sta PTRD             ; ,,
            lda #>COLOR         ; ,,
            sta PTRD+1          ; ,,
            ldy #22             ; ,,
            ldx #0              ; ,,
-loop:      lda #SELCOL         ; ,,
            sta (PTRD,x)        ; ,,
            lda PTRD            ; ,,
            clc                 ; ,,
            adc #22             ; ,,
            sta PTRD            ; ,,
            bcc nc_cl           ; ,,
            inc PTRD+1          ; ,,
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
            lda #" "            ; Disappear the MIDI indicator
            sta SCREEN          ; ,,
            jmp HOME
            
; Display Status Message
; in X            
Status:     txa 
            pha
            lda #<(STATUSDISP+12)
            sta FIELD
            lda #>(STATUSDISP+12)
            sta FIELD+1
            lda StatusH,x
            tay 
            lda StatusL,x
            jsr WriteText
            pla
            tax
            rts

; Show Current Program Number
ShowPrgNum: ldy CURLIB_IX       ; Get current program number
            jsr PrgLoc          ; ,,
            ldy #2              ; Show received program number
-loop:      lda PRGLOC,y        ; ,,
            sta STATUSDISP+2,y  ; ,,
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
Popup:      ldx #SM_BLANK       ; Blank status prior to popup
            jsr Status          ; ,,
            ldy #230            ; Color everything blue before opening
            lda #6              ;   a popup window
-loop:      sta COLOR,y         ;   ,,
            sta COLOR+230,y     ;   ,,
            dey                 ;   ,,
            cpy #0              ;   ,,
            bne loop            ;   ,,
            jsr HOME            ; Places a popup window, and positions cursor
            lda #<Window        ;   for a top-line label
            ldy #>Window        ;   ,,
            jsr PrintStr        ;   ,,
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
            cpy #RUN            ; If RUN is pressed,
            beq stop_seq        ;   always stop the sequencer
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
stop_seq:   jsr StopSeq
            jmp Keypress
         
; Library View Field Change
; When a field is changed in Library View, it changes the current program            
LibViewF:   lda PAGE            ; Are we on the Library View?
            cmp #4              ; ,,
            bne lvf_r           ; ,, return if not
            lda FIELD_IX        ; Field index
            sta LAST_LIB_IX     ;   ,, (Preserve last library index)
            clc                 ;   ,,
            adc VIEW_START      ;   plus start-of-view
            sec                 ;   ,,
            sbc TopParamIX+4    ;   minus first page parameter...
            sta CURLIB_IX       ; ...Equals the new current program index
            sta TGTLIB_IX       ; ...and the target program index
            tay                 ; Select this library entry
            jsr SelLib          ; ,,
            ldy CURLIB_IX       ; Show program number
            jsr ShowPrgNum      ; ,,
            jsr PopFields       ; ,,
lvf_r:      rts

; Put Hex on Screen
; at FIELD pointer
; with value in A
;      column number in X
PutHex:     pha                 ; Save A twice; once to get low nybble, and once
            pha                 ;   to use A for reverse (stripe) state
            txa                 ; Set stripe state. Put X (column) into A 
            ror                 ;   Sets carry if it's odd 
            ror                 ;   Rolls carry back into bit 7 if it's odd 
            and #$80            ;   Gets rid of everything else
            sta STRIPE          ;   Stores as stripe state (see a little below)
            pla                 ; Get back the original A 
            lsr                 ; Shift high nybble into low
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            jsr put_nyb         ; Put the nybble on screen and advance FIELD
            pla                 ; Get the input byte back
            and #$0f            ;   and mask for low nybble only
put_nyb:    cmp #$0a            ; If > 9, it's a letter
            bcc dec_dig         ;   ,,
            sec                 ;   so subtract 9 to get the letter screen code
            sbc #$09            ;   ,,
            .byte $3c           ; Skip word (SKW)
dec_dig:    ora #$30            ; Decimal digit, so get the digit screen code
            ldx #0              ; Store at FIELD pointer
            ora STRIPE          ; If stripe state is $80, make this reverse
            sta (FIELD,x)       ; ,,
IncFIELD:   inc FIELD           ; Increment field
            bne ph_r
            inc FIELD+1
ph_r:       rts

; Print String
; A = low, Y = high
; Sort of like $cb1e, but uses a ZIP pointer so that size can exceed 255
; characters.
PrintStr:   sta FIELD
            sty FIELD+1
            ldx #0
-loop:      lda (FIELD,x)
            beq prstr_r
            jsr CHROUT
            jsr IncFIELD
            jmp loop
prstr_r:    rts
            
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
            bcc bsend_ok
            ldx #SM_FAIL 
            .byte $3c           ; Skip word (SKW)
bsend_ok:   ldx #SM_SENT
            jsr Status
bsend_r:    jsr SwitchPage
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
            lda #$81            ;   ,,
            ldy #4              ;   ,,
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
            ldy #$9e            ; Make sure that the byte in offset $9E is
            sta (PTR),y         ;   the end of sysex, and that the byte in 
            iny                 ;   offset $9F is $FE (active sensing)
            lda #ST_SENSE       ;   ,,
            sta (PTR),y         ;   ,,
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
lib_good:   ldy #$80            ; When a new program is selected, clear
            lda #0              ;   the Undo buffer
            sta UNDO_LEV        ;   ,, and reset the undo level
-loop:      sta UNDO_NRPN,y     ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda #$ff            ; Set last NRPN to unset
            sta LAST_NRPN       ; ,,
            lda PTR             ; Unpack the library into the edit
            ldy PTR+1           ;   buffer
            jmp UnpBuff         ;   ,,
            
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
-loop:      inx                 ; Set name as INIT
            iny                 ; ,,
            lda Init,x          ; ,,
            sta (PTR),y         ; ,,
            bne loop            ; ,,
            ldy #3              ; Create the sysex header for a valid program
-loop:      lda PrgDump,y       ; ,,
            sta (PTR),y         ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            ldy #$9e            ; ,,
            lda #ST_ENDSYSEX    ; Create the end-of-sysex delimiter
            sta (PTR),y         ; ,,
            lda #$82            ; For a new library entry, set the program
            ldy #4              ;   number to unset
            sta (PTR),y         ;   ,,
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

; Get random numbers of specified sizes
; For mutation
Rand31:     lda #%00001000      ; 5-bit 
            .byte $3c           ; Skip word (SKW)
Rand127:    lda #%00000010      ; 7-bit
            sta RANDOM
-loop:      jsr PRand
            rol RANDOM
            bcc loop
            lda RANDOM
            rts

; Pre NRPN Change
; Manage undo levels    
; NRPN index is in X
NRPNpre:    cpx LAST_NRPN       ; If the last field has changed again,
            beq pre_r           ;   do nothing
            cpx #$90            ; If this is one of the settings parameters
            bcs pre_r           ;   for Ed, do not create an Undo level
            stx LAST_NRPN       ; Store the last NRPN
            ldy UNDO_LEV        ; If there are undo levels remaining,
            cpy #64             ; ,,
            bcc save_lev        ; ,, save a new level
            ldy #1              ; If the level is at 64, then move
-loop:      lda UNDO_NRPN,y     ;   the current levels down one,
            sta UNDO_NRPN-1,y   ;   resulting in the loss of the
            lda UNDO_VAL,y      ;   oldest undo level
            sta UNDO_VAL-1,y    ;   ,,
            iny
            cpy #64             ;   ,,
            bne loop            ;   ,,
            ldy #63             ; Set index back to 63 for next instruction
save_lev:   iny                 ; Move level pointer
            sty UNDO_LEV        ; ,,              
            lda CURPRG,x        ; Get pre-change value
            sta UNDO_VAL,y      ; Store it in UNDO value list
            txa                 ; ,,
            sta UNDO_NRPN,y     ; Store NRPN number in value list
pre_r:      rts            
                        
; Post NRPN Change
; Send NRPN, if enabled
; NRPN index is in X
NRPNpost:   lda NRPN_TX         ; Skip the whole thing is NRPN is disabled
            beq post_r          ; ,,
            cpx #$90            ; If this is one of the settings
            bcs post_r          ;   parameters for Ed, do not send to P5
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
post_r:     rts

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
            lda SEQ_TEMPO       ; Reset tempo countdown
            sta SEQ_COUNT       ; ,,
            lsr                 ; Set the time at which the note is
            sta HALF_TEMPO      ;   turned off
            rts

; Clear the sequence            
ClearSeq:   ldy #63             ; Fill 64 bytes
            lda #0              ; With rests
-loop:      sta SEQUENCE,y      ; ,,
            sta VELOCITY,y      ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            rts

; Stop the sequencer            
; For disk operations
StopSeq:    ldx SEQ_LAST        ; Turn off previous note
            ldy #0              ;   ,,
            jsr NOTEOFF         ;   ,,
            lda #0              ; Turn off sequencer
            sta SEQ_XPORT       ; ,,
            lda #" "            ; Turn off sequence annunciator
            sta SCREEN+21       ; ,, 
            rts
            
; Show Step
; Step number in Y (zero-indexed)
ShowStep:   lda VELOCITY,y      ; Is this step a rest?
            bne show_both       ; ,,
            ldx #TXTCURSOR      ; If so, clear the note name and show only
            stx SCREEN+18       ;   the step number
            ldx #" "            ;   ,,
            stx SCREEN+19       ;   ,,
            bne only_step       ;   ,,
show_both:  lda SEQUENCE,y      ; Get the note number
-loop:      cmp #12             ; Show the note name
            bcc notef           ; ,,
            ;sec                ; ,, (carry is already known to be set here)
            sbc #12             ; ,,
            bcs loop            ; ,,
notef:      tax                 ; X is now the remainder
            lda NoteName,x      ; Get the note name
            sta SCREEN+18       ;   ,,
            lda Accidental,x    ;   and accidental
            sta SCREEN+19       ;   ,,   
only_step:  iny                 ; Increment step number by 1 for 1-indexed
            tya                 ;   display
            jsr TwoDigNum       ; Show step number
            stx SCREEN+15       ; ,,
            sta SCREEN+16       ; ,, 
            dey                 ; Decrement to put Y back where it was
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
            jsr ShowStep        ; Show step number and note name
            iny                 ; Increment the record index
            sty SEQ_REC_IX      ; ,,
            cpy #64             ; Has it reached 64 notes (maximum)?
            bcc irq_r           ; If not, just return
            lda #0              ; Otherwise, reset the record index
            sta SEQ_REC_IX      ; ,,
            beq irq_r
playback:   dec SEQ_COUNT       ; If play is enabled, do the countdown
            beq adv             ; Advance sequencer if count is 0
            lda SEQ_COUNT       ; Before count finishes, 
            cmp HALF_TEMPO      ;   ,,
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
            jmp RFI             ; Restore registers and return from interrupt
sysexwait:  lda #$5a            ; Show MIDI indicator in upper left
            sta SCREEN          ; ,,
            jsr MIDIIN          ; MIDI byte is in A
            tay                 ; Flash color for MIDI input
            dey                 ; ,, Decrementing by 1 here so that MIDI note
            sty COLOR           ; ,,   off of 0 doesn't erase the indicator
            cmp #ST_SYSEX       ; If sysex, 
            bne sy_catch        ;   ,,
            ldy TGTLIB_IX       ; Get target library index
            ldx #1              ;   set sysex listen flag
            stx LISTEN          ;   ,,
            ldx LibraryL,y      ; Set library memory from index
            stx SYIN            ; ,,
            ldx LibraryH,y      ; ,,
            stx SYIN+1          ; ,,
            ldx #0              ; Initialize library location index
            stx SYIN_IX         ; ,,
sy_catch:   ldx LISTEN          ; If sysex listen flag is on, store the byte to
            beq r_isr           ;   specified memory
sy_store:   ldy SYIN_IX         ; Get the index and store the byte there
            sta (SYIN),y        ; ,,
            cmp #ST_ENDSYSEX    ; If the sysex has ended, perform end
            beq sydone          ; ,,
            inc SYIN_IX         ; Increment storage index. If it exceeds 255,
            bne r_isr           ;   end the sysex, for a likely error status
sydone:     ldy #0              ; Set listen flag off
            sta LISTEN          ; ,,
            iny                 ; Set ready flag on
            sta READY           ; ,,
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
            rts                  ; ,,
           
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
Retrigger:  cmp #5
            bne r_ch_4
            lda #<HIR
            ldy #>HIR
            jmp WriteText
r_ch_4:     cmp #4
            bne r_ch_3
            lda #<HI
            ldy #>HI
            jmp WriteText
r_ch_3:     cmp #3
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
            
; Draw Program Line   
PrgLine:    lda DRAW_IX         ; Where we are on the page
            clc                 ; Add the library view's offset
            adc VIEW_START      ; ,,
            sec                 ; Subtract the page's parameter offset
            sbc TopParamIX+4    ; ,,
            sta VIEW_IX         ; Store in view index
            jsr Validate        ; Is it a valid program?
            beq pl_ok           ; ,,
            jsr NewLib
pl_ok:      ldy VIEW_IX         ; Add the two-digit program number first
            iny                 ;   +1 because it's 1-indexed
            jsr TwoDigNum       ;   ,,
            ldy #1              ; Put those fields on the display
            sta (FIELD),y       ; ,,
            dey                 ; ,,
            txa                 ; ,,
            sta (FIELD),y       ; ,,
            ldy VIEW_IX         ; Get the Prophet 5 program number and
            jsr PrgLoc          ;   add that to the display line
            ldy #3              ;   ,,
            lda PRGLOC          ;   ,,
            sta (FIELD),y       ;   ,,
            iny                 ;   ,,
            lda PRGLOC+1        ;   ,,
            sta (FIELD),y       ;   ,,
            iny                 ;   ,,
            lda PRGLOC+2        ;   ,,
            sta (FIELD),y       ;   ,,
            lda PTR
            sta P_START
            sta P_END
            lda PTR+1
            sta P_START+1
            sta P_END+1
            lda #$4e
            clc
            adc P_START
            sta P_START
            bcc pl_nc1 
            inc P_START+1
pl_nc1:     lda #$67
            clc
            adc P_END
            sta P_END
            bcc pl_nc2
            inc P_END+1
pl_nc2:     lda #<PROGNAME
            sta P_RESULT
            lda #>PROGNAME+1
            sta P_RESULT+1
            jsr Unpack
            ldx #2
            ldy #7
-loop:      lda PROGNAME,x
            beq prli_r
            jsr PETtoScr
            sta (FIELD),y
            inx 
            iny
            cpx #15
            bne loop
prli_r:     rts

; Show Hex
; For selected program voice sysex
ShowHex:    ldy CURLIB_IX       ; Pack voice to sysex are before showing
            jsr PackLib         ; ,,
            ldy CURLIB_IX       ; Set library pointer
            jsr SetLibPtr       ; ,,
            ldy #0
            ldx #10             ; X counts values to form a line
-loop:      lda (PTR),y
            stx ANYWHERE        ; Stash X against PutHex
            jsr PutHex          ; Put hex at the FIELD screen location
            ldx ANYWHERE        ; Put X back
            dex
            bne same_line       ; Same line until 10 values have been shown
            jsr IncFIELD        ; Increment FIELD two times time for new line
            jsr IncFIELD        ; ,,
sh_nc1:     ldx #10             ; Reset line counter to 10 values
same_line:  iny
            cpy #$9f 
            bne loop
            rts 

; Show Tempo in BPM        
BPM:        sta ANYWHERE
            lda #<(FPS * 30)    ; x15 because BPM is based on 8th notes
            sta PTRD            ; So PTRD will be 3600 or something like that
            lda #>(FPS * 30)    ; ,,
            sta PTRD+1          ; ,,
            ldy #0              ; This is the quotient, AKA BPM
-loop:      lda PTRD            ; Subtract the tempo (in IRQ triggers)
            sec                 ; ,,
            sbc ANYWHERE        ; ,,
            sta PTRD            ; ,,
            bcs bpm_c           ; ,,
            dec PTRD+1          ; ,,
            bmi bpm_found       ; BPM is found when this goes negative
bpm_c:      iny                 ; Increment the quotient
            bne loop
bpm_found:  tya                 ; All right, now convert the remainder into 
            ldx #$ff            ; a three digit number. Find the 100s place
-loop:      inx                 ; X will be the 100s place digit
            sec                 ; 
            sbc #100            ; ,,
            bcs loop            ; ,,
            pha
            cpx #0
            bne no_shift        ; If hundreds place is 0, field FIELD to left
            dec FIELD           ; ,,
            bne from_shift      ; ,,
            dec FIELD+1         ; ,,
            jmp from_shift      ; ,,
no_shift:   txa                 ; Put 100s place in A
            ora #$30            ; Convert it to a screen code numeral
            ldy #0              ; And store it on the screen
            sta (FIELD),y       ; ,,
from_shift: pla                 ; Put the two-digit remainder back in A
            clc                 ; ,,
            adc #100            ; ,,
            tay                 ; Convert it to a two digit number in X/A
            jsr TwoDigNum       ; ,,
            ldy #2              ; Place the ones digit on screen
            sta (FIELD),y       ; ,,
            dey                 ; Place the tens digit on screen
            txa                 ; ,,
            sta (FIELD),y       ; ,,
            iny                 ; Add a space to account for possible shift
            iny                 ; ,, (moving from tens digit, so two INYs)
            lda #" "            ; ,,
            sta (FIELD),y       ; ,,
            rts
            
; Show QComp
; And also set it, because it has this weird property in which it uses the
; high nybble for its value
QComp:      lsr                 ; If incremented, there will be a value like $81
            php                 ;   ,,
            lsr                 ;   and if decremented, like $7f. This operation
            lsr                 ;   captures the high nybble in A, and the
            lsr                 ;   direction in Carry, with Carry Set=decrement
            bcs q_dec           ;   Skip any adjustment of high nybble value
            plp                 ; Get carry after first LSR
            adc #0              ; ,,
            jmp q_disp          ; Display the Q Comp parameter value
q_dec:      plp                 ; Discard first processor status
q_disp:     clc                 ; Add one for display (value is 1-indexed)
            adc #1              ; ,,
            ora #$30            ; Convert to screen code and write to the field
            sbc #0              ; Un-1-index for storage
            ldy #0              ; ,,
            sta (FIELD),y       ; ,,
            asl                 ; Shift to high nybble to become the ACTUAL 
            asl                 ;   stored value. This shifts away the $30
            asl                 ;   of the screen code 
            asl                 ;   ,,
            ldy FIELD_IX        ; Store the weird value in the program buffer
            ldx FNRPN,y         ; Get the NRPN index
            sta CURPRG,x        ; Store the value
            rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Status messages
Failed:     .asc "    FAILED",0
Received:   .asc "  RECEIVED",0
Sent:       .asc "   SENT OK",0
NotEmpty:   .asc " NOT EMPTY",0
Welcome:    .asc "H FOR HELP",0
Generated:  .asc " GENERATED",0
ClrStatus:  .asc "          ",0
Copied:     .asc "    COPIED",0
Saving:     .asc "    SAVING",0
Loading:    .asc "   LOADING",0
Success:    .asc "   SUCCESS",0
Undone:     .asc "   UNDO 00",0
StatusL:    .byte <Failed,<Received,<Sent,<NotEmpty,<Welcome,<Generated
            .byte <ClrStatus,<Copied,<Saving,<Loading,<Success,<Undone
StatusH:    .byte >Failed,>Received,>Sent,>NotEmpty,>Welcome,>Generated
            .byte >ClrStatus,>Copied,>Saving,>Loading,>Success,>Undone

; MIDI Messages and Headers
EditBuffer: .byte $f0, $01, $32, $03, $ff
PrgDump:    .byte $f0, $01, $32, $02, $ff
PrgRequest: .byte $f0, $01, $32, $05, $05, $01, $ff

; Value Bar Partials
BarPartial: .byte $e7, $ea, $f6, $61, $75, $74, $65, $20

; Library Divisions
; Start entry for each library view page
LibDiv:     .byte 0,16,32,48

; Mutable Parameters
; NRPN numbers
Mutable:    .byte 2,8,9,14,15,17,18,21,26,32,33,37,40,43,44,45,46,47,48,49,50

; Key command subtroutine addresses
KeyCode:    .byte INCR,DECR,F1,F3,F5,F7,PREV,NEXT,EDIT
            .byte PREVLIB,NEXTLIB,OPENSETUP,OPENHELP,GENERATE,SETPRG
            .byte VOICESEND,CLEAR,COPY,RUN,REST,BACKSP,DSAVE,DLOAD
            .byte PRGREQ,UNDO,HEX,0
CommandL:   .byte <IncValue-1,<DecValue-1,<PageSel-1,<PageSel-1
            .byte <PageSel-1,<PageSel-1,<PrevField-1,<NextField-1,
            .byte <EditName-1,<PrevLib-1,<NextLib-1
            .byte <GoSetup-1,<GoHelp-1,<Generate-1,<SetPrg-1,<VoiceSend-1
            .byte <Erase-1,<CopyLib-1,<Sequencer-1,<AddRest-1,<DelNote-1
            .byte <GoSave-1,<GoLoad-1,<Request-1,<Undo-1,<GoHex-1
CommandH:   .byte >IncValue-1,>DecValue-1,>PageSel-1,>PageSel-1
            .byte >PageSel-1,>PageSel-1,>PrevField-1,>NextField-1,
            .byte >EditName-1,>PrevLib-1,>NextLib-1
            .byte >GoSetup-1,>GoHelp-1,>Generate-1,>SetPrg-1,>VoiceSend-1
            .byte >Erase-1,>CopyLib-1,>Sequencer-1,>AddRest+1,>DelNote-1
            .byte >GoSave-1,>GoLoad-1,>Request-1,>Undo-1,>GoHex-1

; Field type subroutine addresses
; 0=Value Bar, 1=Program, 2=Switch, 3=Tracking, 4=Detune, 5=Wheel, 6=Filter
; 7=Name, 8=Unison Voice Count, 9=Retrigger, 10=Frequency
; 11=MIDI Ch,12=Device#, 13=SixtyFour, 14=No Field, 15=Mutations, 16=Hex
; 17=Tempo, 18=Q Comp
TSubL:      .byte <ValBar-1,<PrgLine-1,<Switch-1,<Track-1
            .byte <Num-1,<Num-1,<FiltRev-1,<Name-1,<Num-1,<Retrigger-1,<Freq-1
            .byte <Num-1,<Num-1,<Num-1,<Blank-1,<Num-1,<ShowHex-1,<BPM-1
            .byte <QComp-1
TSubH:      .byte >ValBar-1,>PrgLine-1,>Switch-1,>Track-1
            .byte >Num-1,>Num-1,>FiltRev-1,>Name-1,>Num-1,>Retrigger-1,>Freq-1
            .byte >Num-1,>Num-1,>Num-1,>Blank-1,>Num-1,>ShowHex-1,>BPM-1
            .byte >QComp-1
TRangeL:    .byte 0,  0,  0,0,0, 0,0,48, 0, 0,0 , 1, 8, 1,0, 0,0,16,0
TRangeH:    .byte 127,0,  1,2,7,11,1,90,10, 5,96,16,11,64,0,10,0,80,112

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
HI:         .asc "HI ",0
HIR:        .asc "HIR",0

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
EditL:      .byte <Edit0, <Edit1, <Edit2, <Edit3, <View, <HexView, <Setup, <Help
EditH:      .byte >Edit0, >Edit1, >Edit2, >Edit3, >View, >HexView, >Setup, >Help
TopParamIX: .byte 0,      17,     31,     47,     63,    77,       78,     86

; Field data
; Field page number (0-3)
FPage:      .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3
            .byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
            .byte 5
            .byte 6,6,6,6,6,6,6,6
            .byte 7
LFIELD:     .byte $80 ; Delimiter, and LFIELD - FPage = field count

; Field row
FRow:       .byte 0,3,3,4,5,6,9,9,9,10,11,12,13,14,17,18,19
            .byte 1,2,3,4,5,6,8,9,10,11,14,15,16,17
            .byte 1,2,3,4,5,8,9,10,10,10,13,14,15,16,17,18
            .byte 1,2,3,4,7,8,9,10,11,12,13,14,15,16
            .byte 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
            .byte 3
            .byte 5,6,7,10,11,14,15,16
            .byte 0

; Field column
FCol:       .byte 1,3,8,14,14,14,3,8,12,14,14,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14,14 
            .byte 14,14,14,14,14,14,14,3,8,12,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14,14
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 1
            .byte 14,14,14,14,14,14,14,14
            .byte 1

; Field type
FType:      .byte F_NAME,F_SWITCH,F_SWITCH,F_FREQ,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_FREQ,F_VALUE,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_VALUE,F_VALUE,F_VALUE
            
            .byte F_VALUE,F_VALUE,F_VALUE,F_TRACK,F_FILTER,F_QCOMP
            .byte F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE
            .byte F_VALUE
            
            .byte F_VALUE,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_VALUE
            .byte F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_SWITCH
            
            .byte F_SWITCH,F_RETRIG,F_COUNT,F_DETUNE,F_VALUE,F_VALUE
            .byte F_WHEEL,F_SWITCH,F_SWITCH,F_VALUE
            .byte F_SWITCH,F_SWITCH,F_VALUE,F_SWITCH
            
            .byte F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG
            .byte F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG
            
            .byte F_HEX

            .byte F_MIDICH,F_SWITCH,F_DEVICE,F_64
TEMPO_FLD:  .byte F_TEMPO,F_64,F_64,F_MUTATIONS
            
            .byte F_NONE
            
; Field NRPN number
FNRPN:      .byte 88,3,4,0,8,10,5,6,7,1,2,9,11,12,14,15,16
            .byte 17,18,40,19,20,85,43,45,47,49,44,46,48,50
            .byte 32,33,34,35,36,22,21,23,24,25,26,27,28,29,30,31
            .byte 52,87,53,54,13,37,86,41,42,97,38,39,98,51
            ; These are not really NRPN numbers, but use the CURPRG storage
            ; for menu settings
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Library View
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; ,,
            .byte $ff
            .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
            .byte $ff

; Edit Page Fields
Edit0:      .asc 30,CR,CR
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
                      
Edit1:      .asc 30,CR,"FILTER",CR
            .asc RT,"CUTOFF",CR
            .asc RT,"RESONANCE",CR
            .asc RT,"ENV AMOUNT",CR
            .asc RT,"KEYBOARD",CR
            .asc RT,"REV",CR
            .asc RT,"Q COMP",CR
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
            
Edit2:      .asc 30,CR,"POLY-MOD",CR
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
                        
Edit3:      .asc 30,CR,"UNISON",CR
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
            
Setup:      .asc 30,CR,"   ED FOR PROPHET 5",CR
            .asc "   ",TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc "  2023 JASON JUSTIAN",CR,CR
            .asc "SETTINGS",CR
            .asc RT,"MIDI CHANNEL",CR
            .asc RT,"NRPN SEND",CR
            .asc RT,"DISK DEVICE #",CR
            .asc CR,"SEQUENCER",CR
            .asc RT,"STEPS",CR
            .asc RT,"TEMPO        ",RT,RT,RT," BPM",CR
            .asc CR,"GENERATION",CR
            .asc RT,"SEED",CR
            .asc RT,"SEED",CR
            .asc RT,"MUTATIONS"
            .asc 00
            
Help:       .asc CR
            .asc 5," SPACE",30," SETUP PAGE",CR
            .asc 5," F1-F7",30," EDIT PAGE",CR
            .asc 5," ",RVON,"C=",RVOF,"FN ",30," LIBRARY VIEW",CR
            .asc 5," CRSR ",30," PARAMETER",CR
            .asc 5," < >  ",30," EDIT VALUE",CR
            .asc 5," ",RVON,"C=",RVOF,"Z  ",30," UNDO",CR
            .asc 5," - +  ",30," LIBRARY SELECT",CR
            .asc 5," ",ARROW,"....",30," SEND VOICE(S)",CR
            .asc 5," Q....",30," REQUEST VOICE",CR
            .asc 5," X....",30," HEX VIEW",CR
            .asc 5," G....",30," GENERATE PROG",CR
            .asc 5," P....",30," SET PROG #",CR
            .asc 5," CLR  ",30," ERASE PROG",CR
            .asc 5," C....",30," COPY PROG",CR
            .asc 5," L....",30," DISK LOAD",CR
            .asc 5," S....",30," DISK SAVE",CR
            .asc 5," RUN  ",30," PLAY/STOP",CR
            .asc 5," ",RVON,"C=",RVOF,"RUN",30," RECORD",CR,CR
            .asc 158," WWW.BEIGEMAZE.COM/ED",30
            .asc 00
            
View:       .asc 30,CR,"     LIBRARY VIEW",CR
            .asc "     ",TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc "  # PRG NAME",CR
            .asc 00
            
HexView:    .asc 30,CR,"       HEX VIEW",CR
            .asc "       ",TL,TL,TL,TL,TL,TL,TL,TL,CR
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
ReqLabel:   .asc 5,"REQUEST #",30,0
SaveLabel:  .asc 5,"DISK SAVE",30,0
LoadLabel:  .asc 5,"DISK LOAD",30,0
SendMenu:   .asc 5,"SEND VOICE",CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"P",RVOF,"ROG"," ",RVON,"E",RVOF,"DIT",CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"B",RVOF,"ANK"," ",RVON,"G",RVOF,"ROUP"
            .asc 30,0
SendMenu2:  .asc 5,"SEND VOICE",CR,CR
            .asc RT,RT,RT,RT,RT,RT,RVON,"E",RVOF,"DIT BUFF"
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
