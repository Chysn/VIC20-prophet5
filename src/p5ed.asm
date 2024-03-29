;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                               Ed for Prophet-5
;                           (c) 2023, Jason Justian
;                  
; Assembled with XA
; Version 1.0 December 23, 2023
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2023, Jason Justian
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARTRIDGE LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* = $a000
Vectors:    .word Start         ; Start
            .word NMISR         ; NMI Address
            .byte $41,$30,$c3,$c2,$cd  ; Uncomment for production
            ;.byte $ff,$ff,$ff,$ff,$ff  ; Uncomment for development

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
UNDOS       = 100               ; Number of undo levels

; Application Memory
; In addition, zero page usage by
; MIDI KERNAL uses $9b - $9f
; SEQ PACKING uses $f9 - $ff and $60 - $6f
FIELD       = $00               ; Pointer to field memory location (2 bytes)
SYIN        = $02               ; Sysex In pointer (2 bytes)
SYIN_IX     = $04               ; Sysex In position index
PTR         = $05               ; Library pointer (2 bytes)
PTRD        = $07               ; Destination pointer (2 bytes)
FIELD_IX    = $0a               ; Current field index
VIEW_START  = $0b               ; Voice index for current Library View page
COMMODORE   = $0c               ; Commodore key flag (merge, swap, etc.)
PAGE        = $0e               ; Current page number
CVOICE_IX   = $0f               ; Current voice index
REPEAT      = $10               ; Repeat speed
IX          = $12               ; General use index
TVOICE_IX   = $17               ; Target library index
DISKLIB_IX  = $18               ; Disk library index
P_RAND      = $19               ; Random number seed (2 bytes)
S_GROUP     = $1b               ; Group search
S_BANK      = $1c               ; Bank search
DUMPTYPE    = $1d               ; Bit 7 set = Group, clear = Bank
DEST10      = $1e               ; Tens digit of copy destination
DEST1       = $1f               ; Ones digit of copy destination
NRPN_NUM    = $20               ; NRPN for NRPN MIDI messages
BREAK       = $21               ; STOP has been pressed during Load
LAST_NOTE   = $23               ; Last note played
RANDOM      = $24               ; Random number for mutation
DRAW_IX     = $25               ; Drawn field index
VIEW_IX     = $26               ; Field index in Library View
HALF_TEMPO  = $27               ; Time left before note off
UNDO_LEV    = $28               ; Current undo level
LAST_FIX    = $29               ; Last field, used for keeping track of Undo
STRIPE      = $2a               ; Mod 2 state for reverse ($80 when reversed)
LAST_LIB_IX = $39               ; Last index in Library View (7 bytes)
LISTEN      = $41               ; Sysex listen flag
READY       = $42               ; Sysex ready flag
ANYWHERE    = $43               ; Temporary iterator
LAST_VCE    = $44               ; Last voice edited, used for undo

; Application settings
MIDI_CH     = CVOICE+$a0        ; MIDI channel
NRPN_TX     = CVOICE+$a1        ; NRPN transmit toggle
PRGCH_TX    = CVOICE+$a2        ; Program change transmit toggle
DEVICE_NUM  = CVOICE+$a3        ; Storage Device number
SEED1_PRG   = CVOICE+$a4        ; Generator Seed 1
SEED2_PRG   = CVOICE+$a5        ; Generator Seed 2
MUTATE      = CVOICE+$a6        ; Mutate flag
VELOCITY    = CVOICE+$a7        ; Playback velocity
MIDINOTE    = CVOICE+$a8        ; Assigned MIDI notes

; Application Data Storage
UNDO_FIX    = $3c00             ; Field Index for undo level (100 levels)
UNDO_VAL    = UNDO_FIX+UNDOS    ; Values for undo level
UNDO_VCE    = UNDO_VAL+UNDOS    ; Voice numbers for undo level
MARKED      = $02b0             ; Save markers (64 bytes)
TEMPNAME    = $033c             ; Name storage. Filename, voice name (20 bytes)
TEMPBUFF    = $1200             ; Outgoing sysex stage (256 bytes)
SEED1       = $1200             ; Seed 1 program for generator
SEED2       = $1280             ; Seed 2 program for generator
CVOICE      = $1300             ; Current voice indexed buffer (176 bytes)
LIBRARY     = $1400             ; Storage for 64 voices (160x64=10240 bytes)
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
SM_GEN      = 5                 ; A voice has been generated
SM_BLANK    = 6                 ; (Clear status)
SM_COPIED   = 7                 ; A voice has been copied
SM_SAVING   = 8                 ; Save is in progress
SM_LOADING  = 9                 ; Load is in progress
SM_OK       = 10                ; The operation was successful
SM_UNDONE   = 11                ; Undo operation completed
SM_ERASED   = 12                ; A voice has been erased
SM_SWAPPED  = 13                ; Two voices are swapped
SM_NOGROUP  = 14                ; Group is not set

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
CURSOR      = $5a               ; Cursor screen code PETSCII
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

; Display Constants
SCREEN      = $1000             ; Screen character memory (expanded)
COLOR       = $9400             ; Screen color memory (expanded)
PARCOL      = 3                 ; Parameter color (cyan)
SELCOL      = 7                 ; Selected field color (yellow)
STACOL      = 2                 ; Status line color (red)
LIBCOL      = 5                 ; Library display color (green)
STATUSDISP  = SCREEN+484        ; Status line starting location
WINDOW_ED   = SCREEN+247        ; Window editor location
PROGRESSBAR = SCREEN+205        ; Progress bar location

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
NEXTVCE     = 5                 ; +
PREVVCE     = 61                ; -
OPENSETUP   = 32                ; Space
CANCEL      = 24                ; STOP
OPENHELP    = 43                ; H
GENERATE    = 19                ; G
SETPRG      = 13                ; P
CLEAR       = 62                ; CLR
VOICESEND   = 27                ; V
COPY        = 34                ; C
REST        = 10                ; R
DSAVE       = 41                ; S 
DLOAD       = 21                ; L
PRGREQ      = 48                ; Q
UNDO        = 33                ; Z
HEX         = 26                ; X
MARK        = 14                ; * (asterisk)

; Field Types
F_VALUE     = 0                 ; Value field 0-120
F_PRG       = 1                 ; Library view program information
F_SWITCH    = 2                 ; Switch 0-1
F_TRACK     = 3                 ; Keyboard tracking (OFF, HALF, FULL)
F_DETUNE    = 4                 ; Detune 0-7
F_WHEEL     = 5                 ; Wheel Range 0-12
F_FILTER    = 6                 ; Filter type (1/2, 3)
F_NAME      = 7                 ; Program name
F_COUNT     = 8                 ; Voice count 0-9
F_RETRIG    = 9                 ; Unison retrigger (LO, LOR, LAS, LAR)
F_FREQ      = 10                ; Frequency (C0 ~ C4)
F_MIDICH    = 11                ; MIDI Channel (1-16)
F_DEVICE    = 12                ; Storage Device Number (8-11)
F_VOICE     = 13                ; Voice number
F_NONE      = 14                ; Blank field
F_MUTATIONS = 15                ; Number of mutations
F_HEX       = 16                ; Full-page hex view
F_QCOMP     = 17                ; Q Compensation
F_BTMODE    = 18                ; Bi-Timbral Mode
F_NOTE      = 19                ; Note Number
F_PROGRAM   = 20                ; P5 Program Number
F_BANK      = 21                ; P5 Bank Number

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
MSGFLG      = $9d               ; KERNAL message mode flag
REBOOT      = $fd22

; Disk KERNAL Calls
SETLFS      = $ffba             ; Setup logical file
SETNAM      = $ffbd             ; Setup file name
SAVE        = $ffd8             ; Save
LOAD        = $ffd5             ; Load
OPEN        = $ffc0             ; Open logical file
CLOSE       = $ffc3             ; Close logical file
CLALL       = $ffe7             ; Close all files
CHKIN       = $ffc6             ; Define file as input
CHKOUT      = $ffc9             ; Define file as output
CHRIN       = $ffcf             ; Get input
CLRCHN      = $ffcc             ; Close channel
READST      = $ffb7             ; Get status

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
Start:      jsr $fd8d           ; Test RAM, initialize VIC chip
            jsr $fd52           ; Restore default I/O vectors
            jsr $fdf9           ; Initialize I/O registers
            jsr $e518           ; Initialize hardware

            ; Some hardware settings
Reset:      sei                 ; Disable interrupt; re-enabled at end of Start
            jsr CLSR            ; Clear screen
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,            
            sta VIAT            ; Start VIA timer
            lda #13             ; Screen color
            sta VIC+$0f         ; ,,
            lda #$40            ; Set aux color to purple for MIDI indicator
            sta VIC+$04         ; ,,
            
            ; Initialize the library
            ldy #LIB_TOP        ; For all 80 locations
            sty IX              ; ,,
-loop:      ldy IX              ; ,,
            lda #0              ; ,, Initialize save markers
            sta MARKED,y        ; ,, ,,
            jsr VoicePtr        ; ,,
            jsr NewVoice        ; ,, Create a new voice
lib_ok:     dec IX
            bpl loop
            
            ; Set Interupts
            lda #<IRQ           ; Set IRQ, key scan only
            sta CINV            ; ,,
            lda #>IRQ           ; ,,
            sta CINV+1          ; ,,
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
            sta TVOICE_IX       ;   * Target voice index
            sta CVOICE_IX       ;   * Current voice index
            sta MUTATE          ;   * Generator mutation enable
            sta MIDI_CH         ;   * MIDI Channel
            lda #1              ;
            sta SEED1_PRG       ;   * Generator seed 1
            sta PRGCH_TX        ;   * Program change
            lda #8              ;   * Device Number
            sta DEVICE_NUM      ;     ,,
            lda #2              ;   * Generator seed 2
            sta SEED2_PRG       ;     ,,
            lda #100            ;   * Velocity
            sta VELOCITY        ;     ,,
                                    
            ; Initialize scale and last library indices for each page
            ldy #7              ; Eight pages, eight notes. Coincidence??
-loop:      lda DefScale,y      ; ,, Copy the scale table
            sta MIDINOTE,y      ; ,, ,,
            lda TopParamIX,y    ; ,, Copy the last library index
            sta LAST_LIB_IX,y   ; ,, ,,
            dey                 ; ,,
            bpl loop            ; ,,
            inc LAST_LIB_IX     ;   ,, Page 0 is +1 because of the name field
                                    
            ; Initialize user interface
            ldy CVOICE_IX       ; Select first voice
            jsr SelVoice        ; ,,
            ldx #SM_WELCOME     ; Show welcome message in status bar
            jsr Status          ; ,,
            cli                 ; Start Interrupt
            ; Fall through to MainSwitch

; Switch page and Main Loop
; A shortcut for a common pattern, redraw the page, then
; go back to main loop.
MainSwitch: jsr SwitchPage      ; Generate the edit page
            ; Fall through to MainLoop

; Main Program Loop
; Wait for a key, then act on valid commands            
MainLoop:   clc                 ; Clear Commodore key flag
            ror COMMODORE       ; ,,
            lda #$40            ; Debounce the key press
-debounce:  cmp KEY             ; ,,
            bne debounce        ; ,,
waitkey:    ldx READY           ; If Sysex is ready, handle it
            bne SysexReady      ; ,,
            lda KEY             ; Get key press
            cmp #$40            ; If no key down, wait
            bne keydown         ; ,,
            ldx #$30            ; Reset key repeat rate
            stx REPEAT          ; ,,
            bne waitkey
keydown:    tay                 ; Preserve key pressed
            ldx #0              ; Look through Key Code table for a valid
-loop:      lda KeyCode,x       ;   command key press
            beq PlayScale       ;   ,, 0 delimits command list
            cmp KEY
            beq cmd_addr
            inx
            bne loop
cmd_addr:   lda CommandH,x      ; Set dispatch address on the stack
            pha                 ; ,,
            lda CommandL,x      ; ,,
            pha                 ; ,,
            tya                 ; Put key pressed back in A
            ldy SHIFT           ; Check for Commodore key
            cpy #2              ; ,,
            bne dispatch        ; ,,
            sec                 ; Perform swap if Commodore is pressed
            ror COMMODORE       ; ,,            
dispatch:   rts                 ; Dispatch command with key in A

; Play Custom Scale
PlayScale:  sty ANYWHERE        ; Store the pressed key code
            jsr unshift         ; Not a command, check for 1-8
            cmp #"1"            ; ,,
            bcc MainLoop        ; ,,
            cmp #"8"+1          ; ,,
            bcs MainLoop        ; ,,
            eor #$30            ; Convert numeral to number 1-8
            tay                 ; Use it as index
            lda MIDINOTE-1,y    ; Get note from MIDI table
            tax                 ; Set X for note on message
            stx LAST_NOTE       ; ,,
            ldy VELOCITY        ; Set Y for velocity
            lda MIDI_CH         ; Set MIDI channel
            jsr SETCH           ; ,,
            jsr NOTEON          ; Send note on message
            ldy ANYWHERE        ; Wait until this key is no longer down
-loop:      cpy KEY             ; ,,
            beq loop            ; ,,
            ldx LAST_NOTE       ; Note number
            ldy #0              ; Release velocity 0
            jsr NOTEOFF         ; Turn note off
            ldy KEY             ; Jump back to check scale, to allow
            jmp PlayScale       ;   legato playing

; Handle Incoming SysEx
SysexReady: lda #0              ; Clear the sysex ready flag
            sta READY           ; ,,
            ldy CVOICE_IX       ; Is this a valid Prophet-5 voice dump?
            jsr ClrUndo         ; .. (Clear undo whether good or not)
            jsr Validate        ; ,, (Y is preserved by ClrUndo)
            bne sysex_fail      ; ,,
            ldx #SM_RECV        ; Show received success status
            jsr Status          ; ,,
            jsr UnpBuff         ; Unpack to edit buffer
            jsr PopFields       ; And populate field for current page
            jmp MainLoop
sysex_fail: jsr NewVoice        ; Generate new voice to clear bad sysex
            ldx #SM_FAIL        ; Show fail status message
            jsr Status          ;   ,,
            jmp MainLoop        ;   ,,
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; Previous Voice
PrevVoice:  jsr PackVoice
            lda #1              ; Default value to substract, one
            sta IX              ; ,,
            ldy SHIFT           ; If shift is held, decrement by 10 instead
            beq pl_def          ; ,,
            lda #10             ; ,,
            sta IX              ; ,,
pl_def:     lda CVOICE_IX       ; Subtract the specified number from the
            sec                 ;   library number
            sbc IX              ;   ,,
            sta CVOICE_IX       ;   ,,
            bcs switchlib       ; If it's below 0, set it back to 0
            lda #0              ; ,,
            sta CVOICE_IX       ; ,,
switchlib:  jsr ClrCursor
            ldy CVOICE_IX       ; Set the target voice to be the newly-
            sty TVOICE_IX       ;   selected voice, so new sysex starts here
            jsr SelVoice        ; Select library and show the fields for the
            jsr PopFields       ;   current page
            ldx #SM_BLANK       ; Blank the status when a new voice is chosen
            jsr Status          ; ,,
chlib_r:    jmp MainLoop

; Move Cursor to Previous Field            
PrevField:  lda PAGE            ; Is this the Library view?
            cmp #4              ; ,,
            bne pr_nlv          ; ,,
            lda FIELD_IX        ; If so, is this the first field number?
            cmp #FLIBSTART-FPage; ,,
            bne pr_nlv          ; ,, If below last field, proceed as normal
            lda VIEW_START      ; Can the voices advance any further?
            beq pr_mv           ; ,, If not, stay put
            dec VIEW_START      ; ,, If so, decrement start of view index
pr_mv:      jmp ScrVoice        ; Change voice when scrolling
pr_nlv:     ldy FIELD_IX        ; If the current index is 0, stay here
            beq pf_r            ; ,,
            dey
            lda FPage,y
            cmp PAGE            ; If the field change would cross pages,
            bne pf_r            ;   stay here
            jsr ClrCursor
            ldy FIELD_IX
            dey
ch_f:       sty FIELD_IX        ; Endpoint for changing the field
            beq edit_name       ; If this is the name field, edit it
            jsr DrawCursor
            lda PAGE            ; Are we on the Library View?
            cmp #4              ; ,,
            bne lvf_r           ; If so, change the current program
ScrVoice:   jsr PackVoice       ; Pack changes on the current voice to mem
            lda FIELD_IX        ; Field index
            sta LAST_LIB_IX+4   ;   ,, (Preserve last library index)
            clc                 ;   ,,
            adc VIEW_START      ;   plus start-of-view
            sec                 ;   ,,
            sbc TopParamIX+4    ;   minus first page parameter...
            sta CVOICE_IX       ; ...Equals the new current program index
            sta TVOICE_IX       ; ...and the target program index
            tay                 ; Select this voice
            jsr SelVoice        ; ,,
            ldy CVOICE_IX       ; Show program number
            jsr PopFields       ; ,,
lvf_r:      ldy PAGE            ; Keep track of the index for the current
            lda FIELD_IX        ;   page
            sta LAST_LIB_IX,y   ;   ,,
pf_r:       jmp MainLoop
edit_name:  jmp EditName

; Next Voice
NextVoice:  jsr PackVoice
            lda #1              ; Default value to add, one
            sta IX              ; ,,
            ldy SHIFT           ; If shift is held, increment by 10 instead
            beq nl_def          ; ,,
            lda #10             ; ,,
            sta IX              ; ,,
nl_def:     lda CVOICE_IX       ; Add the specified number to the
            clc                 ;   library number
            adc IX              ;   ,,
            cmp #LIB_TOP        ; If it overflows, set back to 80
            bcc nl_set          ; ,,
            lda #LIB_TOP-1      ; ,,
nl_set:     sta CVOICE_IX       ; Store the new index
            jmp switchlib       ; Swith library from PrevVoice
                      
; Move Cursor to Next Field
NextField:  lda PAGE            ; Is this the Library view?
            cmp #4              ; ,,
            bne nx_nlv          ; ,,
            lda FIELD_IX        ; If so, is this the last field number?
            cmp #FLIBEND-FPage-1; ,,
            bne nx_nlv          ; ,, If above last field, proceed as normal
            lda VIEW_START      ; Can the voices advance any further?
            cmp #48             ; ,,
            beq nx_mv           ; ,, If not, stay put
            inc VIEW_START      ; ,, If so, increment start of view index
nx_mv:      jmp ScrVoice        ; Change voice when scrolling
nx_nlv:     ldy FIELD_IX
            cpy #LFIELD-FPage
            beq pf_r
            iny
            lda FPage,y
            cmp PAGE
            bne pf_r
            jsr ClrCursor
            ldy FIELD_IX
            iny
            jmp ch_f            ; Go to change field code in PrevField
                                            
; Select Page           
PageSel:    sec
            sbc #39             ; Determines which F key was pressed
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            ldy SHIFT           ; If C= or SHIFT is held down, jump to
            bne LibView         ;   Library selection
            cmp PAGE            ; If already on this page, don't redraw
            beq pagesel_r       ; ,,
            sta PAGE            ; Set the page and draw it
            jsr SwitchPage      ; ,,
pagesel_r:  jmp MainLoop
            
; Select Library Section
LibView:    tay                 ; Get library division for this key
            lda LibDiv,y        ; Set view starting point
            sta VIEW_START      ; ,,
            jsr PackVoice       ; ,,
            lda #4              ; Set page number
            sta PAGE            ; ,,
            jmp MainSwitch

; Increment Field by 1
IncValue:   jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeH,y
            bcs id_r            ; Already at maximum, so do nothing
            jsr SetUndo         ; Pre-change (Undo)
            inc CVOICE,x
nrpn_msg:   lda PAGE            ; If < and > are pressed on the
            cmp #4              ;   Library View, do nothing
            beq topspeed        ;   ,,
            jsr NRPNpost        ; Send NRPN message, handle Undo
            ldy FIELD_IX
            jsr DrawField
            ldy FIELD_IX
            lda FType,y         ; Some field types should not debounce the
            cmp #F_VALUE        ;   key, so check those types here
            beq no_deb          ;   ,,
            cmp #F_FREQ         ;   ,,
            beq no_deb          ;   ,,
            cmp #F_NOTE         ;   ,,
            beq no_deb          ;   ,,
            cmp #F_VOICE        ;   ,,
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
DecValue:   jsr PrepField       ; Get field value
            bcc id_r
            cmp TRangeL,y
            beq id_r            ; Already at minimum, so do nothing
            jsr SetUndo         ; Pre-change (Undo)
            dec CVOICE,x
            jmp nrpn_msg

; Single-Key Edit (RETURN)
; * Toggles Switches
; * Selects Programs in Library
; * Advances Others
Advance:    ldy FIELD_IX        ; Check the field's type for this edit     
            lda FType,y         ;   behavior.
            cmp #F_PRG          ; If it's a program, it will be selected
            beq sel_prog        ; ,,
            tya                 ; Save Y for after undo level
            pha                 ; ,,
            ldx FNRPN,y         ; Pass NRPN number to SetUndo to set undo
            jsr SetUndo         ;   ,,
            pla                 ;   ,,
            tay                 ;   ,,
            ldx FNRPN,y         ; Anything else will advance to its max
            lda FType,y         ;   and then roll back to 0
            tay                 ;   ,,
            lda CVOICE,x        ;   ,,
            cmp TRangeH,y       ;   ,,
            bcc adv_f           ;   ,,
            cpy #F_VALUE        ; If this is a value type, do not roll back      
            beq edna_r          ;   after passing the high range; do nothing
            lda TRangeL,y       ; If above high range, set to low
            sta CVOICE,x        ;   and save that
            jmp val_ch          ;   ,,
adv_f:      inc CVOICE,x        ;   ,,
val_ch:     jsr NRPNpost        ;   ,,
            ldy FIELD_IX        ;   Draw the new field value
            jsr DrawField       ;   ,,
edna_r:     jmp MainLoop        ;   ,,
sel_prog:   tya                 ; Field index
            clc                 ;   ,,
            adc VIEW_START      ;   plus start-of-view
            sec                 ;   ,,
            sbc TopParamIX+4    ;   minus first page parameter...
            sta CVOICE_IX       ; ...Equals the new current program index
            tay                 ; Select this voice
            jsr SelVoice        ; ,,
            lda #0              ; Drill down to edit page
            sta PAGE            ; ,,
            jmp MainSwitch
            
EditName:   jsr DrawCursor      ; Draw name cursor
            ldy FIELD_IX        ; Get starting screen position of Name field
            jsr FieldLoc        ; ,,
pos_cur:    jsr find_end        ; Set IX to the character after the last one
            lda #TXTCURSOR      ; Show a cursor in that place
            sta (FIELD),y       ; ,,
getkey:     jsr Keypress        ; Get key code in Y, PETSCII in A
            cpy #BACKSP         ; Has backspace been pressed?
            beq backsp          ; ,,
            cpy #NEXT           ; Has next field been selected?
            beq entername       ; ,,
            cpy #EDIT           ; Has RETURN been pressed?
            beq entername       ; ,,
            cmp #" "            ; Constrain values for character
            bcc getkey          ; ,,
            cmp #$60            ; ,,
            bcs getkey          ; ,,
            ldy IX              ; Put this character into the NRPN buffer
            cpy #20             ;   ,, (if it's less than 20)
            bcs pos_cur         ;   ,,
            sta CVOICE+65,y     ;   ,,
            jsr PETtoScr        ; Convert to screen code for display
            ldy IX              ; ,,
            sta (FIELD),y       ; ,,
            jmp pos_cur         ; ,,
backsp:     ldy IX              ; Backspace
            lda #" "            ; Clear the old cursor
            sta (FIELD),y       ; ,,
            beq getkey          ; If at the beginning already, do not backspace
            dec IX              ; Backspace by moving index back
            ldy IX              ; ,,
            lda #0              ; And adding a 0 in the NRPN buffer
            sta CVOICE+65,y     ; ,,
            beq pos_cur
entername:  jsr find_end        ; RETURN has been pressed, so remove the cursor
            lda #" "            ; ,,
            sta (FIELD),y       ; ,,
            jsr ClrCursor       ; Clear old cursor
            inc FIELD_IX        ; Advance to next field
            jsr DrawCursor      ; Replace removed field-level cursor
            jmp MainLoop        ; And go back to Main
find_end:   ldy #0              ; Starting NRPN index of Name
-loop:      lda CVOICE+65,y     ; Find the end of the current name, where the
            beq fc_r            ;   cursor should go
            iny                 ;   ,,
            cpy #20             ;   ,,
            bne loop            ;   ,,
fc_r:       sty IX              ;   ,,
            rts

; Generate Program
; From two spefcified parent programs in the library
Generate:   ldy CVOICE_IX       ; First, make sure that the current voice
            jsr Validate        ;   is either invalid, or lacks a
            bne gen_ok          ;   program number. We don't want to overwrite
            ldy #4              ;   an assigned program.
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
            dex                 ;   -1 because SEED2_PRG is 1-indexed
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
            sta CVOICE,y        ;   ,,
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
            sta CVOICE,x        ; Store it in the current program
            dec IX              ; Decrement the mutation count
            bne mutate          ; Go back for more
no_mutate:  jsr BufferSend      ; Send the edit buffer            
            ldx #SM_GEN         ; Write status message when done
            jsr Status          ; ,,
            ldy CVOICE_IX       ; Clear undo info for a generated voice
            jsr ClrUndo         ; ,,
            jsr PopFields
            jmp MainLoop
             
; Set Program Number
; for current voice                   
SetPrg:     ldy CVOICE_IX       ; Get program location to TEMPNAME 
            jsr PrgLoc          ; ,,
            jsr UnpBuff         ; ,,
            bit COMMODORE       ; If COMMODORE is held, change all groups
            bpl set_prg         ;   instead of setting one program number
            jmp SetGrp          ;   ,,
set_prg:    jsr Popup
            lda #<PrgLabel
            ldy #>PrgLabel
            jsr PrintStr
            lda TEMPNAME        ; Is the program name already set?
            cmp #"-"            ;   ,,
            beq is_unset        ;   ,, if not, start at beginning
            ldy #2              ; Set up editor with current program
-loop:      lda TEMPNAME,y      ; ,,
            sta WINDOW_ED,y     ; ,,
            dey                 ; ,,
            bpl loop            ; ,, 
            ldy #3              ; ,,
            .byte $3c           ; Skip word (SKW)
is_unset:   ldy #0              ; Cursor position in edit field        
            jsr SetPrgNum       ; Get program number from user
            bcc do_set          ;   If ok, do the set
            cpy #CANCEL         ;   If canceled, do nothing
            beq setp_r          ;   ,,
            lda IX              ;   If at beginning, mark as unset
            bne setp_r          ;   ,,
            lda #$80            ; Mark the program as unset if the input was
            sta PTRD            ;   totally empty
            lda #$00            ;   ,,
            sta PTRD+1          ;   ,,
do_set:     ldy #4              ; Get the user input from PTRD and update the
            lda PTRD            ;   actual sysex in the library with the
            sta (PTR),y         ;   new program number
            iny                 ;   ,,
            lda PTRD+1          ;   ,,
            sta (PTR),y         ;   ,,
setp_r:     jmp MainSwitch
            
; Change Group Numbers    
SetGrp:     ldy CVOICE_IX       ; Get program location to TEMPNAME 
            jsr PrgLoc          ; ,,
            lda TEMPNAME        ; Is the group set?
            cmp #"-"            ; ,,
            bne set_group       ; ,, If not, show an error
            ldx #SM_NOGROUP     ; ,,
            jsr Status          ; ,,
            jmp MainLoop
set_group:  pha
            jsr Popup
            pla
            sta SCREEN+210      ; Show original group number on screen
            lda #<GrpLabel      ; Show label
            ldy #>GrpLabel      ; ,,
            jsr PrintStr        ; ,,
grp_cur:    ldy #0              ; Set index for cursor
            sty IX              ; ,,
            lda #TXTCURSOR      ; Draw cusror
            sta WINDOW_ED,y     ; ,,
grp_key:    jsr Keypress        ; Wait for keypress
            cpy #CANCEL         ; If cancel, return
            beq setgrp_r        ; ,,
            cpy #BACKSP         ; If backspace, remove set number
            beq grp_bksp        ; 
            cpy #EDIT           ; If RETURN, submit
            beq grp_done        ; ,,
            cmp #"1"            ; If less than one
            bcc grp_key         ;   ,,
            cmp #"5"+1          ;   or greater than five
            bcs grp_key         ;   go back for another key
            ldy IX              ; Or if there's already a group number
            bne grp_key         ;   go back
            sta WINDOW_ED       ;
            lda #TXTCURSOR      ; Move the cursor
            sta WINDOW_ED+1     ; ,,
            inc IX              ; ,,
            jmp grp_key 
grp_bksp:   lda IX              ; If already at the start, cannot backspace
            beq grp_key         ; ,,
            lda #" "            ; Remove the entered character
            sta WINDOW_ED+1     ; ,,
            jmp grp_cur         ; ,,
grp_done:   lda IX              ; If no group was entered, go back for more
            beq grp_key         ; ,,
            lda WINDOW_ED       ; Get user input
            eor #$30            ; Remove the screen code and leave the number
            sec                 ; Zero-index the group number 
            sbc #1              ; ,,
            sta ANYWHERE        ; Store the new (target) group
            jsr SetCurPtr       ; From the current library,
            ldy #4              ;   get the group for which to search
            lda (PTR),y         ;   ,,
            sta S_GROUP         ;   ,,
            ldy #0              ; Go through each program, looking for the
-loop:      sty IX              ;   
            ldy IX
            jsr VoicePtr
            ldy #4              ; Group number sysex byte
            lda (PTR),y         ; ,,
            cmp S_GROUP         ; ,,
            bne gs_next         ; ,, If no match, go to the next program
            lda ANYWHERE        ; If matches, change group number in the
            sta (PTR),y         ;   sysex
gs_next:    ldy IX
            iny
            cpy #LIB_TOP
            bne loop
            ldx #SM_OK          ; Show success status
            jsr Status          ; ,,
setgrp_r:   jmp MainSwitch
            
; System Exclusive Voice Dump
; of program, bank, or group
GoSend:     ldy CVOICE_IX       ; If this is not a valid program, cannot
            jsr Validate        ;   do dump
            bne dump_r          ;   ,,
            jsr Popup           ; Put the dump selection menu 
            ldy #4              ; Does this voice have a program number?
            lda (PTR),y         ; ,,
            bpl all_opt         ; ,, If so, show all options
            lda #<SendMenu2     ; ,, If no prog number, show only edit buffer
            ldy #>SendMenu2     ; ,,   option
            jmp voice_menu
all_opt:    cmp #5              ; Is this a factory program?
            bcc all_menu        ; ,, If not just show menu
            ror COMMODORE       ; ,, If so, set flag to show TO FACTORY
all_menu:   lda #<SendMenu      ; Put options into popup window
            ldy #>SendMenu      ;   ,,
voice_menu: jsr PrintStr        ;   ,,
            bit COMMODORE       ; Factory flag?
            bpl vgetkey         ; ,,
            ldy #0              ; Then show the words "TO FACTORY" after SEND
-loop:      lda FactoryLab,y    ; ,,
            beq vgetkey         ; ,,
            sta SCREEN+203,y    ; ,,
            iny                 ; ,,
            bne loop            ; ,,
vgetkey:    jsr Keypress        ; Get pressed key
            cpy #CANCEL         ; Check for cancel key
            beq dump_r          ; ,,
            cmp #"E"            ; Dumping edit buffer?
            bne ch_prg          ; ,,
            jsr BufferSend      ; ,,
            jmp MainSwitch      ; 
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
            jmp MainSwitch

; Erase the current program      
GoErase:    jsr Popup           ; Show confirmation popup
            lda #<EraseConf     ; ,,
            ldy #>EraseConf     ; ,,
            jsr PrintStr        ; ,,
            jsr srcvce_un       ; ,,
            jsr Keypress        ; ,,
            cmp #"Y"            ; If Y was pressed erase. Anything else,
            bne erase_r         ;   return doing nothing
            jsr SetCurPtr       ; Set pointer to the voice and create new
            jsr NewVoice        ;   voice
            jsr UnpBuff         ; Unpack the new sysex into the current buffer
            ldy CVOICE_IX       ; Unmark erased voice for save
            lda #0              ; ,,
            sta MARKED,y        ; ,,
            ldx #SM_ERASED
            .byte $3c           ; Skip word (SKW)
erase_r:    ldx #SM_BLANK
            jsr Status
            jmp MainSwitch

; Copy the current voice
; to another location
GoCopy:     jsr Popup
            lda #<CopyLabel
            ldy #>CopyLabel
            bit COMMODORE
            bpl copy_lab
            lda #<SwapLabel
            ldy #>SwapLabel   
copy_lab:   jsr PrintStr
            jsr srcvce_un       ; Show the source voice         
            ldy #0              ; Set up editor for voice
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
ones:       ldx #"9"+1          ; By default, maximum is 9
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
            dey                 ; Re-zero-index the voice number
            jsr ClrUndo         ; Clear undo levels for the target voice
            jsr VoicePtr        ; Set PTR to this voice
            ldy #$9e            ; Copy the selected voice into the temporary
-loop:      lda (PTR),y         ;   buffer for swap. Yeah, this is done whether
            sta TEMPBUFF,y      ;   or not swap is actually selected.
            dey                 ;   ,,
            cpy #$ff            ;   ,,
            bne loop            ;   ,,
            lda PTR             ; Move PTR to the destination pointer for
            sta PTRD            ;   copying
            lda PTR+1           ;   ,,
            sta PTRD+1          ;   ,,
            jsr PackVoice       ; Pack program data to library prior to copy
            ldy #$9f            ; Perform the copy operation
-loop:      lda (PTR),y         ; ,,
            sta (PTRD),y        ; ,,
            dey                 ; ,,
            cpy #$05            ; ,, (leave dest program number alone)
            bne loop            ; ,,
            bit COMMODORE       ; In Swap mode, leave program numbers alone
            bpl cp_status       ; ,,
            ldx #SM_SWAPPED     ; ,,
            .byte $3c           ; ,, Skip word (SKW)
cp_status:  ldx #SM_COPIED      ; Indicate copy success
            jsr Status          ; ,,
            bit COMMODORE       ; If swap is selcted, copy the destination sysex
            bpl copy_r          ;   into the existing record
            ldy #$9e            ;   ,,
-loop:      lda TEMPBUFF,y      ;   ,,
            sta (PTR),y         ;   or not swap is actually selected.
            dey                 ;   ,,
            cpy #$05            ;   ,, (leave program numbers alone)
            bne loop            ;   ,,
            jsr UnpBuff         ; ,,
copy_r:     jmp MainSwitch

; Go to Setup, Help, or Hex View
GoHex:      lda #5
            .byte $3c           ; Skip word (SKW)
GoSetup:    lda #6
            .byte $3c           ; Skip word (SKW)
GoHelp:     lda #7
            cmp PAGE
            beq setup_r
            sta PAGE
setup_r:    jmp MainSwitch

; Disk Save
; When Commodore is held, save only the selected voices
GoSave:     jsr PackVoice
            jsr CLALL
            jsr Popup
            lda #<SaveLabel
            ldy #>SaveLabel
            bit COMMODORE       ; If the Commodore key was held, show the
            bpl prompt          ;   Save Marked popup
            lda #<SaveLabel2    ;   ,,
            ldy #>SaveLabel2    ;   ,,
prompt:     jsr PrintStr
            jsr FileName        ; Get user name input
            bcc start_save      ; If OK, start save
            jmp disk_canc       ; Cancel, so return
start_save: lda #0              ; Turn off KERNAL messages
            sta MSGFLG          ; ,,
            tya                 ; Length of name for SETNAM call
            ldx #<TEMPNAME      ; Pointer to name (low)
            ldy #>TEMPNAME      ; Pointer to name (high)
            jsr SETNAM          ; Call SETNAM            
            ldx DEVICE_NUM      ; Set up LFS. Device number
            ldy #2              ;   Secondary address
            lda #2              ;   File number
            jsr SETLFS          ;   ,,                  
            jsr OPEN            ; Open file
            bcs disk_error      ; ,,
            ldx #2              ; CHKOUT
            jsr CHKOUT          ; ,,
            ldx #SM_SAVING      ; Show "SAVING..."
            jsr Status          ; ,,
            ldy #0              ; Initialize disk save index
-next_rec:  sty DISKLIB_IX      ; ,,
            tya                 ; Show progress bar
            asl                 ; ,, Multiple progress by 2
            jsr ProgPopup       ; ,,
            ldy DISKLIB_IX      ; Get library index
            bit COMMODORE       ; Saving only selected?
            bpl ok2save         ;   If so,
            lda MARKED,y        ;   is the program marked?
            beq snext_prg       ;   If not, skip it
ok2save:    jsr Validate        ; Is the program valid?
            bne snext_prg       ; ,,
            ldy #0              ; Send sysex to file
-loop:      lda (PTR),y         ; ,,
            jsr CHROUT          ; ,,
            cmp #ST_ENDSYSEX    ; If it's the end of sysex, advance
            beq snext_prg       ;   to next program
            iny                 ; Increment the byte counter and loop
            bne loop            ; ,,
snext_prg:  ldy DISKLIB_IX      ; Increment the disk library index 
            iny                 ; ,,
            cpy #LIB_TOP        ; Has it reached the end?
            bne next_rec        ; If not, loop
save_r:     lda #2              ; Close the save
            jsr CLOSE           ; ,,
            jsr CLRCHN          ; ,,
            jmp disk_ok
            
; Disk Error Message
disk_error: lda #2
            jsr CLOSE
            jsr CLRCHN
            ldx #SM_FAIL
            jsr Status
            jmp MainSwitch

; Disk Load
GoLoad:     jsr CLALL
            jsr Popup
            lda #<LoadLabel
            ldy #>LoadLabel
            jsr PrintStr
            bit COMMODORE       ; If this is the Commodore mode,
            bpl loadlib         ;   add letters so that it reads
            lda #20             ;   LOAD TO
            sta SCREEN+203      ;   ,,
            lda #15             ;   ,,
            sta SCREEN+204      ;   ,,
loadlib:    jsr SourceVce  
            jsr FileName        ; Get name from user
            bcc start_load
            jmp disk_canc
start_load: lda #0              ; Turn off KERNAL messages
            sta MSGFLG          ; ,,
            sta BREAK           ; Clear BREAK flag
            tya                 ; Length of name for SETNAM call
            sec                 ; Subtract the ",P,W" from the filename
            sbc #4              ; ,,
            ldx #<TEMPNAME      ; Pointer to name (low)
            ldy #>TEMPNAME      ; Pointer to name (high)
            jsr SETNAM          ; Call SETNAM       
            ldx DEVICE_NUM      ; Device number
            ldy #2              ; Load to header location
            lda #2              ; File number
            jsr SETLFS          ; ,,
            jsr OPEN
            bcs disk_error      ; ,,
            ldx #2              ; CHKIN
            jsr CHKIN           ; ,,
            bcs disk_error      ; ,,
            ldx #SM_LOADING
            jsr Status       
            ldy #0              ; Initialize library pointer
            bit COMMODORE       ; ,, Was Commodore held down?
            bpl load_cur        ; ,,  If so, start at current voice
            ldy CVOICE_IX       ; ,,  ,,
load_cur:   sty DISKLIB_IX      ; ,,
-next_rec:  ldy DISKLIB_IX      ; Get next voice pointer
            jsr VoicePtr        ; ,,
            ldy #0              ; Index within current message
get_byte:   lda KEY             ; If CANCEL key is pressed, set BREAK flag
            cmp #CANCEL         ; ,,
            bne no_break        ; ,,
            sec                 ; ,,
            ror BREAK           ; ,,
no_break:   jsr READST
            bne eof
            jsr CHRIN           ; Get next byte
            cmp #ST_SYSEX       ; Is it start of sysex?
            bne ch_sysex        ; ,,
            sta TEMPBUFF,y      ; Stash it away
            ldy #1              ; ,,
            bne get_byte        ; Go back for next byte
ch_sysex:   cpy #0              ; Has sysex started?
            beq get_byte        ; If not, go back
            sta (PTR),y         ; Store in current voice
            iny                 ; Increment the message index
            cmp #ST_ENDSYSEX    ; Is this the end of the record?
            bne get_byte        ; ,,
eorec:      ldy DISKLIB_IX      ; Is the incomcing sysex message an actual 
            jsr Validate        ;   Prophet-5 voice?
            bne next_rec        ;   ,, If not, use the same DISKLIB_IX again
            ldy DISKLIB_IX      ; END OF RECORD. Get disk voice index
            lda #0              ;   ,, (Reset save marker for loaded voice)
            sta MARKED,y        ;   ,, ,,
            jsr ClrUndo         ;   ,, (Clear undo levels for this voice)
            iny                 ;   Increment index for 1-indexed display
            jsr TwoDigNum       ;   Show voice index in status area
            stx STATUSDISP+18   ;   ,,
            sta STATUSDISP+19   ;   ,,
            lda DISKLIB_IX      ; Show progress bar
            asl                 ; ,, Multiply progress by 2
            jsr ProgPopup       ; ,,
            bit BREAK           ; Is the BREAK flag set?
            bmi load_good       ; ,, If so, end the load
            inc DISKLIB_IX      ; Increment the disk library index.
            lda #LIB_TOP        ;   If it's reached the library top,
            cmp DISKLIB_IX      ;   then act as though we're EOF 
            bne next_rec        ;   otherwise go back for another record
eof:        and #$40            ; If this is a read error, show error message
            bne load_good
            jmp disk_error
load_good:  lda #2
            jsr CLOSE
            jsr CLRCHN
            jsr SetCurPtr       ; Unpack current program into the edit buffer
            jsr UnpBuff         ;   ,,
            ; Fall through to disk ok
            
            ; Shared exit points for both save and load
disk_ok:    ldx #SM_OK          ; Show success message
            jsr Status          ; ,,
disk_canc:  jmp MainSwitch      ; ,,
            
; Request Program
Request:    jsr SetCurPtr       ; Get pointer to sysex in library
            sty TVOICE_IX       ; Want the requested program to go HERE
            ldy #4              ; Cannot use the request for a voice
            lda (PTR),y         ;   that is already assigned to a program
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
            sta TEMPBUFF+4      ; ,,
            lda PTRD+1          ; Add the bank/program number to the sysex
            sta TEMPBUFF+5      ; ,,
            lda #ST_ENDSYSEX    ; Add the end-of-sysex message
            sta TEMPBUFF+6      ; ,,
            jsr SendSysex       ; Send the request message
req_r2:     jmp MainSwitch      
         
; Undo
; If undo level > 0, then get last NRPN and set it to its last value           
Undo:       bit COMMODORE       ; Undo works only with COMMODORE down
            bpl undo_r          ; ,,
            ldx #SM_UNDONE      ; Show status message, onto which the number of
            jsr Status          ;   remaining levels will be added below
            ldy UNDO_LEV        ; Start looking at this undo level
            beq undo_r          ; ,,
-loop:      lda UNDO_VCE,y      ; What voice is this level for?
            cmp CVOICE_IX       ; Is it for the current one?
            beq undo_this       ; If so, an undo level was found 
            dey                 ; If not, try the next level
            bpl loop            ; ,,
            bmi undo_r          ; Do nothing if no undo for this voice
undo_this:  cpy UNDO_LEV        ; If this is not the last entry,
            bne no_dec          ;   do not decrement, but set the voice
            dec UNDO_LEV        ;   index to $ff. If it is the last entry, DEC
no_dec:     sty IX              ; Store undo index for later display
            lda #$ff            ; Mark as $ff to indicate the level was used
            sta UNDO_VCE,y      ; ,,
            lda UNDO_FIX,y      ; Get the field index for the level
            pha                 ; ,, (store the field index)
            tax                 ; ,, (store in X)
            lda FNRPN,x         ; Get the NRPN for this field
            tax                 ;   into X
            lda UNDO_VAL,y      ; Get the value for the level
            sta CVOICE,x        ; Restore the program value
            lda #$ff            ; Reset the last field index number
            sta LAST_FIX        ; ,,
            jsr NRPNpost        ; Transmit NRPN CCs, if enabled (passing X)
            pla                 ; Draw the field. Get the field index back.
            tay                 ; ,,
            ldx FPage,y         ; Get the page number for the undone field
            sta LAST_LIB_IX,x   ; ,, (set the last index to move the cursor)
            cpx PAGE            ; If it's the same page, just populate
            beq pop_only        ;   fields
            stx PAGE            ; ,,
            jsr SwitchPage      ; ,,
pop_only:   jsr ClrCursor 
            jsr PopFields       ; ,,
            ldy #0              ; Count the number of undo levels for this voice
            ldx UNDO_LEV        ; ,,
            beq show_levs       ; ,, If no levels, show dashes
-loop:      lda UNDO_VCE,x      ; ,,
            cmp CVOICE_IX       ; ,,
            bne diff_vce        ; ,,
            iny                 ; ,, Increment undo count for this voice
diff_vce:   dex                 ; ,,
            bne loop            ; ,, (end at 0, because 0 isn't used)
show_levs:  jsr TwoDigNum       ; Show how many undo levels remain for this
            stx STATUSDISP+18   ;   voice
            sta STATUSDISP+19   ;   ,,
undo_r:     jmp MainLoop

; Mark Voice for Save
Mark:       bit COMMODORE       ; If Commodore was held down, clear out
            bmi clearmarks      ;   all the save marks
            ldy CVOICE_IX
            lda MARKED,y
            eor #$01
            sta MARKED,y
markupdate: jsr savemark        ; Takes Y as index
            lda PAGE            ; If this is the Library View, refresh the
            cmp #4              ;   screen, because the mark indicator there
            bne mark_r          ;   might change
            jsr PopFields       ;   ,,
mark_r:     jmp MainLoop
clearmarks: lda #0              ; If Commodore was held down, clear all
            ldy #LIB_TOP        ;   of the save marks
-loop:      sta MARKED,y        ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            ldy CVOICE_IX       ;   ,, Set index for savemark
            bpl markupdate      ;   ,, (unconditional because of LDY)
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERFACE SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Filename Field
; If canceled, return with carry set
; If OK, return with carry clear and call SETNAM
FileName:   lda #"."            ; Add file extension .P5
            sta WINDOW_ED+8     ; ,,
            lda #19             ; ,, (S)
            sta WINDOW_ED+9     ; ,,
            lda #25             ; ,, (Y)
            sta WINDOW_ED+10    ; ,,
            lda #24             ; ,, (X)
            sta WINDOW_ED+11    ; ,,
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
            beq fbacksp         ; ,,
            cpy #EDIT           ; Has return been pressed?
            beq fdone           ; ,,
            cmp #"@"            ; Disallow @
            beq fgetkey         ; ,,
            ldx #EOD-Disallow   ; For each disallowed character
-loop:      cmp Disallow,x      ;   If it's this, then reject it
            beq fgetkey         ;   ,,
            dex                 ;   ,,
            bpl loop            ;   ,,
            cmp #" "            ; Constrain values for character
            bcc fgetkey         ; ,,
            cmp #$60            ; ,,
            bcs fgetkey         ; ,,
char_ok:    ldy IX              ; ,,
            cpy #8              ; Limit size of filename
            bcs fgetkey         ; ,,
            sta TEMPNAME,y      ; Store PETSCII in name storage
            jsr PETtoScr        ; Convert to screen code for display
            ldy IX              ; ,,
            sta WINDOW_ED,y     ; ,,
            inc IX              ; Advance the cursor
fpos_cur:   lda #TXTCURSOR      ; ,, Add cursor at end
            ldy IX              ; ,,
            sta WINDOW_ED,y     ; ,,
            lda #"."            ; .. Add file extension .P5
            sta WINDOW_ED+8     ; ,, ,,           
            bne fgetkey         ; ,,
fbacksp:    ldy IX
            beq fgetkey
            lda #" "
            sta WINDOW_ED,y
            sta TEMPNAME,y
            dec IX
            jmp fpos_cur
fdone:      ldy IX
            cpy #0              ; Do not allow RETURN if there's no name
            beq fgetkey         ; ,,
            ldx #0
-loop:      lda SyxExt,x
            sta TEMPNAME,y
            iny
            inx
            cpx #8
            bne loop            
            clc                 ; Carry clear for return
            rts

; Program Number Input
; Cursor position in Y
SetPrgNum:  sty IX              ; Set current cursor position
            lda #TXTCURSOR      ; ,, Add cursor at end
            sta WINDOW_ED,y     ; ,,
pgetkey:    jsr Keypress        ; Keycode in Y, PETSCII in A
            cpy #CANCEL         ; Cancel
            bne pch_bk          ; ,,
            jmp setprg_r        ; ,,
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
            sta TEMPNAME,y      ;   ,,
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
            sta TEMPNAME,y
            dec IX
            jmp ppos_cur
pdone:      sec                 ; Set carry (error) as default
            ldy IX              ; If edit isn't complete, then do nothing
            beq setprg_r        ; ,, unless it's totally empty
            cpy #3              ; ,,
            bne pgetkey         ; ,,
            lda TEMPNAME           ; Get the input numeral
            sec                 ; Subtract 1, because group is zero-indexed
            sbc #1              ; ,,
            and #$07            ; Constrain to actual group number
            sta PTRD            ; Store in destination location
            lda TEMPNAME+1      ; Here's the bank number
            sec                 ; Subtract 1, because bank is zero-indexed
            sbc #1              ; ,,
            and #$07            ; Constrain to a bank number
            asl                 ; Multiply by 8, since it's going along
            asl                 ;   with the program number
            asl                 ;   ,,
            sta IX              ;   and store it temporarily
            lda TEMPNAME+2      ; Now the program number 
            sec                 ; Same stuff as above, yadda yadda yadda
            sbc #1              ; ,,
            and #$07            ; ,,
            ora IX              ; Combine with the previously-stored bank number
            ldy #5              ; Store in the destination location
            sta PTRD+1          ;   and that's it!
            clc                 ; Clear carry to indicate everything's good
setprg_r:   rts                 ; Return with carry set if invalid

; Prepare Field
; for increment or decrement
PrepField:  ldy FIELD_IX
            lda FNRPN,y
            tax
            lda FType,y
            cmp #F_NAME
            beq no_decinc
            tay
            lda CVOICE,x
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
            ; Fall through to PopFields

; Populate Fields
; at PAGE        
PopFields:  ldy PAGE            ; Recall the last field from this page
            lda LAST_LIB_IX,y   ;   ,,
            sta FIELD_IX        ;   ,,
pf_prg:     ldx PAGE
            cpx #7              ; If Help page, do not populate any fields
            beq ShowPrgNum      ; ,, and do not draw a cursor
            ldy TopParamIX,x
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
            ldx #0
            lda (FIELD,x)       ; If there's anything other than a space
            cmp #" "            ;   here, then do not draw the cursor
            bne dc_col          ;   ,,
            lda #CURSOR
            sta (FIELD,x)       ;   ,,
dc_col:     lda #SELCOL
            jsr field_col       ; Color field, if necessary
            ; Fall through to ShowPrgNum

; Show Current Voice and Program Numbers
ShowPrgNum: ldy CVOICE_IX       ; Get current program number
            jsr PrgLoc          ; ,,
            ldy #2              ; Show program number or unset (---)
-loop:      lda TEMPNAME,y      ; ,,
            sta STATUSDISP+2,y  ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            ldx #" "            ; Default suffix
            ldy #4              ; Is the program a factory program
            lda (PTR),y         ;   (group number > 5)?
            bmi vce_num         ;   (and also, bit 7 is clear)
            cmp #05             ;   ,,
            bcc vce_num         ;   ,,
            ldx #$86            ; A reverse "F"
vce_num:    stx STATUSDISP+5    ; ,,
            ldy CVOICE_IX       ; Get current voice 
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
savemark:   lda MARKED,y        ; Show or hide the Save mark
            bne mark_on         ; ,,
            lda #" "            ; ,,
            .byte $3c           ; ,. Skip word (SKW)
mark_on:    lda #$93            ; ,, (Reverse S screen code)
            sta STATUSDISP+7    ; .. Add to status display    
            rts
          
; Clear Previous Cursor 
; Alias as the Blank field type
Blank:
ClrCursor:  ldy FIELD_IX        ; Remove the previous cursor
            lda FRow,y          ; ,,
            jsr FieldRow        ; ,,
            ldx #0              ; ,,
            lda (FIELD,x)       ; If there's not a cursor to delete, then
            cmp #CURSOR         ;   do not do anything
            bne cc_col          ;   ,,
            lda #" "            ; ,,
            sta (FIELD,x)       ; ,,
cc_col:     lda #PARCOL
field_col:  sta ANYWHERE        ; A is the color. Stash it...
            jsr FieldColor      ; Set FIELD to the color address
            lda FType,y         ; Color some cells based on the field type
            tax                 ; ,,
            lda TColor,x        ; ,, (number of cells to color)
            beq clr_cur_r       ; ,,    
            tay                 ; Y is the cell count
            dey                 ;   minus one
            lda ANYWHERE
-loop:      sta (FIELD),y
            dey 
            bpl loop
clr_cur_r:  ldy #0
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
            lda CVOICE,y        ;   and put the current value in A
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
            cpy #20             ; Max size, for the NAME field
            bcc loop
wr_r:       rts
            
; Convert PETSCII to Screen Code
; In A
PETtoScr:   cmp #0              ; A zero will be treated as a space here
            beq space           ; ,,
            cmp #123            ; Make the Name field case-insensitive
            bcs ch_pet          ;   by subtracting 32 for uppercase
            cmp #97             ;   ,,
            bcc ch_pet          ;   ,,
            ;sec                ;   ,, (carry already known clear)
            sbc #$20            ;   ,, 
ch_pet:     cmp #$ff            ; Is pi, which is an odd duck
            beq pi              ; ,,
            cmp #$5c            ; Change GBP to backslash
            beq backsl          ; ,,
            cmp #$5f            ; Change left arrow to underscore
            beq undersc         ; ,,
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
space:      lda #" "            ; Screen code for space
            .byte $3c           ; (SKW)
pi:         lda #$5e            ; Screen code for pi
            .byte $3c           ; (SKW)
backsl:     lda #$4d            ; Screen code for backslash
            .byte $3c           ; (SKW)
undersc:    lda #$52            ; Screen code for underscore
            rts   
            
; Clear Screen
; And color appropriately
ClrScr:     ldx #230            ; Clear the entire screen, except for the
-loop:      lda #" "            ;   bottom 2 rows, which are used for status,
            sta SCREEN+22,x     ;   and the top row.
            sta SCREEN+231,x    ;   ,,
            lda #PARCOL         ;   ,, (for parameters)
            sta COLOR,x         ;   ,,
            sta COLOR+231,x     ;   ,,
            dex                 ;   ,,
            cpx #$ff            ;   ,,
            bne loop            ;   ,,
            lda #<(COLOR+22)    ; Set margin cursor to the select color
            sta PTRD            ; ,,
            lda #>(COLOR+22)    ; ,,
            sta PTRD+1          ; ,,
            ldy #21             ; ,,
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
            lda #$2a            ; Show the MIDI indicator in lower right
            sta SCREEN+505      ; ,,
            jmp HOME
            
; Display Status Message
; in X            
Status:     txa                 ; TODO Why?
            pha
            lda #<(STATUSDISP+10)
            sta FIELD
            lda #>(STATUSDISP+10)
            sta FIELD+1
            lda StatusH,x
            tay 
            lda StatusL,x
            jsr WriteText
            pla
            tax
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
; For voice index in Y
; For display. Sets 3 TEMPNAME  locations with group, bank, and program numbers
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
            sta TEMPNAME        ;   Set it to first digit
            iny
            lda (PTR),y         ; Get bank/program number
            pha
            and #$07            ; Isolate the program number
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta TEMPNAME+2      ;   Set it to third digit
            pla
            lsr                 ; Isolate the bank number
            lsr                 ;   ,,
            lsr                 ;   ,,
            clc                 ;   Add #$31 to make it a screen code numeral
            adc #$31            ;   ,,
            sta TEMPNAME+1      ;   Set it to second digit
            rts
unset:      lda #"-"
            sta TEMPNAME 
            sta TEMPNAME+1
            sta TEMPNAME+2
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
            ldy #231            ; Color everything blue before opening
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

; Source Voice Number
; Show voice number for Commodore+S, Commodore+L, C, and Commodore+C
; srcvce_un is the unconditional entry point
SourceVce:  bit COMMODORE       ; If Commodore was pressed,
            bpl fvce_r          ;   show the voice number in the prompt
srcvce_un:  ldy CVOICE_IX       ;   ,,
            iny                 ;   ,, increment for 1-index
            jsr TwoDigNum       ;   ,,
            ora #$80            ;   ,, put ones place in reverse
            sta SCREEN+214      ;   ,, in the popup window
            txa                 ;   ,, same with tens place
            ora #$80            ;   ,,
            sta SCREEN+213      ;   ,, 
fvce_r:     rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O AND DATA SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dump Single Program
; At the current library
; To dump another voice, set PTR and call the DumpVoice endpoint instead
DumpPrg:    jsr SetCurPtr
            sty IX              ; Store in temporary index for status message
            jsr DumpVoice
            jmp MainSwitch

; Dump Bank or Group
; Find all members of the current program's bank and/or group and dump them     
DumpGroup:  sec                 ; Set DUMPTYPE flag to indicate group dump
            .byte $34           ; Skip byte (SKB)
DumpBank:   clc                 ; Clear DUMPTYPE flag to indicate bank dump
            ror DUMPTYPE        ; ,,
            jsr Popup           ; Set up progress bar popup
            jsr SetCurPtr       ; Get the current program's bank number
            ldy #4              ;   Get the group
            lda (PTR),y         ;   ,,
            sta S_GROUP         ;   and set it as the search group
            iny                 ;   Get the bank
            lda (PTR),y         ;   ,,
            and #$f8            ;   ,, (isolate the bank bits)
            sta S_BANK          ;   and set it as the search bank
            ldy #0              ; IX is going to be the index of the search
            sty IX              ; ,,
-loop:      jsr Validate        ; Set the pointer to this voice
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
d_match:    jsr DumpVoice       ; If it does, dump the voice
d_nomatch:  lda KEY             ; If STOP is held at this point, exit
            cmp #CANCEL         ; ,,
            beq dumpbank_r      ; ,,
            inc IX              ; Move to the next voice
            lda IX              ; Draw the value bar based on index
            asl                 ;   ,, (twice the index, actually)
            jsr ProgPopup       ;   ,,
            ldy IX              ; Check the search index for the end
            cpy #LIB_TOP        ; ,,
            bne loop
dumpbank_r: jmp MainSwitch
            
; Dump a Voice
; Set PTR before the call with VoicePtr or Validate            
DumpVoice:  ldy #0              ; Set the output index
-loop:      lda (PTR),y         ; Get the next byte to output
            cpy #4              ; Is this the group number byte?
            bne dv_send         ; ,, If not, send
            bit COMMODORE       ; Is the factory flag set?
            bpl dv_send         ; ,, If not, send
            cmp #5              ; Is this already in a Factory group?
            bcs dv_send         ; ,, If so, send
            ;clc                ; If all the aforementioned conditions pass,
            adc #5              ;   add 5 to the group number
dv_send:    jsr MIDIOUT         ; Send it to the Beige Maze MIDI KERNAL
            bcs dv_err          ; Show error if timeout
            cmp #ST_ENDSYSEX    ; Was this an end-of-sysex status?
            beq dv_ok           ; If so, dump is done with success
            iny                 ; Go to the next index
            bne loop            ; If we get all the way to 0, something's wrong
dv_err:     ldx #SM_FAIL        ; Fail by either (1) timing out at the
            jmp Status          ;   interface, or (2) invalid sysex
dv_ok:      ldx #SM_SENT        ; Success!
            jsr Status          ; Show the status and the voice
            ldy IX              ;   number
            iny                 ;   ,, (which is 1-indexed)
            jsr TwoDigNum       ;   ,,
            stx STATUSDISP+18   ;   ,,
            sta STATUSDISP+19   ;   ,,
            rts
            
; Send Edit Buffer
; From the current program        
BufferSend: jsr PackVoice       ; Pack prior to sending so it's correct
            lda #<EditBuffer    ; Construct an edit buffer dump with the
            ldy #>EditBuffer    ;   documented header
            jsr SysexMsg        ;   ,,
            lda PTR             ; Set up addresses for packing the voice data
            sta P_RESULT        ;   into the TEMPBUFF (whose address is set in
            lda PTR+1           ;   PTR by SysexMsg a few lines up).
            sta P_RESULT+1      ;   ,,
            lda #<CVOICE        ;   Start at the current voice
            sta P_START         ;   ,,
            clc                 ;   Add 136 to start to get the end offset. This
            adc #136            ;   is a bit larger than the parameter data, but
            sta P_END           ;   it's necessary to get to the end. Extra size
            lda #>CVOICE        ;   doesn't matter because we're adding #$7F
            sta P_START+1       ;   explicitly at the end, to end the sysex.
            sta P_END+1         ;   ,,
            jsr Pack            ; Perform the packing operation into TEMPBUFF
            lda #ST_ENDSYSEX    ; Then add #$7F
            sta TEMPBUFF+$9c    ; ,,
            jmp SendSysex       ; Send the whole TEMPBUFF and show status
            
; Unpack to Buffer
; Using current PTR
; For the UnpSeed endpoint, prepare by setting P_RESULT
UnpBuff:    lda PTR
            ldy PTR+1
            ldx #<CVOICE        ; Set program buffer as result
            stx P_RESULT
            ldx #>CVOICE
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
; The current voice
; Generates system exclusive in a 159-byte voice memory region
PackVoice:  ldy CVOICE_IX
            jsr Validate        ; Validate the existing voice, which
            beq hdr_ok          ;   sets PTR. If OK, continue
            ldy #03             ; Otherwise, generate a sysex header in the
-loop:      lda PrgDump,y       ;   library, with a group number byte of
            sta (PTR),y         ;   $80, which indicates that no group is
            dey                 ;   set.
            bpl loop            ;   ,,
            jsr UnsetProg       ;   ,, Unset the program number in sysex
            lda #$00            ;   ,,
            iny                 ;   ,,
            sta (PTR),y         ;   ,,
hdr_ok:     lda PTR
            clc
            adc #$06
            sta P_RESULT
            lda PTR+1
            sta P_RESULT+1
            lda #<CVOICE
            sta P_START
            clc
            adc #$80
            sta P_END
            lda #>CVOICE
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
; from a documented sysex header
; from A=low/Y=high to TEMPBUFF
; PTR points to the next byte in the sysex output stage
SysexMsg:   sta PTR
            sty PTR+1
            ldy #0
-loop:      lda (PTR),y
            beq msg_done        ; Messages in the table are delimited by 00
            sta TEMPBUFF,y      ; Store the message in TEMPBUFF
            iny
            bne loop
msg_done:   tya                 ; Update PTR so that the caller can continue
            clc                 ;   with the message
            adc #<TEMPBUFF      ;   ,,
            sta PTR             ;   ,,
            lda #>TEMPBUFF      ;   ,,
            sta PTR+1           ;   ,,
sm_r:       rts
            
; Send Sysex
; Then show status message depending on result
SendSysex:  ldy #0              ; Send a byte at a time from TEMPBUFF
-loop:      lda TEMPBUFF,y      ; ,,
            jsr MIDIOUT         ; ,,
            bcs send_fail       ; ,, Error out if timeout
            cmp #ST_ENDSYSEX    ; Until end of sysex
            beq send_ok         ; ,,
            iny                 ; ,,
            bne loop            ; ,, (Fail if overread, but that would be a bug)
send_fail:  ldx #SM_FAIL        ; Set message type
            .byte $3c           ; ,, Skip word (SKW)
send_ok:    ldx #SM_SENT        ; ,,
            jmp Status          ;  
       
; Select Voice
; Unpack specified library index (in Y) to the current voice buffer
SelVoice:   jsr Validate
            beq lib_good
            jsr NewVoice
lib_good:   jsr UnpBuff
            ; Fall through to PrgChgMsg
            
; Send Program Change
; Including bank select, for the current voice pointer, if it's a program
PrgChgMsg:  lda PRGCH_TX        ; Is program change transmit on?
            beq pch_r           ; ,, If not, do nothing
            ldy #4              ; Get group nunber
            lda (PTR),y         ; ,,
            bmi pch_r           ; If unret, do nothing
            pha                 ; Save group for bank select message
            ldx #$00            ; Send bank select message using group
            ldy #$00            ; ,,
            jsr CONTROLC        ; ,, Bank select MSB
            ldx #$20            ; ,,
            pla                 ; ,,
            tay                 ; ,,
            jsr CONTROLC        ; ,, Bank select LSB
            ldy #5              ; Send program change message using
            lda (PTR),y         ;   bank and program numbers
            tax                 ;   ,,
            jsr PROGRAMC        ;   ,,
pch_r:      rts

; Set Voice Pointer
; to voice index in Y
SetCurPtr:  ldy CVOICE_IX       ; For this endpoint, use the current index
VoicePtr:   lda LibraryL,y      ; In case of soft reset, advance library
            sta PTR             ;   pointer to the last voice
            lda LibraryH,y      ;   ,,
            sta PTR+1           ;   ,,
            rts

; Validate Library
; Check sysex for Program Dump message, and #$7F in the right place
; Library index in Y
; Valid if zero flag is set, as in BEQ is_good
Validate:   jsr VoicePtr        ; Set the voice pointer
            ldy #3              ; Compare the data at the pointer to the
-loop:      lda (PTR),y         ;   documented program dump message
            cmp PrgDump,y       ;   ,,
            bne invalid         ;   ,, and bail if a byte doesn't match
            dey                 ;   ,,
            bpl loop            ;   ,,
            ldy #$9e            ; If the header is okay, also make sure that the
            lda (PTR),y         ;   data has $7F at offset $9E, which is where
            cmp #ST_ENDSYSEX    ;   the Prophet-5 voice message ends.
invalid:    rts                 ; Result of last compare is in Z

; New Voice
; With data pointer already in PTR
NewVoice:   ldy #0              ; Zero out 159 bytes
            lda #0              ; ,,
-loop:      sta (PTR),y         ; ,,
            iny                 ; ,,
            cpy #$a0            ; ,,
            bne loop            ; ,,
            ldx #$ff            ; X is the table for the INIT name
            ldy #$50            ; Y is the offset of name in sysex
-loop:      inx                 ; Set name from the table
            iny                 ; ,,
            lda Init,x          ; ,,
            sta (PTR),y         ; ,,
            bne loop            ; ,, Name is delimited by 0
            ldy #3              ; Create the sysex header for a valid program
-loop:      lda PrgDump,y       ; ,,
            sta (PTR),y         ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            ldy #$9e            ; ,,
            lda #ST_ENDSYSEX    ; Create the end-of-sysex delimiter
            sta (PTR),y         ; ,,
UnsetProg:  lda #$80            ; For a new voice, set the program
            ldy #4              ;   number to unset with b7
            sta (PTR),y         ;   ,,
            rts
       
; Pseudo-Random
; One bit of "random," returned in carry         
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

; Clear Undo Leveels
; For voice index in Y            
ClrUndo:    sty IX
            ldx UNDO_LEV
-loop:      lda UNDO_VCE,x
            cmp IX
            bne nx_clr
            lda #$ff
            sta UNDO_VCE,x
nx_clr:     dex
            bpl loop
            rts
            
; Set Undo
; Manage undo levels    
SetUndo:    txa 
            pha
            ldy CVOICE_IX       ; Y = current voice index
            ldx FIELD_IX        ; X = current field index
            cpx LAST_FIX        ; If the last field has changed again,
            bne diff_f          ;   and on the same voice number,
            cpy LAST_VCE        ;   do nothing
            beq pre_r
diff_f:     cpx TopParamIX+4    ; If this is one of the settings parameters
            bcs pre_r           ;   for Ed, do not create an Undo level
            stx LAST_FIX        ; Store the last field index
            sty LAST_VCE        ; Store the last voice
            ldy UNDO_LEV        ; If there are undo levels remaining,
            cpy #UNDOS          ; ,,
            bcc save_lev        ; ,, save a new level
            ldy #1              ; If the level is at max, then move
-loop:      lda UNDO_FIX,y      ;   the current levels down one,
            sta UNDO_FIX-1,y    ;   resulting in the loss of the
            lda UNDO_VAL,y      ;   oldest undo level
            sta UNDO_VAL-1,y    ;   ,,
            lda UNDO_VCE,y      ;   ,,
            sta UNDO_VCE-1,y    ;   ,,
            iny
            cpy #UNDOS          ;   ,,
            bne loop            ;   ,,
            ldy #UNDOS-2        ; Wants to be UNDOS-1, but there's an INY coming
save_lev:   iny                 ; Move level pointer
            sty UNDO_LEV        ; ,,   
            txa                 ; ,,
            sta UNDO_FIX,y      ; Store field number in value list
            lda CVOICE_IX       ; Store voice number in voice list
            sta UNDO_VCE,y      ; ,,
            lda FNRPN,x         ; Convert X from field index to NRPN
            tax                 ; ,,           
            lda CVOICE,x        ; Get pre-change value
            sta UNDO_VAL,y      ; Store it in UNDO value list
pre_r:      pla 
            tax
            rts            
                        
; Post NRPN Change
; Send NRPN, if enabled
; NRPN index is in X
NRPNpost:   lda NRPN_TX         ; Skip the whole thing is NRPN is disabled
            beq post_r          ; ,,
            cpx #$90            ; If this is one of the settings
            bcs post_r          ;   parameters for Ed, do not send to P5
            stx NRPN_NUM        ; Temporarily store the NRPN number in IX
            lda MIDI_CH         ; Get MIDI channel
            ora #%10110000      ; Control Change
            jsr MIDIOUT         ; ,,
            lda #%01100011      ; NRPN parameter number MSB CC
            jsr MIDIOUT         ; ,,
            lda #%00000000      ; Parameter Number MSB
            jsr MIDIOUT         ; ,,
            lda #%01100010      ; NRPN parameter number LSB CC
            jsr MIDIOUT         ; ,,
            lda NRPN_NUM        ; Parameter number LSB
            jsr MIDIOUT         ; ,,
            lda #%00000110      ; NRPN parameter value MSB CC
            jsr MIDIOUT         ; ,,
            lda #%00000000      ; Parameter value MSB
            jsr MIDIOUT         ; ,,
            lda #%00100110      ; NRPN parameter value LSB CC
            jsr MIDIOUT         ; ,,
            ldx NRPN_NUM        ; Get the NRPN number
            lda CVOICE,x        ; Get the value
            and #$7f            ; Constrain for CC
            jsr MIDIOUT         ; ,,
post_r:     rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERRUPT HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NMI watches for incoming MIDI data and the reset key combo
NMISR:      pha                 ; NMI does not automatically save registers like
            txa                 ;   IRQ does, so that needs to be done
            pha                 ;   ,,
            tya                 ;   ,,
            pha                 ;   ,,
            jsr CHKMIDI         ; Is this a MIDI-based interrupt?
            bne midi            ;   If so, handle MIDI input
            bit $9111           ; Read VIA to clear interrupt
            lda SHIFT           ; Check for both Commodore and Control.
            cmp #6              ; ,,
            bne ignore          ; ., otherwise, RFI
            pla                 ; Remove X, Y, A, status flag, and return
            pla                 ;   address because RTI isn't being done.
            pla                 ;   ,,
            pla                 ;   ,,
            pla                 ;   ,,
            pla                 ;   ,,
            jmp Reset           ; Reset application
ignore:     jmp RFI             ; Back to normal NMI, after register saves
midi:       jsr MIDIIN          ; MIDI byte is in A
            cmp #0              ; Flash indicator if non-zero byte
            beq skip_ind        ; ,,
            sta COLOR+505       ; ,,
skip_ind:   cmp #ST_SYSEX       ; If sysex, 
            bne sy_catch        ;   ,,
            ldy TVOICE_IX       ; Get target voice index
            ldx #1              ;   set sysex listen flag
            stx LISTEN          ;   ,,
            lda #0              ; Unmark an incoming program for selected save
            sta MARKED,y        ; ,,
            ldx LibraryL,y      ; Set library memory from index
            stx SYIN            ; ,,
            ldx LibraryH,y      ; ,,
            stx SYIN+1          ; ,,
            ldx #0              ; Initialize library location index
            stx SYIN_IX         ; ,,
            lda #ST_SYSEX       ; Restore byte to sysex start for recording
sy_catch:   ldx LISTEN          ; If sysex listen flag is on, store the byte to
            beq r_isr           ;   specified memory
sy_store:   ldy SYIN_IX         ; Get the index and store the byte there
            sta (SYIN),y        ; ,,
            cmp #ST_ENDSYSEX    ; If the sysex has ended, perform end
            beq sydone          ; ,,
            inc SYIN_IX         ; Increment storage index. If it exceeds 160
            lda SYIN_IX         ;   end the sysex, for a likely error message,
            cmp #$a0            ;   But we don't want it to overwrite subsequent
            bcc r_isr           ;   voice's memory.
sydone:     ldy #0              ; Set listen flag off
            sty LISTEN          ; ,,
            iny                 ; Set sysex ready flag on, to be handled in
            sty READY           ;   the main loop
            lda TVOICE_IX       ; Copy library index to current library index 
            sta CVOICE_IX       ; ,,
            cmp #LIB_TOP-1      ; If not at the top voice yet, advance target
            beq r_isr           ;   library index
            inc TVOICE_IX       ;   ,,
r_isr:      jmp RFI             ; Restore registers and return from interrupt
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA FIELDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Value Bar
; at FIELD position, with value in A
ValBar:     and #$7f            ; Constrain A to 0-127
            ldy #0              ; Y is the position offset
            lsr                 ; Divide A by 2
            clc                 ; Add one so that zero shows one bar
            adc #1              ; ,,
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
-loop:      cpy #8
            beq var_r
            sta (FIELD),y       ;   ,,
            lda #$20            ; Clear out the rightmost unused characters
            iny                 ; ,,
            bne loop            ; ,,
var_r:      rts                 ; ,,
           
; Draw Switch
; At field location            
Switch:     cmp #1              ; If the value is 1, the switch is ON
            bne s_off           ; ,,
            lda #SW_ON          ; ,, So set the display character
            .byte $3c           ; ,,
s_off:      lda #SW_OFF         ; If the switch is 0, the switch if OFF
            ldx #0              ; Store the display character in the field
            sta (FIELD,x)       ;   screen address
            cpy #12             ; Special case - When the KEYBOARD switch is
            bne switch_r        ;   changed, redraw the Osc B Frequency field,
            ldy #9              ;   because its displayed value is dependent
            jsr DrawField       ; ,,
switch_r:   rts
                   
; Draw Name Field           
Name:       ldy #20
            lda #" "
-loop:      sta SCREEN+22,y
            dey 
            bne loop
            lda #65             ; Offset for name location
            ldy #>CVOICE        ; Current program location
            jmp WriteText
                 
; Draw Enum Field
; (Track, Revision, Retrigger, Bi-Timbral, Unison Count PU2)       
Enum:       sty ANYWHERE        ; Save the NRPN for comparison
            ldx #(EnumInt-EnumNRPN-1)
-loop:      lda EnumNRPN,x      ; Is the entry the current NRPN?
            cmp ANYWHERE        ; ,,
            bne next_enum
            lda EnumInt,x       ; Does the entry match the NRPN value?
            cmp CVOICE,y        ; ,,
            beq found_enum      ; If so, write its text
next_enum:  dex
            bpl loop
            lda CVOICE,y        ; If value not found in enum table, fall back
            bpl Num1Ind         ;   to displaying an integer
found_enum: lda EnumTxtH,x
            tay
            lda EnumTxtL,x
            jmp WriteText

; Draw Program Number            
Program:    pha 
            lsr
            lsr 
            lsr 
            ldy #0
            jsr bankprg
            pla 
            and #$07
            iny 
bankprg:    ora #$30
            clc 
            adc #1
            sta (FIELD),y
            rts

; Voice Number, which is like Num but zero-padded
VoiceNum:   cmp #10             ; If the value is two digits, draw as normal
            bcs Num             ; ,,
            tax                 ; Stash the one-digit number
            lda #"0"            ; Draw a zero in front of it
            ldy #0              ; ,,
            sta (FIELD),y       ; ,,
            iny                 ; Shift Y to the next place
            txa                 ; Put the value back in A
            ora #$30            ; Convert value to numeral
            jmp one_dig+1       ; Show one-digit number (skipping PLA)

; Draw bank number
; Which is basically 1-5, with 6-10 having a factory indicator
BankNum:    tax                 ; Save original value
            lda #" "            ; Space screen code is default indicator
            cpx #5              ; Is this a Factory bank number?
            bcc not_fact2       ; If not, show the space
            lda #$86            ; Reverse "F" is factory indicator
            dex                 ; Decrement X to show bank 1-5
            dex                 ; ,,
            dex                 ; ,,
            dex                 ; ,,
            dex                 ; ,,
not_fact2:  ldy #3              ; Show the indicator
            sta (FIELD),y       ; ,,
            txa                 ; Restore original (or modified) bank number
            ; Fall through to Num1Ind
                                  
; Draw 1-Indexed Numeric Field
Num1Ind:    clc 
            adc #1
            ; Fall through to Num

; Draw Numeric Field       
Num:        tay                 ; Get two digit number numeral codes in
            jsr TwoDigNum       ;   X (tens numeral) and A (ones numeral)
            ldy #0              ; Place the first numeral on the screen
            pha                 ; ,,
            txa                 ; ,,
            cmp #"0"            ; ,, unless it's a zero, in which case,
            beq one_dig         ; ,, refrain from placing it
            sta (FIELD),y       ; ,,
            iny                 ; Advance to the next place
one_dig:    pla                 ; Get the ones place back
            sta (FIELD),y       ;   and place it on the screen
            ldx FIELD_IX        ; Does this field type have a range >= 9?
            lda FType,x         ; ,, (This check is done specifically for
            tax                 ; ,, the group # setting of the P-10
            lda TRangeH,x       ; ,, layer B field)
            cmp #10              ; ,,
            bcc num_r           ; ,, If so, put a space after the value
            iny                 ; ,, to clear out an unused tens place
            lda #" "
            sta (FIELD),y
num_r:      rts
            
; Draw Note Number
; This leverages Freq, below, but represents the actual MIDI note number
; rather than a frequency conversion. Its range is 36 to 96, representing
; the Prophet's keyboard. So this routine will fix the note number data up
; so the Freq draw can finish the job.
NoteNum:    cmp #36             ; If the field is currently less than the low
            bcs low_ok          ;   value, force it middle C
            lda #60             ;   ,,
            sta CVOICE,y        ;   ,,
low_ok:     sec                 ; Now subtract 36 for the benefit of the Freq
            sbc #24             ;   view
            ldx #72             ; Set C6 (36+60) as the upper boundary
            jmp ch_hi           ; Let Freq finish the display work

; Draw Frequency
; For Oscillator A, C0-C4
; For Oscillator B, C0-C4, or C0-C9 if the Keyboard switch is ON
Freq:       ldx #48             ; Default upper range for C0-C4
            cpy #1              ; Is this the Oscillator B NRPN?
            bne c0_4            ; ,, If A, use C0-C4 range
            ldy CVOICE+12       ; Is the "Keyboard" switch ON?
            bne c0_4            ;    If so, it's a C0-C4 range
            clc                 ; Add one to the field, for display only...
            adc #1              ;   The range is C#0 to C9, with 0 being C#0 and
            ldx #108            ;   107 being C9, so +1 to correct display
            bcc ch_hi           ;   then constrain high range to 108
c0_4:       lsr                 ; Divide the value by 2 (2 increments per note)
ch_hi:      stx ANYWHERE        ; Check the proper upper boundary for the
            cmp ANYWHERE        ;   selected frequency range
            bcc get_oct         ;   ,,
            lda ANYWHERE        ;   and enforce it
get_oct:    ldx #0              ; Get the octave number by subtracting 12
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
            
; Draw Voice Line   
VoiceLine:  lda DRAW_IX         ; Where we are on the page
            clc                 ; Add the library view's offset
            adc VIEW_START      ; ,,
            sec                 ; Subtract the page's parameter offset
            sbc TopParamIX+4    ; ,,
            sta VIEW_IX         ; Store in view index
            tay                 ; Is it a valid program?
            jsr Validate        ; ,,
            beq pl_ok           ; ,,
            jsr NewVoice
pl_ok:      ldy VIEW_IX         ; Add the two-digit program number first
            iny                 ;   +1 because it's 1-indexed
            jsr TwoDigNum       ;   ,,
            ldy #1              ; Put those fields on the display
            sta (FIELD),y       ; ,,
            dey                 ; ,,
            txa                 ; ,,
            sta (FIELD),y       ; ,,
            ldx VIEW_IX         ; Get the Prophet 5 voice number
            lda MARKED,x        ; ,, (if marked for save, show the reverse
            bne is_marked       ; ,, S after the voice number)
            lda #" "            ; ,, 
            .byte $3c           ; ,, Skip word (SKW)
is_marked:  lda #$93            ; ,,
            ldy #2              ; ,,
            sta (FIELD),y       ; ,,
            ldy VIEW_IX         ; Add program number to the display line
            jsr PrgLoc          ; ,,
            ldy #3              ; ,,
            lda TEMPNAME        ; ,,
            sta (FIELD),y       ; ,,
            iny                 ; ,,
            lda TEMPNAME+1      ; ,,
            sta (FIELD),y       ; ,,
            iny                 ; ,,
            lda TEMPNAME+2      ; ,,
            sta (FIELD),y       ; ,,
            ldy #4              ; Get group number to check for factory prog
            lda (PTR),y         ; ,,
            bmi not_fact        ; ,, Not factory if $80
            cmp #5              ; ,, If group >= 5, then it's a factory
            bcc not_fact        ; ,, program
            lda #$86            ; ,,
            .byte $3c           ; ,, Skip word (SKW)
not_fact:   lda #" "            ; ,, Reverse F after program number
            ldy #6              ; ,, Set Y to 5 and ut the factory indicator
            sta (FIELD),y       ; ,,   or space on the screen
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
pl_nc2:     lda #<TEMPNAME 
            sta P_RESULT
            lda #>TEMPNAME +1
            sta P_RESULT+1
            jsr Unpack
            ldx #2
            ldy #7
-loop:      lda TEMPNAME,x
            jsr PETtoScr
            sta (FIELD),y
            inx 
            iny
            cpx #15
            bne loop
prli_r:     rts

; Show Hex
; For selected program voice sysex
ShowHex:    jsr PackVoice       ; ,,
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
            ldy #4              ; If this program has an unset program number,
            lda (PTR),y         ;   show question marks instead of the
            bpl showh_r         ;   group number and program number bytes
            lda #$3f            ;   ,,
            sta SCREEN+97       ;   ,,
            sta SCREEN+98       ;   ,,
            lda #$3f+$80        ;   ,,
            sta SCREEN+99       ;   ,,
            sta SCREEN+100      ;   ,,
showh_r:    rts 
            
; Show QComp
; And also set it, because it has this weird property in which it uses the
; high nybble for its value
QComp:      lsr                 ; If incremented, there will be a value like $81
            php
            lsr                 ;   and if decremented, like $7f. This operation
            lsr                 ;   captures the high nybble in A, and the
            lsr                 ;   direction in Carry, with Carry Set=decrement
            bcs q_dec           ;   Skip any adjustment of high nybble value
            plp                 ; Carry set = a change was made
            bcc q_disp          ; If Carry clear, no change
            adc #0              ; ,,
            jmp q_disp          ; Display the Q Comp parameter value
q_dec:      plp                 ; Discard processor status
q_disp:     ora #$30            ; Convert to screen code and write to the field
            ldx #0              ; ,,
            sta (FIELD,x)       ; ,,
            asl                 ; Shift to high nybble to become the ACTUAL 
            asl                 ;   stored value. This shifts away the $30
            asl                 ;   of the screen code 
            asl                 ;   ,,
            sta CVOICE,y        ; Store the value
            rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Status messages
Failed:     .asc "    FAILED",0
Received:   .asc "  RECEIVED",0
Sent:       .asc "   SENT OK",0
NotEmpty:   .asc "PROG # SET",0
Welcome:    .asc "H FOR HELP",0
Generated:  .asc " GENERATED",0
ClrStatus:  .asc "          ",0
Copied:     .asc "    COPIED",0
Saving:     .asc "    SAVING",0
Loading:    .asc "LOADING 00",0
Success:    .asc "   SUCCESS",0
Undone:     .asc "   UNDO --",0
Erased:     .asc "    ERASED",0
Swapped:    .asc "   SWAPPED",0
NoGroup:    .asc "  NO GROUP",0
StatusL:    .byte <Failed,<Received,<Sent,<NotEmpty,<Welcome,<Generated
            .byte <ClrStatus,<Copied,<Saving,<Loading,<Success,<Undone,<Erased,
            .byte <Swapped,<NoGroup
StatusH:    .byte >Failed,>Received,>Sent,>NotEmpty,>Welcome,>Generated
            .byte >ClrStatus,>Copied,>Saving,>Loading,>Success,>Undone,>Erased,
            .byte >Swapped,>NoGroup

; MIDI Messages and Headers
EditBuffer: .byte $f0, $01, $32, $03, 00
PrgDump:    .byte $f0, $01, $32, $02, 00
PrgRequest: .byte $f0, $01, $32, $05, $05, $01, 00

; System Exclusive File Extension
SyxExt:     .asc ".SYX,P,W"

; Value Bar Partials
BarPartial: .byte $e7, $ea, $f6, $61, $75, $74, $65, $20

; Disllowed Characters in Filenames
Disallow:   .asc $22,"#$",$2a,",/:<=>?@",$5c
EOD:        ; End of disallowed

; Library Divisions
; Start voice index for each Library View page
LibDiv:     .byte 0,16,32,48

; Default Scale (C Major)
DefScale:   .byte 60,62,64,65,67,69,71,72

; Mutable Parameters for Generate
; NRPN numbers
Mutable:    .byte 2,8,9,14,15,17,18,21,26,32,33,37,40,43,44,45,46,47,48,49,50

; Key command subtroutine addresses
KeyCode:    .byte INCR,DECR,F1,F3,F5,F7,PREV,NEXT,EDIT
            .byte PREVVCE,NEXTVCE,OPENSETUP,OPENHELP,GENERATE,SETPRG
            .byte VOICESEND,CLEAR,COPY,DSAVE,DLOAD
            .byte PRGREQ,UNDO,HEX,MARK,0
CommandL:   .byte <IncValue-1,<DecValue-1,<PageSel-1,<PageSel-1
            .byte <PageSel-1,<PageSel-1,<PrevField-1,<NextField-1,
            .byte <Advance-1,<PrevVoice-1,<NextVoice-1
            .byte <GoSetup-1,<GoHelp-1,<Generate-1,<SetPrg-1,<GoSend-1
            .byte <GoErase-1,<GoCopy-1,<GoSave-1,<GoLoad-1,<Request-1
            .byte <Undo-1,<GoHex-1,<Mark-1
CommandH:   .byte >IncValue-1,>DecValue-1,>PageSel-1,>PageSel-1
            .byte >PageSel-1,>PageSel-1,>PrevField-1,>NextField-1,
            .byte >Advance-1,>PrevVoice-1,>NextVoice-1
            .byte >GoSetup-1,>GoHelp-1,>Generate-1,>SetPrg-1,>GoSend-1
            .byte >GoErase-1,>GoCopy-1,>GoSave-1,>GoLoad-1,>Request-1
            .byte >Undo-1,>GoHex-1,>Mark-1

; Field type subroutine addresses
; 0=Value Bar, 1=Voice Line, 2=Switch, 3=Tracking, 4=Detune, 5=Wheel, 6=Filter
; 7=Name, 8=Unison Voice Count, 9=Retrigger, 10=Frequency
; 11=MIDI Ch,12=Device#, 13=SixtyFour, 14=No Field, 15=Mutations, 16=Hex
; 17=Q Comp, 18=Bi-Timbral Mode, 19=Note Number, 20=Program Number, 21=Bank
TSubL:      .byte <ValBar-1,<VoiceLine-1,<Switch-1,<Enum-1
            .byte <Num-1,<Num1Ind-1,<Enum-1,<Name-1,<Enum-1,<Enum-1,<Freq-1
            .byte <Num1Ind-1,<Num-1,<VoiceNum-1,<Blank-1,<Num-1,<ShowHex-1
            .byte <QComp-1,<Enum-1,<NoteNum-1,<Program-1,<BankNum-1
TSubH:      .byte >ValBar-1,>VoiceLine-1,>Switch-1,>Enum-1
            .byte >Num-1,>Num1Ind-1,>Enum-1,>Name-1,>Enum-1,>Enum-1,>Freq-1
            .byte >Num1Ind-1,>Num-1,>VoiceNum-1,>Blank-1,>Num-1,>ShowHex-1
            .byte >QComp-1,>Enum-1,>NoteNum-1,>Program-1,>BankNum-1
TRangeL:    .byte 0,  0,  0,0,0, 0,0,48, 0, 0,  0, 0, 8, 1,0, 0,0,  0,0,36, 0,0
TRangeH:    .byte 127,0,  1,2,7,11,1,90,10, 5,107,15,11,64,0,10,0,112,3,96,39,9
TColor:     .byte 8, 20,  1,4,2, 2,3,21, 3, 3,  3, 2, 2, 2,0, 2,0,  1,3, 3, 2,1

; Enum NRPN, integer values, and enum text locations
EnumNRPN:   .byte 19,19,19,20,20,87,87,87,87,87,87,89,89,89,89,53
EnumInt:    .byte 0,  1, 2, 0, 1, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3,10
EnumTxtL:   .byte <NoTrack,<HalfTrack,<FullTrack
            .byte <Rev1,<Rev3
            .byte <LO,<LOR,<LAS,<LAR,<HI,<HIR
            .byte <P5,<NOR,<STC,<SPL,<PU2
EnumTxtH:   .byte >NoTrack,>HalfTrack,>FullTrack
            .byte >Rev1,>Rev3
            .byte >LO,>LOR,>LAS,>LAR,>HI,>HIR
            .byte >P5,>NOR,>STC,>SPL,>PU2

; Enum field text
NoTrack:    .asc "NONE",0       ; Filter keyboard modes
HalfTrack:  .asc "HALF",0       ; ,,
FullTrack:  .asc "FULL",0       ; ,,
Rev1:       .asc "1/2",0        ; Revision
Rev3:       .asc "3  ",0        ; ,,
LO:         .asc "LO ",0        ; Unison priority
LOR:        .asc "LOR",0        ; ,,
LAS:        .asc "LAS",0        ; ,,
LAR:        .asc "LAR",0        ; ,,
HI:         .asc "HI ",0        ; ,,
HIR:        .asc "HIR",0        ; ,,
P5:         .asc "---",0        ; Bi-Timbral modes
NOR:        .asc "NOR",0        ; ,,
STC:        .asc "STC",0        ; ,,
SPL:        .asc "SPL",0        ; ,,
PU2:        .asc "PU2",0        ; Polyphonic Unison

; Note Name Tables
; Flats are constructed of two screen code characters, Commodre-M and
; Commodore-S
NoteName:   .asc   3,  3,  4,  4,  5,  6,  6,  7,  7,  1,  1,  2
Accidental: .asc ' ','#',' ','#',' ',' ','#',' ','#',' ','#',' '

; Initialized Program
Init:       .asc "INIT",0

; Edit Page Data
EditL:      .byte <Edit0, <Edit1, <Edit2, <Edit3, <View, <HexView, <Setup, <Help
EditH:      .byte >Edit0, >Edit1, >Edit2, >Edit3, >View, >HexView, >Setup, >Help
TopParamIX: .byte 0,      17,     32,     48,     67,    83,       84,     100

; Field data
; Field page number (0-3)
FPage:      .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 3,3,3,3,3,3,3,3,3,3,3,3,3
            .byte 3,3,3,3,3,3 ; Prophet-10
FLIBSTART:  .byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
FLIBEND:    
            .byte 5
            .byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
            .byte 7
LFIELD:     .byte $80 ; Delimiter, and LFIELD - FPage = field count

; Field row
FRow:       .byte 0,3,3,4,5,6,9,9,9,10,11,12,13,14,17,18,19
            .byte 1,2,3,4,5,6,7,8,9,10,13,14,15,16,17
            .byte 1,2,3,4,5,8,9,10,10,10,13,14,15,16,17,18
            .byte 0,1,2,3,6,7,8,9,10,11,12,13,14
            .byte 15,16,16,17,18,19 ; Prophet-10
            .byte 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
            .byte 3
            .byte 5,6,7,8,11,12,13,16,17,17,17,18,18,18,19,19
            .byte 1

; Field column
FCol:       .byte 1,3,8,14,14,14,3,8,12,14,14,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,3,8,12,14,14,14,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14
            .byte 14,14,15,14,14,14 ; Prophet-10
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 1
            .byte 14,14,14,14,14,14,14,14,2,8,14,2,8,14,2,8
            .byte 1

; Field type
FType:      .byte F_NAME,F_SWITCH,F_SWITCH,F_FREQ,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_FREQ,F_VALUE,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_VALUE,F_VALUE,F_VALUE
            
            .byte F_VALUE,F_VALUE,F_VALUE,F_TRACK,F_FILTER,F_QCOMP
            .byte F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE
            .byte F_VALUE,F_SWITCH
            
            .byte F_VALUE,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_VALUE
            .byte F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_SWITCH
            
            .byte F_SWITCH,F_RETRIG,F_COUNT,F_DETUNE,F_VALUE,F_VALUE
            .byte F_WHEEL,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_VALUE,F_SWITCH,F_SWITCH
            .byte F_BTMODE,F_BANK,F_PROGRAM,F_NOTE,F_VALUE,F_VALUE ; P10
            
            .byte F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG
            .byte F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG,F_PRG
            
            .byte F_HEX

            .byte F_MIDICH,F_SWITCH,F_SWITCH,F_DEVICE,F_VOICE,F_VOICE
            .byte F_MUTATIONS,F_VALUE
            .byte F_NOTE,F_NOTE,F_NOTE,F_NOTE,F_NOTE,F_NOTE,F_NOTE,F_NOTE
            
            .byte F_NONE
            
; Field NRPN number
FNRPN:      .byte 88,3,4,0,8,10,5,6,7,1,2,9,11,12,14,15,16
            .byte 17,18,40,19,20,85,43,45,47,49,44,46,48,50,51
            .byte 32,33,34,35,36,22,21,23,24,25,26,27,28,29,30,31
            .byte 52,87,53,54,13,37,86,97,41,42,98,38,39
            .byte 89,90,91,95,88,96 ; P10 parameters

            ; The following are not really NRPN numbers, but use the CVOICE
            ; storage for menu settings
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Library View
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; ,,
            .byte $ff
            .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6
            .byte $a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
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
            .asc RT,"ENVELOPE AMT",CR
            .asc RT,"KEYBOARD",CR
            .asc RT,"REV",CR
            .asc RT,"Q COMP",CR
            .asc RT,"ATTACK",CR
            .asc RT,"DECAY",CR
            .asc RT,"SUSTAIN",CR
            .asc RT,"RELEASE",CR
            .asc CR,"AMPLIFIER",CR
            .asc RT,"ATTACK",CR
            .asc RT,"DECAY",CR
            .asc RT,"SUSTAIN",CR
            .asc RT,"RELEASE",CR
            .asc RT,"RELEASE/HOLD",CR
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
            .asc RT,"RETRIGGER",CR
            .asc RT,"VOICE COUNT",CR
            .asc RT,"DETUNE",CR
            .asc CR,"OTHER",CR
            .asc RT,"GLIDE RATE",CR
            .asc RT,"VINTAGE",CR
            .asc RT,"WHEEL RANGE",CR
            .asc RT,"VELOCITY",CR
            .asc RT," FILT",CR
            .asc RT," AMP",CR
            .asc RT,"AFTERTOUCH",CR
            .asc RT," FILT",CR
            .asc RT," LFO",CR
            .asc RT,"PROPHET-10",CR
            .asc RT," LAYER B",CR
            .asc RT," SPLIT POINT",CR
            .asc RT," VOLUME A",CR
            .asc RT," VOLUME B",CR
            .asc 00 
            
Setup:      .asc 30,CR,"   ED FOR PROPHET-5",CR
            .asc "   ",TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc "  2023 JASON JUSTIAN",CR,CR
            .asc "SETTINGS",CR
            .asc RT,"MIDI CHANNEL",CR
            .asc RT,"SEND NRPN",CR
            .asc RT,"PROG CHANGE",CR
            .asc RT,"DISK DEVICE #",CR
            .asc CR,"GENERATION",CR
            .asc RT,"SEED VOICE",CR
            .asc RT,"SEED VOICE",CR
            .asc RT,"MUTATIONS",CR
            .asc CR,"CUSTOM SCALE",CR
            .asc RT,"VELOCITY",CR
            .asc RT,RVON,"1",RVOF,RT,RT,RT,RT
            .asc RT,RVON,"2",RVOF,RT,RT,RT,RT
            .asc RT,RVON,"3",RVOF,RT,RT,RT,RT,CR
            .asc RT,RVON,"4",RVOF,RT,RT,RT,RT
            .asc RT,RVON,"5",RVOF,RT,RT,RT,RT
            .asc RT,RVON,"6",RVOF,RT,RT,RT,RT,CR
            .asc RT,RVON,"7",RVOF,RT,RT,RT,RT
            .asc RT,RVON,"8",RVOF,RT,RT,RT,RT
            .asc 00
            
Help:       .asc CR,158," WWW.BEIGEMAZE.COM/ED",CR,CR
            .asc 5," SPACE",30," SETUP PAGE",CR
            .asc 5," F1-F7",30," EDIT PAGE",CR
            .asc 5," ",RVON,"C=",RVOF,"FN ",30," LIBRARY VIEW",CR
            .asc 5," CRSR ",30," SELECT PARAM",CR
            .asc 5," <  > ",30," EDIT VALUE",CR
            .asc 5," ",RVON,"C=",RVOF,"Z  ",30," UNDO",CR
            .asc 5," -  + ",30," SELECT VOICE",CR
            .asc 5," CLR  ",30," ERASE VOICE",CR
            .asc 5," P    ",30," PRG# ",5,RVON,"C=",RVOF,30,"GROUP#",CR
            .asc 5," C    ",30," COPY ",5,RVON,"C=",RVOF,30,"SWAP",CR
            .asc 5," V    ",30," SEND ",5,RVON,"C=",RVOF,30,"FACTORY",CR
            .asc 5," Q    ",30," REQUEST PROG",CR
            .asc 5," G    ",30," GENERATE VOICE",CR
            .asc 5," L    ",30," LOAD ",5,RVON,"C=",RVOF,30,"TO CURR",CR
            .asc 5," S    ",30," SAVE ",5,RVON,"C=",RVOF,30,"MARKED",CR
            .asc 5," *    ",30," MARK FOR SAVE",CR
            .asc 5," X    ",30," HEX VIEW",CR
            .asc 5," 1-8  ",30," PLAY NOTE",CR
            .asc 00
            
View:       .asc 30,CR,"     LIBRARY VIEW",CR
            .asc "     ",TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc " V# PRG NAME",CR
            .asc 00
            
HexView:    .asc 30,CR,"       HEX VIEW",CR
            .asc "       ",TL,TL,TL,TL,TL,TL,TL,TL,CR
            .asc 00

; Popup Window            
Window:     .asc 5 ; White
            .asc CR,CR,CR,CR,CR,CR
            .asc RT,RT,RT,RT
            .asc P_TL,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_T,P_TR,CR
            .asc RT,RT,RT,RT,P_L,"           ",30,RVON,$d6,RVOF,5,P_R,CR
            .asc RT,RT,RT,RT
            .asc P_L,30,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,TL,5,P_R,CR
            .asc RT,RT,RT,RT,P_L,"            ",P_R,CR
            .asc RT,RT,RT,RT,P_L,"            ",P_R,CR
            .asc RT,RT,RT,RT,P_L,"            ",P_R,CR
            .asc RT,RT,RT,RT
            .asc P_BL,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_B,P_BR,CR
            .asc RT,RT,RT,RT,RT,UP,UP,UP,UP,UP,UP ; Position for label
            .asc 30,00

; Popup Window Dialogs            
PrgLabel:   .asc 5,"CHANGE",CR,CR
            .asc RT,RT,RT,RT,RT,"PROGRAM # TO",30,0
GrpLabel:   .asc 5,"CHANGE",CR,CR
            .asc RT,RT,RT,RT,RT,"GROUP #",RT," TO",30,0
ReqLabel:   .asc 5,"REQUEST #",30,0
SaveLabel:  .asc 5,"SAVE",30,0
SaveLabel2: .asc 5,"SAVE MARKED",30,0
LoadLabel:  .asc 5,"LOAD",30,0
SendMenu:   .asc 5,"SEND VOICE",CR,CR,CR
            .asc RT,RT,RT,RT,RT,RVON,"P",RVOF,"ROGRAM"," ",RVON,"E",RVOF,"DIT",CR
            .asc RT,RT,RT,RT,RT,RVON,"B",RVOF,"ANK"," ",RVON,"G",RVOF,"ROUP"
            .asc 30,0
SendMenu2:  .asc 5,"SEND VOICE",CR,CR,CR
            .asc RT,RT,RT,RT,RT,RVON,"E",RVOF,"DIT BUFF"
            .asc 30,0       
EraseConf:  .asc 5,"ERASE",CR,CR
            .asc RT,RT,RT,RT,RT,"VOICE",CR,CR
            .asc RT,RT,RT,RT,RT,"SURE? (Y/N)",CR
            .asc 30,0
CopyLabel:  .asc 5,"COPY",CR,CR
            .asc RT,RT,RT,RT,RT,"VOICE",CR
            .asc RT,RT,RT,RT,RT,"TO VOICE",30,0
SwapLabel:  .asc 5,"SWAP",CR,CR
            .asc RT,RT,RT,RT,RT,"VOICE",CR
            .asc RT,RT,RT,RT,RT,"WITH VOICE",30,0
FactoryLab: .asc 20,15,32,6,1,3,20,15,18,25,0 ; TO FACTORY as screen codes
                         
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
                                              
LibraryH:   .byte $14,$14,$15,$15,$16,$17,$17,$18
            .byte $19,$19,$1a,$1a,$1b,$1c,$1c,$1d
            .byte $1e,$1e,$1f,$1f,$20,$21,$21,$22
            .byte $23,$23,$24,$24,$25,$26,$26,$27
            .byte $28,$28,$29,$29,$2a,$2b,$2b,$2c
            .byte $2d,$2d,$2e,$2e,$2f,$30,$30,$31
            .byte $32,$32,$33,$33,$34,$35,$35,$36
            .byte $37,$37,$38,$38,$39,$3a,$3a,$3b
DATAEND:    ; End of data tables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBMODULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
#include "./submodules/MIDI-KERNAL/src/midikernal.asm"
#include "./submodules/sequential_lib/6502/sequential_packing.asm"
