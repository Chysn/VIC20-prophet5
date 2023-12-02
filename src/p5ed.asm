;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                               Prophet 5 Editor
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
Vectors:    .word Welcome       ; Welcome Screen
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
DATAPOS     = $02               ; Data position pointer (2 bytes)
PTR         = $04               ; General use pointer
READY       = $033c             ; New sysex is ready
PAGE        = $033d             ; Current page number
FIELD_IX    = $033e             ; Current field index
LISTEN      = $033f             ; Sysex listen flag
TOGGLE      = $0340             ; General use toggle value
CURRPRG     = $1300             ; Current program indexed buffer (128 bytes)
SYSEX       = $1400             ; System exclusive storage (8 programs,$A0 ea.)

; Character Constants
CR          = $0d               ; Carriage return PETSCII
CLSC        = $93               ; Clear screen PETSCII   
RT          = $1d               ; Cursor right 
W_TRI       = $ce               ; Triangle waveform
W_TRI2      = $cd               ; ,,
W_SAW       = $ce               ; Saw waveform
W_SAW2      = $a5               ; ,,
W_SQU       = $a7               ; Square waveform
W_SQU2      = $d0               ; ,,
SW_ON       = $51               ; Switch on screen code
SW_OFF      = $57               ; Switch off screen code
CURSOR      = $3e               ; Cursor screen code

; Key Constants
F1          = 39
F3          = 47
F5          = 55
F7          = 63
PREV        = 31
NEXT        = 23

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
F_RETRIG    = 9                 ; Unison retrigger ()

; System Resources
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eb12             ; System ISR return point
VIC         = $9000             ; VIC starting address
CHROUT      = $ffd2             ; Character out
PRSTR       = $cb1e             ; Print string at A/Y
CASECT      = $0291             ; Disable Commodore case
SYSNMI      = $feb2             ; System NMI
RFI         = $ff56             ; Return from interrupt
KEY         = $c5               ; Pressed key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
Welcome:    jsr $fd8d           ; Test RAM, initialize VIC chip
            jsr $fd52           ; Restore default I/O vectors
            jsr $fdf9           ; Initialize I/O registers
            jsr $e518           ; Initialize hardware
            cli                 ; Clear interrupt flag from ROM jump

Restart:    lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,            
            lda #13             ; Screen color
            sta VIC+$0f         ; ,,
            lda #30             ; Text color
            jsr CHROUT          ; ,,
            lda #<NMISR
            sta NMINV
            lda #>NMISR
            sta NMINV+1    
            jsr SETIN      
            lda #0
            sta PAGE
            sta READY

            lda #<SYSEX         ; Unpack systex into the program buffer
            ldy #>SYSEX         ; ,,            
            jsr UnpBuff

            jsr SwitchPage
            jmp init_data
            
MainLoop:   lda KEY             ; Debounce the key press
            cmp #$40
            bne MainLoop
getkey:     bit READY
            bmi SysexReady
            lda KEY
            cmp #$40
            beq getkey
            ldx #13
            stx VIC+$0f
            cmp #F1
            beq PageSel
            cmp #F3
            beq PageSel
            cmp #F5
            beq PageSel
            cmp #F7
            beq PageSel
            cmp #PREV
            beq PrevField
            cmp #NEXT
            beq NextField
            bne getkey
            
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

NextField:  ldy FIELD_IX
            cpy #57
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

SysexReady: clc                 ; Clear the sysex ready flag
            ror READY           ; ,,
            lda DATAPOS
            cmp #$9f
            bne sysexerr
            lda #14
            sta VIC+$0f
            lda #<SYSEX         ; Unpack systex into the program buffer
            ldy #>SYSEX         ; ,,
            jsr UnpBuff         ; ,,
            jsr SwitchPage
init_data:  ldy #<SYSEX         ; Initialize sysex storage pointer
            sty DATAPOS         ;   ,,
            ldy #>SYSEX         ;   ,,
            sty DATAPOS+1       ;   ..
            jmp MainLoop
sysexerr:   ldy #10
            sty VIC+$0f
            jmp init_data
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw Edit Page
; at PAGE
SwitchPage: lda #CLSC           ; Clear the screen
            jsr CHROUT          ; ,,
            lda #CR             ; One space at the top of each screen
            jsr CHROUT          ; ,,
            ldy #0              ; Set color memory to the parameter color
            lda #1              ;   to make parameters distinct from
-loop:      sta COLOR,y         ;   labels
            sta COLOR+$0100,y   ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            lda #2              ; Red for the MIDI IN indicator
            sta COLOR+504       ;   ,,
            sta COLOR+505       ;   ,,
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
; At field index in A
DrawCursor: ldy FIELD_IX 
            lda FRow,y
            jsr FieldRow
            lda #CURSOR
            ldx #0
            sta (FIELD,x)
            rts
          
; Clear Previous Cursor 
ClrCursor:  ldy FIELD_IX        ; Remove the previous cursor
            lda FRow,y          ; ,,
            jsr FieldRow        ; ,,
            lda #" "            ; ,,
            ldx #0              ; ,,
            sta (FIELD,x)       ; ,,
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
            lda FCol,y
            clc 
            adc FIELD
            sta FIELD
            bcc f_nc2
            inc FIELD+1
f_nc2:      rts

; Set Field Row
; Multiply A by 22 and add to SCREEN+22
; Store result in FIELD pointer
FieldRow:   ldx #<SCREEN
            stx FIELD
            ldx #>SCREEN
            stx FIELD+1
            tax
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
            jsr dataind         ; Show data byte indicator
            cmp #$f0            ; If sysex, 
            bne ch_catch        ;   initialize storage pointer
            sec                 ;   and set sysex listen flag
            ror LISTEN          ;   ,,
ch_catch:   bit LISTEN          ; If sysex listen flag is on, store the byte to
            bpl r_isr           ;   specified memory
sy_store:   ldx #0              ; Clear X to use as indirect index
            sta (DATAPOS,x)     ; Save data byte
            inc DATAPOS         ; Increment storage pointer
            bne nc_isr          ; ,,
            inc DATAPOS+1       ; ,,
nc_isr:     cmp #$f7
            bne r_isr
            clc
            ror LISTEN
            sec
            ror READY
r_isr:      jmp RFI             ; Restore registers and return from interrupt
dataind:    pha 
            lsr
            lsr
            lsr
            lsr
            jsr hexb
            sta SCREEN+504
            pla
            pha
            and #$0f
            jsr hexb
            sta SCREEN+505
            pla
            rts
hexb:       cmp #$0a 
            bcs hexl 
            ora #$30
            rts
hexl:       sbc #$09
            rts     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA FIELDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unimplemented Field
Un:         ldy #0
            lda #"-"
            sta (FIELD),y
            rts
            
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
            lda #>LOR
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
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Value Bar Partials
BarPartial: .byte $e7, $ea, $f6, $61, $75, $74, $65, $20

; Edit Page Data
EditL:      .byte <Edit0, <Edit1, <Edit2, <Edit3
EditH:      .byte >Edit0, >Edit1, >Edit2, >Edit3
TopParamIX: .byte 0,      16,     29,     45

; Field type subroutine addresses
; 0=Value Bar, 1=Ext Value, 2=Switch, 3=Tracking, 4=Detune, 5=Wheel, 6=Filter
; 7=Name, 8=Unison Voice Count, 9=Unison Retrigger
TSubL:      .byte <ValBar-1,<ValBar-1,<Switch-1
            .byte <Track-1,<Num-1,<Num-1,<FiltRev-1,<Name-1,<Num-1,<Retrigger-1
TSubH:      .byte >ValBar-1,>ValBar-1,>Switch-1
            .byte >Track-1,>Num-1,>Num-1,>FiltRev-1,>Name-1,>Num-1,>Retrigger-1
TRangeL:    .byte 0,  0,  0,0,0, 0,0,48, 0, 0
TRangeH:    .byte 120,127,1,2,7,11,1,90,10, 3

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

; Field data
; Field page number (0-3)
FPage:      .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 1,1,1,1,1,1,1,1,1,1,1,1,1
            .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            .byte 3,3,3,3,3,3,3,3,3,3,3,3,3
            .byte $80 ; Delimiter to end field list

; Field row
FRow:       .byte 1,1,2,3,4,7,7,7,8,9,10,11,12,15,16,17
            .byte 1,2,3,4,5,7,8,9,10,13,14,15,16
            .byte 1,2,3,4,5,8,9,10,10,10,13,14,15,16,17,18
            .byte 1,4,5,6,7,10,11,12,13,14,15,16,17

; Field column
FCol:       .byte 3,8,14,14,10,3,8,12,14,14,14,10,10,14,14,14
            .byte 14,14,14,14,14,14,14,14,14,14,14,14,14  
            .byte 14,14,8,8,8,14,14,3,8,12,14,8,8,8,8,8
            .byte 1,14,14,14,14,14,14,14,14,14,14,14,14

; Field type
FType:      .byte F_SWITCH,F_SWITCH,F_VALUE,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_VALUE,F_XVALUE,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_VALUE,F_VALUE,F_VALUE
            
            .byte F_VALUE,F_VALUE,F_VALUE,F_TRACK,F_FILTER,F_VALUE,F_VALUE
            .byte F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE,F_VALUE
            
            .byte F_VALUE,F_VALUE,F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_VALUE
            .byte F_SWITCH,F_SWITCH,F_SWITCH,F_VALUE,F_SWITCH,F_SWITCH
            .byte F_SWITCH,F_SWITCH,F_SWITCH
            
            .byte F_NAME,F_SWITCH,F_RETRIG,F_COUNT,F_DETUNE,F_VALUE,F_VALUE
            .byte F_WHEEL,F_SWITCH,F_SWITCH,F_SWITCH,F_SWITCH,F_SWITCH

; Field NRPN number
FNRPN:      .byte 3,4,0,8,10,5,6,7,1,2,9,11,12,14,15,16
            .byte 17,18,40,19,20,43,45,47,49,44,46,48,50
            .byte 32,33,34,35,36,22,21,23,24,25,26,27,28,29,30,31
            .byte 88,52,87,53,54,13,37,86,41,42,38,39,51

            
; Edit Page Fields
Edit0:      .asc "OSCILLATOR A",CR
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
                      
Edit1:      .asc "FILTER",CR
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
            
Edit2:      .asc "POLY-MOD",CR
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
                        
Edit3:      .asc "NAME",CR
            .asc CR
            .asc CR,"UNISON",CR
            .asc RT,"ON",CR
            .asc RT,"RETRIGGER",CR
            .asc RT,"VOICE COUNT",CR
            .asc RT,"DETUNE",CR
            .asc CR,"OTHER",CR
            .asc RT,"GLIDE RATE",CR
            .asc RT,"VINTAGE",CR
            .asc RT,"WHEEL RANGE",CR
            .asc RT,"VEL : FILTER",CR
            .asc RT,"    : AMP",CR
            .asc RT,"AT  : FILTER",CR
            .asc RT,"    : LFO",CR
            .asc RT,"RELEASE/HOLD"
            .asc 00            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBMODULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
#include "./submodules/MIDI-KERNAL/src/midikernal.asm"
#include "./submodules/sequential_lib/6502/sequential_packing.asm"

            