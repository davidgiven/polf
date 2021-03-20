    .cpu "6502"

PIA1 = $e810
PIA2 = $e820
VIA = $e840
VIA_PB = VIA + 0
VIA_PCR = VIA + 12
CRTC = $e880
CRTC_ADDR = CRTC+0
CRTC_STATUS = CRTC+1
CRTC_DATA = CRTC+1

SCREEN = $8000
MAP_SIZE = 8

.section zp
    player_x:   .word ?
    player_y:   .word ?
    player_h:   .byte ?
.send

; --- Header ----------------------------------------------------------------

; BASIC stub which runs the machine code program.

    * = $401
    .word +, 1
    .enc "none"
    .null $9e, format("%d", _entry)
    .enc "screen"
+   .word 0

; --- Main program ----------------------------------------------------------

_entry:
    lda #12
    sta VIA_PCR
    sei

    ; Initialise.

    lda #0
    sta player_h
    lda #128
    sta player_x+0
    sta player_y+0
    lda #4
    sta player_x+1
    sta player_y+1

-
    jsr cls
    inc player_h
    jsr render
    jsr redraw
    jmp -

; --- Screen redraw ---------------------------------------------------------

; The backbuffer is copied onto the screen. Bytes in the range $80 to $8f are
; considered block graphic bitmaps and converted.

drawpix .macro
    ldy drawbuffer+\1, x
    lda block_lookup, y
    sta SCREEN+\1, x
    .endm

redraw:
    ; Wait for a vsync.
;   lda VIA_PB
;   and #%00100000
;   bne redraw

    ldx #0
-
    drawpix $000
    drawpix $100
    drawpix $200
    drawpix $300
    dex
    bne -
    rts

; --- Main renderer ---------------------------------------------------------

render:
    .block
        lda #79
        sta rcolumn

    column_loop:
        lda player_h
        sec
        sbc #40
        adc rcolumn
        tax
        ldy #0
        lda sin_table, x
        sta vx+0
        bpl +
        dey
+
        sty vx+1

        ldy #0
        lda cos_table, x
        sta vy+0
        bpl +
        dey
+
        sty vy+1

        lda player_x+0
        sta px+0
        lda player_x+1
        sta px+1
        lda player_y+0
        sta py+0
        lda player_y+1
        sta py+1

        lda #0
        sta rsteps

    step_loop:
        inc rsteps

        ; Add the player's direction vector to px/py.

        lda px+0
        clc
        adc vx+0
        sta px+0
        lda px+1
        adc vx+1
        sta px+1

        lda py+0
        clc
        adc vy+0
        sta py+0
        lda py+1
        adc vy+1
        sta py+1

        ; Have we hit a wall?

        lda py+1
        asl
        asl
        asl
        clc
        adc px+1
        tax
        lda map_table, x
        beq step_loop

        ldx rsteps
        ldy height_table, x
        sty rsteps

        ldx rcolumn
        lda #25
        sec
        sbc rsteps
        tay
        jsr plot

        ldx rcolumn
        lda #25
        clc
        adc rsteps
        tay
        jsr plot

    nodraw:
        dec rcolumn
        bpl column_loop

        rts

        .section zp
            rcolumn: .byte ?
            rsteps:  .byte ?
            vx:      .word ?
            vy:      .word ?
            px:      .word ?
            py:      .word ?
        .send
    .bend

; --- Miscellaneous ---------------------------------------------------------

; Plots a pixel at X, Y.
plot:
    .block
        sta pa
        stx px
        sty py

        lda #0
        lsr py
        rol
        lsr px
        rol             ; divide X and Y by two, calculate bitmask
        sta pm

        ldx py
        lda screen_row_table_lo, x
        sta pptr+0
        lda screen_row_table_hi, x
        sta pptr+1

        ldy px
        ldx pm
        lda (pptr), y
        ora pixel_lookup, x
        sta (pptr), y

        rts

        .section zp
            pptr: .word ?
            px:   .byte ?
            py:   .byte ?
            pa:   .byte ?
            pm:   .byte ?
        .send
    .bend


cls:
    lda #0
    tax
-
    sta drawbuffer+$000, x
    sta drawbuffer+$100, x
    sta drawbuffer+$200, x
    sta drawbuffer+$300, x
    dex
    bne -
    rts

block_lookup:
    .byte 32  ; 0 " "
    .byte 126 ; 1 "▘"
    .byte 124 ; 2 "▝"
    .byte 226 ; 3 "▀"
    .byte 123 ; 4 "▖"
    .byte 97  ; 5 "▌"
    .byte 255 ; 6 "▞"
    .byte 236 ; 7 "▛"
    .byte 108 ; 8 "▗"
    .byte 127 ; 9 "▚"
    .byte 225 ; a "▐"
    .byte 251 ; b "▜"
    .byte 98  ; c "▄"
    .byte 252 ; d "▙"
    .byte 254 ; e "▟"
    .byte 160 ; f "█"

pixel_lookup:
    .byte 1
    .byte 2
    .byte 4
    .byte 8

screen_row_table_lo:
    .for i := 0, i < 25, i += 1
        .byte <(drawbuffer+(i*40))
    .next

screen_row_table_hi:
    .for i := 0, i < 25, i += 1
        .byte >(drawbuffer+(i*40))
    .next

cos_table = sin_table + 64
sin_table:
    .for i := 0, i <= (255+64), i += 1
        .char sin((i / 255.0) * 2*pi) * 127.0
    .next

map_table:
    .byte 1, 1, 1, 1, 1, 1, 1, 1
    .byte 1, 0, 0, 1, 1, 0, 0, 1
    .byte 1, 0, 0, 0, 0, 0, 0, 1
    .byte 1, 0, 0, 1, 0, 0, 0, 1
    .byte 1, 0, 0, 1, 0, 0, 1, 1
    .byte 1, 0, 0, 1, 0, 1, 1, 1
    .byte 1, 0, 0, 1, 0, 1, 1, 1
    .byte 1, 1, 1, 1, 1, 1, 1, 1

height_table:
    .for i := 1, i <= 16, i += 1
        .byte 30 / i
    .next

.align $100
drawbuffer:
    .fill 1024, ?

    * = 0
    .dsection zp
    .cerror * > $ff

; vim: sw=4 ts=4 et

