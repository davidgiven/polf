    .cpu "6502"

PIA1    = $e810
PIA1_PA = PIA1 + 0
PIA1_PB = PIA1 + 2

PIA2    = $e820
PIA2_PA = PIA2 + 0
PIA2_PB = PIA2 + 2

VIA     = $e840
VIA_PB  = VIA + 0
VIA_PCR = VIA + 12

CRTC    = $e880
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
    jsr moveplayer
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

; --- Handle player motion --------------------------------------------------

moveplayer:
    .block
        lda #3
        sta PIA1_PA
        lda PIA1_PB
        tax
        and #%00000001
        beq a_pressed
        txa
        and #%00000010
        beq d_pressed
        
        lda #4
        sta PIA1_PA
        lda PIA1_PB
        and #%00000010
        beq w_pressed

        lda #2
        sta PIA1_PA
        lda PIA1_PB
        and #%00000010
        beq s_pressed
        rts

     a_pressed:
        lda player_x+0
        sec
        sbc #$40
        sta player_x+0
        lda player_x+1
        sbc #0
        sta player_x+1
        rts

     d_pressed:
        lda player_x+0
        clc
        adc #$40
        sta player_x+0
        lda player_x+1
        adc #0
        sta player_x+1
        rts

     w_pressed:
        lda player_y+0
        sec
        sbc #$40
        sta player_y+0
        lda player_y+1
        sbc #0
        sta player_y+1
        rts

     s_pressed:
        lda player_y+0
        clc
        adc #$40
        sta player_y+0
        lda player_y+1
        adc #0
        sta player_y+1
        rts

    .bend

; --- Main renderer ---------------------------------------------------------

; This is the DDA-based raycaster from
; https://lodev.org/cgtutor/raycasting.html. I hope.

render:
    .block
        lda #79
        sta rcolumn

    column_loop:
        lda player_h        ; calculate ray angle
        sec
        sbc #40
        adc rcolumn
        sta rdir
        tax

        lda player_x+1
        sta mappos+0
        lda player_y+1
        sta mappos+1

        lda delta_dist_x_table_lo, x ; initialise DDA
        sta delta_dist_lo+0
        lda delta_dist_x_table_hi, x
        sta delta_dist_hi+0
        lda delta_dist_y_table_lo, x
        sta delta_dist_lo+1
        lda delta_dist_y_table_hi, x
        sta delta_dist_hi+1

        ; Initialise stepx and stepy.

        ldy #1
        lda cos_table, x
        bpl +
        ldy #-1
+       sty step+0

        ldy #1
        lda sin_table, x
        bpl +
        ldy #-1
+       sty step+1


        ; Compute the initial distx and disty, from (x - floor(x)) * delta_dist_x.

        lda delta_dist_lo+0
        sta mulr+0
        lda delta_dist_hi+0
        sta mulr+1
        lda #0
        ldx player_x+0      ; fractional part of player_x
        jsr mul_16x16
        sty dist_lo+0
        sta dist_hi+0

        lda delta_dist_lo+1
        sta mulr+0
        lda delta_dist_hi+1
        sta mulr+1
        lda #0
        ldx player_y+0      ; fractional part of player_y
        jsr mul_16x16
        sty dist_lo+1
        sta dist_hi+1

    step_loop:
        ldx #0              ; x is 0 for an X step, 1 for a Y step
        lda dist_lo+0
        cmp dist_lo+1
        lda dist_hi+0
        sbc dist_hi+1
        bpl +
        inx
    +

        clc
        lda dist_lo, x
        adc delta_dist_lo, x
        sta dist_lo, x
        lda dist_hi, x
        adc delta_dist_hi, x
        sta dist_hi, x

        clc
        lda mappos, x
        adc step, x
        sta mappos, x

        lda mappos+1
        asl
        asl
        asl
        clc
        adc mappos+0
        tay
        lda map_table, y
        beq step_loop

        ; We've finished the looping; calculate the distance from the camera plane to
        ; the intersection point.

        ldx dist_lo+0
        lda dist_hi+0
        stx mulr+0
        sta mulr+1
        jsr mul_16x16
        sty dist_lo+0
        sta dist_hi+0

        ldx dist_lo+1
        lda dist_hi+1
        stx mulr+0
        sta mulr+1
        jsr mul_16x16
        sty dist_lo+1

        clc
        adc dist_hi+0
        lsr
        lsr
        lsr
        tay
        ldx rcolumn
        jsr plot

        dec rcolumn
        bmi +
        jmp column_loop
+

        rts

        .section zp
            rdir:    .byte ?
            rcolumn: .byte ?
            rsteps:  .byte ?

            delta_dist_lo: .word ?
            delta_dist_hi: .word ?
            step:          .word ?
            dist_lo:       .word ?
            dist_hi:       .word ?
            mappos:        .word ?
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

; --- Maths -----------------------------------------------------------------

; Fast multiplication routines from
; https://codebase64.org/doku.php?id=base:seriously_fast_multiplication.

; Computes AY = A*X, unsigned; preserves X. Note that the result is 16 bits
; wide.
mul_8x8:
    sta sm1+1
    sta sm3+1
    eor #$ff
    sta sm2+1
    sta sm4+1
    ; falls through
; Computes AX = A * lhs of previous multiply.
mul_8x8_again:
    sec
sm1 lda square1_lo, x
sm2 sbc square2_lo, x
    tay
sm3 lda square1_hi, x
sm4 sbc square2_hi, x
    rts

; Computes AY = AX * mulr, unsigned. Note that the result is 16 bits.
;
;
;       L1 L0
;       R1 R0
;    --------
;       A1 A0 = L0 * R0
;    B2 B1    = L1 * R0
;    C2 C1    = L0 * R1
;    --------
;       R1 R0

mul_16x16:
    stx mull+0
    sta mull+1

    ; mull+0 * mulr+0

    lda mulr+0
    jsr mul_8x8
    sty mulres+0
    sta mulres+1

    ; mull+0 * mulr+1

    ldx mulr+1
    jsr mul_8x8_again
    tya
    clc
    adc mulres+1
    sta mulres+1

    ; mull+1 * mulr+0

    lda mull+1
    ldx mulr+0
    jsr mul_8x8
    tya
    clc
    adc mulres+1
    ldy mulres+0
    rts

    .section zp
        mulr: .word ?
        mull: .word ?
        mulres: .word ?
    .send

; Fast 16-bit square root routine, Y = sqrt(AX).
; Taken from https://codebase64.org/doku.php?id=base:fast_sqrt.

sqrt_16:
    .block
        stx m+0
        sta m+1
        ldy #0          ; R = 0
        ldx #7          ; loop counter
    loop:
        tya
        ora stab-1, x
        sta t+1         ; calculate (r<<8) | (d<<7)
        lda m+1
        cmp t+1
        bcc +           ; if t <= m
        sbc t+1
        sta m+1         ; m = m - t
        tya
        ora stab, x
        tay
    +
        asl m+0
        rol m+1
        dex
        bne loop

        sty t+1
        lda m+0
        cmp #$80
        lda m+1
        sbc t+1
        bcc +
        iny
    +
        rts

    .section zp
        t:  .word ?
        m:  .word ?
    .send

    stab:
        .byte $01, $02, $04, $08, $10, $20, $40, $80
    .bend

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
    .byte 1, 0, 0, 0, 0, 0, 0, 1
    .byte 1, 0, 0, 0, 0, 0, 0, 1
    .byte 1, 0, 0, 0, 0, 0, 0, 1
    .byte 1, 0, 0, 0, 0, 0, 0, 1
    .byte 1, 1, 1, 1, 1, 1, 1, 1

height_table:
    .for i := 1, i <= 16, i += 1
        .byte 30 / i
    .next

; delta_dist precomputation tables, one per angle.

calculate_delta_dist .function i
    theta := (i / 256.0) * 2*pi
    .if theta == 0
        x := 65535
    .else
        x := abs(256 / sin(theta))
    .endif
    .if x > 65535
        x := 65535
    .endif
.endf x

delta_dist_x_table_lo = delta_dist_y_table_lo + 64
delta_dist_y_table_lo:
    .for i := 0, i < 256+64, i += 1
        .byte <calculate_delta_dist(i)
    .next

delta_dist_x_table_hi = delta_dist_y_table_hi + 64
delta_dist_y_table_hi:
    .for i := 0, i < 256+64, i += 1
        .byte >calculate_delta_dist(i)
    .next

; square multiplication tables.

.align $100 ; required or it won't work
square1_lo:
    .for i := 0, i < 512, i += 1
        .byte <((i*i)/4)
    .next

square1_hi:
    .for i := 0, i < 512, i += 1
        .byte >((i*i)/4)
    .next

square2_lo:
    .for i := 0, i < 512, i += 1
        .byte <(((i-255)*(i-255))/4)
    .next

square2_hi:
    .for i := 0, i < 512, i += 1
        .byte <(((i-255)*(i-255))/4)
    .next


.align $100
drawbuffer:
    .fill 1024, ?

    * = 0
    .dsection zp
    .cerror * > $ff

; vim: sw=4 ts=4 et

