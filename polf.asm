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

; --- Screen draw -----------------------------------------------------------

; The backbuffer is copied onto the screen.

redraw:
    .block
    ; Wait for a vsync.
    lda VIA_PB
    and #%00100000
    bne redraw

    copy .macro
        lda backbuffer+\1, x
        sta SCREEN+\1, x
        .endm

        ldx #0
    -
        copy $000
        copy $100
        copy $200
        copy $300
        dex
        bne -
        rts
    .bend

cls:
    lda #32
    ldx #0
-
    sta backbuffer+$000, x
    sta backbuffer+$100, x
    sta backbuffer+$200, x
    sta backbuffer+$300, x
    dex
    bne -
    rts

; On entry: x=x coord, y=height
vline:
    .block
        sta char
        sty height
        sec
        lda #24
        sbc height
        lsr
        tay
        lda row_table_lo, y
        clc
        adc identity_table, x
        sta drawp+0
        lda row_table_hi, y
        adc #0
        sta drawp+1

        ldy #0
        ldx char
        inc height
    -
        dec height
        beq exit

        txa
        sta (drawp), y
        clc
        lda drawp+0
        adc #40
        sta drawp+0
        bcc -
        inc drawp+1
        jmp -

    exit:
        rts

        .section zp
            char:   .byte ?
            height: .byte ?
            drawp:  .word ?
        .send
    .bend

; --- Main renderer ---------------------------------------------------------

; This is the DDA-based raycaster from
; https://lodev.org/cgtutor/raycasting.html. I hope.

render:
    .block
        lda #32+128
        ldx #1
        ldy #1
        jsr vline
        lda #32+128
        ldx #2
        ldy #2
        jsr vline
        lda #32+128
        ldx #3
        ldy #3
        jsr vline
        lda #$66
        ldx #4
        ldy #4
        jsr vline
        lda #$66
        ldx #5
        ldy #5
        jsr vline
        lda #$66
        ldx #6
        ldy #6
        jsr vline
        lda #$66
        ldx #7
        ldy #7
        jsr vline
        lda #$66
        ldx #8
        ldy #8
        jsr vline
        rts
    .bend

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

map_table:
  .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
  .byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,0,1,3,0,0,0,0,3,0,0,0,0,0,0,1
  .byte 1,0,1,0,0,3,0,0,0,0,0,3,0,0,0,1
  .byte 1,0,1,0,0,0,1,1,1,1,1,0,0,0,0,1
  .byte 1,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1
  .byte 1,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1
  .byte 1,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1
  .byte 1,0,1,0,0,0,1,1,0,1,1,0,0,0,0,1
  .byte 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

; backbuffer row addresses.

row_table_lo:
    .for i := 0, i < 25, i += 1
        .byte <(backbuffer + i*40)
    .next

row_table_hi:
    .for i := 0, i < 25, i += 1
        .byte >(backbuffer + i*40)
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

identity_table:
    .for i := 0, i < 256, i += 1
        .byte i
    .next

torad .function i
    .endf i * (PI * 2 / 256)

clamp .function n, lo, hi
    .if n < lo
        n := lo
    .endif
    .if n > hi
        n := hi
    .endif
    .endf n

div .function x, y
    .if y == 0
        n := 9999
    .else
        n := x / y
    .endif
    .endf n

sin_table:
cos_table = sin_table + 64
    .for i := 0, i < 256+64, i += 1
        .char 127.0 * sin(torad(i))
    .next

inv_sincos_table:
    .for i := 0, i < 256+64, i += 1
        .char 16.0 * clamp(div(1.0, sin(torad(i))), -7.9, 7.9)
    .next

deltadistx_table:
deltadisty_table = deltadistx_table + 64
    .for i := 0, i < 256+64, i += 1
        .char 16.0 * clamp(abs(div(1, sin(torad(i)))), -7.9, 7.9)
    .next

.align $100
backbuffer:
    .fill 1024, ?

    * = 0
    .dsection zp
    .cerror * > $ff

; vim: sw=4 ts=4 et

