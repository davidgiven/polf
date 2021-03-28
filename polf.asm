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

ACCELERATION = $08

.section zp
    player_x:   .byte ?
    player_y:   .byte ?
    player_h:   .byte ?
    player_vx:  .byte ?
    player_vy:  .byte ?
    player_vh:  .byte ?
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

    lda #$2e
    sta player_h
    lda #$9c
    sta player_x
    lda #$a2
    sta player_y
    lda #0
    sta player_vx
    sta player_vy
    sta player_vh

-
    jsr cls
    jsr moveplayer
    jsr render
    jsr draw_status
    jsr redraw
    jmp -

; --- Screen draw -----------------------------------------------------------

; Draw stuff on the top line.

draw_status:
    .block
        lda #<backbuffer
        sta ptr+0
        lda #>backbuffer
        sta ptr+1

        ldy #0
        lda player_h
        jsr drawbyte

        iny
        lda player_x
        jsr drawbyte

        iny
        lda player_y
        jsr drawbyte

        iny
        lda player_vx
        jsr drawbyte

        iny
        lda player_vy
        jsr drawbyte

        iny
        lda player_vh
        jsr drawbyte

        rts

    drawbyte:
        pha
        lsr
        lsr
        lsr
        lsr
        jsr drawnibble
        pla
        ; fall through
    drawnibble:
        and #$0f
        tax
        lda hex_table, x
        sta (ptr), y
        iny
        rts

        .section zp
            ptr: .word ?
        .send
    .bend
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

; On entry: x=x coord, y=height, a=char
vline:
    .block
        sta char

        tya
        cmp #20
        bcc +
        lda #20
    +
        sta height

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
        lda #40
        sta column
    column_loop:
        dec column

        ; Calculate ray direction.

        lda column
        bpl +
        rts                     ; break out of loop if no more columns
   +
        sec
        sbc #20
        clc
        adc player_h
        sta raydir

        ; Calculate map location.

        lda player_x
        and #$f0                ; truncate
        sta mx

        lda player_y
        and #$f0                ; truncate
        sta my

        ; Initialise DDA.

        ldx raydir
        lda deltadistx_table, x
        sta deltadistx
        lda deltadisty_table, x
        sta deltadisty

        ; sidedistx

        .block
            ldx raydir
            lda dirx_table, x
            bpl do_pos_x

        do_neg_x:
            lda player_x
            and #$0f            ; fractional part only
            tax
            lda deltadistx
            jsr mul_8x8_8f
            sta sidedistx

            lda #-$10
            jmp exit

        do_pos_x:
            lda player_x
            and #$0f            ; fractional part only
            tax
            sec
            lda #$10            ; 1.0
            sbc identity_table, x
            tax
            lda deltadistx
            jsr mul_8x8_8fs
            sta sidedistx

            lda #$10
        exit:
            sta stepx
        .bend

        ; sidedisty

        .block
            ldx raydir
            lda diry_table, x
            bpl do_pos_y

        do_neg_y:
            lda player_y
            and #$0f            ; fractional part only
            tax
            lda deltadisty
            jsr mul_8x8_8f
            sta sidedisty

            lda #-$10
            jmp exit

        do_pos_y:
            lda player_y
            and #$0f            ; fractional part only
            tax
            sec
            lda #$10            ; 1.0
            sbc identity_table, x
            tax
            lda deltadisty
            jsr mul_8x8_8fs
            sta sidedisty

            lda #$10
        exit:
            sta stepy
        .bend

        ; Now actually do the DDA.

        .block
        loop:
            ; set side; 0 for x, 1 for y

            lda sidedistx
            cmp sidedisty
            lda #0
            rol
            tax

            ; do one DDA step

            clc
            lda sidedistx, x
            adc deltadistx, x
            bcs overflow            ; too far, give up
            sta sidedistx, x
            clc
            lda mx, x
            adc stepx, x
            sta mx, x

            ; check the map

            lda mx
            lsr
            lsr
            lsr
            lsr
            ora my
            tay
            lda map_table, y
            beq loop
        .bend

        ; Compute the intersection distance.

        .block
            sec
            lda player_x, x
            sbc mx, x               ; pos - map
            tay

            sec
            lda #$10                ; 1.0
            sbc stepx, x            ; 1-step
            lsr                     ; /2
            
            sec
            sbc identity_table, y   ; (1-step)/2 - (pos-map)
            tay

            txa                     ; set flags
            sta side
            beq +

            clc
            lda raydir
            adc #$40
            jmp do_multiply
        +
            lda raydir
        do_multiply:
            tax
            lda inv_sincos_table, x
            tax
            tya
            jsr mul_8x8_8fs         ; signed multiply
            sta distance
        .bend

        ; Compute the 'texture' coordinate.

        .block
            tax
            ldy raydir
            lda side
            bne side_y

            lda diry_table, y
            jsr mul_8x8_8fs
            clc
            adc player_y
            jmp exit

        side_y:
            lda dirx_table, y
            jsr mul_8x8_8fs
            clc
            adc player_x

        exit:
            ldx #0
            and #$08                ; fractional part
            bne +
            inx
        +
            stx texx
        .bend

    draw:
        ldx distance
        ldy height_table, x
        lda texx
        asl
        ora side
        tax
        lda textures_table, x
        tax
        ldx column
        jsr vline

        jmp column_loop

    ; Looking diagonally across a 16x16 grid produces a distance which is too
    ; big to fit into our number representation, so we just give up.

    overflow:
        lda #$ff
        jmp draw

        .section zp
            column:     .byte ?
            raydir:     .byte ?
            mx:         .byte ?
            my:         .byte ?
            deltadistx: .byte ?
            deltadisty: .byte ?
            sidedistx:  .byte ?
            sidedisty:  .byte ?
            stepx:      .byte ?
            stepy:      .byte ?
            side:       .byte ?
            distance:   .byte ?
            texx:       .byte ?
        .send

        textures_table:
            .byte 32+128
            .byte 92+128
            .byte 102
            .byte 92
    .bend

; --- Handle player motion --------------------------------------------------

moveplayer:
    .block
        ; Move player according to current velocity.

        clc
        lda player_x
        adc player_vx
        sta player_x

        clc
        lda player_y
        adc player_vy
        sta player_y

        ; Velocity decays each tick.

        lda player_vx
        cmp #$ff
        bne +
        lda #0
    +
        cmp #$80
        ror
        sta player_vx

        lda player_vy
        cmp #$ff
        bne +
        lda #0
    +
        cmp #$80
        ror
        sta player_vy

        ; Apply rotational velocity.

        clc
        lda player_h
        adc player_vh
        sta player_h

        ; Rotational velocity decays too.

        lda player_vh
        cmp #$ff
        bne +
        lda #0
    +
        cmp #$80
        ror
        sta player_vh

        ; Process keys.

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

        lda #7
        sta PIA1_PA
        lda PIA1_PB
        and #%00001000
        beq comma_pressed

        lda #6
        sta PIA1_PA
        lda PIA1_PB
        and #%00001000
        beq dot_pressed

        rts

    w_pressed:
        lda player_h
        jmp accelerate

    s_pressed:
        lda player_h
        clc
        adc #$80
        jmp accelerate

    a_pressed:
        lda player_h
        sec
        sbc #$40
        jmp accelerate

    d_pressed:
        lda player_h
        clc
        adc #$40
        jmp accelerate

    dot_pressed:
        clc
        lda player_vh
        adc #$03
        sta player_vh
        rts

    comma_pressed:
        sec
        lda player_vh
        sbc #$03
        sta player_vh
        rts

    accelerate:
        tax

        lda dirx_table, x
        cmp #$80
        ror
        cmp #$80
        ror
        cmp #$80
        ror
        adc player_vx
        sta player_vx

        lda diry_table, x
        cmp #$80
        ror
        cmp #$80
        ror
        cmp #$80
        ror
        adc player_vy
        sta player_vy
        rts
    .bend

; --- Maths -----------------------------------------------------------------

; Fast multiplication routines from
; https://codebase64.org/doku.php?id=base:seriously_fast_multiplication.

; Computes AY = A*X, unsigned; preserves X. Note that the result is 16 bits
; wide.
mul_8x8_16:
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

; Computes A = A*X unsigned, where all numbers are (optionally signed) fixeds.
mul_8x8_8f:
    .block
        jsr mul_8x8_16
        sty lo
        asl lo
        rol
        asl lo
        rol
        asl lo
        rol
        asl lo
        rol
        rts

        .section zp
            lo: .byte ?
        .send
    .bend

; Computes A = A*X signed, where all numbers are (optionally signed) fixeds.
mul_8x8_8fs:
    .block
        sta lhs
        stx rhs
        jsr mul_8x8_16
        sta hi
        sty lo

        bit lhs
        bpl +
        sec
        lda hi
        sbc rhs
        sta hi
    +

        bit rhs
        bpl +
        sec
        lda hi
        sbc lhs
        sta hi
    +

        ; hi already in A
        asl lo
        rol
        asl lo
        rol
        asl lo
        rol
        asl lo
        rol
        rts

        .section zp
            lhs: .byte ?
            rhs: .byte ?
            lo: .byte ?
            hi: .byte ?
        .send
    .bend

map_table:
    .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    .byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1
    .byte 1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1
    .byte 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1
    .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

hex_table:
    .text "0123456789ABCDEF"

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
        .byte >(((i-255)*(i-255))/4)
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

height_table:
    .for i := 0, i < 256, i += 1
        f := i / 16.0
        h := $ff
        .if i != 0
            h := 10.0 * 1.0/f
        .endif
        .if h < 1
            h := 1
        .endif
        .byte h
    .next

sin_table:
cos_table = sin_table + 64
    .for i := 0, i < 256+64, i += 1
        .char 15.99 * sin(torad(i))
    .next

dirx_table = sin_table
diry_table = cos_table

inv_sincos_table:
    .for i := 0, i < 256, i += 1
        .char 16.0 * clamp(div(1.0, sin(torad(i))), -7.99, 7.99)
    .next

deltadistx_table:
deltadisty_table = deltadistx_table + 64
    .for i := 0, i < 256+64, i += 1
        .char 16.0 * clamp(abs(div(1, sin(torad(i)))), -7.99, 7.99)
    .next

.align $100
backbuffer:
    .fill 1024, ?

    * = 0
    .dsection zp
    .cerror * > $ff

; vim: sw=4 ts=4 et

