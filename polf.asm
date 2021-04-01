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
    ticks:      .byte ?

    player_x:   .byte ?
    player_y:   .byte ?
    player_h:   .byte ?
    player_vx:  .byte ?
    player_vy:  .byte ?
    player_vh:  .byte ?

    object_x:   .byte ?
    object_y:   .byte ?
    object_vx:  .byte ?
    object_vy:  .byte ?
    object_dh:  .byte ?
    object_d:   .byte ?

    hole_x:     .byte ?
    hole_y:     .byte ?
    hole_dh:    .byte ?
    hole_d:     .byte ?

    x1:         .byte ?
    y1:         .byte ?
    x2:         .byte ?
    y2:         .byte ?

    level_size: .byte ?
    level_seed: .byte ?
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
    sta level_seed
    lda #9
    sta level_size
    jsr create_level

    lda #$00
    sta player_h
    lda #0
    sta player_vx
    sta player_vy
    sta player_vh
    sta object_vx
    sta object_vy

-
    dec ticks

    jsr cls
    jsr moveplayer
    jsr render
    jsr move_object
    jsr move_hole

    lda object_d
    cmp hole_d
    bcs +
    jsr draw_object
    jsr draw_hole
    jmp ++
+
    jsr draw_hole
    jsr draw_object
+

    jsr draw_status
    jsr redraw
    jmp -

; Tests to see if (X, A) is on a wall. Preserves Y. Returns Z or !Z.

test_wall:
    .block
        and #$f0
        sta offset

        txa
        lsr
        lsr
        lsr
        lsr
        ora offset
        tax

        lda map_table, x
        rts

        .section zp
            offset: .byte ?
        .send
    .bend

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

        iny
        lda object_vx
        jsr drawbyte

        iny
        lda object_vy
        jsr drawbyte

        iny
        lda object_dh
        jsr drawbyte

        iny
        lda object_d
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
            stx side
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

            lda sideoffset_table, x
            clc
            adc raydir
            tax
            lda inv_sincos_table, x
            tax
            tya
            jsr mul_8x8_8fs         ; signed multiply
            sta distance
            ldx column
            sta zbuffer, x
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
            and #$08
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
        ldx column
        sta zbuffer, x
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

        sideoffset_table:
            .byte $40
            .byte 0
    .bend

; --- Object handling -------------------------------------------------------

move_object:
    .block
        ; Calculate view angle of object.

        lda object_x
        ldy object_y

        jsr arctan2
        sta object_dh           ; also sets x1 and y1

        ; Calculate real distance of object.

        lda x1
        jsr square
        sta distance+1
        sty distance+0

        lda y1
        jsr square
        tax
        tya
        clc
        adc distance+0
        sta distance+0
        txa
        adc distance+1
        ldx distance+0
        jsr sqrt16
        sta object_d            ; real distance

        ; Grab mechanic: pressing SPACE lets you drag the object with you.

        cmp #$10
        bcs +
        lda #8
        sta PIA1_PA
        lda PIA1_PB
        and #%00000100
        bne +

        lda player_vx
        sta object_vx
        lda player_vy
        sta object_vy
        jmp nobump
    +

        ; Close enough that the player's colliding with it?

        lda object_d
        cmp #$08
        bcs nobump

        ; Normalise the collision vector in x1/y1.

        ldx object_d
        lda inverse_table, x
        ldx x1
        jsr mul_8x8_8f
        sta x1

        ldx object_d
        lda inverse_table, x
        ldx y1
        jsr mul_8x8_8f
        sta y1

        ; Calculate dot product of player velocity vector with the collision vector.

        lda x1
        ldx player_vx
        jsr mul_8x8_8fs
        sta dotproduct

        lda y1
        ldx player_vy
        jsr mul_8x8_8fs
        clc
        adc dotproduct
        sta dotproduct
        bpl nobump              ; don't push if object is departing

        ; Scale the dot product to something sensible.

        cmp #$80
        ror
        cmp #$f1
        bcc nobump
        sta dotproduct

        ; Now scale the collision vector and apply to the object velocity
        ; vector.

        ldx x1
        jsr mul_8x8_8fs
        clc
        adc object_vx
        sta object_vx

        lda dotproduct
        ldx y1
        jsr mul_8x8_8fs
        clc
        adc object_vy
        sta object_vy

        ; As an optimisation, just stop the player dead.

        lda #0
        sta player_vx
        sta player_vy

    nobump:

        ; Apply motion (will take effect next frame).

        clc
        lda object_x
        tay
        adc object_vx
        sta object_x
        tax
        lda object_y
        jsr test_wall
        beq +
        sty object_x            ; if collision, bounce in X axis
        lda object_vx
        eor #$ff
        sta object_vx
    +

        clc
        lda object_y
        tay
        adc object_vy
        sta object_y
        ldx object_x
        jsr test_wall
        beq +
        sty object_y            ; if collision, bounce in Y axis
        lda object_vy
        eor #$ff
        sta object_vy
    +

        ; Diminish the object's velocity.

        lda ticks
        and #7
        bne noslow

        ldx #1
        lda object_vx
        beq ++
        bmi +
        ldx #-1
    +
        clc
        adc identity_table, x
        sta object_vx
    +

        ldx #1
        lda object_vy
        beq ++
        bmi +
        ldx #-1
    +
        clc
        adc identity_table, x
        sta object_vy
    +
    noslow:
        rts

        .section zp
            distance: .word ?
            dotproduct: .byte ?
            p: .byte ?
        .send
    .bend

draw_object:
    .block

        ; Prepare for drawing.

        sec
        lda object_dh
        sbc player_h
        clc
        adc #20
        sta column

        ldx object_d
        lda height_table, x     ; height of object
        sta width
        sta height

        lsr                     ; half the size
        tax
        sec
        lda column
        sbc identity_table, x   ; adjust column to LHS of sprite

    loop:
        ldx column
        cpx #40
        bcs invisible
        lda object_d
        cmp zbuffer, x          ; check zbuffer to see if this is drawable
        bcs invisible
        sta zbuffer, x          ; update zbuffer

        lda #0
        ldy height
        jsr vline

    invisible:
        inc column
        dec width
        bne loop

        rts

        .section zp
            column: .byte ?
            width: .byte ?
            height: .byte ?
        .send
   .bend

; --- Hole handline ---------------------------------------------------------

move_hole:
    .block
        ; Calculate view angle of hole.

        lda hole_x
        ldy hole_y

        jsr arctan2
        sta hole_dh           ; also sets x1/y1

        ; Calculate real distance of hole.

        lda x1
        jsr square
        sta distance+1
        sty distance+0

        lda y1
        jsr square
        tax
        tya
        clc
        adc distance+0
        sta distance+0
        txa
        adc distance+1
        ldx distance+0
        jsr sqrt16
        sta hole_d            ; real distance

        rts

        .section zp
            distance: .word ?
            dotproduct: .byte ?
        .send
    .bend

draw_hole:
    .block

        ; Prepare for drawing.

        sec
        lda hole_dh
        sbc player_h
        clc
        adc #20
        sta column

        ldx hole_d
        lda height_table, x     ; height of hole
        sta width
        sta height

        lsr                     ; half the size
        tax
        sec
        lda column
        sbc identity_table, x   ; adjust column to LHS of sprite

    loop:
        ldx column
        cpx #40
        bcs invisible
        lda hole_d
        cmp zbuffer, x          ; check zbuffer to see if this is drawable
        bcs invisible
        sta zbuffer, x          ; update zbuffer

        lda #1
        ldy height
        jsr vline

    invisible:
        inc column
        dec width
        bne loop

        rts

        .section zp
            column: .byte ?
            width: .byte ?
            height: .byte ?
        .send
   .bend

; --- Handle player motion --------------------------------------------------

moveplayer:
    .block
        ; Move player according to current velocity.

        clc
        lda player_x
        tay
        adc player_vx
        sta player_x
        tax
        lda player_y
        jsr test_wall
        beq +
        sty player_x
    +

        clc
        lda player_y
        tay
        adc player_vy
        sta player_y
        ldx player_x
        jsr test_wall
        beq +
        sty player_y
    +

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
        and #%00000001
        bne +

        lda player_h                ; A pressed
        sec
        sbc #$40
        jsr accelerate
    +

        lda PIA1_PB
        and #%00000010
        bne +

        lda player_h                ; D pressed
        clc
        adc #$40
        jsr accelerate
    +

        lda #4
        sta PIA1_PA
        lda PIA1_PB
        and #%00000010
        bne +

        lda player_h                ; W pressed
        jsr accelerate
    +

        lda #2
        sta PIA1_PA
        lda PIA1_PB
        and #%00000010
        bne +

        lda player_h                ; S pressed
        clc
        adc #$80
        jsr accelerate
    +
        
        lda #7
        sta PIA1_PA
        lda PIA1_PB
        and #%00001000
        bne +

        sec                         ; comma pressed
        lda player_vh
        sbc #$03
        sta player_vh
    +

        lda #6
        sta PIA1_PA
        lda PIA1_PB
        and #%00001000
        bne +

        clc                         ; dot pressed
        lda player_vh
        adc #$03
        sta player_vh
    +
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

; --- Level creation --------------------------------------------------------

; Creates a maze based on level_size and level_seed.  The state used in each
; cell is the offset of the parent. Offsets $00, $01 and $02 are special,
; representing a normal wall, an impassable wall, and an empty block
; respectively.

create_level:
    .block
        ; Fill the maze with impassable walls.

        ldx #0
        lda #$01
    -
        sta map_table, x
        dex
        bne -

        ; Now fill the area that the maze will occupy.

        ldx #16+1

        ldy level_size
        dey
        dey
        sty count

        lda #$01
    outer_loop:
        ldy level_size
        dey
        dey
        lda #$00
    inner_loop:
        sta map_table, x
        inx
        dey
        bne inner_loop

        txa                     ; advance to next row
        clc
        and #$f0
        adc #$11
        tax

        dec count
        bne outer_loop
        
        ; Initialise recursion.

        ldy #16+1
        sty co
        lda #$02                ; mark the starting point
        sta map_table, y
        lda level_seed
        sta seed

        ; Begin random walk.

    walk_loop:
        jsr shuffled            ; pick a starting direction
        tay
        lda #4                  ; number of iterations
        sta count       
    dir_loop:
        tya
        and #$03
        tay
        lda direction_table, y
        clc
        adc co
        sta cop1                ; pos + 1
        tax
        clc
        adc direction_table, y
        sta cop2                ; pos + 2

        lda map_table, x
        cmp #$01
        beq next_dir            ; don't pass through impassable walls

        ldx cop2
        lda map_table, x
        bne next_dir            ; only go in directions we haven't been

        lda co
        sta map_table, x        ; mark new location and store breadcrumb

        lda #$02
        ldx cop1
        sta map_table, x        ; clear intermediate wall

        lda cop2
        sta co                  ; move to new location

        jmp walk_loop           ; start walking again

    next_dir:
        iny                     ; can't go this direction, so try another
        dec count
        bne dir_loop            ; give up if we've done all directions

        ldx co
        lda map_table, x        ; fetch breadcrumb to previous location
        sta co
        cmp #$02
        bne walk_loop           ; if we haven't reached the end, back up

        ; We've created a perfect maze with no loops; this is annoying and
        ; boring, so punch small holes in it.

        ldy level_size
    hole_loop:
        jsr shuffled            ; fetch a random position
        tax
        lda map_table, x        ; look for a clearable wall ($00)
        bne hole_loop

        lda #$02
        sta map_table, x
        dey
        bne hole_loop

        ; Place the player, object and hole.

        jsr find_hole
        stx co                  ; player

    -
        jsr find_hole           ; object
        cpx co
        beq -
        stx cop1

    -
        jsr find_hole           ; hole
        cpx co
        beq -
        cpx cop1
        beq -
        stx cop2

        lda co
        jsr convert
        stx player_x
        sty player_y

        lda cop1
        jsr convert
        stx object_x
        sty object_y

        lda cop2
        jsr convert
        stx hole_x
        sty hole_y

        ; We should now have a working maze. Go through and convert into the
        ; form the actual game wants.

        ldx #0
    -
        lda #0
        ldy map_table, x
        cpy #2                  ; < $02 is a wall
        bcs +
        lda #1
    +
        sta map_table, x
        dex
        bne -

        rts

    find_hole:
        jsr shuffled
        tax
        lda map_table, x
        cmp #2
        bcc find_hole
        rts

    convert:
        pha
        and #$0f
        asl
        asl
        asl
        asl
        ora #$08
        tax

        pla
        and #$f0
        ora #$08
        tay
        rts

    direction_table:
        .char -16
        .char 1
        .char 16
        .char -1

        .section zp
            co: .byte ?
            cop1: .byte ?
            cop2: .byte ?
            count: .byte ?
        .send
    .bend

draw_level:
    .block
        lda #<(backbuffer-1)
        sta sptr+0
        lda #>(backbuffer-1)
        sta sptr+1
        lda #<(map_table-1)
        sta mptr+0
        lda #>(map_table-1)
        sta mptr+1

        ldx #16
        stx row
    outer_loop:
        ldy #16
    inner_loop:
        lda (mptr), y
        tax
        lda map_char_table, x
        sta (sptr), y
        dey
        bne inner_loop

        clc
        lda sptr+0
        adc #40
        sta sptr+0
        bcc +
        inc sptr+1
    +

        clc
        lda mptr+0
        adc #16
        sta mptr+0
        bcc +
        inc mptr+1
    +

        dec row
        bne outer_loop
        rts

    map_char_table:
        .byte 32        ; space
        .byte 102 ; $5f       ; fill

        .section zp
            row:  .byte ?
            mptr: .word ?
            sptr: .word ?
        .send
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

; Computes AY = A, unsigned.
square:
    .block
        tax                     ; set flags
        bpl +
        eor #$ff                ; approximate abs
    +
        tax
        jmp mul_8x8_16
    .bend

; Computes A = A*X unsigned, where all numbers are unsigned fixeds.
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

; Computes A = A*X signed, where all numbers are signed fixeds.
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

; Super-fast arctan2 code without division (!), taken from here:
; https://codebase64.org/doku.php?id=base:8bit_atan2_8-bit_angle
;
; Computes A = arctan2(Y-player_y, A-player_x)
arctan2:
    .block
        sec
        tax
        lda player_x
        sbc identity_table, x
        sta x1
        bcs +
        eor #$ff
    +
        tax
        rol octant

        sec
        lda player_y
        sbc identity_table, y
        sta y1
        bcs +
        eor #$ff
    +
        tay
        rol octant

        sec
        lda log2_table, x
        sbc log2_table, y
        bcc +
        eor #$ff
    +
        tax

        lda octant
        rol
        and #%111
        tay

        lda atan_table, x
        eor octant_adjust_table, y
        rts

    octant_adjust_table:
        .byte %00111111     ; x+,y+,|x|>|y|
        .byte %00000000     ; x+,y+,|x|<|y|
        .byte %11000000     ; x+,y-,|x|>|y|
        .byte %11111111     ; x+,y-,|x|<|y|
        .byte %01000000     ; x-,y+,|x|>|y|
        .byte %01111111     ; x-,y+,|x|<|y|
        .byte %10111111     ; x-,y-,|x|>|y|
        .byte %10000000     ; x-,y-,|x|<|y|

        .section zp
            octant: .byte ?
        .send
    .bend

; Medium fast 16-bit square root, taken from here:
; https://codebase64.org/doku.php?id=base:16bit_and_24bit_sqrt
;
; Computes A = sqrt(AX).

sqrt16:
    .block
        sta hi
        stx lo

        ldy #1
        sty t1
        dey
        sty t2
    again:
        sec
        lda lo
        tax
        sbc t1
        sta lo

        lda hi
        sbc t2
        sta hi

        bcs continue
        tya
        rts

    continue:
        iny
        lda t1
        adc #1
        sta t1
        bcc again
        inc t2
        jmp again

        .section zp
            lo: .byte ?
            hi: .byte ?
            t1: .byte ?
            t2: .byte ?
        .send
    .bend

; Produce a shuffled number: if you call this 256 times, you get all 256 values.
; Returns the value in A; preserves X and Y.

shuffled:
    .block
        lda seed
        beq do_eor
        asl
        beq no_eor
        bcc no_eor
    do_eor:
        eor #$1d
    no_eor:
        sta seed
        rts
    .bend

    .section zp
        seed: .byte ?
    .send

; As above, but produces a number less than A. This is not good code.

shuffled_limited:
    .block
        clc
        adc #1
        sta max
    -
        jsr shuffled
        cmp max
        bcs -
        rts

        .section zp
            max: .byte ?
        .send
    .bend

hex_table:
    .text "0123456789ABCDEF"

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

nround .function x
    .endf trunc(x + 0.5)

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

inverse_table:
    .byte $ff
    .for i := 1, i < 256, i += 1
        f := i / 16.0
        .byte clamp(nround(16.0 / f), 0, 255.0)
    .next

atan_table:
    ; This is supposed to be atan(2^(x/32))*128/pi, but I can't make that
    ; reproduce the numbers here, so it's hard-coded.

    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01
    .byte $01,$01,$01,$01,$01,$02,$02,$02
    .byte $02,$02,$02,$02,$02,$02,$02,$02
    .byte $02,$02,$02,$02,$02,$02,$02,$02
    .byte $03,$03,$03,$03,$03,$03,$03,$03
    .byte $03,$03,$03,$03,$03,$04,$04,$04
    .byte $04,$04,$04,$04,$04,$04,$04,$04
    .byte $05,$05,$05,$05,$05,$05,$05,$05
    .byte $06,$06,$06,$06,$06,$06,$06,$06
    .byte $07,$07,$07,$07,$07,$07,$08,$08
    .byte $08,$08,$08,$08,$09,$09,$09,$09
    .byte $09,$0a,$0a,$0a,$0a,$0b,$0b,$0b
    .byte $0b,$0c,$0c,$0c,$0c,$0d,$0d,$0d
    .byte $0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f
    .byte $10,$10,$10,$11,$11,$11,$12,$12
    .byte $12,$13,$13,$13,$14,$14,$15,$15
    .byte $15,$16,$16,$17,$17,$17,$18,$18
    .byte $19,$19,$19,$1a,$1a,$1b,$1b,$1c
    .byte $1c,$1c,$1d,$1d,$1e,$1e,$1f,$1f

log2_table:
    .byte 0
    .for i := 1, i < 256, i += 1
        .byte clamp(nround(log(i)*32.0 / log(2)), 0, 255)
    .next

sin_table:
cos_table = sin_table + 64
    .for i := 0, i < 256+64, i += 1
        .char clamp(nround(16.0 * sin(torad(i))), -127, 127)
    .next

dirx_table = cos_table
diry_table = sin_table

inv_sincos_table:
    .for i := 0, i < 256, i += 1
        .char clamp(nround(16.0 * div(1.0, sin(torad(i)))), -127, 127)
    .next

deltadisty_table:
deltadistx_table = deltadisty_table + 64
    .for i := 0, i < 256+64, i += 1
        .char clamp(nround(16.0 * abs(div(1, sin(torad(i))))), -127, 127)
    .next

.section zp
zbuffer:
    .fill 40, ?
.send

.align $100
backbuffer:
    .fill 1024, ?
map_table:
    .fill 16*16, ?

    * = 0
    .dsection zp
    .cerror * > $ff

; vim: sw=4 ts=4 et

