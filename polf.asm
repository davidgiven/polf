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

FACTOR_BITS = 5
FACTOR = 1.0<<FACTOR_BITS
INT_BITS = 8 - FACTOR_BITS
INT_MASK = ($ff << FACTOR_BITS) & $ff
FRAC_MASK = ~INT_MASK

MAP_BITS = INT_BITS
MAP_ROW_SIZE = 1<<MAP_BITS
MAP_ROW_MASK = ($ff<<MAP_BITS) & $ff
MAP_COLUMN_MASK = ~MAP_ROW_MASK

SCREEN = $8000

shift .macro insn, n
    .rept \n
        \insn
    .next
    .endm
    
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
    level:      .byte ?
    level_complete: .byte ?

    space:      .byte ?
    push:       .byte ?

map_table:
    .fill MAP_ROW_SIZE*MAP_ROW_SIZE, ?
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
    sei                         ; entire program runs with interrupts off

    ; Title screen sequence.

    lda #' ' | $80
    jsr fill_screen
    jsr redraw_slow

    lda #14
    sta VIA_PCR

    jsr cls
    jsr redraw_slow
    jsr redraw_slow
    jsr draw_title_screen
    jsr redraw_slow
    jsr spacebar

    ; Initialise.

    lda #0
    sta level
    lda #8
    sta level_size

level_loop:
    jsr cls
    jsr redraw_slow
-
    jsr cls
    jsr draw_level_banner
    jsr redraw
    jsr create_level
    jsr draw_level
    jsr draw_spacebar_prompt
    jsr redraw
    jsr select_level
    bcs -

    jsr cls
    jsr redraw
    jsr redraw_slow

    lda #$80
    sta player_h
    lda #0
    sta player_vx
    sta player_vy
    sta player_vh
    sta object_vx
    sta object_vy
    sta push
    sta space
    sta level_complete

    ;lda #$36
    ;sta player_x
    ;lda #$bf
    ;sta player_y
    ;lda #$bd
    ;sta player_h
    
    jsr render
    jsr draw_moving_objects
    jsr draw_status
    jsr redraw_slow

-
    dec ticks

    jsr cls
    jsr moveplayer
    jsr render
    jsr draw_moving_objects
    jsr draw_status
    jsr redraw

    lda level_complete
    beq -

    jsr next_level
    jmp level_loop

draw_moving_objects:
    jsr move_object
    jsr move_hole

    lda object_d
    cmp hole_d
    bcs +
    jsr draw_object
    jmp draw_hole
+
    jsr draw_hole
    jmp draw_object

; Tests to see if (X, A) is on a wall. Preserves Y. Returns Z or !Z.

test_wall:
    .block
        and #INT_MASK
        sta offset

        txa
        shift lsr, INT_BITS
        ora offset
        shift lsr, FACTOR_BITS-INT_BITS
        tax

        lda map_table, x
        rts

        .section zp
            offset: .byte ?
        .send
    .bend

; Waits for the spacebar to be pressed.

spacebar:
    .block
        lda #8
        sta PIA1_PA
    -
        lda PIA1_PB
        and #%00000100
        beq -                   ; wait for release

    -
        lda PIA1_PB
        and #%00000100
        bne -                   ; wait for press

        rts
    .bend

wait_for_release:
    lda PIA1_PB
    cmp #$ff
    bne wait_for_release
    rts

; --- Screen draw -----------------------------------------------------------

; Draw stuff on the top line.

draw_status:
    .block
        ; Draw the compass line.

        lda player_h
        clc
        adc #20
        tax
        ldy #40

    -
        lda compass_table, x
        sta (backbuffer+22*40)-1, y
        lda #$40
        sta backbuffer-1, y
        sta (backbuffer+21*40)-1, y
        dex
        dey
        bne -

        ; Draw the top and bottom borders.

        ldx #110
        stx backbuffer+19
        ldx #112
        stx backbuffer+20
        ldx #125
        stx backbuffer+19+21*40
        ldx #109
        stx backbuffer+20+21*40

        ; Draw the status labels.

        ldx #$fb
    -
        lda ball_string-$fb, x
        sta backbuffer+0+24*40-$fb, x
        lda hole_string-$fb, x
        sta backbuffer+35+24*40-$fb, x
        inx
        bne -

        ; Draw the object distance meter.

        lda object_d
        eor #$ff
        lsr
        lsr
        lsr
        lsr
        sta ptr+0
        ldx #0
        lda #' ' | $80
    -
        sta backbuffer+5+24*40-1, x
        inx
        cpx ptr+0
        bne -

        lda object_d
        eor #$ff
        lsr
        and #$07
        tay
        lda scale_table, y
        sta backbuffer+5+24*40-1, x

        ; Draw the hole distance meter.

        lda hole_d
        eor #$ff
        lsr
        lsr
        lsr
        lsr
        tay
        sta ptr+0
        lda #16
        sec
        sbc ptr+0
        sta ptr+0
        tax
        lda #' ' | $80
    -
        sta backbuffer+20+24*40, x
        inx
        dey
        bne -

        lda hole_d
        lsr
        and #$07
        tay
        ldx ptr+0
        lda scale_table, y
        eor #$80
        sta backbuffer+19+24*40, x
        rts
    .bend

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
    ora (ptr), y
    sta (ptr), y
    iny
    rts

    .section zp
        ptr: .word ?
    .send

draw_spacebar_prompt:
    .block
        address = backbuffer + 20*40 + 20 - (spacebar_string_size/2)

        ldx #spacebar_string_size-1
    -
        lda #' ' | $80
        sta address+0*40, x
        sta address+2*40, x
        lda spacebar_string, x
        sta address+1*40, x
        dex
        bpl -
        rts
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

redraw_slow:
    .block

    copy .macro
        lda backbuffer+\1, x
        sta SCREEN+\1, x
        .endm

        lda #0
        sta count
    loop:
    -
        lda VIA_PB
        and #%00100000
        bne -
    -
        lda VIA_PB
        and #%00100000
        beq -

        ldy #10
    -
        jsr shuffled
        tax
        copy $000
        copy $100
        copy $200
        copy $300
        dec count
        beq exit
        dey
        bne -
        jmp loop
    exit:
        rts

        .section zp
            count: .byte ?
        .send
    .bend

draw_title_screen:
    .block

    copy .macro
        lda titlescreen_table+\1, x
        sta backbuffer+\1, x
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
fill_screen:
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
        lda #22
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
        and #INT_MASK
        sta mx

        lda player_y
        and #INT_MASK
        sta my

        ; Initialise DDA.

        ldx raydir
        lda deltadistx_table, x
        sta deltadistx
        lda deltadisty_table, x
        sta deltadisty
        lda #0
        sta sidedistx_hi
        sta sidedisty_hi

        ; sidedistx

        .block
            ldx raydir
            lda dirx_table, x
            bpl do_pos_x

        do_neg_x:
            lda player_x
            and #FRAC_MASK
            tax
            lda deltadistx
            cmp #$ff
            beq +                   ; work around division by zero case
            jsr mul_8x8_8f
        +
            sta sidedistx_lo

            lda #-1.0*FACTOR
            jmp exit

        do_pos_x:
            lda player_x
            and #FRAC_MASK
            tax
            sec
            lda #1.0*FACTOR
            sbc identity_table, x
            tax
            lda deltadistx
            cmp #$ff
            beq +                   ; work around division by zero case
            jsr mul_8x8_8f
        +
            sta sidedistx_lo

            lda #1.0*FACTOR
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
            and #FRAC_MASK
            tax
            cmp #$ff
            lda deltadisty
            beq +                   ; work around division by zero case
            jsr mul_8x8_8f
        +
            sta sidedisty_lo

            lda #-1.0*FACTOR
            jmp exit

        do_pos_y:
            lda player_y
            and #FRAC_MASK
            tax
            sec
            lda #1.0*FACTOR
            sbc identity_table, x
            tax
            lda deltadisty
            cmp #$ff
            beq +                   ; work around division by zero case
            jsr mul_8x8_8f
        +
            sta sidedisty_lo

            lda #1.0*FACTOR
        exit:
            sta stepy
        .bend

        ; Now actually do the DDA.

        .block
        loop:
            ; set side; 0 for x, 1 for y

            ldx #1
            lda sidedistx_hi
            cmp sidedisty_hi
            bcc is_side_x
            bne is_side_y
            lda sidedistx_lo
            cmp sidedisty_lo
            bcs is_side_y
        is_side_x:
            dex
        is_side_y:

            ; do one DDA step

            clc
            lda sidedistx_lo, x
            adc deltadistx, x
            sta sidedistx_lo, x
            bcc +
            inc sidedistx_hi, x
        +

            clc
            lda mx, x
            adc stepx, x
            sta mx, x

            ; check the map

            lda mx
            shift lsr, INT_BITS
            ora my
            shift lsr, FACTOR_BITS-MAP_BITS
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
            lda #1.0*FACTOR
            sbc stepx, x            ; 1-step
            lsr                     ; /2
            
            sec
            sbc identity_table, y   ; (1-step)/2 - (pos-map)
            bpl +
            eor #$ff                ; approximate absolute value
        +
            sta x1+0

            lda sideoffset_table, x
            clc
            adc raydir
            tay
            ldx inv_sincos_table_lo, y
            lda inv_sincos_table_hi, y
            ldy x1+0
            jsr mul_8x16_24
            tay                     ; set flags
            beq +
            ldx #$80                ; overflow
        +
            stx distance
            ldy column
            stx zbuffer, y
        .bend

        ; Compute the 'texture' coordinate.

        .block
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
            and #1<<(FACTOR_BITS-1)
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


        .section zp
            column:       .byte ?
            raydir:       .byte ?
            mx:           .byte ?
            my:           .byte ?
            deltadistx:   .byte ?
            deltadisty:   .byte ?
            sidedistx_lo: .byte ?
            sidedisty_lo: .byte ?
            sidedistx_hi: .byte ?
            sidedisty_hi: .byte ?
            stepx:        .byte ?
            stepy:        .byte ?
            side:         .byte ?
            distance:     .byte ?
            texx:         .byte ?
        .send

        textures_table:
            .byte 32+128
            .byte 102
            .byte 32+128
            .byte 102

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

        ; Close enough to be able to push the object?

        cmp #1.0*FACTOR
        bcs nopush

        ; Player pressed the spacebar?

        lda push
        beq nopush
        lda #0
        sta push

        ; Ideally we want to pretend the player is pushing a pool cue straight
        ; out in front of them, which hits the object and moves it. However,
        ; the maths to do this turns out to be surprisingly complicated, so
        ; instead we're going to push the object directly away from the player.

        ; Normalise the collision vector in x1/y1.

        ldx object_d
        lda inverse_table, x
        ldx x1
        jsr mul_8x8_8fs
        sta x1

        ldx object_d
        lda inverse_table, x
        ldx y1
        jsr mul_8x8_8fs
        sta y1

        ; Scale, and set the object velocity vector.

        lda #-0.1*FACTOR
        ldx x1
        jsr mul_8x8_8fs
        sta object_vx

        lda #-0.1*FACTOR
        ldx y1
        jsr mul_8x8_8fs
        sta object_vy

    nopush:

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
        sec
        lda #0
        sbc object_vx
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
        sec
        lda #0
        sbc object_vy
        sta object_vy
    +

        ; Diminish the object's velocity.

        lda ticks
        and #15
        bne noslow

    diminish_vx:
        lda object_vx
        beq diminish_vy
        bmi +

        dec object_vx
        jmp diminish_vy
    +
        inc object_vx
    diminish_vy:
        lda object_vy
        beq noslow
        bmi +

        dec object_vy
        rts
    +
        inc object_vy
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
        sta centre

        ; Set up Bresenham's circle algorithm.

        ldx object_d
        ldy height_table, x     ; diameter of object

        sec
        lda #3
        sbc identity_table, y
        sta cd

        lda #0
        sta cx

        tya
        lsr
        sty cy

        ; Draw the centre column.

        lda centre
        jsr do_draw

    loop:
        lda cx
        cmp cy
        bcs exit

        inc cx

        lda cd
        bmi d_neg
    d_pos:
        dec cy                  ; cy -= 1
        sec                     ; cd += 4*(x-y) + 10
        lda cx
        sbc cy
        asl
        asl
        clc
        adc cd
        clc
        adc #10
        sta cd
        jmp draw
    d_neg:
        lda cx                  ; cd += 4*x + 6
        asl
        asl
        clc
        adc cd
        clc
        adc #6
        sta cd
    draw:
        ; Near RHS

        clc
        lda centre
        adc cx
        jsr do_draw

        ; Near LHS

        sec
        lda centre
        sbc cx
        jsr do_draw

        jmp loop

    do_draw:
        cmp #40
        bcs exit
        tax

        lda object_d
        cmp zbuffer, x          ; check zbuffer to see if this is drawable
        bcc noexit
    exit:
        rts
    noexit:
        lda object_d
        sta zbuffer, x          ; update zbuffer

        lda cy
        ora #1
        tay

        lda #94
        jmp vline
        
        .section zp
            cx: .byte ?
            cy: .byte ?
            cd: .byte ?
            centre: .byte ?
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

        ; Calculate real distance from hole to object.

        sec
        lda object_x
        sbc hole_x
        bpl +
        eor #$ff                ; approximate negation negation
    +
        jsr square
        sta distance+0

        sec
        lda object_y
        sbc hole_y
        bpl +
        eor #$ff
    +
        jsr square
        clc
        adc distance+0
        beq +
        lda #1
    +
        eor #1
        sta level_complete
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

        lda ticks
        and #1
        beq +
        lda #10
    +
        clc
        adc #95
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
        sbc #$04
        sta player_vh
    +

        lda #6
        sta PIA1_PA
        lda PIA1_PB
        and #%00001000
        bne +

        clc                         ; dot pressed
        lda player_vh
        adc #$04
        sta player_vh
    +

        lda #8
        sta PIA1_PA
        lda PIA1_PB
        and #%00000100
        cmp space                   ; compare with old state
        beq +                       ; changed?

        sta space                   ; save new state
        tax                         ; set flags
        bne +                       ; pressed?

        lda #1                      ; trigger push
        sta push
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

; Creates a maze based on level_size and level. The state used in each
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
        inx
        cpx #(MAP_ROW_SIZE*MAP_ROW_SIZE) & $ff
        bne -

        ; Now fill the area that the maze will occupy.

        ldx #MAP_ROW_SIZE+1

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
        and #MAP_ROW_MASK
        adc #MAP_ROW_SIZE+1
        tax

        dec count
        bne outer_loop
        
        ; Initialise recursion.

        ldy #MAP_ROW_SIZE+1
        sty co
        lda #$02                ; mark the starting point
        sta map_table, y
        lda level
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

        lda level_size
        lsr
        tay
    hole_loop:
        lda #(MAP_ROW_SIZE*MAP_ROW_SIZE) & $ff
        jsr shuffled_limited    ; fetch a random position
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
        inx
        cpx #(MAP_ROW_SIZE*MAP_ROW_SIZE) & $ff
        bne -

        rts

    find_hole:
        lda #(MAP_ROW_SIZE*MAP_ROW_SIZE) & $ff
        jsr shuffled_limited
        tax
        lda map_table, x
        cmp #2
        bcc find_hole
        rts

    convert:
        pha
        and #MAP_COLUMN_MASK
        shift asl, FACTOR_BITS
        ora #1<<(FACTOR_BITS-1)
        tax

        pla
        and #MAP_ROW_MASK
        shift asl, FACTOR_BITS-MAP_BITS
        ora #1<<(FACTOR_BITS-1)
        tay
        rts

    direction_table:
        .char -MAP_ROW_SIZE
        .char 1
        .char MAP_ROW_SIZE
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
        draw_address = backbuffer+8*40+16-1
        lda #<draw_address
        sta ptr+0
        lda #>draw_address
        sta ptr+1
        lda #<(map_table-1)
        sta mptr+0
        lda #>(map_table-1)
        sta mptr+1

        ldx #MAP_ROW_SIZE
        stx row
    outer_loop:
        ldy #MAP_ROW_SIZE
    inner_loop:
        lda (mptr), y
        tax
        lda map_char_table, x
        sta (ptr), y
        dey
        bne inner_loop

        clc
        lda ptr+0
        adc #40
        sta ptr+0
        bcc +
        inc ptr+1
    +

        clc
        lda mptr+0
        adc #MAP_ROW_SIZE
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
        .send
    .bend

draw_level_banner:
    .block
        title_address = backbuffer + (2*40) + 20 - (world_string_size/2)
        level_address = title_address + (1*40) + world_string_size - 3

        ldx #world_string_size-1
    -
        lda #128+32
        sta title_address+0*40, x
        sta title_address+2*40, x
        lda world_string, x
        sta title_address+1*40, x
        dex
        bpl -

        lda #<level_address
        sta ptr+0
        lda #>level_address
        sta ptr+1
        lda level
        jsr bintobcd
        ldy #0
        jsr drawbyte
        rts
    .bend

; Waits for A, D or SPACE, and adjusts the level number based on A.
; Returns with the carry clear if SPACE was pressed.

select_level:
    .block
    -
        lda #3
        sta PIA1_PA
        lda PIA1_PB
        and #%00000011
        cmp #%00000011
        bne change_level        ; A or D pressed

        lda #8
        sta PIA1_PA
        lda PIA1_PB
        and #%00000100
        bne -

        jsr wait_for_release
        clc
        rts

    change_level:
        and #%00000010
        beq +
        jsr prev_level
        jsr wait_for_release
        sec
        rts
    +
        jsr next_level
        jsr wait_for_release
        sec
        rts
    .bend

prev_level:
    dec level
    bpl +
    lda #99
    sta level
+
    rts

next_level:
    lda level
    clc
    adc #1
    cmp #100
    bne +
    lda #0
+
    sta level
    rts

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

; Computes AXY = AX*Y, unsigned.
;    P1 P0 +
;       Q0
;    -----
;    P0*Q0 +
; P1*Q0    +
mul_8x16_24:
    .block
        sta p+1
        stx p+0

        tya
        ldx p+0
        jsr mul_8x8_16
        sta r+1
        sty r+0

        ldx p+1
        jsr mul_8x8_again
        sta r+2
        tya
        clc
        adc r+1
        tax
        lda r+2
        adc #0
        ldy r+0
        rts
        
        .section zp
            p: .word ?
            r: .fill 3, ?
        .send
    .bend

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
        sta hi
        tya
        .rept FACTOR_BITS
            lsr hi
            ror
        .next
        rts

        .section zp
            hi: .byte ?
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

        lda lo
        .rept FACTOR_BITS
            lsr hi
            ror
        .next
        rts

        .section zp
            lhs: .byte ?
            rhs: .byte ?
            lo: .byte ?
            hi: .byte ?
        .send
    .bend

; 8x8 unsigned divide.
; Computes A = A/X.

div_8x8_8:
    .block
        lda #$00
        ldx #$07
        clc
    -
        rol num
        rol
        cmp denom
        bcc +
        sbc denom
    +
        dex
        bpl -
        lda num
        rol
        rts

        .section zp
            num: .byte ?
            denom: .byte ?
        .send
    .bend

; 8x8 signed divide.
; Computs A = A/X.

div_8x8_8s:
    .block
        sta lhs
        stx rhs
        eor rhs                 ; discover sign of result

        bit lhs
        bpl +
        sec
        lda #0
        sbc lhs
        sta lhs
    +

        bit rhs
        bpl +
        sec
        lda #0
        sbc rhs
        sta rhs
    +

        lda lhs
        ldx rhs
        jsr div_8x8_8

        bit rsign
        bpl +
        sta rsign
        sec
        lda #0
        sbc rsign
    +
        rts

        .section zp
            lhs: .byte ?
            rhs: .byte ?
            rsign: .byte ?
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
        cmp #1
        beq shuffled
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

; Slow and horrible routine to convert binary to BCD.

bintobcd:
    tax
    lda #$99
    sed
-
    clc
    adc #1
    dex
    bpl -
    cld
    rts

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
        f := i / FACTOR
        h := $ff
        .if i != 0
            h := 7.9 * 1.0/f
        .endif
        .if h < 1
            h := 1
        .endif
        .byte h
    .next

inverse_table:
    .byte $ff
    .for i := 1, i < 256, i += 1
        f := i / FACTOR
        .byte clamp(nround(FACTOR / f), 0, 255.0)
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

inv_sincos_table_fn .function i
    .endf clamp(256.0 * div(1.0, abs(sin(torad(i)))), 0, 65535)

inv_sincos_table_lo:
    .for i := 0, i < 256, i += 1
        .byte <inv_sincos_table_fn(i)
    .next

inv_sincos_table_hi:
    .for i := 0, i < 256, i += 1
        .byte >inv_sincos_table_fn(i)
    .next

compass_table:
    .text 'e             ese               se             sse              '
    .text 's             ssw               sw             wsw              '
    .text 'w             wnw               nw             nnw              '
    .text 'n             nne               ne             nne              '

ball_string:
    .byte 'b' | $80
    .byte 'a' | $80
    .byte 'l' | $80
    .byte 'l' | $80
    .byte ' ' | $80

hole_string:
    .byte ' ' | $80
    .byte 'h' | $80
    .byte 'o' | $80
    .byte 'l' | $80
    .byte 'e' | $80

world_string:
    .byte ' ' | $80
    .byte 'w' | $80
    .byte 'o' | $80
    .byte 'r' | $80
    .byte 'l' | $80
    .byte 'd' | $80
    .byte ' ' | $80
    .byte       $80
    .byte       $80
    .byte ' ' | $80
world_string_size = * - world_string

spacebar_string:
    .byte ' ' | $80
    .byte 'p' | $80
    .byte 'R' | $80
    .byte 'E' | $80
    .byte 'S' | $80
    .byte 'S' | $80
    .byte ' ' | $80
    .byte 's' | $80
    .byte 'p' | $80
    .byte 'a' | $80
    .byte 'c' | $80
    .byte 'e' | $80
    .byte ' ' | $80
    .byte 'T' | $80
    .byte 'O' | $80
    .byte ' ' | $80
    .byte 'S' | $80
    .byte 'T' | $80
    .byte 'A' | $80
    .byte 'R' | $80
    .byte 'T' | $80
    .byte ' ' | $80
spacebar_string_size = * - spacebar_string

scale_table:
    .byte $60, $65, $74, $75, $61, $f6, $ea, $e7, $e0

sin_table:
cos_table = sin_table + 64
    .for i := 0, i < 256+64, i += 1
        .char clamp(nround(FACTOR * sin(torad(i))), -127, 127)
    .next

dirx_table = cos_table
diry_table = sin_table

deltadisty_table:
deltadistx_table = deltadisty_table + 64
    .for i := 0, i < 256+64, i += 1
        .byte clamp(nround(FACTOR * abs(div(1, sin(torad(i))))), 0, 255)
    .next

titlescreen_table:
    .include "titlescreen.inc"

.section zp
zbuffer:
    .fill 40, ?
.send

.align $100
backbuffer:
    .fill 1024, ?

    * = 0
    .dsection zp
    .cerror * > $ff

; vim: sw=4 ts=4 et

