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

.section zp
p:	.word ?			; general purpose pointer
.send

	* = $401
	.word +, 1
	.enc "none"
	.null $9e, format("%d", _entry)
	.enc "screen"
+	.word 0

_entry:
	lda #12
	sta VIA_PCR
	sei
-
	jsr cls
	ldx #4
	ldy #4
	jsr plot

	ldx #5
	ldy #4
	jsr plot

	ldx #4
	ldy #5
	jsr plot

	jsr redraw
	jmp *

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
;	lda VIA_PB
;	and #%00100000
;	bne redraw

	ldx #0
-
	drawpix $000
	drawpix $100
	drawpix $200
	drawpix $300
	dex
	bne -
	rts

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
		rol				; divide X and Y by two, calculate bitmask
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
	.byte 32  ;	0 " "
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

.align $100
drawbuffer:
	.fill 1024, ?

	* = 0
	.dsection zp
	.cerror * > $ff

