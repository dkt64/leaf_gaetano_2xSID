//====================================================================
// Calming stream - 2xSID by Gaetano
// Code by DKT/Samar
//====================================================================

#import "inc/makra.asm"

//====================================================================
// Stałe
//====================================================================

.const	IRQ1_LINE		= $ff
.const 	MUSIC		= $1000
.const	BMP		= $4000
.const	SCREEN		= $6000

.const 	OFFSET 		= 0

//====================================================================
// Zeropage
//====================================================================

.const	IRQ_ZP		= $02	// zajmuje 4

.const	x0 		= $06
.const	y0 		= $07
.const	x1 		= $08
.const	y1 		= $09
.const	dx 		= $0a
.const	dy 		= $0b
.const	delta 		= $0c

.const 	plot_ptr		= $0e
.const	plot_ptr_l	= plot_ptr
.const	plot_ptr_h	= plot_ptr+1

.const 	lines_ptr		= $10
.const	lines_ptr_l	= lines_ptr
.const	lines_ptr_h	= lines_ptr+1

//====================================================================
// Basic i start programu
//====================================================================

	BasicUpstart2(start)

//====================================================================
// Muza
//====================================================================

	* = MUSIC
	.import binary "data/tune.prg",2

//====================================================================
// grafika
//====================================================================

	* = BMP
	.import binary "data/guitarist06bezgitarzysty.art",2,$1f40

	* = SCREEN
	.import binary "data/guitarist06bezgitarzysty.art",2+$1f40,$3e8

//====================================================================
// Code
//====================================================================

start:	
	sei
	lda #$35
	sta $01

	lda #0
	jsr $1000

	Sync()

	lda #$0e
	sta $d020
	sta $d021
	sta $d011

	jsr init_gfx

	lda op1
	eor op2
	sta op3

	lda $dc0d
	lda $dd0d
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda #$01
	sta $d01a
	lsr $d019
	IrqSetup(IRQ1_LINE, irq1)

	ldx #100
wa5:	Sync()
	dex
	bne wa5

	cli

	ldx #200
wa3:	Sync()
	dex
	bne wa3
	lda #$3b
	sta $d011
	lda #$02
	sta $dd00
	lda #$0e
	sta $d020
	sta $d021
	SetD018(BMP, SCREEN)

	ldx #200
wa4:	Sync()
	dex
	bne wa4

	jmp main

//====================================================================
// irq
//====================================================================

irq1:
	IrqEntry(IRQ_ZP)

	jsr $1003

	lsr $d019
	IrqExit(IRQ_ZP)

//====================================================================
// main
//====================================================================

main:

	ldx iteracje
	lda speed_tab,x
	sta speed1+1
	sta speed2+1

	lda #<lines
	sta lines_ptr_l
	lda #>lines
	sta lines_ptr_h

	lda #<kreski2
	clc
kr1:	adc #0
	sta kkres1+1
	sta kkres2+1
kad:	lda #>kreski2
	sta kkres1+2
	sta kkres2+2

draw_all_lines:

// 	ldx #2
// !:	Sync()
// 	dex
// 	bne !-

	ldy #0
	lda (lines_ptr),y
	sta y0
	iny
	lda (lines_ptr),y
	sta x0
	iny
	lda (lines_ptr),y
	beq move		// Jeżeli zero to MoveTo
	sta y1
	iny
	lda (lines_ptr),y
	sta x1
	jsr kline

	lda lines_ptr_l
	clc
	adc #2
	sta lines_ptr_l
	bcc draw_all_lines
	inc lines_ptr_h
	jmp draw_all_lines
move:
	// Jeżei drugie zero to koniec
	iny
	lda (lines_ptr),y
	beq !+
	
	lda lines_ptr_l
	clc
	adc #3
	sta lines_ptr_l
	bcc draw_all_lines
	inc lines_ptr_h
	jmp draw_all_lines

!:

	// koniec - kolejna iteracja

	ldx #50
wa1:	Sync()
	dex
	bne wa1

	inc kr1+1
	inc kr1+1

	inc iteracje
	lda iteracje
	cmp #3
	bne !+

	ldx #100
wa2:	Sync()
	dex
	bne wa2

	// zmiana kierunku

	lda #0
	sta iteracje
	sta kr1+1

	lda kad+1
	cmp #>kreski2
	bne kad1
	lda #>kreski3
	sta kad+1
	jmp kad2
kad1:
	lda #>kreski2
	sta kad+1
kad2:

	lda wymaz1
	eor op3
	sta wymaz1
	lda wymaz2
	eor op3
	sta wymaz2

	lda wymaz3+1
	eor #$ff
	sta wymaz3+1
	sta wymaz4+1

!:

	jmp main

//====================================================================

iteracje:	.byte 0
op1:	and (plot_ptr),y
op2:	ora (plot_ptr),y
op3:	.byte 0
speed_tab:
	.byte $c0,$60,$20

//====================================================================
// Rysowanie linii kreskowanej
//====================================================================

kline:

// 	lda x0
// 	cmp x1
// 	bne !+
// 	lda y0
// 	cmp y1
// 	bne !+
// 	rts
// !:

	// Sprawdzamy kierunek linii - góra/dół
	lda y0
	cmp y1
	bcc kline_down
	
	// Obliczamy deltę y
	lda y0
	sbc y1
	sta dy
		
	lda #$ce 		// dec
	sta kkiery1
	sta kkiery2
	
	jmp kline_down2
	
kline_down:
	
	// Obliczamy deltę y
	lda y1
	sec
	sbc y0
	sta dy
	
	lda #$ee		// inc
	sta kkiery1
	sta kkiery2
	
kline_down2:
	
	lda y0
	sta klp1+1
	sta klp2+1
	lda y1
	sta klp11+1

	ldy #$e8 		// inx
	// Sprawdzamy kierunek linii - prawo/lewo
	ldx x0
	cpx x1
	bcc !+
	
	// Linia w lewo
	
	lda x0		// delta x (odwrotna)
	sbc x1
	sta dx
	
	ldy #$ca		// dex
	jmp !++
!:
	lda x1
	sec
	sbc x0
	sta dx

!:	sty kkierx1
	sty kkierx2

	lda x1
	sta kkierx3+1
	
	// Sprawdzamy znaczącą oś - pion czy poziom
	lda dx
	cmp dy
	bcs kpozioma
	
	// --- Pionowa ---

	lda dy
	lsr
	sta delta

klp1:	ldy #0
klp11:	cpy #0
	beq !++
kkiery1:	inc klp1+1
	
kkres1:	lda kreski2
	beq !+
	
	lda plot_add_y_lo,y
	sta plot_ptr_l
kbuf_pl1:	lda plot_add_y_hi,y
	sta plot_ptr_h
	ldy plot_add_x,x
	lda plot_mask,x
wymaz3:	eor #$ff
wymaz1:	and (plot_ptr),y
	sta (plot_ptr),y
!:	
	lda $d012
	clc
speed1:	adc #$40
	cmp $d012
	bne *-3

	inc kkres1+1
	inc kkres2+1
	lda delta
	sec
	sbc dx
	sta delta
	bcs klp1
	adc dy
	sta delta
kkierx1:	inx
	bne klp1
!:
	rts

	// --- Pozioma ---

kpozioma:

	lsr
	sta delta
	
kkres2:	lda kreski2
	beq !+

klp2:	ldy #0
	lda plot_add_y_lo,y
	sta plot_ptr_l
kbuf_pl2:	lda plot_add_y_hi,y
	sta plot_ptr_h
	ldy plot_add_x,x
	lda plot_mask,x
wymaz4:	eor #$ff
wymaz2:	and (plot_ptr),y
	sta (plot_ptr),y
!:
	lda $d012
	clc
speed2:	adc #$40
	cmp $d012
	bne *-3

	inc kkres1+1
	inc kkres2+1
	lda delta
	sec
	sbc dy
	bcs !+
	adc dx
kkiery2:	inc klp2+1
!:	sta delta
kkierx2:	inx
kkierx3:	cpx #0
	bne kkres2
	rts

//====================================================================
// gfx
//====================================================================

init_gfx:

// 	ldx #18
// 	lda #$01
// !:	sta SCREEN+$28*09+18,x
// 	sta SCREEN+$28*10+18,x
// 	sta SCREEN+$28*11+18,x
// 	dex
// 	bpl !-

	rts

//====================================================================
// dane
//====================================================================

.align $100
plot_add_x:
	.byte 8*00,8*00,8*00,8*00,8*00,8*00,8*00,8*00
	.byte 8*01,8*01,8*01,8*01,8*01,8*01,8*01,8*01
	.byte 8*02,8*02,8*02,8*02,8*02,8*02,8*02,8*02
	.byte 8*03,8*03,8*03,8*03,8*03,8*03,8*03,8*03
	.byte 8*04,8*04,8*04,8*04,8*04,8*04,8*04,8*04
	.byte 8*05,8*05,8*05,8*05,8*05,8*05,8*05,8*05
	.byte 8*06,8*06,8*06,8*06,8*06,8*06,8*06,8*06
	.byte 8*07,8*07,8*07,8*07,8*07,8*07,8*07,8*07
	.byte 8*08,8*08,8*08,8*08,8*08,8*08,8*08,8*08
	.byte 8*09,8*09,8*09,8*09,8*09,8*09,8*09,8*09
	.byte 8*10,8*10,8*10,8*10,8*10,8*10,8*10,8*10
	.byte 8*11,8*11,8*11,8*11,8*11,8*11,8*11,8*11
	.byte 8*12,8*12,8*12,8*12,8*12,8*12,8*12,8*12
	.byte 8*13,8*13,8*13,8*13,8*13,8*13,8*13,8*13
	.byte 8*14,8*14,8*14,8*14,8*14,8*14,8*14,8*14
	.byte 8*15,8*15,8*15,8*15,8*15,8*15,8*15,8*15
	.byte 8*16,8*16,8*16,8*16,8*16,8*16,8*16,8*16
	.byte 8*17,8*17,8*17,8*17,8*17,8*17,8*17,8*17
	.byte 8*18,8*18,8*18,8*18,8*18,8*18,8*18,8*18
	.byte 8*19,8*19,8*19,8*19,8*19,8*19,8*19,8*19

.align $100
plot_add_y_lo:
	.byte <(BMP+OFFSET+$140*00+0),<(BMP+OFFSET+$140*00+1),<(BMP+OFFSET+$140*00+2),<(BMP+OFFSET+$140*00+3),<(BMP+OFFSET+$140*00+4),<(BMP+OFFSET+$140*00+5),<(BMP+OFFSET+$140*00+6),<(BMP+OFFSET+$140*00+7)
	.byte <(BMP+OFFSET+$140*01+0),<(BMP+OFFSET+$140*01+1),<(BMP+OFFSET+$140*01+2),<(BMP+OFFSET+$140*01+3),<(BMP+OFFSET+$140*01+4),<(BMP+OFFSET+$140*01+5),<(BMP+OFFSET+$140*01+6),<(BMP+OFFSET+$140*01+7)
	.byte <(BMP+OFFSET+$140*02+0),<(BMP+OFFSET+$140*02+1),<(BMP+OFFSET+$140*02+2),<(BMP+OFFSET+$140*02+3),<(BMP+OFFSET+$140*02+4),<(BMP+OFFSET+$140*02+5),<(BMP+OFFSET+$140*02+6),<(BMP+OFFSET+$140*02+7)
	.byte <(BMP+OFFSET+$140*03+0),<(BMP+OFFSET+$140*03+1),<(BMP+OFFSET+$140*03+2),<(BMP+OFFSET+$140*03+3),<(BMP+OFFSET+$140*03+4),<(BMP+OFFSET+$140*03+5),<(BMP+OFFSET+$140*03+6),<(BMP+OFFSET+$140*03+7)
	.byte <(BMP+OFFSET+$140*04+0),<(BMP+OFFSET+$140*04+1),<(BMP+OFFSET+$140*04+2),<(BMP+OFFSET+$140*04+3),<(BMP+OFFSET+$140*04+4),<(BMP+OFFSET+$140*04+5),<(BMP+OFFSET+$140*04+6),<(BMP+OFFSET+$140*04+7)
	.byte <(BMP+OFFSET+$140*05+0),<(BMP+OFFSET+$140*05+1),<(BMP+OFFSET+$140*05+2),<(BMP+OFFSET+$140*05+3),<(BMP+OFFSET+$140*05+4),<(BMP+OFFSET+$140*05+5),<(BMP+OFFSET+$140*05+6),<(BMP+OFFSET+$140*05+7)
	.byte <(BMP+OFFSET+$140*06+0),<(BMP+OFFSET+$140*06+1),<(BMP+OFFSET+$140*06+2),<(BMP+OFFSET+$140*06+3),<(BMP+OFFSET+$140*06+4),<(BMP+OFFSET+$140*06+5),<(BMP+OFFSET+$140*06+6),<(BMP+OFFSET+$140*06+7)
	.byte <(BMP+OFFSET+$140*07+0),<(BMP+OFFSET+$140*07+1),<(BMP+OFFSET+$140*07+2),<(BMP+OFFSET+$140*07+3),<(BMP+OFFSET+$140*07+4),<(BMP+OFFSET+$140*07+5),<(BMP+OFFSET+$140*07+6),<(BMP+OFFSET+$140*07+7)
	.byte <(BMP+OFFSET+$140*08+0),<(BMP+OFFSET+$140*08+1),<(BMP+OFFSET+$140*08+2),<(BMP+OFFSET+$140*08+3),<(BMP+OFFSET+$140*08+4),<(BMP+OFFSET+$140*08+5),<(BMP+OFFSET+$140*08+6),<(BMP+OFFSET+$140*08+7)
	.byte <(BMP+OFFSET+$140*09+0),<(BMP+OFFSET+$140*09+1),<(BMP+OFFSET+$140*09+2),<(BMP+OFFSET+$140*09+3),<(BMP+OFFSET+$140*09+4),<(BMP+OFFSET+$140*09+5),<(BMP+OFFSET+$140*09+6),<(BMP+OFFSET+$140*09+7)
	.byte <(BMP+OFFSET+$140*10+0),<(BMP+OFFSET+$140*10+1),<(BMP+OFFSET+$140*10+2),<(BMP+OFFSET+$140*10+3),<(BMP+OFFSET+$140*10+4),<(BMP+OFFSET+$140*10+5),<(BMP+OFFSET+$140*10+6),<(BMP+OFFSET+$140*10+7)
	.byte <(BMP+OFFSET+$140*11+0),<(BMP+OFFSET+$140*11+1),<(BMP+OFFSET+$140*11+2),<(BMP+OFFSET+$140*11+3),<(BMP+OFFSET+$140*11+4),<(BMP+OFFSET+$140*11+5),<(BMP+OFFSET+$140*11+6),<(BMP+OFFSET+$140*11+7)
	.byte <(BMP+OFFSET+$140*12+0),<(BMP+OFFSET+$140*12+1),<(BMP+OFFSET+$140*12+2),<(BMP+OFFSET+$140*12+3),<(BMP+OFFSET+$140*12+4),<(BMP+OFFSET+$140*12+5),<(BMP+OFFSET+$140*12+6),<(BMP+OFFSET+$140*12+7)
	.byte <(BMP+OFFSET+$140*13+0),<(BMP+OFFSET+$140*13+1),<(BMP+OFFSET+$140*13+2),<(BMP+OFFSET+$140*13+3),<(BMP+OFFSET+$140*13+4),<(BMP+OFFSET+$140*13+5),<(BMP+OFFSET+$140*13+6),<(BMP+OFFSET+$140*13+7)
	.byte <(BMP+OFFSET+$140*14+0),<(BMP+OFFSET+$140*14+1),<(BMP+OFFSET+$140*14+2),<(BMP+OFFSET+$140*14+3),<(BMP+OFFSET+$140*14+4),<(BMP+OFFSET+$140*14+5),<(BMP+OFFSET+$140*14+6),<(BMP+OFFSET+$140*14+7)
	.byte <(BMP+OFFSET+$140*15+0),<(BMP+OFFSET+$140*15+1),<(BMP+OFFSET+$140*15+2),<(BMP+OFFSET+$140*15+3),<(BMP+OFFSET+$140*15+4),<(BMP+OFFSET+$140*15+5),<(BMP+OFFSET+$140*15+6),<(BMP+OFFSET+$140*15+7)
	.byte <(BMP+OFFSET+$140*16+0),<(BMP+OFFSET+$140*16+1),<(BMP+OFFSET+$140*16+2),<(BMP+OFFSET+$140*16+3),<(BMP+OFFSET+$140*16+4),<(BMP+OFFSET+$140*16+5),<(BMP+OFFSET+$140*16+6),<(BMP+OFFSET+$140*16+7)
	.byte <(BMP+OFFSET+$140*17+0),<(BMP+OFFSET+$140*17+1),<(BMP+OFFSET+$140*17+2),<(BMP+OFFSET+$140*17+3),<(BMP+OFFSET+$140*17+4),<(BMP+OFFSET+$140*17+5),<(BMP+OFFSET+$140*17+6),<(BMP+OFFSET+$140*17+7)
	.byte <(BMP+OFFSET+$140*18+0),<(BMP+OFFSET+$140*18+1),<(BMP+OFFSET+$140*18+2),<(BMP+OFFSET+$140*18+3),<(BMP+OFFSET+$140*18+4),<(BMP+OFFSET+$140*18+5),<(BMP+OFFSET+$140*18+6),<(BMP+OFFSET+$140*18+7)
	.byte <(BMP+OFFSET+$140*19+0),<(BMP+OFFSET+$140*19+1),<(BMP+OFFSET+$140*19+2),<(BMP+OFFSET+$140*19+3),<(BMP+OFFSET+$140*19+4),<(BMP+OFFSET+$140*19+5),<(BMP+OFFSET+$140*19+6),<(BMP+OFFSET+$140*19+7)
	.byte <(BMP+OFFSET+$140*20+0),<(BMP+OFFSET+$140*20+1),<(BMP+OFFSET+$140*20+2),<(BMP+OFFSET+$140*20+3),<(BMP+OFFSET+$140*20+4),<(BMP+OFFSET+$140*20+5),<(BMP+OFFSET+$140*20+6),<(BMP+OFFSET+$140*20+7)
	.byte <(BMP+OFFSET+$140*21+0),<(BMP+OFFSET+$140*21+1),<(BMP+OFFSET+$140*21+2),<(BMP+OFFSET+$140*21+3),<(BMP+OFFSET+$140*21+4),<(BMP+OFFSET+$140*21+5),<(BMP+OFFSET+$140*21+6),<(BMP+OFFSET+$140*21+7)
	.byte <(BMP+OFFSET+$140*22+0),<(BMP+OFFSET+$140*22+1),<(BMP+OFFSET+$140*22+2),<(BMP+OFFSET+$140*22+3),<(BMP+OFFSET+$140*22+4),<(BMP+OFFSET+$140*22+5),<(BMP+OFFSET+$140*22+6),<(BMP+OFFSET+$140*22+7)
	.byte <(BMP+OFFSET+$140*23+0),<(BMP+OFFSET+$140*23+1),<(BMP+OFFSET+$140*23+2),<(BMP+OFFSET+$140*23+3),<(BMP+OFFSET+$140*23+4),<(BMP+OFFSET+$140*23+5),<(BMP+OFFSET+$140*23+6),<(BMP+OFFSET+$140*23+7)
	.byte <(BMP+OFFSET+$140*24+0),<(BMP+OFFSET+$140*24+1),<(BMP+OFFSET+$140*24+2),<(BMP+OFFSET+$140*24+3),<(BMP+OFFSET+$140*24+4),<(BMP+OFFSET+$140*24+5),<(BMP+OFFSET+$140*24+6),<(BMP+OFFSET+$140*24+7)

.align $100
plot_add_y_hi:
	.byte >(BMP+OFFSET+$140*00+0),>(BMP+OFFSET+$140*00+1),>(BMP+OFFSET+$140*00+2),>(BMP+OFFSET+$140*00+3),>(BMP+OFFSET+$140*00+4),>(BMP+OFFSET+$140*00+5),>(BMP+OFFSET+$140*00+6),>(BMP+OFFSET+$140*00+7)
	.byte >(BMP+OFFSET+$140*01+0),>(BMP+OFFSET+$140*01+1),>(BMP+OFFSET+$140*01+2),>(BMP+OFFSET+$140*01+3),>(BMP+OFFSET+$140*01+4),>(BMP+OFFSET+$140*01+5),>(BMP+OFFSET+$140*01+6),>(BMP+OFFSET+$140*01+7)
	.byte >(BMP+OFFSET+$140*02+0),>(BMP+OFFSET+$140*02+1),>(BMP+OFFSET+$140*02+2),>(BMP+OFFSET+$140*02+3),>(BMP+OFFSET+$140*02+4),>(BMP+OFFSET+$140*02+5),>(BMP+OFFSET+$140*02+6),>(BMP+OFFSET+$140*02+7)
	.byte >(BMP+OFFSET+$140*03+0),>(BMP+OFFSET+$140*03+1),>(BMP+OFFSET+$140*03+2),>(BMP+OFFSET+$140*03+3),>(BMP+OFFSET+$140*03+4),>(BMP+OFFSET+$140*03+5),>(BMP+OFFSET+$140*03+6),>(BMP+OFFSET+$140*03+7)
	.byte >(BMP+OFFSET+$140*04+0),>(BMP+OFFSET+$140*04+1),>(BMP+OFFSET+$140*04+2),>(BMP+OFFSET+$140*04+3),>(BMP+OFFSET+$140*04+4),>(BMP+OFFSET+$140*04+5),>(BMP+OFFSET+$140*04+6),>(BMP+OFFSET+$140*04+7)
	.byte >(BMP+OFFSET+$140*05+0),>(BMP+OFFSET+$140*05+1),>(BMP+OFFSET+$140*05+2),>(BMP+OFFSET+$140*05+3),>(BMP+OFFSET+$140*05+4),>(BMP+OFFSET+$140*05+5),>(BMP+OFFSET+$140*05+6),>(BMP+OFFSET+$140*05+7)
	.byte >(BMP+OFFSET+$140*06+0),>(BMP+OFFSET+$140*06+1),>(BMP+OFFSET+$140*06+2),>(BMP+OFFSET+$140*06+3),>(BMP+OFFSET+$140*06+4),>(BMP+OFFSET+$140*06+5),>(BMP+OFFSET+$140*06+6),>(BMP+OFFSET+$140*06+7)
	.byte >(BMP+OFFSET+$140*07+0),>(BMP+OFFSET+$140*07+1),>(BMP+OFFSET+$140*07+2),>(BMP+OFFSET+$140*07+3),>(BMP+OFFSET+$140*07+4),>(BMP+OFFSET+$140*07+5),>(BMP+OFFSET+$140*07+6),>(BMP+OFFSET+$140*07+7)
	.byte >(BMP+OFFSET+$140*08+0),>(BMP+OFFSET+$140*08+1),>(BMP+OFFSET+$140*08+2),>(BMP+OFFSET+$140*08+3),>(BMP+OFFSET+$140*08+4),>(BMP+OFFSET+$140*08+5),>(BMP+OFFSET+$140*08+6),>(BMP+OFFSET+$140*08+7)
	.byte >(BMP+OFFSET+$140*09+0),>(BMP+OFFSET+$140*09+1),>(BMP+OFFSET+$140*09+2),>(BMP+OFFSET+$140*09+3),>(BMP+OFFSET+$140*09+4),>(BMP+OFFSET+$140*09+5),>(BMP+OFFSET+$140*09+6),>(BMP+OFFSET+$140*09+7)
	.byte >(BMP+OFFSET+$140*10+0),>(BMP+OFFSET+$140*10+1),>(BMP+OFFSET+$140*10+2),>(BMP+OFFSET+$140*10+3),>(BMP+OFFSET+$140*10+4),>(BMP+OFFSET+$140*10+5),>(BMP+OFFSET+$140*10+6),>(BMP+OFFSET+$140*10+7)
	.byte >(BMP+OFFSET+$140*11+0),>(BMP+OFFSET+$140*11+1),>(BMP+OFFSET+$140*11+2),>(BMP+OFFSET+$140*11+3),>(BMP+OFFSET+$140*11+4),>(BMP+OFFSET+$140*11+5),>(BMP+OFFSET+$140*11+6),>(BMP+OFFSET+$140*11+7)
	.byte >(BMP+OFFSET+$140*12+0),>(BMP+OFFSET+$140*12+1),>(BMP+OFFSET+$140*12+2),>(BMP+OFFSET+$140*12+3),>(BMP+OFFSET+$140*12+4),>(BMP+OFFSET+$140*12+5),>(BMP+OFFSET+$140*12+6),>(BMP+OFFSET+$140*12+7)
	.byte >(BMP+OFFSET+$140*13+0),>(BMP+OFFSET+$140*13+1),>(BMP+OFFSET+$140*13+2),>(BMP+OFFSET+$140*13+3),>(BMP+OFFSET+$140*13+4),>(BMP+OFFSET+$140*13+5),>(BMP+OFFSET+$140*13+6),>(BMP+OFFSET+$140*13+7)
	.byte >(BMP+OFFSET+$140*14+0),>(BMP+OFFSET+$140*14+1),>(BMP+OFFSET+$140*14+2),>(BMP+OFFSET+$140*14+3),>(BMP+OFFSET+$140*14+4),>(BMP+OFFSET+$140*14+5),>(BMP+OFFSET+$140*14+6),>(BMP+OFFSET+$140*14+7)
	.byte >(BMP+OFFSET+$140*15+0),>(BMP+OFFSET+$140*15+1),>(BMP+OFFSET+$140*15+2),>(BMP+OFFSET+$140*15+3),>(BMP+OFFSET+$140*15+4),>(BMP+OFFSET+$140*15+5),>(BMP+OFFSET+$140*15+6),>(BMP+OFFSET+$140*15+7)
	.byte >(BMP+OFFSET+$140*16+0),>(BMP+OFFSET+$140*16+1),>(BMP+OFFSET+$140*16+2),>(BMP+OFFSET+$140*16+3),>(BMP+OFFSET+$140*16+4),>(BMP+OFFSET+$140*16+5),>(BMP+OFFSET+$140*16+6),>(BMP+OFFSET+$140*16+7)
	.byte >(BMP+OFFSET+$140*17+0),>(BMP+OFFSET+$140*17+1),>(BMP+OFFSET+$140*17+2),>(BMP+OFFSET+$140*17+3),>(BMP+OFFSET+$140*17+4),>(BMP+OFFSET+$140*17+5),>(BMP+OFFSET+$140*17+6),>(BMP+OFFSET+$140*17+7)
	.byte >(BMP+OFFSET+$140*18+0),>(BMP+OFFSET+$140*18+1),>(BMP+OFFSET+$140*18+2),>(BMP+OFFSET+$140*18+3),>(BMP+OFFSET+$140*18+4),>(BMP+OFFSET+$140*18+5),>(BMP+OFFSET+$140*18+6),>(BMP+OFFSET+$140*18+7)
	.byte >(BMP+OFFSET+$140*19+0),>(BMP+OFFSET+$140*19+1),>(BMP+OFFSET+$140*19+2),>(BMP+OFFSET+$140*19+3),>(BMP+OFFSET+$140*19+4),>(BMP+OFFSET+$140*19+5),>(BMP+OFFSET+$140*19+6),>(BMP+OFFSET+$140*19+7)
	.byte >(BMP+OFFSET+$140*20+0),>(BMP+OFFSET+$140*20+1),>(BMP+OFFSET+$140*20+2),>(BMP+OFFSET+$140*20+3),>(BMP+OFFSET+$140*20+4),>(BMP+OFFSET+$140*20+5),>(BMP+OFFSET+$140*20+6),>(BMP+OFFSET+$140*20+7)
	.byte >(BMP+OFFSET+$140*21+0),>(BMP+OFFSET+$140*21+1),>(BMP+OFFSET+$140*21+2),>(BMP+OFFSET+$140*21+3),>(BMP+OFFSET+$140*21+4),>(BMP+OFFSET+$140*21+5),>(BMP+OFFSET+$140*21+6),>(BMP+OFFSET+$140*21+7)
	.byte >(BMP+OFFSET+$140*22+0),>(BMP+OFFSET+$140*22+1),>(BMP+OFFSET+$140*22+2),>(BMP+OFFSET+$140*22+3),>(BMP+OFFSET+$140*22+4),>(BMP+OFFSET+$140*22+5),>(BMP+OFFSET+$140*22+6),>(BMP+OFFSET+$140*22+7)
	.byte >(BMP+OFFSET+$140*23+0),>(BMP+OFFSET+$140*23+1),>(BMP+OFFSET+$140*23+2),>(BMP+OFFSET+$140*23+3),>(BMP+OFFSET+$140*23+4),>(BMP+OFFSET+$140*23+5),>(BMP+OFFSET+$140*23+6),>(BMP+OFFSET+$140*23+7)
	.byte >(BMP+OFFSET+$140*24+0),>(BMP+OFFSET+$140*24+1),>(BMP+OFFSET+$140*24+2),>(BMP+OFFSET+$140*24+3),>(BMP+OFFSET+$140*24+4),>(BMP+OFFSET+$140*24+5),>(BMP+OFFSET+$140*24+6),>(BMP+OFFSET+$140*24+7)

// .align $100
// plot_add_y_hi2:
// 	.byte >(BMP2+OFFSET+$140*00+0),>(BMP2+OFFSET+$140*00+1),>(BMP2+OFFSET+$140*00+2),>(BMP2+OFFSET+$140*00+3),>(BMP2+OFFSET+$140*00+4),>(BMP2+OFFSET+$140*00+5),>(BMP2+OFFSET+$140*00+6),>(BMP2+OFFSET+$140*00+7)
// 	.byte >(BMP2+OFFSET+$140*01+0),>(BMP2+OFFSET+$140*01+1),>(BMP2+OFFSET+$140*01+2),>(BMP2+OFFSET+$140*01+3),>(BMP2+OFFSET+$140*01+4),>(BMP2+OFFSET+$140*01+5),>(BMP2+OFFSET+$140*01+6),>(BMP2+OFFSET+$140*01+7)
// 	.byte >(BMP2+OFFSET+$140*02+0),>(BMP2+OFFSET+$140*02+1),>(BMP2+OFFSET+$140*02+2),>(BMP2+OFFSET+$140*02+3),>(BMP2+OFFSET+$140*02+4),>(BMP2+OFFSET+$140*02+5),>(BMP2+OFFSET+$140*02+6),>(BMP2+OFFSET+$140*02+7)
// 	.byte >(BMP2+OFFSET+$140*03+0),>(BMP2+OFFSET+$140*03+1),>(BMP2+OFFSET+$140*03+2),>(BMP2+OFFSET+$140*03+3),>(BMP2+OFFSET+$140*03+4),>(BMP2+OFFSET+$140*03+5),>(BMP2+OFFSET+$140*03+6),>(BMP2+OFFSET+$140*03+7)
// 	.byte >(BMP2+OFFSET+$140*04+0),>(BMP2+OFFSET+$140*04+1),>(BMP2+OFFSET+$140*04+2),>(BMP2+OFFSET+$140*04+3),>(BMP2+OFFSET+$140*04+4),>(BMP2+OFFSET+$140*04+5),>(BMP2+OFFSET+$140*04+6),>(BMP2+OFFSET+$140*04+7)
// 	.byte >(BMP2+OFFSET+$140*05+0),>(BMP2+OFFSET+$140*05+1),>(BMP2+OFFSET+$140*05+2),>(BMP2+OFFSET+$140*05+3),>(BMP2+OFFSET+$140*05+4),>(BMP2+OFFSET+$140*05+5),>(BMP2+OFFSET+$140*05+6),>(BMP2+OFFSET+$140*05+7)
// 	.byte >(BMP2+OFFSET+$140*06+0),>(BMP2+OFFSET+$140*06+1),>(BMP2+OFFSET+$140*06+2),>(BMP2+OFFSET+$140*06+3),>(BMP2+OFFSET+$140*06+4),>(BMP2+OFFSET+$140*06+5),>(BMP2+OFFSET+$140*06+6),>(BMP2+OFFSET+$140*06+7)
// 	.byte >(BMP2+OFFSET+$140*07+0),>(BMP2+OFFSET+$140*07+1),>(BMP2+OFFSET+$140*07+2),>(BMP2+OFFSET+$140*07+3),>(BMP2+OFFSET+$140*07+4),>(BMP2+OFFSET+$140*07+5),>(BMP2+OFFSET+$140*07+6),>(BMP2+OFFSET+$140*07+7)
// 	.byte >(BMP2+OFFSET+$140*08+0),>(BMP2+OFFSET+$140*08+1),>(BMP2+OFFSET+$140*08+2),>(BMP2+OFFSET+$140*08+3),>(BMP2+OFFSET+$140*08+4),>(BMP2+OFFSET+$140*08+5),>(BMP2+OFFSET+$140*08+6),>(BMP2+OFFSET+$140*08+7)
// 	.byte >(BMP2+OFFSET+$140*09+0),>(BMP2+OFFSET+$140*09+1),>(BMP2+OFFSET+$140*09+2),>(BMP2+OFFSET+$140*09+3),>(BMP2+OFFSET+$140*09+4),>(BMP2+OFFSET+$140*09+5),>(BMP2+OFFSET+$140*09+6),>(BMP2+OFFSET+$140*09+7)
// 	.byte >(BMP2+OFFSET+$140*10+0),>(BMP2+OFFSET+$140*10+1),>(BMP2+OFFSET+$140*10+2),>(BMP2+OFFSET+$140*10+3),>(BMP2+OFFSET+$140*10+4),>(BMP2+OFFSET+$140*10+5),>(BMP2+OFFSET+$140*10+6),>(BMP2+OFFSET+$140*10+7)
// 	.byte >(BMP2+OFFSET+$140*11+0),>(BMP2+OFFSET+$140*11+1),>(BMP2+OFFSET+$140*11+2),>(BMP2+OFFSET+$140*11+3),>(BMP2+OFFSET+$140*11+4),>(BMP2+OFFSET+$140*11+5),>(BMP2+OFFSET+$140*11+6),>(BMP2+OFFSET+$140*11+7)
// 	.byte >(BMP2+OFFSET+$140*12+0),>(BMP2+OFFSET+$140*12+1),>(BMP2+OFFSET+$140*12+2),>(BMP2+OFFSET+$140*12+3),>(BMP2+OFFSET+$140*12+4),>(BMP2+OFFSET+$140*12+5),>(BMP2+OFFSET+$140*12+6),>(BMP2+OFFSET+$140*12+7)
// 	.byte >(BMP2+OFFSET+$140*13+0),>(BMP2+OFFSET+$140*13+1),>(BMP2+OFFSET+$140*13+2),>(BMP2+OFFSET+$140*13+3),>(BMP2+OFFSET+$140*13+4),>(BMP2+OFFSET+$140*13+5),>(BMP2+OFFSET+$140*13+6),>(BMP2+OFFSET+$140*13+7)
// 	.byte >(BMP2+OFFSET+$140*14+0),>(BMP2+OFFSET+$140*14+1),>(BMP2+OFFSET+$140*14+2),>(BMP2+OFFSET+$140*14+3),>(BMP2+OFFSET+$140*14+4),>(BMP2+OFFSET+$140*14+5),>(BMP2+OFFSET+$140*14+6),>(BMP2+OFFSET+$140*14+7)
// 	.byte >(BMP2+OFFSET+$140*15+0),>(BMP2+OFFSET+$140*15+1),>(BMP2+OFFSET+$140*15+2),>(BMP2+OFFSET+$140*15+3),>(BMP2+OFFSET+$140*15+4),>(BMP2+OFFSET+$140*15+5),>(BMP2+OFFSET+$140*15+6),>(BMP2+OFFSET+$140*15+7)
// 	.byte >(BMP2+OFFSET+$140*16+0),>(BMP2+OFFSET+$140*16+1),>(BMP2+OFFSET+$140*16+2),>(BMP2+OFFSET+$140*16+3),>(BMP2+OFFSET+$140*16+4),>(BMP2+OFFSET+$140*16+5),>(BMP2+OFFSET+$140*16+6),>(BMP2+OFFSET+$140*16+7)
// 	.byte >(BMP2+OFFSET+$140*17+0),>(BMP2+OFFSET+$140*17+1),>(BMP2+OFFSET+$140*17+2),>(BMP2+OFFSET+$140*17+3),>(BMP2+OFFSET+$140*17+4),>(BMP2+OFFSET+$140*17+5),>(BMP2+OFFSET+$140*17+6),>(BMP2+OFFSET+$140*17+7)
// 	.byte >(BMP2+OFFSET+$140*18+0),>(BMP2+OFFSET+$140*18+1),>(BMP2+OFFSET+$140*18+2),>(BMP2+OFFSET+$140*18+3),>(BMP2+OFFSET+$140*18+4),>(BMP2+OFFSET+$140*18+5),>(BMP2+OFFSET+$140*18+6),>(BMP2+OFFSET+$140*18+7)
// 	.byte >(BMP2+OFFSET+$140*19+0),>(BMP2+OFFSET+$140*19+1),>(BMP2+OFFSET+$140*19+2),>(BMP2+OFFSET+$140*19+3),>(BMP2+OFFSET+$140*19+4),>(BMP2+OFFSET+$140*19+5),>(BMP2+OFFSET+$140*19+6),>(BMP2+OFFSET+$140*19+7)

.align $100
plot_mask:
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01

	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01
	.byte $80,$40,$20,$10,$08,$04,$02,$01


// .align $100
// kreski1:
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// 	.byte 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0
// kreski1_end:
.align $100
kreski2:
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
	.byte 1,1,1,0,0,0,1,1,1,0,0,0
kreski2_end:
.align $100
kreski3:
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
	.byte 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0
kreski3_end:
// .align $100
// kreski4:
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 1,1,0,1,1,0,1,1,0,1,1,0,1,1,0
// 	.byte 0
// reski4_end:

.align $100
lines:
	.import binary "data/guitarisr05a.lll"

//====================================================================
//====================================================================
code_end:
