//====================================================================
// Leaf - 2xSID by Gaetano
// Code by DKT/Samar
//====================================================================

#import "inc/makra.asm"

//====================================================================
// Stałe
//====================================================================

.const	IRQ1_LINE		= $fa
.const 	MUSIC		= $1000

//====================================================================
// Zeropage
//====================================================================

.const	IRQ_ZP		= $02	// zajmuje 4

.const 	T1 		= $10
.const 	T2 		= $12
.const 	PRODUCT 		= $16

//====================================================================
// Basic i start programu
//====================================================================

	BasicUpstart2(start)

start:	
	sei
	lda #$35
	sta $01

	lda #0
	jsr $1000

	jsr multiply_table_generator

	lda $dc0d
	lda $dd0d
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda #$01
	sta $d01a
	asl $d019
	lda #$1b
	sta $d011
	IrqSetup(IRQ1_LINE, irq1)
	cli
lp:
	jsr main_loop

	jmp lp

//====================================================================
// irq
//====================================================================

irq1:
	IrqEntry(IRQ_ZP)

	ldx #5
	dex
	bne *-1
	inc $d020
	jsr $1003
	dec $d020

	IrqExit(IRQ_ZP)


//====================================================================
// Muza
//====================================================================

	* = MUSIC
	.import binary "data/tune.prg",2

//====================================================================
// Random
//====================================================================

get_random:
	lda seed
	beq doEor
	asl
	beq noEor // if the input was $80, skip the EOR
	bcc noEor
doEor:	eor #$1d
noEor:	sta seed
	rts

seed:	.byte 0

//====================================================================
// main_loop
//====================================================================

main_loop:

	Sync()
	dec $d020

	jsr get_random
	
// 	cmp par10
// 	bcs !+
// 	jmp range1
// !:
// 	cmp par20
// 	bcs !+
// 	jmp range2
// !:
// 	cmp par30
// 	bcs !+
// 	jmp range3
// !:
	// range 4

	// nextX

	// x

	lda par41+0
	sta T1+0
	lda par41+1
	sta T1+1

	lda x+0
	sta T2+0
	lda x+1
	sta T2+1
	lda x+2
	sta T2+2
	lda x+3
	sta T2+3

	jsr multiply_32bit_unsigned

	lda PRODUCT32+0
	sta nextX+0
	lda PRODUCT32+1
	sta nextX+1
	lda PRODUCT32+2
	sta nextX+2
	lda PRODUCT32+3
	sta nextX+3

	// y

	lda par42+0
	sta T1+0
	lda par42+1
	sta T1+1

	lda y+0
	sta T2+0
	lda y+1
	sta T2+1
	lda y+2
	sta T2+2
	lda y+3
	sta T2+3

	jsr multiply_32bit_unsigned
	
	lda nextX+0
	clc
	adc PRODUCT32+0
	sta nextX+0
	lda nextX+1
	adc PRODUCT32+1
	sta nextX+1
	lda nextX+2
	adc PRODUCT32+2
	sta nextX+2
	lda nextX+3
	adc PRODUCT32+3
	sta nextX+3

	// nextY

	// x

	lda par43+0
	sta T1+0
	lda par43+1
	sta T1+1

	lda x+0
	sta T2+0
	lda x+1
	sta T2+1
	lda x+2
	sta T2+2
	lda x+3
	sta T2+3

	jsr multiply_32bit_unsigned

	lda PRODUCT32+0
	sta nextY+0
	lda PRODUCT32+1
	sta nextY+1
	lda PRODUCT32+2
	sta nextY+2
	lda PRODUCT32+3
	sta nextY+3

	// y

	lda par43+0
	sta T1+0
	lda par43+1
	sta T1+1

	lda y+0
	sta T2+0
	lda y+1
	sta T2+1
	lda y+2
	sta T2+2
	lda y+3
	sta T2+3

	jsr multiply_32bit_unsigned
	
	lda nextY+0
	clc
	adc PRODUCT32+0
	sta nextY+0
	lda nextY+1
	adc PRODUCT32+1
	sta nextY+1
	lda nextY+2
	adc PRODUCT32+2
	sta nextY+2
	lda nextY+3
	adc PRODUCT32+3
	sta nextY+3

	lda nextY+2
	clc
	adc par45+0
	sta nextY+2
	lda nextY+3
	adc par45+1
	sta nextY+3

range3:
range2:
range1:

	// przepisanie

	lda nextX+1
	sta x+0
	lda nextX+2
	sta x+1
	lda nextX+3
	sta x+2
	lda #0
	sta x+3

	lda nextY+1
	sta y+0
	lda nextY+2
	sta y+1
	lda nextY+3
	sta y+2
	lda #0
	sta y+3

	inc $d020
	rts

	// r < 2
par10:	.byte 2
par11:	.word $0000 // 0
par12: 	.word $0019 // 25
	// r < 220
par20: 	.byte 220
par21:	.word $00d9 // 217
par22:	.word $000a // 10
par23: 	.word $0135 // 309
	// r < 238
par30:	.byte 238
par31:	.word $0033 // 51
par32:	.word $0042 // 66
par33: 	.word $003a // 58
par34: 	.word $0038 // 56
par35: 	.word $0135 // 309
	// else
par41: 	.word $0026 // 38
par42:	.word $0047 // 71
par43:	.word $0042 // 66
par44:	.word $003d // 61
par45:	.word $0070 // 112

PRODUCT32: 
	.dword 0
x: 	.dword 0
y: 	.dword 0
nextX: 	.dword 0
nextY: 	.dword 0
r:	.byte 0
plotX:	.byte 0
plotY:	.byte 0

// ;==========================================================
// ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// ; Seriously fast multiplication by JackAsser
// ; https://codebase64.org/doku.php?id=base:seriously_fast_multiplication
// ;
// ; Implementatin of "multiply_32bit_unsigned" by DKT
// ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// ;==========================================================

// .const 	T1 		= $10 // 16-bit input
// .const 	T2 		= $12 // 16-bit input
// .const 	PRODUCT 		= $14 // 32-bit result

// .const 	T32_1 		= $18 // 32-bit input
// .const 	T32_2 		= $1c // 32-bit input
// .const 	PRODUCT32 	= $20 // 32-bit result

// ;==========================================================
// ; Multiply tables generator by Graham
// ;==========================================================
multiply_table_generator:
	ldx #$00
	txa
	.byte $c9
lb1:	tya
	adc #$00
ml1:	sta square1_hi,x
	tay
	cmp #$40
	txa
	ror
ml9:	adc #$00
	sta ml9+1
	inx
ml0:	sta square1_lo,x
	bne lb1
	inc ml0+2
	inc ml1+2
	clc
	iny
	bne lb1
	
	ldx #$00
	ldy #$ff
!:	lda square1_hi+1,x
	sta square2_hi+$100,x
	lda square1_hi,x
	sta square2_hi,y
	lda square1_lo+1,x
	sta square2_lo+$100,x
	lda square1_lo,x
	sta square2_lo,y
	dey
	inx
	bne !-
	rts

// ;==========================================================
// ; Description: Unsigned 8-bit multiplication with unsigned 16-bit result.
// ;                                                                        
// ; Input: 8-bit unsigned value in T1                                      
// ;        8-bit unsigned value in T2                                      
// ;        Carry=0: Re-use T1 from previous multiplication (faster)        
// ;        Carry=1: Set T1 (slower)                                        
// ;                                                                        
// ; Output: 16-bit unsigned value in PRODUCT                               
// ;                                                                        
// ; Clobbered: PRODUCT, X, A, C                                            
// ;                                                                        
// ; Allocation setup: T1,T2 and PRODUCT preferably on Zero-page.           
// ;                   square1_lo, square1_hi, square2_lo, square2_hi must be
// ;                   page aligned. Each table are 512 bytes. Total 2kb.    
// ;                                                                         
// ; Table generation: I:0..511                                              
// ;                   square1_lo = <((I*I)/4)                               
// ;                   square1_hi = >((I*I)/4)                               
// ;                   square2_lo = <(((I-255)*(I-255))/4)                   
// ;                   square2_hi = >(((I-255)*(I-255))/4)                   
// ;==========================================================
multiply_8bit_unsigned:
	bcc !+
	lda T1
	sta sm1+1
	sta sm3+1
	eor #$ff
	sta sm2+1
	sta sm4+1
!:	ldx T2
sm1:	lda square1_lo,x
	sec
sm2:	sbc square2_lo,x
	sta PRODUCT+0
sm3:	lda square1_hi,x
sm4:	sbc square2_hi,x
	sta PRODUCT+1   
	rts
	
// ;==========================================================
// ; Description: Signed 8-bit multiplication with signed 16-bit result.
// ;                                                                    
// ; Input: 8-bit signed value in T1                                    
// ;        8-bit signed value in T2                                    
// ;        Carry=0: Re-use T1 from previous multiplication (faster)    
// ;        Carry=1: Set T1 (slower)                                    
// ;                                                                    
// ; Output: 16-bit signed value in PRODUCT                             
// ;                                                                    
// ; Clobbered: PRODUCT, X, A, C                                        
// ;==========================================================
multiply_8bit_signed:
	jsr multiply_8bit_unsigned
	// ; Apply sign (See C=Hacking16 for details).
	lda T1
	bpl !+
	lda PRODUCT+1
	sec
	sbc T2
	sta PRODUCT+1
!:	lda T2
	bpl !+
	lda PRODUCT+1
	sec
	sbc T1
	sta PRODUCT+1
!:	rts

// ;==========================================================
// ; Description: Unsigned 16-bit multiplication with unsigned 32-bit result.
// ;                                                                         
// ; Input: 16-bit unsigned value in T1                                      
// ;        16-bit unsigned value in T2                                      
// ;        Carry=0: Re-use T1 from previous multiplication (faster)         
// ;        Carry=1: Set T1 (slower)                                         
// ;                                                                         
// ; Output: 32-bit unsigned value in PRODUCT                                
// ;                                                                         
// ; Clobbered: PRODUCT, X, A, C                                             
// ;                                                                         
// ; Allocation setup: T1,T2 and PRODUCT preferably on Zero-page.            
// ;                   square1_lo, square1_hi, square2_lo, square2_hi must be
// ;                   page aligned. Each table are 512 bytes. Total 2kb.    
// ;                                                                         
// ; Table generation: I:0..511                                              
// ;                   square1_lo = <((I*I)/4)                               
// ;                   square1_hi = >((I*I)/4)                               
// ;                   square2_lo = <(((I-255)*(I-255))/4)                   
// ;                   square2_hi = >(((I-255)*(I-255))/4)                   
// ;==========================================================
multiply_16bit_unsigned:
	// ; <T1 * <T2 = AAaa
	// ; <T1 * >T2 = BBbb
	// ; >T1 * <T2 = CCcc
	// ; >T1 * >T2 = DDdd
	// ;
	// ;       AAaa
	// ;     BBbb
	// ;     CCcc                                                
	// ; + DDdd                                                  
	// ; ----------                                              
	// ;   PRODUCT!                                              
	// ; Setup T1 if changed
	bcc !+
	lda T1+0
	sta sm1a+1
	sta sm3a+1
	sta sm5a+1
	sta sm7a+1
	eor #$ff
	sta sm2a+1
	sta sm4a+1
	sta sm6a+1
	sta sm8a+1
	lda T1+1
	sta sm1b+1
	sta sm3b+1
	sta sm5b+1
	sta sm7b+1
	eor #$ff
	sta sm2b+1
	sta sm4b+1
	sta sm6b+1
	sta sm8b+1
!:
	// ; Perform <T1 * <T2 = AAaa
	ldx T2+0
	sec
sm1a:	lda square1_lo,x
sm2a:	sbc square2_lo,x
	sta PRODUCT+0
sm3a:	lda square1_hi,x
sm4a:	sbc square2_hi,x
	sta _AA+1
	// ; Perform >T1_hi * <T2 = CCcc
	sec
sm1b:	lda square1_lo,x
sm2b:	sbc square2_lo,x
	sta _cc+1
sm3b:	lda square1_hi,x
sm4b:	sbc square2_hi,x
	sta _CC+1
	// ; Perform <T1 * >T2 = BBbb
	ldx T2+1
	sec
sm5a:	lda square1_lo,x 
sm6a:	sbc square2_lo,x
	sta _bb+1
sm7a:	lda square1_hi,x 
sm8a:	sbc square2_hi,x
	sta _BB+1
	// ; Perform >T1 * >T2 = DDdd
	sec
sm5b:	lda square1_lo,x
sm6b:	sbc square2_lo,x
	sta _dd+1
sm7b:	lda square1_hi,x
sm8b:	sbc square2_hi,x
	sta PRODUCT+3
	// ; Add the separate multiplications together
	clc
_AA:	lda #0
_bb:	adc #0
	sta PRODUCT+1
_BB:	lda #0
_CC:	adc #0
	sta PRODUCT+2
	bcc !+
	inc PRODUCT+3
	clc
!:
_cc:	lda #0
	adc PRODUCT+1
	sta PRODUCT+1
_dd:	lda #0
	adc PRODUCT+2
	sta PRODUCT+2
	bcc !+
	inc PRODUCT+3
!:	rts

// ;==========================================================
// ; Description: Signed 16-bit multiplication with signed 32-bit result.
// ;                                                                     
// ; Input: 16-bit signed value in T1                                    
// ;        16-bit signed value in T2                                    
// ;        Carry=0: Re-use T1 from previous multiplication (faster)     
// ;        Carry=1: Set T1 (slower)                                     
// ;                                                                     
// ; Output: 32-bit signed value in PRODUCT                              
// ;
// ; Clobbered: PRODUCT, X, A, C
// ;==========================================================
multiply_16bit_signed:
	jsr multiply_16bit_unsigned
	// ; Apply sign (See C=Hacking16 for details).
	lda T1+1
	bpl !+
	sec
	lda PRODUCT+2
	sbc T2+0
	sta PRODUCT+2
	lda PRODUCT+3
	sbc T2+1
	sta PRODUCT+3
!:	lda T2+1
	bpl !+
	sec
	lda PRODUCT+2
	sbc T1+0
	sta PRODUCT+2
	lda PRODUCT+3
	sbc T1+1
	sta PRODUCT+3
!:	rts

// ;==========================================================
// ; Description: Unsigned 16-bit multiplication with unsigned 32-bit result.
// ;                                                                         
// ; Input: 16-bit unsigned value in T1
// ;        32-bit unsigned value in T2
// ;                                                                         
// ; Output: 32-bit unsigned value in PRODUCT
// ;                                                                         
// ; Clobbered: PRODUCT, X, A, C
// ;==========================================================
multiply_32bit_unsigned:

	// $FFFF
	// $ffff.FFFF
	sec
	jsr multiply_16bit_unsigned

	lda PRODUCT+0
	sta PRODUCT32+0
	lda PRODUCT+1
	sta PRODUCT32+1
	lda PRODUCT+2
	sta PRODUCT32+2
	lda PRODUCT+3
	sta PRODUCT32+3

	lda T2+2
	sta T2+0
	lda T2+3
	sta T2+1

	// $FFFF
	// $FFFF.ffff

	clc
	jsr multiply_16bit_unsigned

	lda PRODUCT+0
	clc
	adc PRODUCT32+2
	sta PRODUCT32+2
	lda PRODUCT+1
	adc PRODUCT32+3
	sta PRODUCT32+3

	rts

// ;==========================================================

.align $100
square1_lo:	.fill 512,0
square1_hi:	.fill 512,0
square2_lo:	.fill 512,0
square2_hi:	.fill 512,0

//====================================================================
code_end:
//====================================================================
