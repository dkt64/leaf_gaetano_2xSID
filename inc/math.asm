// ;==========================================================
// ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// ; Seriously fast multiplication by JackAsser
// ; https://codebase64.org/doku.php?id=base:seriously_fast_multiplication
// ;
// ; Implementatin of "multiply_32bit_unsigned" by DKT
// ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// ;==========================================================

.const 	T1 		= $10 // 16-bit input
.const 	T2 		= $12 // 16-bit input
.const 	PRODUCT 		= $14 // 32-bit result

.const 	T32_1 		= $18 // 32-bit input
.const 	T32_2 		= $1c // 32-bit input
.const 	PRODUCT32 	= $20 // 32-bit result

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
// ; Description: Unsigned 32-bit multiplication with unsigned 32-bit result.
// ;                                                                         
// ; Input: 32-bit unsigned value in T32_1
// ;        32-bit unsigned value in T32_2
// ;                                                                         
// ; Output: 32-bit unsigned value in PRODUCT
// ;                                                                         
// ; Clobbered: PRODUCT, X, A, C
// ;==========================================================
multiply_32bit_unsigned:

	lda T32_1+0
	sta T1+0
	lda T32_1+1
	sta T1+1
	lda T32_2+0
	sta T2+0
	lda T32_2+1
	sta T2+1
	sec
	jsr multiply_16bit_unsigned
	lda T32_2+2
	sta T2+0
	lda T32_2+3
	sta T2+1
	sec
	jsr multiply_16bit_unsigned

	rts

// ;==========================================================

.align $100
square1_lo:	.fill 512,0
square1_hi:	.fill 512,0
square2_lo:	.fill 512,0
square2_hi:	.fill 512,0
