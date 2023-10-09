; ######################## UInt library ########################

uWIDE0 = U0
uWIDE1 = U1
uWIDE2 = U2
uWIDE3 = U3

  .ifndef STACK8
NEEDUINTGT = 1
  .else
  .ifdef DICTIONARIES
NEEDUINTGT = 1
  .endif
  .endif

; used if not STACK8 and also by DICTIONARIES (checkCapacity)
  .ifdef NEEDUINTGT
utilityUIntGT:
  pha
  phx
  ldx #0 ; NEXT <= TOP
  lda NEXTH
  cmp TOPH
  bne doneUIntGT
  lda NEXTL
  cmp TOPL
doneUIntGT:
  ; http://6502.org/tutorials/compare_instructions.html
  beq phUIntGT ; NEXT == TOP (not >)
  bcc phUIntGT ; NEXT <  TOP (not >)
  ldx #1   ; NEXT > TOP
phUIntGT:
  stx TOPL
  stz TOPH
  plx
  pla
  rts
  .endif

  .ifndef STACK8
NEEDUINTCOMPARES = 1
  .else
  .ifdef CHECKED
NEEDUINTCOMPARES = 1
  .else
  .ifdef STRINGS
NEEDUINTCOMPARES = 1
  .endif
  .endif
  .endif

  .ifdef NEEDUINTCOMPARES
  ; used by the CHECKED build for STACK8 in syscallStringInsertChar. Also used by StringIndexOf (not just CHECKED)
utilityUIntLE:
  pha
  phx
  ldx #1 ; NEXT <= TOP
  lda NEXTH
  cmp TOPH
  bne doneUIntLE
  lda NEXTL
  cmp TOPL
doneUIntLE:
  ; http://6502.org/tutorials/compare_instructions.html
  beq phUIntLE ; NEXT == TOP (not >)
  bcc phUIntLE ; NEXT <  TOP (not >)
  ldx #0   ; NEXT > TOP
phUIntLE:
  stx TOPL
  stz TOPH
  plx
  pla
  rts
  
; used by the CHECKED build for STACK8 for range checking in Arrays and Strings
utilityUIntLT:
  pha
  phx
  ldx #1 ; NEXT < TOP
  lda NEXTH
  cmp TOPH
  bne doneUIntLT
  lda NEXTL
  cmp TOPL
doneUIntLT:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc phUIntLT ; NEXT < TOP
  ldx #0   
phUIntLT:
  stx TOPL
  stz TOPH
  plx
  pla
  rts
  .endif
  
  .ifndef STACK8
utilityUIntGE:
  pha
  phx
  ldx #0 ; NEXT < TOP
  lda NEXTH
  cmp TOPH
  bne doneUIntGE
  lda NEXTL
  cmp TOPL
doneUIntGE:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc phUIntGE ; NEXT < TOP
  ldx #1   
phUIntGE:
  stx TOPL
  stz TOPH
  plx
  pla
  rts
  
utilityUIntEQ:
  pha
  phx
  ldx #0 ; !=
  lda NEXTL
  cmp TOPL
  bne phUIntEQ
  lda NEXTH
  cmp TOPH
  bne phUIntEQ
  ldx #1 ; ==
phUIntEQ:
  stx TOPL
  stz TOPH
  plx
  pla
  rts
  
utilityUIntNE:
  pha
  phx
  ldx #1 ; !=
  lda NEXTL
  cmp TOPL
  bne phUIntNE
  lda NEXTH
  cmp TOPH
  bne phUIntNE
  ldx #0 ; ==
phUIntNE:
  stx TOPL
  stz TOPH
  plx
  pla
  rts
  .endif ; !STACK9
  
syscallUIntToInt:
  jsr popTOPUInt
  
  .ifdef CHECKED
  lda TOPH
  and #$80
  beq syscallUIntToIntInRange
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte "?UIntToInt+", 0
  .endif
  jmp throwToys
syscallUIntToIntInRange
  .endif
  
  lda #tInt  
  jmp pushTOPExit

  .ifdef LONGS
syscallUIntToLong:
  jsr popTOPUInt
  
  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ldy #2
  lda TOPL
  sta (IDX), Y
  iny
  lda TOPH
  sta (IDX), Y
  iny
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  lda #tLong  
  jmp pushIDXExit
  
  .endif
  .ifdef FASTINTS
utilityMULTOP16:
  asl TOPL
  rol TOPH
utilityMULTOP8:
  asl TOPL
  rol TOPH
utilityMULTOP4:
  asl TOPL
  rol TOPH
utilityMULTOP2:
  asl TOPL
  rol TOPH
  rts
  .endif
  
utilityMUL:
  .ifdef FASTINTS
  ; TOP = NEXT * TOP
  
  lda NEXTH
  bne utilityMULNextNonTrivial ; MSB not zero?

  lda NEXTL
  cmp #1
  bne notUtilityMUL1_2
  ; just return TOP
  rts
notUtilityMUL1_2:

  lda NEXTL
  cmp #2
  beq utilityMULTOP2

  lda NEXTL
  cmp #4
  beq utilityMULTOP4

  lda NEXTL
  cmp #8
  beq utilityMULTOP8

  lda NEXTL
  cmp #16
  beq utilityMULTOP16
  
utilityMULNextNonTrivial:

  lda TOPH
  bne utilityMULTOPNonTrivial ; MSB not zero?

  lda TOPL
  cmp #1
  bne notUtilityMUL1
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  rts
notUtilityMUL1:

  lda TOPL
  cmp #2
  bne notUtilityMUL2
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  bra utilityMULTOP2
notUtilityMUL2:

  lda TOPL
  cmp #4
  bne notUtilityMUL4
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  bra utilityMULTOP4
notUtilityMUL4:

  lda TOPL
  cmp #8
  bne notUtilityMUL8
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  bra utilityMULTOP8
notUtilityMUL8:

  lda TOPL
  cmp #16
  bne notUtilityMUL16
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  jmp utilityMULTOP16
notUtilityMUL16:

utilityMULTOPNonTrivial:

  lda TOPH
  bne utilityMULNot8x8
  lda NEXTH
  bne utilityMULNot8x8

; mul 8x8 16 bit result for when you can't afford big tables
; by djmips 
;
; inputs are mul1 and X.  mul1 and mul2 should be zp locations
; A should be zero entering but if you want it will factor in as 1/2 A added to the result.
;
; output is 16 bit in A : mul1   (A is high byte)
;
; length = 65 bytes 
; total cycles worst case = 113
; total cycles best case = 97
; avg = 105
; inner loop credits Damon Slye CALL APPLE, JUNE 1983, P45-48.
;
; https://codebase64.org/doku.php?id=base:8bit_multiplication_16bit_product_fast_no_tables

     ldx TOPL
     dex          ; decrement TOPL because we will be adding with carry set for speed (an extra one)
     stx TOPL
     ror NEXTL
     bcc b1_8x8
     adc TOPL
b1_8x8:
     ror
     ror NEXTL
     bcc b2_8x8
     adc TOPL
b2_8x8:
     ror
     ror NEXTL
     bcc b3_8x8
     adc TOPL
b3_8x8:
     ror
     ror NEXTL
     bcc b4_8x8
     adc TOPL
b4_8x8:
     ror
     ror NEXTL
     bcc b5_8x8
     adc TOPL
b5_8x8:
     ror
     ror NEXTL
     bcc b6_8x8
     adc TOPL
b6_8x8:
     ror
     ror NEXTL
     bcc b7_8x8
     adc TOPL
b7_8x8:
     ror
     ror NEXTL
     bcc b8_8x8
     adc TOPL
b8_8x8:  
     ror
     ror NEXTL
     sta TOPH
     lda NEXTL
     sta TOPL
     rts
  
  
utilityMULNot8x8:
  
  .endif ; FASTINTS
  
  ; https://llx.com/Neil/a2/mult.html
  ;Initialize RESULT to 0
  lda #0
  sta uWIDE2
  ldx #16      ;There are 16 bits in TOP
opcodeMUL1:
  lsr TOPH   ;Get low bit of TOP
  ror TOPL
  bcc opcodeMUL2       ;0 or 1?
  tay          ;If 1, add NUM1 (hi byte of RESULT is in A)
  clc
  lda NEXTL
  adc uWIDE2
  sta uWIDE2
  tya
  adc NEXTH
opcodeMUL2:
  ror A        ;"Stairstep" shift
  ror uWIDE2
  ror uWIDE1
  ror uWIDE0
  dex
  bne opcodeMUL1
  sta uWIDE3
  
  lda uWIDE0
  sta TOPL
  lda uWIDE1
  sta TOPH
  rts

  .ifdef FASTINTS
utilityDIV10:

  ; NEXT = NEXT / 10
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  bra startBigDivide
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    UNSIGNED DIVIDE BY 10 (16 BIT)
;    111 cycles (max), 96 bytes
; https://forums.atariage.com/blogs/entry/11044-16-bit-division-fast-divide-by-10/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TensRemaining:
    .byte 0,25,51,76,102,128,153,179,204,230
ModRemaining:
    .byte 0,6,2,8,4,0,6,2,8,4
overflowFound:
    cmp    #4                    ;  We have overflowed, but we can apply a shortcut.
    lda    #25                   ;  Divide by 10 will be at least 25, and the
    bne    finishLowTen          ;  carry is set when higher for the next addition.                                 ; always branch
startBigDivide:
    lda    TOPH
    sta    ACCL
    lsr        
    adc    #13 
    adc    ACCL
    ror        
    lsr        
    lsr        
    adc    ACCL
    ror        
    adc    ACCL
    ror        
    lsr        
    and    #$7C                  ; AND'ing here...
    sta    ACCL                  ; and saving result as highTen (times 4)
    lsr        
    lsr        
    sta    NEXTH
    adc    ACCL                  ; highTen (times 5)
    asl                          ; highTen (times 10)
    sbc    TOPH
    eor    #$FF
    tay                          ; mod 10 result!
    lda    TensRemaining,Y       ; Fill the low byte with the tens it should
    sta    NEXTL                 ; have at this point from the high byte divide.
    lda    TOPL
    adc    ModRemaining,Y          ;4  @69
    bcs    overflowFound
    sta    ACCL
    lsr        
    adc    #13 
    adc    ACCL
    ror        
    lsr        
    lsr        
    adc    ACCL
    ror        
    adc    ACCL
    ror        
    lsr        
    lsr        
    lsr        
    clc        
finishLowTen:
    adc    NEXTL
    sta    NEXTL
  
  rts
  .endif ; FASTINTS
  
utilityDIV:
  ; NEXT = NEXT (dividend=result) / TOP (divisor)
  
  .ifdef FASTINTS
  
  lda TOPH
  bne utilityDIVMOD ; MSB of divisor not zero?
  lda TOPL
  
  cmp #1
  bne notUtilityDIV1
  rts
notUtilityDIV1:
  
  cmp #2
  bne notUtilityDIV2
  lsr NEXTH        ; / 2
  ror NEXTL       
  rts
notUtilityDIV2:

  cmp #4
  bne notUtilityDIV4
  lsr NEXTH        ; / 2
  ror NEXTL       
  lsr NEXTH        ; / 2
  ror NEXTL       
  rts
notUtilityDIV4:

  cmp #10
  bne notUtilityDIV10
  jmp utilityDIV10
notUtilityDIV10:

  cmp #100
  bne notUtilityDIV100
  jsr utilityDIV10
  jmp utilityDIV10
notUtilityDIV100:
  
  cmp #50
  bne notUtilityDIV50
  jsr utilityDIV10 ; / 10
  asl NEXTL        ; * 2
  rol NEXTH       
  jmp utilityDIV10 ; / 10
notUtilityDIV50:
  .endif ; FASTINTS
  
  ; fall through
  
utilityDIVMOD:
  ; NEXT = NEXT (dividend=result) / TOP (divisor)
  ; ACC (remainder)
  
  ; https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
  ; https://llx.com/Neil/a2/mult.html

  stz ACCL
  stz ACCH
  ldx #16
  
divLoop:
  asl NEXTL
  rol NEXTH
  rol ACCL
  rol ACCH
  lda ACCL
  sec
  sbc TOPL
  tay
  lda ACCH
  sbc TOPH
  bcc divSkip
  
  sta ACCH
  sty ACCL
  inc NEXTL
  
divSkip:
  dex
  bne divLoop
  rts
  
popTOPUInt:
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta TOPH
  dex
  lda HopperValueStack, X
  sta TOPL
  stx SP8
  
  .else ; STACK8

  jsr decSP          ; MSB
  lda (SP)
  sta TOPH
  jsr decSP          ; LSB
  lda (SP)
  sta TOPL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif ; CHECKED
  
  .endif ; !STACK8
  rts