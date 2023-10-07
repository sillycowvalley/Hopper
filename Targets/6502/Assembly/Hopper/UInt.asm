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
  
utilityMUL:
  ; TOP = NEXT * TOP
  
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
  
utilityDIV:
  ; TOP = NEXT (dividend=result) / TOP (divisor)
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