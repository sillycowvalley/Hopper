; ######################## Int library ########################

  .ifndef STACK8
NEEDPOPTOPINT = 1
  .else
  .ifdef LONGS
NEEDPOPTOPINT = 1
  .endif
  .endif
  
; used by syscallIntToLong and when !STACK8
  .ifdef NEEDPOPTOPINT
popTOPInt:

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
  cmp #tByte
  beq popTOPIntAlwaysFits
  cmp #tUInt
  bne popTOPIntAssetType
  
  pha
  
  ; check in UInt is in range
  .ifndef PERMISSIVE
  lda TOPH
  asl           ; sign bit into carry
  bcc popTOPIntUIntFits
  pla
  
  lda (TSP)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda NEXTH
  jsr diagnosticOutHex
  lda NEXTL
  jsr diagnosticOutHex
  
  lda #$0D ; numeric type out of range / overflow
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  .endif ; PERMISSIVE
  
popTOPIntUIntFits:
  pla
  bra popTOPIntAlwaysFits
popTOPIntAssetType:  
  jsr assertInt
popTOPIntAlwaysFits:  
  .endif ; CHECKED
  
  .endif ; !STACK8
  
  rts
  .endif ; NEEDPOPTOPINT

  .ifndef STACK8
popNEXTInt:

  jsr decSP          ; MSB
  lda (SP)
  sta NEXTH
  jsr decSP          ; LSB
  lda (SP)
  sta NEXTL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  cmp #tByte
  beq popNEXTIntAlwaysFits
  cmp #tUInt
  bne popNEXTIntAssetType
  
  pha
  
  ; check in UInt is in range
  .ifndef PERMISSIVE
  lda NEXTH
  asl           ; sign bit into carry
  bcc popNEXTIntUIntFits
  pla
  
  lda (TSP)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda NEXTH
  jsr diagnosticOutHex
  lda NEXTL
  jsr diagnosticOutHex
  
  lda #$0D ; numeric type out of range / overflow
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  .endif
popNEXTIntUIntFits:
  pla
  bra popNEXTIntAlwaysFits
popNEXTIntAssetType:  
  jsr assertInt
popNEXTIntAlwaysFits:  
  .endif
  
  rts
  .endif ; !STACK8

  .ifdef LONGS
syscallIntToLong:

  jsr popTOPInt
  
  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate
  
  ldy #2
  lda TOPL
  sta (IDX), Y
  iny
  lda TOPH
  sta (IDX), Y
  
  ; http://forum.6502.org/viewtopic.php?f=2&t=6069
  ; sign extension
  
  asl           ; sign bit into carry
  lda #$00
  adc #$FF      ; C set:   A = $FF + C = $00
                ; C clear: A = $FF + C = $FF
  eor #$FF      ; Flip all bits and they all now match C
  
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  lda #tLong  
  jmp pushIDXExit
  
  .endif
  
  .ifndef STACK8
utilityIntGT:
  ; NEXT > TOP?
  ; NEXT - TOP > 0
  sec
  lda NEXTL
  sbc TOPL
  sta NEXTL
  lda NEXTH
  sbc TOPH
  sta NEXTH
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  stz TOPH
  
  bcs utilityIntGTNegativeOrZero
  ; 0 or positive
  lda NEXTL
  bne utilityIntGTPositive
  lda NEXTH
  beq utilityIntGTNegativeOrZero
utilityIntGTPositive:  
  ;true
  lda #1
  sta TOPL
utilityIntGTNegativeOrZero:  
  rts
  
utilityIntLT:
  ; NEXT < TOP?
  ; TOP - NEXT > 0
  sec
  lda TOPL
  sbc NEXTL
  sta TOPL
  lda TOPH
  sbc NEXTH
  sta TOPH
  
  asl           ; sign bit into carry
  
  ; false
  stz ACCL
  stz ACCH
  
  bcs utilityIntLTNegativeOrZero
  ; 0 or positive
  lda TOPL
  bne utilityIntLTPositive
  lda TOPH
  beq utilityIntLTNegativeOrZero
utilityIntLTPositive:  
  ;true
  lda #1
  sta ACCL
utilityIntLTNegativeOrZero:  
  lda ACCL
  sta TOPL
  lda ACCH
  sta TOPH
  rts
  
utilityIntGE:
  ; NEXT >= TOP?
  ; NEXT - TOP >= 0
  sec
  lda NEXTL
  sbc TOPL
  sta NEXTL
  lda NEXTH
  sbc TOPH
  sta NEXTH
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  stz TOPH
  
  bcs utilityIntGENegative
  ; 0 or positive
  ;true
  lda #1
  sta TOPL
utilityIntGENegative:  
  rts
  
utilityIntLE:
  ; NEXT <= TOP?
  ; TOP - NEXT >= 0
  sec
  lda TOPL
  sbc NEXTL
  sta TOPL
  lda TOPH
  sbc NEXTH
  sta TOPH
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  stz TOPH
  
  bcs utilityIntLENegative
  ; 0 or positive
  ;true
  lda #1
  sta TOPL
utilityIntLENegative:  
  rts
  .endif ; !STACK8
  
negateNEXT:
  ; NEXT = 0 - NEXT
  sec
  lda #0
  sbc NEXTL
  sta NEXTL
  lda #0
  sbc NEXTH
  sta NEXTH
  rts

negateTOP:
  ; TOP = 0 - TOP
  sec
  lda #0
  sbc TOPL
  sta TOPL
  lda #0
  sbc TOPH
  sta TOPH
  rts
  
utilityDoSigns:
  phx

  ldx #0
  lda NEXTH
  asl ; sign bit into carry
  bcc utilityMULINEXTIsPositive ; +ve
  inx ; count the -ve
  jsr negateNEXT ; NEXT = -NEXT
utilityMULINEXTIsPositive:
  lda TOPH
  asl ; sign bit into carry
  bcc utilityMULITOPIsPositive ; +ve
  inx ; count the -ve
  jsr negateTOP ; TOP = -TOP
utilityMULITOPIsPositive:
  stx fSIGN ; store the sign count
  
  plx
  rts

utilityMULI:

  jsr utilityDoSigns
  jsr utilityMUL
  
  lda fSIGN ; load the sign count
  cmp #1
  bne utilityMULISignsEven
  jsr negateTOP ; TOP = -TOP
utilityMULISignsEven:
  rts
  
utilityDIVI:
  jsr utilityDoSigns
  jsr utilityDIV
  
  lda fSIGN ; load the sign count
  cmp #1
  bne utilityDIVISignsEven
  jsr negateNEXT ; NEXT = -NEXT
utilityDIVISignsEven:
  rts
  
utilityMODI
  ; https://llx.com/Neil/a2/mult.html
  ; supporting floored division remainder is always positive
  ;
  ;   dividend = divisor * quotient + remainder
  ;    10 /  3 = q  3, r  1
  ;   -10 / -3 = q  3, r -1
  ;   -10 /  3 = q -3, r -1
  ;    10 / -3 = q -3, r -11 ?!
  ;
  
  jsr utilityDoSigns
  jsr utilityDIVMOD

  ; always leave remainder ACC as positive
  rts
  
  

  .ifdef LISTS
syscallIntToBytes:

  jsr popTOPInt
  jsr listUtilityCreateList ; returns new list as IDX (zeroes fields too)
  
  stz lLENGTHH
  lda #2
  sta lLENGTHL
  jsr listUtilityStoreLengthToIDX
  
  lda #tByte
  ldy #4
  sta (IDX), Y ; list item type
  
  ; save pointer to list
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; save the pFirst pointer
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH
  
  ; pFirst offset in tList
  clc
  lda fDESTINATIONADDRESSL  ; LSB
  adc #listpFirstOffset
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  ; create 2x list items using the data from TOP
  lda TOPL
  sta fVALUEL
  stz fVALUEH
  
  lda #tByte
  jsr listUtilityCreateValueItem
  
  ldy #1
  lda fITEML
  sta (fDESTINATIONADDRESS)
  lda fITEMH
  sta (fDESTINATIONADDRESS), Y
  
  ; save the pNext pointer
  lda fITEML
  sta fDESTINATIONADDRESSL
  lda fITEMH
  sta fDESTINATIONADDRESSH
  
  ; pNext offset tListItem:
  ; 4x incDESTINATIONADDRESS
  clc
  lda fDESTINATIONADDRESSL  ; LSB
  adc #listItempNextOffset
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  
  
  lda TOPH
  sta fVALUEL
  stz fVALUEH
  lda #tByte
  jsr listUtilityCreateValueItem
  
  ldy #1
  lda fITEML
  sta (fDESTINATIONADDRESS)
  lda fITEMH
  sta (fDESTINATIONADDRESS), Y
  
  ; save the pNext pointer
  lda fITEML
  sta fDESTINATIONADDRESSL
  lda fITEMH
  sta fDESTINATIONADDRESSH
  
  ; pNext offset tListItem:
  ; 4x incDESTINATIONADDRESS
  clc
  lda fDESTINATIONADDRESSL  ; LSB
  adc #listItempNextOffset
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  ; IDX points to list
  pla
  sta IDXH
  pla
  sta IDXL

  lda #tList
  jmp pushIDXExit
  .endif  
  