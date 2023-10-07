; ######################## Pair functionality ########################

; Pair memory map:
;   0000 heap allocator size
;   10   type = tPair
;   00   reference count
;   xx   kType
;   xx   vType
;   xxxx pKey   - always a variant
;   xxxx pData  - always a variant


utilityPairNew:
  ; dKTYPE and dVType
  ;   result in IDX
  lda #6
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tPair
  jsr gcCreate
  
  ldy #2
  lda dKTYPE
  sta (IDX), Y  ; key type
  iny
  lda dVTYPE
  sta (IDX), Y  ; value type
  iny
  lda #0
  sta (IDX), Y ; pKey
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y ; pData
  iny
  sta (IDX), Y
  
  rts
  
syscallPairNew:

  .ifdef STACK8
  
  ; value type -> stack
  ldx SP8
  dex
  dex
  lda HopperValueStack, X
  sta dVTYPE
  
  ; key type -> stack
  dex
  dex
  lda HopperValueStack, X
  sta dKTYPE
  stx SP8
  
  .else ; STACK8
  
  ; value type -> stack
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta dVTYPE
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; key type -> stack
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta dKTYPE
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif ; !STACK8
  
  jsr utilityPairNew
  
  lda #tPair  
  jmp pushIDXExit
  
  
    
utilityPairClear:
  ; pair is in IDX
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ldy #4 ; key value
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  lda IDXH
  bne utilityPairClearKey
  lda IDXL
  bne utilityPairClearKey
  bra utilityPairKeyNull
  
utilityPairClearKey:

  .ifdef MEMDEBUG2
  
  lda #"p"
  jsr diagnosticOutChar
  lda #"k"
  jsr diagnosticOutChar
  
  .endif
  
  ; release key
  jsr gcRelease
  
utilityPairKeyNull:
  pla
  sta IDXH
  pla
  sta IDXL
  
  ldy #4 ; key = nullptr
  lda #0
  sta (IDX), Y
  iny
  lda (IDX), Y
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ldy #6
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  lda IDXH
  bne utilityPairClearValue
  lda IDXL
  bne utilityPairClearValue
  bra utilityPairValueNull
  
utilityPairClearValue:

  .ifdef MEMDEBUG2
  
  lda #"p"
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  
  .endif
  
  ; release value
  jsr gcRelease
  
utilityPairValueNull:
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  ldy #6 ; value = nullptr
  lda #0
  sta (IDX), Y
  iny
  lda (IDX), Y
  rts
  
syscallPairKey:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else ; STACK8
  
  ; this -> IDX
  jsr decSP
  lda (SP)
  sta IDXH
  jsr decSP
  lda (SP)
  sta IDXL
  
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertPair
  .endif
  .endif ; !STACK8
  
  jsr utilityPairKey ; pair IDX -> key IDY, dKTYPE, munts IDX
  
  jsr releaseSP ; release 'this'
  
  .ifdef STACK8
  
  ldx SP8
  lda IDYL
  sta HopperValueStack, X
  lda dKTYPE
  sta HopperTypeStack, X
  inx
  lda IDYH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  
  .else
  
  lda IDYL
  sta (SP)
  jsr incSP          ; LSB
  lda IDYH
  sta (SP)
  jsr incSP          ; MSB
  lda dKTYPE
  sta (TSP)
  jsr incTSP
  
  .endif
  
  jmp nextInstruction
  
syscallPairValue:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else ; STACK8
  ; this -> IDX
  jsr decSP
  lda (SP)
  sta IDXH
  jsr decSP
  lda (SP)
  sta IDXL
  
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertPair
  .endif
  .endif ; !STACK8
  
  jsr utilityPairValue ; pair IDX -> value IDY, dVTYPE, munts IDX
  
  jsr releaseSP ; release 'this'
  
  ;jsr diagnosticOutNewLine
  ;lda #"V"
  ;jsr diagnosticOutChar
  ;lda dVTYPE
  ;jsr diagnosticOutHex
  ;lda #":"
  ;jsr diagnosticOutChar
  ;lda IDYH
  ;jsr diagnosticOutHex
  ;lda IDYL
  ;jsr diagnosticOutHex
  
  .ifdef STACK8
  
  ldx SP8
  lda IDYL
  sta HopperValueStack, X
  lda dVTYPE
  sta HopperTypeStack, X
  inx
  lda IDYH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  
  .else
  
  lda IDYL
  sta (SP)
  jsr incSP          ; LSB
  lda IDYH
  sta (SP)
  jsr incSP          ; MSB
  lda dVTYPE
  sta (TSP)
  jsr incTSP
  
  .endif
  jmp nextInstruction
  
utilityPairKey:
  ; pair IDX -> key IDY, dKTYPE, munts IDX
  ldy #2
  lda (IDX), Y
  sta dKTYPE
  
  ldy #4
  lda (IDX), Y
  sta IDYL
  iny
  lda (IDX), Y
  sta IDYH
  
  lda dKTYPE
  cmp #tVariant
  bne utilityPairKeyNotVariant
  
  ; get the type from the variant
  ldy #2
  lda (IDY), Y
  sta dKTYPE
  
utilityPairKeyNotVariant:  
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc utilityPairKeyValueType
  ; key is a reference type
  
  ; type is in A
  ; reference type to clone is at IDY
  lda dKTYPE
  jsr cloneIDY ; IDY -> IDX
  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  bra utilityPairKeyExit
  
utilityPairKeyValueType:
  ; get the value from the variant
  ldy #3
  lda (IDY), Y
  tax
  iny
  lda (IDY), Y
  sta IDYH
  stx IDYL
  
utilityPairKeyExit:
  rts
  
utilityPairValue:
  ; pair IDX -> value IDY, dVTYPE
  ldy #3
  lda (IDX), Y
  sta dVTYPE
  
  ldy #6
  lda (IDX), Y
  sta IDYL
  iny
  lda (IDX), Y
  sta IDYH
  
  lda dVTYPE
  cmp #tVariant
  bne utilityPairValueNotVariant
  
  ; get the type from the variant
  ldy #2
  lda (IDY), Y
  sta dVTYPE
  
utilityPairValueNotVariant: 
  
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc utilityPairValueValueType
  ; value is a reference type
  
  ; type is in A
  ; reference type to clone is at IDY
  lda dVTYPE
  jsr cloneIDY ; IDY -> IDX
  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  bra utilityPairValueExit
  
utilityPairValueValueType:
  ; get the value from the variant
  ldy #3
  lda (IDY), Y
  tax
  iny
  lda (IDY), Y
  sta IDYH
  stx IDYL
  
utilityPairValueExit:
  rts
