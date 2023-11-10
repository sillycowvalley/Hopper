; ######################## GC wrapper around heap manager ########################

gcCreate:
  ; create new object on heap (reference count 1)
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  phx
  tax
  
  lda IDYL
  pha
  lda IDYH
  pha
  
  lda ACCL
  pha
  lda ACCH
  pha
  
  .ifdef MEMDEBUG
  lda #"A"
  jsr diagnosticOutChar
  lda fSIZEH
  jsr diagnosticOutHex
  lda fSIZEL
  jsr diagnosticOutHex
  .endif
  
  clc
  lda fSIZEL  ; LSB
  ;adc #2  ; allocate enough space for type and reference count (2 more bytes)
  ;adc #$09 ; 8 + 1, ok do that, but also round up to the nearest 8 byte boundary to reduce fragmentation
  adc #$05 ; 4 + 1, ok do that, but also round up to the nearest 4 byte boundary to reduce fragmentation
  sta ACCL
  lda fSIZEH  ; MSB
  adc #0
  sta ACCH
  
  lda ACCL
  ;and #$F8
  and #$FC
  sta ACCL
  
  .ifdef MEMDEBUG
  ;jsr diagnosticOutNewLine
  lda #":"
  jsr diagnosticOutChar
  lda ACCH
  jsr diagnosticOutHex
  lda ACCL
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  .endif
  
  .ifdef MEMDEBUG
  lda #" "
  jsr diagnosticOutChar
  lda #"c"
  jsr diagnosticOutChar
  txa
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda ACCH
  beq gcCreateMSBZero
  jsr diagnosticOutHex
gcCreateMSBZero:  
  lda ACCL
  jsr diagnosticOutHex
  .endif
  
  
  ; size is in ACC
  ; return address in IDX
  jsr memoryAllocate
  
  ldy #0
  txa
  sta (IDX), Y ; type
  iny
  lda #1
  sta (IDX), Y ; reference count starts at 1
  
  .ifdef MEMDEBUG
  ;jsr diagnosticOutNewLine
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;jsr memoryHeapWalk
  .endif
  
  pla
  sta ACCH
  pla
  sta ACCL
  
  pla
  sta IDYH
  pla
  sta IDYL
  
  .ifdef MEMDEBUG2
  
  jsr diagnosticOutNewLine
  lda #"C"
  jsr diagnosticOutChar
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  
  .endif
  
  plx
  rts
  
  .ifdef CHECKED
addReference:
  ; IDX points to stack slot
  ; munts A and Y (in gcAddReference)
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; (IDX) -> IDX
  ldy #0
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL

  jsr gcAddReference
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  rts
  .endif
  
; increment the reference count by 1
gcAddReference:
  ; address in IDX
  ldy #1
  lda (IDX), Y ; reference count
  inc
  ; check for overflow
  
  .ifdef CHECKED
  bne addReferenceNotZero
  
  .ifndef NODIAGNOSTICS
  ; $FF + 1 = $00
  jsr diagnosticOutNewLine
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda (IDX)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?GCRef+", 0
  .endif
  jmp throwToys
  
addReferenceNotZero:  
  .endif
  
  sta (IDX), Y
  
  rts
  
releaseIDXIDY:
  ; IDY points to type stack slot
  ; IDX points to stack slot
  pha
  phx
  
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  .ifdef MEMDEBUG2
  
  lda #"i"
  jsr diagnosticOutChar
  lda #"3"
  jsr diagnosticOutChar
  
  .endif
  
  jsr rawReleaseIDXIDY
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  plx
  pla
  rts
  
rawReleaseSP:
  ; does not preserve IDX and IDY
  
  .ifdef MEMDEBUG2
  
  jsr diagnosticOutNewLine
  lda #"s"
  jsr diagnosticOutChar
  
  .endif
  
  ; (SP) -> IDX
  .ifdef STACK8
  
  ldx SP8
  lda HopperValueStack, X
  STA IDXL
  inx
  lda HopperValueStack, X
  sta IDXH
  
  .else
  
  lda (SP)
  STA IDXL
  ldy #1
  lda (SP), Y
  sta IDXH
  
  .endif
  
  
  ; TSP -> IDY
  .ifdef CHECKED
  .ifdef STACK8
  
  lda #>HopperTypeStack
  sta IDYH
  lda SP8
  sta IDYL
  
  .else
  
  lda TSPH
  sta IDYH
  lda TSPL
  sta IDYL
  
  .endif
  .endif
  
  bra rawReleaseIDXIDY2
  
rawReleaseIDXIDY:
  ; does not preserve IDX and IDY
  ; IDY points to type stack slot (.ifdef CHECKED)
  ; IDX points to stack slot 'SP'
  
  .ifdef CHECKED
  ;lda IDXL
  ;sta dSCRATCHL
  ;lda IDXH
  ;sta dSCRATCHH
  .endif
  
  ; (IDX) -> IDX
  lda (IDX)
  tax
  ldy #1
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
rawReleaseIDXIDY2:
  
  ; IDY points to type stack slot (.ifdef CHECKED)
  ; IDX points to object IN the stack slot '(SP)'

  .ifdef CHECKED
  ; does the type in the stack slot match the type of the object to release?
  lda (IDY)
  cmp (IDX)
  beq typesMatchForRelease
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutNewLine
  lda (IDY)
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda (IDX)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  ;lda #":"
  ;jsr diagnosticOutChar
  ;lda dSCRATCHH
  ;jsr diagnosticOutHex
  ;lda dSCRATCHL
  ;jsr diagnosticOutHex
  
  jsr diagnosticOutString
  .byte "?GCRel", 0
  .endif
  jmp throwToysNoStack
  
typesMatchForRelease:
  .endif
  
  ; fall through to gcRelease
  
  .ifdef MEMDEBUG2
  
  lda #"^"
  jsr diagnosticOutChar
  
  .endif
  
; decrement reference count, if zero, free
gcRelease:
  .ifdef MEMDEBUG2
  
  lda #"<"
  jsr diagnosticOutChar
  lda #"R"
  jsr diagnosticOutChar
  lda (IDX)
  ldy #1
  lda (IDX), Y
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  
  .endif

  ; address in IDX
  ldy #1
  lda (IDX), Y ; reference count
  
  .ifdef CHECKED
  bne releaseNotZero
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutNewLine
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda (IDX)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?GCRef-", 0
  .endif
  jmp throwToys
  
releaseNotZero:  
  .endif
  dec
  sta (IDX), Y
  
  ; if zero, free
  ;cmp #0
  beq notNoReleaseFree
  jmp noReleaseFree
notNoReleaseFree:
  
  .ifdef MEMDEBUG
  jsr diagnosticOutNewLine
  lda #"F"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  
  lda #"T"
  jsr diagnosticOutChar
  ldy #0
  lda (IDX), Y ; type
  jsr diagnosticOutHex
  
  ;jsr memoryHeapWalk
  .endif
  
  ldy #0
  lda (IDX), Y ; type
  
  .ifdef LISTS
  cmp #tList
  bne gcReleaseNotList

  jsr listUtilityClear
  bra gcReleaseCompoundDone
  
gcReleaseNotList:
  .endif
  
  .ifdef DICTIONARIES
  cmp #tDictionary
  bne gcReleaseNotDictionary

  jsr utilityDictionaryClear
  bra gcReleaseCompoundDone
  
gcReleaseNotDictionary:
  cmp #tPair
  bne gcReleaseNotPair
  
  jsr utilityPairClear
  bra gcReleaseCompoundDone
  
gcReleaseNotPair:
  .endif

  cmp #tVariant
  bne gcReleaseNotVariant
  
  ldy #2
  lda (IDX), Y ; type contained by variant
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc gcReleaseCompoundDone ; value variant
  
  .ifndef NODIAGNOSTICS
  lda #"V"
  jsr diagnosticOutChar
  ldy #2
  lda (IDX), Y ; type contained by variant
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  
  jsr diagnosticOutString
  .byte $0D, "?release tVariant", 0
  .endif
  jsr throwToys ; TODO : release tVariant
  
gcReleaseNotVariant:

gcReleaseCompoundDone:

  jsr memoryFree
  
  .ifdef MEMDEBUG
  jsr diagnosticOutNewLine
  lda #"F"
  jsr diagnosticOutChar
  jsr memoryHeapWalk
  .endif
  
noReleaseFree:
  .ifdef MEMDEBUG2
  
  lda #"R"
  jsr diagnosticOutChar
  lda #">"
  jsr diagnosticOutChar
  
  .endif
  rts



cloneIDY:
  ; type is in A
  ; reference type to clone is at IDY, resulting clone in IDX
  tax
  
  ; cloneList can go recursive:       preserve lCURRENT, lNEXT, IDY
  
  lda lCURRENTL
  pha
  lda lCURRENTH
  pha
  lda lNEXTL
  pha
  lda lNEXTH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  txa
  .ifdef LONGS
  cmp #tLong
  bne notCloneLong
  jsr cloneLong
  bra cloneSuccess
notCloneLong:
  .endif
  
  .ifdef STRINGS
  cmp #tString
  bne notCloneString
  jsr stringUtilityClone
  bra cloneSuccess
notCloneString:
  .endif

  .ifdef ARRAYS
  cmp #tArray
  bne notCloneArray
  jsr cloneArray
  bra cloneSuccess
notCloneArray:
  .endif
  
  .ifdef LISTS
  cmp #tList
  bne notCloneList
  jsr listUtilityClone
  bra cloneSuccess
notCloneList:
  .endif
  
  .ifdef DICTIONARIES
  cmp #tDictionary
  bne notCloneDictionary
  jsr cloneDictionary
  bra cloneSuccess
notCloneDictionary:
  .endif
  
  cmp #tVariant
  bne notCloneVariant
  jsr cloneVariant
  bra cloneSuccess
notCloneVariant:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?Clone", 0
  .endif
  jmp throwToys
cloneSuccess:

  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta lNEXTH
  pla
  sta lNEXTL
  pla
  sta lCURRENTH
  pla
  sta lCURRENTL

  rts

cloneSP:
  phy
  phx
  
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  ; SP is at reference type in question
  ; TSP is at type
  
  ; make a clone of valueStack[SP] -> [IDX]
  
  .ifdef STACK8
  ldx SP8
  lda HopperValueStack, X
  sta IDYL
  lda HopperTypeStack, X
  tay
  inx
  lda HopperValueStack, X
  sta IDYH
  tya
  .else
  ldy #1
  lda (SP)
  sta IDYL
  lda (SP), Y
  sta IDYH
  
  lda (TSP)
  .endif
  
  ; type is in A
  ; reference type to clone is at IDY
  jsr cloneIDY
  
  ; preserves IDX, IDY, SP, TSP
  jsr releaseSP
  
  ; IDX -> [SP]  
  .ifdef STACK8
  ldx SP8
  lda IDXL
  sta HopperValueStack, X
  inx
  lda IDXH
  sta HopperValueStack, X
  .else
  ldy #1
  lda IDXH
  sta (SP), Y
  lda IDXL
  sta (SP)
  .endif
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  plx
  ply
  rts
  

  .ifdef STACK8

releaseSPwithOFFSET:

  asl mfOFFSET ; *= 2
  
  .ifdef CHECKED
  
  lda #>HopperTypeStack
  sta IDYH
  clc
  lda SP8
  adc mfOFFSET
  sta IDYL
  
  .endif
  
  lda #>HopperValueStack
  sta IDXH
  clc
  lda SP8
  adc mfOFFSET
  sta IDXL
  
  jsr rawReleaseIDXIDY

  rts
  
  .else
  
releaseSPwithOFFSET:
  
   
  .ifdef CHECKED
  
  clc
  lda TSPL
  adc mfOFFSET
  sta IDYL
  lda TSPH
  adc #0
  sta IDYH
  
  .endif
  
  asl mfOFFSET ; *= 2
  
  clc
  lda SPL
  adc mfOFFSET
  sta IDXL
  lda SPH
  adc #0
  sta IDXH
  
  jsr rawReleaseIDXIDY

  rts
  .endif
  
releaseSP:
  pha
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  stz mfOFFSET ; SP
  jsr releaseSPwithOFFSET
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  pla
  rts
  
releaseSPNEXT:

  pha
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  lda #1 ; SPNEXT
  sta mfOFFSET
  jsr releaseSPwithOFFSET
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  pla
  rts
  
releaseSPandSPNEXT:
  pha
  phx
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  stz mfOFFSET ; SP
  jsr releaseSPwithOFFSET
  
  lda #1 ; SPNEXT
  sta mfOFFSET
  jsr releaseSPwithOFFSET
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  plx
  pla
  rts
  
releaseSPandSPNEXTandSPNEXTNEXT:
  pha
  phx
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  stz mfOFFSET ; SP
  jsr releaseSPwithOFFSET
  
  lda #1 ; SPNEXT
  sta mfOFFSET
  jsr releaseSPwithOFFSET
  
  lda #2 ; SPNEXTNEXT
  sta mfOFFSET
  jsr releaseSPwithOFFSET
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  plx
  pla
  rts
  
releaseSPandSPNEXTNEXT:
  pha
  phx
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  stz mfOFFSET ; SP
  jsr releaseSPwithOFFSET
  
  lda #2 ; SPNEXTNEXT
  sta mfOFFSET
  jsr releaseSPwithOFFSET
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  plx
  pla
  rts
