; ######################## String utilities ########################

; available variables:
;
; fSIGN
; fSIZE - used (stringUtilityClone)
; fSOURCEADDRESS - used (stringUtilityClone)
; fDESTINATIONADDRESS - used (stringUtilityClone)
; fTYPE
; fLENGTH - used
; fVALUE  - used
; fITEM
; lCOUNT  - used

; Utility:
;
; stringUtilityPopCharToACCL:                pop char argument -> ACCL
; stringUtilityPopUIntToIndex:               pop uint argument -> IDY
; stringUtilityPopUIntToLength:              pop uint argument -> fLENGTH
; stringUtilityPopUIntToCount :              pop uint argument -> lCOUNT
; stringUtilityPopRefToIndex:                pop ref argument -> IDY
; stringUtilityRefIDYtoIDX:                  dereference IDY (reference to string address) to IDX address
; stringUtilityPopUIntToValue:               pop uint argument -> fVALUE
; stringUtilityPopStringToIDX:               pop string argument -> IDX
; stringUtilityPopStringToIDY:               pop string argument -> IDY
; stringUtilityLoadLengthFromIDX:            IDX -> string, load fLENGTH
; stringUtilityLoadLengthFromIDY:            IDY -> string, load fLENGTH
; stringUtilityStoreLengthToIDX:             fLENGTH -> string IDX
; stringUtilityLoadDestFromIDX:              string IDX, 4 -> fDESTINATIONADDRESS
; stringUtilityLoadDestFromIDY:              string IDY, 4 -> fDESTINATIONADDRESS
; stringUtilityLoadSourceFromIDX:            string IDX, 4 -> fSOURCEADDRESS
; stringUtilityAdvanceIDXToFirstChar:        IDX -> string, IDX += 4
; stringUtilityVerifyIndexLTLength:          make sure index < length (IDY < LENGTH)
;
; stringUtilityClone:                        IDY -> sourceString, returns cloned string in IDX (preserves fSOURCEADDRESS)
; stringUtilityGetCapacity:                  IDX -> string, returns capacity in fSIZE
; stringUtilityEqual:                        TOP -> string0, NEXT -> string1, result Z set if equal (used in hashTableFindEntry), munts ACC
; stringUtilityCreateString:                 fSIZE is number of characters needed in string, returns string as IDX
; 
; stringUtilityCopyChars:                    copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
; stringUtilityCopyCharsToIDX::              copy lCOUNT chars from fSOURCEADDRESS to string at IDX
; stringUtilityCopyCharsWithInsert           copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS, but insert ACCL at position IDY
; utilityStringEnlarge:                      string in IDX, required new length in fSIZE - new string returned in IDX (stack references updated)
; stringUtilityAppendChar:                   string in IDX, appendChar in ACCL, returned IDX may have changed (munts fDESTINATIONADDRESS, fLENGTH, fSIZE)
; stringUtilityInsertCharFront:              string in IDX, insertChar in ACCL, returned IDX may have changed (munts fDESTINATIONADDRESS, fSOURCEADDRESS, fLENGTH, fSIZE)
; stringUtilitySubstringStart:               IDX source string, start position in fVALUE (characters moved left and length field changed)
; stringUtilityTrimLeft:                     string IDX -> same string IDX with spaces trimmed from left (characters moved left and length field changed)
; stringUtilityTrimRight:                    string IDX -> same string IDX with spaces trimmed from right (length simply adjusted)
; stringUtilityToLower                       string IDX -> same string IDX with all characters converted to lower case
; stringUtilityToUpper                       string IDX -> same string IDX with all characters converted to upper case

; pop char argument -> ACCL
stringUtilityPopCharToACCL:
  .ifdef STACK8 
  ldx SP8
  dex                     ; ignore MSB
  dex
  lda HopperValueStack, X ; LSB
  sta ACCL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUIntOrPlusIntOk
  .endif
  stx SP8
  .else ; STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  jsr decSP          ; ignore MSB
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  .endif ; !STACK8
  rts


; pop uint argument -> IDY
stringUtilityPopUIntToIndex:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUIntOrPlusIntOk
  .endif
  stx SP8
  .else ; STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  jsr decSP              ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  .endif ; !STACK8
  rts

; pop uint argument -> fLENGTH
stringUtilityPopUIntToLength:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta fLENGTHH
  dex
  lda HopperValueStack, X ; LSB
  sta fLENGTHL
  .ifdef CHECKED
  ; TODO : restore this check - one of the STACK8 methods is not preserving type
  ;lda HopperTypeStack, X
  ;jsr assertUIntOrPlusIntOk
  .endif
  stx SP8
  .else ; STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  jsr decSP              ; MSB
  lda (SP)
  sta fLENGTHH
  jsr decSP          ; LSB
  lda (SP)
  sta fLENGTHL
  .endif ; !STACK8
  rts


; pop uint argument -> lCOUNT
stringUtilityPopUIntToCount:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta lCOUNTH
  dex
  lda HopperValueStack, X ; LSB
  sta lCOUNTL
  .ifdef CHECKED
  ; TODO : restore this check - one of the STACK8 methods is not preserving type
  lda HopperTypeStack, X
  jsr assertUIntOrPlusIntOk
  .endif
  stx SP8
  .else ; STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  jsr decSP              ; MSB
  lda (SP)
  sta lCOUNTH
  jsr decSP          ; LSB
  lda (SP)
  sta lCOUNTL
  .endif ; !STACK8
  rts

; pop ref argument -> IDY
stringUtilityPopRefToIndex:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertReference
  .endif
  stx SP8
  .else ; STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  jsr decSP              ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  .endif ; !STACK8
  rts


; dereference IDY (reference to string address) to IDX address
stringUtilityRefIDYtoIDX:
  ldy #1
  lda (IDY)
  sta IDXL
  lda (IDY), Y
  sta IDXH
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  rts


; pop uint argument -> fVALUE
stringUtilityPopUIntToValue:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta fVALUEH
  dex
  lda HopperValueStack, X ; LSB
  sta fVALUEL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUIntOrPlusIntOk
  .endif
  stx SP8
  .else ; STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  jsr decSP              ; MSB
  lda (SP)
  sta fVALUEH
  jsr decSP          ; LSB
  lda (SP)
  sta fVALUEL
  .endif ; !STACK8
  rts


; pop string argument -> IDX
stringUtilityPopStringToIDX:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDXH
  dex
  lda HopperValueStack, X ; LSB
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  .else ; STACK8
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif 
  .endif ; !STACK8
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  rts


; pop string argument -> IDY
stringUtilityPopStringToIDY:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  .else ; STACK8
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif 
  .endif ; !STACK8
  .ifdef CHECKED
  lda (IDY)
  jsr assertString
  .endif
  rts


; IDX -> string, load fLENGTH
stringUtilityLoadLengthFromIDX:
  ldy #2
  lda (IDX), Y       ; LSB of length
  sta fLENGTHL
  iny
  lda (IDX), Y       ; MSB of length
  sta fLENGTHH
  rts


; IDY -> string, load fLENGTH
stringUtilityLoadLengthFromIDY:
  ldy #2
  lda (IDY), Y       ; LSB of length
  sta fLENGTHL
  iny
  lda (IDY), Y       ; MSB of length
  sta fLENGTHH
  rts


; fLENGTH -> string IDX
stringUtilityStoreLengthToIDX:
  ldy #2
  lda fLENGTHL
  sta (IDX), Y
  iny
  lda fLENGTHH
  sta (IDX), Y
  rts


; string IDX  -> fSOURCEADDRESS
stringUtilityLoadSourceFromIDX:
  clc
  lda IDXL
  adc #4
  sta fSOURCEADDRESSL
  lda IDXH
  adc #0
  sta fSOURCEADDRESSH
  rts


; string IDX  -> fDESTINATIONADDRESS
stringUtilityLoadDestFromIDX:
  clc
  lda IDXL
  adc #4
  sta fDESTINATIONADDRESSL
  lda IDXH
  adc #0
  sta fDESTINATIONADDRESSH
  rts

; string IDY  -> fDESTINATIONADDRESS
stringUtilityLoadDestFromIDY:
  clc
  lda IDYL
  adc #4
  sta fDESTINATIONADDRESSL
  lda IDYH
  adc #0
  sta fDESTINATIONADDRESSH
  rts


; IDX -> string, IDX += 4
stringUtilityAdvanceIDXToFirstChar:
  ; skip past type, reference count and length
  clc
  lda #4
  adc IDXL
  sta IDXL
  lda #0
  adc IDXH
  sta IDXH
  rts

  .ifdef CHECKED
; make sure index < length (IDY < LENGTH)
stringUtilityVerifyIndexLTLength:
  lda fLENGTHL
  sta TOPL
  lda fLENGTHH
  sta TOPH
  lda IDYL
  sta NEXTL
  lda IDYH
  sta NEXTH
  jsr utilityUIntLT ; TOPL = (NEXT < TOP)
  lda TOPL
  ;cmp #0
  bne stringUtilityVerifyItemLTLengthOk
  
  lda #$05 ; string index out of range
  sta ACCL
  stz ACCH
  
  jmp utilityDiagnosticsDie
stringUtilityVerifyItemLTLengthOk:
  rts
  .endif ; CHECKED

; IDY -> sourceString, returns cloned string in IDX
stringUtilityClone:
  ; used by cloneDictionary, cloneString and cloneLong
  lda fSOURCEADDRESSH
  pha
  lda fSOURCEADDRESSL
  pha
  
  ; initialized from string at IDY
  ldy #1
  lda IDYH
  sta fSOURCEADDRESSH
  lda IDYL
  sta fSOURCEADDRESSL
  
  ldy #2
  lda (fSOURCEADDRESS), Y
  sta fSIZEL
  iny
  lda (fSOURCEADDRESS), Y
  sta fSIZEH
  
  ; store the string length  
  lda fSIZEL  ; LSB
  sta fLENGTHL
  lda fSIZEH  ; MSB
  sta fLENGTHH
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  ; type in A
  ; size is in fSIZE = length in characters + 2 bytes for string length field
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ;WriteWord(stringAddress+2, length);
  ldy #2
  lda fLENGTHL
  sta (IDX), Y
  iny
  lda fLENGTHH
  sta (IDX), Y
  iny
  
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
  
  clc
  lda IDXL  ; LSB
  adc #4
  sta fDESTINATIONADDRESSL
  lda IDXH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  ; SOURCEADDRESS -> DESTINATIONADDRESS
copyCharactersClone:
  lda #0
  cmp fLENGTHL
  bne nextCharacterToCopyClone
  cmp fLENGTHH
  beq stringInitializedClone
nextCharacterToCopyClone:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS;
  jsr decLENGTH
  
  bra copyCharactersClone
stringInitializedClone:

  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  rts

; IDX -> string, returns capacity in fSIZE
stringUtilityGetCapacity:
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  sec
  lda IDXL
  sbc #2
  sta IDYL
  lda IDXH
  sbc #0
  sta IDYH

  ldy #1
  lda (IDY)
  sta fSIZEL
  lda (IDY), Y
  sta fSIZEH
  
  ; subtract 6
  ;   0000 heap allocator size
  ;   0F   type = tString
  ;   00   GC reference count
  ;   0000 string length n
  sec
  lda fSIZEL
  sbc #6
  sta fSIZEL
  lda fSIZEH
  sbc #0
  sta fSIZEH
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  rts

; compare strings TOP and NEXT, result Z set if equal (used in hashTableFindEntry), munts ACC
stringUtilityEqual:
  
  ; skip past types and reference counts
  jsr incTOP   ; TODO PERF
  jsr incTOP
  jsr incNEXT
  jsr incNEXT
  
  ; length of TOP == length of NEXT?
  lda (TOP)
  cmp (NEXT)
  bne stringsEqualExit ; not equal
  sta ACCL
  jsr incTOP   ; TODO PERF
  jsr incNEXT
  lda (TOP)
  cmp (NEXT)
  bne stringsEqualExit ; not equal
  sta ACCH
  
  jsr incTOP   ; TODO PERF
  jsr incNEXT
  
stringsEqualLoop
  lda ACCL
  bne stringsEqualNext
  lda ACCH
  bne stringsEqualNext
  
  ; must have been equal, Z set
  bra stringsEqualExit
stringsEqualNext:
  lda (TOP)
  cmp (NEXT)
  bne stringsEqualExit ; not equal
  
  jsr incTOP
  jsr incNEXT
  jsr decACC
  bra stringsEqualLoop
  
stringsEqualExit:
  rts


; string in IDX, required new length in fSIZE - new string returned in IDX (stack references updated)
;    munts fSIZE, fDESTINATIONADDRESS, fTYPE
utilityStringEnlarge:
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  lda lCOUNTL
  pha
  lda lCOUNTH
  pha
  lda fSOURCEADDRESSL
  pha
  lda fSOURCEADDRESSH
  pha
  
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ; clone size: string length + 2 bytes for length field + another 2 bytes for type and reference count
  jsr incSIZE
  jsr incSIZE
  
  lda IDYL
  sta fSOURCEADDRESSL
  lda IDYH
  sta fSOURCEADDRESSH
  
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH
  
  lda fSIZEL
  sta lCOUNTL
  lda fSIZEH
  sta lCOUNTH
  
  ; NOTES: in places we're using CopyChars to clone the string header too (type, reference count and length : 4 bytes more than length)
  ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  jsr stringUtilityCopyChars
  
  ; IDY -> IDX
  lda #tString
  sta fTYPE
  jsr replaceStackReferences
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  jsr memoryFree ; free the original from under the nose of the GC
  
  pla
  sta IDXL
  pla
  sta IDXH
  
  pla
  sta fSOURCEADDRESSH
  pla
  sta fSOURCEADDRESSL
  pla
  sta lCOUNTH
  pla
  sta lCOUNTL
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  rts

; fSIZE is number of characters needed in string, returns string as IDX
stringUtilityCreateString:
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  pha
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  pha
  adc #0
  sta fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ; set the length field to number of characters by default
  ldy #3
  pla
  sta (IDX), Y
  dey
  pla
  sta (IDX), Y
  
  rts


; string in IDX, appendChar in ACCL, returned IDX may have changed
;   munts fDESTINATIONADDRESS, fLENGTH, fSIZE
stringUtilityAppendChar:
  jsr stringUtilityGetCapacity       ; IDX -> string, returns capacity in fSIZE
  jsr stringUtilityLoadLengthFromIDX ; (IDX), 2 -> fLENGTH
  
  ; fLENGTH >= fSIZE?
  lda fLENGTHH
  cmp fSIZEH
  bne donesyscallStringBuild1CheckGT
  lda fLENGTHL
  cmp fSIZEL
donesyscallStringBuild1CheckGT:
  bcs syscallStringBuild1Resize 
  jmp syscallStringBuild1LengthOk ; fLENGTH < fSIZE

syscallStringBuild1Resize:
  ; fLENGTH >= fSIZE
  lda fLENGTHL
  sta fSIZEL
  lda fLENGTHH
  sta fSIZEH
  jsr incSIZE ; 1 for the new extra character
  ; string in IDX, required new length in fSIZE - new string returned in IDX (stack references updated)
  jsr utilityStringEnlarge

syscallStringBuild1LengthOk:
  jsr stringUtilityLoadDestFromIDX
  clc
  lda fDESTINATIONADDRESSL
  adc fLENGTHL
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH
  adc fLENGTHH
  sta fDESTINATIONADDRESSH                  ; build[length] = appendChar
  
  lda ACCL
  sta (fDESTINATIONADDRESS)
  
  jsr incLENGTH                             ; length++
  jsr stringUtilityStoreLengthToIDX         ; fLENGTH -> (IDX), 2
  rts

; string in IDX, insertChar in ACCL, returned IDX may have changed
;   munts fDESTINATIONADDRESS, fSOURCEADDRESS, fLENGTH, fSIZE
stringUtilityInsertCharFront:
  jsr stringUtilityGetCapacity        ; IDX -> string, returns capacity in fSIZE
  jsr stringUtilityLoadLengthFromIDX  ; (IDX), 2 -> fLENGTH
  
  ; fLENGTH >= fSIZE?
  lda fLENGTHH
  cmp fSIZEH
  bne donesyscallStringBuildFrontGE
  lda fLENGTHL
  cmp fSIZEL
donesyscallStringBuildFrontGE:
  bcs syscallStringBuildFrontLT 
  jmp syscallStringBuildFrontLengthOk ; fLENGTH < fSIZE
  
syscallStringBuildFrontLT:
  ; fLENGTH >= fSIZE
  lda fLENGTHL
  sta fSIZEL
  lda fLENGTHH
  sta fSIZEH
  
  jsr incSIZE
  
  ; string in IDX, required new length in fSIZE - new string returned in IDX (stack references updated)
  jsr utilityStringEnlarge
  
syscallStringBuildFrontLengthOk:
  
  ; shift string forward by one in reverse (make room in front)
  jsr stringUtilityLoadSourceFromIDX
  clc
  lda fSOURCEADDRESSL
  adc fLENGTHL
  sta fSOURCEADDRESSL
  sta fDESTINATIONADDRESSL
  lda fSOURCEADDRESSH
  adc fLENGTHH
  sta fSOURCEADDRESSH
  sta fDESTINATIONADDRESSH
  
  jsr decSOURCEADDRESS        ; by one..
  
  lda fLENGTHL
  sta fSIZEL
  lda fLENGTHH
  sta fSIZEH
  
syscallStringBuildFrontCopyLoop:
  lda fSIZEL
  bne syscallStringBuildFrontCopyNext
  lda fSIZEH
  bne syscallStringBuildFrontCopyNext
  bra syscallStringBuildFrontCopyDone
syscallStringBuildFrontCopyNext:

  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  
  jsr decSOURCEADDRESS
  jsr decDESTINATIONADDRESS
  jsr decSIZE
  bra syscallStringBuildFrontCopyLoop
  
syscallStringBuildFrontCopyDone:
  ; build[0] = insertChar
  lda ACCL
  sta (fDESTINATIONADDRESS)
  
  ; length++
  jsr incLENGTH
  jsr stringUtilityStoreLengthToIDX
  rts




; copy lCOUNT chars from fSOURCEADDRESS to string at IDX
stringUtilityCopyCharsToIDX:
  clc
  lda IDXL  ; LSB
  adc #4                    ; add 4 bytes for type, reference count and length field
  sta fDESTINATIONADDRESSL
  lda IDXH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  ; fall through

; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
stringUtilityCopyChars:
  lda lCOUNTH
  pha
  lda lCOUNTL
  pha
stringUtilityCopyCharsNext:
  lda #0
  cmp lCOUNTL
  bne stringUtilityCopyNextCharacter
  cmp lCOUNTH
  bne stringUtilityCopyNextCharacter
  
  pla
  sta lCOUNTL
  pla
  sta lCOUNTH
  rts
  
stringUtilityCopyNextCharacter:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS;
  jsr decCOUNT
  bra stringUtilityCopyCharsNext

; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS, but insert ACCL at position IDY
stringUtilityCopyCharsWithInsert:
  lda lCOUNTH
  pha
  lda lCOUNTL
  pha
  lda IDYH
  pha
  lda IDYL
  pha
  
  clc
  lda fDESTINATIONADDRESSL
  adc IDYL
  sta IDYL
  lda fDESTINATIONADDRESSH
  adc IDYH
  sta IDYH
  
stringUtilityCopyCharsNext1:
  lda #0
  cmp lCOUNTL
  bne stringUtilityCopyNextCharacter1
  cmp lCOUNTH
  bne stringUtilityCopyNextCharacter1
  
  lda fDESTINATIONADDRESSL
  cmp IDYL
  bne stringUtilityCopyNextCharacterNoInsertExit
  lda fDESTINATIONADDRESSH
  cmp IDYH
  bne stringUtilityCopyNextCharacterNoInsertExit
  lda ACCL
  sta (fDESTINATIONADDRESS)
stringUtilityCopyNextCharacterNoInsertExit:
  pla
  sta IDYL
  pla
  sta IDYH
  pla
  sta lCOUNTL
  pla
  sta lCOUNTH
  rts
  
stringUtilityCopyNextCharacter1:
  lda fDESTINATIONADDRESSL
  cmp IDYL
  bne stringUtilityCopyNextCharacterNoInsert
  lda fDESTINATIONADDRESSH
  cmp IDYH
  bne stringUtilityCopyNextCharacterNoInsert
  lda ACCL
  sta (fDESTINATIONADDRESS)
  jsr incDESTINATIONADDRESS
stringUtilityCopyNextCharacterNoInsert:
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS;
  jsr decCOUNT
  bra stringUtilityCopyCharsNext1


; string IDX -> same string IDX with spaces trimmed from left (characters moved left and length field changed)
stringUtilityTrimLeft:
  jsr stringUtilityLoadLengthFromIDX  ; IDX -> string, load fLENGTH
  
  lda fLENGTHL
  bne syscallStringTrimLeftNonTrivial
  lda fLENGTHH
  beq syscallStringTrimLeftExit       ; zero length case
syscallStringTrimLeftNonTrivial
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  jsr stringUtilityLoadDestFromIDX    ; string IDX, 4 -> fDESTINATIONADDRESS

syscallStringTrimNext:
  lda (fSOURCEADDRESS)
  cmp #" "
  bne syscallStringTrimNonSpace
  ; found a space
  
  jsr incSOURCEADDRESS                ; source is now after the space
  jsr decLENGTH
  lda fLENGTHL
  bne syscallStringTrimNext
  lda fLENGTHH
  bne syscallStringTrimNext 
  ; drop through if fLENGTH hits zero (empty string)
  
syscallStringTrimNonSpace:
  
  lda fLENGTHL
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  jsr stringUtilityCopyChars              ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  
  jsr stringUtilityStoreLengthToIDX       ; store the new length
syscallStringTrimLeftExit:
  rts


; string IDX -> same string IDX with spaces trimmed from right (length simply adjusted)
stringUtilityTrimRight:
  jsr stringUtilityLoadLengthFromIDX  ; IDX -> string, load fLENGTH
  
  lda fLENGTHL
  bne syscallStringTrimRightNonTrivial
  lda fLENGTHH
  beq syscallStringTrimRightExit      ; zero length case
syscallStringTrimRightNonTrivial
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  clc
  lda fSOURCEADDRESSL
  adc fLENGTHL
  sta fSOURCEADDRESSL
  lda fSOURCEADDRESSH
  adc fLENGTHH
  sta fSOURCEADDRESSH
  
  jsr decSOURCEADDRESS
  
syscallStringTrimRightNext:
  lda (fSOURCEADDRESS)
  cmp #" "
  bne syscallStringTrimRightNonSpace
  
  jsr decSOURCEADDRESS                ; source is now before the space
  jsr decLENGTH
  lda fLENGTHL
  bne syscallStringTrimRightNext
  lda fLENGTHH
  bne syscallStringTrimRightNext 

  ; drop through if fLENGTH hits zero (empty string)
syscallStringTrimRightNonSpace:
  jsr stringUtilityStoreLengthToIDX       ; store the new length
syscallStringTrimRightExit:
  rts


; IDX source string, start position in fVALUE (characters moved left and length field changed)
stringUtilitySubstringStart:
  jsr stringUtilityLoadLengthFromIDX  ; IDX -> string, load fLENGTH
  
  lda fLENGTHL
  bne syscallStringSubstringStartNonTrivial
  lda fLENGTHH
  beq syscallStringSubstringStartExit       ; zero length case
syscallStringSubstringStartNonTrivial
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  jsr stringUtilityLoadDestFromIDX    ; string IDX, 4 -> fDESTINATIONADDRESS

  clc                                 ; fSOURCEADDRESS = fSOURCEADDRESS + fVALUE
  lda fSOURCEADDRESSL
  adc fVALUEL
  sta fSOURCEADDRESSL
  lda fSOURCEADDRESSH
  adc fVALUEH
  sta fSOURCEADDRESSH
  
  sec                                 ; fLENGTH = fLENGTH - fVALUE
  lda fLENGTHL
  sbc fVALUEL
  sta fLENGTHL
  lda fLENGTHH
  sbc fVALUEH
  sta fLENGTHH
  bpl syscallSubstringStartCopy
  
  ; -ve length means start was beyond the end of the string
  stz fLENGTHL
  stz fLENGTHH
  
syscallSubstringStartCopy:
  
  lda fLENGTHL
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  jsr stringUtilityCopyChars              ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  
  jsr stringUtilityStoreLengthToIDX       ; store the new length
syscallStringSubstringStartExit:
  rts

; string IDX -> same string IDX with all characters converted to upper case
stringUtilityToUpper:
  jsr stringUtilityLoadLengthFromIDX  ; IDX -> string, load fLENGTH
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  
stringUtilityToUpperNext:
  lda #0
  cmp fLENGTHL
  bne stringUtilityToUpperMore
  cmp fLENGTHH
  beq stringUtilityToUpperExit
  
stringUtilityToUpperMore:

  lda (fSOURCEADDRESS)
  cmp #'a'
  bcc stringUtilityToUpperNot
  cmp #'z'+1
  bcs stringUtilityToUpperNot
  ; isLower
  and #$DF ; lower to upper ASCII
stringUtilityToUpperNot
  sta (fSOURCEADDRESS)
  
  jsr decLENGTH
  jsr incSOURCEADDRESS
  bra stringUtilityToUpperNext

stringUtilityToUpperExit:
  rts


; string IDX -> same string IDX with all characters converted to lower case
stringUtilityToLower:
  jsr stringUtilityLoadLengthFromIDX  ; IDX -> string, load fLENGTH
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  
stringUtilityToLowerNext:
  lda #0
  cmp fLENGTHL
  bne stringUtilityToLowerMore
  cmp fLENGTHH
  beq stringUtilityToLowerExit
  
stringUtilityToLowerMore:

  lda (fSOURCEADDRESS)
  cmp #'A'
  bcc stringUtilityToLowerNot
  cmp #'Z'+1
  bcs stringUtilityToLowerNot
  ; isUpper
  ora #$20 ; upper to lower ASCII
stringUtilityToLowerNot
  sta (fSOURCEADDRESS)
  
  jsr decLENGTH
  jsr incSOURCEADDRESS
  bra stringUtilityToLowerNext

stringUtilityToLowerExit:
  rts