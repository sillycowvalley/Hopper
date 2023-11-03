; ######################## List utilities ########################

; available variables:
;
; F1-F2   fSIZE - used (call to gcCreate)
; F3      lTYPE
; F4-F5   lLENGTH
; F6-F7   lPREVIOUS
; F8-F9   lNEXT        preserved during recursive clone calls
; F10-F11 lCURRENT     preserved during recursive clone calls      F10-F11   fVALUE (only used in call to createValueVariant)
; F12-F13 fITEM
; F14-F15 lCOUNT


; listUtilityPopListToIDX:               pop list argument -> IDX
; listUtilityPopListToIDY:               pop list argument -> IDY
; listUtilityPopValueToIDXandTYPE:       pop value argument -> IDX and lTYPE
; listUtilityPopValueToTOPandTYPE:       pop value argument -> TOP and lTYPE
; listUtilityPopUIntToType :             pop uint argument -> lTYPE
; listUtilityPopUIntToTOP:               pop uint argument -> TOP
; listUtilityPopUIntToIndex:             pop uint argument -> IDY
; listUtilityLoadLengthFromIDX:          IDX -> list, load lLENGTH
; listUtilityStoreLengthToIDX:           IDX -> list, store lLENGTH
;
; listUtilityClone:                      IDY -> sourceList, returns cloned list in IDX
; listUtilityClear:                      IDX -> list to clear : disposes items in list (but not list itself)
; listUtilityZeroFields:                 IDX -> list : set length, pFirst, pRecent and iRecent to zero
; listUtilityCreateList:                 returns new list as IDX (zeroes fields too)
; listUtilityMoveToItem:                 IDX -> list, IDY is index of interest, returns lCURRENT (and updates iRecent and pRecent)
;
; listUtilityReleaseItemIDY:             IDY -> listItem : frees the listItem (but not the contents of pData even if reference type)
; listUtilityRangeCheckTOP ;             list -> IDY, index -> TOP

; listUtilityReleaseItemValue:           ...

; pop uint argument -> IDY
listUtilityPopUIntToIndex:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
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
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  .endif ; !STACK8
  rts

; pop uint argument -> TOP
listUtilityPopUIntToTOP:
  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; LSB
  sta TOPL
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
  sta TOPH
  jsr decSP          ; LSB
  lda (SP)
  sta TOPL
  .endif ; !STACK8
  rts
  rts


listUtilityPopValueToTOPandTYPE:
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta TOPH                   ; MSB
  dex
  lda HopperValueStack, X
  sta TOPL                   ; LSB
  lda HopperTypeStack, X
  sta lTYPE                  ; item type
  stx SP8
  .else
  jsr decSP          ; MSB
  lda (SP)
  sta TOPH
  jsr decSP          ; LSB
  lda (SP)
  sta TOPL
  jsr decTSP
  lda (TSP)
  sta lTYPE           ; item type
  .endif
  rts

listUtilityPopValueToIDXandTYPE:
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH                   ; MSB
  dex
  lda HopperValueStack, X
  sta IDXL                   ; LSB
  lda HopperTypeStack, X
  sta lTYPE                  ; item type
  stx SP8
  .else
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  lda (TSP)
  sta lTYPE           ; item type
  .endif
  rts


listUtilityPopListToIDX:
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
  jsr assertList
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
  jsr assertList
  .endif 
  .endif ; !STACK8
  .ifdef CHECKED
  lda (IDX)
  jsr assertList
  .endif
  rts


listUtilityPopListToIDY:
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
  jsr assertList
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
  jsr assertList
  .endif 
  .endif ; !STACK8
  .ifdef CHECKED
  lda (IDY)
  jsr assertList
  .endif
  rts

; pop uint argument -> lTYPE
listUtilityPopUIntToType:
  .ifdef STACK8
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  sta lTYPE
  .else
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta lTYPE
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif
  rts

; IDY -> sourceList, returns cloned list in IDX (lCURRENT, lNEXT and IDY preserved in cloneIDY recursive calls)
listUtilityClone:
  ; called from cloneIDY (gc.asm)
  lda #9
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE = length in characters + 2 bytes for string length field
  ; return address in IDX
  lda #tList
  jsr gcCreate
  
  ; preserve for return value
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; number of items
  ldy #listLengthOffset
  lda (IDY), Y
  sta (IDX), Y
  iny
  lda (IDY), Y
  sta (IDX), Y
  
  ; item type
  ldy #listItemTypeOffset
  lda (IDY), Y
  sta (IDX), Y
  sta lTYPE
  
  ; CURRENT : location to put the pointer to new list item (tList.pFirst)
  clc
  lda IDXL
  adc #listpFirstOffset
  sta lCURRENTL
  lda IDXH
  adc #0
  sta lCURRENTH
  
  ; NEXT : list item to clone (tList.pFirst)
  ldy #listpFirstOffset
  lda (IDY), Y
  sta lNEXTL
  iny
  lda (IDY), Y
  sta lNEXTH
  
cloneListItemNext:
  lda lNEXTL
  bne cloneListItem
  lda lNEXTH
  bne cloneListItem
  bra clonedListExit
cloneListItem:

  lda #4
  sta ACCL
  stz ACCH
  ; size is in ACC
  ; return address in IDX
  jsr memoryAllocate
  
  ldy #1
  lda IDXL
  sta (lCURRENT)
  lda IDXH
  sta (lCURRENT), Y
  
  ; CURRENT : location to put the pointer to new list item (tListItem.pNext)
  clc
  lda IDXL
  adc #listItempNextOffset
  sta lCURRENTL
  lda IDXH
  adc #0
  sta lCURRENTH
  
  ; active listItem
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; pData from existing listItem
  ldy #listItempDataOffset
  lda (lNEXT), Y
  sta IDYL
  iny
  lda (lNEXT), Y
  sta IDYH
  
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs cloneListItemHeapType
  ; item is value type
  
  ldy #listItempDataOffset
  lda IDYL
  sta (IDX), Y
  iny
  lda IDYH
  sta (IDX), Y
  
  ; active listItem
  pla
  sta IDYH
  pla
  sta IDYL
  
  bra cloneListItemAfterData
  
cloneListItemHeapType:
  lda (IDY)
  ; type is in A
  ; reference type to clone is at IDY
  ; (preserves lCURRENT, lNEXT and IDY for recursive calls)
  jsr cloneIDY 
  
  ; active listItem
  pla
  sta IDYH
  pla
  sta IDYL

  ; pData
  ldy #listItempDataOffset
  lda IDXL
  sta (IDY), Y
  iny
  lda IDXH
  sta (IDY), Y

cloneListItemAfterData:
  
  ; NEXT : list item to clone
  ldy #listItempNextOffset
  lda (lNEXT), Y
  tax
  iny
  lda (lNEXT), Y
  sta lNEXTH
  stx lNEXTL
  
  jmp cloneListItemNext

clonedListExit:

  pla
  sta IDXH
  pla
  sta IDXL
  rts




; returns new list as IDX (zeroes fields too)
listUtilityCreateList:
  lda #9
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tList
  jsr gcCreate
  ; fall through to zero fields below

; IDX -> list : set length, pFirst, pRecent and iRecent to zero
listUtilityZeroFields:
  lda #0
  ; length
  ldy #listLengthOffset
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  
  iny ; skip type
  
  ; pFirst
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  ; pRecent
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  ; iRecent
  sta (IDX), Y
  iny
  sta (IDX), Y
  rts

; IDX -> length, load lLENGTH
listUtilityLoadLengthFromIDX:
  ; length:
  ldy #listLengthOffset
  lda (IDX), Y
  sta lLENGTHL
  iny
  lda (IDX), Y
  sta lLENGTHH
  rts


; IDX -> list, store lLENGTH
listUtilityStoreLengthToIDX: 
  ; length:
  ldy #listLengthOffset
  lda lLENGTHL
  sta (IDX), Y
  iny
  lda lLENGTHH
  sta (IDX), Y
  rts


; IDX has list, IDY is index of interest, returns lCURRENT (and updates iRecent and pRecent)
listUtilityMoveToItem: ; used by Insert, Remove, GetItem, SetItem
  ; IDX is reference of list
  ; IDY is index of interest
  ; lCURRENT is reference item on return
  ; iRecent and pRecent are updated
  
  ; pFirst
  ldy #listpFirstOffset
  lda (IDX), Y
  sta lCURRENTL
  iny
  lda (IDX), Y
  sta lCURRENTH
  
  .ifdef CHECKED
  lda lCURRENTL
  bne listMoveToItemNotEmpty
  lda lCURRENTH
  bne listMoveToItemNotEmpty
  
  ; pFirst == 0 : empty list
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  
listMoveToItemNotEmpty:
  .endif
  
  ; lCOUNT = iRecent
  ldy #listiRecentOffset
  lda (IDX), Y
  sta lCOUNTL
  iny
  lda (IDX), Y
  sta lCOUNTH
  
  ; iRecent == 0?
  lda lCOUNTL
  bne listMoveToItemRecentNotZero
  lda lCOUNTH
  bne listMoveToItemRecentNotZero
  bra listMoveToItemNotRecent
listMoveToItemRecentNotZero:  
  
  ; iRecent <= index?
  ;   lCOUNT <= IDY?
  lda lCOUNTH
  cmp IDYH
  bne listMoveToItemRecentLEDone
  lda lCOUNTL
  cmp IDYL
listMoveToItemRecentLEDone:
  ; http://6502.org/tutorials/compare_instructions.html
  beq listMoveToItemRecentLE  ; lCOUNT == IDY (not >)
  bcc listMoveToItemRecentLE  ; lCOUNT <  IDY (not >)
  bra listMoveToItemNotRecent ; lCOUNT > IDY
listMoveToItemRecentLE:
  
  ; iRecent <= index
  
  ldy #listpRecentOffset
  lda (IDX), Y
  sta lCURRENTL
  iny
  lda (IDX), Y
  sta lCURRENTH
  
  .ifdef CHECKED
  lda lCURRENTL
  bne listMoveToItemRecentGood
  lda lCURRENTH
  bne listMoveToItemRecentGood
  
  ; pFirst == 0 : empty list
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  .else
  bra listMoveToItemRecentGood
  .endif
  
listMoveToItemNotRecent:
  
  stz lCOUNTL
  stz lCOUNTH
  
listMoveToItemRecentGood:

listMoveToItemLoop:  
  lda lCOUNTL
  cmp IDYL
  bne listMoveToItemNext
  lda lCOUNTH
  cmp IDYH
  bne listMoveToItemNext
  
  ; lCOUNT == IDY
  bra listMoveToItemFound
  
listMoveToItemNext:
  
  ; CURRENT = CURRENT.pNext
  ldy #listItempNextOffset
  lda (lCURRENT), Y
  tax
  iny
  lda (lCURRENT), Y
  sta lCURRENTH
  txa
  sta lCURRENTL
  
  .ifdef CHECKED
  lda lCURRENTL
  bne listMoveToItemCurrentOk
  lda lCURRENTH
  bne listMoveToItemCurrentOk
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  
listMoveToItemCurrentOk:
  .endif

  ; lCOUNT++ 
  inc lCOUNTL
  bne listMoveToItemLoop
  inc lCOUNTH
  bra listMoveToItemLoop
  
listMoveToItemFound:

  ; ListItem memory map:
  ;   0000 heap allocator size
  ;   xxxx variant box for value types, pData for reference types
  ;   0000 pNext
  
  ; update iRecent and pRecent
  ; pRecent = lCURRENT
  ldy #listpRecentOffset
  lda lCURRENTL
  sta (IDX), Y
  iny
  lda lCURRENTH
  sta (IDX), Y
  
  ; iRecent = index
  ldy #listiRecentOffset
  lda IDYL
  sta (IDX), Y
  iny
  lda IDYH
  sta (IDX), Y
  rts


; IDX -> list to clear : disposes items in list (but not list itself)
listUtilityClear: 
  ; called from syscallListClear and gcRelease
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  lda lNEXTL
  pha
  lda lNEXTH
  pha
  
  ldy #listItemTypeOffset
  lda (IDX), Y
  sta lTYPE
  
  ; pFirst
  ldy #listpFirstOffset
  lda (IDX), Y
  sta lNEXTL
  iny
  lda (IDX), Y
  sta lNEXTH
  
listUtilityClearNext:
  lda lNEXTL
  bne listUtilityClearItem
  lda lNEXTH
  bne listUtilityClearItem
  bra listUtilityClearDone
listUtilityClearItem:

  ; IDY = lNEXT.pData
  ldy #listItempDataOffset
  lda (lNEXT), Y
  sta IDYL
  iny
  lda (lNEXT), Y
  iny
  sta IDYH
  
  ; release the data memory
  jsr listUtilityReleaseItemValue ; release pData
  
  lda lNEXTL  
  sta IDYL
  lda lNEXTH
  sta IDYH
  
  ; lNEXT = lNEXT.pNext
  ldy #listItempNextOffset
  lda (lNEXT), Y
  tax
  iny
  lda (lNEXT), Y
  iny
  sta lNEXTH
  stx lNEXTL
  
  ; release the listitem memory
  jsr listUtilityReleaseItemIDY ; release IDY (previous pNext)
  
  bra listUtilityClearNext

listUtilityClearDone:

  pla
  sta lNEXTH
  pla
  sta lNEXTL
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  rts





; IDY -> listItem : frees the listItem (but not the contents of pData even if reference type)
listUtilityReleaseItemIDY: ; used by Remove and Clear
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  jsr memoryFree
  
  pla
  sta IDXH
  pla
  sta IDXL
  rts


; list -> IDY, index -> TOP
listUtilityRangeCheckTOP:
  ldy #2
  lda (IDY), Y
  sta lLENGTHL
  iny
  lda (IDY), Y
  sta lLENGTHH
  
  ; TOP <= lLENGTH?
  lda TOPH
  cmp lLENGTHH
  bne donelistUtilityRangeCheckTOPLE
  lda TOPL
  cmp lLENGTHL
donelistUtilityRangeCheckTOPLE:
  ; http://6502.org/tutorials/compare_instructions.html
  beq listUtilityRangeCheckTOPLE ; TOP == lLENGTH (not >)
  bcc listUtilityRangeCheckTOPLE ; TOP <  lLENGTH (not >)
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
listUtilityRangeCheckTOPLE:
  ; TOP <= lLENGTH
  rts


listUtilityReleaseItemValue:
  ; preserves lCURRENT, lTYPE
  ; type in lTYPE, reference in IDY (works for tListItem.pData)
  
  lda lTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc listUtilityReleaseItemValueType
  
  lda lCURRENTL
  pha
  lda lCURRENTH
  pha
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  
  jsr gcRelease ; address in IDX
  
  pla
  sta lCURRENTH
  pla
  sta lCURRENTL
listUtilityReleaseItemValueType:
  rts

; type in A, value in fVALUEL, resulting item in tListItem in fITEM
listUtilityCreateValueItem:

  lda fVALUEL
  sta IDXL
  lda fVALUEH
  sta IDXH
  
  ; pData value is in IDX
  ; uses fSIZE
  ; sets pNext = 0
  jsr listUtilityItemCreate
  ; returns tListItem in fITEM
  rts


listUtilityItemCreate:
  ; pData value is in IDX
  ; uses fSIZE
  ; sets pNext = 0
  ; returns tListItem in fITEM
  
  lda IDYH
  pha
  lda IDYL
  pha
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  lda #4
  sta ACCL
  stz ACCH
  ; size is in ACC
  ; return address in IDX
  jsr memoryAllocate
  
  lda IDXH
  sta fITEMH
  lda IDXL
  sta fITEML
  
  ldy #listItempDataOffset
  pla
  sta (fITEM), Y ; pData LSB
  iny
  pla 
  sta (fITEM), Y ; pData MSB
  
  ldy #listItempNextOffset
  lda #0
  sta (fITEM), Y ; pNext LSB
  iny
  sta (fITEM), Y ; pNext MSB
  
  pla
  sta IDYL
  pla
  sta IDYH
  rts
