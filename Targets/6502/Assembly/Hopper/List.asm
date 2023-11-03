; ######################## List syscalls ########################

  .include ListUtilities.asm

; List memory map:
;   0000 heap allocator size
;   19   type = tList
;   00   GC reference count
;   0000 current number of items
;   xx   type of items
;   xxxx pFirst
;   xxxx pRecent
;   xxxx iRecent

listLengthOffset   = 2
listItemTypeOffset = 4
listpFirstOffset   = 5
listpRecentOffset  = 7
listiRecentOffset  = 9

; ListItem memory map:
;   0000 heap allocator size
;   xxxx inline for value types, pData for reference types and when item type is variant
;   0000 pNext

listItempDataOffset = 0
listItempNextOffset = 2

; syscallListNew:            list New(type itemType)
; syscallListClear:          Clear(<V> this) system;
; syscallListLengthGet:      uint Length { get system; }  
; syscallListGetItem:        V GetItem(<V> this, uint index) system;
;
;
; syscallListAppend:
; syscallListInsert:
; syscallListSetItem:
; syscallListRemove:
 




;Append(<V> this, V value) system;
syscallListAppend:
  jsr listUtilityPopValueToIDXandTYPE   ; value -> IDX, lTYPE
  jsr listUtilityPopListToIDY           ; this -> IDY
  
  
  ; list item type
  ldy #listItemTypeOffset
  lda (IDY), Y
  
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs syscallListAppendReferenceType
  ; item is value type
  
  ; save the value
  lda IDXH
  sta fVALUEH
  lda IDXL
  sta fVALUEL
  
  ; list item type
  ldy #listItemTypeOffset
  lda (IDY), Y
  jsr listUtilityCreateValueItem
  
  jmp syscallListAppendAppendItem
  
syscallListAppendReferenceType:
  ; item is reference type
    
  .ifdef CHECKED
  ; TODO : if listItemType == tVariant, then itemType must not be value type
  ;        if listItemType != tVariant, then listItemType must be same as itemType
  .endif
  
  ;  IDX -> IDX   variantItem = Variant_Clone(variantOriginal)) and release original
  
  .ifdef STACK8
  inc SP8
  inc SP8
  .else
  jsr incSP
  jsr incSP
  jsr incTSP
  .endif
  jsr cloneSP ; clones the item at this stack slot and releases the original at that slot
  
  .ifdef STACK8
  ldx SP8
  lda HopperValueStack, X
  sta IDXL
  inx
  lda HopperValueStack, X
  sta IDXH
  dec SP8
  dec SP8
  .else
  ldy #0
  lda (SP), Y
  sta IDXL
  iny
  lda (SP), Y
  sta IDXH
  jsr decSP
  jsr decSP
  jsr decTSP
  .endif

  ; list is in IDY, item to append is in IDX
  
  ; pData value is in IDX
  ; returns new tListItem in fITEM
  jsr listUtilityItemCreate
  
syscallListAppendAppendItem:
  
  stz lNEXTL
  stz lNEXTL
  stz lCURRENTL
  stz lCURRENTH
  stz lPREVIOUSL
  stz lPREVIOUSH
  
  ; append it to the end of the list
  ;  List_Append(list, variantItem);

  
  ; pRecent
  ldy #listpRecentOffset
  lda (IDY), Y
  sta lNEXTL
  iny
  lda (IDY), Y
  sta lNEXTH
  lda lNEXTL
  bne listAppendRecentIsValid
  lda lNEXTH
  bne listAppendRecentIsValid
  
  ; pFirst
  ldy #listpFirstOffset
  lda (IDY), Y
  sta lNEXTL
  iny
  lda (IDY), Y
  sta lNEXTH
  
listAppendRecentIsValid:
  
  lda lNEXTL
  bne listAppendNextItem
  lda lNEXTH
  bne listAppendNextItem
  ; NEXT == 0, special case for first item
  
  ; pFirst = IDX
  ldy #listpFirstOffset
  lda fITEML
  sta (IDY), Y
  iny
  lda fITEMH
  sta (IDY), Y
  
  jmp listAppendExit
  
listAppendNextItem:
  
  lda lCURRENTL
  sta lPREVIOUSL
  lda lCURRENTH
  sta lPREVIOUSH
  lda lNEXTL
  sta lCURRENTL
  lda lNEXTH
  sta lCURRENTH
  
  ldy #listItempNextOffset
  lda (lCURRENT), Y
  sta lNEXTL
  iny
  lda (lCURRENT), Y
  sta lNEXTH
  
  ; NEXT == 0?
  lda lNEXTL
  bne listAppendNextItem
  lda lNEXTH
  bne listAppendNextItem
  
  ; NEXT == 0 
  ; CURRENT.pData = fITEM
  ldy #listItempNextOffset
  lda fITEML
  sta (lCURRENT), Y
  iny
  lda fITEMH
  sta (lCURRENT), Y
  
  ; PREVIOUS.pNext = CURRENT
  ldy #listItempNextOffset
  lda lCURRENTL
  sta (lPREVIOUS), Y
  iny
  lda lCURRENTH
  sta (lPREVIOUS), Y
  
listAppendExit:

  ; pRecent = IDX  
  ldy #listpRecentOffset
  lda fITEML
  sta (IDY), Y
  iny
  lda fITEMH
  sta (IDY), Y
  
  ; previous length
  ldy #listLengthOffset
  lda (IDY), Y
  sta lLENGTHL
  iny
  lda (IDY), Y
  sta lLENGTHH
  
  ; iRecent = previous length
  ldy #listiRecentOffset
  lda lLENGTHL
  sta (IDY), Y
  iny
  lda lLENGTHH
  sta (IDY), Y
  
  ; length: increment the item count in the list
  ldy #listLengthOffset
  lda lLENGTHL
  inc
  sta (IDY), Y
  bne incItemCountEnd
  iny
  lda lLENGTHH
  inc
  sta (IDY), Y
incItemCountEnd:
  
  jsr releaseSP ; we popped 'this', decrease reference count
  jmp nextInstruction



; Insert(<V> this, uint index, V value) system;
syscallListInsert:
  jsr listUtilityPopValueToIDXandTYPE
  jsr listUtilityPopUIntToTOP
  jsr listUtilityPopListToIDY
  
  ldy #listItemTypeOffset
  lda (IDY), Y
  sta lTYPE ; type of list items
  
  .ifdef CHECKED
  jsr listUtilityRangeCheckTOP ; list -> IDY, index -> TOP
  .endif
  
  lda TOPL
  bne syscallListInsertNotZero
  lda TOPH
  bne syscallListInsertNotZero
  
  ; IDY is zero
  
  ; 'fake' lCURRENT so that #4 offset works below
  clc
  lda IDYL
  adc #listpFirstOffset-listItempNextOffset
  sta lCURRENTL
  lda IDYH
  adc #0
  sta lCURRENTH
  
  bra syscallListInsertStart
  
syscallListInsertNotZero:  
  lda IDXH
  pha
  lda IDXL
  pha
  lda IDYH
  sta IDXH
  pha
  lda IDYL
  sta IDXL
  pha
  lda TOPH
  sta IDYH
  lda TOPL
  sta IDYL
  jsr decIDY
  
  ; IDX is reference of list
  ; IDY is index of item before
  jsr listUtilityMoveToItem ; -> lCURRENT
  
  pla
  sta IDYL
  pla
  sta IDYH
  pla
  sta IDXL
  pla
  sta IDXH

syscallListInsertStart:  

  ; save 'lCURRENT'
  lda lCURRENTH
  pha
  lda lCURRENTL
  pha
  
  lda lTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs syscallListInsertReferenceType
  ; item is value type
  
  ; save the value
  lda IDXH
  sta fVALUEH
  lda IDXL
  sta fVALUEL
  
  ; get the list item type
  lda lTYPE
  ; type in A, value in fVALUEL, resulting item in tListItem in fITEM
  jsr listUtilityCreateValueItem
  jmp syscallListInsertInsertItem
  
syscallListInsertReferenceType:
  ; item is reference type
  
  
  ;  IDX -> IDX clone, and release original
  
  .ifdef STACK8
  inc SP8
  inc SP8
  inc SP8
  inc SP8
  .else
  jsr incSP
  jsr incSP
  jsr incSP
  jsr incSP
  jsr incTSP
  jsr incTSP
  .endif
  
  jsr cloneSP ; clones the item at this stack slot and releases the original at that slot
  
  .ifdef STACK8
  ldx SP8
  lda HopperValueStack, X
  sta IDXL
  inx
  lda HopperValueStack, X
  sta IDXH
  dec SP8
  dec SP8
  dec SP8
  dec SP8
  .else
  ldy #0
  lda (SP), Y
  sta IDXL
  iny
  lda (SP), Y
  sta IDXH
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decTSP
  jsr decTSP
  .endif
  
  ; list is in IDY, item to append is in IDX
  
  ; pData value is in IDX
  ; returns new tListItem in fITEM
  jsr listUtilityItemCreate
syscallListInsertInsertItem:
  
  pla
  sta lCURRENTL
  pla
  sta lCURRENTH
  
  ldy #listItempNextOffset
  lda (lCURRENT), Y
  sta lNEXTL
  iny
  lda (lCURRENT), Y
  sta lNEXTH
  
  ldy #listItempNextOffset
  lda lNEXTL
  sta (fITEM), Y ; pNext LSB
  iny
  lda lNEXTH
  sta (fITEM), Y ; pNext MSB
  
  
  ldy #listItempNextOffset
  lda fITEML
  sta (lCURRENT), Y
  iny
  lda fITEMH
  sta (lCURRENT), Y
  
  ; pRecent = 0  
  ldy #7
  lda #0
  sta (IDY), Y
  iny
  sta (IDY), Y
  ; iRecent
  iny
  sta (IDY), Y
  iny
  sta (IDY), Y
  
  ; previous length
  ldy #listLengthOffset
  lda (IDY), Y
  sta lLENGTHL
  iny
  lda (IDY), Y
  sta lLENGTHH
  
  ; length: increment the item count in the list
  ldy #listLengthOffset
  lda lLENGTHL
  inc
  sta (IDY), Y
  bne incInsertItemCountEnd
  iny
  lda lLENGTHH
  inc
  sta (IDY), Y
incInsertItemCountEnd:

  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  jmp nextInstruction
  
  
; Remove(<V> this, uint index) system;
syscallListRemove:
  jsr stringUtilityPopUIntToIndex ; pop uint argument -> IDY
  jsr listUtilityPopListToIDX     ; this -> IDX
  
  ldy #listItemTypeOffset
  lda (IDX), Y
  sta lTYPE
  
  lda IDXL
  pha
  lda IDXH
  pha

  .ifdef CHECKED
  ldy #listLengthOffset
  lda (IDX), Y
  sta lLENGTHL
  iny
  lda (IDX), Y
  sta lLENGTHH
  
  ; IDY < lLENGTH?
  lda IDYH
  cmp lLENGTHH
  bne donesyscallListRemoveLT
  lda IDYL
  cmp lLENGTHL
donesyscallListRemoveLT:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc syscallListRemoveLT ; IDY < lLENGTH
  
  ; IDY >= lLENGTH
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  
syscallListRemoveLT:
  .endif
  
  ; IDY < lLENGTH
  
  lda IDYL
  bne syscallListRemoveNotZero
  lda IDYH
  bne syscallListRemoveNotZero
  
  ; IDY is zero
  
  ; 'fake' lCURRENT so that listItem.pNext offset works below
  clc
  lda IDXL
  adc #listpFirstOffset-listItempNextOffset
  sta lCURRENTL
  lda IDXH
  adc #0
  sta IDXH
  sta lCURRENTH
  
  bra syscallListRemoveStart
  
syscallListRemoveNotZero:  
  jsr decIDY
  ; IDX is reference of list
  ; IDY+1 is index of interest
  jsr listUtilityMoveToItem ; -> lCURRENT

syscallListRemoveStart:  

  ; lNEXT  = lCURRENT.pNext
  ldy #listItempNextOffset
  lda (lCURRENT), Y
  sta lNEXTL
  iny
  lda (lCURRENT), Y
  sta lNEXTH
  
  ; IDY  = lNEXT.pData
  ldy #listItempDataOffset
  lda (lNEXT), Y
  sta IDYL
  iny
  lda (lNEXT), Y
  sta IDYH
  
  ; reference in IDY, type in lTYPE (works for tListItem.pData)
  ;    preserves lCURRENT, lTYPE
  jsr listUtilityReleaseItemValue ; release pData
  
  ; IDY  = lCURRENT.pNext
  ldy #listItempNextOffset
  lda (lCURRENT), Y
  sta IDYL
  iny
  lda (lCURRENT), Y
  sta IDYH
  
  ; lNEXT  = IDY.pNext
  ldy #listItempNextOffset
  lda (IDY), Y
  sta lNEXTL
  iny
  lda (IDY), Y
  sta lNEXTH
  
  ; lCURRENT.pNext = lNEXT
  ldy #listItempNextOffset
  lda lNEXTL
  sta (lCURRENT), Y
  iny
  lda lNEXTH
  sta (lCURRENT), Y
  
  jsr listUtilityReleaseItemIDY ; release IDY (previous lCURRENT)
  
  pla
  sta IDXH
  pla
  sta IDXL

  ; count  
  ldy #listLengthOffset
  sec
  lda (IDX), Y
  sbc #1
  sta (IDX), Y
  iny
  lda (IDX), Y
  sbc #0
  sta (IDX), Y
  
  ; pRecent
  ldy #listpRecentOffset
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  ; iRecent
  ldy #listiRecentOffset
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  jmp nextInstruction





;SetItem(<V> this, uint index, V value) system;
syscallListSetItem:
  jsr listUtilityPopValueToTOPandTYPE    ; value -> TOP and lTYPE
  jsr listUtilityPopUIntToIndex          ; UInt index -> IDY
  jsr listUtilityPopListToIDX            ; this -> IDX
  
  lda lTYPE
  sta NEXTL ; type of value

  ldy #listItemTypeOffset
  lda (IDX), Y
  sta NEXTH ; type of list items
  
  ; IDX is reference of list
  ; IDY is index of interest
  jsr listUtilityMoveToItem ; -> lCURRENT
  
  lda NEXTH ; type of list items
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs listSetItemReferenceType
  
  ; value types - just reset in item, no need to release
  ldy #listItempDataOffset
  lda TOPL
  sta (lCURRENT), Y
  iny
  lda TOPH
  sta (lCURRENT), Y
  jsr releaseSP ; we popped 'this', decrease reference count
  jmp nextInstruction
  
  
listSetItemReferenceType:
  
  lda lTYPE
  pha
  
  ; lCURRENT.pData -> IDY
  ldy #listItempDataOffset
  lda (lCURRENT), Y
  sta IDYL
  iny
  lda (lCURRENT), Y
  sta IDYH
  
  lda (IDY)
  lda lTYPE

  ; type in lTYPE, reference in IDY
  jsr listUtilityReleaseItemValue ; release pData
  
  ; clone value and put it into lCURRENT/NEXT
  lda TOPL
  sta IDYL
  lda TOPH
  sta IDYH
  pla ; lTYPE
  jsr cloneIDY
  
  ldy #2
  lda IDXL
  sta (NEXT), Y
  iny
  lda IDXH
  sta (NEXT), Y
  
  ; IDX -> lCURRENT.pData
  ldy #listItempDataOffset
  lda IDXL
  sta (lCURRENT), Y
  iny
  lda IDXH
  lda (lCURRENT), Y
  
  ; when 'value' is reference type:
  jsr releaseSPandSPNEXTNEXT ; we popped 'this' and consumed 'value', decrease reference counts

  jmp nextInstruction
  
  
  


; list New(type itemType)
syscallListNew:
  jsr listUtilityPopUIntToType   ; pop uint argument -> lTYPE
  jsr listUtilityCreateList      ; returns new list as IDX (zeroes fields too)
  
  ldy #4
  lda lTYPE
  sta (IDX), Y                  ; set list item type

  lda #tList
  jmp pushIDXExit


;Clear(<V> this) system;
syscallListClear:
  jsr listUtilityPopListToIDX
  jsr listUtilityClear
  jsr listUtilityZeroFields

  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  jmp nextInstruction


;uint Length { get system; }  
syscallListLengthGet:
  jsr listUtilityPopListToIDX
  jsr listUtilityLoadLengthFromIDX
  
  jsr releaseSP

  lda lLENGTHL
  sta TOPL  
  lda lLENGTHH
  sta TOPH
  
  lda #tUInt
  jmp pushTOPExit



;V GetItem(<V> this, uint index) system;
syscallListGetItem:
  jsr stringUtilityPopUIntToIndex ; pop uint argument -> IDY
  jsr listUtilityPopListToIDX     ; this -> IDX
  
  ldy #listItemTypeOffset
  lda (IDX), Y
  sta lTYPE
  
  ; IDX is reference of list
  ; IDY is index of interest
  jsr listUtilityMoveToItem ; -> lCURRENT
  
  ldy #listItempDataOffset
  lda (lCURRENT), Y
  sta IDYL
  iny
  lda (lCURRENT), Y
  sta IDYH
  
  lda lTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallListGetItemValueType
  
  ; reference type from pData
  lda (IDY)
  cmp #tVariant
  bne syscallListGetItemReferenceType
  
  ldy #variantpDataOffset ; variant which implies value type in variant
  lda (IDY), Y
  sta TOPL
  iny
  lda (IDY), Y
  sta TOPH
  jsr releaseSP ; we popped 'this', decrease reference count
  lda (TOP) ; type
  jmp pushTOPExit
  
syscallListGetItemReferenceType:
  ; type in A, reference in IDY
  jsr cloneIDY
  lda IDXL
  sta TOPL
  lda IDXH
  sta TOPH
  
  jsr releaseSP ; we popped 'this', decrease reference count
  lda (TOP) ; type
  jmp pushTOPExit

syscallListGetItemValueType:
  lda IDYL
  sta TOPL
  lda IDYH
  sta TOPH
  jsr releaseSP ; we popped 'this'
  lda lTYPE
  jmp pushTOPExit
  

