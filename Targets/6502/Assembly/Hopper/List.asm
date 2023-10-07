; ######################## List syscalls ########################

; List memory map:
;   0000 heap allocator size
;   19   type = tList
;   00   GC reference count
;   0000 current number of items
;   xx   type of items
;   xxxx pFirst
;   xxxx pRecent
;   xxxx iRecent

; ListItem memory map:
;   0000 heap allocator size
;   11   type = tListItem
;   01   reference count (always 1)
;   xxxx variant box for value types, pData for reference types
;   0000 pNext


listitemCreate:
  ; pData value is in IDX
  ; uses fSIZE
  ; sets pNext = 0
  ; returns tListItem in fITEM
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tListItem
  jsr gcCreate
  
  lda IDXH
  sta fITEMH
  lda IDXL
  sta fITEML
  
  ldy #2
  pla
  sta (fITEM), Y ; pData LSB
  iny
  pla 
  sta (fITEM), Y ; pData MSB
  iny
  lda #0
  sta (fITEM), Y ; pNext LSB
  iny
  sta (fITEM), Y ; pNext MSB
  
  rts

; list New(type itemType)
syscallListNew:

  ; item type -> stack
  .ifdef STACK8
  
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  sta fTYPE
  pha
  
  .else
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta fTYPE
  pha           
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif
  
  lda #9
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tList
  jsr gcCreate
  
  lda #0
  ldy #2
  ; current number of items
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  pla ; list item type
  sta (IDX), Y
  iny
  ; pFirst
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  ; pRecent
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  ; iRecent
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny

  lda #tList
  jmp pushIDXExit
  
;Append(<V> this, V value) system;
syscallListAppend:

  ; value -> IDX
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH                   ; MSB
  dex
  lda HopperValueStack, X
  sta IDXL                   ; LSB
  lda HopperTypeStack, X
  sta fTYPE                  ; item type
  
  .else
  
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  lda (TSP)
  sta fTYPE           ; item type
  
  .endif
  
  ; this -> IDY
  .ifdef STACK8
  
  dex
  lda HopperValueStack, X
  sta IDYH                   ; MSB
  dex
  lda HopperValueStack, X
  sta IDYL                   ; LSB
  stx SP8
  
  .else
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
  .endif
  
  lda fTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs syscallListAppendReferenceType
  ; item is value type
  
  ; save the value
  lda IDXH
  sta fVALUEH
  lda IDXL
  sta fVALUEL
  
  ; list item type
  ldy #4
  lda (IDY), Y
  
  ; type in A
  ; value in fVALUE
  ; return tVariant in IDX  
  jsr createValueVariant
  
  jmp syscallListAppendAppendItem
syscallListAppendReferenceType:
  ; item is reference type
  
  .ifdef CHECKED
  ; TODO : if listItemType == tVariant, then itemType must not be value type
  ;        if listItemType != tVariant, then listItemType must be same as itemType
  .endif
  
  ;  variantItem = Variant_Clone(variantOriginal));
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
  
syscallListAppendAppendItem:
  ; list is in IDY, item to append is in IDX
  
  ; pData value is in IDX
  ; returns new tListItem in fITEM
  jsr listitemCreate
  
  stz lNEXTL
  stz lNEXTL
  stz lCURRENTL
  stz lCURRENTH
  stz lPREVIOUSL
  stz lPREVIOUSH
  
  ; append it to the end of the list
  ;  List_Append(list, variantItem);

  
  ; pRecent
  ldy #7
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
  ldy #5
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
  ldy #5
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
  
  ldy #4
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
  ldy #4
  lda fITEML
  sta (lCURRENT), Y
  iny
  lda fITEMH
  sta (lCURRENT), Y
  
  ; PREVIOUS.pNext = CURRENT
  ldy #4
  lda lCURRENTL
  sta (lPREVIOUS), Y
  iny
  lda lCURRENTH
  sta (lPREVIOUS), Y
  
listAppendExit:

  ; pRecent = IDX  
  ldy #7
  lda fITEML
  sta (IDY), Y
  iny
  lda fITEMH
  sta (IDY), Y
  
  ; previous length
  ldy #2
  lda (IDY), Y
  sta fLENGTHL
  iny
  lda (IDY), Y
  sta fLENGTHH
  
  ; iRecent = previous length
  ldy #9
  lda fLENGTHL
  sta (IDY), Y
  iny
  lda fLENGTHH
  sta (IDY), Y
  
  ; length: increment the item count in the list
  ldy #2
  lda fLENGTHL
  inc
  sta (IDY), Y
  bne incItemCountEnd
  iny
  lda fLENGTHH
  inc
  sta (IDY), Y
incItemCountEnd:
  
  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  jmp nextInstruction
  
utilityListClear:
  ; list reference is in IDX
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  lda NEXTL
  pha
  lda NEXTH
  pha
  
  ; pFirst
  ldy #5
  lda (IDX), Y
  sta NEXTL
  iny
  lda (IDX), Y
  sta NEXTH
  
  syscallListClearNext:

  lda NEXTL
  bne syscallListClearItem
  lda NEXTH
  bne syscallListClearItem
  bra syscallListClearDone

syscallListClearItem:

  ; IDY = NEXT.pData
  ldy #2
  lda (NEXT), Y
  sta IDYL
  iny
  lda (NEXT), Y
  iny
  sta IDYH
  
  ; release the data memory
  jsr releaseListItemValue
  
  lda NEXTL
  sta IDYL
  lda NEXTH
  sta IDYH
  
  ; NEXT = NEXT.pNext
  ldy #4
  lda (NEXT), Y
  tax
  iny
  lda (NEXT), Y
  iny
  sta NEXTH
  stx NEXTL
  
  ; release the listitem memory
  jsr releaseListItemValue
  
  bra syscallListClearNext

syscallListClearDone:

  pla
  sta NEXTH
  pla
  sta NEXTL
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  rts
  
;Clear(<V> this) system;
syscallListClear:
  

  ; this -> TOP
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
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
  .endif
  
  jsr utilityListClear

  lda #0
  ; length
  ldy #2
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
  iny
  
  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  jmp nextInstruction

;uint Length { get system; }  
syscallListLengthGet:

  ; this -> IDY
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  stx SP8
  
  .else
  
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
  .endif
  
  ; length: increment the item count in the list
  ldy #2
  lda (IDY), Y
  sta TOPL
  iny
  lda (IDY), Y
  sta TOPH

  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  lda #tUInt
  jmp pushTOPExit


listMoveToItem:
  ; IDX is reference of list
  ; IDY is index of interest
  ; lCURRENT is reference item on return
  ; iRecent and pRecent are updated
  
  ; pFirst
  ldy #5
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
  ldy #9
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
  
  ldy #7
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
  ldy #4
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
  ;   11   type = tListItem
  ;   01   reference count (always 1)
  ;   xxxx variant box for value types, pData for reference types
  ;   0000 pNext
  
  ; update iRecent and pRecent
  ; pRecent = lCURRENT
  ldy #7
  lda lCURRENTL
  sta (IDX), Y
  iny
  lda lCURRENTH
  sta (IDX), Y
  iny
  ; iRecent = index
  lda IDYL
  sta (IDX), Y
  iny
  lda IDYH
  sta (IDX), Y
  rts
  
; Insert(<V> this, uint index, V value) system;
syscallListInsert:

  .ifdef STACK8
  
  ; value -> IDX
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  lda HopperTypeStack, X
  sta fTYPE ; type of value
  
  ; index -> TOP
  
  dex
  lda HopperValueStack, X
  sta TOPH
  dex
  lda HopperValueStack, X
  sta TOPL
  
  ; this -> IDY
  
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  stx SP8
  
  .else  
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x6
  sec
  lda SPL
  sbc #6
  sta SPL
  bcs syscallListInsertSkipMSB
  dec SPH
syscallListInsertSkipMSB:
  
  .endif
  
  ; value -> IDX
  ldy #5
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  jsr decTSP
  lda (TSP)
  sta fTYPE ; type of value
  
  ; index -> TOP
  ldy #3
  lda (SP), Y        ; MSB
  sta TOPH
  dey
  lda (SP), Y        ; LSB
  sta TOPL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; this -> IDY
  dey
  lda (SP), Y        ; MSB
  sta IDYH
  dey
  lda (SP), Y        ; LSB
  sta IDYL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertList
  .else
  
  ; decTSP x3
  sec
  lda TSPL
  sbc #2
  sta TSPL
  bcs syscallListInsertSkipMSB2
  dec TSPH
syscallListInsertSkipMSB2:
  
  .endif
  .endif
  
  .ifdef CHECKED
  ldy #2
  lda (IDY), Y
  sta fLENGTHL
  iny
  lda (IDY), Y
  sta fLENGTHH
  
  ; TOP <= fLENGTH?
  lda TOPH
  cmp fLENGTHH
  bne doneSyscallListInsertLE
  lda TOPL
  cmp fLENGTHL
doneSyscallListInsertLE:
  ; http://6502.org/tutorials/compare_instructions.html
  beq syscallListInsertLE ; TOP == fLENGTH (not >)
  bcc syscallListInsertLE ; TOP <  fLENGTH (not >)
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  
syscallListInsertLE:
  .endif
  
  ; TOP <= fLENGTH
  
  lda TOPL
  bne syscallListInsertNotZero
  lda TOPH
  bne syscallListInsertNotZero
  
  ; IDY is zero
  
  ; 'fake' lCURRENT so that #4 offset works below
  jsr incIDY
  lda IDYL
  sta lCURRENTL
  lda IDYH
  sta lCURRENTH
  jsr decIDY
  
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
  jsr listMoveToItem ; -> lCURRENT
  
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
  
  lda fTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs syscallListInsertReferenceType
  ; item is value type
  
  ; save the value
  lda IDXH
  sta fVALUEH
  lda IDXL
  sta fVALUEL
  
  ; get the list item type
  ldy #4
  lda (IDY), Y
  
  ; type in A
  ; value in fVALUE
  ; return tVariant in IDX  
  jsr createValueVariant

  jmp syscallListInsertInsertItem


  
syscallListInsertReferenceType:
  ; item is reference type
  
  .ifdef CHECKED
  ; TODO : if listItemType == tVariant, then itemType must not be value type
  ;        if listItemType != tVariant, then listItemType must be same as itemType
  .endif
  
;  variantItem = Variant_Clone(variantOriginal));
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
  
syscallListInsertInsertItem:
  ; list is in IDY, item to append is in IDX
  
  ; pData value is in IDX
  ; returns new tListItem in fITEM
  jsr listitemCreate
    
  
  pla
  sta lCURRENTL
  pla
  sta lCURRENTH
 
  ldy #4
  lda (lCURRENT), Y
  sta lNEXTL
  iny
  lda (lCURRENT), Y
  sta lNEXTH
  
  ldy #4
  lda lNEXTL
  sta (fITEM), Y ; pNext LSB
  iny
  lda lNEXTH
  sta (fITEM), Y ; pNext MSB
  
  
  ldy #4
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
  ldy #2
  lda (IDY), Y
  sta fLENGTHL
  iny
  lda (IDY), Y
  sta fLENGTHH
  
  ; length: increment the item count in the list
  ldy #2
  lda fLENGTHL
  inc
  sta (IDY), Y
  bne incInsertItemCountEnd
  iny
  lda fLENGTHH
  inc
  sta (IDY), Y
incInsertItemCountEnd:
  
  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  jmp nextInstruction
  
  
; Remove(<V> this, uint index) system;
syscallListRemove:

  .ifdef STACK8
  
  ; index -> IDY
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else

  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x4
  sec
  lda SPL
  sbc #4
  sta SPL
  bcs syscallListRemoveSkipMSB
  dec SPH
syscallListRemoveSkipMSB:
  
  .endif
  
  ; index -> IDY
  ldy #3
  lda (SP), Y        ; MSB
  sta IDYH
  dey
  lda (SP), Y        ; LSB
  sta IDYL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertList
  .else
  
  ; decTSP x2
  sec
  lda TSPL
  sbc #2
  sta TSPL
  bcs syscallListRemoveSkipMSB2
  dec TSPH
syscallListRemoveSkipMSB2:
  
  .endif
  .endif
  
  lda IDXL
  pha
  lda IDXH
  pha

  .ifdef CHECKED
  ldy #2
  lda (IDX), Y
  sta fLENGTHL
  iny
  lda (IDX), Y
  sta fLENGTHH
  
  ; IDY < fLENGTH?
  lda IDYH
  cmp fLENGTHH
  bne donesyscallListRemoveLT
  lda IDYL
  cmp fLENGTHL
donesyscallListRemoveLT:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc syscallListRemoveLT ; IDY < fLENGTH
  
  ; IDY >= fLENGTH
  
  ; list index out of range
  lda #$01 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  
syscallListRemoveLT:
  .endif
  
  ; IDY < fLENGTH
  
  lda IDYL
  bne syscallListRemoveNotZero
  lda IDYH
  bne syscallListRemoveNotZero
  
  ; IDY is zero
  
  ; 'fake' lCURRENT so that #4 offset works below
  jsr incIDX
  lda IDXL
  sta lCURRENTL
  lda IDXH
  sta lCURRENTH
  
  bra syscallListRemoveStart
  
syscallListRemoveNotZero:  
  jsr decIDY
  ; IDX is reference of list
  ; IDY+1 is index of interest
  jsr listMoveToItem ; -> lCURRENT

syscallListRemoveStart:  

  ; lNEXT  = lCURRENT.pNext
  ldy #4
  lda (lCURRENT), Y
  sta lNEXTL
  iny
  lda (lCURRENT), Y
  sta lNEXTH
  
  ; IDY  = lNEXT.pData
  ldy #2
  lda (lNEXT), Y
  sta IDYL
  iny
  lda (lNEXT), Y
  sta IDYH
  
  ; reference in IDY (works for either tListItem or tListItem.pData)
  ;    preserves lCURRENT, fTYPE
  jsr releaseListItemValue ; release pData
  
  ; IDY  = lCURRENT.pNext
  ldy #4
  lda (lCURRENT), Y
  sta IDYL
  iny
  lda (lCURRENT), Y
  sta IDYH
  
  ; lNEXT  = IDY.pNext
  ldy #4
  lda (IDY), Y
  sta lNEXTL
  iny
  lda (IDY), Y
  sta lNEXTH
  
  
  ; lCURRENT.pNext = lNEXT
  ldy #4
  lda lNEXTL
  sta (lCURRENT), Y
  iny
  lda lNEXTH
  sta (lCURRENT), Y
  
  jsr releaseListItemValue ; release pNext
  
  pla
  sta IDXH
  pla
  sta IDXL

  ; count  
  ldy #2
  sec
  lda (IDX), Y
  sbc #1
  sta (IDX), Y
  iny
  lda (IDX), Y
  sbc #0
  sta (IDX), Y
  
  ; pRecent
  ldy #7
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  ; iRecent
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  jmp nextInstruction

;V GetItem(<V> this, uint index) system;
syscallListGetItem:

  .ifdef STACK8
  
  ; index -> IDY
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x4
  sec
  lda SPL
  sbc #4
  sta SPL
  bcs syscallListGetItemSkipMSB
  dec SPH
syscallListGetItemSkipMSB:
  
  .endif
  
  ; index -> IDY
  ldy #3
  lda (SP), Y        ; MSB
  sta IDYH
  dey
  lda (SP), Y        ; LSB
  sta IDYL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertList
  .else
  
  ; decTSP x2
  sec
  lda TSPL
  sbc #2
  sta TSPL
  bcs syscallListGetItemSkipMSB2
  dec TSPH
syscallListGetItemSkipMSB2:
  
  .endif
  .endif ; ifndef STACK8
  
  ; IDX is reference of list
  ; IDY is index of interest
  jsr listMoveToItem ; -> lCURRENT
  
  ldy #2
  lda (lCURRENT), Y
  sta IDYL
  iny
  lda (lCURRENT), Y
  sta IDYH
  
  lda (IDY)
  sta fTYPE
  
  cmp #tVariant
  bne listGetItemReferenceType

  ldy #2
  lda (IDY), Y
  pha
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc listGetItemSimpleTypeInVariant
  
  .ifndef NODIAGNOSTICS
  ; TODO : deal with variant containing reference type case
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?ListGetItem", 0
  
  jsr memoryHeapWalk
  .endif
  jsr throwToys
  
listGetItemSimpleTypeInVariant:
  
  ldy #3
  lda (IDY), Y
  sta NEXTL
  iny
  lda (IDY), Y
  sta NEXTH
  
  ; value type: data in NEXT
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  .ifdef STACK8
  
  ldx SP8
  lda NEXTL
  sta HopperValueStack, X
  pla
  sta HopperTypeStack, X
  inx
  lda NEXTH
  sta HopperValueStack, X
  
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X
  .endif
  
  inx
  stx SP8
  
  .else
  
  lda NEXTL
  sta (SP)
  jsr incSP          ; LSB
  lda NEXTH
  sta (SP)
  jsr incSP          ; MSB
  pla
  sta (TSP)
  jsr incTSP
  
  .endif
  
  jmp nextInstruction
  
  
listGetItemReferenceType:

  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs listGetItemReferenceType2
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?ListGetItemRef", 0
  .endif
  jsr throwToys ; we expect only reference types here
  
listGetItemReferenceType2
  

  lda fTYPE
  pha
  ; type in A, reference in IDY
  jsr cloneIDY
  
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex
  ;lda IDXL
  ;jsr diagnosticOutHex
  
  lda IDXL
  sta TOPL
  lda IDXH
  sta TOPH
  
  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  pla
  jmp pushTOPExit

;SetItem(<V> this, uint index, V value) system;
syscallListSetItem:

  .ifdef STACK8
  
  ; value -> TOP
  ldx SP8
  dex
  lda HopperValueStack, X
  sta TOPH
  dex
  lda HopperValueStack, X
  sta TOPL
  lda HopperTypeStack, X
  sta NEXTL ; type of value
  
  ; index -> IDY
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x6
  sec
  lda SPL
  sbc #6
  sta SPL
  bcs syscallListSetItemSkipMSB
  dec SPH
syscallListSetItemSkipMSB:
  
  .endif
  
  ; value -> TOP
  ldy #5
  lda (SP), Y        ; MSB
  sta TOPH
  dey
  lda (SP), Y        ; LSB
  sta TOPL
  
  jsr decTSP
  lda (TSP)
  sta NEXTL ; type of value
  
  ; index -> IDY
  ldy #3
  lda (SP), Y        ; MSB
  sta IDYH
  dey
  lda (SP), Y        ; LSB
  sta IDYL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertList
  .else
  
  ; decTSP x2
  sec
  lda TSPL
  sbc #2
  sta TSPL
  bcs syscallListSetItemSkipMSB2
  dec TSPH
syscallListSetItemSkipMSB2:
  
  .endif
  .endif
  
  ldy #4
  lda (IDX), Y
  sta NEXTH ; type of list items
  
  ; IDX is reference of list
  ; IDY is index of interest
  jsr listMoveToItem ; -> lCURRENT
  
  ldy #2
  lda (lCURRENT), Y
  sta IDYL
  iny
  lda (lCURRENT), Y
  sta IDYH
  
  lda (IDY)
  sta fTYPE
  
  cmp #tVariant
  bne listSetItemReferenceType
  
  lda NEXTL ; type from value argument
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc listSetItemSimpleTypeInVariant
  
  .ifndef NODIAGNOSTICS
  ; TODO : deal with reference type case to insert into variant (clone and release)
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?ListSetItem", 0
  
  jsr memoryHeapWalk
  .endif
  jsr throwToys
  
listSetItemSimpleTypeInVariant:
  
  ldy #2
  lda NEXTH ; type from tList
  sta (IDY), Y
  iny
  lda TOPL
  sta (IDY), Y
  iny
  lda TOPH
  sta (IDY), Y
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  jmp nextInstruction

listSetItemReferenceType:

  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs listSetItemReferenceType2
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?ListSetItemRef", 0
  .endif
  jsr throwToys ; we expect only reference types here
  
listSetItemReferenceType2:

  lda lCURRENTL
  sta NEXTL
  lda lCURRENTH
  sta NEXTH
  
  lda fTYPE
  pha

  ; type in fTYPE, reference in IDY
  jsr releaseListItemValue
  
  ; clone value and put it into lCURRENT/NEXT
  lda TOPL
  sta IDYL
  lda TOPH
  sta IDYH
  pla ; fTYPE
  jsr cloneIDY
  
  ldy #2
  lda IDXL
  sta (NEXT), Y
  iny
  lda IDXH
  sta (NEXT), Y
  
  ; when 'value' is reference type:
  jsr releaseSPandSPNEXTNEXT ; we popped 'this' and consumed 'value', decrease reference counts

  jmp nextInstruction
  
releaseListItemValue:
  ; preserves lCURRENT, fTYPE
  ; reference in IDY (works for either tListItem or tListItem.pData)
  
  lda lCURRENTL
  pha
  lda lCURRENTH
  pha
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  
  .ifdef CHECKED
  ; reference count for list item values should always be 1
  
  ldy #1
  lda (IDX), Y ; reference count
  cmp #1
  beq releaseListItemValueOk
  .ifndef NODIAGNOSTICS
  pha
  lda #"R"
  jsr diagnosticOutChar
  pla
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?releaseListItem", 0
  .endif
  jmp throwToys
  
releaseListItemValueOk:
  .endif
  
  .ifdef MEMDEBUG2
  
  lda #"r"
  jsr diagnosticOutChar
  lda #"i"
  jsr diagnosticOutChar
  
  .endif
  
  jsr gcRelease ; address in IDX
  
  pla
  sta lCURRENTH
  pla
  sta lCURRENTL
  
  rts



cloneList:
  ; list to clone is at IDY
  lda #9
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE = length in characters + 2 bytes for string length field
  ; return address in IDX
  lda #tList
  jsr gcCreate
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ldy #2
  ; number of items
  lda (IDY), Y
  sta (IDX), Y
  iny
  lda (IDY), Y
  sta (IDX), Y
  iny
  ; item type
  lda (IDY), Y
  sta (IDX), Y
  iny
  
  ; CURRENT : location to put the pointer to new list item
  clc
  lda IDXL
  adc #5
  sta lCURRENTL
  lda IDXH
  adc #0
  sta lCURRENTH
  
  ; NEXT : list item to clone
  ldy #5
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
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tListItem
  jsr gcCreate
  
  ldy #1
  lda IDXL
  sta (lCURRENT)
  lda IDXH
  sta (lCURRENT), Y
  
  ; CURRENT : location to put the pointer to new list item
  clc
  lda IDXL
  adc #4
  sta lCURRENTL
  lda IDXH
  adc #0
  sta lCURRENTH
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; pData from existing listItem
  ldy #2
  lda (lNEXT), Y
  sta IDYL
  iny
  lda (lNEXT), Y
  sta IDYH
  
  lda (IDY)
  ; type is in A
  ; reference type to clone is at IDY
  ; (preserves lCURRENT, lNEXT and IDY for recursive calls)
  jsr cloneIDY 
  
  pla
  sta IDYH
  pla
  sta IDYL
  
  ; pData
  ldy #2
  lda IDXL
  sta (IDY), Y
  iny
  lda IDXH
  sta (IDY), Y
  
  ; NEXT : list item to clone
  ldy #4
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


; add tests for:
;    bool Contains(<V> this, V value) system;
