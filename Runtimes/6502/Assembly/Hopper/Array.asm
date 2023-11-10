; ######################## Array syscalls ########################

; Array memory map:
;   0000 heap allocator size
;   0F   type = tArray
;   00   GC reference count
;   0000 number of elements
;   xx   type of elements
;   0000 first element
;   ..
;   <nn> last element

  
; array New(uint size, type elementType)
syscallArrayNew:

  ; element type -> stack
  .ifdef STACK8
  ldx SP8
  dex
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  .endif
  sta fTYPE
  pha           
  
  .ifndef STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif
  
  ; number of elements -> IDX
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB
  lda (SP)
  .endif
  sta fSIZEH
  pha
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  stx SP8
  .else
  jsr decSP          ; LSB
  lda (SP)
  .endif
  sta fSIZEL
  pha
  
  .ifndef STACK8
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  .endif
  
  lda fTYPE
  cmp #tBool
  bne arrayTryByte
  ; size = number of elements / 8 + 1
  stz aCARRY
  lda fSIZEL
  and #$07
  beq noExtraBits
  lda #1
  sta aCARRY
noExtraBits
  lsr fSIZEH
  ror fSIZEL
  lsr fSIZEH
  ror fSIZEL
  lsr fSIZEH
  ror fSIZEL
  clc
  lda fSIZEL
  adc aCARRY
  sta fSIZEL
  bcc arraySizeCalculated
  inc fSIZEH
  bra arraySizeCalculated
arrayTryByte:
  cmp #tByte
  bne arrayTryChar
  ; size = number of elements == size
  bra arraySizeCalculated
arrayTryChar:
  cmp #tChar
  bne arrayOther
  ; size = number of elements
  bra arraySizeCalculated
arrayOther:
  ; size = number of elements x 2
  asl fSIZEL
  rol fSIZEH
arraySizeCalculated:

  ; add 2 bytes for number of elements field and 1 byte for type of element field
  clc
  lda fSIZEL  ; LSB
  adc #3
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta ACCH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tArray
  jsr gcCreate
  
  ; number of elements
  pla
  sta fLENGTHL
  pla
  sta fLENGTHH
  ; element type
  pla
  sta ACCL 
  
  ldy #2
  lda fLENGTHL
  sta (IDX), Y
  iny
  lda fLENGTHH
  sta (IDX), Y
  iny
  lda ACCL
  sta (IDX), Y
  
  
  lda #tArray
  jmp pushIDXExit


syscallArraySetItemUInt:  
  .ifdef CHECKED
  jmp syscallArraySetItem
  .endif
  .ifndef STACK8
  jmp syscallArraySetItem
  .else

  ; value
  ldx SP8
  dex
  lda HopperValueStack, X
  sta ACCH
  dex
  lda HopperValueStack, X
  sta ACCL
  
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
  stx SP8
  sta IDXL
  
  
  ; two byte elements : IDY << 1
  asl IDYL
  rol IDYH
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDYL
  lda IDXH  ; MSB
  adc IDYH
  sta IDYH
  
  lda ACCL
  ldy #5
  sta (IDY), Y
  lda ACCH
  iny
  sta (IDY), Y

  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
  ;bne syscallArraySetItemNoRelease
  ;jsr memoryFree
;syscallArraySetItemNoRelease:
  
  jmp nextInstruction
  .endif ; STACK8


; SetItem(V[] this, uint index, V value) system;
syscallArraySetItem:

  .ifndef STACK8
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
  bcs syscallArraySetItemSkipMSB
  dec SPH
syscallArraySetItemSkipMSB:
  
  .endif
  .endif
  
  ; value
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  .else
  ldy #5
  lda (SP), Y        ; MSB
  .endif
  sta ACCH
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  .else
  dey
  lda (SP), Y       ; LSB
  .endif
   
  sta ACCL
  
  .ifndef STACK8
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUIntOrInt
  .endif
  .endif
  
  ; index -> IDY
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  .else
  dey
  lda (SP), Y        ; MSB
  .endif
  sta IDYH
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  .else
  dey
  lda (SP), Y        ; LSB
  .endif
  sta IDYL
  
  .ifndef STACK8
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  .endif
  
  ; this -> IDX
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  .else
  dey
  lda (SP), Y        ; MSB
  .endif
  sta IDXH
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  stx SP8
  .else
  lda (SP)           ; LSB
  .endif
  sta IDXL
  
  .ifndef STACK8
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertArray
  .else
  
  ; decTSP x3
  sec
  lda TSPL
  sbc #3
  sta TSPL
  bcs syscallArraySetItemSkipMSB2
  dec TSPH
syscallArraySetItemSkipMSB2:
  
  .endif
  .endif
  
  .ifdef CHECKED
  
  ldy #2
  lda (IDX), Y
  sta TOPL
  iny
  lda (IDX), Y
  sta TOPH
  
  ; make sure index < count (IDY < TOP)
  lda IDYL
  sta NEXTL
  lda IDYH
  sta NEXTH
  jsr utilityUIntLT ; TOPL = (NEXT < TOP)
  lda TOPL
  ;cmp #0
  bne syscallArraySetItemRangeOk
  lda #$02 ; array index out of range
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
syscallArraySetItemRangeOk:
  .endif
  
  ; element type
  ldy #4
  lda (IDX), Y
  cmp #tBool
  beq syscallArraySetItemOffsetBool
  sta fTYPE
  cmp #tByte
  beq syscallArraySetItemOffsetGood
  cmp #tChar
  beq syscallArraySetItemOffsetGood
  
  ; two byte elements : IDY << 1
  asl IDYL
  rol IDYH
  bra syscallArraySetItemOffsetGood
  
syscallArraySetItemOffsetBool:

  ; capture the bit
  lda IDYL
  and #$07
  tax
  lda BitMaskTable, X
  sta aBITMASK
  ; divide offset by 8
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  
  
  jmp syscallArraySetItemBool
  
syscallArraySetItemOffsetGood:  
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDYL
  lda IDXH  ; MSB
  adc IDYH
  sta IDYH
  
  lda ACCL
  ldy #5
  sta (IDY), Y
  
  lda fTYPE
  cmp #tByte
  beq syscallArraySetItemOnlyLSB
  cmp #tChar
  beq syscallArraySetItemOnlyLSB
  
  lda ACCH
  iny
  sta (IDY), Y
  
syscallArraySetItemOnlyLSB:
  
  bra syscallArraySetItemExit
  
syscallArraySetItemBool:
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDYL
  lda IDXH  ; MSB
  adc IDYH
  sta IDYH
  
  ldy #5
  
  lda ACCL
  beq syscallArraySetItemBoolUnset
  ; set bit
  lda aBITMASK
  ora (IDY), Y
  sta (IDY), Y
  
  bra syscallArraySetItemExit
  
syscallArraySetItemBoolUnset:
  ; unset bit
  lda aBITMASK
  eor #$FF
  and (IDY), Y
  sta (IDY), Y

syscallArraySetItemExit:  

  .ifdef CHECKED
  
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  
  .else
  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
  bne syscallArraySetItemNoRelease
  jsr memoryFree
syscallArraySetItemNoRelease:
  .endif
  
  jmp nextInstruction


syscallArrayGetItemUInt:  
  .ifdef CHECKED
  jmp syscallArrayGetItem
  .endif
  .ifndef STACK8
  jmp syscallArrayGetItem
  .else
  
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
  ;stx SP8

  ; two byte elements : IDY << 1
  asl IDYL
  rol IDYH
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDYL
  lda IDXH  ; MSB
  adc IDYH
  sta IDYH

  ;ldx SP8
  ldy #5
  lda (IDY), Y       ; MSB
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  iny
  inx
  lda (IDY), Y       ; LSB
  sta HopperValueStack, X
  inx
  stx SP8
  
  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
;  bne syscallArrayGetItemNoReleaseUInt
;  jsr memoryFree
;syscallArrayGetItemNoReleaseUInt:
  
  jmp nextInstruction
  .endif
  
; V GetItem(V[] this, uint index) system;
syscallArrayGetItem:

  .ifndef STACK8
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
  bcs syscallArrayGetItemSkipMSB
  dec SPH
syscallArrayGetItemSkipMSB:
  
  .endif
  .endif
  
  ; index -> IDY
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  .else
  ldy #3
  lda (SP), Y        ; MSB
  sta IDYH
  dey
  lda (SP), Y        ; LSB
  sta IDYL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUIntOrPlusIntOk
  .endif
  .endif
  
  ; this -> IDX
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  .else
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  lda (SP)           ; LSB
  sta IDXL
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertArray
  .else
  
  ; decTSP x2
  sec
  lda TSPL
  sbc #2
  sta TSPL
  bcs syscallArrayGetItemSkipMSB2
  dec TSPH
syscallArrayGetItemSkipMSB2:
  
  .endif
  .endif
  
  
  ; skip past block header (type and reference count)
  .ifdef CHECKED
  
  ; skip past number of elements
  ldy #2
  lda (IDX), Y
  sta TOPL
  iny
  lda (IDX), Y
  sta TOPH
  
  ; make sure index < count (IDY < TOP)
  lda IDYL
  sta NEXTL
  lda IDYH
  sta NEXTH
  jsr utilityUIntLT ; TOPL = (NEXT < TOP)
  lda TOPL
  ;cmp #0
  bne syscallArrayGetItemRangeOk
  lda #$02 ; array index out of range
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
syscallArrayGetItemRangeOk:
  .endif

  ; element type
  ldy #4
  lda (IDX), Y
  cmp #tBool
  beq syscallArrayGetItemOffsetBool
  sta fTYPE
  cmp #tByte
  beq syscallArrayGetItemOffsetGood
  cmp #tChar
  beq syscallArrayGetItemOffsetGood
  
  ; two byte elements : IDY << 1
  asl IDYL
  rol IDYH
  bra syscallArrayGetItemOffsetGood
  
syscallArrayGetItemOffsetBool:
  ; capture the bit
  lda IDYL
  and #$07
  tax
  lda BitMaskTable, X
  sta aBITMASK
  ; divide offset by 8
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  jmp syscallArrayGetBool
  
syscallArrayGetItemOffsetGood:  
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDYL
  lda IDXH  ; MSB
  adc IDYH
  sta IDYH

  ldy #6
  
  lda fTYPE
  cmp #tByte
  beq syscallArrayGetItemOnlyLSB
  cmp #tChar
  beq syscallArrayGetItemOnlyLSB
  lda (IDY), Y       ; MSB
  bra syscallArrayGetItemDidMSB  
syscallArrayGetItemOnlyLSB:
  lda #0
syscallArrayGetItemDidMSB
  pha
  dey
  lda (IDY), Y       ; LSB
  pha
  
  
  .ifdef CHECKED
  
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  
  .else
  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
  bne syscallArrayGetItemNoRelease
  jsr memoryFree
syscallArrayGetItemNoRelease:
  .endif
  
  
  .ifdef STACK8
  ldx SP8
  pla
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  inx
  pla
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  .else
  pla
  sta (SP)
  jsr incSP          ; LSB
  pla
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  
  jmp nextInstruction
  
syscallArrayGetBool:
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDYL
  lda IDXH  ; MSB
  adc IDYH
  sta IDYH

  ldx #0
  ldy #5
  lda (IDY), Y          ; LSB
  and aBITMASK
  beq syscallArrayGetItemBoolUnset
  ldx #1
syscallArrayGetItemBoolUnset:
  phx
  
  .ifdef CHECKED
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  .else
  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
  bne syscallArrayGetItemNoRelease2
  jsr memoryFree
syscallArrayGetItemNoRelease2:
  .endif
  
  .ifdef STACK8
  ldx SP8
  pla
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  inx
  stz HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  .else
  pla
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  jmp nextInstruction
  
syscallArrayCountGet:

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
  
  ; this -> IDX
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertArray
  .endif
  .endif
  
  ldy #2
  lda (IDX), Y       ; LSB of count
  sta TOPL
  iny
  lda (IDX), Y       ; MSB of count
  sta TOPH
  
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  
  lda #tUInt
  jmp pushTOPExit
  
  


cloneArray:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutNewLine
  jsr diagnosticOutString
  .byte $0D, "?CloneArray", 0
  .endif
  jmp throwToys
  