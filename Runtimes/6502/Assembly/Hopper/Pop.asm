; ######################## POP OpCode subroutines ######################## 

  .ifdef HEAP
opcodePOPCOPYRELB:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif

opcodePOPRELB:
  
  ; short offset = (short)operand;
  ; if (offset > 127)
  ; {
  ;   offset = (short)(offset - 256); // 255 -> -1
  ; }
  ; ushort referenceAddress = (ushort)(bp + offset);
  ; ushort localAddress = (ushort)GetStack(referenceAddress);
  
  ; TODO - test positive offset:
  jsr incPC ; PC++
  lda (PC)  ; offset
  
  ; IDX = BP
  pha
  .ifdef STACK8
  
  lda BP8
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  
  .else
  
  lda BPL
  sta IDXL
  lda BPH
  sta IDXH
  
  .endif
  pla
  
  ; IDX += offset
  
  bpl plrbPositive
  ; offset >= 128
  ; -= 256
  dec IDXH
plrbPositive:
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc plrbEnd
  inc IDXH
plrbEnd:

  ;jsr convertSPtoTSP ; IDX -> IDY
  
  ;.ifdef CHECKED
  ;lda (IDY)
  ;jsr assertReference
  ;.endif
  
  ; IDX = (IDX)
  ldy #1
  lda (IDX)
  tax
  lda (IDX), Y
  STA IDXH
  txa
  sta IDXL
  
  jmp regularPOPLOCALB



  .ifdef STACK8

 .ifdef HEAP
opcodePOPCOPYLOCALB00:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif

opcodePOPLOCALB00:
  ldx SP8
  dex
  dex
  lda HopperTypeStack, X
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs opcodePOPLOCALBReferenceType0
  stx SP8
  
  ldy BP8
  
  lda HopperTypeStack, X
  sta HopperTypeStack, Y
  lda HopperValueStack, X ; LSB
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, X ; MSB
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  jmp nextInstruction
  
opcodePOPLOCALBReferenceType0:
  lda #0 ; reload the offset
  bra opcodePOPLOCALBShared

  .ifdef HEAP
opcodePOPCOPYLOCALB02:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif

opcodePOPLOCALB02:
  ldx SP8
  dex
  dex
  lda HopperTypeStack, X
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs opcodePOPLOCALBReferenceType2
  stx SP8
  
  ldy BP8
  iny
  iny
  
  lda HopperTypeStack, X
  sta HopperTypeStack, Y
  lda HopperValueStack, X ; LSB
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, X ; MSB
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  jmp nextInstruction
  
opcodePOPLOCALBReferenceType2:
  lda #2 ; reload the offset
  bra opcodePOPLOCALBShared
  
 .ifdef HEAP
opcodePOPCOPYLOCALB:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif
  
opcodePOPLOCALB:
  ; PC++
  inc PCL
  bne incPCopcodePOPLOCALB
  inc PCH
incPCopcodePOPLOCALB:
  
  ldx SP8
  dex
  dex
  lda HopperTypeStack, X
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs opcodePOPLOCALBReferenceType
  stx SP8
  
  lda (PC)  ; offset
  clc
  adc BP8
  tay
  
  lda HopperTypeStack, X
  sta HopperTypeStack, Y
  lda HopperValueStack, X ; LSB
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, X ; MSB
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  jmp nextInstruction
  
opcodePOPLOCALBReferenceType:
  lda (PC) ; reload the offset
  bra opcodePOPLOCALBShared
  
  .else ; STACK8
  
  .ifdef HEAP
opcodePOPCOPYLOCALB00:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif
  
opcodePOPLOCALB00:
  lda #0
  bra opcodePOPLOCALBShared
  
  .ifdef HEAP
opcodePOPCOPYLOCALB02:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif

opcodePOPLOCALB02:
  lda #2
  bra opcodePOPLOCALBShared

  .ifdef HEAP
opcodePOPCOPYLOCALB:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif
   
opcodePOPLOCALB:

  ; TODO - test positive offset:
  jsr incPC ; PC++
  lda (PC)  ; offset
  
  .endif
  
opcodePOPLOCALBShared:
  ; IDX = BP
  pha
  
  .ifdef STACK8
  
  lda BP8
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  
  .else
  
  lda BPL
  sta IDXL
  lda BPH
  sta IDXH
  .endif
  
  pla
  
  ; IDX += offset
  
  bpl pllbPositive
  ; offset >= 128
  ; -= 256
  dec IDXH
pllbPositive:
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc pllbEnd
  inc IDXH
pllbEnd:

regularPOPLOCALB:

  jsr convertSPtoTSP ; IDX -> IDY
  
  ; We're about to overwrite valueStack[IDX], check if it needs gcRelease
  lda (IDY)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePOPLOCALBValueType
  
  .ifdef HEAP
  jsr releaseIDXIDY
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodePOPLOCALBValueType:
  
  .ifdef STACK8
  dec SP8
  dec SP8
  .else
  jsr decSP         
  jsr decSP         
  jsr decTSP
  .endif
  
  .ifdef HEAP
  ; COPYNEXTPOP = 1 implies a reference type
  ; if valueStack[IDX] == valueStack[SP] then no copy is needed.
  ; otherwise:
  ; - decrease the reference count of valueStack[SP]
  ; - make a copy of valueStack[SP] to put in valueStack[IDX] 
   
  lda COPYNEXTPOP
  beq noCopyNeededPOPLOCALB
  
  ldy #1
  .ifdef STACK8
  inx
  lda HopperValueStack, X
  dex
  .else
  lda (SP), Y
  .endif
  cmp (IDX), Y
  bne copyNeededPOPLOCALB
  .ifdef STACK8
  lda HopperValueStack, X
  .else
  lda (SP)
  .endif
  
  cmp (IDX)
  beq noCopyNeededPOPLOCALB
copyNeededPOPLOCALB:
  jsr cloneSP ; preserves IDX and Y
  
noCopyNeededPOPLOCALB:  
  stz COPYNEXTPOP
  
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif ; HEAP
  
  ; Pop into valueStack[IDX] (IDX is the absolute localAddress in the stack)
  ; Reference count for reference types stays the same (-1 for pop, +1 for assignment)
  ldy #1
  .ifdef STACK8
  ldx SP8
  inx
  lda HopperValueStack, X
  dex
  .else
  lda (SP), Y
  .endif
  sta (IDX), Y
  .ifdef STACK8
  lda HopperValueStack, X
  .else
  lda (SP)
  .endif
  sta (IDX)
  
  .ifdef STACK8
  lda HopperTypeStack, X
  .else
  lda (TSP)
  .endif
  sta (IDY)
  
  jmp nextInstruction
  
  .ifdef HEAP
opcodePOPCOPYGLOBALB:
  lda #1
  sta COPYNEXTPOP
  ; fall through
  .endif
  
opcodePOPGLOBALB:
   
  ; IDX = HopperValueStack
  lda #<HopperValueStack
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  
    ; PC++
  inc PCL
  bne incPCopcodePOPGLOBALB
  inc PCH
incPCopcodePOPGLOBALB:

  lda (PC)  ; offset 0..255
  
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc plgbEnd
  inc IDXH
plgbEnd:
  jsr convertSPtoTSP ; IDX -> IDY
  
  ; We're about to overwrite valueStack[IDX], check if it needs gcRelease
  lda (IDY)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePOPGLOBALBValueType
  
  .ifdef HEAP
  jsr releaseIDXIDY
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodePOPGLOBALBValueType:

  .ifdef STACK8
  dec SP8
  dec SP8
  .else
  ldy #1
  jsr decSP         
  jsr decSP         
  jsr decTSP
  .endif
  
  .ifdef HEAP
  ; COPYNEXTPOP = 1 implies a reference type
  ; if valueStack[IDX] == valueStack[SP] then no copy is needed.
  ; otherwise:
  ; - decrease the reference count of valueStack[SP]
  ; - make a copy of valueStack[SP] to put in valueStack[IDX] 
  
  lda COPYNEXTPOP
  beq noCopyNeededPOPGLOBALB
  
  .ifdef STACK8
  
  ldx SP8
  lda HopperValueStack, X
  cmp (IDX)
  bne copyNeededPOPGLOBALB  
  inx
  ldx SP8
  ldy #1
  lda HopperValueStack, X
  cmp (IDX), Y
  beq noCopyNeededPOPGLOBALB
  
  .else
  
  ldy #1
  lda (SP), Y
  cmp (IDX), Y
  bne copyNeededPOPGLOBALB
  lda (SP)
  cmp (IDX)
  beq noCopyNeededPOPGLOBALB
  
  .endif
  
copyNeededPOPGLOBALB:
  jsr cloneSP ; preserves IDX and Y
  
noCopyNeededPOPGLOBALB:  
  
  stz COPYNEXTPOP
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif ; HEAP
  
  ; Pop into valueStack[IDX] (IDX is the absolute globalAddress in the stack)
  ; Reference count for reference types stays the same (-1 for pop, +1 for assignment)
  .ifdef STACK8
  
  ldx SP8
  lda HopperValueStack, X
  sta (IDX)
  lda HopperTypeStack, X
  sta (IDY)
  inx
  lda HopperValueStack, X
  ldy #1
  sta (IDX), Y
  .ifdef CHECKED
  lda #$AA
  sta (IDY), Y
  .endif
  
  .else
  
  ldy #1
  lda (SP), Y       ; MSB
  sta (IDX), Y      
  lda (SP)          ; LSB  
  sta (IDX)
  lda (TSP)
  sta (IDY)
  
  .endif

  jmp nextInstruction


