; ######################## PUSH OpCode subroutines ######################## 



opcodePUSHSTACKADDRB:
  ; PC++
  inc PCL
  bne incPCopcodePUSHSTACKADDRB
  inc PCH
incPCopcodePUSHSTACKADDRB:
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
  
  bpl phsbPositive
  ; offset >= 128
  ; BP -= 256
  dec IDXH
phsbPositive:
  ; BP += offset
  clc
  adc IDXL
  sta IDXL
  bcc phsbEnd
  inc IDXH
phsbEnd:

  ;ushort localAddress = (ushort)(bp + offset);
  ;Push(localAddress, HopperType.tReference);
  
  lda #tReference
  jmp pushIDXExit






opcodePUSHGP:

  .ifdef STACK8
  
  ldx SP8
  stz HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  inx
  lda #>HopperValueStack
  sta HopperValueStack, X
  inx
  stx SP8
  
  .else
  
  lda #<HopperValueStack ; LSB
  sta (SP)
  jsr incSP ; SP++
  
  lda #>HopperValueStack ; MSB
  sta (SP)
  jsr incSP ; SP++
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  
  .endif
  
  jmp nextInstruction
  
  
  
opcodePUSHIBLE:

   ; top
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIBLEEnd
  inc PCH
incPCopcodePUSHIBLEEnd:
  .endif

  lda (PC)  ; LSB
  sta TOPL
  
  lda (PC)  ; MSB
  stz TOPH
  bra sharedPUSHILE


opcodePUSHIWLE:
  
  ; top
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIWLEEnd
  inc PCH
incPCopcodePUSHIWLEEnd:
  .endif

  lda (PC)  ; LSB
  sta TOPL
  
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIWLEEnd2
  inc PCH
incPCopcodePUSHIWLEEnd2:
  .endif
  lda (PC)  ; MSB
  sta TOPH
  
sharedPUSHILE:
  ; next
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta NEXTH
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; MSB
  .else
  lda SPL
  bne decSPopcodePUSHIWLE
  dec SPH
decSPopcodePUSHIWLE:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTH
  .endif
  
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta NEXTL
  stx SP8
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; LSB
  .else
  lda SPL
  bne decSPopcodePUSHIWLE2
  dec SPH
decSPopcodePUSHIWLE2:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTL
  .ifdef CHECKED
  jsr decTSPnoA
  .else
  
  lda TSPL
  bne decTSPopcodePUSHIWLE
  dec TSPH
decTSPopcodePUSHIWLE:
  dec TSPL
  
  .endif
  .endif
  
  ; opcodeLE
  ldx #1 ; NEXT <= TOP
  lda NEXTH
  cmp TOPH
  bne donePUSHIWLE
  lda NEXTL
  cmp TOPL
donePUSHIWLE:
  ; http://6502.org/tutorials/compare_instructions.html
  beq phPUSHIWLE ; NEXT == TOP (not >)
  bcc phPUSHIWLE ; NEXT <  TOP (not >)
  ldx #0   ; NEXT > TOP
phPUSHIWLE:
  stx TOPL
  stz TOPH

  lda #tBool
  jmp pushTOPExit





opcodePUSHIBEQ:
  
  ; top
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIBEQEnd
  inc PCH
incPCopcodePUSHIBEQEnd:
  .endif

  lda (PC)  ; LSB
  sta TOPL
  
  stz TOPH  ; MSB
  
  ; next
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta NEXTH
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; MSB
  .else
  lda SPL
  bne decSPopcodePUSHIBEQ
  dec SPH
decSPopcodePUSHIBEQ:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTH
  .endif
  
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta NEXTL
  stx SP8
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; LSB
  .else
  lda SPL
  bne decSPopcodePUSHIBEQ2
  dec SPH
decSPopcodePUSHIBEQ2:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTL
  .ifdef CHECKED
  jsr decTSPnoA
  .else
  
  lda TSPL
  bne decTSPopcodePUSHIBEQ
  dec TSPH
decTSPopcodePUSHIBEQ:
  dec TSPL
  
  .endif
  .endif
  
  ; opcodeLE
  ldx #0 ; NEXT != TOP
  lda NEXTH
  cmp TOPH
  bne donePUSHIBEQ
  lda NEXTL
  cmp TOPL
  bne donePUSHIBEQ
  
  ldx #1   ; NEXT == TOP
donePUSHIBEQ:
  stx TOPL
  stz TOPH


  lda #tBool
  jmp pushTOPExit







opcodePUSHIWLEI:
  
  ; top
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIWLEIEnd
  inc PCH
incPCopcodePUSHIWLEIEnd:
  .endif

  lda (PC)  ; LSB
  sta TOPL
  
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIWLEIEnd2
  inc PCH
incPCopcodePUSHIWLEIEnd2:
  .endif
  lda (PC)  ; MSB
  sta TOPH
  
  ; next
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta NEXTH
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; MSB
  .else
  lda SPL
  bne decSPopcodePUSHIWLEI
  dec SPH
decSPopcodePUSHIWLEI:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTH
  .endif
  
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta NEXTL
  stx SP8
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; LSB
  .else
  lda SPL
  bne decSPopcodePUSHIWLEI2
  dec SPH
decSPopcodePUSHIWLEI2:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTL
  .ifdef CHECKED
  jsr decTSPnoA
  .else
  
  lda TSPL
  bne decTSPopcodePUSHIWLEI
  dec TSPH
decTSPopcodePUSHIWLEI:
  dec TSPL
  
  .endif
  .endif
  
  ; opcodeLEI : utilityIntLE

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
  
  bcs opcodePUSHIWLEINegative
  ; 0 or positive
  ;true
  lda #1
  sta TOPL
opcodePUSHIWLEINegative: 

  lda #tBool
  jmp pushTOPExit





opcodePUSHIWLT:
  
  ; top
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIWLTEnd
  inc PCH
incPCopcodePUSHIWLTEnd:
  .endif

  lda (PC)  ; LSB
  sta TOPL
  
  .ifdef CHECKED
  jsr incPC ; PC++
  .else
  inc PCL
  bne incPCopcodePUSHIWLTEnd2
  inc PCH
incPCopcodePUSHIWLTEnd2:
  .endif
  lda (PC)  ; MSB
  sta TOPH
  
  ; next
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta NEXTH
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; MSB
  .else
  lda SPL
  bne decSPopcodePUSHIWLT
  dec SPH
decSPopcodePUSHIWLT:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTH
  .endif
  
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta NEXTL
  stx SP8
  .else
  .ifdef CHECKED
  jsr decSPnoA       ; LSB
  .else
  lda SPL
  bne decSPopcodePUSHIWLT2
  dec SPH
decSPopcodePUSHIWLT2:
  dec SPL
  .endif
  
  lda (SP)
  sta NEXTL
  .ifdef CHECKED
  jsr decTSPnoA
  .else
  
  lda TSPL
  bne decTSPopcodePUSHIWLT
  dec TSPH
decTSPopcodePUSHIWLT:
  dec TSPL
  
  .endif
  .endif

  ; opcodeLT
  ldx #1 ; NEXT < TOP
  lda NEXTH
  cmp TOPH
  bne donePUSHIWLT
  lda NEXTL
  cmp TOPL
donePUSHIWLT:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc phPUSHIWLT ; NEXT < TOP
  ldx #0   
phPUSHIWLT:
  stx TOPL
  stz TOPH

  lda #tBool
  jmp pushTOPExit
  
  
  
  
  


opcodePUSHDW:
  ; PC++
  inc PCL
  bne incPCopcodePUSHDW
  inc PCH
incPCopcodePUSHDW:

  lda (PC)  ; LSB
  sta ACCL
  
  ; PC++
  inc PCL
  bne incPCopcodePUSHDW2
  inc PCH
incPCopcodePUSHDW2:
  
  lda (PC)  ; MSB
  sta ACCH
  
  bit ACCH
  bmi delegateLookup
  bra sharedPUSHIW
delegateLookup:
  bvc sharedPUSHIW ; only lookup if both bits set

  ; method table index is in bottom 14 bits of ACC
  lda ACCH
  and #$3F
  sta ACCH
  
  lda #<HopperMethodTable
  sta IDXL
  lda #>HopperMethodTable
  sta IDXH
  
delegateHunt:
  ldy #1
  
  lda (IDX)
  cmp ACCL
  bne nextDelegate
  lda (IDX), Y
  cmp ACCH
  bne nextDelegate
  
  ; we have a winner: load actual address from method table to ACC
  iny
  lda (IDX), Y
  sta ACCL
  iny
  lda (IDX), Y
  sta ACCH
  
  ; modify this PUSHDW's operand:
  lda PCL
  sta IDYL
  lda PCH
  sta IDYH
  jsr decIDY
  
  ; don't forget relocation offset
  ldy #1
  clc
  lda ACCL
  adc #<HopperData
  sta ACCL
  sta (IDY)
  lda ACCH
  adc #>HopperData
  sta ACCH
  sta (IDY), Y
  
  bra sharedPUSHIW
  
nextDelegate:
  clc
  lda IDXL
  adc #4
  sta IDXL
  bcc jumpDelegateHunt
  inc IDXH
jumpDelegateHunt:
  bra delegateHunt

  
opcodePUSHIW:
  ; PC++
  inc PCL
  bne incPCopcodePUSHIW
  inc PCH
incPCopcodePUSHIW:

  lda (PC)  ; LSB
  sta ACCL
  
  ; PC++
  inc PCL
  bne incPCopcodePUSHIW2
  inc PCH
incPCopcodePUSHIW2:
  
  lda (PC)  ; MSB
  sta ACCH
  
sharedPUSHIW:
  
  .ifdef STACK8
  
  ldx SP8
  lda ACCL
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  inx
  lda ACCH
  sta HopperValueStack, X
  
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  inx
  stx SP8
  
  .else ; STACK8
  
  lda ACCL
  sta (SP)
  lda #tUInt
  sta (TSP)
  jsr incTSP
  jsr incSP ; SP++
  
  lda ACCH
  sta (SP)
  jsr incSP ; SP++
  
  .endif ; !STACK8
  
  jmp nextInstruction




  
  
  .ifdef STACK8
  
opcodePUSHIM1:
  ldx SP8
  lda #$FF
  sta HopperValueStack, X
  lda #tInt
  sta HopperTypeStack, X
  inx
  lda #$FF
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  jmp nextInstruction
  
opcodePUSHI0:
  ldx SP8
  stz HopperValueStack, X
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
  jmp nextInstruction
  
opcodePUSHI1:
  ldx SP8
  lda #1
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
  jmp nextInstruction
  
opcodePUSHIB:
  ; PC++
  inc PCL
  bne incPCopcodePUSHIB
  inc PCH
incPCopcodePUSHIB:
  
  lda (PC)  ; LSB
  ldx SP8
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
  jmp nextInstruction
  
  .else  

opcodePUSHIM1:

  lda #$FF
  sta (SP)
  jsr incSP ; SP++
  sta (SP)
  jsr incSP ; SP++
  
  lda #tInt
  sta (TSP)
  jsr incTSP
  jmp nextInstruction
  
opcodePUSHIB:
  jsr incPC ; PC++
  lda (PC)  ; LSB
  bra pushByte

opcodePUSHI1:
  lda #1
  bra pushByte

opcodePUSHI0:
  lda #0
  
   ; fall through to pushByte
pushByte:
  
  .ifdef CHECKED
  
  sta (SP)           ; LSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  jsr incSP ; SP++
  lda #0
  sta (SP)           ; MSB
  jsr incSP ; SP++
  
  .else
  
  sta (SP)           ; LSB
  inc SPL
  bne pushByteEnd
  inc SPH
pushByteEnd:  
  
  lda #0
  sta (SP)           ; MSB
  inc SPL
  bne pushByteEnd2
  inc SPH
pushByteEnd2:
  lda #tUInt
  sta (TSP)
  inc TSPL
  bne pushByteEnd3
  inc TSPH
pushByteEnd3:
  
  .endif
  
  jmp nextInstruction
  .endif





  .ifdef STACK8

opcodePUSHGLOBALBB:
  ; exact 2nd copy of the code below from opcodePUSHGLOBALB (without the jmp nextInstruction)
  ; PC++
  inc PCL
  bne incPCutilityPUSHGLOBALBB
  inc PCH
incPCutilityPUSHGLOBALBB:

  ; IDX = HopperValueStack
  ; IDY = HopperTypeStack
  lda #>HopperValueStack
  sta IDXH
  lda #>HopperTypeStack
  sta IDYH
  
  lda (PC)  ; offset 0..255
  sta IDXL
  sta IDYL
  
  ; push from valueStack[IDX] (IDX is the absolute globalAddress in the stack)
  ldx SP8
  lda (IDX)
  sta HopperValueStack, X
  lda (IDY)
  sta HopperTypeStack, X
  inx
  ldy #1
  lda (IDX), Y
  sta HopperValueStack, X
  inx
  stx SP8
  
  lda (IDY)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePUSHGLOBALBValueType2
  
  .ifdef CHECKED
  jsr addReference
  .else
  
  ; (IDX) -> IDX
  ldy #0
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  ; address in IDX
  ldy #1
  lda (IDX), Y ; reference count
  inc
  sta (IDX), Y
  
  .endif
opcodePUSHGLOBALBValueType2:
  

  
  
opcodePUSHGLOBALB:
  ; PC++
  inc PCL
  bne incPCutilityPUSHGLOBALB
  inc PCH
incPCutilityPUSHGLOBALB:

  ; IDX = HopperValueStack
  ; IDY = HopperTypeStack
  lda #>HopperValueStack
  sta IDXH
  lda #>HopperTypeStack
  sta IDYH
  
  lda (PC)  ; offset 0..255
  sta IDXL
  sta IDYL
  
  ; push from valueStack[IDX] (IDX is the absolute globalAddress in the stack)
  ldx SP8
  lda (IDX)
  sta HopperValueStack, X
  lda (IDY)
  sta HopperTypeStack, X
  inx
  ldy #1
  lda (IDX), Y
  sta HopperValueStack, X
  inx
  stx SP8
  
  lda (IDY)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePUSHGLOBALBValueType
  
  .ifdef CHECKED
  jsr addReference
  .else
  
  ; (IDX) -> IDX
  ldy #0
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  ; address in IDX
  ldy #1
  lda (IDX), Y ; reference count
  inc
  sta (IDX), Y
  
  .endif
opcodePUSHGLOBALBValueType:
  jmp nextInstruction
  
  
  .else ; STACK8


opcodePUSHGLOBALBB:

  jsr utilityPUSHGLOBALB
  jsr utilityPUSHGLOBALB
  
  jmp nextInstruction

opcodePUSHGLOBALB:  

  jsr utilityPUSHGLOBALB
  jmp nextInstruction
  
utilityPUSHGLOBALB:
  
  ; PC++
  inc PCL
  bne incPCutilityPUSHGLOBALB
  inc PCH
incPCutilityPUSHGLOBALB:

  ; IDX = HopperValueStack
  
  lda #>HopperValueStack
  sta IDXH
  lda (PC)  ; offset 0..255
  sta IDXL
 
  jsr convertSPtoTSP ; IDX -> IDY

  ; push from valueStack[IDX] (IDX is the absolute globalAddress in the stack)
  lda (IDX)
  sta (SP)
  jsr incSP
  ldy #1
  lda (IDX), Y
  sta (SP)
  jsr incSP
  
  lda (IDY)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePUSHGLOBALBValueType
  
  .ifdef CHECKED
  
  jsr addReference
  
  .else
  ; inline: munts X,Y and A
  ; (IDX) -> IDX
  ldy #0
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  ; address in IDX
  ldy #1
  lda (IDX), Y ; reference count
  inc
  sta (IDX), Y
  
  .endif
  
opcodePUSHGLOBALBValueType:
  
  lda (IDY)
  sta (TSP)
  jsr incTSP
  rts
  

  .endif ; !STACK8


opcodePUSHRELB:
  ; PC++
  inc PCL
  bne incPCopcodePUSHRELB
  inc PCH
incPCopcodePUSHRELB:
  lda (PC)  ; offset
  
  ; A   = offset
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
  
  bpl phrbPositive
  ; offset >= 128
  ; BP -= 256
  dec IDXH
phrbPositive:
  ; BP += offset
  clc
  adc IDXL
  sta IDXL
  bcc phrbEnd
  inc IDXH
phrbEnd:

  jsr convertSPtoTSP ; IDX -> IDY
  
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
  jsr opcodePUSHLOCALBSimpleOffset
  jmp nextInstruction


  .ifdef STACK8
opcodePUSHLOCALValues:
  ldy SP8
  sta HopperTypeStack, Y
  lda HopperValueStack, X
  sta HopperValueStack, Y
  iny
  inx
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y
  .endif
  lda HopperValueStack, X
  sta HopperValueStack, Y
  iny
  sty SP8
  jmp nextInstruction
  .endif

opcodePUSHLOCALB00:
  .ifdef STACK8
  ldx BP8
  lda HopperTypeStack, X
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePUSHLOCALValues
  stx IDXL
  lda #>HopperValueStack
  sta IDXH
  .else
  lda BPL
  sta IDXL
  lda BPH
  sta IDXH
  .endif
  jsr opcodePUSHLOCALBSimpleOffset
  jmp nextInstruction

opcodePUSHLOCALB02:
  .ifdef STACK8
  ldx BP8
  inx
  inx
  lda HopperTypeStack, X
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePUSHLOCALValues
  stx IDXL
  lda #>HopperValueStack
  sta IDXH
  jsr opcodePUSHLOCALBSimpleOffset
  jmp nextInstruction
  .else
  lda #2
  jsr opcodePUSHLOCALBShared
  jmp nextInstruction
  .endif
  
  
  .ifdef STACK8

opcodePUSHLOCALBB:
  ; PC++
  inc PCL
  bne incPCopcodePUSHLOCALB1
  inc PCH
incPCopcodePUSHLOCALB1:
  lda (PC)  ; offset
  jsr opcodePUSHLOCAL8Helper 
  
  ; PC++
  inc PCL
  bne incPCopcodePUSHLOCALB2
  inc PCH
incPCopcodePUSHLOCALB2:
  lda (PC)  ; offset
  jsr opcodePUSHLOCAL8Helper 
  
  jmp nextInstruction
  
opcodePUSHLOCALB:
  ; PC++
  inc PCL
  bne incPCopcodePUSHLOCALB
  inc PCH
incPCopcodePUSHLOCALB:
  lda (PC)  ; offset
  jsr opcodePUSHLOCAL8Helper 
  jmp nextInstruction
  
opcodePUSHLOCAL8Helper:
  clc
  adc BP8
  tay
  lda HopperTypeStack, Y
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs opcodePUSHLOCALBReferenceType
  
  ldx SP8
  sta HopperTypeStack, X
  lda HopperValueStack, Y
  sta HopperValueStack, X
  
  iny
  inx
  lda HopperValueStack, Y
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  rts
  
opcodePUSHLOCALBReferenceType:
  lda (PC) ; reload the offset
  bra opcodePUSHLOCALBShared
  .else
  
opcodePUSHLOCALBB:
  jsr incPC ; PC++
  lda (PC)  ; offset
  jsr opcodePUSHLOCALBShared
  jsr incPC ; PC++
  lda (PC)  ; offset
  jsr opcodePUSHLOCALBShared
  jmp nextInstruction
  
opcodePUSHLOCALB:
  jsr incPC ; PC++
  lda (PC)  ; offset
  jsr opcodePUSHLOCALBShared
  jmp nextInstruction
  .endif
  
  ; fall through
opcodePUSHLOCALBShared:
  ; A   = offset
  ; IDX = BP
  tax
  
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
  txa
  
  ; IDX += offset
  bpl phlbPositive
  
  ; offset >= 128
  ; BP -= 256
  dec IDXH
phlbPositive:
  ; BP += offset
  clc
  adc IDXL
  sta IDXL
  bcc phlbEnd
  inc IDXH
phlbEnd:
  
opcodePUSHLOCALBSimpleOffset:
  jsr convertSPtoTSP ; IDX -> IDY
  ; push from valueStack[IDX] (IDX is the absolute localAddress in the stack)
  lda (IDX)
  .ifdef STACK8
  
  ldx SP8
  sta HopperValueStack, X
  
  .else
  
  sta (SP)
  
  .endif
  ldy #1
  lda (IDX), Y
  .ifdef STACK8
  inx
  sta HopperValueStack, X
  .else
  sta (SP), Y
  .endif
  
  .ifdef CHECKED
  .ifdef STACK8
  inc SP8
  inc SP8
  .else
  jsr incSP
  jsr incSP
  .endif
  .else
  .ifdef STACK8
  inc SP8
  inc SP8
  .else
  ; inline:
  clc
  lda #2
  adc SPL
  sta SPL
  bcc incSPEndPUSHLOCALB
  inc SPH
incSPEndPUSHLOCALB:  
  .endif
  .endif
  
  lda (IDY)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodePUSHLOCALBValueType
  
  .ifdef CHECKED
  jsr addReference
  .else
  ; inline: munts X,Y and A
  ; (IDX) -> IDX
  ldy #0
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  ; address in IDX
  ldy #1
  lda (IDX), Y ; reference count
  inc
  sta (IDX), Y
  
  .endif
opcodePUSHLOCALBValueType:
  
  lda (IDY)
  .ifdef STACK8
  ldx SP8
  dex
  dex
  sta HopperTypeStack, X
  .else
  sta (TSP)
  .endif
  
  .ifndef STACK8
  .ifdef CHECKED
  jsr incTSP
  .else
  
  ; inline:
  inc TSPL
  bne incTSPEndPUSHLOCALB
  inc TSPH
incTSPEndPUSHLOCALB:
  .endif
  .endif
  
  rts
