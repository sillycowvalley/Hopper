

throwToys:
  jsr diagnosticOut
  ;jsr diagnosticOutStack
  jsr diagnosticOutCallStack
  jmp monitorReEntry

throwToysNoStack:
  jsr diagnosticOut
  jmp monitorReEntry

  .ifdef UNUSED
diagnosticOutMini:
  pha
  
  lda (PC)
  
  ;cmp #$49 ; ENTER
  ;beq diagnosticOutMiniShow
  
  cmp #$34 ; CALLW
  beq diagnosticOutMiniShow
  
  cmp #$2A ; RETB
  beq diagnosticOutMiniShow
  
  cmp #$2B ; RETRETB
  beq diagnosticOutMiniShow
  
  cmp #$4A ; RET0
  beq diagnosticOutMiniShow
  
  cmp #$4B ; CALLREL
  beq diagnosticOutMiniShow
  
  bra diagnosticOutMiniExit
  
diagnosticOutMiniShow:
  ; PC
  lda PCH
  sec
  sbc #>HopperData
  jsr diagnosticOutHex
  lda PCL
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
  ; current instruction at PC
  lda (PC)         
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
diagnosticOutMiniExit:

  jsr diagnosticOutCallStack
  
  pla
  rts
  .endif ; UNUSED
  
diagnosticOut:
  pha
  
  jsr diagnosticOutNewLine
  
  ; PC
  lda #"P"
  jsr diagnosticOutChar
  lda #"C"
  jsr diagnosticOutChar
  lda #"="
  jsr diagnosticOutChar
  lda PCH
  sec
  sbc #>HopperData
  jsr diagnosticOutHex
  lda PCL
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
  lda #"C"
  jsr diagnosticOutChar
  lda #"S"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  lda #"="
  jsr diagnosticOutChar
  lda CSP
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
  lda #"S"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  lda #"="
  jsr diagnosticOutChar
  .ifdef STACK8
  lda #>HopperValueStack
  jsr diagnosticOutHex
  lda SP8
  jsr diagnosticOutHex
  .else
  lda SPH
  jsr diagnosticOutHex
  lda SPL
  jsr diagnosticOutHex
  .endif
  lda #" "
  jsr diagnosticOutChar
  
  lda #"T"
  jsr diagnosticOutChar
  lda #"S"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  lda #"="
  jsr diagnosticOutChar
  .ifdef STACK8
  lda #>HopperTypeStack
  jsr diagnosticOutHex
  lda SP8
  jsr diagnosticOutHex
  .else
  lda TSPH
  jsr diagnosticOutHex
  lda TSPL
  jsr diagnosticOutHex
  .endif
  lda #" "
  jsr diagnosticOutChar
  
  lda #"B"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  lda #"="
  jsr diagnosticOutChar
  
  .ifdef STACK8
  lda #>HopperValueStack
  jsr diagnosticOutHex
  lda BP8
  jsr diagnosticOutHex
  .else
  lda BPH
  jsr diagnosticOutHex
  lda BPL
  jsr diagnosticOutHex
  .endif
  
  lda #" "
  jsr diagnosticOutChar
  lda #"s"
  jsr diagnosticOutChar
  lda #"p"
  jsr diagnosticOutChar
  lda #"="
  jsr diagnosticOutChar
  
  phx
  tsx
  txa
  jsr diagnosticOutHex
  plx
  
  pla
  rts

  .ifdef VERIFYSTACK
  
diagnosticsVerifyStack:
  pha
  phx
  tsx
  cpx DIAGSP
  beq diagnosticsVerifyStackSame
  
  lda DIAGSP
  jsr diagnosticOutHex
  stx DIAGSP ; update it so we get one message after the opCode where there was a change
  jsr diagnosticOut
  
diagnosticsVerifyStackSame
  
  plx
  pla
  rts
  .endif
  
diagnosticOutNewLine:
  ;jsr LCDNewLine
  lda #$0D
  jsr SerialOut
  rts
  
diagnosticOutSpaces:
  lda #" "
  jsr diagnosticOutChar
  dey
  bne diagnosticOutSpaces
  rts
  
diagnosticOutChar:
  jsr SerialOut
  ;jsr LCDCharacter
  rts
  
  
  .ifndef NODIAGNOSTICS
diagnosticOutString:
  ; Inspired by Ross Archer's HEX file loader : http://www.6502.org/source/monitors/intelhex/intelhex.htm
  ; and by https://beebwiki.mdfs.net/6502_routines
  ; Note: trashes A
  pla                           ; Get the low part of "return" address (data start address)
  sta     ScratchPointerL
  pla                           ; Get the high part of "return" address
  sta     ScratchPointerH       ; (data start address)
  phy
  ; Note: actually we're pointing one short
diagnosticOutStringStringByte:
  ldy     #1
  lda     (ScratchPointerL),y         ; Get the next string character
  inc     ScratchPointerL             ; update the pointer
  bne     diagnosticOutStringNextCharacter          ; if not, we're pointing to next character
  inc     ScratchPointerH             ; account for page crossing
diagnosticOutStringNextCharacter:
  ora     #0                ; Set flags according to contents of Accumulator
  beq     diagnosticOutStringStringExit1       ; don't print the final NULL
  jsr     diagnosticOutChar ; write it out
  jmp     diagnosticOutStringStringByte        ; back around
diagnosticOutStringStringExit1:
  inc     ScratchPointerL
  bne     diagnosticOutStringStringExit2
  inc     ScratchPointerH             ; account for page crossing
diagnosticOutStringStringExit2:
  ply
  jmp     (ScratchPointerL)           ; return to byte following final NULL
  .endif ; NODIAGNOSTICS

diagnosticOutHex:
  jsr SerialHexOut
  ;jsr LCDHexCharacters
  rts

  
  .ifdef UNUSED
diagnosticFreeList:
  pha
  phy
  
  lda FREELISTH
  sta ScratchPointerH
  lda FREELISTL
  sta ScratchPointerL
  
  jsr diagnosticOutNewLine
nextFreeList:  
  jsr diagnosticOutNewLine
  lda ScratchPointerH
  jsr diagnosticOutHex
  lda ScratchPointerL
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  ; size
  ldy #1
  lda (ScratchPointer), Y
  jsr diagnosticOutHex
  dey
  lda (ScratchPointer), Y
  jsr diagnosticOutHex
  lda #","
  jsr diagnosticOutChar
  ; next
  ldy #3
  lda (ScratchPointer), Y
  jsr diagnosticOutHex
  dey
  lda (ScratchPointer), Y
  jsr diagnosticOutHex
  lda #","
  jsr diagnosticOutChar
  ; prev
  ldy #5
  lda (ScratchPointer), Y
  jsr diagnosticOutHex
  dey
  lda (ScratchPointer), Y
  jsr diagnosticOutHex
  
  ldy #3
  lda (ScratchPointer), Y
  pha
  dey
  lda (ScratchPointer), Y
  sta ScratchPointerL
  pla
  sta ScratchPointerH
  lda #0
  cmp ScratchPointerL
  bne nextFreeList
  cmp ScratchPointerH
  bne nextFreeList
  
  ply
  pla
  rts
  .endif ; UNUSED
  
diagnosticOutCallStack:
  pha
  phx
  phy
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "        RET  BP", 0
  .else
  jsr diagnosticOutNewLine
  ldy #8
  jsr diagnosticOutSpaces
  lda #"R"
  jsr diagnosticOutChar
  lda #"E"
  jsr diagnosticOutChar
  lda #"T"
  jsr diagnosticOutChar
  ldy #2
  jsr diagnosticOutSpaces
  lda #"B"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  .endif
  ldx #0
  cpx CSP
  beq diagnosticOutCallStackExit ; empty call stack
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "     ", 0
  .else
  jsr diagnosticOutNewLine
  ldy #5
  jsr diagnosticOutSpaces
  .endif
  ; emit X
  txa
  jsr diagnosticOutHex
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte "      ", 0
  .else
  ldy #6
  jsr diagnosticOutSpaces
  .endif
   
  bra diagnosticOutCallStackFirstSlot
  
nextCallStackItem:
  ; compare X to CSP
  cpx CSP
  bne emitCallStackItem
  ; call stack is empty
  jmp diagnosticOutCallStackExit
  
emitCallStackItem:
  
  ; leading spaces
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "     ", 0
  .else
  jsr diagnosticOutNewLine
  ldy #5
  jsr diagnosticOutSpaces
  .endif
  ; emit IDX
  txa
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
  ; RET: emit call stack stack item at IDX
  inx
  LDA HopperCallStack, X
  sec
  sbc #>HopperData
  jsr diagnosticOutHex ; MSB
  dex
  LDA HopperCallStack, X
  jsr diagnosticOutHex ; LSB
  inx
  inx
  
  lda #" "
  jsr diagnosticOutChar
  
  ; TODO REMOVE
  cpx CSP
  beq diagnosticOutCallStackExit
  
  
diagnosticOutCallStackFirstSlot:
  
  ; BP: emit call stack stack item at IDX
  inx
  LDA HopperCallStack, X
  jsr diagnosticOutHex ; MSB
  dex
  LDA HopperCallStack, X
  jsr diagnosticOutHex ; LSB
  inx
  inx
  
  jmp nextCallStackItem
  
diagnosticOutCallStackExit:

  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "CSP  ", 0
  .else
  jsr diagnosticOutNewLine
  lda #"C"
  jsr diagnosticOutChar
  lda #"S"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  lda #" "
  jsr diagnosticOutChar
  jsr diagnosticOutChar
  .endif
  ; emit CSP
  txa
  jsr diagnosticOutHex
  jsr diagnosticOutNewLine
  
  ply
  plx
  pla
  rts

  .ifdef UNUSED
diagnosticCheckStack:
  pha
  phx
  phy
  lda dSCRATCHL
  pha
  lda dSCRATCHH
  pha
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  ;jsr diagnosticOutNewLine
  
  .ifdef STACK8
  lda SP8
  .else
  lda SPL
  .endif
  and #$01
  beq diagnosticCheckStackEven
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?SP is odd?!", 0
  .else
  jmp throwToys
  .endif
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  pla
  sta dSCRATCHH
  pla
  sta dSCRATCHL
  ply
  plx
  pla
  
  jmp monitorReEntry
  
  
diagnosticCheckStackEven:
  
  stz IDXL
  lda #>HopperValueStack
  sta IDXH
  
  jsr convertSPtoTSP ; IDX -> IDY
  
  .ifdef STACK8
  
  lda IDXL
  cmp SP8
  bne nextCheckStackItem
  
  .else
  
  lda IDXL
  cmp SPL
  bne nextCheckStackItem
  lda IDXH
  cmp SPH
  bne nextCheckStackItem
  .endif
  
  ; stack is empty
  jmp diagnosticCheckStackExit
  
nextCheckStackItem:

  ;lda #"."
  ;jsr diagnosticOutChar

  lda (IDX)
  sta dSCRATCHL
  
  jsr incIDX
  
  lda (IDX)
  sta dSCRATCHH
  
  jsr incIDX
  
  lda (IDY) ; type stack item
  pha
  
  .ifdef STACK8
  jsr incIDY
  .endif
  jsr incIDY
  
  pla
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc simpleTypeStackCheck
  
  ldy #0
  cmp (dSCRATCH), Y
  beq simpleTypeStackCheck
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda (dSCRATCH), Y
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  
  jsr diagnosticOutString
  .byte $0D, "?stack type != object type", 0
  .else
  jmp throwToys
  .endif
  
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  pla
  sta dSCRATCHH
  pla
  sta dSCRATCHL
  ply
  plx
  pla
  
  jmp monitorReEntry
  

simpleTypeStackCheck:
 
  .ifdef STACK8
  lda SP8
  .else
  lda SPL
  .endif
  and #$01
  bne diagnosticCheckStackExit ; don't try continuing to dump the stack if SP is odd
  
  .ifdef STACK8
  lda IDXL
  cmp SP8
  bne nextCheckStackItemJump
  .else
  lda IDXH
  cmp SPH
  bne nextCheckStackItemJump
  lda IDXL
  cmp SPL
  bne nextCheckStackItemJump
  .endif

diagnosticCheckStackExit:

  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  pla
  sta dSCRATCHH
  pla
  sta dSCRATCHL
  ply
  plx
  pla
  
  rts
  
nextCheckStackItemJump:
  jmp nextCheckStackItem
  .endif ; UNUSED diagnosticCheckStack

diagnosticOutStack:
  ; TODO : this could be much simpler just using X for SP8 (instead of IDX and IDY)
  pha
  phx
  phy
  lda dSCRATCHL
  pha
  lda dSCRATCHH
  pha
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha
  
  stz IDXL
  lda #>HopperValueStack
  sta IDXH
  
  jsr convertSPtoTSP ; IDX -> IDY
  
  ; compare IDX to SP
  .ifdef STACK8
  
  lda IDXL
  cmp SP8
  bne nextStackItem
  
  .else
  lda IDXL
  cmp SPL
  bne nextStackItem
  lda IDXH
  cmp SPH
  bne nextStackItem
  .endif
  
  ; stack is empty
  jmp diagnosticOutStackExit
  
nextStackItem:
  
  ; leading spaces
  jsr diagnosticOutNewLine
  lda #" "
  jsr diagnosticOutChar
  
  .ifdef STACK8
  
  lda IDXL
  cmp BP8
  bne notBP
  
  .else
  lda IDXL
  cmp BPL
  bne notBP
  lda IDXH
  cmp BPH
  bne notBP
  .endif
  
  lda #"B"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  
  
  jmp emitIDX
  
notBP:
  
  lda #" "
  jsr diagnosticOutChar
  jsr diagnosticOutChar
  
emitIDX:
  lda #" "
  jsr diagnosticOutChar
  

  ; emit IDX
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
  ; emit value stack item at IDX
  LDA (IDX)
  sta dSCRATCHL
  
  ; inc IDX
  inc IDXL
  bne skipSPInc0
  inc IDXH
skipSPInc0:
  
  LDA (IDX)
  jsr diagnosticOutHex ; MSB
  sta dSCRATCHH
  lda dSCRATCHL
  jsr diagnosticOutHex ; LSB

  ; inc IDX
  inc IDXL
  bne skipSPInc1
  inc IDXH
skipSPInc1:

  ; emit type stack item at IDY
  lda #" "
  jsr diagnosticOutChar
  LDA (IDY)
  pha
  jsr diagnosticOutHex

  ; inc IDY
  inc IDYL
  bne skipTSPInc0
  inc IDYH
skipTSPInc0:

  .ifdef STACK8
  
  inc IDYL
  bne skipTSPInc1
  inc IDYH
skipTSPInc1:
  
  .endif

  pla
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc simpleTypeStack
  
  pha
  
  lda #"="
  jsr diagnosticOutChar
  ldy #0
  lda (dSCRATCH), Y
  jsr diagnosticOutHex ; type in object
  
  pla
  pha
  cmp (dSCRATCH), Y
  beq typesMatch
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?stack type != object type", 0
  jmp monitorReEntry
  .else
  jmp throwToys
  .endif
  
typesMatch:
  
  
  iny
  lda #" "
  jsr diagnosticOutChar
  lda (dSCRATCH), Y
  jsr diagnosticOutHex ; reference count in object
  
  ; this relies on dSCRATCH and uCURRENT being the same (memoryHeapWalk uses uCURRENT)
  sec
  lda dSCRATCHL
  sbc #2
  sta dSCRATCHL
  bcs referenceStackSkipMSB
  dec dSCRATCHH
referenceStackSkipMSB:
  
  pla
  
  .ifdef LONGS
  cmp #tLong
  bne emitStackNotLong
  jsr memoryHeapWalkLong
  jmp referenceStackDone
emitStackNotLong:
  .endif
  
  .ifdef STRINGS
  cmp #tString
  bne emitStackNotString
  jsr memoryHeapWalkString
  jmp referenceStackDone
emitStackNotString:
  .endif

  .ifdef DICTIONARIES
  cmp #tDictionary
  bne emitStackNotDictionary
  jsr memoryHeapWalkDictionary
  jmp referenceStackDone
emitStackNotDictionary:

  cmp #tPair
  bne emitStackNotPair
  jsr memoryHeapWalkPair
  jmp referenceStackDone
emitStackNotPair:
  .endif
  
  .ifdef LISTS
  cmp #tList
  bne emitStackNotList
  jsr memoryHeapWalkList
  jmp referenceStackDone
emitStackNotList:
  .endif
  
  .ifdef HEAP
referenceStackDone:
  .endif
  
simpleTypeStack:
  
  
  ; compare IDX to SP
  .ifdef STACK8
  
  lda SP8
  and #$01
  bne diagnosticOutStackExit ; don't try continuing to dump the stack if SP is odd
  
  lda IDXL
  cmp SP8
  beq diagnosticOutStackDone
  
  .else
  
  lda SPL
  and #$01
  bne diagnosticOutStackExit ; don't try continuing to dump the stack if SP is odd
  
  lda IDXH
  cmp SPH
  bne nextStackItemJump
  lda IDXL
  cmp SPL
  bne nextStackItemJump
  bra diagnosticOutStackDone
  
nextStackItemJump:
  .endif

  ; trying not to run off the rails (should also test HopperValueStack + 0x100)
  lda IDXH
  cmp #>HopperValueStack
  bne diagnosticOutStackDone
  
  jmp nextStackItem
  
diagnosticOutStackDone:
  jsr diagnosticOutNewLine
  lda #" "
  jsr diagnosticOutChar
  lda #"S"
  jsr diagnosticOutChar
  lda #"P"
  jsr diagnosticOutChar
  lda #" "
  jsr diagnosticOutChar
  .ifdef STACK8
  lda #>HopperValueStack
  jsr diagnosticOutHex
  lda SP8
  jsr diagnosticOutHex
  .else
  lda SPH
  jsr diagnosticOutHex
  lda SPL
  jsr diagnosticOutHex
  .endif
  
diagnosticOutStackExit:
  pla
  sta IDYH
  pla
  sta IDYL
  pla
  sta IDXH
  pla
  sta IDXL
  pla
  sta dSCRATCHH
  pla
  sta dSCRATCHL
  ply
  plx
  pla
  rts

  
  .ifdef PROFILE
incHopperOpProfile:
  pha
  
  lda #>HopperOpProfile
  sta IDXH
  lda #<HopperOpProfile
  sta IDXL
  
  pla
  pha
  
  ; add opCode to index
  clc
  adc IDXL
  sta IDXL
  bcc incHopperOpProfile0
  inc IDXH
incHopperOpProfile0:
  
  pla
  pha
  
  ; add opCode to index again
  clc
  adc IDXL
  sta IDXL
  bcc incHopperOpProfile1
  inc IDXH
incHopperOpProfile1:

  lda (IDX)
  inc
  sta (IDX)
  bne incHopperOpProfile2
  ldy #1
  lda (IDX), Y
  inc
  sta (IDX), Y
incHopperOpProfile2:
  
  pla
  rts
  
incHopperSysProfile:
  pha
  
  lda #>HopperSysProfile
  sta IDXH
  lda #<HopperSysProfile
  sta IDXL
  
  pla
  pha
  
  ; add sysCall to index
  clc
  adc IDXL
  sta IDXL
  bcc incHopperSysProfile0
  inc IDXH
incHopperSysProfile0:
  
  pla
  pha
  
  ; add sysCall to index again
  clc
  adc IDXL
  sta IDXL
  bcc incHopperSysProfile1
  inc IDXH
incHopperSysProfile1:

  lda (IDX)
  inc
  sta (IDX)
  bne incHopperSysProfile2
  ldy #1
  lda (IDX), Y
  inc
  sta (IDX), Y
incHopperSysProfile2:
  
  pla
  rts
  .endif ; PROFILE
  
  .ifdef CHECKED  

assertByte:
  cmp #tByte
  beq assertByteOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?byte", 0
  .endif
  jmp throwToys
assertByteOk:
  rts

assertDelegate:
  cmp #tDelegate
  beq assertDelegateOk
  
  .ifdef PERMISSIVE
  cmp #tUInt ; also ok under PERMISSIVE
  beq assertDelegateOk
  .endif
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?delegate", 0
  .endif
  jmp throwToys
assertDelegateOk:
  rts


assertUIntOrPlusInt
  ; assumes type is in A and MSB for tInt is in X
  cmp #tUInt
  beq assertUIntOrPlusIntOk
  cmp #tByte
  beq assertUIntOrPlusIntOk
  cmp #tChar
  beq assertUIntOrPlusIntOk
  cmp #tBool
  beq assertUIntOrPlusIntOk
  cmp #tInt
  bne assertUIntOrPlusIntNotOk
  pha
  txa
  bmi assertUIntOrPlusIntNotOk
  pla
  bra assertUIntOrPlusIntOk
assertUIntOrPlusIntNotOk:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?uint or +int", 0
  .endif
  jmp throwToys
assertUIntOrPlusIntOk:
  rts

assertUInt:
  cmp #tUInt
  beq assertUIntOk
  cmp #tByte
  beq assertUIntOk
  cmp #tChar
  beq assertUIntOk
  cmp #tBool
  beq assertUIntOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?uint", 0
  .endif
  jmp throwToys
assertUIntOk:
  rts

assertUIntOrInt:
  cmp #tUInt
  beq assertUIntOrIntOk
  cmp #tByte
  beq assertUIntOrIntOk
  cmp #tChar
  beq assertUIntOrIntOk
  cmp #tBool
  beq assertUIntOrIntOk
  cmp #tInt
  beq assertUIntOrIntOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?uint or int", 0
  .endif
  jmp throwToys
assertUIntOrIntOk:
  rts



assertInt:
  cmp #tInt
  beq assertIntOk
  cmp #tByte
  beq assertIntOk
  cmp #tChar
  beq assertIntOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?int", 0
  .endif
  jmp throwToys
assertIntOk:
  rts
  
verifyBoolStackTop:
  ; verify the bool type and value that we pushed to the top of the stack
  pha
  phx
  
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta fVALUEH
  dex
  lda HopperValueStack, X
  sta fVALUEL
  lda HopperTypeStack, X
  sta fTYPE
  
  .else
  lda IDXL
  pha
  lda IDXH
  pha
  
  lda SPL
  sta IDXL
  lda SPH
  sta IDXH
  jsr decIDX
  lda (IDX)
  sta fVALUEH
  jsr decIDX
  lda (IDX)
  sta fVALUEL
  
  lda TSPL
  sta IDXL
  lda TSPH
  sta IDXH
  jsr decIDX
  lda (IDX)
  sta fTYPE
  
  pla
  sta IDXH
  pla
  sta IDXL
  .endif
  
  plx
  pla
  ; fall through
  
verifyBool:
  ; assumes the type is in fTYPE and the value is in fVALUEH and fVALUEL
  ; verifies that the type is tBool and the value is 0 or 1
  pha
  lda fTYPE
  cmp #tBool
  bne verifyBoolNot
  lda #0
  cmp fVALUEH
  bne verifyBoolNot
  cmp fVALUEL
  beq verifyBoolOk
  lda #1
  cmp fVALUEL
  beq verifyBoolOk
  
verifyBoolNot:
  .ifndef NODIAGNOSTICS
  lda fTYPE
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda fVALUEH
  jsr diagnosticOutHex
  lda fVALUEL
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?bool", 0
  .endif
  jmp throwToys

verifyBoolOk:
  pla
  rts
  
verifyZeroOrOne:
  ; verify that the value in A is 0 or 1
  cmp #0
  beq verifyZeroOrOneOk
  cmp #1
  beq verifyZeroOrOneOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?bool L != 0|1", 0
  .endif
  jmp throwToys
  
verifyZeroOrOneOk:
  rts

verifyZero:
  ; verify that the value in A is 0
  cmp #0
  beq verifyZeroOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?bool H != 0", 0
  .endif
  jmp throwToys
  
verifyZeroOk:
  rts
  

assertReference:
  cmp #tReference
  beq assertReferenceOk
  cmp #tUInt
  beq assertReferenceOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?Ref", 0
  .endif
  jmp throwToys
assertReferenceOk:
  rts

  .ifdef LISTS
assertList:
  cmp #tList
  beq assertListOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?List", 0
  .endif
  jmp throwToys
assertListOk:
  rts
  .endif

  .ifdef DICTIONARIES
assertDictionary:
  cmp #tDictionary
  beq assertDictionaryOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?Dictionary", 0
  .endif
  jmp throwToys
assertDictionaryOk:
  rts
  
  assertPair:
  cmp #tPair
  beq assertPairOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Pair", 0
  .endif
  jmp throwToys
assertPairOk:
  rts
  .endif
  
  .ifdef LONGS
assertLong:
  cmp #tLong
  beq assertLongOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?long", 0
  .endif
  jmp throwToys
assertLongOk:
  rts
  .endif

  .ifdef STRINGS
assertString:
  cmp #tString
  beq assertStringOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?string", 0
  .endif
  jmp throwToys
assertStringOk:
  rts
  .endif

  .ifdef ARRAYS
assertArray:
  cmp #tArray
  beq assertArrayOk
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?array", 0
  .endif
  jmp throwToys
assertArrayOk:
  rts
  .endif
  
  .endif ; CHECKED



  ; used by "M" command (which is used by "Z" command and "U" command)
diagnosticPageMemory:
  ; page # is in A
  sta IDXH
  stz IDXL
  
  ldy #0
  
nextDump:

  tya
  and #$0F
  bne noDumpBreak
  
  jsr diagnosticOutNewLine
  lda IDXH
  jsr diagnosticOutHex
  clc
  tya
  adc IDXL
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
noDumpBreak:
  and #$07
  bne noDumpBreak2
  
  lda #" "
  jsr diagnosticOutChar
  
noDumpBreak2

  lda (IDX), Y
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  iny
  
  cpy #$00
  bne nextDump
  jsr diagnosticOutNewLine
  
  rts
  
; used by "F" command
diagnosticFastPageMemory:
  ; page # is in A
  sta IDXH
  stz IDXL
  
  ldy #0
nextDumpF:

  tya
  and #$0F
  bne noDumpBreakF
  
  jsr diagnosticOutNewLine
  
noDumpBreakF:

  lda (IDX), Y
  jsr diagnosticOutHex
  iny
  
  cpy #$00 ; after $FF
  bne nextDumpF
  
  jsr diagnosticOutNewLine
  
  rts
  


