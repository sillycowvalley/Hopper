mfCURRENT = IDYL
mfCURRENTL = IDYL
mfCURRENTH = IDYH
  
mfPREVIOUS  = M0
mfPREVIOUSL = M0
mfPREVIOUSH = M1

mfCURRENTSIZE = M2
mfCURRENTSIZEL = M2
mfCURRENTSIZEH = M3

mfCURRENTNEXT = M4
mfCURRENTNEXTL = M4
mfCURRENTNEXTH = M5

mfCURRENTPREV = M6
mfCURRENTPREVL = M6
mfCURRENTPREVH = M7

mfFREESLOT = M8
mfFREESLOTL = M8
mfFREESLOTH = M9

; the next 3 GAP variables share the same slot
mfGAPFRONT = M10
mfGAPFRONTL = M10
mfGAPFRONTH = M11
mfGAPBACK = M10
mfGAPBACKL = M10
mfGAPBACKH = M11
mfGAPNEXT = M10
mfGAPNEXTL = M10
mfGAPNEXTH = M11

; same as mfCURRENT which is not used again after mfNEXTNEXT is needed
mfNEXTNEXT = IDYL   
mfNEXTNEXTL = IDYL  
mfNEXTNEXTH = IDYH

; these two size variables share the same slot:
mfPREVSIZE = M12
mfPREVSIZEL = M12
mfPREVSIZEH = M13
mfNEXTSIZE = M12
mfNEXTSIZEL = M12
mfNEXTSIZEH = M13

mfSIZE = M14
mfSIZEL = M14
mfSIZEH = M15

mfOFFSET = M15 ; used in releaseSP, no need to preserve
  
memoryFree:
  ; address is in IDX
  ; uses mfCURRENT
  
  .ifdef CHECKED
  lda #0
  cmp IDXL
  bne addressNotZero
  cmp IDXH
  bne addressNotZero
  
  ; this is a bug (to try to free nullptr
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?memoryFreeNull", 0
  .endif
  jmp throwToys
  
addressNotZero:
  .endif
  
  .ifdef MEMDEBUG
  lda #"a"
  jsr diagnosticOutChar
  .endif

  ; mfSIZE  = ReadWord(IDX - 2)	
  lda IDXL
  sta IDYL
  lda IDXH
  sta IDYH
  
  jsr decIDY
  jsr decIDY
  ldy #1
  lda (IDY)
  sta mfSIZEL
  lda (IDY), Y
  sta mfSIZEH
  
  ; mfCURRENT = FreeList
  lda FREELISTL
  sta mfCURRENTL
  lda FREELISTH
  sta mfCURRENTH

  lda #0
  sta mfPREVIOUSL  
  sta mfPREVIOUSH
  
findBeyondAddress:
  .ifdef MEMDEBUG
  lda #"b"
  jsr diagnosticOutChar
  .endif
	
  lda #0
  cmp mfCURRENTL
  bne currentNotZero
  cmp mfCURRENTH
  beq beyondAddress
currentNotZero:
  ; walk current (mfCURRENT) FreeList till next record beyond address (IDX)
  ldx #0 ; mfCURRENT <= IDX
  lda mfCURRENTH
  cmp IDXH
  bne donemfCURRENTGTIDX
  lda mfCURRENTL
  cmp IDXL
donemfCURRENTGTIDX:
  ; http://6502.org/tutorials/compare_instructions.html
  beq notmfCURRENTGTIDX ; mfCURRENT == IDX (not >)
  bcc notmfCURRENTGTIDX ; mfCURRENT <  IDX (not >)
  bra beyondAddress
notmfCURRENTGTIDX:

  .ifdef MEMDEBUG
  lda #"c"
  jsr diagnosticOutChar
  .endif

  lda mfCURRENTL
  sta mfPREVIOUSL
  lda mfCURRENTH
  sta mfPREVIOUSH
  
  ; currentNext = ReadWord(mfCURRENT + 2);
  ldy #2
  lda (mfCURRENT), Y
  sta mfCURRENTNEXTL
  iny
  lda (mfCURRENT), Y
  sta mfCURRENTNEXTH
  ; mfCURRENT = currentNext;
  sta mfCURRENTH
  lda mfCURRENTNEXTL
  sta mfCURRENTL
  
  bra findBeyondAddress
beyondAddress:

  .ifdef MEMDEBUG
  lda #"d"
  jsr diagnosticOutChar
  .endif

  ; currentPrev = previous;
  lda mfPREVIOUSL
  sta mfCURRENTPREVL
  lda mfPREVIOUSH
  sta mfCURRENTPREVH
  lda #0
  sta mfCURRENTSIZEL
  sta mfCURRENTSIZEH
  sta mfCURRENTNEXTL
  sta mfCURRENTNEXTH
  
  lda #0
  cmp mfCURRENTL
  bne freemfCURRENTNotZero
  cmp mfCURRENTH
  bne freemfCURRENTNotZero
  bra freemfCURRENTZero
freemfCURRENTNotZero:

  .ifdef MEMDEBUG
  lda #"e"
  jsr diagnosticOutChar
  .endif
  
  ; currentSize = ReadWord(mfCURRENT)
  ldy #0
  lda (mfCURRENT), Y
  sta mfCURRENTSIZEL
  iny
  lda (mfCURRENT), Y
  sta mfCURRENTSIZEH
  ;currentNext = ReadWord(mfCURRENT + 2)
  iny
  lda (mfCURRENT), Y
  sta mfCURRENTNEXTL
  iny
  lda (mfCURRENT), Y
  sta mfCURRENTNEXTH
  ; currentPrev = ReadWord(mfCURRENT + 4); // already set above
  
freemfCURRENTZero:
  
  .ifdef MEMDEBUG
  lda #"f"
  jsr diagnosticOutChar
  .endif
  
  ; freeSlot = IDX-2
  lda IDXL
  sta mfFREESLOTL
  lda IDXH
  sta mfFREESLOTH
  
  sec
  lda mfFREESLOTL
  sbc #2
  sta mfFREESLOTL
  lda mfFREESLOTH
  sbc #0
  sta mfFREESLOTH
  
  .ifdef MEMDEBUG
  lda mfCURRENTH
  jsr diagnosticOutHex
  lda mfCURRENTL
  jsr diagnosticOutHex
  lda #","
  jsr diagnosticOutChar
  lda mfCURRENTSIZEH
  jsr diagnosticOutHex
  lda mfCURRENTSIZEL
  jsr diagnosticOutHex
  lda #","
  jsr diagnosticOutChar
  lda mfCURRENTNEXTH
  jsr diagnosticOutHex
  lda mfCURRENTNEXTL
  jsr diagnosticOutHex
  lda #","
  jsr diagnosticOutChar
  lda mfCURRENTPREVH
  jsr diagnosticOutHex
  lda mfCURRENTPREVL
  jsr diagnosticOutHex
  lda #","
  jsr diagnosticOutChar
  lda mfFREESLOTH
  jsr diagnosticOutHex
  lda mfFREESLOTL
  jsr diagnosticOutHex
  .endif
  
  lda #0
  cmp mfCURRENTPREVL
  bne freemfCURRENTPREVNotZero1
  cmp mfCURRENTPREVH
  bne freemfCURRENTPREVNotZero1
  bra freemfCURRENTPREVZero
freemfCURRENTPREVNotZero1:
  jmp freemfCURRENTPREVNotZero

freemfCURRENTPREVZero:

  .ifdef MEMDEBUG
  lda #"g"
  jsr diagnosticOutChar
  .endif
  
  ; current (mfCURRENT) is front of FreeList, insert in front of it
  ; WriteWord(freeSlot+2, mfCURRENT)
  ldy #2
  lda mfCURRENTL
  sta (mfFREESLOT), Y
  iny
  lda mfCURRENTH
  sta (mfFREESLOT), Y
  ; WriteWord(freeSlot+4, 0);
  ldy #4
  lda #0
  sta (mfFREESLOT), Y
  iny
  sta (mfFREESLOT), Y
  ; WriteWord(mfCURRENT+ 4, freeSlot);
  ldy #4
  lda mfFREESLOTL
  sta (mfCURRENT), Y
  iny
  lda mfFREESLOTH
  sta (mfCURRENT), Y
  
  ;	gapFront = freeList - (freeSlot+size);
  clc
  lda mfFREESLOTL
  adc mfSIZEL
  sta mfGAPFRONTL
  lda mfFREESLOTH
  adc mfSIZEH
  sta mfGAPFRONTH
  sec
  lda FREELISTL
  sbc mfGAPFRONTL
  sta mfGAPFRONTL
  lda FREELISTH
  sbc mfGAPFRONTH
  sta mfGAPFRONTH
  
  lda #0
  cmp mfGAPFRONTL
  bne freemfGAPFRONTNotZero
  cmp mfGAPFRONTH
  bne freemfGAPFRONTNotZero
  
  ; GAPFRONT == 0
  .ifdef MEMDEBUG
  lda #"z"
  jsr diagnosticOutChar
  .endif
  
  ; nextSize = ReadWord(freeList)
  ldy #1
  lda (FREELIST)
  sta mfNEXTSIZEL
  lda (FREELIST), Y
  sta mfNEXTSIZEH
  ; nextNext = ReadWord(freeList+2);
  iny
  lda (FREELIST), Y
  sta mfNEXTNEXTL
  iny
  lda (FREELIST), Y
  sta mfNEXTNEXTH
  
  ; no gap between freeSlot and freeList so absorb it into freeSlot block
  clc
  lda mfNEXTSIZEL
  adc mfSIZEL
  sta mfSIZEL
  lda mfNEXTSIZEH
  adc mfSIZEH
  sta mfSIZEH
  
  ; WriteWord(freeSlot, size+nextSize);
  ldy #1
  lda mfSIZEL
  sta (mfFREESLOT)
  lda mfSIZEH
  sta (mfFREESLOT), Y
  
  ; WriteWord(freeSlot+2, nextNext);
  iny
  lda mfNEXTNEXTL
  sta (mfFREESLOT), Y
  iny
  lda mfNEXTNEXTH
  sta (mfFREESLOT), Y
  
  lda #0
  cmp mfNEXTNEXTL
  bne freemfNEXTNEXTNotZero
  cmp mfNEXTNEXTH
  bne freemfNEXTNEXTNotZero
  bra freemfGAPFRONTNotZero
freemfNEXTNEXTNotZero:

  ; mfNEXTNEXT != 0
  .ifdef MEMDEBUG
  lda #"h"
  jsr diagnosticOutChar
  .endif


  ; WriteWord(nextNext+4, freeSlot)
  ldy #4
  lda mfFREESLOTL
  sta (mfNEXTNEXT), Y
  iny
  lda mfFREESLOTH
  sta (mfNEXTNEXT), Y
  
freemfGAPFRONTNotZero:	

  ; GAPFRONT != 0

  lda mfFREESLOTL
  sta FREELISTL
  lda mfFREESLOTH
  sta FREELISTH
  
  jmp memoryFreeExit
freemfCURRENTPREVNotZero:	

  .ifdef MEMDEBUG
  lda #"i"
  jsr diagnosticOutChar
  .endif

  lda #0
  cmp mfCURRENTL
  bne freemfCURRENTNotZero1
  cmp mfCURRENTH
  bne freemfCURRENTNotZero1
  
  .ifdef MEMDEBUG
  lda #"+"
  jsr diagnosticOutChar
  .endif
  
  ; currentPrev != 0 means we are at the end of the FreeList
  ; append to end of freelist (after currentPrev)
  
  ; WriteWord(currentPrev+2, freeSlot);
  ldy #2
  lda mfFREESLOTL
  sta (mfCURRENTPREV), Y
  iny
  lda mfFREESLOTH
  sta (mfCURRENTPREV), Y
  
  ; WriteWord(freeSlot   +4, currentPrev);
  ldy #4
  lda mfCURRENTPREVL
  sta (mfFREESLOT), Y
  iny
  lda mfCURRENTPREVH
  sta (mfFREESLOT), Y
  
  ; WriteWord(freeSlot   +2, 0);
  ldy #2
  lda #0
  sta (mfFREESLOT), Y
  iny
  sta (mfFREESLOT), Y
  
  ; prevSize = ReadWord(currentPrev);
  ldy #1
  lda (mfCURRENTPREV)
  sta mfPREVSIZEL
  lda (mfCURRENTPREV), Y
  sta mfPREVSIZEH
	
  ; gapBack = freeSlot - (currentPrev+prevSize);
  
  clc
  lda mfCURRENTPREVL
  adc mfPREVSIZEL
  sta mfGAPBACKL
  lda mfCURRENTPREVH
  adc mfPREVSIZEH
  sta mfGAPBACKH
  sec
  lda mfFREESLOTL
  sbc mfGAPBACKL
  sta mfGAPBACKL
  lda mfFREESLOTH
  sbc mfGAPBACKH
  sta mfGAPBACKH
  
  lda #0
  cmp mfGAPBACKL
  bne freemfGAPBACKNotZero
  cmp mfGAPBACKH
  bne freemfGAPBACKNotZero
  
  ; GAPBACK == 0
  
  ; no gap between freeSlot and previous so absorb it into previous block
  ; WriteWord(currentPrev, prevSize+size);
  clc
  lda mfSIZEL
  adc mfPREVSIZEL
  sta mfSIZEL
  lda mfSIZEH
  adc mfPREVSIZEH
  sta mfSIZEH
  ldy #1
  lda mfSIZEL
  sta (mfCURRENTPREV)
  lda mfSIZEH
  sta (mfCURRENTPREV), Y
  
  ; WriteWord(currentPrev+2, 0); // nothing beyond freeSlot, tail of FreeList
  lda #0
  iny
  sta (mfCURRENTPREV), Y
  iny
  sta (mfCURRENTPREV), Y

freemfGAPBACKNotZero:

  ; GAPBACK != 0

  .ifdef MEMDEBUG
  lda #"j"
  jsr diagnosticOutChar
  .endif
  
  jmp memoryFreeExit
	
freemfCURRENTNotZero1:

  .ifdef MEMDEBUG
  lda #"k"
  jsr diagnosticOutChar
  .endif
  
  ; insert into freelist before current (mfCURRENT)
  ; WriteWord(currentPrev+2, freeSlot);
  ldy #2
  lda mfFREESLOTL
  sta (mfCURRENTPREV), Y
  iny
  lda mfFREESLOTH
  sta (mfCURRENTPREV), Y
  
  ; WriteWord(freeSlot   +4, currentPrev);
  ldy #4
  lda mfCURRENTPREVL
  sta (mfFREESLOT), Y
  iny
  lda mfCURRENTPREVH
  sta (mfFREESLOT), Y
  
  ; WriteWord(freeSlot   +2, mfCURRENT);
  ldy #2
  lda mfCURRENTL
  sta (mfFREESLOT), Y
  iny
  lda mfCURRENTH
  sta (mfFREESLOT), Y
  
  ; WriteWord(mfCURRENT    +4, freeSlot);
  ldy #4
  lda mfFREESLOTL
  sta (mfCURRENT), Y
  iny
  lda mfFREESLOTH
  sta (mfCURRENT), Y
  
  ; prevSize = ReadWord(currentPrev);
  ldy #1
  lda (mfCURRENTPREV)
  sta mfPREVSIZEL
  lda (mfCURRENTPREV), Y
  sta mfPREVSIZEH
  
  ; gapBack = freeSlot - (currentPrev+prevSize);
  
  clc
  lda mfCURRENTPREVL
  adc mfPREVSIZEL
  sta mfGAPBACKL
  lda mfCURRENTPREVH
  adc mfPREVSIZEH
  sta mfGAPBACKH
  sec
  lda mfFREESLOTL
  sbc mfGAPBACKL
  sta mfGAPBACKL
  lda mfFREESLOTH
  sbc mfGAPBACKH
  sta mfGAPBACKH
  
  lda #0
  cmp mfGAPBACKL
  bne freemfGAPBACKNotZero1
  cmp mfGAPBACKH
  bne freemfGAPBACKNotZero1
  
  ; no gap between freeSlot and previous so absorb it into previous block
  ; WriteWord(currentPrev, prevSize+size);
  
  ; size = prevSize+size;
  clc
  lda mfSIZEL
  adc mfPREVSIZEL
  sta mfSIZEL
  lda mfSIZEH
  adc mfPREVSIZEH
  sta mfSIZEH
  
  ldy #1
  lda mfSIZEL
  sta (mfCURRENTPREV)
  lda mfSIZEH
  sta (mfCURRENTPREV), Y
  
  ; WriteWord(currentPrev+2, mfCURRENT);
  ldy #2
  lda mfCURRENTL
  sta (mfCURRENTPREV), Y
  iny
  lda mfCURRENTH
  sta (mfCURRENTPREV), Y
  
  ; WriteWord(mfCURRENT+4, currentPrev);
  ldy #4
  lda mfCURRENTPREVL
  sta (mfCURRENT), Y
  iny
  lda mfCURRENTPREVH
  sta (mfCURRENT), Y
  
  lda mfCURRENTPREVL
  sta mfFREESLOTL
  lda mfCURRENTPREVH
  sta mfFREESLOTH
  
freemfGAPBACKNotZero1:

  .ifdef MEMDEBUG
  lda #"l"
  jsr diagnosticOutChar
  .endif

  ; gapNext = mfCURRENT - (freeSlot+size);
  clc
  lda mfFREESLOTL
  adc mfSIZEL
  sta mfGAPNEXTL
  lda mfFREESLOTH
  adc mfSIZEH
  sta mfGAPNEXTH
  sec
  lda mfCURRENTL
  sbc mfGAPNEXTL
  sta mfGAPNEXTL
  lda mfCURRENTH
  sbc mfGAPNEXTH
  sta mfGAPNEXTH
  
  lda #0
  cmp mfGAPNEXTL
  bne memoryFreeExit
  cmp mfGAPNEXTH
  bne memoryFreeExit
  
  ; no gap between freeSlot and current (mfCURRENT) so absorb it into freeSlot block
  
  ; WriteWord(freeSlot, size+currentSize);
  clc
  lda mfSIZEL
  adc mfCURRENTSIZEL
  sta mfSIZEL
  lda mfSIZEH
  adc mfCURRENTSIZEH
  sta mfSIZEH
  ldy #1
  lda mfSIZEL
  sta (mfFREESLOT)
  lda mfSIZEH
  sta (mfFREESLOT), Y
  
  ; WriteWord(freeSlot+2, currentNext);
  iny
  lda mfCURRENTNEXTL
  sta (mfFREESLOT), Y
  iny
  lda mfCURRENTNEXTH
  sta (mfFREESLOT), Y
  
  lda #0
  cmp mfCURRENTNEXTL
  bne currentNextNotZero
  cmp mfCURRENTNEXTH
  bne currentNextNotZero
  
  bra memoryFreeExit
currentNextNotZero:

  .ifdef MEMDEBUG
  lda #"m"
  jsr diagnosticOutChar
  .endif

  ; WriteWord(currentNext+4, freeSlot);
  ldy #4
  lda mfFREESLOTL
  sta (mfCURRENTNEXT), Y
  iny
  lda mfFREESLOTH
  sta (mfCURRENTNEXT), Y
  
memoryFreeExit:  

  .ifdef MEMDEBUG
  lda #"n"
  jsr diagnosticOutChar
  .endif

  rts