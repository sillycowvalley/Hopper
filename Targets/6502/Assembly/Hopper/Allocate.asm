
maBEST  = M0
maBESTL = M0
maBESTH = M1

maBESTSIZE = M2
maBESTSIZEL = M2
maBESTSIZEH = M3

maBESTNEXT = M4
maBESTNEXTL = M4
maBESTNEXTH = M5

maBESTPREV = M6
maBESTPREVL = M6
maBESTPREVH = M7

maSCRATCH = M8
maSCRATCHL = M8
maSCRATCHH = M9

maNEWHOLE = M10
maNEWHOLEL = M10
maNEWHOLEH = M11

maNEWHOLESIZE = M12
maNEWHOLESIZEL = M12
maNEWHOLESIZEH = M13

memoryAllocate:
  ; size is in ACC
  ; return address in IDX
  
  .ifdef MEMDEBUG
  jsr diagnosticOutNewLine
  lda #"a"
  jsr diagnosticOutChar
  .endif
  
  ;lda #"a"
  ;jsr diagnosticOutChar
  ;lda ACCH
  ;beq memoryAllocateMSBZero
  ;jsr diagnosticOutHex
;memoryAllocateMSBZero:  
  ;lda ACCL
  ;jsr diagnosticOutHex
  
  ;stz IDXL
  ;stz IDXH
  
  .ifdef CHECKED
  cmp ACCL
  bne memoryAllocateNonZero
  cmp ACCH
  bne memoryAllocateNonZero
  ; this is an error (asking for a zero size block)
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?memoryAllocateZero", 0
  .endif
  jmp throwToys
  
memoryAllocateNonZero:
  .endif
  
  .ifdef MEMDEBUG
  lda #"b"
  jsr diagnosticOutChar
  .endif
  

  lda FREELISTL
  sta IDYL
  lda FREELISTH
  sta IDYH
  
  ; size += 2 (space for 'size')
  jsr incACC
  jsr incACC
  
  
  stz maBESTL
  stz maBESTH
  stz maBESTSIZEL
  stz maBESTSIZEH
  stz maBESTNEXTL
  stz maBESTNEXTH
  stz maBESTPREVL
  stz maBESTPREVH
  
  cmp ACCH
  bne availableBlockSearch ; size > 255
  lda #6
  cmp ACCL
  bcc availableBlockSearch ; size >= 6
  ; minimum size for participation in free list
  sta ACCL
  
  .ifdef MEMDEBUG
  lda #"c"
  jsr diagnosticOutChar
  .endif
  
availableBlockSearch:

  .ifdef MEMDEBUG
  lda #"d"
  jsr diagnosticOutChar
  .endif

  lda #0
  cmp IDYL
  bne availableBlockSearch0
  cmp IDYH
  bne availableBlockSearch0
  jmp availableBlockSearchDone

availableBlockSearch0:
  ; read current FreeList
  
  .ifdef MEMDEBUG
  lda #"e"
  jsr diagnosticOutChar
  .endif
  
  ; (IDY) < ACC
  ldy #1
  lda (IDY), Y
  cmp ACCH
  bne doneIDXLTACC
  lda (IDY)
  cmp ACCL
doneIDXLTACC:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc availableBlockSearch2 ; (IDY) < ACC
  ; (IDY) >= ACC
  
  .ifdef MEMDEBUG
  lda #"f"
  jsr diagnosticOutChar
  .endif
  
  ; (0 == maBESTSIZE) ? 
  lda #0
  cmp maBESTSIZEL
  bne availableBlockSearch4
  cmp maBESTSIZEH
  beq availableBlockSearch3
  
availableBlockSearch4:

  .ifdef MEMDEBUG
  lda #"g"
  jsr diagnosticOutChar
  .endif

  ; (IDY) < maBESTSIZE ?
  ldy #1
  lda (IDY), Y
  cmp maBESTSIZEH
  bne doneIDYLTmaBESTSIZE
  lda (IDY)
  cmp maBESTSIZEL
doneIDYLTmaBESTSIZE:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc availableBlockSearch3 ; (IDY) < maBESTSIZE
  bra availableBlockSearch2
  
availableBlockSearch3:	

  .ifdef MEMDEBUG
  lda #"h"
  jsr diagnosticOutChar
  .endif

  ; first available block
  ;  or
  ; better than what we've seen so far in terms of fit   
  lda IDYL
  sta maBESTL
  lda IDYH
  sta maBESTH

  ; bestSize = ReadWord(best);
  ldy #0
  lda (maBEST), Y
  sta maBESTSIZEL
  iny
  lda (maBEST), Y
  sta maBESTSIZEH
  ; bestNext = ReadWord(best + 2);
  iny
  lda (maBEST), Y
  sta maBESTNEXTL
  iny
  lda (maBEST), Y
  sta maBESTNEXTH
  ; bestPrev = ReadWord(best + 4);
  iny
  lda (maBEST), Y
  sta maBESTPREVL
  iny
  lda (maBEST), Y
  sta maBESTPREVH

availableBlockSearch2

  .ifdef MEMDEBUG
  lda #"i"
  jsr diagnosticOutChar
  .endif

  lda maBESTSIZEL
  cmp ACCL
  bne availableBlockSearch1
  lda maBESTSIZEH
  cmp ACCH
  bne availableBlockSearch1
	
  ; maBESTSIZE == ACC
  ; can't get better than that
  jmp availableBlockSearchDone

availableBlockSearch1:

  .ifdef MEMDEBUG
  lda #"j"
  jsr diagnosticOutChar
  .endif

  ; IDY = ReadWord(IDY + 2);
  ldy #2
  lda (IDY), Y
  pha
  iny
  lda (IDY), Y
  sta IDYH
  pla
  sta IDYL
  jmp availableBlockSearch
  
availableBlockSearchDone:

  .ifdef MEMDEBUG
  lda #"k"
  jsr diagnosticOutChar
  .endif
  
  ; address = best + 2
  lda maBESTL
  sta IDXL
  lda maBESTH
  sta IDXH
  ; IDX += 2
  jsr incIDX
  jsr incIDX
  
  .ifdef MEMDEBUG
  lda maBESTH
  jsr diagnosticOutHex
  lda maBESTL
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda maBESTSIZEH
  jsr diagnosticOutHex
  lda maBESTSIZEL
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda maBESTNEXTH
  jsr diagnosticOutHex
  lda maBESTNEXTL
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda maBESTPREVH
  jsr diagnosticOutHex
  lda maBESTPREVL
  jsr diagnosticOutHex
  .endif
  
  ; maSCRATCH = size+6
  clc
  lda ACCL
  adc #6
  sta maSCRATCHL
  lda ACCH
  adc #0
  sta maSCRATCHH
  
  ; maBESTSIZE < maSCRATCH?
  lda maBESTSIZEH
  cmp maSCRATCHH
  bne doneLTCantSplit
  lda maBESTSIZEH
  cmp maSCRATCHL
doneLTCantSplit:
  ; http://6502.org/tutorials/compare_instructions.html
  bcs memoryAllocateCanSplit  ; maBESTSIZE >= maSCRATCH
  jmp memoryAllocateCantSplit ; maBESTSIZE < maSCRATCH
memoryAllocateCanSplit

  .ifdef MEMDEBUG
  lda #"l"
  jsr diagnosticOutChar
  .endif

  ; (bestSize >= size + 6)

  ; so we now how much to free later
  ; block size includes the size of the size field itself
  ldy #1
  lda ACCL
  sta (maBEST)
  lda ACCH
  sta (maBEST), Y
		
  ; enough extra to make a new freelist record from the balance
  clc
  lda maBESTL
  adc ACCL
  sta maNEWHOLEL
  lda maBESTH
  adc ACCH
  sta maNEWHOLEH
  
  sec
  lda maBESTSIZEL
  sbc ACCL
  sta maNEWHOLESIZEL
  lda maBESTSIZEH
  sbc ACCH
  sta maNEWHOLESIZEH
  
  .ifdef MEMDEBUG
  lda #"H"
  jsr diagnosticOutChar
  lda maNEWHOLEH
  jsr diagnosticOutHex
  lda maNEWHOLEL
  jsr diagnosticOutHex
  lda #"-"
  jsr diagnosticOutChar
  lda maNEWHOLESIZEH
  jsr diagnosticOutHex
  lda maNEWHOLESIZEL
  jsr diagnosticOutHex
  
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda maBESTH
  ;jsr diagnosticOutHex
  ;lda maBESTL
  ;jsr diagnosticOutHex
  ;lda #"-"
  ;jsr diagnosticOutChar
  ;ldy #1
  ;lda (maBEST), Y
  ;jsr diagnosticOutHex
  ;lda (maBEST)
  ;jsr diagnosticOutHex
  ;lda #"-"
  ;jsr diagnosticOutChar
  ;ldy #3
  ;lda (maBEST), Y
  ;jsr diagnosticOutHex
  ;dey
  ;lda (maBEST), Y
  ;jsr diagnosticOutHex
  ;lda #"-"
  ;jsr diagnosticOutChar
  ;ldy #5
  ;lda (maBEST), Y
  ;jsr diagnosticOutHex
  ;dey
  ;lda (maBEST), Y
  ;jsr diagnosticOutHex
  .endif
	
  ldy #1
  lda maNEWHOLESIZEL
  sta (maNEWHOLE)
  lda maNEWHOLESIZEH
  sta (maNEWHOLE), Y
  
  lda #0
  cmp maBESTPREVL
  bne bestPrevNotZero0
  cmp maBESTPREVH
  bne bestPrevNotZero0
  
  .ifdef MEMDEBUG
  lda #"."
  jsr diagnosticOutChar
  .endif
  
  ; 0 == bestPrev
  lda maNEWHOLEL
  sta FREELISTL
  lda maNEWHOLEH
  sta FREELISTH
  iny
  lda maBESTNEXTL
  sta (maNEWHOLE), Y
  iny
  lda maBESTNEXTH
  sta (maNEWHOLE), Y
  iny
  lda #0
  sta (maNEWHOLE), Y
  iny
  sta (maNEWHOLE), Y
  
  cmp maBESTNEXTL
  bne bestNextNotZero
  cmp maBESTNEXTH
  bne bestNextNotZero
  jmp memoryAllocateExit
bestNextNotZero:

  .ifdef MEMDEBUG
  lda #"m"
  jsr diagnosticOutChar
  .endif

  ldy #4
  lda maNEWHOLEL
  sta (maBESTNEXT), Y
  iny
  lda maNEWHOLEH
  sta (maBESTNEXT), Y
  jmp memoryAllocateExit
		
bestPrevNotZero0:

  .ifdef MEMDEBUG
  lda #"n"
  jsr diagnosticOutChar
  .endif

  ldy #2
  lda maBESTNEXTL
  sta (maNEWHOLE), Y
  iny
  lda maBESTNEXTH
  sta (maNEWHOLE), Y
  iny
  lda maBESTPREVL
  sta (maNEWHOLE), Y
  iny
  lda maBESTPREVH
  sta (maNEWHOLE), Y
  ldy #2	
  lda maNEWHOLEL
  sta (maBESTPREV), Y
  iny
  lda maNEWHOLEH
  sta (maBESTPREV), Y
  lda #0
  cmp maBESTNEXTL
  bne bestNextNotZero1
  cmp maBESTNEXTH
  bne bestNextNotZero1
  jmp memoryAllocateExit
bestNextNotZero1:

  .ifdef MEMDEBUG
  lda #"o"
  jsr diagnosticOutChar
  .endif

  ldy #4
  lda maNEWHOLEL
  sta (maBESTNEXT), Y
  iny
  lda maNEWHOLEH
  sta (maBESTNEXT), Y
  jmp memoryAllocateExit

memoryAllocateCantSplit:

  .ifdef MEMDEBUG
  lda #"p"
  jsr diagnosticOutChar
  .endif

  ; maBESTSIZE < ACC?
  lda maBESTSIZEH
  cmp ACCH
  bne doneLTFailed
  lda maBESTSIZEL
  cmp ACCL
doneLTFailed:
  bcs bestSizeGESize 
  jmp memoryAllocateFailed ; maBESTSIZE < ACC
  
bestSizeGESize:

  .ifdef MEMDEBUG
  lda #"q"
  jsr diagnosticOutChar
  .endif

  ; (bestSize >= size)
  
  ; just link the freelist past the new hole
  ; and give allocate the entire slot (more than was asked)
	   
  ; so we now how much to free later
  ; block size includes the size of the size field itself
  ; WriteWord(best, bestSize);
  ldy #1
  lda maBESTSIZEL
  sta (maBEST)
  lda maBESTSIZEH
  sta (maBEST), Y
  
  lda #0
  cmp maBESTPREVL
  bne bestPrevNotZero1
  cmp maBESTPREVH
  bne bestPrevNotZero1
	
  ; 0 == bestPrev
		
  ; best was the old FreeList
  lda maBESTNEXTL
  sta FREELISTL
  lda maBESTNEXTH
  sta FREELISTH
		
  lda #0
  cmp maBESTNEXTL
  bne bestNextNotZero0
  cmp maBESTNEXTH
  bne bestNextNotZero0
  jmp memoryAllocateExit
bestNextNotZero0:

  .ifdef MEMDEBUG
  lda #"r"
  jsr diagnosticOutChar
  .endif

  ; WriteWord(freeList+4, 0); // start of list now so no previous
  lda #0
  ldy #4
  sta (FREELIST), Y
  iny
  sta (FREELIST), Y
  jmp memoryAllocateExit
bestPrevNotZero1:
   
  .ifdef MEMDEBUG
  lda #"s"
  jsr diagnosticOutChar
  .endif

  ; 0 != bestPrev
  
  ;WriteWord(bestPrev+2, bestNext);
  ldy #2
  lda maBESTNEXTL
  sta (maBESTPREV), Y
  iny
  lda maBESTNEXTH
  sta (maBESTPREV), Y
		
  lda #0
  cmp maBESTNEXTL
  bne bestNextNotZero2
  cmp maBESTNEXTH
  bne bestNextNotZero2
  jmp memoryAllocateExit
bestNextNotZero2:		
  
  .ifdef MEMDEBUG
  lda #"t"
  jsr diagnosticOutChar
  .endif

  ; WriteWord(bestNext+4, bestPrev);
  ldy #4
  lda maBESTPREVL
  sta (maBESTNEXT), Y
  iny
  lda maBESTPREVH
  sta (maBESTNEXT), Y
		
  jmp memoryAllocateExit

memoryAllocateFailed:

  .ifdef MEMDEBUG
  lda #"u"
  jsr diagnosticOutChar
  .endif
  
  ; failed to allocate
  .ifndef NODIAGNOSTICS  
  lda ACCH
  jsr diagnosticOutHex
  lda ACCL
  jsr diagnosticOutHex
  
  jsr diagnosticOutString
  .byte $0D, "?Alloc!", 0
  .endif
  jmp throwToysNoStack

memoryAllocateExit:
  
  ; address in IDX
  
  .ifdef MEMDEBUG
  lda #"v"
  jsr diagnosticOutChar
  lda #"A"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  .endif
  
  ; zero initialize
  
  ; size -= 2 (space for 'size')
  jsr decACC
  jsr decACC
  
  clc
  lda IDXL
  adc ACCL
  sta maSCRATCHL
  lda IDXH
  adc ACCH
  sta maSCRATCHH
  
memoryAllocateClearNext:
  lda maSCRATCHH
  cmp IDXH
  bne memoryAllocateClearContinue
  lda maSCRATCHL
  cmp IDXL
  bne memoryAllocateClearContinue
  
  .ifdef MEMDEBUG
  lda #"w"
  jsr diagnosticOutChar
  .endif
  
  rts ; done
memoryAllocateClearContinue:
  ; maSCRATCH--  
  lda maSCRATCHL
  bne decmaSCRATCHSkip
  dec maSCRATCHH
decmaSCRATCHSkip:
  dec maSCRATCHL
  
  ; clear
  lda #0
  sta (maSCRATCH)
  
  bra memoryAllocateClearNext
