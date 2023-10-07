
uCURRENT = U0
uCURRENTL = U0
uCURRENTH = U1


uCURRENTSIZE = U2
uCURRENTSIZEL = U2
uCURRENTSIZEH = U3

uCURRENTNEXT = U4
uCURRENTNEXTL = U4
uCURRENTNEXTH = U5

uCURRENTPREV = U6
uCURRENTPREVL = U6
uCURRENTPREVH = U7

uFREESLOT = U8
uFREESLOTL = U8
uFREESLOTH = U9

uLASTCURRENT = U10
uLASTCURRENTL = U10
uLASTCURRENTH = U11
   
checkHeapRange:
  ; TODO : improve this to use HEAPSTART and HEAPSIZE
  rts
  and #$F0
  cmp #$40
  beq checkHeapRangeOk
  cmp #$50
  beq checkHeapRangeOk
  cmp #$60
  beq checkHeapRangeOk
  cmp #$70
  beq checkHeapRangeOk
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?checkHeapRange", 0
  .endif
  jmp throwToys
checkHeapRangeOk  
  rts
  
isCurrentFree:
  lda FREELISTL
  sta uFREESLOTL
  lda FREELISTH
  sta uFREESLOTH

lookForNextFree:
  lda uCURRENTH
  cmp uFREESLOTH
  bne notThisSlot
  lda uCURRENTL
  cmp uFREESLOTL
  bne notThisSlot
  ; found a match
  sec
  rts ; C is set
notThisSlot:
  ldy #2
  lda (uFREESLOT), Y
  pha
  iny
  lda (uFREESLOT), Y
  sta uFREESLOTH
  pla
  sta uFREESLOTL
  ;cmp #0
  bne lookForNextFree
  lda uFREESLOTH
  ;cmp #0
  bne lookForNextFree
  clc ; C is clear
  rts

memoryHeapWalk:
  phy
  phx
  pha
  
  jsr diagnosticOutNewLine
  ;pla
  ;jsr diagnosticOutChar
  ;pha
    
  lda FREELISTL
  sta uCURRENTL
  lda FREELISTH
  sta uCURRENTH
  
  stz uLASTCURRENTL
  stz uLASTCURRENTH
  
walkMore:

  ldy #0
  lda (uCURRENT), Y
  sta uCURRENTSIZEL
  iny
  lda (uCURRENT), Y
  sta uCURRENTSIZEH
  iny
  lda (uCURRENT), Y
  sta uCURRENTNEXTL
  iny
  lda (uCURRENT), Y
  sta uCURRENTNEXTH
  iny
  lda (uCURRENT), Y
  sta uCURRENTPREVL
  iny
  lda (uCURRENT), Y
  sta uCURRENTPREVH
  
  lda uCURRENTH
  jsr diagnosticOutHex
  lda uCURRENTL
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda uCURRENTSIZEH
  jsr diagnosticOutHex
  lda uCURRENTSIZEL
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda uCURRENTNEXTH
  jsr diagnosticOutHex
  lda uCURRENTNEXTL
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda uCURRENTPREVH
  jsr diagnosticOutHex
  lda uCURRENTPREVL
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  
  lda uLASTCURRENTH
  cmp uCURRENTPREVH
  bne heapPreviousError
  lda uLASTCURRENTL
  cmp uCURRENTPREVL
  bne heapPreviousError
  jmp heapPreviousOk  
heapPreviousError:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?heapPreviousError", 0
  .endif
  jmp throwToys
  
heapPreviousOk:  
  
  lda uCURRENTH
  jsr checkHeapRange
  
  lda uCURRENTL
  sta uLASTCURRENTL
  lda uCURRENTH
  sta uLASTCURRENTH
  
  lda uCURRENTNEXTL
  bne nextRecord
  lda uCURRENTNEXTH
  bne nextRecord
  jmp doneWalking
nextRecord:
  lda uCURRENTNEXTL
  sta uCURRENTL
  lda uCURRENTNEXTH
  sta uCURRENTH
  jmp walkMore
doneWalking:  

  lda HEAPSTART
  sta uCURRENTH
  stz uCURRENTL
  
walkMore2:

  ldy #0
  lda (uCURRENT), Y
  sta uCURRENTSIZEL
  iny
  lda (uCURRENT), Y
  sta uCURRENTSIZEH
  
  clc
  lda uCURRENTL
  adc uCURRENTSIZEL
  sta uCURRENTNEXTL
  lda uCURRENTH
  adc uCURRENTSIZEH
  sta uCURRENTNEXTH
  
  jsr isCurrentFree ; C is set if on free list
  bcc notFreeBlock
  jmp skipFreeBlock
notFreeBlock:
  jsr diagnosticOutNewLine
  lda #" "
  jsr diagnosticOutChar
  jsr diagnosticOutChar
  lda uCURRENTH
  jsr diagnosticOutHex
  lda uCURRENTL
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda uCURRENTSIZEH
  jsr diagnosticOutHex
  lda uCURRENTSIZEL
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  
  ldy #2
  lda (uCURRENT), Y
  pha
  jsr diagnosticOutHex
  iny
  lda #":"
  jsr diagnosticOutChar
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  pla
  
  .ifdef LONGS
  cmp #tLong
  bne notMemoryHeapWalkLong
  jsr memoryHeapWalkLong
  jmp skipFreeBlock
notMemoryHeapWalkLong:
  .endif
  .ifdef STRINGS
  cmp #tString
  bne notMemoryHeapWalkString
  jsr memoryHeapWalkString
  jmp skipFreeBlock
notMemoryHeapWalkString
  .endif

  .ifdef ARRAYS
  cmp #tArray
  bne notMemoryHeapWalkArray
  jsr memoryHeapWalkArray
  jmp skipFreeBlock
notMemoryHeapWalkArray:
  .endif
 
  .ifdef LISTS
  cmp #tListItem
  bne notMemoryHeapWalkListItem
  jmp memoryHeapWalkListItem
notMemoryHeapWalkListItem:
  .endif
  
  cmp #tVariant
  bne notMemoryHeapWalkVariant
  jsr memoryHeapWalkVariant
  jmp skipFreeBlock
notMemoryHeapWalkVariant:

  .ifdef LISTS
  cmp #tList
  bne notMemoryHeapWalkList
  jsr memoryHeapWalkList
  jmp skipFreeBlock
notMemoryHeapWalkList:
  .endif
  
  .ifdef DICTIONARIES
  cmp #tDictionary
  bne notMemoryHeapWalkDictionary
  jsr memoryHeapWalkDictionary
  jmp skipFreeBlock
notMemoryHeapWalkDictionary:
  cmp #tPair
  bne notMemoryHeapWalkPair
  jsr memoryHeapWalkPair
  jmp skipFreeBlock
notMemoryHeapWalkPair:
  .endif
  
  jmp skipFreeBlock
  
memoryHeapWalkString:
  lda #" "
  jsr diagnosticOutChar
  ldy #4
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  tax ; string length up to 255
  lda #" "
  jsr diagnosticOutChar
  lda #"'"
  jsr diagnosticOutChar
  iny
nextStringCharacter:  
  iny
  cpx #0
  bne nextStringCharacter2
  lda #"'"
  jsr diagnosticOutChar
  rts
nextStringCharacter2:  
  lda (uCURRENT), Y
  jsr diagnosticOutChar
  dex
  bra nextStringCharacter
  
memoryHeapWalkLong:
  lda #" "
  jsr diagnosticOutChar
  ldy #7
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  rts

memoryHeapWalkVariant:
  lda #" "
  jsr diagnosticOutChar
  lda #"T"
  jsr diagnosticOutChar
  ldy #4
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  lda #"D"
  jsr diagnosticOutChar
  lda #">"
  jsr diagnosticOutChar
  ldy #6
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  rts

memoryHeapWalkListItem:
  lda #" "
  jsr diagnosticOutChar
  lda #"D"
  jsr diagnosticOutChar
  lda #">"
  jsr diagnosticOutChar
  ldy #5
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  lda #" "
  jsr diagnosticOutChar
  lda #"N"
  jsr diagnosticOutChar
  lda #">"
  jsr diagnosticOutChar
  ldy #7
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  jmp skipFreeBlock
  
  
memoryHeapWalkArray:
  lda #" "
  jsr diagnosticOutChar
  ldy #6
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda #" "
  jsr diagnosticOutChar
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  rts
  
memoryHeapWalkDictionary:
  lda #" "
  jsr diagnosticOutChar
  ldy #7
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; count of entries
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  lda #"k"
  jsr diagnosticOutChar
  ldy #4
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; kType
  
  lda #" "
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  iny
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; vType
  
  lda #" "
  jsr diagnosticOutChar
  ldy #9
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; capacity
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  ldy #11
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; pEntries
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  rts
  
memoryHeapWalkPair:
  
  lda #" "
  jsr diagnosticOutChar
  lda #"k"
  jsr diagnosticOutChar
  ldy #4
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; kType
  
  lda #" "
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  iny
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; vType
  
  lda #" "
  jsr diagnosticOutChar
  ldy #7
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; pKey
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  ldy #9
  lda (uCURRENT), Y
  jsr diagnosticOutHex ; pData
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  rts
  
memoryHeapWalkList:
  lda #" "
  jsr diagnosticOutChar
  ldy #5
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  ldy #6
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  ldy #8
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  ldy #10
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  lda #" "
  jsr diagnosticOutChar
  ldy #12
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  dey
  lda (uCURRENT), Y
  jsr diagnosticOutHex
  
  rts
  
skipFreeBlock:  
  
  lda uCURRENTH
  jsr checkHeapRange
  
  lda uCURRENTNEXTL
  cmp #$00
  bne nextRecord2
  lda uCURRENTNEXTH
  cmp #$80
  bne nextRecord2
  jmp doneWalking2
nextRecord2:
  lda uCURRENTNEXTL
  cmp uCURRENTL
  bne nextRecord2Legit
  lda uCURRENTNEXTH
  cmp uCURRENTH
  bne nextRecord2Legit
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?nextRecord2", 0
  .endif
  jmp throwToys
  
nextRecord2Legit:  
  lda uCURRENTNEXTL
  sta uCURRENTL
  lda uCURRENTNEXTH
  sta uCURRENTH
  jmp walkMore2
doneWalking2:  
  pla
  plx
  ply
  rts