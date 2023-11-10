
  .include Allocate.asm
  .include Free.asm
  .include HeapWalk.asm
  
memoryInit:
  ; Zero initialize
  
  stz IDXL
  lda HEAPSTART
  sta IDXH
  ldx HEAPSIZE ; number of 256 byte pages is same as MSB of size
  jsr clearPages
  
  ; FreeList = Hopper heap start
  lda HEAPSTART
  sta FREELISTH
  stz FREELISTL
  
  ; all memory is in this single free list record
  lda #0
  sta (FREELIST)
  lda HEAPSIZE
  ldy #1
  sta (FREELIST), Y
  
  ; next = null
  lda #0
  iny
  sta (FREELIST), Y
  iny
  sta (FREELIST), Y
  
  ; prev = null
  iny
  sta (FREELIST), Y
  iny
  sta (FREELIST), Y
  
  rts
  
memoryMaximum:
  ; uses IDX and IDY
  ; returns result in ACC
  stz ACCL
  stz ACCH
  lda FREELISTL
  sta IDXL
  lda FREELISTH
  sta IDXH

memoryMaximumLoop:
  lda #0
  cmp IDXL
  bne memoryMaximumNext
  cmp IDXH
  bne memoryMaximumNext
  
  cmp ACCL
  bne memoryMaximumDecACC
  cmp ACCH
  beq memoryMaximumDone
memoryMaximumDecACC
  jsr decACC
  jsr decACC
memoryMaximumDone
  rts
memoryMaximumNext:
  ldy #1
  lda (IDX)
  sta IDYL
  lda (IDX), Y
  sta IDYH
  
  ldx #0 ; IDY <= ACC
  lda IDYH
  cmp ACCH
  bne memoryMaximumGT
  lda IDYL
  cmp ACCL
memoryMaximumGT:
  ; http://6502.org/tutorials/compare_instructions.html
  beq memoryMaximumGTDone ; IDY == ACC (not >)
  bcc memoryMaximumGTDone ; IDY <  ACC (not >)
  ; IDY > ACC
  ; so ACC = IDY;
  lda IDYL
  sta ACCL
  lda IDYH
  sta ACCH
memoryMaximumGTDone:
  iny
  lda (IDX), Y
  pha
  iny
  lda (IDX), Y
  sta IDXH
  pla
  sta IDXL
  bra memoryMaximumLoop


memoryAvailable:
  ; uses IDX
  ; returns result in ACC
  stz ACCL
  stz ACCH
  lda FREELISTL
  sta IDXL
  lda FREELISTH
  sta IDXH
        
memoryAvailableLoop:
  lda #0
  cmp IDXL
  bne memoryAvailableNext
  cmp IDXH
  bne memoryAvailableNext
  
  ;jsr memoryHeapWalk
  
  rts
memoryAvailableNext:

  ldy #1
  clc
  lda (IDX)
  adc ACCL
  sta ACCL
  lda (IDX), Y
  adc ACCH
  sta ACCH
  
  jsr decACC
  jsr decACC
  
  iny
  lda (IDX), Y
  pha
  iny
  lda (IDX), Y
  sta IDXH
  pla
  sta IDXL
  
  bra memoryAvailableLoop
