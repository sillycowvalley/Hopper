; ######################## Hopper stack related code ########################
                         ; 256-byte zero page               0x0000..0x00FF
                         ; 256-byte 6502 stack              0x0100..0x01FF
                         ; 256-byte keyboard buffer:        0x0200..0x02FF
                         ; 256-byte serial buffer:          0x0300..0x03FF
                    
HopperCallStack  = $0400 ; 256-byte callstack (128 slots):  0x0400..0x04FF
HopperTypeStack  = $0500 ; 256-byte type stack (256 slots): 0x0500..0x05FF
HopperValueStack = $0600 ; 512-byte value stack(256 slots): 0x0600..0x06FF or 0x07FF (8 or 16 bit stack pointer)

  .ifdef PROFILE
HopperOpProfile  = $0800 ; 512-byte opcode call counts    : 0x0800..0x09FF
HopperSysProfile = $0A00 ; 512-byte syscall call counts   : 0x0A00..0x0BFF

HopperData       = $0C00 ; where we load Hopper programs to
  .else
  .ifdef STACK8
HopperData       = $0700 ; where we load Hopper programs to
  .else
HopperData       = $0800 ; where we load Hopper programs to
  .endif
  .endif

HopperMethodTable = HopperData+6


stackInit:
  .ifdef STACK8
  
  stz SP8
  stz BP8
  stz CSP
  
  .else
  
  ; all stack LSBs start at zero
  stz BPL
  stz SPL
  stz TSPL
  stz CSP
  
  ; stack (and thus base pointer) start at $0700
  lda #>HopperValueStack
  sta SPH
  sta BPH
  
  ; type stack starts at $0600
  lda #>HopperTypeStack
  sta TSPH
  .endif
  
  .ifdef CHECKED
  
  stz IDXL
  lda #>HopperValueStack
  sta IDXH
  .ifdef STACK8
  ldx #1
  .else
  ldx #2
  .endif
  jsr clearPages ; with IDX (memory location) and X (number of pages) initialized
  
  stz IDXL
  lda #>HopperTypeStack
  sta IDXH
  ldx #1
  jsr clearPages ; with IDX (memory location) and X (number of pages) initialized
  
  stz IDXL
  lda #>HopperCallStack
  sta IDXH
  ldx #1
  jsr clearPages ; with IDX (memory location) and X (number of pages) initialized
  
  .endif
  
  rts
  
convertSPtoTSP:
  
  ; IDX -> IDY
  
  pha
  .ifdef STACK8
  
  lda IDXL
  sta IDYL
  lda #>HopperTypeStack
  sta IDYH
  
  .else
  ; IDY = IDX
  ; IDY -= HopperValueStack
  sec
  lda IDXL
  sbc #<HopperValueStack
  sta IDYL
  lda IDXH
  sbc #>HopperValueStack
  sta IDYH
  
  ; IDY /=  2
  lsr IDYH ; MSB
  ror IDYL ; LSB
  
  ; IDY += HopperTypeStack
  clc
  lda IDYL
  adc #<HopperTypeStack
  sta IDYL
  lda IDYH
  adc #>HopperTypeStack
  sta IDYH
  
  .endif
  pla
  
  rts

  .ifndef STACK8
incSP:
  .ifdef CHECKED
  pha
  lda #>HopperValueStack ; 256 slots from 0x0600..0x07FF in increments of 2
  cmp SPH
  beq incSPstackOK ; any SPL is ok
  lda #>HopperValueStack+1
  cmp SPH
  bne incSPstackNotOK
  lda #$FF
  cmp SPL
  bne incSPstackOK
incSPstackNotOK:
  pla
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?SP overflow", 0
  .endif
  jmp throwToys
incSPstackOK:
  pla
  .endif
  
  inc SPL
  bne incSPEnd
  inc SPH
incSPEnd:

  .ifdef TESTINGHWM
  pha
  phx
  ldx #0 ; SP <= HWM
  lda SPH
  cmp HWMH
  bne doneHWMGT
  lda SPL
  cmp HWML
doneHWMGT:
  beq phHWMGT ; SP == HWM (not >)
  bcc phHWMGT ; SP <  HWM (not >)
  ldx #1   ; SP > HWM
phHWMGT:
  txa
  beq notHighWaterMark
  
  lda SPL
  and #1
  cmp #1
  beq notHighWaterMark
  
  lda SPH
  sta HWMH
  lda SPL
  sta HWML
  
  ;jsr diagnosticOutStack
  jsr diagnosticOutCallStack
  jsr diagnosticOut
  
notHighWaterMark:
  plx
  pla
  .endif

  rts

incTSP:
  .ifdef CHECKED
  pha
  lda #>HopperTypeStack ; 256 single byte slots from 0x0500..0x05FF
  cmp TSPH
  bne incTSPstackNotOK
  lda #$FF
  cmp TSPL
  bne incTSPstackOK
incTSPstackNotOK:
  pla
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?TSP overflow", 0
  .endif
  jmp throwToys
incTSPstackOK:
  pla
  .endif
  
  inc TSPL
  bne incTSPEnd
  inc TSPH
incTSPEnd:
  
  rts
  
decSP:

  pha
  .ifdef CHECKED
  lda #>HopperValueStack+1 ; 256 slots from 0x0600..0x07FF in increments of 2
  cmp SPH
  beq decSPstackOK ; any SPL is ok
  lda #>HopperValueStack
  cmp SPH
  bne decSPstackNotOK
  lda #$00
  cmp SPL
  bne decSPstackOK
decSPstackNotOK:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?SP underflow", 0
  .endif
  jmp throwToysNoStack
decSPstackOK:
  .endif
  
  lda SPL
  bne decSPSkipMSB
  dec SPH
decSPSkipMSB:
  dec SPL
  pla
  
  rts

  .ifdef CHECKED
; decSP without preserving A
decSPnoA:
  
  .ifdef CHECKED
  pha
  lda #>HopperValueStack+1 ; 256 slots from 0x0600..0x07FF in increments of 2
  cmp SPH
  beq decSPnoAstackOK ; any SPL is ok
  lda #>HopperValueStack
  cmp SPH
  bne decSPnoAstackNotOK
  lda #$00
  cmp SPL
  bne decSPnoAstackOK
decSPnoAstackNotOK:
  pla
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?SP underflow", 0
  .endif
  jmp throwToysNoStack
decSPnoAstackOK:
  pla
  .endif
  
  
  lda SPL
  bne decSPSkipMSBnoA
  dec SPH
decSPSkipMSBnoA:
  dec SPL
  
  rts
  .endif
  
decTSP:

  pha
  
  .ifdef CHECKED
  lda #>HopperTypeStack ; 256 single byte slots from 0x0500..0x05FF
  cmp TSPH
  bne decTSPstackNotOK
  lda #$00
  cmp TSPL
  bne decTSPstackOK
decTSPstackNotOK:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?TSP underflow", 0
  .endif
  jmp throwToysNoStack
decTSPstackOK:
  .endif
  
  lda TSPL
  bne decTSPSkipMSB
  dec TSPH
decTSPSkipMSB:
  dec TSPL
  pla
  rts

  .ifdef CHECKED
; decTSP without preserving A
decTSPnoA:
  
  pha
  lda #>HopperTypeStack ; 256 single byte slots from 0x0500..0x05FF
  cmp TSPH
  bne decTSPnoAstackNotOK
  lda #$00
  cmp TSPL
  bne decTSPnoAstackOK
decTSPnoAstackNotOK:
  pla
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?TSP underflow", 0
  .endif
  jmp throwToysNoStack
decTSPnoAstackOK:
  pla
  
  lda TSPL
  bne decTSPSkipMSBnoA
  dec TSPH
decTSPSkipMSBnoA:
  dec TSPL
  rts
  .endif ; CHECKED
  .endif ; .ifndef STACK8

  .ifdef STRINGS
  .ifdef STACK8
replaceStackReferences:

  phx
  phy
  
  ldx SP8
replaceStackReferencesNext8:
  cpx #0
  bne replaceStackReferencesContinue8
  bra replaceStackReferencesDone8
replaceStackReferencesContinue8:
  dex
  dex
  lda HopperTypeStack, X
  cmp fTYPE
  bne replaceStackReferencesNext8
  
  lda HopperValueStack, X ; LSB
  cmp IDYL
  bne replaceStackReferencesNext8
  txa
  tay
  iny
  lda HopperValueStack, Y ; MSB
  cmp IDYH
  bne replaceStackReferencesNext8
  
  ; winner
  lda IDXL
  sta HopperValueStack, X ; LSB
  lda IDXH
  sta HopperValueStack, Y ; MSB
  
  bra replaceStackReferencesNext8
replaceStackReferencesDone8:  
  ply
  plx
  
  rts
  
  .else ; STACK8
  
replaceStackReferences:
  ; replace all references 'IDY' of fTYPE with 'IDX'
  lda SPH
  pha
  lda SPL
  pha
  lda TSPH
  pha
  lda TSPL
  pha
  
replaceStackReferencesNext:
  lda SPL
  cmp #<HopperValueStack
  bne replaceStackReferencesContinue
  lda SPH
  cmp #>HopperValueStack
  bne replaceStackReferencesContinue
  bra replaceStackReferencesDone
replaceStackReferencesContinue:
  
  jsr decSP  
  jsr decSP
  jsr decTSP
  
  lda (TSP)
  cmp fTYPE
  bne replaceStackReferencesNext
  
  ldy #1
  lda (SP)
  cmp IDYL
  bne replaceStackReferencesNext
  lda (SP), Y
  cmp IDYH
  bne replaceStackReferencesNext
  
  ; winner
  lda IDXL
  sta (SP)
  lda IDXH
  sta (SP), Y
  
  bra replaceStackReferencesNext
  
replaceStackReferencesDone:  
  pla
  sta TSPL
  pla
  sta TSPH
  pla
  sta SPL
  pla
  sta SPH
  rts
  .endif ; !STACK8
  .endif ; STRINGS
  
  .ifdef CHECKED
incCSP:
  pha
  lda #$FF
  cmp CSP
  bne incCSPstackOK
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?CSP overflow", 0
  .endif
  pla
  jmp throwToys
incCSPstackOK:
  pla
  
  inc CSP
  rts

decCSP:
  pha
  lda #0
  cmp CSP
  bne decCSPstackOK
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?CSP underflow", 0
  .endif
  jmp throwToys
decCSPstackOK:
  dec CSP
  pla
  rts
  .endif

pushIDXExit:
  ; IDX : return result to push
  ; A   : type of return result
  .ifdef STACK8
  ldx SP8
  sta HopperTypeStack, X
  lda IDXL
  sta HopperValueStack, X
  inx
  lda IDXH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  .else
  sta (TSP)
  jsr incTSP
  lda IDXL
  sta (SP)
  jsr incSP          ; LSB
  lda IDXH
  sta (SP)
  jsr incSP          ; MSB
  .endif

  jmp nextInstruction
  
pushTOPExit:
  ; TOP : return result to push
  ; A   : type of return result
  
  .ifdef CHECKED
  sta fTYPE
  cmp #tBool
  bne pushTOPExitNotBool
  lda TOPL
  sta fVALUEL
  lda TOPH
  sta fVALUEH
  jsr verifyBool
  lda fTYPE ; just to make sure it is still in A
pushTOPExitNotBool:
  .endif
  
  .ifdef STACK8
  ldx SP8
  sta HopperTypeStack, X
  lda TOPL
  sta HopperValueStack, X
  inx
  lda TOPH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  .else
  sta (TSP)
  jsr incTSP
  lda TOPL
  sta (SP)
  jsr incSP          ; LSB
  lda TOPH
  sta (SP)
  jsr incSP          ; MSB
  .endif
  jmp nextInstruction
  
pushNEXTExit:
  ; NEXT : return result to push
  ; A    : type of return result
  
  .ifdef CHECKED
  sta fTYPE
  cmp #tBool
  bne pushNEXTExitNotBool
  lda NEXTL
  sta fVALUEL
  lda NEXTH
  sta fVALUEH
  jsr verifyBool
  lda fTYPE ; just to make sure it is still in A
pushNEXTExitNotBool:
  .endif
  
  .ifdef STACK8
  ldx SP8
  sta HopperTypeStack, X
  lda NEXTL
  sta HopperValueStack, X
  inx
  lda NEXTH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  .else ; STACK8
  sta (TSP)
  jsr incTSP
  lda NEXTL
  sta (SP)
  jsr incSP          ; LSB
  lda NEXTH
  sta (SP)
  jsr incSP          ; MSB
  .endif ; !STACK8

  jmp nextInstruction
  
  
pushXBoolExit:
  ; X : 'true' (1) or 'false' (0) return result to push
  
  .ifdef CHECKED
  lda #tBool
  sta fTYPE
  stx fVALUEL
  stz fVALUEH
  jsr verifyBool
  .endif
  
  .ifdef STACK8
  txa
  ldx SP8
  sta HopperValueStack, X
  lda #tBool
  sta HopperTypeStack, X
  inx
  lda #0
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  .else
  lda #tBool
  sta (TSP)
  jsr incTSP
  txa
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  .endif
  jmp nextInstruction
  

; https://forums.atariage.com/topic/186656-clearing-a-section-of-memory/
clearPages: ; with IDX (memory location) and X (number of pages) initialized
  lda #0
clearPages2:
  ldy #0
clearPages1:
  sta (IDX), Y
  dey
  bne clearPages1
  ; next page ..
  inc IDXH
  dex
  bne clearPages2
  rts
