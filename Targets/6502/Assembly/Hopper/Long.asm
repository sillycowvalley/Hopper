; ######################## Long syscalls ########################

; Long memory map:
;   0000 heap allocator size
;   0E   type = tLong
;   00   GC reference count
;   00   long LSB
;   00
;   00
;   00   long MSB

; long New()
syscallLongNew:

  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; make sure it is zero initialized
  ldy #2
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  lda #tLong
  jmp pushIDXExit
  
; long NewFromConstant(uint constantOffset)
syscallLongNewFromConstant:

  .ifdef STACK8
  
  ; offset
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  ; offset
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  
  .endif
  
  ;uint constantStart = ReadWord(hopperStart + uint(2));
  clc
  lda #<HopperData  ; LSB
  adc #2
  sta IDYL
  lda #>HopperData  ; MSB
  adc #0
  sta IDYH
  
  ldy #0
  lda (IDY), Y
  sta fSOURCEADDRESSL
  iny
  lda (IDY), Y
  sta fSOURCEADDRESSH
    
  ;uint constantAddress = constantStart + constantOffset + hopperStart;
  clc
  lda #<HopperData  ; LSB
  adc fSOURCEADDRESSL
  sta fSOURCEADDRESSL
  lda #>HopperData  ; MSB
  adc fSOURCEADDRESSH
  sta fSOURCEADDRESSH
  
  clc
  lda IDXL  ; LSB
  adc fSOURCEADDRESSL
  sta IDXL
  lda IDXH  ; MSB
  adc fSOURCEADDRESSH
  sta IDXH
  
  ;jsr diagnosticOut
  ;jsr diagnosticOutStack
  ;jsr throwToys
  
  ; fall through with address in IDX
  
syscallLongNewFromAddress:

  ; address of 4 bytes of data in IDX
  lda IDXL
  pha
  lda IDXH
  pha
  
  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; initialize from data
  pla
  sta IDYH
  pla
  sta IDYL
  
  ;jsr diagnosticOutNewLine
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda IDYH
  ;jsr diagnosticOutHex
  ;lda IDYL
  ;jsr diagnosticOutHex
  
  ;lda #"D"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex
  ;lda IDXL
  ;jsr diagnosticOutHex
  
  ; to start Y from 2 below
  jsr decIDY
  jsr decIDY
  
  ldy #2 ; skip type and reference field
  lda (IDY), Y
  sta (IDX), Y
  iny
  lda (IDY), Y
  sta (IDX), Y
  iny
  lda (IDY), Y
  sta (IDX), Y
  iny
  lda (IDY), Y
  sta (IDX), Y
  
  lda #tLong
  jmp pushIDXExit
  

  
syscallLongAdd:

  jsr commonLongNEXTTOP
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; (IDX) = (TOP) + (NEXT)
  
  clc
  lda #2
  adc IDXL
  sta IDYL
  lda #0
  adc IDXH
  sta IDYH
  
  
  
  ; signed 32 bit addition (https://forums.nesdev.org/viewtopic.php?t=17804)
  ldy #0
  clc
  lda (NEXT), Y
  adc (TOP), Y
  sta (IDY), Y
  iny
  lda (NEXT), Y
  adc (TOP), Y
  sta (IDY), Y
  iny
  lda (NEXT), Y
  adc (TOP), Y
  sta (IDY), Y
  iny
  lda (NEXT), Y
  adc (TOP), Y
  sta (IDY), Y

  ; we popped 'top', decrease reference count
  ; we popped 'next', decrease reference count
  jsr releaseSPandSPNEXT
  
  lda #tLong
  jmp pushIDXExit
  
syscallLongSub:

  jsr commonLongNEXTTOP
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; (IDX) = (NEXT) - (TOP)
  
  clc
  lda #2
  adc IDXL
  sta IDYL
  lda #0
  adc IDXH
  sta IDYH
  
  
  ; signed 32 bit addition (https://forums.nesdev.org/viewtopic.php?t=17804)
  ldy #0
  sec
  lda (NEXT), Y
  sbc (TOP), Y
  sta (IDY), Y
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta (IDY), Y
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta (IDY), Y
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta (IDY), Y
  
  ; we popped 'top', decrease reference count
  ; we popped 'next', decrease reference count
  jsr releaseSPandSPNEXT
  
  lda #tLong
  jmp pushIDXExit
  
negateLongTOP:
  ; T = 0 - T
  sec
  lda #0
  sbc lTOP0
  sta lTOP0
  lda #0
  sbc lTOP1
  sta lTOP1
  lda #0
  sbc lTOP2
  sta lTOP2
  lda #0
  sbc lTOP3
  sta lTOP3
  rts

negateLongNEXT:
  ; N = 0 - N
  sec
  lda #0
  sbc lNEXT0
  sta lNEXT0
  lda #0
  sbc lNEXT1
  sta lNEXT1
  lda #0
  sbc lNEXT2
  sta lNEXT2
  lda #0
  sbc lNEXT3
  sta lNEXT3
  rts

  
utilityDoLongSign:
  phx
  
  ldx #0
  lda lTOP3
  asl ; sign bit into carry
  bcc utilityLongTOPIsPositive2 ; +ve
  inx ; count the -ve
  jsr negateLongTOP ; TOP = -TOP
utilityLongTOPIsPositive2:
  stx fSIGN ; store the sign count
  
  plx
  rts

utilityDoLongSigns:
  phx
  
  ldx #0
  lda lNEXT3
  asl ; sign bit into carry
  bcc utilityLongNEXTIsPositive ; +ve
  inx ; count the -ve
  jsr negateLongNEXT ; NEXT = -NEXT
utilityLongNEXTIsPositive:
  lda lTOP3
  asl ; sign bit into carry
  bcc utilityLongTOPIsPositive ; +ve
  inx ; count the -ve
  jsr negateLongTOP ; TOP = -TOP
utilityLongTOPIsPositive:
  stx fSIGN ; store the sign count
  
  plx
  rts



syscallLongMul:

  jsr commonLongNEXTTOP
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; transfer to N (after gcCreate)
  ldy #0
  lda (TOP), Y
  sta lTOP0
  lda (NEXT), Y
  sta lNEXT0
  iny
  lda (TOP), Y
  sta lTOP1
  lda (NEXT), Y
  sta lNEXT1
  iny
  lda (TOP), Y
  sta lTOP2
  lda (NEXT), Y
  sta lNEXT2
  iny
  lda (TOP), Y
  sta lTOP3
  lda (NEXT), Y
  sta lNEXT3
  
  jsr utilityDoLongSigns
  
  ; (IDX) = (NEXT) * (TOP)
  
  clc
  lda #2
  adc IDXL
  sta IDYL
  lda #0
  adc IDXH
  sta IDYH

  ; #### https://llx.com/Neil/a2/mult.html ####
  ; http://www.6502.org/source/integers/32muldiv.htm
  
  jsr utilityLongMUL
  
  ; we popped 'top', decrease reference count (munts all Nx variables if memoryFree is called)
  ; we popped 'next', decrease reference count (munts all Nx variables if memoryFree is called)
  jsr releaseSPandSPNEXT
  
  lda #tLong
  jmp pushIDXExit

syscallLongDiv:

  jsr longDivMod
  
  lda fSIGN ; load the sign count
  cmp #1
  bne syscallLongDivSignsEven
  jsr negateLongNEXT ; NEXT  = -NEXT 
syscallLongDivSignsEven:
  
  ; use the N
  ldy #0
  lda lNEXT0
  sta (IDY), Y
  iny
  lda lNEXT1
  sta (IDY), Y
  iny
  lda lNEXT2
  sta (IDY), Y
  iny
  lda lNEXT3
  sta (IDY), Y
  
  ; we popped 'top', decrease reference count (munts all Nx variables if memoryFree is called)
  ; we popped 'next', decrease reference count (munts all Nx variables if memoryFree is called)
  jsr releaseSPandSPNEXT
  
  lda #tLong
  jmp pushIDXExit

commonLongTOP:
  
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta TOPH
  dex
  lda HopperValueStack, X
  sta TOPL
  stx SP8
  
  .else
  
  jsr decSP          ; MSB
  lda (SP)
  sta TOPH
  jsr decSP          ; LSB
  lda (SP)
  sta TOPL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertLong
  .endif
  .endif
  
  clc
  lda #2
  adc TOPL
  sta TOPL
  bcc commonLongTOPSkipMSB
  inc TOPH
commonLongTOPSkipMSB
  ;lda #0
  ;adc TOPH
  ;sta TOPH
  
  rts

commonLongNEXTTOP:
  jsr commonLongTOP
  
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta NEXTH
  dex
  lda HopperValueStack, X
  sta NEXTL
  stx SP8
  
  .else
  
  jsr decSP          ; MSB
  lda (SP)
  sta NEXTH
  jsr decSP          ; LSB
  lda (SP)
  sta NEXTL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertLong
  .endif
  .endif
  
  clc
  lda #2
  adc NEXTL
  sta NEXTL
  bcc commonLongNEXTTOPSkipMSB
  inc NEXTH
commonLongNEXTTOPSkipMSB  
  ;lda #0
  ;adc NEXTH
  ;sta NEXTH
  
  rts
  
commonLongCompareExit:  
  ; we popped 'top', decrease reference count
  ; we popped 'next', decrease reference count
  jsr releaseSPandSPNEXT
  
  pla
  .ifdef STACK8
  
  ldx SP8
  sta HopperValueStack, X
  lda #tBool
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
  
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tBool
  sta (TSP)
  jsr incTSP
  
  .endif
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
syscallLongLE:

  jsr commonLongNEXTTOP
  
  ;   NEXT <= TOP
  ; same as
  ;   TOP >= NEXT
  
  ; swap NEXT and TOP:
  lda TOPL
  sta ACCL
  lda TOPH
  sta ACCH
  
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  
  lda ACCL
  sta NEXTL
  lda ACCH
  sta NEXTH
  
  bra commonLongGE

syscallLongGE:
  
  jsr commonLongNEXTTOP
  
commonLongGE:

  ; NEXT >= TOP?
  
  ; signed 32 bit subtraction (https://forums.nesdev.org/viewtopic.php?t=17804)
  ldy #0
  sec
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT0
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT1
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT2
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT3
  
  ; is negative sign set?
  ldx #1
  lda lNEXT3
  and #$80
  beq syscallLongGEPositive ;  NEXT >= TOP? -> +ve or zero (sign not set)
  ldx #0
syscallLongGEPositive:
  phx
  
  jmp commonLongCompareExit
  
syscallLongGT:

  jsr commonLongNEXTTOP
  ;   NEXT > TOP?
  ; same as:
  ;   TOP < NEXT?
  
   ; swap NEXT and TOP:
  lda TOPL
  sta ACCL
  lda TOPH
  sta ACCH
  
  lda NEXTL
  sta TOPL
  lda NEXTH
  sta TOPH
  
  lda ACCL
  sta NEXTL
  lda ACCH
  sta NEXTH
  
  bra commonLongLT
  
syscallLongLT:

  jsr commonLongNEXTTOP
  
commonLongLT:  
  ; NEXT < TOP?
  
  ; signed 32 bit subtraction (https://forums.nesdev.org/viewtopic.php?t=17804)
  ldy #0
  sec
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT0
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT1
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT2
  iny
  lda (NEXT), Y
  sbc (TOP), Y
  sta lNEXT3
  
  ; is negative sign set?
  ldx #0
  lda lNEXT3
  and #$80
  beq syscallLongLTPositive ;  NEXT >= TOP? -> +ve or zero (sign not set)
  ldx #1
syscallLongLTPositive:
  phx
  
  jmp commonLongCompareExit
  
syscallLongEQ:

  jsr commonLongNEXTTOP
  
  ; NEXT == TOP?
  ldx #0
  ldy #0
  lda (NEXT), Y
  cmp (TOP), Y
  bne syscallLongNotEQ
  iny
  lda (NEXT), Y
  cmp (TOP), Y
  bne syscallLongNotEQ
  iny
  lda (NEXT), Y
  cmp (TOP), Y
  bne syscallLongNotEQ
  iny
  lda (NEXT), Y
  cmp (TOP), Y
  bne syscallLongNotEQ
  ldx #1
syscallLongNotEQ:
  phx
  
  jmp commonLongCompareExit
  
syscallLongNegate:

  jsr commonLongTOP
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; (IDX) = 0 - (TOP)
  
  clc
  lda #2
  adc IDXL
  sta IDYL
  lda #0
  adc IDXH
  sta IDYH
  
  ; signed 32 bit addition (https://forums.nesdev.org/viewtopic.php?t=17804)
  ldy #0
  sec
  lda #0
  sbc (TOP), Y
  sta (IDY), Y
  iny
  lda #0
  sbc (TOP), Y
  sta (IDY), Y
  iny
  lda #0
  sbc (TOP), Y
  sta (IDY), Y
  iny
  lda #0
  sbc (TOP), Y
  sta (IDY), Y
  
  jsr releaseSP ; we popped 'to', decrease reference count (munts all Nx variables if memoryFree is called)
  
  lda #tLong
  jmp pushIDXExit


  .ifdef LISTS
syscallLongToBytes:

  jsr commonLongTOP
  
  ; (TOP) now points to first byte
  lda TOPL
  sta fSOURCEADDRESSL
  lda TOPH
  sta fSOURCEADDRESSH
  
  lda #9
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tList
  jsr gcCreate ; destroys Nx variables in memoryAllocate (preserves TOP)
  
  ldy #2
  ; current number of items
  lda #4
  sta (IDX), Y
  iny
  lda #0
  sta (IDX), Y
  iny
  lda #tByte
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
  
  ; save pointer to list
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; save the pFirst pointer
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH
  
  ; 5x incDESTINATIONADDRESS
  clc
  lda fDESTINATIONADDRESSL  ; LSB
  adc #5
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  ; create 4x list items using the data at (SOURCEADDRESS)
  
  ldx #4
syscallLongToBytesNext:  
  phx
  
  lda (fSOURCEADDRESS)
  sta fVALUEL
  stz fVALUEH
  
  lda #tByte
  
  ; type in A
  ; value in fVALUE
  ; uses fSIZE
  jsr createValueVariant
  ; return tVariant in IDX

  ; pData value is in IDX
  ; uses fSIZE
  ; sets pNext = 0
  jsr listitemCreate
  ; returns tListItem in fITEM
  
  jsr incSOURCEADDRESS
  
  ldy #1
  lda fITEML
  sta (fDESTINATIONADDRESS)
  lda fITEMH
  sta (fDESTINATIONADDRESS), Y
  
  ; save the pNext pointer
  lda fITEML
  sta fDESTINATIONADDRESSL
  lda fITEMH
  sta fDESTINATIONADDRESSH
  
  ; pNext is at offset 4 in tListItem:
  ; 4x incDESTINATIONADDRESS
  clc
  lda fDESTINATIONADDRESSL  ; LSB
  adc #4
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  plx
  dex
  bne syscallLongToBytesNext
  
  jsr releaseSP ; we popped 'to', decrease reference count (preserves IDX)
  
  ; IDX points to list
  pla
  sta IDXH
  pla
  sta IDXL

  lda #tList
  jmp pushIDXExit
  .endif  
  
syscallLongToInt:

  jsr commonLongTOP
  
  ; (TOP) -> TOP
  
  ; transfer to N
  ldy #0
  lda (TOP), Y
  sta lTOP0
  iny
  lda (TOP), Y
  sta lTOP1
  iny
  lda (TOP), Y
  sta lTOP2
  iny
  lda (TOP), Y
  sta lTOP3
  
  jsr utilityDoLongSign
  
  .ifdef CHECKED
  lda #0
  cmp lTOP3
  bne syscallLongToIntOverflow
  cmp lTOP2
  bne syscallLongToIntOverflow
  bra syscallLongToIntInRange
syscallLongToIntOverflow:

  sta ACCH
  lda #$0D ; numeric type out of range / overflow
  sta ACCL
  jmp utilityDiagnosticsDie
  
syscallLongToIntInRange:
  .endif
  
  lda lTOP0
  sta TOPL
  lda lTOP1
  sta TOPH
  
  lda fSIGN ; load the sign count
  cmp #1
  bne syscallLongToIntEven
  jsr negateTOP ; TOP  = -TOP
syscallLongToIntEven:
  
  jsr releaseSP ; we popped 'to', decrease reference count (munts all Nx variables if memoryFree is called)
  
  lda #tInt
  jmp pushTOPExit
  
syscallLongToUInt:

  jsr commonLongTOP
  
  ; (TOP) -> TOP
  
  ; transfer to N
  ldy #0
  lda (TOP), Y
  sta lTOP0
  iny
  lda (TOP), Y
  sta lTOP1
  iny
  lda (TOP), Y
  sta lTOP2
  iny
  lda (TOP), Y
  sta lTOP3
  
  jsr utilityDoLongSign
  
  .ifdef CHECKED
  
  cmp #1
  bne syscallLongToUIntEven
  sta ACCH
  lda #$0D
  sta ACCL
  jmp utilityDiagnosticsDie
  
syscallLongToUIntEven:
  .endif
  
  lda lTOP0
  sta TOPL
  lda lTOP1
  sta TOPH
  
  jsr releaseSP ; we popped 'to', decrease reference count
  
  lda #tUInt
  jmp pushTOPExit  


syscallLongMod: 
  
  jsr longDivMod
  
  ; use the remainder, ignore sign
  ldy #0
  lda lRESULT0
  sta (IDY), Y
  iny
  lda lRESULT1
  sta (IDY), Y
  iny
  lda lRESULT2
  sta (IDY), Y
  iny
  lda lRESULT3
  sta (IDY), Y
  
  ; we popped 'top', decrease reference count (munts all Nx variables if memoryFree is called)
  ; we popped 'next', decrease reference count (munts all Nx variables if memoryFree is called)
  jsr releaseSPandSPNEXT
  
  lda #tLong
  jmp pushIDXExit

  
utilityLongMUL:
  ; (IDX) = lNEXT0..lNEXT3 * lTOP0..lTOP3
  
  ; https://llx.com/Neil/a2/mult.html
  ; http://www.6502.org/source/integers/32muldiv.htm
  
  lda #$00
  sta lRESULT4   ;Clear upper half of
  sta lRESULT5   ;product
  sta lRESULT6
  sta lRESULT7
  ldx #$20     ; set binary count to 32
utilityLongMULShiftR:   
  lsr lNEXT3   ; shift multiplyer right
  ror lNEXT2
  ror lNEXT1
  ror lNEXT0
  bcc utilityLongMULRotateR ;Go rotate right if c = 0
  lda lRESULT4   ; get upper half of product and add multiplicand to it
  clc               
  adc lTOP0
  sta lRESULT4
  lda lRESULT5
  adc lTOP1
  sta lRESULT5
  lda lRESULT6
  adc lTOP2
  sta lRESULT6
  lda lRESULT7
  adc lTOP3
utilityLongMULRotateR:
  ror a    ; rotate partial product
  sta lRESULT7   ; right
  ror lRESULT6
  ror lRESULT5
  ror lRESULT4
  ror lRESULT3
  ror lRESULT2
  ror lRESULT1
  ror lRESULT0
  dex              ; decrement bit count and
  bne     utilityLongMULShiftR  ; loop until 32 bits are done
  ;clc              
  ;lda     MULXP1   ; add dps and put sum in MULXP2
  ;adc     MULXP2
  ;sta     MULXP2
  
  
  lda fSIGN ; load the sign count
  cmp #1
  bne utilityLongMULISignsEven
  jsr negateLongRESULT ; RESULT  = -RESULT 
utilityLongMULISignsEven:
  
  
  ; keep the least significant 4 bytes of the result
  ldy #0
  lda lRESULT0
  sta (IDY), Y
  iny
  lda lRESULT1
  sta (IDY), Y
  iny
  lda lRESULT2
  sta (IDY), Y
  iny
  lda lRESULT3
  sta (IDY), Y
  rts
  
  
negateLongRESULT:
  ; R = 0 - R
  sec
  lda #0
  sbc lRESULT0
  sta lRESULT0
  lda #0
  sbc lRESULT1
  sta lRESULT1
  lda #0
  sbc lRESULT2
  sta lRESULT2
  lda #0
  sbc lRESULT3
  sta lRESULT3
  rts



  
  
longDivMod:
  jsr commonLongNEXTTOP
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; transfer to N (after gcCreate)
  ldy #0
  lda (TOP), Y
  sta lTOP0
  lda (NEXT), Y
  sta lNEXT0
  iny
  lda (TOP), Y
  sta lTOP1
  lda (NEXT), Y
  sta lNEXT1
  iny
  lda (TOP), Y
  sta lTOP2
  lda (NEXT), Y
  sta lNEXT2
  iny
  lda (TOP), Y
  sta lTOP3
  lda (NEXT), Y
  sta lNEXT3
  
  jsr utilityDoLongSigns
   
  ; #### https://llx.com/Neil/a2/mult.html ####
  ; http://www.6502.org/source/integers/32muldiv.htm
  
  
  ;NUM1 = NUM1 / NUM2 + REM
  ;lNEXT = lNEXT / lTOP + lRESULT
  
  ;Initialize remainder to 0
  stz lRESULT0
  stz lRESULT1
  stz lRESULT2
  stz lRESULT3
  ldx #32   ; there are 16 bits in N
longDivModNextBit:
  asl lNEXT0    ; shift hi bit of N into R
  rol lNEXT1    ; (vacating the lo bit, which will be used for the quotient)
  rol lNEXT2
  rol lNEXT3
  rol lRESULT0
  rol lRESULT1
  rol lRESULT2
  rol lRESULT3
  
  sec       ; trial subtraction
  lda lRESULT0
  sbc lTOP0
  sta lRESULT4
  lda lRESULT1
  sbc lTOP1
  sta lRESULT5
  lda lRESULT2
  sbc lTOP2
  sta lRESULT6
  lda lRESULT3
  sbc lTOP3
  ;sta lRESULT7
  
  bcc longDivModNextNo      ; did subtraction succeed?
  ;lda lRESULT7
  sta lRESULT3   ; if yes, save it
  lda lRESULT6
  sta lRESULT2
  lda lRESULT5 
  sta lRESULT1
  lda lRESULT4
  sta lRESULT0
  inc lNEXT0   ; and record a 1 in the quotient
longDivModNextNo:
  dex
  bne longDivModNextBit
  
  clc
  lda #2
  adc IDXL
  sta IDYL
  lda #0
  adc IDXH
  sta IDYH

  rts


cloneLong:
  ; used by cloneDictionary, cloneString and cloneLong
  lda fSOURCEADDRESSH
  pha
  lda fSOURCEADDRESSL
  pha
  
  ; munts A, Y and all Nx
  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ; initialized from long at IDY
  lda IDYL
  sta fSOURCEADDRESSL
  lda IDYH
  sta fSOURCEADDRESSH
  
  ldy #2
  lda (fSOURCEADDRESS), Y
  sta (IDX), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (IDX), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (IDX), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (IDX), Y
  
  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  
  rts