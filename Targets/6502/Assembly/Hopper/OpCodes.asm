; ######################## OpCode lookup table ########################

opCodeJumpTable:
  ; pop2Push1Unsigned:     ; $00..$10
  .word opcodeADD          ; 00
  .word opcodeSUB          ; 01
  .word opcodeDIV          ; 02
  .word opcodeMUL          ; 03
  .word opcodeMOD          ; 04
  .word opcodeEQ           ; 05
  .word opcodeNE           ; 06
  .word opcodeGT           ; 07
  .word opcodeLT           ; 08
  .word opcodeGE           ; 09
  .word opcodeLE           ; 0A
  .word opcodeBOOLOR       ; 0B
  .word opcodeBOOLAND      ; 0C
  .word opcodeBITOR        ; 0D
  .word opcodeBITAND       ; 0E
  .word opcodeBITSHL       ; 0F
  .word opcodeBITSHR       ; 10
  
  ; pop2Push1Signed:       ; $11..$19
  .word opcodeADDI         ; 11
  .word opcodeSUBI         ; 12
  .word opcodeDIVI         ; 13
  .word opcodeMULI         ; 14
  .word opcodeMODI         ; 15
  .word opcodeGTI          ; 16
  .word opcodeLTI          ; 17
  .word opcodeGEI          ; 18
  .word opcodeLEI          ; 19
  
  ; byteOperand:           ; $1A.$30
  .word opcodePUSHIB       ; 1A
  .word opcodePOPLOCALB    ; 1B
  .word opcodePUSHLOCALB   ; 1C
  .word opcodePOPRELB      ; 1D
  .word opcodePUSHRELB     ; 1E
  .word opcodePOPGLOBALB   ; 1F
  .word opcodePUSHGLOBALB  ; 20
  .word opcodePUSHSTACKADDRB ; 21
  .word opcodeINCLOCALB    ; 22
  .word opcodeDECLOCAL     ; 23
  .word opcodeSYSCALL0     ; 24
  .word opcodeSYSCALL1     ; 25
  .word opcodeSYSCALL      ; 26
  .word opcodeDUP          ; 27
  .word opcodeDECSP        ; 28
  .word opcodeDIE          ; 29
  .word opcodeRETB         ; 2A
  .word opcodeRETRETB      ; 2B
  .word unknownOpCode      ; 2C CALLB
  .word opcodeTESTBPB      ; 2D
  .word opcodeJZB          ; 2E
  .word opcodeJNZB         ; 2F
  .word opcodeJB           ; 30
  
  ; wordOperand:           ; $31..$3F
  .word opcodeJZW          ; 31
  .word opcodeJNZW         ; 32
  .word opcodeJW           ; 33
  .word opcodeCALLW        ; 34
  .word unknownOpCode      ; 35 RETW
  .word unknownOpCode      ; 36 RETRETW
  .word opcodePUSHIW       ; 37
  .word unknownOpCode      ; 38 POPLOCALW
  .word unknownOpCode      ; 39 PUSHLOCALW
  .word unknownOpCode      ; 3A POPRELW
  .word unknownOpCode      ; 3B PUSHRELW
  .word unknownOpCode      ; 3C POPGLOBALW
  .word unknownOpCode      ; 3D PUSHGLOBALW
  .word unknownOpCode      ; 3E PUSHSTACKADDRW
  .word opcodeINCLOCALBB   ; 3F
  .word opcodePUSHIWLE     ; 40
  
  ; noOperand:             ; $41..$50
  .word opcodeBOOLNOT      ; 41
  .word unknownOpCode      ; 42 BITNOT
  .word opcodeSWAP         ; 43
  .word opcodePUSHI0       ; 44
  .word opcodePUSHI1       ; 45
  .word opcodePUSHIM1      ; 46
  .word opcodePUSHGP       ; 47
  .ifdef HEAP
  .word opcodeCOPYNEXTPOP  ; 48
  .else
  .word unknownOpCode
  .endif
  .word opcodeENTER        ; 49
  .word opcodeRET0           ; 4A
  .word opcodeCALLREL       ; 4B
  .word opcodePOPLOCALB00   ; 4C
  .word opcodePOPLOCALB02   ; 4D
  .word opcodePUSHLOCALB00  ; 4E
  .word opcodePUSHLOCALB02  ; 4F
  .word opcodeNOP           ; 50
  .word opcodeCAST          ; 51
  .word opcodePUSHGLOBALBB  ; 52
  .word opcodeINCGLOBALB    ; 53
  .word opcodeDECGLOBALB    ; 54
  
  .word opcodePUSHIWLT      ; 55
  
  .word opcodePUSHLOCALBB   ; 56
  .ifdef HEAP
  .word opcodePOPCOPYLOCALB ; 57
  .word opcodePOPCOPYRELB   ; 58
  .word opcodePOPCOPYGLOBALB; 59
  .else
  .word unknownOpCode
  .word unknownOpCode
  .word unknownOpCode
  .endif
  .word unknownOpCode      ; 5A opcodePOPCOPYLOCALW   // unused?
  .word unknownOpCode      ; 5B opcodePOPCOPYRELW     // unused?
  .word unknownOpCode      ; 5C opcodePOPCOPYGLOBALW

  .ifdef HEAP
  .word opcodePOPCOPYLOCALB00 ; 5D
  .word opcodePOPCOPYLOCALB02 ; 5E
  .else
  .word unknownOpCode
  .word unknownOpCode
  .endif
  
  .word opcodeENTERB          ; 5F

  .ifdef UNUSED
  
operandSizeTable:
  .byte 0 ; ADD
  .byte 0 ; SUB
  .byte 0 ; DIV
  .byte 0 ; MUL
  .byte 0 ; MOD
  .byte 0 ; EQ
  .byte 0 ; NE
  .byte 0 ; GT
  .byte 0 ; LT
  .byte 0 ; GE
  .byte 0 ; LE
  .byte 0 ; BOOLOR
  .byte 0 ; BOOLAND
  .byte 0 ; BITOR
  .byte 0 ; BITAND
  .byte 0 ; BITSHL
  .byte 0 ; BITSHR
  
  .byte 0 ; ADDI
  .byte 0 ; SUBI
  .byte 0 ; DIVI
  .byte 0 ; MULI
  .byte 0 ; MODI
  .byte 0 ; GTI
  .byte 0 ; LTI
  .byte 0 ; GEI
  .byte 0 ; LEI
  
  .byte 1 ; PUSHIB
  .byte 1 ; POPLOCALB
  .byte 1 ; PUSHLOCALB
  .byte 1 ; POPRELB
  .byte 1 ; PUSHRELB
  .byte 1 ; POPGLOBALB
  .byte 1 ; PUSHGLOBALB
  .byte 1 ; PUSHSTACKADDRB
  .byte 1 ; INCLOCALB
  .byte 1 ; DECLOCAL
  .byte 1 ; SYSCALL0
  .byte 1 ; SYSCALL1
  .byte 1 ; SYSCALL
  .byte 1 ; DUP
  .byte 1 ; DECSP
  .byte 1 ; DIE
  .byte 1 ; RETB
  .byte 1 ; RETRETB
  .byte 1 ; CALLB
  .byte 1 ; TESTBPB
  .byte 1 ; JZB
  .byte 1 ; JNZB
  .byte 1 ; JB
  
  .byte 2 ; JZW
  .byte 2 ; JNZW
  .byte 2 ; JW
  .byte 2 ; CALLW
  .byte 2 ; RETW
  .byte 2 ; RETRETW
  .byte 2 ; PUSHIW
  .byte 2 ; POPLOCALW
  .byte 2 ; PUSHLOCALW
  .byte 2 ; POPRELW
  .byte 2 ; PUSHRELW
  .byte 2 ; POPGLOBALW
  .byte 2 ; PUSHGLOBALW
  .byte 2 ; PUSHSTACKADDRW
  .byte 2 ; INCLOCALBB
  .byte 2 ; PUSHIWLE
  
  .byte 0 ; BOOLNOT
  .byte 0 ; BITNOT
  .byte 0 ; SWAP
  .byte 0 ; PUSHI0
  .byte 0 ; PUSHI1
  .byte 0 ; PUSHIM1
  .byte 0 ; PUSHGP
  .byte 0 ; COPYNEXTPOP
  .byte 0 ; ENTER
  .byte 0 ; RET0
  .byte 0 ; CALLREL
  .byte 0 ; POPLOCALB00
  .byte 0 ; POPLOCALB02
  .byte 0 ; PUSHLOCALB00
  .byte 0 ; PUSHLOCALB02
  .byte 0 ; NOP
  
  .byte 1 ; CAST
  .byte 2 ; PUSHGLOBALBB
  .byte 1 ; INCGLOBALB
  .byte 1 ; DECGLOBALB
  
  .byte 2 ; PUSHIWLT
  
  .byte 2 ; PUSHLOCALBB
  
  .byte 1 ; POPCOPYLOCALB
  .byte 1 ; POPCOPYRELB
  .byte 1 ; POPCOPYGLOBALB
  .byte 2 ; POPCOPYLOCALW
  .byte 2 ; POPCOPYRELW
  .byte 2 ; POPCOPYGLOBALW

  .byte 0 ; POPCOPYLOCALB00
  .byte 0 ; POPCOPYLOCALB02
  
  .byte 1 ; opcodeENTERB
  .endif
  
; ######################## OpCode subroutines ######################## 

  .include Push.asm
  .include Pop.asm
  

opcodeNOP:
  jmp nextInstruction          ; 3
  
  
  .ifdef STACK8

opcodeCAST:
  
  ldx SP8
  dex
  dex
  
  ; PC++
  inc PCL
  bne incPCopcodeCAST
  inc PCH
incPCopcodeCAST:

  lda (PC) 
  
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  cmp #tBool
  bne opcodeCASTNotBool
  jsr verifyBoolStackTop
opcodeCASTNotBool:
  .endif
  
  jmp nextInstruction
  .else

opcodeCAST:
  lda TSPL
  sta IDXL
  lda TSPH
  sta IDXH
  jsr decIDX

  ; PC++
  inc PCL
  bne incPCopcodeCAST
  inc PCH
incPCopcodeCAST:

  lda (PC) 
  sta (IDX)  
  
  .ifdef CHECKED
  cmp #tBool
  bne opcodeCASTNotBool
  jsr verifyBoolStackTop
opcodeCASTNotBool:
  .endif
  
  jmp nextInstruction
  .endif
  
opcodeDIE:
  jsr incPC ; PC++
  lda (PC) 
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie

  .ifndef STACK8
opcodeINCLOCALBB:

  ; IDX = BP
  lda BPL
  sta IDXL
  lda BPH
  sta IDXH
  
  ; TODO - test positive offset:
  jsr incPC ; PC++
  lda (PC)  ; offset
  
  ; IDX += offset
  
  bpl inclPositive0
  ; offset >= 128
  ; -= 256
  dec IDXH
inclPositive0:
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc inclEnd0
  inc IDXH
inclEnd0:

  ; IDY = BP
  lda BPL
  sta IDYL
  lda BPH
  sta IDYH
  
  jsr incPC ; PC++
  lda (PC)  ; offset
  
  ; IDY += offset
  
  bpl inclPositive1
  ; offset >= 128
  ; -= 256
  dec IDYH
inclPositive1:
  ; += offset
  clc
  adc IDYL
  sta IDYL
  bcc inclEnd1
  inc IDYH
inclEnd1:

  ldy #1
  clc
  lda (IDX)
  adc (IDY)
  sta (IDX)
  lda (IDX), Y
  adc (IDY), Y
  sta (IDX), Y
  
  jmp nextInstruction
  .endif
  
  .ifdef STACK8
opcodeADDI:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tInt
  sta HopperTypeStack, Y
  clc
  lda HopperValueStack, Y
  adc HopperValueStack, X
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, Y
  adc HopperValueStack, X
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  
  jmp nextInstruction
  
  .else
  
opcodeADDI:
  clc
  lda NEXTL
  adc TOPL
  sta NEXTL
  lda NEXTH
  adc TOPH
  sta NEXTH
  
  lda #tInt
  jmp pushNEXTExit
  .endif
  
  .ifdef STACK8
  
opcodeADD:

  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tUInt
  sta HopperTypeStack, Y
  clc
  lda HopperValueStack, Y
  adc HopperValueStack, X
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, Y
  adc HopperValueStack, X
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  
  
  jmp nextInstruction
  
  .else
  
opcodeADD:
  clc
  lda NEXTL
  adc TOPL
  sta NEXTL
  lda NEXTH
  adc TOPH
  sta NEXTH
  
  lda #tUInt
  jmp pushNEXTExit
  .endif

  .ifdef STACK8
opcodeSUBI:

  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tInt
  sta HopperTypeStack, Y
  sec
  lda HopperValueStack, Y
  sbc HopperValueStack, X
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, Y
  sbc HopperValueStack, X
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  
  jmp nextInstruction
  
  .else

opcodeSUBI:
  ; TOP = NEXT - TOP
  sec
  lda NEXTL
  sbc TOPL
  sta NEXTL
  lda NEXTH
  sbc TOPH
  sta NEXTH
  
  lda #tInt
  jmp pushNEXTExit
  .endif
  
  .ifdef STACK8
  
opcodeSUB:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tInt
  sta HopperTypeStack, Y
  sec
  lda HopperValueStack, Y
  sbc HopperValueStack, X
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, Y
  sbc HopperValueStack, X
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  
  jmp nextInstruction
  
  .else
  
opcodeSUB:
  ; TOP = NEXT - TOP
  sec
  lda NEXTL
  sbc TOPL
  sta NEXTL
  lda NEXTH
  sbc TOPH
  sta NEXTH
  
  lda #tUInt
  jmp pushNEXTExit
  
  .endif
  
  .ifdef STACK8
opcodeMULI:

  ldx SP8
  dex
  lda HopperValueStack, X ; TOP MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; TOP LSB
  sta TOPL
  dex
  lda HopperValueStack, X ; NEXT MSB
  sta NEXTH
  dex
  lda HopperValueStack, X ; NEXT LSB
  sta NEXTL
  stx SP8
  
  .ifdef FASTINTS
  lda TOPH
  bne utilityMULITryNEXT

  ; try TOPL  
  lda TOPL
  bne utilityMULITOPLNotZero
  ; * 0 optimization
  ; TOP is already zero
  lda #tInt
  jmp pushTOPExit
  
utilityMULITOPLNotZero:

utilityMULITryNEXT:
  ; try NEXTL
  
  lda NEXTL
  bne utilityMULINEXTLNotZero
  ; * 0 optimization
  stz TOPL
  stz TOPH
  lda #tInt
  jmp pushTOPExit
utilityMULINEXTLNotZero:
  
utilityMULIRegular:
  .endif
  
  jsr utilityMULI
  
  lda #tInt
  jmp pushTOPExit
  
  .else

opcodeMULI:
  jsr utilityMULI
  lda #tInt
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
  
opcodeMUL:
  
  ldx SP8
  dex
  lda HopperValueStack, X ; TOP MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; TOP LSB
  sta TOPL
  dex
  lda HopperValueStack, X ; NEXT MSB
  sta NEXTH
  dex
  lda HopperValueStack, X ; NEXT LSB
  sta NEXTL
  stx SP8
  
  .ifdef FASTINTS
  lda TOPH
  bne utilityMULTryNEXT

  ; try TOPL  
  lda TOPL
  bne utilityMULTOPLNotZero
  ; * 0 optimization
  ; TOP is already zero
  lda #tInt
  jmp pushTOPExit
  
utilityMULTOPLNotZero:

utilityMULTryNEXT:
  ; try NEXTL
  
  lda NEXTL
  bne utilityMULNEXTLNotZero
  ; * 0 optimization
  stz TOPL
  stz TOPH
  lda #tInt
  jmp pushTOPExit
utilityMULNEXTLNotZero:
  
utilityMULRegular:
  .endif
  
  jsr utilityMUL
  
  lda #tUInt
  jmp pushTOPExit
  
  .else
  
opcodeMUL:
  jsr utilityMUL
  lda #tUInt
  jmp pushTOPExit

  .endif

  .ifdef STACK8
  
opcodeDIV:
  ldx SP8
  dex
  lda HopperValueStack, X ; TOP MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; TOP LSB
  sta TOPL
  dex
  lda HopperValueStack, X ; NEXT MSB
  sta NEXTH
  dex
  lda HopperValueStack, X ; NEXT LSB
  sta NEXTL
  stx SP8
  
  ; TOP = NEXT (dividend=result) / TOP (divisor)
  ; ACC (remainder)
  jsr utilityDIV
  
  lda #tUInt
  jmp pushNEXTExit

  .else
  
opcodeDIV:
  jsr utilityDIV
  
  lda #tUInt
  jmp pushNEXTExit
  .endif
  
  .ifdef STACK8
  
opcodeMOD:
  ldx SP8
  dex
  lda HopperValueStack, X ; TOP MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; TOP LSB
  sta TOPL
  dex
  lda HopperValueStack, X ; NEXT MSB
  sta NEXTH
  dex
  lda HopperValueStack, X ; NEXT LSB
  sta NEXTL
  stx SP8
  
  ; TOP = NEXT (dividend=result) / TOP (divisor)
  ; ACC (remainder)
  jsr utilityDIVMOD
  
  ldx SP8
  lda ACCL
  sta HopperValueStack, X ; ACC LSB
  lda #tUInt
  sta HopperTypeStack, X
  inx
  lda ACCH
  sta HopperValueStack, X ; ACC MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  inx
  stx SP8
  jmp nextInstruction
  
  .else
  
opcodeMOD:
  jsr utilityDIVMOD
  
  lda ACCL
  sta (SP)
  jsr incSP
  lda ACCH
  sta (SP)
  jsr incSP
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  jmp nextInstruction
  .endif
  
  .ifdef STACK8
opcodeMODI:
  
  ldx SP8
  dex
  lda HopperValueStack, X ; TOP MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; TOP LSB
  sta TOPL
  dex
  lda HopperValueStack, X ; NEXT MSB
  sta NEXTH
  dex
  lda HopperValueStack, X ; NEXT LSB
  sta NEXTL
  stx SP8
  
  jsr utilityMODI
  
  ldx SP8
  lda ACCL
  sta HopperValueStack, X ; TOP LSB
  lda #tUInt
  sta HopperTypeStack, X
  inx
  lda ACCH
  sta HopperValueStack, X ; TOP MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  inx
  stx SP8
  jmp nextInstruction
  
  .else
opcodeMODI:
  jsr utilityMODI
  lda ACCL
  sta (SP)
  jsr incSP
  lda ACCH
  sta (SP)
  jsr incSP
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  jmp nextInstruction
  .endif
  
  .ifdef STACK8
opcodeDIVI:
  
  ldx SP8
  dex
  lda HopperValueStack, X ; TOP MSB
  sta TOPH
  dex
  lda HopperValueStack, X ; TOP LSB
  sta TOPL
  dex
  lda HopperValueStack, X ; NEXT MSB
  sta NEXTH
  dex
  lda HopperValueStack, X ; NEXT LSB
  sta NEXTL
  stx SP8
  
  jsr utilityDIVI
  
  lda #tInt
  jmp pushNEXTExit
  
  .else
opcodeDIVI:
  jsr utilityDIVI
  
  lda #tInt
  jmp pushNEXTExit
  .endif
  

  .ifdef STACK8

opcodeBOOLOR:
  
  ; BOOL: assume bools are 0 or 1 so only look at LSB
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tBool
  sta HopperTypeStack, Y
  
  .ifdef CHECKED
  lda HopperValueStack, X
  jsr verifyZeroOrOne
  lda HopperValueStack, Y
  jsr verifyZeroOrOne
  .endif
  
  lda HopperValueStack, X
  ora HopperValueStack, Y
  sta HopperValueStack, Y
  iny
  lda #0
  sta HopperValueStack, Y
  
  .ifdef CHECKED
  inx
  lda HopperValueStack, X
  jsr verifyZero
  lda HopperValueStack, Y
  jsr verifyZero
  .endif
  
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
   
  .else
  
opcodeBOOLOR:
  
  ; BOOL: assume bools are 0 or 1 so only look at LSB
  
  .ifdef CHECKED
  lda NEXTL
  jsr verifyZeroOrOne
  lda TOPL
  jsr verifyZeroOrOne
  lda NEXTH
  jsr verifyZero
  lda TOPH
  jsr verifyZero
  .endif
  
  lda NEXTL
  ora TOPL
  sta (SP)
  jsr incSP
  lda #0
  sta (SP)
  jsr incSP
  
  lda #tBool
  sta (TSP)
  jsr incTSP
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  .endif

  .ifdef STACK8

opcodeBOOLAND:

  ; BOOL: assume bools are 0 or 1 so only look at LSB
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tBool
  sta HopperTypeStack, Y
  
  .ifdef CHECKED
  lda HopperValueStack, X
  jsr verifyZeroOrOne
  lda HopperValueStack, Y
  jsr verifyZeroOrOne
  .endif
  
  lda HopperValueStack, X
  and HopperValueStack, Y
  sta HopperValueStack, Y
  
  iny
  
  .ifdef CHECKED
  inx
  lda HopperValueStack, X
  jsr verifyZero
  lda HopperValueStack, Y
  jsr verifyZero
  .endif
  
  lda #0
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
   
  .else

opcodeBOOLAND:
  ; BOOL: assume bools are 0 or 1 so only look at LSB
  
  .ifdef CHECKED
  lda NEXTL
  jsr verifyZeroOrOne
  lda TOPL
  jsr verifyZeroOrOne
  lda NEXTH
  jsr verifyZero
  lda TOPH
  jsr verifyZero
  .endif
  
  lda NEXTL
  and TOPL
  sta (SP)
  jsr incSP
  lda #0
  sta (SP)
  jsr incSP
  
  lda #tBool
  sta (TSP)
  jsr incTSP
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  .endif
  
  .ifdef STACK8

opcodeBITAND:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tUInt
  sta HopperTypeStack, Y
  lda HopperValueStack, X
  and HopperValueStack, Y
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, X
  and HopperValueStack, Y
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif

  jmp nextInstruction
   
  .else
  
opcodeBITAND:
  
  ; LSB
  lda NEXTL
  and TOPL
  sta (SP)
  jsr incSP
  ; MSB
  lda NEXTH
  and TOPH
  sta (SP)
  jsr incSP
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  jmp nextInstruction
  
  .endif
  
  .ifdef STACK8

opcodeBITOR:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  lda #tUInt
  sta HopperTypeStack, Y
  lda HopperValueStack, X
  ora HopperValueStack, Y
  sta HopperValueStack, Y
  inx
  iny
  lda HopperValueStack, X
  ora HopperValueStack, Y
  sta HopperValueStack, Y
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, Y ; error marker
  .endif

  jmp nextInstruction
   
  .else
  
opcodeBITOR:
  
  ; LSB
  lda NEXTL
  ora TOPL
  sta (SP)
  jsr incSP
  ; MSB
  lda NEXTH
  ora TOPH
  sta (SP)
  jsr incSP
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  
  jmp nextInstruction
  .endif
  
  .ifdef STACK8

opcodeBITSHL:
  ; TOP = NEXT << TOP
  dec SP8
  dec SP8
  ldy SP8
  lda HopperValueStack, Y
  tax ; TOP
  
  dey
  lda HopperValueStack, Y ; NEXT MSB
  sta NEXTH  
  dey
  lda HopperValueStack, Y ; NEXT LSB
  sta NEXTL
  lda #tUInt
  sta HopperTypeStack, Y
  
nextShiftL:
  cpx #0 ; TOP
  beq doneShiftingL
  
  ; LSB
  asl NEXTL
  ; MSB
  rol NEXTH
  
  dex
  bra nextShiftL
  
doneShiftingL:
  
  lda NEXTL 
  sta HopperValueStack, Y ; TOP LSB
  iny
  lda NEXTH
  sta HopperValueStack, Y ; TOP MSB
  
  jmp nextInstruction
  
  .else
  
opcodeBITSHL:
  ; TOP = NEXT << TOP
  
nextShiftL:
  lda TOPL
  beq doneShiftingL
  
  ; LSB
  asl NEXTL
  ; MSB
  rol NEXTH
  
  dec TOPL
  bra nextShiftL
  
doneShiftingL:
  
  lda #tUInt
  jmp pushNEXTExit
  .endif
  
  .ifdef STACK8

opcodeBITSHR:
  ; TOP = NEXT >> TOP
  dec SP8
  dec SP8
  ldy SP8
  lda HopperValueStack, Y
  tax ; TOP
  
  dey
  lda HopperValueStack, Y ; NEXT MSB
  sta NEXTH  
  dey
  lda HopperValueStack, Y ; NEXT LSB
  sta NEXTL
  lda #tUInt
  sta HopperTypeStack, Y
  
nextShiftR:
  cpx #0
  beq doneShiftingR
  
  ; MSB
  lsr NEXTH
  ; LSB
  ror NEXTL
  
  dex
  bra nextShiftR
  
doneShiftingR:

  lda NEXTL 
  sta HopperValueStack, Y ; TOP LSB
  iny
  lda NEXTH
  sta HopperValueStack, Y ; TOP MSB
  
  jmp nextInstruction
  
   
  .else
  
opcodeBITSHR:
  ; TOP = NEXT >> TOP
  
nextShiftR:
  lda TOPL
  beq doneShiftingR
  
  ; MSB
  lsr NEXTH
  ; LSB
  ror NEXTL
  
  dec TOPL
  bra nextShiftR
  
doneShiftingR:
  
  lda #tUInt
  jmp pushNEXTExit
  .endif
  
  .ifdef HEAP
opcodeCOPYNEXTPOP:
  lda #1
  sta COPYNEXTPOP
  jmp nextInstruction
  .endif
  
  .ifdef STACK8

opcodeENTERB:
  ; push BP on CALL STACK
  ldx CSP
  lda BP8
  sta HopperCallStack, X
  inx
  stz HopperCallStack, X ; placeholder zero for MSB
  inx
  stx CSP
  
  ; BP = SP
  lda SP8
  sta BP8
  
    ; PC++
  inc PCL
  bne incPCopcodeENTERB
  inc PCH
incPCopcodeENTERB:
  lda (PC) ; number of zeros to push
  tay
  
  ldx SP8
  
opcodeENTERBNext:
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
  
  dey
  bne opcodeENTERBNext
  
  stx SP8
  
  jmp nextInstruction
  
opcodeENTER:

  ; push BP on CALL STACK
  ldx CSP
  lda BP8
  sta HopperCallStack, X
  inx
  stz HopperCallStack, X ; placeholder zero for MSB
  inx
  stx CSP
  
  ; BP = SP
  lda SP8
  sta BP8
  
  jmp nextInstruction
  
  .else
  
opcodeENTERB:
  ; push BP on CALL STACK
  ldx CSP
  lda BPL
  sta HopperCallStack, X
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  
  lda BPH
  sta HopperCallStack, X
  
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  stx CSP
  
  ; BP = SP
  lda SPL
  sta BPL
  lda SPH
  sta BPH
  
  ; PC++
  inc PCL
  bne incPCopcodeENTERB
  inc PCH
incPCopcodeENTERB:
  
  lda (PC) ; number of zeros to push
  tay
  
  
opcodeENTERBNext:
  lda #0
  
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
  
  dey
  bne opcodeENTERBNext
  
  
  
  jmp nextInstruction
  
opcodeENTER:
  
  ; push BP on CALL STACK
  ldx CSP
  lda BPL
  sta HopperCallStack, X
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  
  lda BPH
  sta HopperCallStack, X
  
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  stx CSP
  
  ; BP = SP
  lda SPL
  sta BPL
  lda SPH
  sta BPH
  
  jmp nextInstruction
  .endif
  
  .ifdef STACK8

opcodeDECSP:
  ; PC++
  inc PCL
  bne incPCopcodeDECSP
  inc PCH
incPCopcodeDECSP:
  
  lda (PC)  ; SP -= operand
  
clearNext2:
  beq decspDone ; done, no more to clear
  dec SP8
  dec
  dec SP8
  dec
  pha
  
  ldx SP8
  lda HopperTypeStack, X
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodeDECSPNoRelease
  .ifdef HEAP
  jsr releaseSP
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodeDECSPNoRelease:

  pla  
  jmp clearNext2
decspDone:
  jmp nextInstruction

  .else
  
opcodeDECSP:
  ; PC++
  inc PCL
  bne incPCopcodeDECSP
  inc PCH
incPCopcodeDECSP:
  
  lda (PC)  ; SP -= operand
  
clearNext2:
  beq decspDone ; done, no more to clear
  jsr decSP ; SP--
  dec
  jsr decSP ; SP--
  dec
  jsr decTSP
  pha
  
  lda (TSP)
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodeDECSPNoRelease
  .ifdef HEAP
  jsr releaseSP
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodeDECSPNoRelease:

  pla  
  jmp clearNext2
decspDone:
  jmp nextInstruction
  
  .endif
  
opcodeRETB:
  ; PC++
  inc PCL
  bne incPCopcodeRETB
  inc PCH
incPCopcodeRETB:

  lda (PC)  ; number of bytes of locals and arguments to clear from stack
  
clearNext0:
  cmp #0
  bne opcodeRETBNotDone
  jmp opcodeRET0 ; done, no more to clear
opcodeRETBNotDone  
  dec
  dec
  .ifdef STACK8
  dec SP8
  dec SP8
  .else
  jsr decSP ; SP--
  jsr decSP ; SP--
  jsr decTSP
  .endif
  
  ; check if it needs gcRelease
  pha
  
  .ifdef STACK8
  ldx SP8
  lda HopperTypeStack, X
  .else
  lda (TSP)
  .endif
  
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodeRETBValueType
  
  .ifdef HEAP
  jsr releaseSP
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodeRETBValueType:
  pla
  
  jmp clearNext0
  
  
opcodeRETRETB:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta TOPH
  dex
  lda HopperValueStack, X
  sta TOPL
  lda HopperTypeStack, X
  stx SP8
  
  .else
  
  jsr decSP
  lda (SP)
  sta TOPH
  jsr decSP
  lda (SP)
  sta TOPL
  jsr decTSP
  lda (TSP)

  .endif
  
  pha
  
  
  ; PC++
  inc PCL
  bne incPCopcodeRETRETB
  inc PCH
incPCopcodeRETRETB:
  lda (PC)  ; number of bytes of locals and arguments to clear from stack
  
clearNext1:
  cmp #0
  beq pushTOP ; done, no more to clear
  
  .ifdef STACK8
  
  dec SP8
  dec SP8
  
  .else
  
  jsr decSP ; SP--
  jsr decSP ; SP--
  jsr decTSP
  
  .endif
  
  dec
  dec
  
  ; check if it needs gcRelease
  pha
  
  .ifdef STACK8
  
  ldx SP8
  lda HopperTypeStack, X
  
  .else
  
  lda (TSP)
  
  .endif
  
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodeRETRETBValueType
  
  .ifdef HEAP
  jsr releaseSP
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodeRETRETBValueType:
  pla
  
  jmp clearNext1
pushTOP:

  .ifdef STACK8
  ldx SP8
  lda TOPL
  sta HopperValueStack, X
  pla
  sta HopperTypeStack, X
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
  lda TOPL
  sta (SP);
  jsr incSP
  lda TOPH
  sta (SP);
  jsr incSP
  
  pla
  sta (TSP)
  jsr incTSP
  .endif
  
  ; fall through to RET0
  
  .ifdef STACK8
  
opcodeRET0:
  ; pop BP from CALL STACK
  ldx CSP
  dex
  dex
  lda HopperCallStack, X  ; LSB
  sta BP8
  stx CSP
  
  ; if CSP == 0 then we are exiting main(..)
  cpx #0
  bne haveReturn
  
  .ifdef CHECKED
  pha
  ; CSP is 0x0500

  jsr memoryAvailable

  ; expect HopperHeapSize-2  
  stz IDXL
  lda HEAPSIZE
  sta IDXH
  jsr decIDX
  jsr decIDX
  
  lda IDXL
  cmp ACCL
  bne memoryLeaks
  lda IDXH
  cmp ACCH
  bne memoryLeaks
  
  jsr memoryMaximum 
  
  ; expect HopperHeapSize-2  
  stz IDXL
  lda HEAPSIZE
  sta IDXH
  jsr decIDX
  jsr decIDX
  
  lda IDXL
  cmp ACCL
  bne memoryLeaks
  lda IDXH
  cmp ACCH
  bne memoryLeaks
  
  bra memoryIsReleased
  
memoryLeaks:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutNewLine
  lda ACCH
  jsr diagnosticOutHex
  lda ACCL
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?memoryLeaks", 0
  .endif
  jmp throwToys
   
memoryIsReleased:
  pla
  .endif
  
  jmp monitorReEntryResetPC ; good clean exit from 'main'
  
haveReturn:
  ; pop PC from CALL STACK
  ldx CSP
  dex
  lda HopperCallStack, X  ; return address MSB
  sta PCH
  dex
  lda HopperCallStack, X  ; return address LSB
  sta PCL
  stx CSP
  jmp nextInstructionNoInc
  
  .else
  
opcodeRET0:
  ; pop BP from CALL STACK
  ldx CSP
  .ifdef CHECKED
  jsr decCSP
  ldx CSP
  .else
  dex
  .endif

  lda HopperCallStack, X  ; MSB
  sta BPH
  
  .ifdef CHECKED
  jsr decCSP
  ldx CSP
  .else
  dex
  .endif
  lda HopperCallStack, X  ; LSB
  
  sta BPL
  stx CSP
  
  ; if CSP == 0 then we are exiting main(..)
  cpx #0
  bne haveReturn
  
  .ifdef CHECKED
  pha
  ; CSP is 0x0500

  jsr memoryAvailable

  ; expect HopperHeapSize-2  
  stz IDXL
  lda HEAPSIZE
  sta IDXH
  jsr decIDX
  jsr decIDX
  
  lda IDXL
  cmp ACCL
  bne memoryLeaks
  lda IDXH
  cmp ACCH
  bne memoryLeaks
  
  jsr memoryMaximum 
  
  ; expect HopperHeapSize-2  
  stz IDXL
  lda HEAPSIZE
  sta IDXH
  jsr decIDX
  jsr decIDX
  
  lda IDXL
  cmp ACCL
  bne memoryLeaks
  lda IDXH
  cmp ACCH
  bne memoryLeaks
  
  bra memoryIsReleased
  
memoryLeaks:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutNewLine
  lda ACCH
  jsr diagnosticOutHex
  lda ACCL
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?memoryLeaks", 0
  .endif
  jmp throwToys
   
memoryIsReleased:
  pla
  .endif
  
  jmp monitorReEntryResetPC ; good clean exit from 'main'
  
haveReturn:
  ; pop PC from CALL STACK
  ldx CSP
  .ifdef CHECKED
  jsr decCSP
  ldx CSP
  .else
  dex
  .endif
  lda HopperCallStack, X  ; return address MSB
  sta PCH
  .ifdef CHECKED
  jsr decCSP
  ldx CSP
  .else
  dex
  .endif
  lda HopperCallStack, X  ; return address LSB
  sta PCL
  stx CSP
  jmp nextInstructionNoInc
  .endif

  .ifdef STACK8
  
opcodeTESTBPB:
  ; we expect BP == SP - operand
  jsr incPC ; PC++
  sec
  lda SP8
  sbc (PC)
  cmp BP8
  bne badBP
  jmp nextInstruction
badBP:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Bad SP", 0
  jmp monitorReEntry
  .else
  jmp throwToys
  .endif
  
  .else
  
opcodeTESTBPB:
  ; we expect BP == SP - operand
  jsr incPC ; PC++
  sec
  lda SPL
  sbc (PC)
  sta ACCL
  lda SPH
  sbc #0    ; MSB of operand is zero since it was a byte
  ;sta ACCH
  
  ;lda ACCH
  cmp BPH
  bne badBP
  lda ACCL
  cmp BPL
  bne badBP
  jmp nextInstruction
badBP:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Bad SP", 0
  jmp monitorReEntry
  .else
  jmp throwToys
  .endif
  .endif
  
opcodeCALLREL:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta ACCH
  dex
  lda HopperValueStack, X
  sta ACCL
  stx SP8
  
  .else

  jsr decSP
  lda (SP)
  sta ACCH
  jsr decSP
  lda (SP)
  sta ACCL
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertDelegate
  .endif
  .endif
  
  ; PC++ - return address is next instruction
  inc PCL
  bne incPCopcodeCALLREL
  inc PCH
incPCopcodeCALLREL:
  
  ; method table index is in ACC
  
  ; push PC to CALL STACK
  ldx CSP
  lda PCL
  sta HopperCallStack, X ; LSB
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  lda PCH
  sta HopperCallStack, X  ; MSB
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  stx CSP
  
  lda #<HopperMethodTable
  sta IDXL
  lda #>HopperMethodTable
  sta IDXH
  
methodHunt2:
  ldy #1
  
  lda (IDX)
  cmp ACCL
  bne nextMethod2
  lda (IDX), Y
  bne nextMethod2
  
  ; we have a winner
  iny
  lda (IDX), Y
  sta PCL
  iny
  lda (IDX), Y
  sta PCH
  
  clc
  lda PCL
  adc #<HopperData
  sta PCL
  lda PCH
  adc #>HopperData
  sta PCH
  
  jmp nextInstructionNoInc
nextMethod2:
  clc
  lda IDXL
  adc #4
  sta IDXL
  bcc jumpMethodHunt2
  inc IDXH
jumpMethodHunt2:
  jmp methodHunt2

  
opcodeCALLW:
  ; PC++
  inc PCL
  bne incPCopcodeCALLW0
  inc PCH
incPCopcodeCALLW0:
  lda (PC)  ; index into method table
  sta ACCL
  ; PC++
  inc PCL
  bne incPCopcodeCALLW1
  inc PCH
incPCopcodeCALLW1:
  lda (PC)
  sta ACCH
  
  ; PC++ - return address is next instruction
  inc PCL
  bne incPCopcodeCALLW2
  inc PCH
incPCopcodeCALLW2:
  
  ; push PC to CALL STACK
  ldx CSP
  lda PCL
  sta HopperCallStack, X ; LSB
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  lda PCH
  sta HopperCallStack, X  ; MSB
  .ifdef CHECKED
  jsr incCSP
  ldx CSP
  .else
  inx
  .endif
  stx CSP
  
  lda ACCH
  ;and #$C0
  ;cmp #$C0
  ;beq opcodeCALLWlookupMethod
  bit ACCH
  bmi opcodeCALLWlookupMethod
  
opcodeCALLWlookupMethodFalseAlarm:
  ; absolute address is in ACC
  ;lda ACCH
  sta PCH
  lda ACCL
  sta PCL
  jmp nextInstructionNoInc
  
opcodeCALLWlookupMethod:
  bvc opcodeCALLWlookupMethodFalseAlarm ; only lookup if both bits set

  ; method table index is in bottom 14 bits of ACC
  lda ACCH
  and #$3F
  sta ACCH
  
  lda #<HopperMethodTable
  sta IDXL
  lda #>HopperMethodTable
  sta IDXH
  
methodHunt:
  ldy #1
  
  lda (IDX)
  cmp ACCL
  bne nextMethod
  lda (IDX), Y
  cmp ACCH
  bne nextMethod
  
  ; we have a winner
  
  lda PCL
  sta IDYL
  lda PCH
  sta IDYH
  jsr decIDY
  jsr decIDY
  
  iny
  lda (IDX), Y
  sta PCL
  iny
  lda (IDX), Y
  sta PCH
  
  ldy #1
  clc
  lda PCL
  adc #<HopperData
  sta PCL
  sta (IDY)
  lda PCH
  adc #>HopperData
  sta PCH
  sta (IDY), Y
  
  jmp nextInstructionNoInc
nextMethod:
  clc
  lda IDXL
  adc #4
  sta IDXL
  bcc jumpMethodHunt
  inc IDXH
jumpMethodHunt:
  jmp methodHunt

  .ifndef STACK8
nextConditionalJCleanupC: ; used by B conditional jumps
  jsr decSP ; SP--
  .endif
nextConditionalJCleanupD: ; used by B conditional jumps
  jsr incPC ; PC++
  jmp nextInstruction



  .ifdef STACK8


opcodeJNZB:

  dec SP8
  dec SP8
  ldx SP8
  
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUIntOrInt
  .endif

  lda HopperValueStack, X  ; LSB
  bne opcodeJB ; not zero
  inx
  lda HopperValueStack, X  ; MSB
  bne opcodeJB ; not zero
  
  ; zero
  bra nextConditionalJCleanupD ; : PC++, jmp nextInstruction
  
opcodeJZB:

  dec SP8
  dec SP8
  ldx SP8
  
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUIntOrInt
  .endif

  lda HopperValueStack, X  ; LSB
  bne nextConditionalJCleanupD ; not Zero: PC++, jmp nextInstruction
  inx
  lda HopperValueStack, X  ; MSB
  bne nextConditionalJCleanupD ; not Zero: PC++, jmp nextInstruction

  .else ; STACK8

opcodeJNZB:

  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrInt
  .endif

  jsr decSP
  lda (SP)  ; MSB
  pha
  jsr decSP
  pla
  bne opcodeJB ; not zero
  lda (SP)  ; LSB
  bne opcodeJB ; not zero
  
  ; zero
  bra nextConditionalJCleanupD
  
opcodeJZB:
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUIntOrInt
  .endif

  jsr decSP
  lda (SP)  ; MSB
  bne nextConditionalJCleanupC ; not Zero
  jsr decSP
  lda (SP)  ; LSB
  bne nextConditionalJCleanupD ; not Zero
  
  .endif ; !STACK8
  
  ; fall through to JB

opcodeJB:
  ; PC++
  inc PCL
  bne incPCopcodeJBEnd
  inc PCH
incPCopcodeJBEnd:
  
  lda (PC)  ; offset
  
  bpl jbPositive
  ; offset >= 128
  ; PC -= 256
  dec PCH
  dec
  ; PC += offset
  clc
  adc PCL
  sta PCL
  bcc jbEnd
  inc PCH
  bra jbEnd
  
jbPositive:
  ; PC += offset
  clc
  adc PCL
  sta PCL
  bcc jbInc
  inc PCH
jbInc:
  jsr decPC ; -1 for positive
  
jbEnd:
  jmp nextInstructionNoInc ; TODO PERF


nextConditionalJCleanup: ; used by W conditional jumps
  .ifdef CHECKED
  jsr incPC ; PC++
  jsr incPC ; PC++
  .else
  
  clc
  lda PCL  ; LSB
  adc #2
  sta PCL
  lda PCH
  adc #0
  sta PCH
  
  .endif
  jmp nextInstruction

opcodeJNZW:

  .ifdef STACK8
  
  dec SP8
  dec SP8
  ldx SP8
  
  .else ; STACK8
  
  .ifdef CHECKED
  jsr decTSP
  lda (TSP)
  jsr assertUIntOrInt
  
  jsr decSP
  jsr decSP
  
  .else 
  
  lda TSPL
  bne opcodeJNZWdecTSPSkipMSB
  dec TSPH
opcodeJNZWdecTSPSkipMSB:
  dec TSPL
  
  ; decSP x2
  sec
  lda SPL
  sbc #2
  sta SPL
  bcs opcodeJNZWSkipMSB
  dec SPH
opcodeJNZWSkipMSB:
  
  .endif
  .endif ; !STACK8
  
  .ifdef STACK8
  lda HopperValueStack, X ; LSB
  .else
  ldy #1
  lda (SP), Y   ; MSB
  .endif
  
  beq tryLSB ; zero
  ; not zero
  jmp opcodeJW
tryLSB:
  .ifdef STACK8
  inx
  lda HopperValueStack, X ; MSB
  .else
  lda (SP)  ; LSB
  .endif
  beq nextConditionalJCleanup ; zero
  ; not zero
  jmp opcodeJW

  .ifdef STACK8
  
opcodeJZW:
  
  dec SP8
  dec SP8
  
  ldx SP8
  
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUIntOrInt
  .endif
  
  lda HopperValueStack, X
  bne nextConditionalJCleanup ; not Zero
  inx
  lda HopperValueStack, X
  bne nextConditionalJCleanup ; not Zero
  
  ; Zero
  bra opcodeJW
  
  .else ; STACK8

opcodeJZW:
  
  .ifdef CHECKED
  
  jsr decTSP
  jsr decSP
  jsr decSP
  
  lda (TSP)
  jsr assertUIntOrInt
  
  .else
  
  lda TSPL
  bne opcodeJZWdecTSPSkipMSB
  dec TSPH
opcodeJZWdecTSPSkipMSB:
  dec TSPL
  
  ; decSP x2
  sec
  lda SPL
  sbc #2
  sta SPL
  bcs opcodeJZWSkipMSB
  dec SPH
opcodeJZWSkipMSB:
  
  .endif ; .ifndef CHECKED
  
  ldy #1
  lda (SP), Y  ; MSB
  bne nextConditionalJCleanup ; not Zero
  lda (SP)  ; LSB
  bne nextConditionalJCleanup ; not Zero
  
  ; Zero
  
  ; fall through to JW
  .endif ; !STACK8
  
opcodeJW:
  ; TODO - negative offset:
  ldy #2
  lda (PC), Y  ; offset MSB
  tax
  dey
  lda (PC), Y  ; offset LSB
  
  ; PC += offset
  clc
  adc PCL
  sta PCL

  txa
  adc PCH
  sta PCH
  
  jmp nextInstructionNoInc
  
  

opcodeBOOLNOT:

  ; BOOL: this works for all integer values: 0 -> 1,  (!0) -> 0
  .ifdef STACK8
  
  lda SP8
  sta IDXL
  sta IDYL
  lda #>HopperValueStack
  sta IDXH
  lda #>HopperTypeStack
  sta IDYH
  
  jsr decIDY ; TSP-1
  
  .else
  
  lda SPL
  sta IDXL
  lda SPH
  sta IDXH
  
  lda TSPL
  sta IDYL
  lda TSPH
  sta IDYH
  
  .endif
  
  jsr decIDX ; SP-1
  jsr decIDX ; SP-2
  
  jsr decIDY ; TSP-1
  
  ldy #1
  lda (IDX)
  bne bnNotZero
  lda (IDX), Y
  bne bnNotZero
  ; zero
  lda #1
  sta (IDX)
  bra bnDone
bnNotZero:
  ; not zero
  lda #0
  sta (IDX)
  sta (IDX), Y
bnDone:

  lda #tBool
  sta (IDY)

  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
opcodeSWAP:
  
  ; (SP-1) <-> (SP-3)
  ; (SP-2) <-> (SP-4)
  .ifdef STACK8
  ; (TSP-2) <-> (TSP-4)
  .else
  ; (TSP-1) <-> (TSP-2)
  .endif
  
  .ifdef STACK8
  lda SP8
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  .else
  lda SPL
  sta IDXL
  lda SPH
  sta IDXH
  .endif
  
  jsr decIDX ; SP-1
  jsr decIDX ; SP-2
  lda IDXL
  sta IDYL
  lda IDXH
  sta IDYH
  jsr decIDX ; SP-3
  jsr decIDX ; SP-4
  
  ; IDY = SP-2
  ; IDX = SP-4
  ldy #0
  ; (SP-2) <-> (SP-4)
  lda (IDX), Y ; SP-4
  tax
  lda (IDY), Y ; SP-2
  sta (IDX), Y ; SP-4
  txa
  sta (IDY), Y ; SP-2
  ; (SP-1) <-> (SP-3)
  iny
  lda (IDX), Y ; SP-3
  tax
  lda (IDY), Y ; SP-1
  sta (IDX), Y ; SP-3
  txa
  sta (IDY), Y ; SP-1
  
  .ifdef STACK8
  lda SP8
  sta IDXL
  lda #>HopperTypeStack
  sta IDXH
  .else
  lda TSPL
  sta IDXL
  lda TSPH
  sta IDXH
  .endif
  
  .ifdef STACK8
  
  ; IDX = TSP-4
  jsr decIDX
  jsr decIDX
  jsr decIDX
  jsr decIDX
  
  ; (TSP-2) <-> (TSP-4)
  ldy #2
  lda (IDX)    ; TSP-4
  tax
  lda (IDX), Y ; TSP-2
  sta (IDX)    ; TSP-4
  txa
  sta (IDX), Y ; TSP-2
  
  .else
  
  ; IDX = TSP-2
  jsr decIDX
  jsr decIDX
  
  
  ; (TSP-1) <-> (TSP-2)
  ldy #1
  lda (IDX)    ; TSP-2
  tax
  lda (IDX), Y ; TSP-1
  sta (IDX)    ; TSP-2
  txa
  sta (IDX), Y ; TSP-1
  
  .endif
  
  jmp nextInstruction

opcodeDUP:

  
  
  ; IDX = SP
  .ifdef STACK8
  
  lda SP8
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  
  .else
  
  lda SPL
  sta IDXL
  lda SPH
  sta IDXH
  
  .endif
  
  ; localAddress = ((SP - 2) - operand); // DUP 0 implies duplicating [top]
  
  ; IDX--
  lda IDXL
  bne dumpSkipMSB0
  dec IDXH
dumpSkipMSB0:
  dec IDXL
  
  ; IDX--
  lda IDXL
  bne dumpSkipMSB1
  dec IDXH
dumpSkipMSB1:
  dec IDXL
  
  ; IDX -= operand
  jsr incPC ; PC++
  sec
  lda IDXL
  sbc (PC)
  sta IDXL
  bcs opcodeDUPSkipMSB
  dec IDXH
opcodeDUPSkipMSB:
  
  ; DUP
  .ifdef STACK8
  
  ldx SP8
  lda (IDX)
  pha
  sta HopperValueStack,X
  
  lda #>HopperTypeStack
  sta IDXH
  lda (IDX)
  sta HopperTypeStack,X
  sta fTYPE
  lda #>HopperValueStack
  sta IDXH
  
  inx
  ldy #1
  lda (IDX), Y
  pha
  sta HopperValueStack,X
  inx
  stx SP8
  
  .else
  
  lda (IDX)
  pha
  sta (SP)
  ldy #1
  lda (IDX), Y
  pha
  sta (SP), Y
  jsr incSP
  jsr incSP
  
  ; mimic the above for TSP
  ; IDX = TSP
  lda TSPL
  sta IDXL
  lda TSPH
  sta IDXH
  ; localAddress = ((TSP - 1) - operand / 2); // DUP 0 implies duplicating [top]
  
  ; IDX--
  lda IDXL
  bne dumpSkipMSB2
  dec IDXH
dumpSkipMSB2:
  dec IDXL
  
  ; IDX -= operand/2
  lda (PC)
  lsr
  sta ACCL
  sec
  lda IDXL
  sbc ACCL
  sta IDXL
  lda IDXH
  sbc #0    ; MSB of operand is zero since it was a byte
  sta IDXH
  
  ; DUP
  lda (IDX)
  sta (TSP)
  sta fTYPE
  jsr incTSP
  
  .endif
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  lda fTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc opcodeDUPNoAddReference
  .ifdef HEAP
  ; address in IDX
  jsr gcAddReference
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
opcodeDUPNoAddReference:
  
  jmp nextInstruction



  .ifdef STACK8
  
opcodeINCGLOBALB:
  
  ; PC++
  inc PCL
  bne incPCopcodeINCGLOBALB
  inc PCH
incPCopcodeINCGLOBALB:
  lda (PC)  ; offset 0..255
  tax
  
  inc HopperValueStack, X
  bne opcodeINCGLOBALBDone
  inx
  inc HopperValueStack, X
opcodeINCGLOBALBDone:

  jmp nextInstruction
  
  
opcodeDECGLOBALB:
  ; PC++
  inc PCL
  bne incPCopcodeDECGLOBALB
  inc PCH
incPCopcodeDECGLOBALB:
  lda (PC)  ; offset 0..255
  tax
  
  lda HopperValueStack, X
  bne opcodeDECGLOBALSkipMSB
  inx
  dec HopperValueStack, X
  dex
opcodeDECGLOBALSkipMSB:
  dec HopperValueStack, X

  jmp nextInstruction
 
opcodeINCLOCALB:  
  ; PC++
  inc PCL
  bne incPCopcodeINCLOCALB
  inc PCH
incPCopcodeINCLOCALB:

  lda (PC)  ; offset
  clc
  adc BP8
  tax
  inc HopperValueStack, X ; LSB
  bne opcodeINCLOCALBDone
  inx
  inc HopperValueStack, X ; MSB
opcodeINCLOCALBDone:

  jmp nextInstruction

opcodeINCLOCALBB:  

  ; PC++
  inc PCL
  bne incPCopcodeINCLOCALBB
  inc PCH
incPCopcodeINCLOCALBB:

  lda (PC)  ; offset
  clc
  adc BP8
  tax
  
  ; PC++
  inc PCL
  bne incPCopcodeINCLOCALBB1
  inc PCH
incPCopcodeINCLOCALBB1:

  lda (PC)  ; offset
  clc
  adc BP8
  tay
  
  ; (X) = (X) + (Y)
  clc
  lda HopperValueStack, X
  adc HopperValueStack, Y
  sta HopperValueStack, X
  inx
  iny
  lda HopperValueStack, X
  adc HopperValueStack, Y
  sta HopperValueStack, X
  
  jmp nextInstruction
  
 
  
  .else
  

opcodeINCGLOBALB:
  
; IDX = HopperValueStack
  lda #<HopperValueStack
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  
  jsr incPC ; PC++
  lda (PC)  ; offset 0..255
  
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc incgbEnd
  inc IDXH
incgbEnd:

  lda (IDX)
  inc
  sta (IDX)
  bne incgbDone
  ldy #1
  lda (IDX), Y
  inc
  sta (IDX), Y
incgbDone:

  jmp nextInstruction

opcodeDECGLOBALB:
  
; IDX = HopperValueStack
  lda #<HopperValueStack
  sta IDXL
  lda #>HopperValueStack
  sta IDXH
  
  jsr incPC ; PC++
  lda (PC)  ; offset 0..255
  
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc decgbEnd
  inc IDXH
decgbEnd:

  lda (IDX)
  bne decgbSkipMSB
  ldy #1
  lda (IDX), Y
  dec
  sta (IDX), Y
decgbSkipMSB:
  lda (IDX)
  dec
  sta (IDX)
  
  jmp nextInstruction


opcodeINCLOCALB:
  ; IDX = BP
  
  lda BPL
  sta IDXL
  lda BPH
  sta IDXH
  
  ; TODO - test positive offset:
  jsr incPC ; PC++
  lda (PC)  ; offset
  
  ; IDX += offset
  
  bpl inclPositive
  ; offset >= 128
  ; -= 256
  dec IDXH
inclPositive:
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc inclEnd
  inc IDXH
inclEnd:

  lda (IDX)
  inc
  sta (IDX)
  bne inclDone
  ldy #1
  lda (IDX), Y
  inc
  sta (IDX), Y
inclDone:

  jmp nextInstruction
  .endif

  .ifdef STACK8
  
opcodeDECLOCAL:
; PC++
  inc PCL
  bne incPCopcodeDECLOCAL
  inc PCH
incPCopcodeDECLOCAL:

  lda (PC)  ; offset
  clc
  adc BP8
  tax
  
  lda HopperValueStack, X
  bne opcodeDECLOCALSkipMSB
  inx
  dec HopperValueStack, X
  dex
opcodeDECLOCALSkipMSB:
  dec HopperValueStack, X
  
  jmp nextInstruction
  
  .else

opcodeDECLOCAL:
  
  ; IDX = BP
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
  
  ; TODO - test positive offset:
  jsr incPC ; PC++
  lda (PC)  ; offset
  
  ; IDX += offset
  
  bpl declPositive
  ; offset >= 128
  ; -= 256
  dec IDXH
declPositive:
  ; += offset
  clc
  adc IDXL
  sta IDXL
  bcc declEnd
  inc IDXH
declEnd:

  lda (IDX)
  bne declSkipMSB
  ldy #1
  lda (IDX), Y
  dec
  sta (IDX), Y
declSkipMSB:
  lda (IDX)
  dec
  sta (IDX)
  jmp nextInstruction
  .endif


  .ifdef STACK8
  
opcodeLE:

  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  inx
  
  ; NEXT(Y) <= TOP(X)? 
  lda HopperValueStack, Y ; NEXT MSB
  cmp HopperValueStack, X ; TOP MSB
  bne doneUIntLE8
  dex
  dey
  lda HopperValueStack, Y ; NEXT LSB
  cmp HopperValueStack, X ; TOP LSB
doneUIntLE8:
  ; http://6502.org/tutorials/compare_instructions.html
  beq phUIntLE8 ; NEXT == TOP (not >)
  bcc phUIntLE8 ; NEXT <  TOP (not >)
  ; NEXT > TOP
  
  ldx SP8
  dex
  stz HopperValueStack, X  ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  dex
  stz HopperValueStack, X  ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
phUIntLE8:
  ; NEXT <= TOP
  ldx SP8
  dex
  stz HopperValueStack, X ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  dex
  lda #1
  sta HopperValueStack, X ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction

  .else
  
opcodeLE:
  jsr utilityUIntLE
  lda #tBool
  jmp pushTOPExit
  .endif
  
  

  .ifndef STACK8

opcodeLT:
  jsr utilityUIntLT
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
  
opcodeGE:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  inx
  
  ; NEXT(Y) >= TOP(X)? 
  lda HopperValueStack, Y ; NEXT MSB
  cmp HopperValueStack, X ; TOP MSB
  bne doneUIntGE8
  dex
  dey
  lda HopperValueStack, Y ; NEXT LSB
  cmp HopperValueStack, X ; TOP LSB
doneUIntGE8:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc phUIntGE8 ; NEXT < TOP
  
  ; NEXT >= TOP
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda #1
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
phUIntGE8:
  ; NEXT < TOP
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  stz HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction

  .else
  
opcodeGE:
  jsr utilityUIntGE
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
  
opcodeEQ:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  inx
  
  ; NEXT(Y) == TOP(X)? 
  lda HopperValueStack, Y ; NEXT MSB
  cmp HopperValueStack, X ; TOP MSB
  bne doneUIntEQ8
  dex
  dey
  lda HopperValueStack, Y ; NEXT LSB
  cmp HopperValueStack, X ; TOP LSB
  bne doneUIntEQ8
  
  ; NEXT == TOP
  ldx SP8
  dex
  stz HopperValueStack, X  ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  dex
  lda #1
  sta HopperValueStack, X  ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
doneUIntEQ8:
  ; NEXT != TOP
  ldx SP8
  dex
  stz HopperValueStack, X  ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  dex
  stz HopperValueStack, X ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction

  .else
  
opcodeEQ:
  jsr utilityUIntEQ
  lda #tBool
  jmp pushTOPExit
  
  .endif
  
  .ifdef STACK8
  
opcodeNE:

  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  inx
  
  ; NEXT(Y) == TOP(X)? 
  lda HopperValueStack, Y ; NEXT MSB
  cmp HopperValueStack, X ; TOP MSB
  bne doneUIntNE8
  dex
  dey
  lda HopperValueStack, Y ; NEXT LSB
  cmp HopperValueStack, X ; TOP LSB
  bne doneUIntNE8
  
  ; NEXT == TOP
  ldx SP8
  dex
  stz HopperValueStack, X  ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  
  dex
  stz HopperValueStack, X  ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
doneUIntNE8:
  ; NEXT != TOP
  ldx SP8
  dex
  stz HopperValueStack, X  ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda #1
  sta HopperValueStack, X ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction

  .else
opcodeNE:
  jsr utilityUIntNE
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
  
opcodeGT:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  inx
  
  ; NEXT(Y) < TOP(X)? 
  lda HopperValueStack, Y ; NEXT MSB
  cmp HopperValueStack, X ; TOP MSB
  bne doneUIntGT8
  dex
  dey
  lda HopperValueStack, Y ; NEXT LSB
  cmp HopperValueStack, X ; TOP LSB
doneUIntGT8:
  ; http://6502.org/tutorials/compare_instructions.html
  beq phUIntGT8 ; NEXT == TOP (not >)
  bcc phUIntGT8 ; NEXT <  TOP (not >)
  ; NEXT > TOP
  
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda #1
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
phUIntGT8:
  ; NEXT <= TOP
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  stz HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction

  .else
  
opcodeGT:
  jsr utilityUIntGT
  lda #tBool
  jmp pushTOPExit
  
  .endif

  .ifdef STACK8
  
opcodeGTI:
  
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  
  ; NEXT > TOP?
  ; NEXT - TOP > 0
  
  sec
  lda HopperValueStack, Y ; NEXT LSB
  sbc HopperValueStack, X ; TOP LSB
  sta HopperValueStack, Y ; NEXT LSB
  iny
  inx
  lda HopperValueStack, Y ; NEXT MSB
  sbc HopperValueStack, X ; TOP MSB
  sta HopperValueStack, Y ; NEXT MSB
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  bcs utilityIntGTNegativeOrZero8
  ; 0 or positive
  lda HopperValueStack, Y ; NEXT MSB
  bne utilityIntGTPositive8
  dey
  lda HopperValueStack, Y ; NEXT LSB
  beq utilityIntGTNegativeOrZero8
utilityIntGTPositive8:  
  ;true
  lda #1
  sta TOPL
utilityIntGTNegativeOrZero8:
  
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda TOPL
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
  .else
opcodeGTI:
  jsr utilityIntGT
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
opcodeGEI:
  
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  
  ; NEXT >= TOP?
  ; NEXT - TOP >= 0
  
  sec
  lda HopperValueStack, Y ; NEXT LSB
  sbc HopperValueStack, X ; TOP LSB
  sta HopperValueStack, Y ; NEXT LSB
  iny
  inx
  lda HopperValueStack, Y ; NEXT MSB
  sbc HopperValueStack, X ; TOP MSB
  sta HopperValueStack, Y ; NEXT MSB
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  bcs utilityIntGENegative8
  
  ; 0 or positive
  ;true
  lda #1
  sta TOPL
  
utilityIntGENegative8:

  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda TOPL
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction

  .else
opcodeGEI:
  jsr utilityIntGE
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
opcodeLEI:
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  
  ; NEXT <= TOP?
  ; TOP - NEXT >= 0
  
  sec
  lda HopperValueStack, X ; TOP LSB
  sbc HopperValueStack, Y ; NEXT LSB
  sta HopperValueStack, X ; TOP LSB
  iny
  inx
  lda HopperValueStack, X ; TOP MSB
  sbc HopperValueStack, Y ; NEXT MSB
  sta HopperValueStack, X ; TOP MSB
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  
  bcs utilityIntLENegative8
  ; 0 or positive
  ;true
  lda #1
  sta TOPL
  
utilityIntLENegative8:

  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda TOPL
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
  .else
opcodeLEI:
  jsr utilityIntLE
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
opcodeLTI:
  
  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  dey
  
  ; NEXT < TOP?
  ; TOP - NEXT > 0
  
  sec
  lda HopperValueStack, X ; TOP LSB
  sbc HopperValueStack, Y ; NEXT LSB
  sta HopperValueStack, X ; TOP LSB
  iny
  inx
  lda HopperValueStack, X ; TOP MSB
  sbc HopperValueStack, Y ; NEXT MSB
  sta HopperValueStack, X ; TOP MSB
  
  asl           ; sign bit into carry
  
  ; false
  stz TOPL
  
  bcs utilityIntLTNegativeOrZero8
  ; 0 or positive
  lda HopperValueStack, X ; TOP MSB
  bne utilityIntLTPositive8
  dex
  lda HopperValueStack, X ; TOP LSB
  beq utilityIntLTNegativeOrZero8
utilityIntLTPositive8: 
  
  ;true
  lda #1
  sta TOPL
  
utilityIntLTNegativeOrZero8:

  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda TOPL
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
  .else
opcodeLTI:
  jsr utilityIntLT
  lda #tBool
  jmp pushTOPExit
  .endif
  
  .ifdef STACK8
  
opcodeLT:

  dec SP8
  dec SP8
  ldx SP8
  ldy SP8
  dey
  inx
  
  ; NEXT(Y) < TOP(X)? 
  lda HopperValueStack, Y ; NEXT MSB
  cmp HopperValueStack, X ; TOP MSB
  bne doneUIntLT8
  dex
  dey
  lda HopperValueStack, Y ; NEXT LSB
  cmp HopperValueStack, X ; TOP LSB
doneUIntLT8:
  ; http://6502.org/tutorials/compare_instructions.html
  bcc phUIntLT8
  
  ; NEXT >= TOP
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  stz HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
phUIntLT8:
  ; NEXT < TOP
  ldx SP8
  dex
  stz HopperValueStack, X   ; MSB
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  dex
  lda #1
  sta HopperValueStack, X   ; LSB
  lda #tBool
  sta HopperTypeStack, X
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
  .endif
  