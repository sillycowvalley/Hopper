; ######################## SysCall lookups ########################
  
syscall0JumpTableLower:
  .ifdef STRINGS
  .word syscallStringNewFromConstant ; 00
  .word syscall0Undefined            ; 01 StringNewFromChar - unused
  .word syscallStringNew             ; 02
  .word syscallStringAppend          ; 03
  .word syscallStringInsertChar      ; 04
  .word syscall0Undefined            ; 05 StringCompare (.hs)
  .word syscallStringLengthGet       ; 06
  .word syscall0Undefined            ; 07 StringEndsWith  (.hs)
  .word syscall0Undefined            ; 08 StringSubstring (.hs)
  .word syscall0Undefined            ; 09 StringReplace   (.hs)
  .word syscallStringGetChar         ; 0A
  .else
  .word syscall0Undefined ; 00
  .word syscall0Undefined            ; 01 StringNewFromChar - unused
  .word syscall0Undefined             ; 02
  .word syscall0Undefined          ; 03
  .word syscall0Undefined      ; 04
  .word syscall0Undefined            ; 05 StringCompare (.hs)
  .word syscall0Undefined       ; 06
  .word syscall0Undefined            ; 07 StringEndsWith  (.hs)
  .word syscall0Undefined            ; 08 StringSubstring (.hs)
  .word syscall0Undefined            ; 09 StringReplace   (.hs)
  .word syscall0Undefined         ; 0A
  
  .endif
  
  .ifdef ARRAYS
  .word syscallArrayNew              ; 0B
  .word syscallArrayCountGet         ; 0C
  .word syscallArrayGetItem          ; 0D
  .word syscallArraySetItem          ; 0E
  .else
  .word syscall0Undefined            ; 0B
  .word syscall0Undefined            ; 0C
  .word syscall0Undefined            ; 0D
  .word syscall0Undefined            ; 0E
  .endif
  
  .ifdef LISTS
  .word syscallListNew               ; 0F
  .word syscallListLengthGet         ; 10
  .word syscallListAppend            ; 11
  .word syscallListInsert            ; 12
  .word syscallListGetItem           ; 13
  .word syscall0Undefined ; 14 ListGetItemAsVariant
  .word syscallListSetItem           ; 15
  .word syscallListClear             ; 16
  .word syscallListRemove            ; 17
  .word syscall0Undefined ; 18 ListContains
  .else
  .word syscall0Undefined            ; 0F
  .word syscall0Undefined            ; 10
  .word syscall0Undefined            ; 11
  .word syscall0Undefined            ; 12
  .word syscall0Undefined            ; 13
  .word syscall0Undefined            ; 14
  .word syscall0Undefined            ; 15
  .word syscall0Undefined            ; 16
  .word syscall0Undefined            ; 17
  .word syscall0Undefined            ; 18
  .endif
  
  .ifdef DICTIONARIES
  
  .word syscallDictionaryNew         ; 19
  .word syscallDictionaryCountGet    ; 1A
  .word syscallDictionarySet         ; 1B
  .word syscallDictionaryContains    ; 1C
  .word syscallDictionaryGet         ; 1D
  .word syscallDictionaryNext ; 1E
  .word syscallDictionaryClear ; 1F
  
  .word syscallPairNew    ; 20
  .word syscall0Undefined ; 21 PairSet
  .word syscallPairKey    ; 22
  .word syscall0Undefined ; 23 PairKeyType
  .word syscallPairValue  ; 24 
  .word syscall0Undefined ; 25 PairValueType
  
  .else
  
  .word syscall0Undefined ; 19
  .word syscall0Undefined ; 1A
  .word syscall0Undefined ; 1B
  .word syscall0Undefined ; 1C
  .word syscall0Undefined ; 1D
  .word syscall0Undefined ; 1E
  .word syscall0Undefined ; 1F
  
  .word syscall0Undefined ; 20
  .word syscall0Undefined ; 21
  .word syscall0Undefined ; 22
  .word syscall0Undefined ; 23
  .word syscall0Undefined ; 24
  .word syscall0Undefined ; 25
  
  
  .endif
  .word syscall0Undefined ; 26 VariantType
  .word syscall0Undefined ; 27 VariantBox
  .word syscall0Undefined ; 28 VariantUnBox
  
  .ifdef VIALCD
  .word syscallScreenPrint      ; 29
  .word syscallScreenPrintLn    ; 2A
  .word syscallScreenClear      ; 2B
  .word syscall0Undefined ; 2C ScreenSetCursor
  .word syscallScreenColumnsGet ; 2D 
  .word syscallScreenRowsGet   ; 2E
  .word syscall0Undefined ; 2F ScreenCursorXGet
  .word syscall0Undefined ; 30 ScreenCursorYGet
  .word syscall0Undefined ; 31 X ScreenSuspend
  .word syscall0Undefined ; 32 X ScreenResume
  .word syscall0Undefined ; 33 ScreenDrawChar
  .else
  .word syscall0Undefined ; 29
  .word syscall0Undefined ; 2A
  .word syscall0Undefined ; 2B
  .word syscall0Undefined ; 2C
  .word syscall0Undefined ; 2D 
  .word syscall0Undefined ; 2E
  .word syscall0Undefined ; 2F
  .word syscall0Undefined ; 30
  .word syscall0Undefined ; 31
  .word syscall0Undefined ; 32
  .word syscall0Undefined ; 33
  .endif
  
  .word syscall0Undefined; 34 IntToFloat
  .ifdef LONGS
  .word syscallIntToLong ; 35
  .word syscallUIntToLong ; 36
  .else
  .word syscall0Undefined ; 35
  .word syscall0Undefined ; 36
  .endif
  .word syscallUIntToInt  ; 37 UIntToInt
  .word syscall0Undefined ; 38 LongToString (.hs)
  
  .ifdef LONGS
  .ifdef LISTS
  .word syscallLongToBytes ; 39 
  .else
  .word syscall0Undefined ; 39 
  .endif
  
  .word syscall0Undefined ; 3A X LongToFloat
  .word syscallLongToInt ; 3B
  .word syscallLongToUInt ; 3C
  .word syscallLongNew ; 3D
  .word syscallLongNewFromConstant ; 3E
  .word syscallLongAdd ; 3F
  .word syscallLongSub ; 40
  .word syscallLongDiv ; 41
  .word syscallLongMul ; 42
  .word syscallLongMod ; 43
  .word syscallLongEQ ; 44
  .word syscallLongLT ; 45
  .word syscallLongLE ; 46
  .word syscallLongGT ; 47
  .word syscallLongGE ; 48
  .word syscallLongNegate ; 49
  .else
  .word syscall0Undefined ; 39 
  .word syscall0Undefined ; 3A X LongToFloat
  .word syscall0Undefined ; 3B
  .word syscall0Undefined ; 3C
  .word syscall0Undefined ; 3D
  .word syscall0Undefined ; 3E
  .word syscall0Undefined ; 3F
  .word syscall0Undefined ; 40
  .word syscall0Undefined ; 41
  .word syscall0Undefined ; 42
  .word syscall0Undefined ; 43
  .word syscall0Undefined ; 44
  .word syscall0Undefined ; 45
  .word syscall0Undefined ; 46
  .word syscall0Undefined ; 47
  .word syscall0Undefined ; 48
  .word syscall0Undefined ; 49
  .endif
  .word syscall0Undefined; 4A FloatToString
  .word syscall0Undefined; 4B FloatToBytes
  .word syscall0Undefined; 4C FloatNew
  .word syscall0Undefined; 4D FloatNewFromConstant
  .word syscall0Undefined; 4E FloatAdd
  .word syscall0Undefined; 4F FloatSub
  .word syscall0Undefined; 50 FloatDiv
  .word syscall0Undefined; 51 FloatMul
  .word syscall0Undefined; 52 FloatEQ
  .word syscall0Undefined; 53 FloatLT
  .word syscall0Undefined; 54 FloatLE
  .word syscall0Undefined; 55 FloatGT
  .word syscall0Undefined; 56 FloatGE
  .ifdef LONGS
  .word syscallTimerMillis; 57
  .else
  .word syscall0Undefined; 57
  .endif
  .word syscall0Undefined; 58 TimeMicrosGet
  .word syscall0Undefined; 59
  .word syscall0Undefined; 5A
  .word syscall0Undefined; 5B
  .word syscall0Undefined; 5C
  .word syscall0Undefined; 5D
  .word syscall0Undefined; 5E
  .word syscall0Undefined; 5F
  .word syscall0Undefined; 60
  .word syscall0Undefined; 61
  .word syscall0Undefined; 62
  .word syscall0Undefined; 63
  .word syscall0Undefined; 64
  .word syscall0Undefined; 65
  .word syscall0Undefined; 66
  .word syscall0Undefined; 67
  .word syscall0Undefined; 68
  .word syscall0Undefined; 69
  .word syscall0Undefined; 6A
  .word syscall0Undefined; 6B
  .word syscall0Undefined; 6C
  .word syscall0Undefined; 6D
  .word syscall0Undefined; 6E
  .word syscall0Undefined; 6F
  .word syscall0Undefined; 70
  .word syscall0Undefined; 71
  .word syscall0Undefined; 72 KeyboardReadKey
  .word syscall0Undefined; 73 KeyboardIsAvailableGet
  .word syscall0Undefined; 74 KeyboardToKey
  .word syscall0Undefined; 75 KeyboardClickXGet
  .word syscall0Undefined; 76 KeyboardClickYGet
  .word syscall0Undefined; 77 KeyboardClickUpGet
  .word syscall0Undefined; 78 KeyboardClickDoubleGet
  .word syscall0Undefined; 79 KeyboardScrollDeltaGet
  .word syscall0Undefined; 7A DiagnosticsOutputDebug
  .word syscall0Undefined; 7B DiagnosticsAssert
  .word syscallDiagnosticsDie; 7C
  .word syscall0Undefined; 7D DiagnosticsSetError
  .word syscallTypesTypeOf; 7E
  .ifdef HEAP
  .word syscallValueTypeOf; 7F
  .else
  .word syscall0Undefined; 7F
  .endif
  
syscall0JumpTableHigher:
  .ifdef DICTIONARIES
  .word syscallTypesKeyTypeOf ; 80
  .else
  .word syscall0Undefined;
  .endif
  .word syscall0Undefined; 81 TypesBoxTypeOf
  .word syscall0Undefined; 82 TypesVerifyValueTypes
  .ifdef STRINGS
  .word syscallStringBuild  ; 83
  .else
  .word syscall0Undefined; 83
  .endif
  .word syscall0Undefined; 84
  .word syscall0Undefined; 85
  .word syscall0Undefined; 86
  .word syscall0Undefined; 87
  .word syscall0Undefined; 88
  .word syscall0Undefined; 89
  .word syscall0Undefined; 8A
  .word syscall0Undefined; 8B
  .word syscall0Undefined; 8C
  .word syscall0Undefined; 8D
  .word syscall0Undefined; 8E
  .word syscall0Undefined; 8F
  .word syscall0Undefined; 90
  .word syscall0Undefined; 91
  .word syscall0Undefined; 92
  .word syscall0Undefined; 93
  .word syscall0Undefined; 94
  .word syscall0Undefined; 95
  .word syscall0Undefined; 96
  .word syscall0Undefined; 97
  .word syscall0Undefined; 98
  .word syscall0Undefined; 99
  .word syscall0Undefined; 9A
  .word syscall0Undefined; 9B
  .word syscall0Undefined; 9C
  .word syscall0Undefined; 9D
  .word syscall0Undefined; 9E
  .word syscall0Undefined; 9F
  .word syscall0Undefined; A0
  .word syscall0Undefined; A1
  .ifdef ACIASERIAL
  .word syscall0Undefined; A2 SerialConnect
  .word syscall0Undefined; A3 SerialClose
  .word syscall0Undefined; A4 SerialIsValid
  .word syscallSerialIsAvailableGet ; A5
  .word syscallSerialReadChar       ; A6
  .word syscallSerialWriteChar      ; A7
  .else
  .word syscall0Undefined ; A2
  .word syscall0Undefined ; A3
  .word syscall0Undefined ; A4
  .word syscall0Undefined ; A5
  .word syscall0Undefined ; A6
  .word syscall0Undefined ; A7
  .endif
  .word syscallHardwareLEDSet       ; A8
  .ifdef HEAP
  .word syscallMemoryReadByte; A9
  .word syscallMemoryWriteByte; AA
  .word syscallMemoryAvailable; AB
  .word syscallMemoryMaximum; AC
  .word syscallMemoryAllocate; AD
  .word syscallMemoryFree; AE
  .else
  .word syscall0Undefined; A9
  .word syscall0Undefined; AA
  .word syscall0Undefined; AB
  .word syscall0Undefined; AC
  .word syscall0Undefined; AD
  .word syscall0Undefined; AE
  .endif
  .word syscallTraceSet; AF
  .word syscallTraceGet; B0
  .ifdef DICTIONARIES
  
  .ifdef UNUSED
  .word syscallHashKey; B1
  .else
  .word syscall0Undefined; B1
  .endif
  
  .else
  .word syscall0Undefined; B1
  .endif
  .word syscall0Undefined; B2
  .word syscall0Undefined; B3
  .word syscall0Undefined; B4
  .ifdef STRINGS
  .word syscallStringBuildFront ; B5
  .else
  .word syscall0Undefined; B5
  .endif
  .ifdef HEAP
  .word syscallMemoryReadBit ; B6
  .word syscallMemoryWriteBit ; B7
  .else
  .word syscall0Undefined;
  .word syscall0Undefined;
  .endif
  .word syscallCharToUpper
  .word syscallCharIsUpper
  .word syscallCharIsDigit
  .word syscallCharIsLetterOrDigit
  .word syscallCharIsLower
  .word syscallCharToDigit
  .word syscallCharToHex
  .word syscallCharIsHexDigit 
  
  .word syscallCharToLower ; C0
  .ifdef STRINGS
  .word syscallStringStartsWith
  .word syscallStringContains
  .word syscallStringIndexOf
  .else
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .endif
  .word syscallWarpSet    ; 0xC4
  .word syscallWarpGet    ; 0xC5
  .word syscallTimeDelay  ; 0xC6
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined ; CF
  
  .word syscall0Undefined ; D0
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined ; DF
  
  
  .word syscall0Undefined ; E0
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined ; EF
  
  .word syscall0Undefined ; F0
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined
  .word syscall0Undefined ; FF
  
  
opcodeSYSCALL0:
  jsr incPC ; PC++
  lda (PC)  ; load syscall index
  
  .ifdef PROFILE
  jsr incHopperSysProfile
  .endif
  
  ; http://6502.org/tutorials/compare_instructions.html
  cmp #$80                 ; C will be set if A >= $80
  bcs opcodeSYSCALL0Higher ; 0x80..0xFF
  
  asl ; x2
  tax
  jmp ( syscall0JumpTableLower, X)
  
opcodeSYSCALL0Higher:
  asl ; strips bit 7 and x2
  tax
  jmp ( syscall0JumpTableHigher, X)
  
syscall0Undefined:
  .ifndef NODIAGNOSTICS
  jsr decPC
  jsr diagnosticOutString
  .byte $0D, "?sysCall0", 0
  .endif
  jmp throwToys
  
opcodeSYSCALL1:
  jsr incPC ; PC++
  lda (PC)  ; load syscall index
  
  .ifdef PROFILE
  jsr incHopperSysProfile
  .endif

  .ifdef STRINGS
  cmp #$0
  bne notSyscallStringNewFromConstant1
  jmp syscallStringNewFromConstant1
notSyscallStringNewFromConstant1:
  cmp #$3
  bne notSyscallStringAppend1
  jmp syscallStringAppend1
notSyscallStringAppend1:
  cmp #$29
  bne notSyscallScreenPrint1
  jmp syscallScreenPrint1
notSyscallScreenPrint1:
  cmp #$83
  bne notSyscallStringBuild1
  jmp syscallStringBuild1
notSyscallStringBuild1:
  cmp #$C3
  bne notsyscallStringIndexOf1
  jmp syscallStringIndexOf1
notsyscallStringIndexOf1:
  .endif
  
  .ifndef NODIAGNOSTICS
  jsr decPC
  jsr diagnosticOutString
  .byte $0D, "?sysCall1", 0
  .endif
  jmp throwToys

opcodeSYSCALL:
  jsr incPC ; PC++
  lda (PC)  ; load syscall index
  tax
  
  .ifdef PROFILE
  jsr incHopperSysProfile
  .endif
  
  .ifdef STACK8
  
  ; doesn't check syscall index, pops it from the stack assumes it is 2
  dec SP8
  dec SP8
  
  .else ; STACK8
  
  .ifdef CHECKED
  
  jsr decTSP
  lda (TSP)
  jsr assertUInt
  
  jsr decSP
  jsr decSP
  
  .else
  
  lda TSPL
  bne opcodeSYSCALLTSPSkipMSB
  dec TSPH
opcodeSYSCALLTSPSkipMSB:
  dec TSPL
  
  ; decSP x2
  sec
  lda SPL
  sbc #2
  sta SPL
  bcs opcodeSYSCALLSkipMSB
  dec SPH
opcodeSYSCALLSkipMSB:
  
  .endif ; !STACK8
  
  .ifdef CHECKED
  ldy #1
  lda (SP), Y  ; MSB
  bne notSyscallNot2
  lda (SP)  ; LSB
  cmp #2
  beq syscall2
  
notSyscallNot2:
  .ifndef NODIAGNOSTICS
  jsr decPC
  jsr diagnosticOutString
  .byte $0D, "?sysCall not 2", 0
  .endif
  jmp throwToys
syscall2:  
  .endif
  .endif
  
  .ifdef STRINGS
  cpx #$83
  bne notSyscallStringBuild2
  jmp syscallStringBuild2
notSyscallStringBuild2:
  .endif
  .ifndef NODIAGNOSTICS
  jsr decPC
  txa
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte $0D, "?sysCall", 0
  .endif
  jmp throwToys
  
  
; ######################## SysCall subroutines ########################



syscallDiagnosticsDie:

  
  .ifdef STACK8
  
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  sta ACCL
  inx
  lda HopperValueStack, X
  sta ACCH
  
  .else
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr diagnosticOutHex
  jsr assertUInt
  .endif
  
  jsr decSP          ; MSB
  lda (SP)
  sta ACCH
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  .endif
  
utilityDiagnosticsDie:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "DIE:", 0
  lda ACCL
  jsr diagnosticOutHex
  ;jsr memoryHeapWalk
  .endif
  jmp throwToysNoStack
  
syscallTraceSet:

  .ifdef STACK8
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  jsr decSP          ; MSB
  lda (SP)
  jsr decSP          ; LSB
  lda (SP)
  
  .endif
  
  beq traceOff
  ; turn Trace on
  smb0 FLAGS
  jmp nextInstruction
traceOff:
  ; turn Trace off
  rmb0 FLAGS
  jmp nextInstruction
  
syscallTraceGet:

  lda FLAGS
  and #%00000001     ; works because Trace is bit 0
  
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
  
  
syscallWarpSet:

  .ifdef STACK8
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  jsr decSP          ; MSB
  lda (SP)
  jsr decSP          ; LSB
  lda (SP)
  
  .endif
  
  beq warpOff
  ; turn Warp on
  smb1 FLAGS
  jmp nextInstruction
warpOff:
  ; turn Warp off
  rmb1 FLAGS
  jmp nextInstruction
  
syscallWarpGet:

  lda #0
  bbr1 FLAGS, notWarp
  lda #1
notWarp:
  
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
  
  .ifdef HEAP
  
BitMaskTable:
  .byte %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000
  
syscallMemoryReadBit:

  .ifdef STACK8
  
  ldx SP8
  ; index
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  
  ; address
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  
  stx SP8
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; index
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; address
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  
  .endif
  
  ; capture the bit
  lda IDYL
  and #$07
  tax
  lda BitMaskTable, X
  sta aBITMASK
  ; divide offset by 8
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDXL
  lda IDXH  ; MSB
  adc IDYH
  sta IDXH
  
  ; IDX now points to the address of the byte containing the bit
  ldx #0
  lda (IDX)
  and aBITMASK
  beq syscallMemoryReadBitIsClear
  ; is set
  ldx #1
syscallMemoryReadBitIsClear:
  
  ; resulting bit but as a uint on the stack
  .ifdef STACK8
  
  txa
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
  
  .else
  
  txa
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  
  jmp nextInstruction
  
syscallMemoryWriteBit:

  .ifdef STACK8
  
  ; value
  ldx SP8
  
  dex
  lda HopperValueStack, X
  sta ACCH
  dex
  lda HopperValueStack, X
  sta ACCL
    
  ; index
  
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
    
  ; address
  
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  
  stx SP8
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; value
  jsr decSP          ; MSB
  lda (SP)
  sta ACCH
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; index
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; address
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  
  .endif
  
  ; capture the bit
  lda IDYL
  and #$07
  tax
  lda BitMaskTable, X
  sta aBITMASK
  ; divide offset by 8
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  lsr IDYH
  ror IDYL
  
  clc
  lda IDXL  ; LSB
  adc IDYL
  sta IDXL
  lda IDXH  ; MSB
  adc IDYH
  sta IDXH
  
  ; IDX now points to the address of the byte containing the bit
  ; ACCL contains the value to set (0 or !0)
  
  
  lda ACCL
  beq syscallMemoryWriteBitUnset
  ; set bit
  lda aBITMASK
  ora (IDX)
  sta (IDX)
  
  jmp nextInstruction
  
syscallMemoryWriteBitUnset:
  ; unset bit
  lda aBITMASK
  eor #$FF
  and (IDX)
  sta (IDX)
  
  jmp nextInstruction
  
  
syscallMemoryReadByte:

  .ifdef STACK8
  
  ; address
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; address
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  .endif
  
  
  
  
  ; resulting byte but as a uint on the stack
  .ifdef STACK8
  
  ldx SP8
  lda (IDX)
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
  
  .else
  lda (IDX)
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  
  jmp nextInstruction

syscallMemoryAllocate:

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
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; size
  jsr decSP          ; MSB
  lda (SP)
  sta ACCH
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  .endif

  ; size is in ACC
  ; return address in IDX
  jsr memoryAllocate
  
  ; address
  .ifdef STACK8
  ldx SP8
  lda IDXL
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
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
  lda IDXL
  sta (SP)
  jsr incSP          ; LSB
  ldy #1
  lda IDXH
  sta (SP)
  jsr incSP          ; MSB
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  jmp nextInstruction


  
syscallMemoryFree:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; address
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  .endif

  jsr memoryFree
  
  jmp nextInstruction

  
syscallMemoryAvailable:
  
  jsr memoryAvailable
  
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
  
  
  .else
  
  lda ACCL
  sta (SP)
  jsr incSP          ; LSB
  lda ACCH
  sta (SP)
  jsr incSP          ; MSB
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  
  jmp nextInstruction

syscallMemoryMaximum:

  jsr memoryMaximum
  
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
  
  .else
  
  lda ACCL
  sta (SP)
  jsr incSP          ; LSB
  lda ACCH
  sta (SP)
  jsr incSP          ; MSB
  
  lda #tUInt
  sta (TSP)
  jsr incTSP
  
  .endif
  
  jmp nextInstruction
  

syscallMemoryWriteByte:
  
  .ifdef STACK8
  
  ldx SP8
  ; value byte from stack but as a uint
  dex
  lda HopperValueStack, X ; MSB
  dex
  lda HopperValueStack, X ; LSB
  sta ACCL
  
  ; address
  
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; value byte from stack but as a uint
  ; MSB
  jsr decSP
  
  .ifdef CHECKED
  ; verify that the MSB is zero
  lda (SP)
  beq writeByteOk
  lda #tUInt
  jsr assertUInt
writeByteOk:
  .endif
  
  ; LSB
  jsr decSP          
  lda (SP)
  sta ACCL
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; address
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  .endif
  
  lda ACCL
  sta (IDX)
  
  jmp nextInstruction
  .endif
  
syscallScreenPrint:
  
  .ifdef STACK8
  dec SP8
  dec SP8
  dec SP8
  dec SP8
  .else  
  ; backColour:
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  jsr decTSP
  
  ; foreColour:
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  jsr decTSP
  .endif
  
  ; c
  .ifdef STACK8
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB (zero for char)
  jsr decSP          ; LSB
  lda (SP)
  jsr decTSP
  .endif
  jsr LCDCharacter
  
  jmp nextInstruction
  
  .ifdef STRINGS
syscallScreenPrint1:

  .ifdef STACK8
  ldx SP8
  dex
  dex
  dex
  dex
  .else
  
  ; backColour:
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  jsr decTSP
  
  ; foreColour:
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  jsr decTSP
  .endif
  
  ; str
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB
  lda (SP)
  .endif
  sta IDXH
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  stx SP8
  .else
  jsr decSP          ; LSB
  jsr decTSP
  lda (SP)
  .endif
  sta IDXL
  
  .ifndef STACK8
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif
  .endif
  
  jsr releaseSP ; we popped it, decrease reference count
  
  ; skip past block header (type and reference count)
  jsr incIDX
  jsr incIDX
  
  ldy #0
  lda (IDX), Y       ; LSB of length
  sta IDYL
  iny
  lda (IDX), Y       ; MSB of length
  sta IDYH
  
  jsr incIDY
  
  ; skip past length
  jsr incIDX
  jsr incIDX
  
screenPrintNext:  
  jsr decIDY
  lda #0
  cmp IDYL
  bne screenPrintNextChar
  cmp IDYH
  beq screenPrintDone
screenPrintNextChar:  
  lda (IDX)
  jsr LCDCharacter
  jsr incIDX
  
  jmp screenPrintNext
screenPrintDone:
  jmp nextInstruction
  .endif
  
  
syscallScreenClear:
  jsr LCDClear
  jmp nextInstruction

syscallScreenPrintLn:
  jsr LCDNewLine
  jmp nextInstruction

syscallHardwareLEDSet:

  .ifdef STACK8
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  
  .else
  
  jsr decSP           ; MSB (assume it is zero for bool type)
  jsr decSP
  lda (SP)            ; LSB
  jsr decTSP
  .endif
  
  ; LEDWrite          : non-zero A means ON, zero A means off (not LCD but uses same port)
  jsr LEDWrite        ; munts A
  jmp nextInstruction
  
;syscallHardwareDelay:
;  jsr decSP          ; MSB
;  lda (SP)
;  tay
;  jsr decSP          ; LSB
;  lda (SP)
;  jsr decTSP
;  tax
;  ; TimerDelay         : value in X / Y (LSB / MSB) in milliseconds, returns after delay
;  jsr TimerDelay
;  jmp nextInstruction

  .ifdef LONGS
syscallTimerMillis:
  ;jsr memoryHeapWalk
  lda #Ticks
  sta IDXL
  stz IDXH
  jmp syscallLongNewFromAddress
  .endif

  
  

  
  .ifdef ACIASERIAL
  
syscallSerialIsAvailableGet:
  ldx #0
  jsr SerialInAvailable ; is there a character available in the buffer?
  beq syscallSerialIsAvailableGetNo
  ldx #1
syscallSerialIsAvailableGetNo:

  .ifdef STACK8
  
  txa
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
  
  txa
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
   
syscallSerialReadChar:
  ; <ctrl><C> while waiting for character?
  lda SerialBreakFlag
  beq syscallSerialReadNoBreak
  ; we don't clear the flag which means the Monitor will break on the next instruction..
  lda #$03
  bra syscallSerialReadBreak
  
syscallSerialReadNoBreak:
  jsr SerialInAvailable ; is there a character available in the buffer?
  beq syscallSerialReadChar
  jsr SerialInConsume  ; consume it returning the character in A
  
syscallSerialReadBreak:

  .ifdef STACK8
  
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
  
  .else
  
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  
  jmp nextInstruction
   
syscallSerialWriteChar:
 
  .ifdef STACK8
  
  dec SP8
  dec SP8
  ldx SP8
  lda HopperValueStack, X
  
  .else
  
  jsr decSP           ; MSB (assume it is zero for char type)
  jsr decSP
  lda (SP)            ; LSB
  
  jsr decTSP
  .endif
  
  jsr SerialOut ; transmits A
  jmp nextInstruction
  
  .endif
  
  .ifdef VIALCD
syscallScreenColumnsGet:

  .ifdef STACK8
  
  ldx SP8
  lda #LCDCOLUMNS
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
  
  .else
  
  lda #LCDCOLUMNS
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  jmp nextInstruction
  
syscallScreenRowsGet:

  .ifdef STACK8
  
  ldx SP8
  lda #LCDROWS
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
  
  .else

  lda #LCDROWS
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  .endif
  
  jmp nextInstruction
  
  .endif
  
syscallTypesTypeOf:

  .ifdef STACK8
  
  ; this -> IDY
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  stz TOPH
  lda HopperTypeStack, X
  sta TOPL
  stx SP8
  
  .else
  
  ; this -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  stz TOPH
  lda (TSP)
  sta TOPL
  
  .endif
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallTypesTypeOfNoRelease
  
  .ifdef HEAP
  jsr releaseSP
  .else
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Heap", 0
  .endif
  jmp throwToys
  .endif
  
syscallTypesTypeOfNoRelease:

  lda #tUInt
  jmp pushTOPExit
  
  .ifdef HEAP

syscallValueTypeOf:
  
  ; tDictionary, tPair, tList, tArray

  .ifdef STACK8
  
  ; this -> IDY
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  lda HopperTypeStack, X
  sta TOPL
  stx SP8
  
  .else
  
  ; this -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  lda (TSP)
  sta TOPL
  
  .endif
  
  .ifdef LISTS
  cmp #tList
  bne syscallNotValueTypeOfList
  jmp syscallValueTypeOfList
syscallNotValueTypeOfList:  
  .endif
  
  .ifdef ARRAYS
  cmp #tArray
  bne syscallNotValueTypeOfArray
  jmp syscallValueTypeOfArray
syscallNotValueTypeOfArray:  
  .endif
  
  
  .ifdef DICTIONARIES
  cmp #tDictionary
  bne syscallNotValueTypeOfDictionary
  jmp syscallValueTypeOfDictionary
syscallNotValueTypeOfDictionary:  

  cmp #tPair
  bne syscallNotValueTypeOfPair
  jmp syscallValueTypeOfPair
syscallNotValueTypeOfPair: 

  .endif
  
  cmp #tVariant
  beq syscallValueTypeOfVariant
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?ValueTypeOf", 0
  .endif
  jmp throwToys
  
syscallValueTypeOfList:
syscallValueTypeOfArray:

  ldy #4
  lda (IDY), Y

  bra syscallValueTypeOfDone
  
syscallValueTypeOfDictionary:
syscallValueTypeOfPair:
  ldy #3
  lda (IDY), Y

  bra syscallValueTypeOfDone
  
syscallValueTypeOfVariant:

  ldy #2
  lda (IDY), Y
  
  ; fall through
syscallValueTypeOfDone:
  sta TOPL
  stz TOPH
  
  jsr rawReleaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  lda #tUInt
  jmp pushTOPExit
  
syscallTypesKeyTypeOf:
  ; tDictionary, tPair
  
  .ifdef STACK8
  
  ; this -> IDY
  ldx SP8
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  lda HopperTypeStack, X
  sta TOPL
  stx SP8
  
  .else
  
  ; this -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  lda (TSP)
  sta TOPL
  
  .endif
  
  .ifdef DICTIONARIES
  cmp #tDictionary
  bne syscallNotKeyTypeOfDictionary
  jmp syscallKeyTypeOfDictionary
syscallNotKeyTypeOfDictionary:  

  cmp #tPair
  bne syscallNotKeyTypeOfPair
  jmp syscallKeyTypeOfPair
syscallNotKeyTypeOfPair: 

  .endif
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?KeyTypeOf", 0
  .endif
  jmp throwToys
  
syscallKeyTypeOfDictionary:
syscallKeyTypeOfPair:
  ldy #2
  lda (IDY), Y
  sta TOPL
  stz TOPH
  
  jsr rawReleaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  lda #tUInt
  jmp pushTOPExit

  .endif
  
syscallTimeDelay:
  jsr popTOPUInt
  
  ldx TOPL
  ldy TOPH
  ; TimerDelay         : value in X / Y (LSB / MSB) in milliseconds, returns after delay
  jsr TimerDelay
  
  jmp nextInstruction