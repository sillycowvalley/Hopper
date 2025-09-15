# Complete Disassembly with All Debug Markers

## Program Structure
```
0800: JMP 0x09BA         ; Jump to main
0803: JMP [0x0022]       ; BIOS dispatcher 
0806: RTS
0807-0E: "Sum=%d\n\0"    ; String literal
```

## add() Function (0x080F - 0x09B9)

### Function Prologue
```hopper
080F: LDA 0x60           ; Load old BP
0811: PHA                ; Push old BP
0812: TSX                
0813: STX 0x60           ; New BP = stack pointer
```

### Load Parameter 'a' from [BP+4]
```hopper
0815: LDA 0x60           ; BP
0817: CLC
0818: ADC #0x04          ; BP+4 (first parameter)
081A: TAY
```

### 🔍 DEBUG 'f': Y=F5 when loading param 'a'
```hopper
081B: PHX
081C: PHY
081D: LDA #0x66          ; 'f'
081F: LDX #0x12          ; PrintChar
0821: JSR 0x0803
0824: TYA
0825: LDX #0x13          ; PrintHex
0827: JSR 0x0803
082A: LDA #0x20          ; ' '
082C: LDX #0x12
082E: JSR 0x0803
0831: PLY
0832: PLX
```

### Continue Loading 'a'
```hopper
0833: LDA [0x61],Y       ; Load a[0]
0835: STA 0x16           ; ZP.NEXT0
0837: LDA [0x63],Y       ; Load a[1]
0839: STA 0x17           ; ZP.NEXT1
083B: LDA [0x65],Y       ; Load a[2]
083D: STA 0x18           ; ZP.NEXT2
083F: LDA [0x67],Y       ; Load a[3]
0841: STA 0x19           ; ZP.NEXT3
```

### Push 'a' to Stack
```hopper
0843: TSX
0844: TXA
0845: TAY                ; Y=F1
```

### 🔍 DEBUG 'm': Y=F1 when pushing 'a'
```hopper
0846: PHX
0847: PHY
0848: LDA #0x6D          ; 'm'
084A: LDX #0x12
084C: JSR 0x0803
084F: TYA
0850: LDX #0x13
0852: JSR 0x0803
0855: LDA #0x20
0857: LDX #0x12
0859: JSR 0x0803
085C: PLY
085D: PLX
```

### Store 'a' to Parallel Stacks
```hopper
085E: LDA 0x16
0860: STA [0x61],Y
0862: LDA 0x17
0864: STA [0x63],Y
0866: LDA 0x18
0868: STA [0x65],Y
086A: LDA 0x19
086C: STA [0x67],Y
086D: PHA                ; Push slot
```

### Load Parameter 'b' from [BP+5]
```hopper
086E: LDA 0x60           ; BP
0870: CLC
0871: ADC #0x05          ; BP+5
0873: TAY                ; Y=F6
```

### 🔍 DEBUG 'f': Y=F6 when loading param 'b'
```hopper
0874: PHX
0875: PHY
0876: LDA #0x66          ; 'f'
0878: LDX #0x12
087A: JSR 0x0803
087D: TYA
087E: LDX #0x13
0880: JSR 0x0803
0883: LDA #0x20
0885: LDX #0x12
0887: JSR 0x0803
088A: PLY
088B: PLX
```

### Continue Loading 'b'
```hopper
088C: LDA [0x61],Y
088E: STA 0x16
0890: LDA [0x63],Y
0892: STA 0x17
0894: LDA [0x65],Y
0896: STA 0x18
0898: LDA [0x67],Y
089A: STA 0x19
```

### Push 'b' to Stack
```hopper
089C: TSX
089D: TXA
089E: TAY                ; Y=F0
```

### 🔍 DEBUG 'm': Y=F0 when pushing 'b'
```hopper
089F: PHX
08A0: PHY
08A1: LDA #0x6D          ; 'm'
08A3: LDX #0x12
08A5: JSR 0x0803
08A8: TYA
08A9: LDX #0x13
08AB: JSR 0x0803
08AE: LDA #0x20
08B0: LDX #0x12
08B2: JSR 0x0803
08B5: PLY
08B6: PLX
```

### Store 'b' to Parallel Stacks
```hopper
08B7: LDA 0x16
08B9: STA [0x61],Y
08BB: LDA 0x17
08BD: STA [0x63],Y
08BF: LDA 0x18
08C1: STA [0x65],Y
08C3: LDA 0x19
08C5: STA [0x67],Y
08C6: PHA                ; Push slot
```

### Pop 'b' to ZP.ACC
```hopper
08C7: PLA
08C8: TSX
08C9: TXA
08CA: TAY                ; Y=F0
```

### 🔍 DEBUG 'b': Y=F0 after popping 'b'
```hopper
08CB: PHX
08CC: PHY
08CD: LDA #0x62          ; 'b'
08CF: LDX #0x12
08D1: JSR 0x0803
08D4: TYA
08D5: LDX #0x13
08D7: JSR 0x0803
08DA: LDA #0x20
08DC: LDX #0x12
08DE: JSR 0x0803
08E1: PLY
08E2: PLX
```

### Load 'b' from Parallel Stacks to ACC
```hopper
08E3: LDA [0x61],Y
08E5: STA 0x12           ; ZP.ACC0
08E7: LDA [0x63],Y
08E9: STA 0x13
08EB: LDA [0x65],Y
08ED: STA 0x14
08EF: LDA [0x67],Y
08F1: STA 0x15
```

### Pop 'a' to ZP.NEXT
```hopper
08F3: PLA
08F4: TSX
08F5: TXA
08F6: TAY                ; Y=F1
```

### 🔍 DEBUG 't': Y=F1 after popping 'a'
```hopper
08F7: PHX
08F8: PHY
08F9: LDA #0x74          ; 't'
08FB: LDX #0x12
08FD: JSR 0x0803
0900: TYA
0901: LDX #0x13
0903: JSR 0x0803
0906: LDA #0x20
0908: LDX #0x12
090A: JSR 0x0803
090D: PLY
090E: PLX
```

### Load 'a' from Parallel Stacks to NEXT
```hopper
090F: LDA [0x61],Y
0911: STA 0x16
0913: LDA [0x63],Y
0915: STA 0x17
0917: LDA [0x65],Y
0919: STA 0x18
091B: LDA [0x67],Y
091D: STA 0x19
```

### Call Long.Add
```hopper
091F: LDX #0x1A          ; SysCall.LongAdd
0921: JSR 0x0803         ; Result in NEXT
```

### Marshal Result to Stack
```hopper
0924: TSX
0925: TXA
0926: TAY                ; Y=F1
```

### 🔍 DEBUG 'j': Y=F1 when marshaling result
```hopper
0927: PHX
0928: PHY
0929: LDA #0x6A          ; 'j'
092B: LDX #0x12
092D: JSR 0x0803
0930: TYA
0931: LDX #0x13
0933: JSR 0x0803
0936: LDA #0x20
0938: LDX #0x12
093A: JSR 0x0803
093D: PLY
093E: PLX
```

### Store Result to Parallel Stacks
```hopper
093F: LDA 0x16
0941: STA [0x61],Y
0943: LDA 0x17
0945: STA [0x63],Y
0947: LDA 0x18
0949: STA [0x65],Y
094B: LDA 0x19
094D: STA [0x67],Y
094E: PHA                ; Push slot
```

### Pop Result Back
```hopper
094F: PLA
0950: TSX
0951: TXA
0952: TAY                ; Y=F1
```

### 🔍 DEBUG 'q': Y=F1 when popping result
```hopper
0953: PHX
0954: PHY
0955: LDA #0x71          ; 'q'
0957: LDX #0x12
0959: JSR 0x0803
095C: TYA
095D: LDX #0x13
095F: JSR 0x0803
0962: LDA #0x20
0964: LDX #0x12
0966: JSR 0x0803
0969: PLY
096A: PLX
```

### Load Result Back
```hopper
096B: LDA [0x61],Y
096D: STA 0x16
096F: LDA [0x63],Y
0971: STA 0x17
0973: LDA [0x65],Y
0975: STA 0x18
0977: LDA [0x67],Y
0979: STA 0x19
```

### Store Result at BP+5
```hopper
097A: LDA 0x60           ; BP
097C: CLC
097D: ADC #0x05          ; BP+5
097F: TAY                ; Y=F6
```

### 🔍 DEBUG 'c': Y=F6 when WRITING return value
```hopper
0980: PHX
0981: PHY
0982: LDA #0x63          ; 'c'
0984: LDX #0x12
0986: JSR 0x0803
0989: TYA
098A: LDX #0x13
098C: JSR 0x0803
098F: LDA #0x20
0991: LDX #0x12
0993: JSR 0x0803
0996: PLY
0997: PLX
```

### Write Return Value at BP+5
```hopper
0998: LDA 0x16
099A: STA [0x61],Y       ; Write to F6
099C: LDA 0x17
099E: STA [0x63],Y
09A0: LDA 0x18
09A2: STA [0x65],Y
09A4: LDA 0x19
09A6: STA [0x67],Y
```

### Function Epilogue
```hopper
09A8: LDX 0x60
09AA: TXS
09AB: PLA
09AC: STA 0x60
09AE: RTS

09AF: LDX 0x60           ; DUPLICATE!
09B1: TXS
09B2: PLA
09B3: STA 0x60
09B5: RTS
```

## main() Function (0x09BA - 0x0B8F)

### Initialize Runtime Stack
```hopper
09BA: LDX #0x00          ; SysCall.MemAllocate
09BC: JSR 0x0803
09BF: STZ 0x61
09C1: STZ 0x63
09C3: STZ 0x65
09C5: STZ 0x67
09C7: LDA 0x1B           ; IDXH
09C9: STA 0x62
09CB: INC A
09CC: STA 0x64
09CE: INC A
09CF: STA 0x66
09D1: INC A
09D2: STA 0x68
```

### Function Prologue
```hopper
09D4: LDA 0x60
09D6: PHA
09D7: TSX
09D8: STX 0x60           ; New BP
```

### Reserve Slots
```hopper
09DA: PHA                ; Local 'sum'
09DB: PHA                ; Return slot
```

### Push First Argument (5)
```hopper
09DC: LDA #0x05
09DE: STA 0x16
09E0: STZ 0x17
09E2: STZ 0x18
09E4: STZ 0x19
09E6: TSX
09E7: TXA
09E8: TAY                ; Y=F6
```

### 🔍 DEBUG 'l': Y=F6 when pushing arg1
```hopper
09E9: PHX
09EA: PHY
09EB: LDA #0x6C          ; 'l'
09ED: LDX #0x12
09EF: JSR 0x0803
09F2: TYA
09F3: LDX #0x13
09F5: JSR 0x0803
09F8: LDA #0x20
09FA: LDX #0x12
09FC: JSR 0x0803
09FF: PLY
0A00: PLX
```

### Store Arg1
```hopper
0A01: LDA 0x16
0A03: STA [0x61],Y
0A05: LDA 0x17
0A07: STA [0x63],Y
0A09: LDA 0x18
0A0B: STA [0x65],Y
0A0D: LDA 0x19
0A0F: STA [0x67],Y
0A10: PHA                ; Push slot
```

### Push Second Argument (3)
```hopper
0A11: LDA #0x03
0A13: STA 0x16
0A15: STZ 0x17
0A17: STZ 0x18
0A19: STZ 0x19
0A1B: TSX
0A1C: TXA
0A1D: TAY                ; Y=F5
```

### 🔍 DEBUG 'l': Y=F5 when pushing arg2
```hopper
0A1E: PHX
0A1F: PHY
0A20: LDA #0x6C          ; 'l'
0A22: LDX #0x12
0A24: JSR 0x0803
0A27: TYA
0A28: LDX #0x13
0A2A: JSR 0x0803
0A2D: LDA #0x20
0A2F: LDX #0x12
0A31: JSR 0x0803
0A34: PLY
0A35: PLX
```

### Store Arg2
```hopper
0A36: LDA 0x16
0A38: STA [0x61],Y
0A3A: LDA 0x17
0A3C: STA [0x63],Y
0A3E: LDA 0x18
0A40: STA [0x65],Y
0A42: LDA 0x19
0A44: STA [0x67],Y
0A45: PHA                ; Push slot
```

### Call add(5, 3)
```hopper
0A46: JSR 0x080F
```

### Pop Arguments and Return Slot
```hopper
0A49: PLA                ; Pop arg2
0A4A: PLA                ; Pop arg1
0A4B: PLA                ; Pop return slot ⚠️ PROBLEM!
0A4C: TSX
0A4D: TXA
0A4E: TAY                ; Y=F7 (wrong!)
```

### 🔍 DEBUG 's': Y=F7 when READING return value
```hopper
0A4F: PHX
0A50: PHY
0A51: LDA #0x73          ; 's'
0A53: LDX #0x12
0A55: JSR 0x0803
0A58: TYA
0A59: LDX #0x13
0A5B: JSR 0x0803
0A5E: LDA #0x20
0A60: LDX #0x12
0A62: JSR 0x0803
0A65: PLY
0A66: PLX
```

### Read Return Value (from wrong place!)
```hopper
0A67: LDA [0x61],Y       ; Reading from F7, but value is at F6!
0A69: STA 0x16
0A6B: LDA [0x63],Y
0A6D: STA 0x17
0A6F: LDA [0x65],Y
0A71: STA 0x18
0A73: LDA [0x67],Y
0A75: STA 0x19
```

### Store to Local 'sum' at [BP-1]
```hopper
0A76: LDA 0x60           ; BP
0A78: CLC
0A79: ADC #0xFF          ; BP-1
0A7B: TAY                ; Y=F7
```

### 🔍 DEBUG 'd': Y=F7 at BP-1
```hopper
0A7C: PHX
0A7D: PHY
0A7E: LDA #0x64          ; 'd'
0A80: LDX #0x12
0A82: JSR 0x0803
0A85: TYA
0A86: LDX #0x13
0A88: JSR 0x0803
0A8B: LDA #0x20
0A8D: LDX #0x12
0A8F: JSR 0x0803
0A92: PLY
0A93: PLX
```

### Store to sum
```hopper
0A94: LDA 0x16
0A96: STA [0x61],Y
0A98: LDA 0x17
0A9A: STA [0x63],Y
0A9C: LDA 0x18
0A9E: STA [0x65],Y
0AA0: LDA 0x19
0AA2: STA [0x67],Y
```

### Marshal sum for printf
```hopper
0AA4: TSX
0AA5: TXA
0AA6: TAY                ; Y=F7
```

### 🔍 DEBUG 'i': Y=F7 when marshaling for printf
```hopper
0AA7: PHX
0AA8: PHY
0AA9: LDA #0x69          ; 'i'
0AAB: LDX #0x12
0AAD: JSR 0x0803
0AB0: TYA
0AB1: LDX #0x13
0AB3: JSR 0x0803
0AB6: LDA #0x20
0AB8: LDX #0x12
0ABA: JSR 0x0803
0ABD: PLY
0ABE: PLX
```

### Store for printf
```hopper
0ABF: LDA 0x16
0AC1: STA [0x61],Y
0AC3: LDA 0x17
0AC5: STA [0x63],Y
0AC7: LDA 0x18
0AC9: STA [0x65],Y
0ACB: LDA 0x19
0ACD: STA [0x67],Y
0ACE: PHA
```

### printf Setup
```hopper
0ACF: PLA
0AD0: PHA                ; Odd push/pop
0AD1: LDA #0x07          ; String low
0AD3: STA 0x1E
0AD5: LDA #0x08          ; String high
0AD7: STA 0x1F
```

### printf Format Processing
```hopper
0AD9: LDY #0x00
0ADB: LDA [0x1E],Y
0ADD: CMP #0x04          ; Check %d
0ADF: BEQ 0x0AE9
0AE1: LDX #0x12          ; PrintChar
0AE3: JSR 0x0803
0AE6: INY
0AE7: BRA 0x0ADB
```

### Load sum for %d
```hopper
0AE9: LDA 0x60           ; BP
0AEB: CLC
0AEC: ADC #0xFF          ; BP-1
0AEE: TAY                ; Y=F7
```

### 🔍 DEBUG 'f': Y=F7 loading sum for printf
```hopper
0AEF: PHX
0AF0: PHY
0AF1: LDA #0x66          ; 'f'
0AF3: LDX #0x12
0AF5: JSR 0x0803
0AF8: TYA
0AF9: LDX #0x13
0AFB: JSR 0x0803
0AFE: LDA #0x20
0B00: LDX #0x12
0B02: JSR 0x0803
0B05: PLY
0B06: PLX
```

### Load sum
```hopper
0B07: LDA [0x61],Y
0B09: STA 0x16
0B0B: LDA [0x63],Y
0B0D: STA 0x17
0B0F: LDA [0x65],Y
0B11: STA 0x18
0B13: LDA [0x67],Y
0B15: STA 0x19
```

### Push for printf
```hopper
0B17: TSX
0B18: TXA
0B19: TAY                ; Y=F6
```

### 🔍 DEBUG 'm': Y=F6 pushing for printf
```hopper
0B1A: PHX
0B1B: PHY
0B1C: LDA #0x6D          ; 'm'
0B1E: LDX #0x12
0B20: JSR 0x0803
0B23: TYA
0B24: LDX #0x13
0B26: JSR 0x0803
0B29: LDA #0x20
0B2B: LDX #0x12
0B2D: JSR 0x0803
0B30: PLY
0B31: PLX
```

### Store for printf
```hopper
0B32: LDA 0x16
0B34: STA [0x61],Y
0B36: LDA 0x17
0B38: STA [0x63],Y
0B3A: LDA 0x18
0B3C: STA [0x65],Y
0B3E: LDA 0x19
0B40: STA [0x67],Y
0B41: PHA
```

### Pop and Print Integer
```hopper
0B42: PLA
0B43: TSX
0B44: TXA
0B45: TAY                ; Y=F6
```

### 🔍 DEBUG 'a': Y=F6 in printf formatter
```hopper
0B46: PHX
0B47: PHY
0B48: LDA #0x61          ; 'a'
0B4A: LDX #0x12
0B4C: JSR 0x0803
0B4F: TYA
0B50: LDX #0x13
0B52: JSR 0x0803
0B55: LDA #0x20
0B57: LDX #0x12
0B59: JSR 0x0803
0B5C: PLY
0B5D: PLX
```

### Load and Print
```hopper
0B5E: LDA [0x61],Y
0B60: STA 0x12
0B62: LDA [0x63],Y
0B64: STA 0x13
0B66: LDA [0x65],Y
0B68: STA 0x14
0B6A: LDA [0x67],Y
0B6C: STA 0x15
0B6E: LDX #0x1F          ; PrintInt
0B70: JSR 0x0803
```

### Continue printf
```hopper
0B73: LDY #0x06
0B75: LDA [0x1E],Y
0B77: CMP #0x07
0B79: BEQ 0x0B83
0B7B: LDX #0x12
0B7D: JSR 0x0803
0B80: INY
0B81: BRA 0x0B75
```

### Epilogue
```hopper
0B83: PLA
0B84: LDX 0x60
0B86: TXS
0B87: PLA
0B88: STA 0x60
0B8A: RTS
```

## 🔴 **THE BUG: Off-by-One Error**

### Critical Comparison:
- **'c' at F6**: Return value WRITTEN here by add()
- **'s' at F7**: main() READING from here (wrong!)

### The Problem:
The third PLA at 0x0A4B pops the return slot, moving SP from F6 to F7. But the return value is AT F6!

### The Fix:
Remove the PLA at 0x0A4B. Only pop the two arguments, not the return slot.