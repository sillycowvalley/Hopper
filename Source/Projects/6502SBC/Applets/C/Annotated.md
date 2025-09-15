# Disassembly Analysis: All PHA/PLA Operations

## Program Structure
```
0800: JMP 0x08E2         ; Jump to main
0803: JMP [0x0022]       ; BIOS dispatcher 
0806: RTS
0807-0E: "Sum=%d\n\0"    ; String literal
```

## add() Function (0x080F - 0x08E1)

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
081B: LDA [0x61],Y       ; Load a[0] from stack
081D: STA 0x16           ; Store in ZP.NEXT0
081F: LDA [0x63],Y       ; Load a[1]
0821: STA 0x17           ; Store in ZP.NEXT1
0823: LDA [0x65],Y       ; Load a[2]
0825: STA 0x18           ; Store in ZP.NEXT2
0827: LDA [0x67],Y       ; Load a[3]
0829: STA 0x19           ; Store in ZP.NEXT3
```

### Push 'a' to Stack
```hopper
082B: TSX
082C: TXA
082D: TAY                ; Y = SP position
082E: LDA 0x16           ; Store NEXT to parallel stacks
0830: STA [0x61],Y
0832: LDA 0x17
0834: STA [0x63],Y
0836: LDA 0x18
0838: STA [0x65],Y
083A: LDA 0x19
083C: STA [0x67],Y
083D: PHA                ; Push dummy byte to allocate slot
```

### Load Parameter 'b' from [BP+5]
```hopper
083E: LDA 0x60           ; BP
0840: CLC
0841: ADC #0x05          ; BP+5 (second parameter)
0843: TAY
0844: LDA [0x61],Y       ; Load b[0]
0846: STA 0x16           ; Store in ZP.NEXT0
0848: LDA [0x63],Y       ; Load b[1]
084A: STA 0x17           ; Store in ZP.NEXT1
084C: LDA [0x65],Y       ; Load b[2]
084E: STA 0x18           ; Store in ZP.NEXT2
0850: LDA [0x67],Y       ; Load b[3]
0852: STA 0x19           ; Store in ZP.NEXT3
```

### Push 'b' to Stack
```hopper
0854: TSX
0855: TXA
0856: TAY
0857: LDA 0x16
0859: STA [0x61],Y
085B: LDA 0x17
085D: STA [0x63],Y
085F: LDA 0x18
0861: STA [0x65],Y
0863: LDA 0x19
0865: STA [0x67],Y
0866: PHA                ; Push dummy byte to allocate slot
```

### Pop 'b' to ZP.ACC
```hopper
0867: PLA                ; Pop slot
0868: TSX
0869: TXA
086A: TAY                ; Y = SP (now points above popped item)
086B: LDA [0x61],Y       ; Load from parallel stacks to ACC
086D: STA 0x12           ; ZP.ACC0
086F: LDA [0x63],Y
0871: STA 0x13           ; ZP.ACC1
0873: LDA [0x65],Y
0875: STA 0x14           ; ZP.ACC2
0877: LDA [0x67],Y
0879: STA 0x15           ; ZP.ACC3
```

### Pop 'a' to ZP.NEXT
```hopper
087A: PLA                ; Pop slot
087B: TSX
087C: TXA
087D: TAY                ; Y = SP
087E: LDA [0x61],Y       ; Load from parallel stacks to NEXT
0880: STA 0x16           ; ZP.NEXT0
0882: LDA [0x63],Y
0884: STA 0x17           ; ZP.NEXT1
0886: LDA [0x65],Y
0888: STA 0x18           ; ZP.NEXT2
088A: LDA [0x67],Y
088C: STA 0x19           ; ZP.NEXT3
```

### Call Long.Add (ACC + NEXT → NEXT)
```hopper
088D: LDX #0x1A          ; SysCall.LongAdd
088F: JSR 0x0803         ; BIOS call
                          ; Result is now in ZP.NEXT (0x16-0x19)
```

### RETURN Statement - Marshal Result to Stack
```hopper
0892: TSX
0893: TXA
0894: TAY
0895: LDA 0x16           ; Store result to parallel stacks
0897: STA [0x61],Y
0899: LDA 0x17
089B: STA [0x63],Y
089D: LDA 0x18
089F: STA [0x65],Y
08A1: LDA 0x19
08A3: STA [0x67],Y
08A4: PHA                ; Push slot
```

### Pop Result Back (Unnecessary)
```hopper
08A5: PLA                ; Pop what we just pushed
08A6: TSX
08A7: TXA
08A8: TAY
08A9: LDA [0x61],Y       ; Load it back to NEXT
08AB: STA 0x16
08AD: LDA [0x63],Y
08AF: STA 0x17
08B1: LDA [0x65],Y
08B3: STA 0x18
08B5: LDA [0x67],Y
08B7: STA 0x19
```

### Store Result at BP+5 (Return Slot)
```hopper
08B8: LDA 0x60           ; BP
08BA: CLC
08BB: ADC #0x05          ; BP+5 (param_count=2 + 3)
08BD: TAY
08BE: LDA 0x16           ; Store result at BP+5
08C0: STA [0x61],Y
08C2: LDA 0x17
08C4: STA [0x63],Y
08C6: LDA 0x18
08C8: STA [0x65],Y
08CA: LDA 0x19
08CC: STA [0x67],Y
```

### Function Epilogue (Duplicate!)
```hopper
08CE: LDX 0x60           ; Restore BP
08D0: TXS
08D1: PLA
08D2: STA 0x60
08D4: RTS

08D5: LDX 0x60           ; DUPLICATE EPILOGUE!
08D7: TXS
08D8: PLA
08D9: STA 0x60
08DB: RTS
```

## main() Function (0x08E2 - 0x09F5)

### Initialize Runtime Stack
```hopper
08E2: LDX #0x00          ; SysCall.MemAllocate
08E4: JSR 0x0803
08E7: STZ 0x61           ; Clear low bytes
08E9: STZ 0x63
08EB: STZ 0x65
08ED: STZ 0x67
08EF: LDA 0x1B           ; ZP.IDXH (allocated page)
08F1: STA 0x62           ; runtimeStack0H
08F3: INC A
08F4: STA 0x64           ; runtimeStack1H
08F6: INC A
08F7: STA 0x66           ; runtimeStack2H
08F9: INC A
08FA: STA 0x68           ; runtimeStack3H
```

### Function Prologue
```hopper
08FC: LDA 0x60           ; Save old BP
08FE: PHA
08FF: TSX
0900: STX 0x60           ; New BP
```

### Reserve Local 'sum' AND Return Slot
```hopper
0902: PHA                ; Reserve slot for local 'sum'
0903: PHA                ; Reserve slot for return value
```

### Push First Argument (5)
```hopper
0904: LDA #0x05
0906: STA 0x16
0908: STZ 0x17
090A: STZ 0x18
090C: STZ 0x19
090E: TSX
090F: TXA
0910: TAY
0911: LDA 0x16           ; Marshal to parallel stacks
0913: STA [0x61],Y
0915: LDA 0x17
0917: STA [0x63],Y
0919: LDA 0x18
091B: STA [0x65],Y
091D: LDA 0x19
091F: STA [0x67],Y
0920: PHA                ; Push slot
```

### Push Second Argument (3)
```hopper
0921: LDA #0x03
0923: STA 0x16
0925: STZ 0x17
0927: STZ 0x18
0929: STZ 0x19
092B: TSX
092C: TXA
092D: TAY
092E: LDA 0x16           ; Marshal to parallel stacks
0930: STA [0x61],Y
0932: LDA 0x17
0934: STA [0x63],Y
0936: LDA 0x18
0938: STA [0x65],Y
093A: LDA 0x19
093C: STA [0x67],Y
093D: PHA                ; Push slot
```

### Call add(5, 3)
```hopper
093E: JSR 0x080F         ; Call add()
```

### ✅ FIXED: Pop Arguments and Return Value
```hopper
0941: PLA                ; Pop arg2 (3)
0942: PLA                ; Pop arg1 (5)
0943: PLA                ; Pop return slot
```

### 🔴 BUG: Reading from Wrong Stack Position!
```hopper
0944: TSX
0945: TXA
0946: TAY                ; Y = current SP
0947: LDA [0x61],Y       ; Reading from CURRENT position
0949: STA 0x16           ; But return value is BELOW this!
094B: LDA [0x63],Y
094D: STA 0x17
094F: LDA [0x65],Y
0951: STA 0x18
0953: LDA [0x67],Y
0955: STA 0x19
```

### Store to Local Variable 'sum' at [BP-1]
```hopper
0956: LDA 0x60           ; BP
0958: CLC
0959: ADC #0xFF          ; BP-1 (255 = -1 in 8-bit)
095B: TAY
095C: LDA 0x16           ; Store to sum
095E: STA [0x61],Y
0960: LDA 0x17
0962: STA [0x63],Y
0964: LDA 0x18
0966: STA [0x65],Y
0968: LDA 0x19
096A: STA [0x67],Y
```

### Marshal 'sum' for printf
```hopper
096C: TSX
096D: TXA
096E: TAY
096F: LDA 0x16
0971: STA [0x61],Y
0973: LDA 0x17
0975: STA [0x63],Y
0977: LDA 0x18
0979: STA [0x65],Y
097B: LDA 0x19
097D: STA [0x67],Y
097E: PHA                ; Push slot
```

### Discard Unused printf Return Value
```hopper
097F: PLA                ; Pop unused return
0980: PHA                ; Push it back (odd)
```

### Setup String Pointer
```hopper
0981: LDA #0x07          ; String address low
0983: STA 0x1E           ; ZP.STRL
0985: LDA #0x08          ; String address high
0987: STA 0x1F           ; ZP.STRH
```

### printf() Format String Processing
```hopper
0989: LDY #0x00
098B: LDA [0x1E],Y       ; Load char from format string
098D: CMP #0x04          ; Check for %d marker?
098F: BEQ 0x0999         ; Branch if found
0991: LDX #0x12          ; SysCall.PrintChar
0993: JSR 0x0803
0996: INY
0997: BRA 0x098B         ; Loop
```

### Load 'sum' from [BP-1] for printf %d
```hopper
0999: LDA 0x60           ; BP
099B: CLC
099C: ADC #0xFF          ; BP-1
099E: TAY
099F: LDA [0x61],Y       ; Load sum
09A1: STA 0x16
09A3: LDA [0x63],Y
09A5: STA 0x17
09A7: LDA [0x65],Y
09A9: STA 0x18
09AB: LDA [0x67],Y
09AD: STA 0x19
```

### Push for printf %d Processing
```hopper
09AF: TSX
09B0: TXA
09B1: TAY
09B2: LDA 0x16
09B4: STA [0x61],Y
09B6: LDA 0x17
09B8: STA [0x63],Y
09BA: LDA 0x18
09BC: STA [0x65],Y
09BE: LDA 0x19
09C0: STA [0x67],Y
09C1: PHA                ; Push slot
```

### Pop and Print Integer
```hopper
09C2: PLA                ; Pop slot
09C3: TSX
09C4: TXA
09C5: TAY
09C6: LDA [0x61],Y       ; Load value to ACC
09C8: STA 0x12
09CA: LDA [0x63],Y
09CC: STA 0x13
09CE: LDA [0x65],Y
09D0: STA 0x14
09D2: LDA [0x67],Y
09D4: STA 0x15
09D6: LDX #0x1F          ; SysCall.PrintInt
09D8: JSR 0x0803
```

### Continue printf Processing
```hopper
09DB: LDY #0x06          ; Continue after %d
09DD: LDA [0x1E],Y
09DF: CMP #0x07          ; Check for end?
09E1: BEQ 0x09EB
09E3: LDX #0x12          ; SysCall.PrintChar
09E5: JSR 0x0803
09E8: INY
09E9: BRA 0x09DD         ; Loop
```

### Function Epilogue
```hopper
09EB: PLA                ; Clean up extra stack slot
09EC: LDX 0x60
09EE: TXS
09EF: PLA
09F0: STA 0x60
09F2: RTS
```

## 🔴 **FINAL BUG: Reading Return Value from Wrong Position**

### The Problem at 0x0944-0x0955:

After the three PLAs:
```hopper
0941: PLA                ; Pop arg2
0942: PLA                ; Pop arg1  
0943: PLA                ; Pop return slot
```

The stack pointer now points ABOVE where the return value was stored. But the code at 0x0944 reads from the CURRENT SP position:

```hopper
0944: TSX
0945: TXA
0946: TAY                ; Y = current SP (points ABOVE return value)
0947: LDA [0x61],Y       ; Reading from wrong place!
```

### The Issue:

When `add()` stored the return value at BP+5, that was the return slot position. After popping it with PLA, the SP moves ABOVE that position. Reading from current SP reads garbage.

### The Fix:

Since you already popped the return slot with PLA at 0x0943, you need to either:
1. **Don't pop it** - remove the PLA at 0x0943, then read from current position
2. **Adjust Y** - After getting SP into Y, do `INY` to point back to where the return value was

### Current Stack Flow:
```
Before PLAs: SP → [arg2][arg1][return][sum][BP]
After 3 PLAs: SP points above where return was
Reading at SP: Gets garbage above the stack
```

The return value WAS at the position of the third PLA, but after popping, you're reading from the wrong location.