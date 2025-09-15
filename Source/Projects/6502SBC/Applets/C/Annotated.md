# Disassembly Analysis: Return Statement Bug

## Program Structure
```
0800: JMP 0x08E8         ; Jump to main
0803: JMP [0x0022]       ; BIOS dispatcher 
0806: RTS
0807-0E: "Sum=%d\n\0"    ; String literal
```

## add() Function (0x080F - 0x08E7)

### Function Prologue
```hopper
080F: LDA 0x60           ; Load old BP
0811: PHA                ; Save old BP
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

### Push 'a' Back to Stack (Why?)
```hopper
082B: TSX
082C: TXA
082D: TAY
082E: LDA 0x16           ; Push NEXT back to stack
0830: STA [0x61],Y
0832: LDA 0x17
0834: STA [0x63],Y
0836: LDA 0x18
0838: STA [0x65],Y
083A: LDA 0x19
083C: STA [0x67],Y
083E: DEX                ; Adjust stack
083F: TXS
```

### Load Parameter 'b' from [BP+5]
```hopper
0840: LDA 0x60           ; BP
0842: CLC
0843: ADC #0x05          ; BP+5 (second parameter)
0845: TAY
0846: LDA [0x61],Y       ; Load b[0]
0848: STA 0x16           ; Store in ZP.NEXT0
084A: LDA [0x63],Y       ; Load b[1]
084C: STA 0x17           ; Store in ZP.NEXT1
084E: LDA [0x65],Y       ; Load b[2]
0850: STA 0x18           ; Store in ZP.NEXT2
0852: LDA [0x67],Y       ; Load b[3]
0854: STA 0x19           ; Store in ZP.NEXT3
```

### Push 'b' to Stack
```hopper
0856: TSX
0857: TXA
0858: TAY
0859: LDA 0x16
085B: STA [0x61],Y
085D: LDA 0x17
085F: STA [0x63],Y
0861: LDA 0x18
0863: STA [0x65],Y
0865: LDA 0x19
0867: STA [0x67],Y
0869: DEX
086A: TXS
```

### Pop 'a' to ZP.ACC
```hopper
086B: TSX
086C: INX                ; Move up stack
086D: TXA
086E: TAY
086F: LDA [0x61],Y       ; Pop to ACC
0871: STA 0x12           ; ZP.ACC0
0873: LDA [0x63],Y
0875: STA 0x13           ; ZP.ACC1
0877: LDA [0x65],Y
0879: STA 0x14           ; ZP.ACC2
087B: LDA [0x67],Y
087D: STA 0x15           ; ZP.ACC3
087F: TXS
```

### Pop 'b' to ZP.NEXT
```hopper
0880: TSX
0881: INX
0882: TXA
0883: TAY
0884: LDA [0x61],Y       ; Pop to NEXT
0886: STA 0x16           ; ZP.NEXT0
0888: LDA [0x63],Y
088A: STA 0x17           ; ZP.NEXT1
088C: LDA [0x65],Y
088E: STA 0x18           ; ZP.NEXT2
0890: LDA [0x67],Y
0892: STA 0x19           ; ZP.NEXT3
0894: TXS
```

### Call Long.Add (ACC + NEXT → NEXT)
```hopper
0895: LDX #0x1A          ; SysCall.LongAdd
0897: JSR 0x0803         ; BIOS call
                          ; Result is now in ZP.NEXT (0x16-0x19)
```

### RETURN Statement - Marshal Result
```hopper
089A: TSX
089B: TXA
089C: TAY
089D: LDA 0x16           ; Get result from NEXT
089F: STA [0x61],Y       ; Store at current stack position
08A1: LDA 0x17
08A3: STA [0x63],Y
08A5: LDA 0x18
08A7: STA [0x65],Y
08A9: LDA 0x19
08AB: STA [0x67],Y
08AD: DEX
08AE: TXS
```

### Unnecessary Pop Operation
```hopper
08AF: TSX
08B0: INX
08B1: TXA
08B2: TAY
08B3: LDA [0x61],Y       ; Pop what we just pushed!
08B5: STA 0x16
08B7: LDA [0x63],Y
08B9: STA 0x17
08BB: LDA [0x65],Y
08BD: STA 0x18
08BF: LDA [0x67],Y
08C1: STA 0x19
08C3: TXS
```

### 🐛 **BUG: Wrong Return Value Location!**
```hopper
08C4: LDA 0x60           ; BP
08C6: CLC
08C7: ADC #0x03          ; BP+3 ← WRONG OFFSET!
08C9: TAY
08CA: LDA 0x16           ; Store result at BP+3
08CC: STA [0x61],Y
08CE: LDA 0x17
08D0: STA [0x63],Y
08D2: LDA 0x18
08D4: STA [0x65],Y
08D6: LDA 0x19
08D8: STA [0x67],Y
```

### Function Epilogue (Duplicate!)
```hopper
08DA: LDX 0x60           ; Restore BP
08DC: TXS
08DD: PLA
08DE: STA 0x60
08E0: RTS

08E1: LDX 0x60           ; DUPLICATE EPILOGUE!
08E3: TXS
08E4: PLA
08E5: STA 0x60
08E7: RTS
```

## main() Function (0x08E8 - 0x0A0F)

### Initialize Runtime Stack
```hopper
08E8: LDX #0x00          ; SysCall.MemAllocate
08EA: JSR 0x0803
08ED: STZ 0x61           ; Clear low bytes
08EF: STZ 0x63
08F1: STZ 0x65
08F3: STZ 0x67
08F5: LDA 0x1B           ; ZP.IDXH (allocated page)
08F7: STA 0x62           ; runtimeStack0H
08F9: INC A
08FA: STA 0x64           ; runtimeStack1H
08FC: INC A
08FD: STA 0x66           ; runtimeStack2H
08FF: INC A
0900: STA 0x68           ; runtimeStack3H
```

### Function Prologue
```hopper
0902: LDA 0x60           ; Save old BP
0904: PHA
0905: TSX
0906: STX 0x60           ; New BP
```

### Reserve Space for 'sum' Local Variable
```hopper
0908: PHA                ; Reserve slot (but wrong - should be 4x PHA for 32-bit)
0909: TSX
090A: DEX
090B: TXS
```

### Push First Argument (5)
```hopper
090C: LDA #0x05
090E: STA 0x16
0910: STZ 0x17
0912: STZ 0x18
0914: STZ 0x19
0916: TSX
0917: TXA
0918: TAY
0919: LDA 0x16           ; Marshal to stack
091B: STA [0x61],Y
091D: LDA 0x17
091F: STA [0x63],Y
0921: LDA 0x18
0923: STA [0x65],Y
0925: LDA 0x19
0927: STA [0x67],Y
0929: DEX
092A: TXS
```

### Push Second Argument (3)
```hopper
092B: LDA #0x03
092D: STA 0x16
092F: STZ 0x17
0931: STZ 0x18
0933: STZ 0x19
0935: TSX
0936: TXA
0937: TAY
0938: LDA 0x16           ; Marshal to stack
093A: STA [0x61],Y
093C: LDA 0x17
093E: STA [0x63],Y
0940: LDA 0x18
0942: STA [0x65],Y
0944: LDA 0x19
0946: STA [0x67],Y
0948: DEX
0949: TXS
```

### Call add(5, 3)
```hopper
094A: JSR 0x080F
```

### Clean Up Arguments (Pop 2 args)
```hopper
094D: TSX
094E: INX
094F: TXS
0950: TSX
0951: INX
0952: TXS
```

### Pop Return Value
```hopper
0953: TSX
0954: INX
0955: TXA
0956: TAY
0957: LDA [0x61],Y       ; Get return value
0959: STA 0x16
095B: LDA [0x63],Y
095D: STA 0x17
095F: LDA [0x65],Y
0961: STA 0x18
0963: LDA [0x67],Y
0965: STA 0x19
0967: TXS
```

## 🔴 **ROOT CAUSE IDENTIFIED**

The bug is at address **0x08C7**: The return statement stores the result at `BP+3` instead of the correct location.

### Stack Layout During add() Execution:
```
[saved BP]     ← BP points here
[return addr L]
[return addr H]
[arg 'b' = 3]  ← BP+3 (WRONG target!)
[arg 'a' = 5]  ← BP+4
[return slot]  ← BP+5 (should be here? or elsewhere?)
```

### The Problem:
1. The return value is being stored at **BP+3**, which overwrites the second argument 'b'
2. The actual return slot that main() expects to read from is never written to
3. When main() pops the "return value", it gets uninitialized data (likely 0)

### Expected Behavior:
The return value should be placed where the caller expects it - likely at a specific offset that the caller has reserved, not overwriting the arguments.

### Additional Issues:
1. **Duplicate epilogue** at 0x08DA and 0x08E1
2. **Unnecessary push/pop cycles** that don't serve any purpose
3. **Wrong stack reservation** in main - only 1 byte reserved for 32-bit 'sum'