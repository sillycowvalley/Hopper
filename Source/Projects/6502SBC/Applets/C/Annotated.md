# Disassembly Analysis: Return Statement Bug (Updated)

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
082D: TAY
082E: LDA 0x16           ; Push NEXT to stack
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

### Pop Result Back (Unnecessary)
```hopper
08AF: TSX
08B0: INX
08B1: TXA
08B2: TAY
08B3: LDA [0x61],Y       ; Pop what we just pushed
08B5: STA 0x16
08B7: LDA [0x63],Y
08B9: STA 0x17
08BB: LDA [0x65],Y
08BD: STA 0x18
08BF: LDA [0x67],Y
08C1: STA 0x19
08C3: TXS
```

### ✅ Store Result at BP+5 (FIXED from BP+3)
```hopper
08C4: LDA 0x60           ; BP
08C6: CLC
08C7: ADC #0x05          ; BP+5 (was BP+3 before)
08C9: TAY
08CA: LDA 0x16           ; Store result at BP+5
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

### 🐛 **BUG #1: No Return Value Reservation!**
```hopper
0908: PHA                ; Reserve for local 'sum' (only 1 byte!)
0909: TSX
090A: DEX                ; Adjust stack pointer
090B: TXS
; MISSING: Should reserve 4 bytes for return value HERE!
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
094A: JSR 0x080F         ; Call add()
```

### Clean Up Arguments (Pop 2 args)
```hopper
094D: TSX
094E: INX                ; Pop first arg
094F: TXS
0950: TSX
0951: INX                ; Pop second arg
0952: TXS
```

### 🐛 **BUG #2: Pop from Wrong Location!**
```hopper
0953: TSX
0954: INX
0955: TXA
0956: TAY
0957: LDA [0x61],Y       ; Trying to pop return value
0959: STA 0x16           ; But there's nothing there!
095B: LDA [0x63],Y
095D: STA 0x17
095F: LDA [0x65],Y
0961: STA 0x18
0963: LDA [0x67],Y
0965: STA 0x19
0967: TXS
```

### Store to Local Variable 'sum' at [BP-1]
```hopper
0968: LDA 0x60           ; BP
096A: CLC
096B: ADC #0xFF          ; BP-1 (255 = -1 in 8-bit)
096D: TAY
096E: LDA 0x16           ; Store "return value" to sum
0970: STA [0x61],Y
0972: LDA 0x17
0974: STA [0x63],Y
0976: LDA 0x18
0978: STA [0x65],Y
097A: LDA 0x19
097C: STA [0x67],Y
```

### printf() Call Setup
```hopper
097E: TSX                ; Push 'sum' again for printf
097F: TXA
0980: TAY
0981: LDA 0x16
0983: STA [0x61],Y
0985: LDA 0x17
0987: STA [0x63],Y
0989: LDA 0x18
098B: STA [0x65],Y
098D: LDA 0x19
098F: STA [0x67],Y
0991: DEX
0992: TXS
```

### More Stack Manipulation
```hopper
0993: TSX
0994: INX
0995: TXS
0996: TSX
0997: DEX
0998: TXS
```

### Setup String Pointer
```hopper
0999: LDA #0x07          ; String address low
099B: STA 0x1E           ; ZP.STRL
099D: LDA #0x08          ; String address high
099F: STA 0x1F           ; ZP.STRH
```

### printf() Format String Processing
```hopper
09A1: LDY #0x00
09A3: LDA [0x1E],Y       ; Load char from format string
09A5: CMP #0x04          ; Check for %d marker?
09A7: BEQ 0x09B1         ; Branch if found
09A9: LDX #0x12          ; SysCall.PrintChar
09AB: JSR 0x0803
09AE: INY
09AF: BRA 0x09A3         ; Loop (using 0x80 as BRA opcode?)
```

### Load 'sum' from [BP-1] for printf
```hopper
09B1: LDA 0x60           ; BP
09B3: CLC
09B4: ADC #0xFF          ; BP-1
09B6: TAY
09B7: LDA [0x61],Y       ; Load sum
09B9: STA 0x16
09BB: LDA [0x63],Y
09BD: STA 0x17
09BF: LDA [0x65],Y
09C1: STA 0x18
09C3: LDA [0x67],Y
09C5: STA 0x19
```

### Push for printf
```hopper
09C7: TSX
09C8: TXA
09C9: TAY
09CA: LDA 0x16
09CC: STA [0x61],Y
09CE: LDA 0x17
09D0: STA [0x63],Y
09D2: LDA 0x18
09D4: STA [0x65],Y
09D6: LDA 0x19
09D8: STA [0x67],Y
09DA: DEX
09DB: TXS
```

### Pop and Print Integer
```hopper
09DC: TSX
09DD: INX
09DE: TXA
09DF: TAY
09E0: LDA [0x61],Y       ; Pop value to ACC
09E2: STA 0x12
09E4: LDA [0x63],Y
09E6: STA 0x13
09E8: LDA [0x65],Y
09EA: STA 0x14
09EC: LDA [0x67],Y
09EE: STA 0x15
09F0: TXS
09F1: LDX #0x1F          ; SysCall.PrintInt
09F3: JSR 0x0803
```

### Continue printf Processing
```hopper
09F6: LDY #0x06          ; Continue after %d
09F8: LDA [0x1E],Y
09FA: CMP #0x07          ; Check for end?
09FC: BEQ 0x0A06
09FE: LDX #0x12          ; SysCall.PrintChar
0A00: JSR 0x0803
0A03: INY
0A04: BRA 0x09F8         ; Loop
```

### Function Epilogue
```hopper
0A06: TSX
0A07: INX
0A08: TXS
0A09: LDX 0x60
0A0B: TXS
0A0C: PLA
0A0D: STA 0x60
0A0F: RTS
```

## 🔴 **ROOT CAUSE: MISMATCHED CALLING CONVENTION**

The problem is **NOT** just the BP+5 offset fix. There are **TWO critical bugs**:

### Bug #1: Caller Doesn't Reserve Return Slot
`main()` never reserves space for the return value before pushing arguments. The calling sequence should be:
1. Reserve return slot (4 bytes)
2. Push arguments
3. JSR to function

But `main()` skips step 1!

### Bug #2: Stack Mismatch After Return
After `add()` returns:
- `add()` wrote the return value to BP+5 (a location in the caller's stack frame)
- But `main()` tries to pop a return value from the current stack top
- These are different locations!

### Current Stack During add():
```
[??? garbage]   ← BP+5 (add() writes here)
[arg 'a' = 5]   ← BP+4
[arg 'b' = 3]   ← BP+3  
[ret addr H]    ← BP+2
[ret addr L]    ← BP+1
[saved BP]      ← BP+0
```

After return and popping args, main() expects:
```
[return value]  ← Stack top (but nothing here!)
```

### The Fix Needed:
Either:
1. **Caller reserves space**: `main()` should push 4 dummy bytes before arguments, then `add()` can write to BP+5
2. **Different convention**: `add()` should push the return value onto its stack before returning, not write to BP+offset

The current code has `add()` writing to one place (BP+5) but `main()` reading from another (stack top after popping args).