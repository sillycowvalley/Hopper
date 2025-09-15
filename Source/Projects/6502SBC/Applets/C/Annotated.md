# Disassembly Analysis: With PHA/PLA Changes

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

### RETURN Statement - Marshal Result to Stack
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

### Store Result at BP+5 (Return Slot)
```hopper
08C4: LDA 0x60           ; BP
08C6: CLC
08C7: ADC #0x05          ; BP+5 (param_count=2 + 3)
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

## main() Function (0x08E8 - 0x0A08)

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

### ✅ FIXED: Reserve Local 'sum' AND Return Slot!
```hopper
0908: PHA                ; Reserve slot for local 'sum'
0909: PHA                ; Reserve slot for return value! ✅
```

### Push First Argument (5)
```hopper
090A: LDA #0x05
090C: STA 0x16
090E: STZ 0x17
0910: STZ 0x18
0912: STZ 0x19
0914: TSX
0915: TXA
0916: TAY
0917: LDA 0x16           ; Marshal to stack
0919: STA [0x61],Y
091B: LDA 0x17
091D: STA [0x63],Y
091F: LDA 0x18
0921: STA [0x65],Y
0923: LDA 0x19
0925: STA [0x67],Y
0927: DEX
0928: TXS
```

### Push Second Argument (3)
```hopper
0929: LDA #0x03
092B: STA 0x16
092D: STZ 0x17
092F: STZ 0x18
0931: STZ 0x19
0933: TSX
0934: TXA
0935: TAY
0936: LDA 0x16           ; Marshal to stack
0938: STA [0x61],Y
093A: LDA 0x17
093C: STA [0x63],Y
093E: LDA 0x18
0940: STA [0x65],Y
0942: LDA 0x19
0944: STA [0x67],Y
0946: DEX
0947: TXS
```

### Call add(5, 3)
```hopper
0948: JSR 0x080F         ; Call add()
```

### Clean Up Arguments (Pop 2 args)
```hopper
094B: TSX
094C: INX                ; Pop first arg
094D: TXS
094E: TSX
094F: INX                ; Pop second arg
0950: TXS
```

### 🔴 BUG: Still Trying to Pop Return Value!
```hopper
0951: TSX
0952: INX                ; INX again - WRONG!
0953: TXA
0954: TAY
0955: LDA [0x61],Y       ; Reading from wrong place
0957: STA 0x16           
0959: LDA [0x63],Y
095B: STA 0x17
095D: LDA [0x65],Y
095F: STA 0x18
0961: LDA [0x67],Y
0963: STA 0x19
0965: TXS
```

### Store to Local Variable 'sum' at [BP-1]
```hopper
0966: LDA 0x60           ; BP
0968: CLC
0969: ADC #0xFF          ; BP-1 (255 = -1 in 8-bit)
096B: TAY
096C: LDA 0x16           ; Store to sum
096E: STA [0x61],Y
0970: LDA 0x17
0972: STA [0x63],Y
0974: LDA 0x18
0976: STA [0x65],Y
0978: LDA 0x19
097A: STA [0x67],Y
```

### Marshal 'sum' for printf
```hopper
097C: TSX
097D: TXA
097E: TAY
097F: LDA 0x16
0981: STA [0x61],Y
0983: LDA 0x17
0985: STA [0x63],Y
0987: LDA 0x18
0989: STA [0x65],Y
098B: LDA 0x19
098D: STA [0x67],Y
098F: DEX
0990: TXS
```

### ✅ FIXED: Discard Unused printf Return Value
```hopper
0991: PLA                ; Pop unused return value ✅
0992: PHA                ; Hmm, push it back? Odd...
```

### Setup String Pointer
```hopper
0993: LDA #0x07          ; String address low
0995: STA 0x1E           ; ZP.STRL
0997: LDA #0x08          ; String address high
0999: STA 0x1F           ; ZP.STRH
```

### printf() Format String Processing
```hopper
099B: LDY #0x00
099D: LDA [0x1E],Y       ; Load char from format string
099F: CMP #0x04          ; Check for %d marker?
09A1: BEQ 0x09AB         ; Branch if found
09A3: LDX #0x12          ; SysCall.PrintChar
09A5: JSR 0x0803
09A8: INY
09A9: BRA 0x099D         ; Loop (0x80 = BRA opcode)
```

### Load 'sum' from [BP-1] for printf
```hopper
09AB: LDA 0x60           ; BP
09AD: CLC
09AE: ADC #0xFF          ; BP-1
09B0: TAY
09B1: LDA [0x61],Y       ; Load sum
09B3: STA 0x16
09B5: LDA [0x63],Y
09B7: STA 0x17
09B9: LDA [0x65],Y
09BB: STA 0x18
09BD: LDA [0x67],Y
09BF: STA 0x19
```

### Push for printf
```hopper
09C1: TSX
09C2: TXA
09C3: TAY
09C4: LDA 0x16
09C6: STA [0x61],Y
09C8: LDA 0x17
09CA: STA [0x63],Y
09CC: LDA 0x18
09CE: STA [0x65],Y
09D0: LDA 0x19
09D2: STA [0x67],Y
09D4: DEX
09D5: TXS
```

### Pop and Print Integer
```hopper
09D6: TSX
09D7: INX
09D8: TXA
09D9: TAY
09DA: LDA [0x61],Y       ; Pop value to ACC
09DC: STA 0x12
09DE: LDA [0x63],Y
09E0: STA 0x13
09E2: LDA [0x65],Y
09E4: STA 0x14
09E6: LDA [0x67],Y
09E8: STA 0x15
09EA: TXS
09EB: LDX #0x1F          ; SysCall.PrintInt
09ED: JSR 0x0803
```

### Continue printf Processing
```hopper
09F0: LDY #0x06          ; Continue after %d
09F2: LDA [0x1E],Y
09F4: CMP #0x07          ; Check for end?
09F6: BEQ 0x0A00
09F8: LDX #0x12          ; SysCall.PrintChar
09FA: JSR 0x0803
09FD: INY
09FE: BRA 0x09F2         ; Loop
```

### Function Epilogue
```hopper
0A00: PLA                ; Clean up extra stack slot
0A01: LDX 0x60
0A03: TXS
0A04: PLA
0A05: STA 0x60
0A07: RTS
```

## 🔴 **CRITICAL BUG REMAINS**

### Good News:
1. ✅ Return slot is now reserved (PHA at 0x0909)
2. ✅ Using PLA to discard unused values (0x0991)

### Bad News - The Real Problem at 0x0951-0x0952:
After popping the 2 arguments, the code does **ANOTHER INX** to try to get the return value!

### Stack After Popping Arguments:
```
[return value]  ← Stack pointer is HERE after 2 pops
[local 'sum']   ← BP-1
[saved BP]      ← BP+0
```

The return value is **already at the stack top** after popping arguments. The extra INX at 0x0952 moves **past** the return value to garbage!

### The Fix:
**Remove the TSX/INX/TXS sequence at 0x0951-0x0952**. After popping arguments, the return value is already at the correct position.

### Current Wrong Sequence:
```hopper
094B: TSX/INX/TXS       ; Pop arg 1
094E: TSX/INX/TXS       ; Pop arg 2  
0951: TSX/INX/TXS       ; WRONG - moves past return value!
0955: Read value        ; Reading garbage
```

### Should Be:
```hopper
094B: TSX/INX/TXS       ; Pop arg 1
094E: TSX/INX/TXS       ; Pop arg 2
0951: TSX/TXA/TAY       ; Just get current position
0955: Read value        ; Return value is here!
```