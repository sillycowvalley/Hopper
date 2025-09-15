# Disassembly of Fibonacci Program with Optimization Analysis

## Memory Layout
```
0800-0806: Entry and BIOS dispatch
0807-0821: String literals
0822-0ABF: fibo() function
0AC0-0E02: main() function and runtime
```

## String Literals
```hopper
0807: "Fibo(%d)=%d\n"     ; 14 bytes including null
0814: "Elapsed: %ld ms\n" ; 17 bytes including null
```

## fibo() Function (0822-0ABF)
```hopper
; Function prologue
0822: LDA 0x60         ; Save old BP
0824: PHA
0825: TSX
0826: STX 0x60         ; New BP = SP

; Load parameter n from BP+4
0828: LDA 0x60
082A: CLC
082B: ADC #0x04        ; BP+4 (skip return addr + saved BP)
082D: TAY
082E-0841: [Load [BP+4] to NEXT via indirect indexed]
0842-0855: [Push NEXT to stack]

; Push literal 1
0856: LDA #0x01
0858: STA 0x16         ; NEXT = 1
085A-085C: STZ 0x17-19  ; Clear upper bytes
085E-0871: [Push NEXT to stack]

; Pop both values for comparison
0872-0885: [Pop to TOP]
0886-0899: [Pop to NEXT]

; Compare n <= 1
089A: LDX #0x24        ; SysCall.LongLE
089C: JSR 0x0803
089F-08B0: [Push result (C flag as 0/1)]

; Test condition
08B1-08C4: [Pop to NEXT]
08C5: LDA 0x16         ; Test result
08C7: ORA 0x17
08C9: ORA 0x18
08CB: ORA 0x19
08CD: BNE 0x08D2       ; If true (n <= 1)
08CF: JMP 0x092E       ; Jump to recursive case

; Then block: return n
08D2: LDA 0x60
08D4: CLC
08D5: ADC #0x04        ; BP+4 (parameter n)
08D7: TAY
08D8-08EB: [Load n to NEXT]
08EC-08FF: [Push NEXT to stack]
0900-0913: [Pop to NEXT]
0914-0927: [Store NEXT at BP+5 (return slot)]
0928: LDX 0x60         ; Function epilogue
092A: TXS
092B: PLA
092C: STA 0x60
092E: RTS

; Recursive case: fibo(n-1) + fibo(n-2)
092F: PHA              ; ⚠️ OPTIMIZATION: Unnecessary push/pop
0930-0943: [Load n from BP+4]
0944-0957: [Push n to stack]
0958-096B: [Push literal 1]
096C-097F: [Pop to TOP]
0980-0993: [Pop to NEXT]
0994: LDX #0x1B        ; SysCall.LongSub
0996: JSR 0x0803
0999-09AC: [Push result]
09AD: JSR 0x0825       ; Recursive call fibo(n-1)
09B0: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
09B1: PHA              ; ⚠️ OPTIMIZATION: Unnecessary

; Second recursive call preparation
09B2-09C5: [Load n from BP+4]
09C6-09D9: [Push n]
09DA-09ED: [Push literal 2]
09EE-0A01: [Pop to TOP]
0A02-0A15: [Pop to NEXT]
0A16: LDX #0x1B        ; SysCall.LongSub
0A18: JSR 0x0803
0A1B-0A2E: [Push result]
0A2F: JSR 0x0825       ; Recursive call fibo(n-2)
0A32: PLA              ; ⚠️ OPTIMIZATION: Unnecessary

; Add results
0A33-0A46: [Pop fibo(n-2) result]
0A47-0A5A: [Pop fibo(n-1) result]
0A5B-0A6E: [Pop to TOP and NEXT]
0A6F: LDX #0x1A        ; SysCall.LongAdd
0A71: JSR 0x0803
0A74-0A87: [Push sum]
0A88-0A9B: [Pop result]
0A9C-0AAF: [Store at BP+5 (return)]
0AB0: LDX 0x60         ; Epilogue
0AB2: TXS
0AB3: PLA
0AB4: STA 0x60
0AB6: RTS

; ⚠️ DUPLICATE EPILOGUE
0AB7: LDX 0x60
0AB9: TXS
0ABA: PLA
0ABB: STA 0x60
0ABD: RTS
```

## main() Function (0AC3-0E02)
```hopper
; Stack initialization
0AC3: LDX #0x00        ; SysCall.MemAllocate
0AC5: JSR 0x0803
0AC8-0AD1: [Initialize runtime stack pointers]
0AD2: LDA 0x1B         ; IDXH (allocated page)
0AD4-0ADB: [Set stack page pointers 62,64,66,68]

; Function prologue
0ADC: LDA 0x60
0ADE: PHA
0ADF: TSX
0AE0: STX 0x60

; long start = millis()
0AE2: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0AE3: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0AE4: LDX #0x18        ; SysCall.TimeMillis
0AE6: JSR 0x0803
0AE9-0AFC: [Push TOP (millis result)]
0AFD-0B10: [Pop and store at BP-1 (start)]

; int f = 12
0B11-0B24: [Push 12]
0B25: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
0B26: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0B27-0B3A: [Push literal 12 again - REDUNDANT!]
0B3B-0B4E: [Pop]
0B4F-0B62: [Store at BP-2 (f)]

; int total = fibo(f)
0B63-0B76: [Push f]
0B77: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
0B78: PHA              ; ⚠️ OPTIMIZATION: Unnecessary  
0B79: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0B7A-0B8D: [Load f from BP-2]
0B8E-0BA1: [Push f]
0BA2: JSR 0x0825       ; Call fibo(f)
0BA5: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
0BA6-0BB9: [Pop result]
0BBA-0BCD: [Store at BP-3 (total)]

; long elapsed = millis() - start
0BCE-0BE1: [Push total - UNNECESSARY!]
0BE2: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
0BE3: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0BE4: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0BE5: LDX #0x18        ; SysCall.TimeMillis
0BE7: JSR 0x0803
0BEA-0BFD: [Push millis()]
0BFE-0C11: [Load start from BP-1]
0C12-0C25: [Push start]
0C26-0C39: [Pop to TOP and NEXT]
0C3A: LDX #0x1B        ; SysCall.LongSub
0C3C: JSR 0x0803
0C3F-0C52: [Push result]
0C53-0C66: [Pop elapsed]
0C67-0C7A: [Store at BP-4 (elapsed)]

; printf("Fibo(%d)=%d\n", f, total)
0C7B-0C8E: [Push elapsed - UNNECESSARY!]
0C8F: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
0C90: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0C91: LDA #0x07        ; String address low
0C93: STA 0x1E
0C95: LDA #0x08        ; String address high
0C97: STA 0x1F
0C99-0CA8: [Print format string loop]
0CA9-0CBC: [Load f from BP-2]
0CBD-0CD0: [Push f]
0CD1-0CE4: [Pop and print %d]
0CE5-0CF8: [Continue format string]
0CF9-0D0C: [Load total from BP-3]
0D0D-0D20: [Push total]
0D21-0D34: [Pop and print %d]
0D35-0D48: [Continue format string]

; printf("Elapsed: %ld ms\n", elapsed)
0D49: PLA              ; ⚠️ OPTIMIZATION: Unnecessary
0D4A: PHA              ; ⚠️ OPTIMIZATION: Unnecessary
0D4B: LDA #0x14        ; String address low
0D4D: STA 0x1E
0D4F: LDA #0x08        ; String address high
0D51: STA 0x1F
0D53-0D62: [Print format string loop]
0D63-0D76: [Load elapsed from BP-4]
0D77-0D8A: [Push elapsed]
0D8B-0D9E: [Pop and print %ld]
0D9F-0DB2: [Continue format string]

; Epilogue
0DB3: PLA
0DB4: LDX 0x60
0DB6: TXS
0DB7: PLA
0DB8: STA 0x60
0DBA: RTS
```

# Major Optimization Opportunities

## 1. **Excessive Push/Pop Operations** (Critical)
- Every expression generates unnecessary PHA/PLA pairs
- Example: Lines 092F, 09B0-09B1, 0A32, 0AE2-0AE3, 0B25-0B26, etc.
- **Impact**: ~30% of code is unnecessary stack operations
- **Fix**: Track expression results in compiler, avoid redundant pushes

## 2. **Duplicate Variable Loads**
- Variables are loaded multiple times for the same use
- Example: `f` loaded twice at 0B27-0B3A and 0B7A-0B8D
- **Fix**: Implement simple CSE (Common Subexpression Elimination)

## 3. **Redundant Epilogue**
- Two identical epilogues in fibo() at 0928 and 0AB7
- **Fix**: Single exit point optimization

## 4. **32-bit Operations for Small Values**
- Using 32-bit math for values that fit in 8/16 bits
- All loop counters, array indices could use 8-bit math
- **Fix**: Type-aware code generation

## 5. **Inefficient Parameter Access**
- Recalculating BP+offset every time
- Could cache frequently accessed locals in zero page

## 6. **Printf Overhead**
- Format string parsing at runtime
- Could pre-compile format strings

## Estimated Performance Impact
Removing these inefficiencies could reduce code size by **40-50%** and improve execution speed by **2-3x**, especially for the recursive Fibonacci function which compounds the overhead.