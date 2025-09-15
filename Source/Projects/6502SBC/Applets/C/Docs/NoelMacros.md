# RetroLab Benchmark Disassembly

## C Source Context
```c
// Noel's RetroLab Benchmark
void main() {
    long s;
    long start = millis();
    int i; int j;
    for (i = 1; i <= 10; i++) {
        s = 0;
        for (j = 1; j <= 1000; j++) {
            s = s + j;
        }
        putchar('.');
    }
    printf("%ld\n", s);
    printf("%ld ms\n", millis() - start);
}
```

## Compiler Macros Used
The following sequences are represented as macros for clarity:
* **PushC** - Push carry result to runtime stack
* **PushNEXT** - Push NEXT registers to runtime stack  
* **PopNEXT** - Pop from runtime stack to NEXT registers
* **PushTOP** - Push TOP registers to runtime stack
* **PopTOP** - Pop from runtime stack to TOP registers
* **PutNEXT** - Store NEXT registers to local variable
* **GetNEXT** - Load local variable to NEXT registers
* **LongNEXT** - Initialize NEXT registers to long value
* **IncNEXT** - Increment NEXT registers by 1
* **DecNEXT** - Decrement NEXT registers by 1
* **LongADD** - Add TOP and NEXT, result in NEXT
* **LongLE** - Less than or Equal comparison, sets flags
* **LongSUB** - Subtract TOP from NEXT, result in NEXT

## Complete Disassembly

### Program Entry and String Literals
```hopper
0800: JMP 0x0814           ; 4C 14 08
0803: JMP [0x0022]         ; 6C 22 00 - BIOS dispatcher 
0806: RTS                  ; 60

; String literals embedded in code
0807: .byte 0x25,0x6C,0x64,0x0A,0x00  ; "%ld\n"
080C: .byte 0x25,0x6C,0x64,0x20,0x6D,0x73,0x0A,0x00  ; "%ld ms\n"
```

### Main Function Start
```hopper
0814: LDX #0x00            ; A2 00 - SysCall.MemAllocate
0816: JSR 0x0803           ; 20 03 08

; Embedded debug string
0819: .byte 0x64,0x61,0x64,0x63,0x64,0x65,0x64,0x67  ; "dadcdedg"
```

### Runtime Stack Initialization  
```hopper
0821: LDA 0x1B             ; A5 1B - Load base pointer
0823: STA 0x62             ; 85 62 - runtimeStack0H
0825: INC A                ; 1A - Increment accumulator
0826: STA 0x64             ; 85 64 - runtimeStack1H  
0828: INC A                ; 1A
0829: STA 0x66             ; 85 66 - runtimeStack2H
082B: INC A                ; 1A
082C: STA 0x68             ; 85 68 - runtimeStack3H
```

### Function Prologue
```hopper
082E: LDA 0x60             ; A5 60 - Save current base pointer
0830: PHA                  ; 48
0831: TSX                  ; BA - Get stack pointer  
0832: STX 0x60             ; 86 60 - Set new base pointer
0834: PHA                  ; 48 - Reserve local variable space
0835: PHA                  ; 48
0836: PHA                  ; 48
```

### Call millis() for start time
```hopper
0837: LDX #0x18            ; A2 18 - SysCall.TimeMillis
0839: JSR 0x0803           ; 20 03 08
083B: PushTOP              ; Push millis() result to runtime stack
0850: PopNEXT              ; Pop result to NEXT registers
0862: PutNEXT              ; Store start time to local var [BP-2]
0878: PushNEXT             ; Push to runtime stack for later use
```

### Initialize i = 1  
```hopper
088D: PHA                  ; 48
088E: PHA                  ; 48
088F: LongNEXT #1          ; Initialize NEXT = 1
0899: PushNEXT             ; Push i=1 to runtime stack
```

### Outer loop condition: i <= 10
```hopper
08AE: PopNEXT              ; Get current i value
08C1: PutNEXT              ; Store i to local variable [BP-3]
08D7: PushNEXT             ; Push to stack
08EB: GetNEXT              ; Get from [BP-3]
0901: PushNEXT             ; Push i
0915: LongNEXT #10         ; Load 10 for comparison
091F: PushNEXT             ; Push 10
0934: PopTOP               ; Pop 10 to TOP
0948: PopNEXT              ; Pop i to NEXT
095C: LongLE               ; Compare i <= 10 (sets carry if true)
095F: PushC                ; Push comparison result
0972: PopNEXT              ; Get result
0985: LDA 0x16             ; A5 16 - Test all bytes of result
0987: ORA 0x17             ; 05 17
0989: ORA 0x18             ; 05 18
098B: ORA 0x19             ; 05 19
098D: BNE 0x0992           ; D0 03 - Branch if non-zero (i <= 10)
098F: JMP 0x0CD5           ; 4C D5 0C - Exit outer loop if zero (i > 10)
```

### Inner loop setup - s = 0, j = 1
```hopper
0992: LongNEXT #0          ; Initialize s = 0
099A: PushNEXT             ; Push s=0 to runtime stack
09AF: PopNEXT              ; Get s
09C2: PutNEXT              ; Store s to local variable [BP+0]
09D8: PushNEXT             ; Push s
09ED: LongNEXT #1          ; Initialize j = 1
09F7: PushNEXT             ; Push j=1 to runtime stack
```

### Inner loop condition: j <= 1000
```hopper
0A0C: PopNEXT              ; Get j
0A1F: PutNEXT              ; Store j to local variable [BP-4]
0A35: PushNEXT             ; Push j
0A49: GetNEXT              ; Get j from [BP-4]
0A5F: PushNEXT             ; Push j
0A73: LongNEXT #1000       ; Load 1000 (0x03E8)
0A7F: PushNEXT             ; Push 1000
0A93: PopTOP               ; Pop 1000 to TOP
0AA7: PopNEXT              ; Pop j to NEXT
0ABA: LongLE               ; Compare j <= 1000 (sets carry if true)
0ABE: PushC                ; Push comparison result
0AD1: PopNEXT              ; Get result
0AE4: LDA 0x16             ; A5 16 - Test result
0AE6: ORA 0x17             ; 05 17
0AE8: ORA 0x18             ; 05 18
0AEA: ORA 0x19             ; 05 19
0AEC: BNE 0x0AF1           ; D0 03 - Branch if non-zero (j <= 1000)
0AEE: JMP 0x0C29           ; 4C 29 0C - Exit inner loop if zero (j > 1000)
```

### Inner loop body: s = s + j
```hopper
0AF1: GetNEXT              ; Load s from [BP+0]
0B07: PushNEXT             ; Push s
0B1B: GetNEXT              ; Load j from [BP-4]
0B31: PushNEXT             ; Push j
0B46: PopTOP               ; Pop j to TOP
0B5A: PopNEXT              ; Pop s to NEXT
0B6D: LongADD              ; Perform s = s + j (addition operation)
0B71: PushNEXT             ; Push result (s = s + j)
0B86: PopNEXT              ; Get result
0B99: PutNEXT              ; Store updated s to [BP+0]
0BAF: PushNEXT             ; Push s
```

### Inner loop increment: j++
```hopper
0BC4: GetNEXT              ; Load j from [BP-4]
0BDA: PushNEXT             ; Push j
0BEE: IncNEXT               ; Increment j by 1 (inline 32-bit arithmetic)
0C07: PutNEXT              ; Store j++ to [BP-4]
0C1E: JMP 0x0A4E           ; 4C 4E 0A - Jump back to inner loop condition
```

### Print dot after each outer loop iteration
```hopper
0C21: PHA                  ; 48
0C22: LongNEXT #'.'        ; Load ASCII '.' (0x2E)
0C2C: PushNEXT             ; Push '.'
0C41: PopNEXT              ; Get character
0C54: LDA 0x16             ; A5 16 - Load character
0C56: LDX #0x12            ; A2 12 - SysCall.PrintChar
0C58: JSR 0x0803           ; 20 03 08
0C5A: PushNEXT             ; Push result
```

### Outer loop increment: i++
```hopper
0C6F: GetNEXT              ; Get i from [BP-3]
0C85: PushNEXT             ; Push i
0C99: IncNEXT               ; Increment i by 1 (inline 32-bit arithmetic)
0CB2: PutNEXT              ; Store i++ to [BP-3]
0CC9: JMP 0x08EE           ; 4C EE 08 - Jump back to outer loop
```

### Print final results: printf("%ld\n", s)
```hopper
0CCC: PHA                  ; 48
0CCD: LDA #0x07            ; A9 07 - String pointer setup
0CCF: STA 0x1E             ; 85 1E
0CD1: LDA #0x08            ; A9 08
0CD3: STA 0x1F             ; 85 1F

0CD5: LDY #0x00            ; A0 00 - Print "%ld\n" string
0CD7: LDA [0x1E], Y        ; B1 1E
0CD9: CMP #0x00            ; C0 00
0CDB: BEQ 0x0CE3           ; F0 08
0CDD: LDX #0x12            ; A2 12 - SysCall.PrintChar
0CDF: JSR 0x0803           ; 20 03 08
0CE1: INY                  ; C8
0CE2: BRA 0x0CD7           ; 80 F2

0CE3: GetNEXT              ; Get final sum for printing from [BP+0]
0CF9: PushNEXT             ; Push sum
0D0E: PopTOP               ; Pop sum to TOP for printf
0D21: LDX #0x1F            ; A2 1F - SysCall.LongPrint
0D23: JSR 0x0803           ; 20 03 08

0D25: LDY #0x03            ; A0 03 - Print remaining part of string
0D27: LDA [0x1E], Y        ; B1 1E
0D29: CMP #0x04            ; C0 04
0D2B: BEQ 0x0D33           ; F0 08
0D2D: LDX #0x12            ; A2 12 - SysCall.PrintChar
0D2F: JSR 0x0803           ; 20 03 08
0D31: INY                  ; C8
0D32: BRA 0x0D27           ; 80 F2
```

### Print elapsed time: printf("%ld ms\n", millis() - start)
```hopper
0D34: PLA                  ; 68
0D35: PHA                  ; 48

0D36: LDA #0x0C            ; A9 0C - Setup for second string
0D38: STA 0x1E             ; 85 1E
0D3A: LDA #0x08            ; A9 08
0D3C: STA 0x1F             ; 85 1F

0D3E: LDY #0x00            ; A0 00 - Print "%ld ms\n" string
0D40: LDA [0x1E], Y        ; B1 1E
0D42: CMP #0x00            ; C0 00
0D44: BEQ 0x0D4C           ; F0 08
0D46: LDX #0x12            ; A2 12 - SysCall.PrintChar
0D48: JSR 0x0803           ; 20 03 08
0D4A: INY                  ; C8
0D4B: BRA 0x0D40           ; 80 F2

0D4C: PHA                  ; 48
0D4D: LDX #0x18            ; A2 18 - SysCall.TimeMillis (get end time)
0D4F: JSR 0x0803           ; 20 03 08

0D51: PushTOP              ; Push current time
0D65: GetNEXT              ; Get start time from [BP-2]
0D7B: PushNEXT             ; Push start time
0D90: PopTOP               ; Pop end time to TOP
0DA4: PopNEXT              ; Pop start time to NEXT
0DB7: LongSUB              ; Calculate elapsed = end - start
0DBB: PushNEXT             ; Push elapsed time
0DD0: PopTOP               ; Pop elapsed time to TOP for printf
0DE3: LDX #0x1F            ; A2 1F - SysCall.LongPrint
0DE5: JSR 0x0803           ; 20 03 08

0DE7: LDY #0x03            ; A0 03 - Print remaining string
0DE9: LDA [0x1E], Y        ; B1 1E
0DEB: CMP #0x07            ; C0 07
0DED: BEQ 0x0DF5           ; F0 08
0DEF: LDX #0x12            ; A2 12 - SysCall.PrintChar
0DF1: JSR 0x0803           ; 20 03 08
0DF3: INY                  ; C8
0DF4: BRA 0x0DE9           ; 80 F2

0DF6: PLA                  ; 68
```

### Function Epilogue
```hopper
0DF7: LDX 0x60             ; A6 60 - Restore stack pointer
0DF9: TXS                  ; 9A
0DFA: PLA                  ; 68 - Restore base pointer
0DFB: STA 0x60             ; 85 60
0DFD: RTS                  ; 60 - Return
```

## Analysis

### Syscalls Identified:
- **0x00**: MemAllocate
- **0x12**: PrintChar (putchar)  
- **0x18**: TimeMillis (millis)
- **0x1A**: LongAdd (LongADD macro)
- **0x1B**: LongSub (LongSUB macro) 
- **0x1F**: LongPrint (printf)
- **0x24**: LongLE (Less than or Equal comparison)

### Key Observations:

1. **Correct Comparison Logic**: The compiler properly generates **LongLE (0x24)** syscalls for `<=` comparisons at addresses 0x095C and 0x0ABD, not addition as initially suspected.

2. **Proper Operation Selection**: 
   - **LongLE** for loop conditions (`i <= 10`, `j <= 1000`)
   - **LongADD** for arithmetic operations (`s = s + j`)
   - **LongSUB** for time calculation (`millis() - start`)

3. **Efficient Optimization**: The compiler uses inline **IncNEXT** macros for simple increments instead of expensive syscalls, showing smart optimization decisions.

4. **Sophisticated Runtime Stack**: Uses 4-page parallel stack system with indexed indirect addressing for 32-bit value management.

5. **Complete BIOS Integration**: All I/O, timing, and complex arithmetic operations are handled via syscalls while simple operations are inlined.

6. **Correct Loop Structure**: 
   - Both loops use proper comparison syscalls and test the carry flag result
   - Loop exit logic based on comparison results, not arithmetic overflow
   - Manual increments avoid syscall overhead for simple operations

### Compiler Quality Assessment:

**Strengths:**
- Correct implementation of C comparison semantics
- Smart optimization choices (inline vs syscall arithmetic)
- Proper 32-bit arithmetic throughout
- Successful BIOS integration
- Working runtime stack system

**Areas for Potential Optimization:**
- Excessive runtime stack traffic for temporary values
- Could reduce push/pop sequences in some cases
- String printing could use BIOS string functions instead of character-by-character output

The code demonstrates a sophisticated and correctly functioning C-to-6502 compiler with proper comparison operations, efficient arithmetic optimization, and comprehensive system integration.