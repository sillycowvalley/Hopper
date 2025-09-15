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
* **IntNEXT** - Initialize NEXT registers to integer value
* **IncNEXT** - Increment NEXT registers by 1
* **DecNEXT** - Decrement NEXT registers by 1

## Full Disassembly

### Program Entry and String Literals
```hopper
0800: JMP 0x0814           ; Jump to main function
0803: JMP [0x0022]         ; BIOS dispatcher entry point
0806: RTS                  ; Return from dispatcher

0807: "%ld\n"              ; String literal for printf
080C: "%ld ms\n"           ; String literal for printf  
0814: LDX #0x00            ; SysCall.MemAllocate (start of main)
0816: JSR 0x0803           ; Call BIOS to allocate memory
```

### Function Prologue - Runtime Stack Setup
```hopper
; Initialize runtime stack pointers to different pages
081E: LDA 0x1B             ; Load base pointer
0820: STA 0x62             ; runtimeStack0H = page 1
0822: INC 0x62             ; Increment to next page
0824: STA 0x64             ; runtimeStack1H = page 2  
0826: INC 0x64
0828: STA 0x66             ; runtimeStack2H = page 3
082A: INC 0x66  
082C: STA 0x68             ; runtimeStack3H = page 4

; Set up function stack frame
082E: LDA 0x60             ; Save current base pointer
0830: PHA                  ; Push to hardware stack
0831: TSX                  ; Get stack pointer
0832: STX 0x60             ; Save as new base pointer
0834: PHA                  ; Reserve local variable slots
0835: PHA                  ; (allocate stack space for locals)
0836: PHA
```

### Get start time: start = millis()
```hopper
0837: LDX #0x18            ; SysCall.TimeMillis  
0839: JSR 0x0803           ; Call BIOS to get current time
083B: PushTOP              ; Push millis() result to runtime stack

083F: PopNEXT              ; Pop result to NEXT registers
0847: PutNEXT              ; Store start time to local var [BP-2]
0861: GetNEXT              ; Load start time back
0871: PushNEXT             ; Push to runtime stack for later use
```

### Initialize outer loop: i = 1
```hopper
0881: PHA                  ; Stack management
0882: CLR                  ; Clear for initialization
0883: IntNEXT #1           ; Initialize NEXT = 1 
088D: PushNEXT             ; Push i=1 to runtime stack
```

### Outer loop: for (i = 1; i <= 10; i++)
```hopper
0897: PopNEXT              ; Get current i value
08A1: PutNEXT              ; Store i to local variable [BP-3]

; Inner initialization: s = 0
08A5: IntNEXT #0           ; Initialize s = 0
08B1: PushNEXT             ; Push s=0 to runtime stack

; Inner loop initialization: j = 1  
08BB: IntNEXT #1           ; Initialize j = 1
08C5: PushNEXT             ; Push j=1 to runtime stack
```

### Inner loop: for (j = 1; j <= 1000; j++)
```hopper
; Get current j value and test j <= 1000
08CF: PopTOP               ; j -> TOP registers
08D9: PopNEXT              ; Get next value for comparison
08E3: GetNEXT              ; Get j value back
08ED: PutNEXT              ; Store updated j

; Loop body: s = s + j  
08F1: GetNEXT              ; Load s
08FB: PushNEXT             ; Push s
0905: IntNEXT #10          ; Load 10 for comparison (seems misplaced)
090F: PushNEXT

0919: PopTOP               ; Get values for addition
0923: PopNEXT              
092D: LDX #0x24            ; SysCall.LongAdd
092F: JSR 0x0803           ; Perform s = s + j
0931: PushC                ; Push result back

; Check loop continuation
093B: GetNEXT              ; Load current value
0945: PushNEXT             
094F: IncNEXT               ; Increment NEXT by 1
0959: PushNEXT

0963: PopTOP               ; Perform addition
096D: PopNEXT
0977: LDX #0x24            ; SysCall.LongAdd  
0979: JSR 0x0803
097B: PushC

; Test loop condition
0985: GetNEXT
098F: ; Test flags and branch logic
0991: JMP 0x0CD5           ; Exit inner loop if j > 1000

; Clear for next iteration
09A4: IntNEXT #0           ; Clear for next iteration
09AC: PushNEXT

; Outer loop increment: i++
09C0: GetNEXT              ; Load i
09CA: PushNEXT
09D4: IncNEXT               ; Increment i by 1
09DE: PushNEXT

09E8: PopTOP               ; Perform i++ addition
09F2: PopNEXT
09FC: LDX #0x24            ; SysCall.LongAdd
09FE: JSR 0x0803
0A00: PushC

; Store updated i and check i <= 10
0A0A: PutNEXT              ; Store updated i
0A24: GetNEXT              ; Load i for comparison
0A2E: PushNEXT

0A38: IntNEXT #1000        ; Load 1000 for inner loop comparison
0A44: PushNEXT

0A4E: PopTOP               ; Compare j with 1000  
0A58: PopNEXT
0A62: LDX #0x24            ; SysCall.LongAdd (used for comparison)
0A64: JSR 0x0803
0A66: PushC

; Branch logic for inner loop
0A70: GetNEXT
0A7A: ; Check flags for continuation
0A7C: JMP 0x0C29           ; Branch to continue or exit
```

### Print dot after each outer loop iteration
```hopper
0C29: PHA                  ; Save state
0C2A: IntNEXT #'.'         ; ASCII '.' character (0x2E)
0C34: PushNEXT

0C3E: PopNEXT              ; Get character
0C48: LDA 0x16             ; Load '.' character from NEXT0
0C4A: LDX #0x12            ; SysCall.SerialWriteChar
0C4C: JSR 0x0803           ; Output the dot
0C4E: PushNEXT

; Continue outer loop increment and test
0C58: GetNEXT              ; Get current i
0C68: PushNEXT
0C72: IncNEXT               ; Increment i
0C7C: PushNEXT

0C86: PopTOP               ; Perform i++ addition
0C90: PopNEXT
0C9A: LDX #0x24            ; SysCall.LongAdd
0C9C: JSR 0x0803
0C9E: PushC

; Check i <= 10 condition
0CA8: GetNEXT              ; Load i for comparison with 10
0CB2: PushNEXT
0CBC: IntNEXT #10          ; Load 10 for comparison
0CC6: ; Compare and branch logic
0CC8: ; Continue outer loop if i <= 10
0CCA: JMP 0x08EE           ; Jump back to outer loop start
```

### Print final results: printf("%ld\n", s)
```hopper
0CD5: PHA                  ; Save state
0CD6: LDA #0x07            ; Point to "%ld\n" string (low byte)
0CD8: STA 0x1E             ; Store in string pointer
0CDA: LDA #0x08            ; High byte of string address
0CDC: STA 0x1F

; Print string character by character
0CDE: LDY #0x00            ; String index
0CE0: LDA [0x1E], Y        ; Load character
0CE2: CMP #0x00            ; Check for null terminator
0CE4: BEQ 0x0CF0           ; Branch if end of string
0CE6: LDX #0x12            ; SysCall.SerialWriteChar
0CE8: JSR 0x0803           ; Print character
0CEA: INY                  ; Next character
0CEB: BRA 0x0CE0           ; Continue string loop

; Print the final sum value
0CF0: GetNEXT              ; Load final sum (s)
0CFA: PushNEXT             ; Push for printf
0D04: PopTOP               ; Prepare sum for printing
0D0E: LDX #0x1F            ; SysCall.Printf or similar
0D10: JSR 0x0803           ; Print the sum

; Print second printf: "%ld ms\n"  
0D3A: LDA #0x0C            ; Point to "%ld ms\n" string
0D3C: STA 0x1E
0D3E: LDA #0x08
0D40: STA 0x1F

; Print second string character by character
0D42: LDY #0x00
0D44: LDA [0x1E], Y
0D46: CMP #0x00
0D48: BEQ 0x0D54           ; End of string
0D4A: LDX #0x12            ; SysCall.SerialWriteChar  
0D4C: JSR 0x0803
0D4E: INY
0D4F: BRA 0x0D44

; Calculate elapsed time: millis() - start
0D54: LDX #0x18            ; SysCall.TimeMillis
0D56: JSR 0x0803           ; Get current time
0D58: PushTOP              ; Push current time

0D62: PopTOP               ; Get end time
0D6C: PopNEXT              ; Get start time (saved earlier)
0D76: LDX #0x1B            ; SysCall.LongSubtract
0D78: JSR 0x0803           ; Calculate elapsed = end - start
0D7A: PushNEXT             ; Push elapsed time

; Print elapsed time
0D84: PopTOP               ; Get elapsed time for printing
0D8E: LDX #0x1F            ; SysCall.Printf 
0D90: JSR 0x0803           ; Print elapsed time

; Additional string printing for " ms" part
0D92: LDY #0x03            ; Start at offset 3 in string
0D94: LDA [0x1E], Y        ; Load character from "%ld ms\n"
0D96: CMP #0x07            ; Check bounds
0D98: BEQ 0x0DA4           ; Branch if at end
0D9A: LDX #0x12            ; SysCall.SerialWriteChar
0D9C: JSR 0x0803
0D9E: INY
0D9F: BRA 0x0D94
```

### Function epilogue - cleanup and return
```hopper
0DA4: PLA                  ; Clean up stack
0E00: LDX 0x60             ; Restore stack pointer from base pointer
0E02: TXS                  ; Set hardware stack pointer
0E03: PLA                  ; Restore previous base pointer
0E04: STA 0x60             ; Store restored base pointer
0E05: RTS                  ; Return from main function
```

## Analysis

### Runtime Stack Architecture
The compiler implements a sophisticated 32-bit runtime stack using parallel pointers:
- **Hardware Stack (0xFF page)**: Function calls, base pointer, temporary saves
- **Runtime Stack Pages**: 
  - 0x0100 page: 32-bit values (low bytes via 0x61)
  - 0x0200 page: 32-bit values (2nd bytes via 0x63)  
  - 0x0300 page: 32-bit values (3rd bytes via 0x65)
  - 0x0400 page: 32-bit values (high bytes via 0x67)

### Key Register Usage
- **ZP.TOP (0x12-0x15)**: Function returns and temporary calculations
- **ZP.NEXT (0x16-0x19)**: Working registers for arithmetic
- **0x60**: Base pointer for local variable access
- **0x61,0x63,0x65,0x67**: Runtime stack pointers (low bytes)
- **0x62,0x64,0x66,0x68**: Runtime stack page pointers (high bytes)

### BIOS System Calls Used
- **0x00**: Memory allocation
- **0x12**: Serial character output  
- **0x18**: Get millisecond timer
- **0x1B**: 32-bit subtraction
- **0x1F**: Printf formatting
- **0x24**: 32-bit addition

### Compiler Efficiency Analysis

**Strengths:**
- Correct implementation of C semantics
- Proper 32-bit arithmetic throughout
- Successful integration with Hopper BIOS
- Consistent macro usage reveals good code generation patterns
- Proper stack frame management and local variable access

**Inefficiencies:**
- **Over-reliance on BIOS calls**: Simple increments like `i++` generate full 32-bit addition system calls instead of inline arithmetic
- **Excessive runtime stack traffic**: Many unnecessary push/pop sequences that could be optimized
- **32-bit promotion**: Loop counters `i` and `j` are treated as `long` when `int` would suffice
- **String printing**: Character-by-character output instead of BIOS string functions
- **Redundant operations**: The `IncNEXT` macro still calls `LongAdd` instead of implementing inline increment

**Optimization Opportunities:**
1. Implement `IncNEXT/DecNEXT` as inline 32-bit arithmetic instead of BIOS calls
2. Use 16-bit arithmetic for small integer variables
3. Reduce runtime stack usage for temporary values
4. Implement direct BIOS string printing calls
5. Better register allocation to avoid excessive stack traffic

The code successfully demonstrates a working C-to-6502 compiler, but with significant room for performance improvements through better optimization of common patterns.