# Annotated Disassembly - For Loop Bug Analysis

## Program Structure
```
void main() {
    int i;                    // [BP+0]
    for (i=0; i < 10; i++)    // init, condition, update
    {
        putchar('.');         // body
    }
}
```

## Key Code Sections

### Main Entry (0807)
```asm
0807: A2 00        LDX #$00          ; Push return slot for main
0809: 20 03 08     JSR $0803         ; Call BIOS dispatch
```

### For Loop Initialization (Before 0883)
```asm
; Assignment i = 0
[... stack setup ...]
08FB: A9 00        LDA #$00          ; Load 0
08FD: 69 00        ADC #$00          ; Add carry (part of assignment)
08FF: 91 61        STA ($61),Y       ; Store to i's location [BP+0]
[... store rest of 32-bit value as zeros ...]

; CRITICAL: Init expression result pushed but properly popped
0877: 68           PLA               ; Discard init expression result ✓
```

### Loop Start (0883) - WHERE THE BUG BEGINS
```asm
0883: A8           TAY               ; Setup Y for stack access
0884: B1 61        LDA ($61),Y       ; Load i[0] from stack
0886: 85 16        STA $16           ; Store in NEXT0
0888: B1 63        LDA ($63),Y       ; Load i[1] 
088A: 85 17        STA $17           ; Store in NEXT1
088C: B1 65        LDA ($65),Y       ; Load i[2]
088E: 85 18        STA $18           ; Store in NEXT2
0890: B1 67        LDA ($67),Y       ; Load i[3]
0892: 85 19        STA $19           ; Store in NEXT3
```

**⚠️ FIRST BUG: Wrong stack offset!**
The code uses ($61),Y, ($63),Y, ($65),Y, ($67),Y - these are ZP locations 61, 63, 65, 67.
But the 6502 indirect indexed mode should use a SINGLE base pointer with Y as offset!
This should be: `LDA ($61),Y` with Y=0,1,2,3 for the 4 bytes.

### Comparison i < 10
```asm
; Push i on evaluation stack
0894: 48           PHA               ; Push placeholder

; Load 10 into TOP for comparison  
08A6: 48           PHA               ; Push placeholder
08A7: A9 0A        LDA #$0A          ; Load 10
08A9: 85 16        STA $16           ; Store in NEXT0
08AB: 64 17        STZ $17           ; Zero NEXT1-3
08AD: 64 18        STZ $18
08AF: 64 19        STZ $19

; Move values for comparison
08E0: 68           PLA               ; Get back
[... stack juggling ...]

; Call Long.LT comparison
08FC: A2 20        LDX #$20          ; SysCall.LongLT
08FE: 20 03 08     JSR $0803         ; Call BIOS

; Check result
0907: A5 16        LDA $16           ; Load comparison result
0909: 05 17        ORA $17           ; OR with other bytes
090B: 05 18        ORA $18
090D: 05 19        ORA $19
090F: D0 03        BNE +3            ; If not zero (true), skip JMP
0911: 4C D7 09     JMP $09D7         ; Exit loop if false
```

### Loop Body - putchar('.')
```asm
0914: 48           PHA               ; Push return slot
0915: A9 2E        LDA #$2E          ; Load '.' (0x2E)
[... push as 32-bit ...]
0960: A2 12        LDX #$12          ; SysCall.putchar  
0962: 20 03 08     JSR $0803         ; Call BIOS
[... cleanup ...]
```

### Increment i++ (09A0)
```asm
; Load i from stack
0980: B1 61        LDA ($61),Y       ; Load i[0] (WRONG ADDRESSING!)
[... load other bytes ...]

; Increment
09A0: 18           CLC
09A1: A5 16        LDA $16           ; Load NEXT0
09A3: 69 01        ADC #$01          ; Add 1
09A5: 85 16        STA $16           ; Store back
[... handle carry for 32-bit ...]

; Store back to stack
09C2: A5 16        LDA $16
09C4: 91 61        STA ($61),Y       ; Store i[0] (WRONG!)
[... store other bytes ...]

09D4: 68           PLA               ; ✓ Pop increment result (FIXED)
09D5: 4C 83 08     JMP $0883         ; Back to loop start
```

## THE BUGS FOUND

### 🐛 Bug #1: Incorrect Indirect Addressing
The code uses `($61),Y`, `($63),Y`, `($65),Y`, `($67),Y` to access the 4 bytes of the integer.
- **Wrong**: Different base addresses (61, 63, 65, 67)
- **Should be**: Same base address with Y=0,1,2,3

This causes the value to be read from/written to incorrect memory locations!

### 🐛 Bug #2: Corrupted Stack Pointer
The stack pointer manipulation at 09BF uses:
```asm
09BF: 69 FF        ADC #$FF          ; This should be SBC #$01 or ADC #$FF with SEC
```
Without proper carry flag setup, this could corrupt the stack offset.

### Why Only 9 Dots?
The incorrect memory addressing likely causes:
1. The initial value of i might not be properly initialized to 0
2. The increment might not properly update the actual i variable
3. The comparison might be reading garbage values

With corrupted values, the loop likely starts with i=1 instead of i=0, or exits one iteration early, resulting in only 9 dots instead of 10.

## Fix Required in CodeGen
The code generator needs to emit correct indirect indexed addressing:
```asm
; Correct pattern for accessing 32-bit value at [BP+offset]:
LDY #offset
LDA (BP),Y    ; byte 0
INY
LDA (BP),Y    ; byte 1  
INY
LDA (BP),Y    ; byte 2
INY  
LDA (BP),Y    ; byte 3
```

Instead of using different zero-page addresses for each byte!