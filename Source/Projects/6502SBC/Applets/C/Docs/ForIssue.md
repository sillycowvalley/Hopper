# Complete Annotated Disassembly - For Loop Bug Analysis

## Source Code
```c
void main() {
    int i;                      // Local at [BP-4]
    for (i=0; i < 10; i++)
    {
        putchar('.');
    }
}
```

## Complete Disassembly with Annotations

### Program Entry and BIOS Dispatcher
```asm
0800: 4C 07 08     JMP 0x0807         ; Jump to program start
0803: 6C 22 00     JMP [0x0022]       ; BIOS dispatch indirect jump
0806: 60           RTS                ; Return
```

### Program Initialization
```asm
0807: A2 00        LDX #0x00          ; Push return slot for main()
0809: 20 03 08     JSR 0x0803         ; Call BIOS (does nothing for main)
```

### Initialize Runtime Stack Pointers
```asm
080C: 64 61        STZ 0x61           ; runtimeStack0L = 0
080E: 64 63        STZ 0x63           ; runtimeStack1L = 0  
0810: 64 65        STZ 0x65           ; runtimeStack2L = 0
0812: 64 67        STZ 0x67           ; runtimeStack3L = 0

0814: A5 1B        LDA 0x1B           ; Load ZP.IDXH (usually 0x01)
0816: 85 62        STA 0x62           ; runtimeStack0H = 0x01
0818: 1A           INC A              ; A = 0x02
0819: 85 64        STA 0x64           ; runtimeStack1H = 0x02  
081B: 1A           INC A              ; A = 0x03
081C: 85 66        STA 0x66           ; runtimeStack2H = 0x03
081E: 1A           INC A              ; A = 0x04
081F: 85 68        STA 0x68           ; runtimeStack3H = 0x04
```

### Function Prologue for main()
```asm
0821: A5 60        LDA 0x60           ; Load old BP
0823: 48           PHA                ; Save old BP on stack
0824: BA           TSX                ; Get stack pointer
0825: 86 60        STX 0x60           ; Set new BP = SP
0827: 48           PHA                ; Reserve space for local var i
```

### Initialize i = 0
```asm
0828: 64 16        STZ 0x16           ; NEXT0 = 0
082A: 64 17        STZ 0x17           ; NEXT1 = 0
082C: 64 18        STZ 0x18           ; NEXT2 = 0
082E: 64 19        STZ 0x19           ; NEXT3 = 0

0830: BA           TSX                ; Get stack pointer
0831: 8A           TXA                ; Transfer to A
0832: A8           TAY                ; Transfer to Y

; Store 0 to i on stack (all 4 bytes)
0833: A5 16        LDA 0x16           ; Load NEXT0 (0)
0835: 91 61        STA [0x61],Y       ; Store to stack byte 0
0837: A5 17        LDA 0x17           ; Load NEXT1 (0)
0839: 91 63        STA [0x63],Y       ; Store to stack byte 1
083B: A5 18        LDA 0x18           ; Load NEXT2 (0)
083D: 91 65        STA [0x65],Y       ; Store to stack byte 2
083F: A5 19        LDA 0x19           ; Load NEXT3 (0)
0841: 91 67        STA [0x67],Y       ; Store to stack byte 3

0843: 48           PHA                ; Push result (init expression)
0844: 68           PLA                ; Pop result (discard)
```

### Load i for Comparison (First Iteration)
```asm
0845: BA           TSX                ; Get stack pointer
0846: 8A           TXA                ; Transfer to A
0847: A8           TAY                ; Transfer to Y

; Load i from stack to NEXT
0848: B1 61        LDA [0x61],Y       ; Load i byte 0
084A: 85 16        STA 0x16           ; Store to NEXT0
084C: B1 63        LDA [0x63],Y       ; Load i byte 1
084E: 85 17        STA 0x17           ; Store to NEXT1
0850: B1 65        LDA [0x65],Y       ; Load i byte 2
0852: 85 18        STA 0x18           ; Store to NEXT2
0854: B1 67        LDA [0x67],Y       ; Load i byte 3
0856: 85 19        STA 0x19           ; Store to NEXT3
```

### Calculate BP Offset for i
```asm
0858: A5 60        LDA 0x60           ; Load BP
085A: 18           CLC                ; Clear carry
085B: 69 FF        ADC #0xFF          ; Add -1 (BP-1, where i is)
085D: A8           TAY                ; Transfer to Y

; Store i value at BP-1
085E: A5 16        LDA 0x16           ; Load NEXT0
0860: 91 61        STA [0x61],Y       ; Store to [BP-1] byte 0
0862: A5 17        LDA 0x17           ; Load NEXT1
0864: 91 63        STA [0x63],Y       ; Store to [BP-1] byte 1
0866: A5 18        LDA 0x18           ; Load NEXT2
0868: 91 65        STA [0x65],Y       ; Store to [BP-1] byte 2
086A: A5 19        LDA 0x19           ; Load NEXT3
086C: 91 67        STA [0x67],Y       ; Store to [BP-1] byte 3
```

### Push i for Assignment Expression Result
```asm
086E: BA           TSX                ; Get stack pointer
086F: 8A           TXA                ; Transfer to A
0870: A8           TAY                ; Transfer to Y

; Push current value of i
0871: A5 16        LDA 0x16           ; Load NEXT0
0873: 91 61        STA [0x61],Y       ; Push byte 0
0875: A5 17        LDA 0x17           ; Load NEXT1
0877: 91 63        STA [0x63],Y       ; Push byte 1
0879: A5 18        LDA 0x18           ; Load NEXT2
087B: 91 65        STA [0x65],Y       ; Push byte 2
087D: A5 19        LDA 0x19           ; Load NEXT3
087F: 91 67        STA [0x67],Y       ; Push byte 3

0881: 48           PHA                ; Update stack pointer
0882: 68           PLA                ; Pop assignment result
```

### ⚠️ LOOP START - THE BUG IS HERE!
```asm
0883: A8           TAY                ; ⚠️ BUG! Y = garbage from PLA, not SP!
                                      ; Should be: TSX, TXA, TAY

; Load i from stack (using WRONG offset in Y!)
0884: B1 61        LDA [0x61],Y       ; Load from wrong location!
0886: 85 16        STA 0x16           ; Store to NEXT0
0888: B1 63        LDA [0x63],Y       ; Load from wrong location!
088A: 85 17        STA 0x17           ; Store to NEXT1
088C: B1 65        LDA [0x65],Y       ; Load from wrong location!
088E: 85 18        STA 0x18           ; Store to NEXT2
0890: B1 67        LDA [0x67],Y       ; Load from wrong location!
0892: 85 19        STA 0x19           ; Store to NEXT3
```

### Calculate BP Offset and Load i (Correctly This Time)
```asm
0894: A5 60        LDA 0x60           ; Load BP
0896: 18           CLC                ; Clear carry
0897: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
0899: A8           TAY                ; Transfer to Y

; Load i from BP-1
089A: A5 16        LDA 0x16           ; NEXT0
089C: 91 61        STA [0x61],Y       ; Store to [BP-1]
089E: A5 17        LDA 0x17           ; NEXT1
08A0: 91 63        STA [0x63],Y       ; Store to [BP-1]
08A2: A5 18        LDA 0x18           ; NEXT2
08A4: 91 65        STA [0x65],Y       ; Store to [BP-1]
08A6: A5 19        LDA 0x19           ; NEXT3
08A8: 91 67        STA [0x67],Y       ; Store to [BP-1]
```

### Push Constant 10 for Comparison
```asm
08AA: 48           PHA                ; Reserve slot
08AB: A9 0A        LDA #0x0A          ; Load 10
08AD: 85 16        STA 0x16           ; NEXT0 = 10
08AF: 64 17        STZ 0x17           ; NEXT1 = 0
08B1: 64 18        STZ 0x18           ; NEXT2 = 0
08B3: 64 19        STZ 0x19           ; NEXT3 = 0

08B5: BA           TSX                ; Get stack pointer
08B6: 8A           TXA                ; Transfer to A
08B7: A8           TAY                ; Transfer to Y

; Push 10 onto stack
08B8: A5 16        LDA 0x16           ; Load 10
08BA: 91 61        STA [0x61],Y       ; Push byte 0
08BC: A5 17        LDA 0x17           ; Load 0
08BE: 91 63        STA [0x63],Y       ; Push byte 1
08C0: A5 18        LDA 0x18           ; Load 0
08C2: 91 65        STA [0x65],Y       ; Push byte 2
08C4: A5 19        LDA 0x19           ; Load 0
08C6: 91 67        STA [0x67],Y       ; Push byte 3

08C8: 48           PHA                ; Update stack pointer
```

### Pop Values for LT Comparison
```asm
08C9: 68           PLA                ; Pop
08CA: BA           TSX                ; Get stack pointer
08CB: 8A           TXA                ; Transfer to A
08CC: A8           TAY                ; Transfer to Y

; Pop 10 into TOP
08CD: B1 61        LDA [0x61],Y       ; Load byte 0 (10)
08CF: 85 12        STA 0x12           ; Store to TOP0
08D1: B1 63        LDA [0x63],Y       ; Load byte 1 (0)
08D3: 85 13        STA 0x13           ; Store to TOP1
08D5: B1 65        LDA [0x65],Y       ; Load byte 2 (0)
08D7: 85 14        STA 0x14           ; Store to TOP2
08D9: B1 67        LDA [0x67],Y       ; Load byte 3 (0)
08DB: 85 15        STA 0x15           ; Store to TOP3

08DD: 68           PLA                ; Pop again
08DE: BA           TSX                ; Get stack pointer
08DF: 8A           TXA                ; Transfer to A
08E0: A8           TAY                ; Transfer to Y

; Pop i into NEXT
08E1: B1 61        LDA [0x61],Y       ; Load i byte 0
08E3: 85 16        STA 0x16           ; Store to NEXT0
08E5: B1 63        LDA [0x63],Y       ; Load i byte 1
08E7: 85 17        STA 0x17           ; Store to NEXT1
08E9: B1 65        LDA [0x65],Y       ; Load i byte 2
08EB: 85 18        STA 0x18           ; Store to NEXT2
08ED: B1 67        LDA [0x67],Y       ; Load i byte 3
08EF: 85 19        STA 0x19           ; Store to NEXT3
```

### Call Long.LT Comparison
```asm
08F1: A2 20        LDX #0x20          ; SysCall.LongLT
08F3: 20 03 08     JSR 0x0803         ; Call BIOS dispatch
                                      ; Returns: NEXT = (i < 10) ? 1 : 0
```

### Marshal Result Back to Stack
```asm
08F6: BA           TSX                ; Get stack pointer
08F7: 8A           TXA                ; Transfer to A
08F8: A8           TAY                ; Transfer to Y

08F9: A9 00        LDA #0x00          ; Load 0
08FB: 69 00        ADC #0x00          ; Add carry (from comparison)
08FD: 91 61        STA [0x61],Y       ; Store result byte 0
08FF: A9 00        LDA #0x00          ; Load 0
0901: 91 63        STA [0x63],Y       ; Store result byte 1
0903: 91 65        STA [0x65],Y       ; Store result byte 2
0905: 91 67        STA [0x67],Y       ; Store result byte 3

0907: 48           PHA                ; Push result
0908: 68           PLA                ; Pop result
```

### Check Comparison Result
```asm
0909: BA           TSX                ; Get stack pointer
090A: 8A           TXA                ; Transfer to A
090B: A8           TAY                ; Transfer to Y

; Load comparison result
090C: B1 61        LDA [0x61],Y       ; Load byte 0
090E: 85 16        STA 0x16           ; Store to NEXT0
0910: B1 63        LDA [0x63],Y       ; Load byte 1
0912: 85 17        STA 0x17           ; Store to NEXT1
0914: B1 65        LDA [0x65],Y       ; Load byte 2
0916: 85 18        STA 0x18           ; Store to NEXT2
0918: B1 67        LDA [0x67],Y       ; Load byte 3
091A: 85 19        STA 0x19           ; Store to NEXT3

; Test if result is zero (false)
091C: A5 16        LDA 0x16           ; Load NEXT0
091E: 05 17        ORA 0x17           ; OR with NEXT1
0920: 05 18        ORA 0x18           ; OR with NEXT2
0922: 05 19        ORA 0x19           ; OR with NEXT3
0924: D0 03        BNE 0x0929         ; If not zero (true), continue loop
0926: 4C D7 09     JMP 0x09D7         ; Exit loop if false
```

### Loop Body - putchar('.')
```asm
0929: 48           PHA                ; Push return slot

; Load '.' character (0x2E)
092A: A9 2E        LDA #0x2E          ; Load '.'
092C: 85 16        STA 0x16           ; NEXT0 = 0x2E
092E: 64 17        STZ 0x17           ; NEXT1 = 0
0930: 64 18        STZ 0x18           ; NEXT2 = 0
0932: 64 19        STZ 0x19           ; NEXT3 = 0

0934: BA           TSX                ; Get stack pointer
0935: 8A           TXA                ; Transfer to A
0936: A8           TAY                ; Transfer to Y

; Push '.' onto stack
0937: A5 16        LDA 0x16           ; Load 0x2E
0939: 91 61        STA [0x61],Y       ; Push byte 0
093B: A5 17        LDA 0x17           ; Load 0
093D: 91 63        STA [0x63],Y       ; Push byte 1
093F: A5 18        LDA 0x18           ; Load 0
0941: 91 65        STA [0x65],Y       ; Push byte 2
0943: A5 19        LDA 0x19           ; Load 0
0945: 91 67        STA [0x67],Y       ; Push byte 3

0947: 48           PHA                ; Update stack pointer
0948: 68           PLA                ; Pop

; Pop argument for putchar
0949: BA           TSX                ; Get stack pointer
094A: 8A           TXA                ; Transfer to A
094B: A8           TAY                ; Transfer to Y

094C: B1 61        LDA [0x61],Y       ; Load byte 0 (0x2E)
094E: 85 16        STA 0x16           ; Store to NEXT0
0950: B1 63        LDA [0x63],Y       ; Load byte 1
0952: 85 17        STA 0x17           ; Store to NEXT1
0954: B1 65        LDA [0x65],Y       ; Load byte 2
0956: 85 18        STA 0x18           ; Store to NEXT2
0958: B1 67        LDA [0x67],Y       ; Load byte 3
095A: 85 19        STA 0x19           ; Store to NEXT3

; Call putchar
095C: A5 16        LDA 0x16           ; Load character '.'
095E: A2 12        LDX #0x12          ; SysCall.putchar
0960: 20 03 08     JSR 0x0803         ; Call BIOS dispatch
```

### Clean Up After putchar Call
```asm
0963: BA           TSX                ; Get stack pointer
0964: 8A           TXA                ; Transfer to A
0965: A8           TAY                ; Transfer to Y

; Store result (not used)
0966: A5 16        LDA 0x16           
0968: 91 61        STA [0x61],Y       
096A: A5 17        LDA 0x17           
096C: 91 63        STA [0x63],Y       
096E: A5 18        LDA 0x18           
0970: 91 65        STA [0x65],Y       
0972: A5 19        LDA 0x19           
0974: 91 67        STA [0x67],Y       

0976: 48           PHA                ; Push
0977: 68           PLA                ; Pop (discard putchar result)
```

### Increment i++
```asm
; Load i from BP-1
0978: A5 60        LDA 0x60           ; Load BP
097A: 18           CLC                ; Clear carry
097B: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
097D: A8           TAY                ; Transfer to Y

097E: B1 61        LDA [0x61],Y       ; Load i byte 0
0980: 85 16        STA 0x16           ; Store to NEXT0
0982: B1 63        LDA [0x63],Y       ; Load i byte 1
0984: 85 17        STA 0x17           ; Store to NEXT1
0986: B1 65        LDA [0x65],Y       ; Load i byte 2
0988: 85 18        STA 0x18           ; Store to NEXT2
098A: B1 67        LDA [0x67],Y       ; Load i byte 3
098C: 85 19        STA 0x19           ; Store to NEXT3

; Push current value of i
098E: BA           TSX                ; Get stack pointer
098F: 8A           TXA                ; Transfer to A
0990: A8           TAY                ; Transfer to Y

0991: A5 16        LDA 0x16           ; Push i byte 0
0993: 91 61        STA [0x61],Y       
0995: A5 17        LDA 0x17           ; Push i byte 1
0997: 91 63        STA [0x63],Y       
0999: A5 18        LDA 0x18           ; Push i byte 2
099B: 91 65        STA [0x65],Y       
099D: A5 19        LDA 0x19           ; Push i byte 3
099F: 91 67        STA [0x67],Y       

09A1: 48           PHA                ; Update stack pointer

; Perform increment
09A2: 18           CLC                ; Clear carry
09A3: A5 16        LDA 0x16           ; Load NEXT0 (i)
09A5: 69 01        ADC #0x01          ; Add 1
09A7: 85 16        STA 0x16           ; Store back to NEXT0
09A9: A5 17        LDA 0x17           ; Load NEXT1
09AB: 69 00        ADC #0x00          ; Add carry
09AD: 85 17        STA 0x17           ; Store back to NEXT1
09AF: A5 18        LDA 0x18           ; Load NEXT2
09B1: 69 00        ADC #0x00          ; Add carry
09B3: 85 18        STA 0x18           ; Store back to NEXT2
09B5: A5 19        LDA 0x19           ; Load NEXT3
09B7: 69 00        ADC #0x00          ; Add carry
09B9: 85 19        STA 0x19           ; Store back to NEXT3

; Store incremented value back to BP-1
09BB: A5 60        LDA 0x60           ; Load BP
09BD: 18           CLC                ; Clear carry
09BE: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
09C0: A8           TAY                ; Transfer to Y

09C1: A5 16        LDA 0x16           ; Store incremented i byte 0
09C3: 91 61        STA [0x61],Y       
09C5: A5 17        LDA 0x17           ; Store incremented i byte 1
09C7: 91 63        STA [0x63],Y       
09C9: A5 18        LDA 0x18           ; Store incremented i byte 2
09CB: 91 65        STA [0x65],Y       
09CD: A5 19        LDA 0x19           ; Store incremented i byte 3
09CF: 91 67        STA [0x67],Y       

09D1: 68           PLA                ; Pop the pre-increment value
09D2: 4C 83 08     JMP 0x0883         ; Jump back to loop start
                                      ; ⚠️ WHERE BUG MANIFESTS!
```

### Loop Exit Point
```asm
09D5: A6 60        LDX 0x60           ; Load BP
09D7: 9A           TXS                ; Restore stack pointer
09D8: 68           PLA                ; Pop old BP
09D9: 85 60        STA 0x60           ; Restore old BP
09DB: 60           RTS                ; Return
```

## Summary

The critical bug is at **0x0883** where the loop continuation begins. The code does:
```asm
0883: A8           TAY                ; BUG! Y gets garbage from previous PLA
```

But it should do:
```asm
0883: BA           TSX                ; Get stack pointer
0884: 8A           TXA                ; Transfer to A
0885: A8           TAY                ; Transfer to Y for indirect addressing
```

Without the correct stack pointer in Y, all subsequent stack accesses read from the wrong memory location, corrupting the loop counter and causing only 9 iterations instead of 10.

The fix is to ensure the code generator emits `TSX, TXA` before recording the loop start address, or at the beginning of the condition check code.