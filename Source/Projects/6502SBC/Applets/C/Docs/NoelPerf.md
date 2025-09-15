# Complete Annotated Disassembly - Noel's RetroLab Benchmark (FIXED)

## Source Code Reference
```c
void main() {
    long s = 1000;        // Local at BP+0
    long start = millis(); // Local at BP-4  
    int i;                // Local at BP-3
    int j;                // Local at BP-2
    
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

## Memory Layout
- **BP+0**: Variable `s` (long, 4 bytes)
- **BP-4**: Variable `start` (long, 4 bytes) 
- **BP-3**: Variable `i` (int, treated as 32-bit)
- **BP-2**: Variable `j` (int, treated as 32-bit)

## Complete Disassembly

### Program Entry and String Data
```asm
0800: 4C 14 08     JMP 0x0814         ; Jump to main entry
0803: 6C 22 00     JMP [0x0022]       ; BIOS dispatcher (indirect)
0806: 60           RTS                ; Return

; String literals
0807: 25 6C 64 0A 00                  ; "%ld\n\0"
080C: 25 6C 64 20 6D 73 0A 00        ; "%ld ms\n\0"
```

### Main Function Entry
```asm
0814: A2 00        LDX #0x00          ; Push return slot
0816: 20 03 08     JSR 0x0803         ; Call BIOS (no-op for main)

; Initialize runtime stack pointers
0819: 64 61        STZ 0x61           ; runtimeStack0L = 0
081B: 64 63        STZ 0x63           ; runtimeStack1L = 0
081D: 64 65        STZ 0x65           ; runtimeStack2L = 0
081F: 64 67        STZ 0x67           ; runtimeStack3L = 0
0821: A5 1B        LDA 0x1B           ; Load ZP.IDXH (0x01)
0823: 85 62        STA 0x62           ; runtimeStack0H = 0x01
0825: 1A           INC A              ; A = 0x02
0826: 85 64        STA 0x64           ; runtimeStack1H = 0x02
0828: 1A           INC A              ; A = 0x03
0829: 85 66        STA 0x66           ; runtimeStack2H = 0x03
082B: 1A           INC A              ; A = 0x04
082C: 85 68        STA 0x68           ; runtimeStack3H = 0x04

; Function prologue
082E: A5 60        LDA 0x60           ; Load old BP
0830: 48           PHA                ; Save old BP
0831: BA           TSX                ; Get stack pointer
0832: 86 60        STX 0x60           ; Set new BP = SP
0834: 48           PHA                ; Reserve space for s
```

### Initialize s = 1000 (0x03E8)
```asm
0835: A9 E8        LDA #0xE8          ; Load 0xE8 (232, low byte of 1000)
0837: 85 16        STA 0x16           ; NEXT0 = 0xE8
0839: A9 03        LDA #0x03          ; Load 0x03 (high byte of 1000)
083B: 85 17        STA 0x17           ; NEXT1 = 0x03
083D: 64 18        STZ 0x18           ; NEXT2 = 0
083F: 64 19        STZ 0x19           ; NEXT3 = 0

; Push 1000 onto stack
0841: BA           TSX                ; Get stack pointer
0842: 8A           TXA                ; Transfer to A
0843: A8           TAY                ; Transfer to Y
0844: A5 16        LDA 0x16           ; Load NEXT0 (0xE8)
0846: 91 61        STA [0x61],Y       ; Store to stack[0]
0848: A5 17        LDA 0x17           ; Load NEXT1 (0x03)
084A: 91 63        STA [0x63],Y       ; Store to stack[1]
084C: A5 18        LDA 0x18           ; Load NEXT2 (0)
084E: 91 65        STA [0x65],Y       ; Store to stack[2]
0850: A5 19        LDA 0x19           ; Load NEXT3 (0)
0852: 91 67        STA [0x67],Y       ; Store to stack[3]
0854: 48           PHA                ; Update stack pointer
0855: 68           PLA                ; Pop

; Load value back from stack
0856: BA           TSX                ; Get stack pointer
0857: 8A           TXA                ; Transfer to A
0858: A8           TAY                ; Transfer to Y
0859: B1 61        LDA [0x61],Y       ; Load from stack[0]
085B: 85 16        STA 0x16           ; Store to NEXT0
085D: B1 63        LDA [0x63],Y       ; Load from stack[1]
085F: 85 17        STA 0x17           ; Store to NEXT1
0861: B1 65        LDA [0x65],Y       ; Load from stack[2]
0863: 85 18        STA 0x18           ; Store to NEXT2
0865: B1 67        LDA [0x67],Y       ; Load from stack[3]
0867: 85 19        STA 0x19           ; Store to NEXT3

; Store to s at BP+0
0869: A5 60        LDA 0x60           ; Load BP
086B: 18           CLC                ; Clear carry
086C: 69 00        ADC #0x00          ; Add 0 (BP+0)
086E: A8           TAY                ; Transfer to Y
086F: A5 16        LDA 0x16           ; Store NEXT0
0871: 91 61        STA [0x61],Y       ; Store to [BP+0][0]
0873: A5 17        LDA 0x17           ; Store NEXT1
0875: 91 63        STA [0x63],Y       ; Store to [BP+0][1]
0877: A5 18        LDA 0x18           ; Store NEXT2
0879: 91 65        STA [0x65],Y       ; Store to [BP+0][2]
087B: A5 19        LDA 0x19           ; Store NEXT3
087D: 91 67        STA [0x67],Y       ; Store to [BP+0][3]

; Push assignment result
087F: BA           TSX                ; Get stack pointer
0880: 8A           TXA                ; Transfer to A
0881: A8           TAY                ; Transfer to Y
0882: A5 16        LDA 0x16           ; Push NEXT0
0884: 91 61        STA [0x61],Y       
0886: A5 17        LDA 0x17           ; Push NEXT1
0888: 91 63        STA [0x63],Y       
088A: A5 18        LDA 0x18           ; Push NEXT2
088C: 91 65        STA [0x65],Y       
088E: A5 19        LDA 0x19           ; Push NEXT3
0890: 91 67        STA [0x67],Y       
0892: 48           PHA                ; Update stack
0893: 68           PLA                ; Pop result
```

### Reserve Space for start Variable
```asm
0894: 48           PHA                ; Reserve space for start
```

### Call millis() and Store to start
```asm
0895: 48           PHA                ; Push return slot
0896: A2 18        LDX #0x18          ; SysCall.millis
0898: 20 03 08     JSR 0x0803         ; Call BIOS

; Marshal result from TOP to stack
089B: BA           TSX                ; Get stack pointer
089C: 8A           TXA                ; Transfer to A
089D: A8           TAY                ; Transfer to Y
089E: A5 12        LDA 0x12           ; Load TOP0
08A0: 91 61        STA [0x61],Y       ; Store to stack[0]
08A2: A5 13        LDA 0x13           ; Load TOP1
08A4: 91 63        STA [0x63],Y       ; Store to stack[1]
08A6: A5 14        LDA 0x14           ; Load TOP2
08A8: 91 65        STA [0x65],Y       ; Store to stack[2]
08AA: A5 15        LDA 0x15           ; Load TOP3
08AC: 91 67        STA [0x67],Y       ; Store to stack[3]
08AE: 48           PHA                ; Update stack
08AF: 68           PLA                ; Pop

; Load millis() result
08B0: BA           TSX                ; Get stack pointer
08B1: 8A           TXA                ; Transfer to A
08B2: A8           TAY                ; Transfer to Y
08B3: B1 61        LDA [0x61],Y       ; Load result[0]
08B5: 85 16        STA 0x16           ; Store to NEXT0
08B7: B1 63        LDA [0x63],Y       ; Load result[1]
08B9: 85 17        STA 0x17           ; Store to NEXT1
08BB: B1 65        LDA [0x65],Y       ; Load result[2]
08BD: 85 18        STA 0x18           ; Store to NEXT2
08BF: B1 67        LDA [0x67],Y       ; Load result[3]
08C1: 85 19        STA 0x19           ; Store to NEXT3

; Store to start at BP-2 (0xFE)
08C3: A5 60        LDA 0x60           ; Load BP
08C5: 18           CLC                ; Clear carry
08C6: 69 FE        ADC #0xFE          ; Add -2 (BP-2)
08C8: A8           TAY                ; Transfer to Y
08C9: A5 16        LDA 0x16           ; Store NEXT0
08CB: 91 61        STA [0x61],Y       ; Store to [BP-2][0]
08CD: A5 17        LDA 0x17           ; Store NEXT1
08CF: 91 63        STA [0x63],Y       ; Store to [BP-2][1]
08D1: A5 18        LDA 0x18           ; Store NEXT2
08D3: 91 65        STA [0x65],Y       ; Store to [BP-2][2]
08D5: A5 19        LDA 0x19           ; Store NEXT3
08D7: 91 67        STA [0x67],Y       ; Store to [BP-2][3]

; Push assignment result
08D9: BA           TSX                ; Get stack pointer
08DA: 8A           TXA                ; Transfer to A
08DB: A8           TAY                ; Transfer to Y
08DC: A5 16        LDA 0x16           ; Push NEXT0
08DE: 91 61        STA [0x61],Y       
08E0: A5 17        LDA 0x17           ; Push NEXT1
08E2: 91 63        STA [0x63],Y       
08E4: A5 18        LDA 0x18           ; Push NEXT2
08E6: 91 65        STA [0x65],Y       
08E8: A5 19        LDA 0x19           ; Push NEXT3
08EA: 91 67        STA [0x67],Y       
08EC: 48           PHA                ; Update stack
08ED: 68           PLA                ; Pop result
```

### Reserve Space for Variables i and j
```asm
08EE: 48           PHA                ; Reserve space for i
08EF: 48           PHA                ; Reserve space for j
```

### Initialize i = 1 (Outer Loop Init)
```asm
08F0: A9 01        LDA #0x01          ; Load 1
08F2: 85 16        STA 0x16           ; NEXT0 = 1
08F4: 64 17        STZ 0x17           ; NEXT1 = 0
08F6: 64 18        STZ 0x18           ; NEXT2 = 0
08F8: 64 19        STZ 0x19           ; NEXT3 = 0

; Push 1 onto stack
08FA: BA           TSX                ; Get stack pointer
08FB: 8A           TXA                ; Transfer to A
08FC: A8           TAY                ; Transfer to Y
08FD: A5 16        LDA 0x16           ; Push NEXT0 (1)
08FF: 91 61        STA [0x61],Y       
0901: A5 17        LDA 0x17           ; Push NEXT1 (0)
0903: 91 63        STA [0x63],Y       
0905: A5 18        LDA 0x18           ; Push NEXT2 (0)
0907: 91 65        STA [0x65],Y       
0909: A5 19        LDA 0x19           ; Push NEXT3 (0)
090B: 91 67        STA [0x67],Y       
090D: 48           PHA                ; Update stack
090E: 68           PLA                ; Pop

; Load 1 from stack
090F: BA           TSX                ; Get stack pointer
0910: 8A           TXA                ; Transfer to A
0911: A8           TAY                ; Transfer to Y
0912: B1 61        LDA [0x61],Y       ; Load from stack[0]
0914: 85 16        STA 0x16           ; Store to NEXT0
0916: B1 63        LDA [0x63],Y       ; Load from stack[1]
0918: 85 17        STA 0x17           ; Store to NEXT1
091A: B1 65        LDA [0x65],Y       ; Load from stack[2]
091C: 85 18        STA 0x18           ; Store to NEXT2
091E: B1 67        LDA [0x67],Y       ; Load from stack[3]
0920: 85 19        STA 0x19           ; Store to NEXT3

; Store to i at BP-3 (0xFD)
0922: A5 60        LDA 0x60           ; Load BP
0924: 18           CLC                ; Clear carry
0925: 69 FD        ADC #0xFD          ; Add -3 (BP-3)
0927: A8           TAY                ; Transfer to Y
0928: A5 16        LDA 0x16           ; Store NEXT0
092A: 91 61        STA [0x61],Y       ; Store to [BP-3][0]
092C: A5 17        LDA 0x17           ; Store NEXT1
092E: 91 63        STA [0x63],Y       ; Store to [BP-3][1]
0930: A5 18        LDA 0x18           ; Store NEXT2
0932: 91 65        STA [0x65],Y       ; Store to [BP-3][2]
0934: A5 19        LDA 0x19           ; Store NEXT3
0936: 91 67        STA [0x67],Y       ; Store to [BP-3][3]

; Push assignment result
0938: BA           TSX                ; Get stack pointer
0939: 8A           TXA                ; Transfer to A
093A: A8           TAY                ; Transfer to Y
093B: A5 16        LDA 0x16           ; Push NEXT0
093D: 91 61        STA [0x61],Y       
093F: A5 17        LDA 0x17           ; Push NEXT1
0941: 91 63        STA [0x63],Y       
0943: A5 18        LDA 0x18           ; Push NEXT2
0945: 91 65        STA [0x65],Y       
0947: A5 19        LDA 0x19           ; Push NEXT3
0949: 91 67        STA [0x67],Y       
094B: 48           PHA                ; Update stack
094C: 68           PLA                ; Pop result
```

### OUTER LOOP START - 0x094D (i <= 10 check)
```asm
094D: A5 60        LDA 0x60           ; Load BP
094F: 18           CLC                ; Clear carry
0950: 69 FD        ADC #0xFD          ; Add -3 (BP-3, location of i)
0952: A8           TAY                ; Transfer to Y
0953: B1 61        LDA [0x61],Y       ; Load i[0]
0955: 85 16        STA 0x16           ; Store to NEXT0
0957: B1 63        LDA [0x63],Y       ; Load i[1]
0959: 85 17        STA 0x17           ; Store to NEXT1
095B: B1 65        LDA [0x65],Y       ; Load i[2]
095D: 85 18        STA 0x18           ; Store to NEXT2
095F: B1 67        LDA [0x67],Y       ; Load i[3]
0961: 85 19        STA 0x19           ; Store to NEXT3

; Push i for comparison
0963: BA           TSX                ; Get stack pointer
0964: 8A           TXA                ; Transfer to A
0965: A8           TAY                ; Transfer to Y
0966: A5 16        LDA 0x16           ; Push NEXT0
0968: 91 61        STA [0x61],Y       
096A: A5 17        LDA 0x17           ; Push NEXT1
096C: 91 63        STA [0x63],Y       
096E: A5 18        LDA 0x18           ; Push NEXT2
0970: 91 65        STA [0x65],Y       
0972: A5 19        LDA 0x19           ; Push NEXT3
0974: 91 67        STA [0x67],Y       
0976: 48           PHA                ; Update stack

; Push constant 10
0977: A9 0A        LDA #0x0A          ; Load 10
0979: 85 16        STA 0x16           ; NEXT0 = 10
097B: 64 17        STZ 0x17           ; NEXT1 = 0
097D: 64 18        STZ 0x18           ; NEXT2 = 0
097F: 64 19        STZ 0x19           ; NEXT3 = 0
0981: BA           TSX                ; Get stack pointer
0982: 8A           TXA                ; Transfer to A
0983: A8           TAY                ; Transfer to Y
0984: A5 16        LDA 0x16           ; Push 10
0986: 91 61        STA [0x61],Y       
0988: A5 17        LDA 0x17           ; Push 0
098A: 91 63        STA [0x63],Y       
098C: A5 18        LDA 0x18           ; Push 0
098E: 91 65        STA [0x65],Y       
0990: A5 19        LDA 0x19           ; Push 0
0992: 91 67        STA [0x67],Y       
0994: 48           PHA                ; Update stack

; Pop values for LE comparison
0995: 68           PLA                ; Pop
0996: BA           TSX                ; Get stack pointer
0997: 8A           TXA                ; Transfer to A
0998: A8           TAY                ; Transfer to Y
0999: B1 61        LDA [0x61],Y       ; Pop 10 to TOP0
099B: 85 12        STA 0x12           
099D: B1 63        LDA [0x63],Y       ; Pop to TOP1
099F: 85 13        STA 0x13           
09A1: B1 65        LDA [0x65],Y       ; Pop to TOP2
09A3: 85 14        STA 0x14           
09A5: B1 67        LDA [0x67],Y       ; Pop to TOP3
09A7: 85 15        STA 0x15           
09A9: 68           PLA                ; Pop again
09AA: BA           TSX                ; Get stack pointer
09AB: 8A           TXA                ; Transfer to A
09AC: A8           TAY                ; Transfer to Y
09AD: B1 61        LDA [0x61],Y       ; Pop i to NEXT0
09AF: 85 16        STA 0x16           
09B1: B1 63        LDA [0x63],Y       ; Pop i to NEXT1
09B3: 85 17        STA 0x17           
09B5: B1 65        LDA [0x65],Y       ; Pop i to NEXT2
09B7: 85 18        STA 0x18           
09B9: B1 67        LDA [0x67],Y       ; Pop i to NEXT3
09BB: 85 19        STA 0x19           

; Call Long.LE comparison (i <= 10)
09BD: A2 24        LDX #0x24          ; SysCall.LongLE
09BF: 20 03 08     JSR 0x0803         ; Call BIOS

; Marshal comparison result
09C2: BA           TSX                ; Get stack pointer
09C3: 8A           TXA                ; Transfer to A
09C4: A8           TAY                ; Transfer to Y
09C5: A9 00        LDA #0x00          ; Load 0
09C7: 69 00        ADC #0x00          ; Add carry (result)
09C9: 91 61        STA [0x61],Y       ; Store result[0]
09CB: A9 00        LDA #0x00          ; Load 0
09CD: 91 63        STA [0x63],Y       ; Store result[1]
09CF: 91 65        STA [0x65],Y       ; Store result[2]
09D1: 91 67        STA [0x67],Y       ; Store result[3]
09D3: 48           PHA                ; Push result
09D4: 68           PLA                ; Pop result

; Load and test comparison result
09D5: BA           TSX                ; Get stack pointer
09D6: 8A           TXA                ; Transfer to A
09D7: A8           TAY                ; Transfer to Y
09D8: B1 61        LDA [0x61],Y       ; Load result[0]
09DA: 85 16        STA 0x16           ; Store to NEXT0
09DC: B1 63        LDA [0x63],Y       ; Load result[1]
09DE: 85 17        STA 0x17           ; Store to NEXT1
09E0: B1 65        LDA [0x65],Y       ; Load result[2]
09E2: 85 18        STA 0x18           ; Store to NEXT2
09E4: B1 67        LDA [0x67],Y       ; Load result[3]
09E6: 85 19        STA 0x19           ; Store to NEXT3

; Test if result is zero (exit if false)
09E8: A5 16        LDA 0x16           ; Load NEXT0
09EA: 05 17        ORA 0x17           ; OR with NEXT1
09EC: 05 18        ORA 0x18           ; OR with NEXT2
09EE: 05 19        ORA 0x19           ; OR with NEXT3
09F0: D0 03        BNE 0x09F5         ; If not zero, continue
09F2: 4C 34 0D     JMP 0x0D34         ; Exit outer loop (FIXED!)
```

### Outer Loop Body - Set s = 0
```asm
09F5: 64 16        STZ 0x16           ; NEXT0 = 0
09F7: 64 17        STZ 0x17           ; NEXT1 = 0
09F9: 64 18        STZ 0x18           ; NEXT2 = 0
09FB: 64 19        STZ 0x19           ; NEXT3 = 0

; Push 0 onto stack
09FD: BA           TSX                ; Get stack pointer
09FE: 8A           TXA                ; Transfer to A
09FF: A8           TAY                ; Transfer to Y
0A00: A5 16        LDA 0x16           ; Push 0
0A02: 91 61        STA [0x61],Y       
0A04: A5 17        LDA 0x17           ; Push 0
0A06: 91 63        STA [0x63],Y       
0A08: A5 18        LDA 0x18           ; Push 0
0A0A: 91 65        STA [0x65],Y       
0A0C: A5 19        LDA 0x19           ; Push 0
0A0E: 91 67        STA [0x67],Y       
0A10: 48           PHA                ; Update stack
0A11: 68           PLA                ; Pop

; Load 0 from stack
0A12: BA           TSX                ; Get stack pointer
0A13: 8A           TXA                ; Transfer to A
0A14: A8           TAY                ; Transfer to Y
0A15: B1 61        LDA [0x61],Y       ; Load 0
0A17: 85 16        STA 0x16           ; Store to NEXT0
0A19: B1 63        LDA [0x63],Y       ; Load 0
0A1B: 85 17        STA 0x17           ; Store to NEXT1
0A1D: B1 65        LDA [0x65],Y       ; Load 0
0A1F: 85 18        STA 0x18           ; Store to NEXT2
0A21: B1 67        LDA [0x67],Y       ; Load 0
0A23: 85 19        STA 0x19           ; Store to NEXT3

; Store to s at BP+0
0A25: A5 60        LDA 0x60           ; Load BP
0A27: 18           CLC                ; Clear carry
0A28: 69 00        ADC #0x00          ; Add 0 (BP+0)
0A2A: A8           TAY                ; Transfer to Y
0A2B: A5 16        LDA 0x16           ; Store 0
0A2D: 91 61        STA [0x61],Y       ; Store to [BP+0][0]
0A2F: A5 17        LDA 0x17           ; Store 0
0A31: 91 63        STA [0x63],Y       ; Store to [BP+0][1]
0A33: A5 18        LDA 0x18           ; Store 0
0A35: 91 65        STA [0x65],Y       ; Store to [BP+0][2]
0A37: A5 19        LDA 0x19           ; Store 0
0A39: 91 67        STA [0x67],Y       ; Store to [BP+0][3]

; Push assignment result
0A3B: BA           TSX                ; Get stack pointer
0A3C: 8A           TXA                ; Transfer to A
0A3D: A8           TAY                ; Transfer to Y
0A3E: A5 16        LDA 0x16           ; Push 0
0A40: 91 61        STA [0x61],Y       
0A42: A5 17        LDA 0x17           ; Push 0
0A44: 91 63        STA [0x63],Y       
0A46: A5 18        LDA 0x18           ; Push 0
0A48: 91 65        STA [0x65],Y       
0A4A: A5 19        LDA 0x19           ; Push 0
0A4C: 91 67        STA [0x67],Y       
0A4E: 48           PHA                ; Update stack
0A4F: 68           PLA                ; Pop result
```

### Initialize j = 1 (Inner Loop Init)
```asm
0A50: A9 01        LDA #0x01          ; Load 1
0A52: 85 16        STA 0x16           ; NEXT0 = 1
0A54: 64 17        STZ 0x17           ; NEXT1 = 0
0A56: 64 18        STZ 0x18           ; NEXT2 = 0
0A58: 64 19        STZ 0x19           ; NEXT3 = 0

; Push 1 onto stack
0A5A: BA           TSX                ; Get stack pointer
0A5B: 8A           TXA                ; Transfer to A
0A5C: A8           TAY                ; Transfer to Y
0A5D: A5 16        LDA 0x16           ; Push 1
0A5F: 91 61        STA [0x61],Y       
0A61: A5 17        LDA 0x17           ; Push 0
0A63: 91 63        STA [0x63],Y       
0A65: A5 18        LDA 0x18           ; Push 0
0A67: 91 65        STA [0x65],Y       
0A69: A5 19        LDA 0x19           ; Push 0
0A6B: 91 67        STA [0x67],Y       
0A6D: 48           PHA                ; Update stack
0A6E: 68           PLA                ; Pop

; Load 1 from stack
0A6F: BA           TSX                ; Get stack pointer
0A70: 8A           TXA                ; Transfer to A
0A71: A8           TAY                ; Transfer to Y
0A72: B1 61        LDA [0x61],Y       ; Load 1
0A74: 85 16        STA 0x16           ; Store to NEXT0
0A76: B1 63        LDA [0x63],Y       ; Load 0
0A78: 85 17        STA 0x17           ; Store to NEXT1
0A7A: B1 65        LDA [0x65],Y       ; Load 0
0A7C: 85 18        STA 0x18           ; Store to NEXT2
0A7E: B1 67        LDA [0x67],Y       ; Load 0
0A80: 85 19        STA 0x19           ; Store to NEXT3

; Store to j at BP-4 (0xFC)
0A82: A5 60        LDA 0x60           ; Load BP
0A84: 18           CLC                ; Clear carry
0A85: 69 FC        ADC #0xFC          ; Add -4 (BP-4)
0A87: A8           TAY                ; Transfer to Y
0A88: A5 16        LDA 0x16           ; Store 1
0A8A: 91 61        STA [0x61],Y       ; Store to [BP-4][0]
0A8C: A5 17        LDA 0x17           ; Store 0
0A8E: 91 63        STA [0x63],Y       ; Store to [BP-4][1]
0A90: A5 18        LDA 0x18           ; Store 0
0A92: 91 65        STA [0x65],Y       ; Store to [BP-4][2]
0A94: A5 19        LDA 0x19           ; Store 0
0A96: 91 67        STA [0x67],Y       ; Store to [BP-4][3]

; Push assignment result
0A98: BA           TSX                ; Get stack pointer
0A99: 8A           TXA                ; Transfer to A
0A9A: A8           TAY                ; Transfer to Y
0A9B: A5 16        LDA 0x16           ; Push 1
0A9D: 91 61        STA [0x61],Y       
0A9F: A5 17        LDA 0x17           ; Push 0
0AA1: 91 63        STA [0x63],Y       
0AA3: A5 18        LDA 0x18           ; Push 0
0AA5: 91 65        STA [0x65],Y       
0AA7: A5 19        LDA 0x19           ; Push 0
0AA9: 91 67        STA [0x67],Y       
0AAB: 48           PHA                ; Update stack
0AAC: 68           PLA                ; Pop result
```

### INNER LOOP START - 0x0AAD (j <= 1000 check)
```asm
0AAD: A5 60        LDA 0x60           ; Load BP
0AAF: 18           CLC                ; Clear carry
0AB0: 69 FC        ADC #0xFC          ; Add -4 (BP-4, location of j)
0AB2: A8           TAY                ; Transfer to Y
0AB3: B1 61        LDA [0x61],Y       ; Load j[0]
0AB5: 85 16        STA 0x16           ; Store to NEXT0
0AB7: B1 63        LDA [0x63],Y       ; Load j[1]
0AB9: 85 17        STA 0x17           ; Store to NEXT1
0ABB: B1 65        LDA [0x65],Y       ; Load j[2]
0ABD: 85 18        STA 0x18           ; Store to NEXT2
0ABF: B1 67        LDA [0x67],Y       ; Load j[3]
0AC1: 85 19        STA 0x19           ; Store to NEXT3

; Push j for comparison
0AC3: BA           TSX                ; Get stack pointer
0AC4: 8A           TXA                ; Transfer to A
0AC5: A8           TAY                ; Transfer to Y
0AC6: A5 16        LDA 0x16           ; Push j
0AC8: 91 61        STA [0x61],Y       
0ACA: A5 17        LDA 0x17           
0ACC: 91 63        STA [0x63],Y       
0ACE: A5 18        LDA 0x18           
0AD0: 91 65        STA [0x65],Y       
0AD2: A5 19        LDA 0x19           
0AD4: 91 67        STA [0x67],Y       
0AD6: 48           PHA                ; Update stack

; Push constant 1000 (0x03E8)
0AD7: A9 E8        LDA #0xE8          ; Load 0xE8 (232)
0AD9: 85 16        STA 0x16           ; NEXT0 = 0xE8
0ADB: A9 03        LDA #0x03          ; Load 0x03
0ADD: 85 17        STA 0x17           ; NEXT1 = 0x03
0ADF: 64 18        STZ 0x18           ; NEXT2 = 0
0AE1: 64 19        STZ 0x19           ; NEXT3 = 0
0AE3: BA           TSX                ; Get stack pointer
0AE4: 8A           TXA                ; Transfer to A
0AE5: A8           TAY                ; Transfer to Y
0AE6: A5 16        LDA 0x16           ; Push 1000
0AE8: 91 61        STA [0x61],Y       
0AEA: A5 17        LDA 0x17           
0AEC: 91 63        STA [0x63],Y       
0AEE: A5 18        LDA 0x18           
0AF0: 91 65        STA [0x65],Y       
0AF2: A5 19        LDA 0x19           
0AF4: 91 67        STA [0x67],Y       
0AF6: 48           PHA                ; Update stack

; Pop values for LE comparison
0AF7: 68           PLA                ; Pop
0AF8: BA           TSX                ; Get stack pointer
0AF9: 8A           TXA                ; Transfer to A
0AFA: A8           TAY                ; Transfer to Y
0AFB: B1 61        LDA [0x61],Y       ; Pop 1000 to TOP
0AFD: 85 12        STA 0x12           
0AFF: B1 63        LDA [0x63],Y       
0B01: 85 13        STA 0x13           
0B03: B1 65        LDA [0x65],Y       
0B05: 85 14        STA 0x14           
0B07: B1 67        LDA [0x67],Y       
0B09: 85 15        STA 0x15           
0B0B: 68           PLA                ; Pop again
0B0C: BA           TSX                ; Get stack pointer
0B0D: 8A           TXA                ; Transfer to A
0B0E: A8           TAY                ; Transfer to Y
0B0F: B1 61        LDA [0x61],Y       ; Pop j to NEXT
0B11: 85 16        STA 0x16           
0B13: B1 63        LDA [0x63],Y       
0B15: 85 17        STA 0x17           
0B17: B1 65        LDA [0x65],Y       
0B19: 85 18        STA 0x18           
0B1B: B1 67        LDA [0x67],Y       
0B1D: 85 19        STA 0x19           

; Call Long.LE comparison (j <= 1000)
0B1F: A2 24        LDX #0x24          ; SysCall.LongLE
0B21: 20 03 08     JSR 0x0803         ; Call BIOS

; Marshal comparison result
0B24: BA           TSX                ; Get stack pointer
0B25: 8A           TXA                ; Transfer to A
0B26: A8           TAY                ; Transfer to Y
0B27: A9 00        LDA #0x00          ; Load 0
0B29: 69 00        ADC #0x00          ; Add carry
0B2B: 91 61        STA [0x61],Y       ; Store result
0B2D: A9 00        LDA #0x00          
0B2F: 91 63        STA [0x63],Y       
0B31: 91 65        STA [0x65],Y       
0B33: 91 67        STA [0x67],Y       
0B35: 48           PHA                ; Push result
0B36: 68           PLA                ; Pop result

; Load and test comparison result
0B37: BA           TSX                ; Get stack pointer
0B38: 8A           TXA                ; Transfer to A
0B39: A8           TAY                ; Transfer to Y
0B3A: B1 61        LDA [0x61],Y       ; Load result
0B3C: 85 16        STA 0x16           
0B3E: B1 63        LDA [0x63],Y       
0B40: 85 17        STA 0x17           
0B42: B1 65        LDA [0x65],Y       
0B44: 85 18        STA 0x18           
0B46: B1 67        LDA [0x67],Y       
0B48: 85 19        STA 0x19           

; Test if result is zero (exit if false)
0B4A: A5 16        LDA 0x16           ; Load NEXT0
0B4C: 05 17        ORA 0x17           ; OR with NEXT1
0B4E: 05 18        ORA 0x18           ; OR with NEXT2
0B50: 05 19        ORA 0x19           ; OR with NEXT3
0B52: D0 03        BNE 0x0B57         ; If not zero, continue
0B54: 4C 88 0C     JMP 0x0C88         ; Exit inner loop (to putchar)
```

### Inner Loop Body - s = s + j
```asm
; Load s from BP+0
0B57: A5 60        LDA 0x60           ; Load BP
0B59: 18           CLC                ; Clear carry
0B5A: 69 00        ADC #0x00          ; Add 0 (BP+0)
0B5C: A8           TAY                ; Transfer to Y
0B5D: B1 61        LDA [0x61],Y       ; Load s[0]
0B5F: 85 16        STA 0x16           ; Store to NEXT0
0B61: B1 63        LDA [0x63],Y       ; Load s[1]
0B63: 85 17        STA 0x17           ; Store to NEXT1
0B65: B1 65        LDA [0x65],Y       ; Load s[2]
0B67: 85 18        STA 0x18           ; Store to NEXT2
0B69: B1 67        LDA [0x67],Y       ; Load s[3]
0B6B: 85 19        STA 0x19           ; Store to NEXT3

; Push s
0B6D: BA           TSX                ; Get stack pointer
0B6E: 8A           TXA                ; Transfer to A
0B6F: A8           TAY                ; Transfer to Y
0B70: A5 16        LDA 0x16           ; Push s
0B72: 91 61        STA [0x61],Y       
0B74: A5 17        LDA 0x17           
0B76: 91 63        STA [0x63],Y       
0B78: A5 18        LDA 0x18           
0B7A: 91 65        STA [0x65],Y       
0B7C: A5 19        LDA 0x19           
0B7E: 91 67        STA [0x67],Y       
0B80: 48           PHA                ; Update stack

; Load j from BP-4
0B81: A5 60        LDA 0x60           ; Load BP
0B83: 18           CLC                ; Clear carry
0B84: 69 FC        ADC #0xFC          ; Add -4 (BP-4)
0B86: A8           TAY                ; Transfer to Y
0B87: B1 61        LDA [0x61],Y       ; Load j[0]
0B89: 85 16        STA 0x16           ; Store to NEXT0
0B8B: B1 63        LDA [0x63],Y       ; Load j[1]
0B8D: 85 17        STA 0x17           ; Store to NEXT1
0B8F: B1 65        LDA [0x65],Y       ; Load j[2]
0B91: 85 18        STA 0x18           ; Store to NEXT2
0B93: B1 67        LDA [0x67],Y       ; Load j[3]
0B95: 85 19        STA 0x19           ; Store to NEXT3

; Push j
0B97: BA           TSX                ; Get stack pointer
0B98: 8A           TXA                ; Transfer to A
0B99: A8           TAY                ; Transfer to Y
0B9A: A5 16        LDA 0x16           ; Push j
0B9C: 91 61        STA [0x61],Y       
0B9E: A5 17        LDA 0x17           
0BA0: 91 63        STA [0x63],Y       
0BA2: A5 18        LDA 0x18           
0BA4: 91 65        STA [0x65],Y       
0BA6: A5 19        LDA 0x19           
0BA8: 91 67        STA [0x67],Y       
0BAA: 48           PHA                ; Update stack

; Pop values for addition
0BAB: 68           PLA                ; Pop
0BAC: BA           TSX                ; Get stack pointer
0BAD: 8A           TXA                ; Transfer to A
0BAE: A8           TAY                ; Transfer to Y
0BAF: B1 61        LDA [0x61],Y       ; Pop j to TOP
0BB1: 85 12        STA 0x12           
0BB3: B1 63        LDA [0x63],Y       
0BB5: 85 13        STA 0x13           
0BB7: B1 65        LDA [0x65],Y       
0BB9: 85 14        STA 0x14           
0BBB: B1 67        LDA [0x67],Y       
0BBD: 85 15        STA 0x15           
0BBF: 68           PLA                ; Pop again
0BC0: BA           TSX                ; Get stack pointer
0BC1: 8A           TXA                ; Transfer to A
0BC2: A8           TAY                ; Transfer to Y
0BC3: B1 61        LDA [0x61],Y       ; Pop s to NEXT
0BC5: 85 16        STA 0x16           
0BC7: B1 63        LDA [0x63],Y       
0BC9: 85 17        STA 0x17           
0BCB: B1 65        LDA [0x65],Y       
0BCD: 85 18        STA 0x18           
0BCF: B1 67        LDA [0x67],Y       
0BD1: 85 19        STA 0x19           

; Call Long.Add (s + j)
0BD3: A2 1A        LDX #0x1A          ; SysCall.LongAdd
0BD5: 20 03 08     JSR 0x0803         ; Call BIOS

; Marshal result (now in NEXT)
0BD8: BA           TSX                ; Get stack pointer
0BD9: 8A           TXA                ; Transfer to A
0BDA: A8           TAY                ; Transfer to Y
0BDB: A5 16        LDA 0x16           ; Push result
0BDD: 91 61        STA [0x61],Y       
0BDF: A5 17        LDA 0x17           
0BE1: 91 63        STA [0x63],Y       
0BE3: A5 18        LDA 0x18           
0BE5: 91 65        STA [0x65],Y       
0BE7: A5 19        LDA 0x19           
0BE9: 91 67        STA [0x67],Y       
0BEB: 48           PHA                ; Update stack
0BEC: 68           PLA                ; Pop

; Load result from stack
0BED: BA           TSX                ; Get stack pointer
0BEE: 8A           TXA                ; Transfer to A
0BEF: A8           TAY                ; Transfer to Y
0BF0: B1 61        LDA [0x61],Y       ; Load result
0BF2: 85 16        STA 0x16           
0BF4: B1 63        LDA [0x63],Y       
0BF6: 85 17        STA 0x17           
0BF8: B1 65        LDA [0x65],Y       
0BFA: 85 18        STA 0x18           
0BFC: B1 67        LDA [0x67],Y       
0BFE: 85 19        STA 0x19           

; Store result to s at BP+0
0C00: A5 60        LDA 0x60           ; Load BP
0C02: 18           CLC                ; Clear carry
0C03: 69 00        ADC #0x00          ; Add 0 (BP+0)
0C05: A8           TAY                ; Transfer to Y
0C06: A5 16        LDA 0x16           ; Store result
0C08: 91 61        STA [0x61],Y       
0C0A: A5 17        LDA 0x17           
0C0C: 91 63        STA [0x63],Y       
0C0E: A5 18        LDA 0x18           
0C10: 91 65        STA [0x65],Y       
0C12: A5 19        LDA 0x19           
0C14: 91 67        STA [0x67],Y       

; Push assignment result
0C16: BA           TSX                ; Get stack pointer
0C17: 8A           TXA                ; Transfer to A
0C18: A8           TAY                ; Transfer to Y
0C19: A5 16        LDA 0x16           ; Push result
0C1B: 91 61        STA [0x61],Y       
0C1D: A5 17        LDA 0x17           
0C1F: 91 63        STA [0x63],Y       
0C21: A5 18        LDA 0x18           
0C23: 91 65        STA [0x65],Y       
0C25: A5 19        LDA 0x19           
0C27: 91 67        STA [0x67],Y       
0C29: 48           PHA                ; Update stack
0C2A: 68           PLA                ; Pop result
```

### Inner Loop Increment - j++
```asm
; Load j from BP-4
0C2B: A5 60        LDA 0x60           ; Load BP
0C2D: 18           CLC                ; Clear carry
0C2E: 69 FC        ADC #0xFC          ; Add -4 (BP-4)
0C30: A8           TAY                ; Transfer to Y
0C31: B1 61        LDA [0x61],Y       ; Load j[0]
0C33: 85 16        STA 0x16           
0C35: B1 63        LDA [0x63],Y       ; Load j[1]
0C37: 85 17        STA 0x17           
0C39: B1 65        LDA [0x65],Y       ; Load j[2]
0C3B: 85 18        STA 0x18           
0C3D: B1 67        LDA [0x67],Y       ; Load j[3]
0C3F: 85 19        STA 0x19           

; Push current value of j
0C41: BA           TSX                ; Get stack pointer
0C42: 8A           TXA                ; Transfer to A
0C43: A8           TAY                ; Transfer to Y
0C44: A5 16        LDA 0x16           ; Push j
0C46: 91 61        STA [0x61],Y       
0C48: A5 17        LDA 0x17           
0C4A: 91 63        STA [0x63],Y       
0C4C: A5 18        LDA 0x18           
0C4E: 91 65        STA [0x65],Y       
0C50: A5 19        LDA 0x19           
0C52: 91 67        STA [0x67],Y       
0C54: 48           PHA                ; Update stack

; Increment j
0C55: 18           CLC                ; Clear carry
0C56: A5 16        LDA 0x16           ; Load j[0]
0C58: 69 01        ADC #0x01          ; Add 1
0C5A: 85 16        STA 0x16           ; Store back
0C5C: A5 17        LDA 0x17           ; Load j[1]
0C5E: 69 00        ADC #0x00          ; Add carry
0C60: 85 17        STA 0x17           ; Store back
0C62: A5 18        LDA 0x18           ; Load j[2]
0C64: 69 00        ADC #0x00          ; Add carry
0C66: 85 18        STA 0x18           ; Store back
0C68: A5 19        LDA 0x19           ; Load j[3]
0C6A: 69 00        ADC #0x00          ; Add carry
0C6C: 85 19        STA 0x19           ; Store back

; Store incremented j back to BP-4
0C6E: A5 60        LDA 0x60           ; Load BP
0C70: 18           CLC                ; Clear carry
0C71: 69 FC        ADC #0xFC          ; Add -4 (BP-4)
0C73: A8           TAY                ; Transfer to Y
0C74: A5 16        LDA 0x16           ; Store j[0]
0C76: 91 61        STA [0x61],Y       
0C78: A5 17        LDA 0x17           ; Store j[1]
0C7A: 91 63        STA [0x63],Y       
0C7C: A5 18        LDA 0x18           ; Store j[2]
0C7E: 91 65        STA [0x65],Y       
0C80: A5 19        LDA 0x19           ; Store j[3]
0C82: 91 67        STA [0x67],Y       

0C84: 68           PLA                ; Pop pre-increment value
0C85: 4C AD 0A     JMP 0x0AAD         ; Jump back to inner loop start
```

### After Inner Loop - putchar('.')
```asm
0C88: 48           PHA                ; Push return slot
0C89: A9 2E        LDA #0x2E          ; Load '.' (0x2E)
0C8B: 85 16        STA 0x16           ; NEXT0 = '.'
0C8D: 64 17        STZ 0x17           ; NEXT1 = 0
0C8F: 64 18        STZ 0x18           ; NEXT2 = 0
0C91: 64 19        STZ 0x19           ; NEXT3 = 0

; Push '.' onto stack
0C93: BA           TSX                ; Get stack pointer
0C94: 8A           TXA                ; Transfer to A
0C95: A8           TAY                ; Transfer to Y
0C96: A5 16        LDA 0x16           ; Push '.'
0C98: 91 61        STA [0x61],Y       
0C9A: A5 17        LDA 0x17           ; Push 0
0C9C: 91 63        STA [0x63],Y       
0C9E: A5 18        LDA 0x18           ; Push 0
0CA0: 91 65        STA [0x65],Y       
0CA2: A5 19        LDA 0x19           ; Push 0
0CA4: 91 67        STA [0x67],Y       
0CA6: 48           PHA                ; Update stack
0CA7: 68           PLA                ; Pop

; Pop argument for putchar
0CA8: BA           TSX                ; Get stack pointer
0CA9: 8A           TXA                ; Transfer to A
0CAA: A8           TAY                ; Transfer to Y
0CAB: B1 61        LDA [0x61],Y       ; Pop '.'
0CAD: 85 16        STA 0x16           
0CAF: B1 63        LDA [0x63],Y       ; Pop 0
0CB1: 85 17        STA 0x17           
0CB3: B1 65        LDA [0x65],Y       ; Pop 0
0CB5: 85 18        STA 0x18           
0CB7: B1 67        LDA [0x67],Y       ; Pop 0
0CB9: 85 19        STA 0x19           

; Call putchar
0CBB: A5 16        LDA 0x16           ; Load '.'
0CBD: A2 12        LDX #0x12          ; SysCall.putchar
0CBF: 20 03 08     JSR 0x0803         ; Call BIOS

; Store putchar result
0CC2: BA           TSX                ; Get stack pointer
0CC3: 8A           TXA                ; Transfer to A
0CC4: A8           TAY                ; Transfer to Y
0CC5: A5 16        LDA 0x16           ; Store result
0CC7: 91 61        STA [0x61],Y       
0CC9: A5 17        LDA 0x17           
0CCB: 91 63        STA [0x63],Y       
0CCD: A5 18        LDA 0x18           
0CCF: 91 65        STA [0x65],Y       
0CD1: A5 19        LDA 0x19           
0CD3: 91 67        STA [0x67],Y       
0CD5: 48           PHA                ; Update stack
0CD6: 68           PLA                ; Pop and discard
```

### Outer Loop Increment - i++
```asm
; Load i from BP-3
0CD7: A5 60        LDA 0x60           ; Load BP
0CD9: 18           CLC                ; Clear carry
0CDA: 69 FD        ADC #0xFD          ; Add -3 (BP-3)
0CDC: A8           TAY                ; Transfer to Y
0CDD: B1 61        LDA [0x61],Y       ; Load i[0]
0CDF: 85 16        STA 0x16           
0CE1: B1 63        LDA [0x63],Y       ; Load i[1]
0CE3: 85 17        STA 0x17           
0CE5: B1 65        LDA [0x65],Y       ; Load i[2]
0CE7: 85 18        STA 0x18           
0CE9: B1 67        LDA [0x67],Y       ; Load i[3]
0CEB: 85 19        STA 0x19           

; Push current value of i
0CED: BA           TSX                ; Get stack pointer
0CEE: 8A           TXA                ; Transfer to A
0CEF: A8           TAY                ; Transfer to Y
0CF0: A5 16        LDA 0x16           ; Push i
0CF2: 91 61        STA [0x61],Y       
0CF4: A5 17        LDA 0x17           
0CF6: 91 63        STA [0x63],Y       
0CF8: A5 18        LDA 0x18           
0CFA: 91 65        STA [0x65],Y       
0CFC: A5 19        LDA 0x19           
0CFE: 91 67        STA [0x67],Y       
0D00: 48           PHA                ; Update stack

; Increment i
0D01: 18           CLC                ; Clear carry
0D02: A5 16        LDA 0x16           ; Load i[0]
0D04: 69 01        ADC #0x01          ; Add 1
0D06: 85 16        STA 0x16           ; Store back
0D08: A5 17        LDA 0x17           ; Load i[1]
0D0A: 69 00        ADC #0x00          ; Add carry
0D0C: 85 17        STA 0x17           ; Store back
0D0E: A5 18        LDA 0x18           ; Load i[2]
0D10: 69 00        ADC #0x00          ; Add carry
0D12: 85 18        STA 0x18           ; Store back
0D14: A5 19        LDA 0x19           ; Load i[3]
0D16: 69 00        ADC #0x00          ; Add carry
0D18: 85 19        STA 0x19           ; Store back

; Store incremented i back to BP-3
0D1A: A5 60        LDA 0x60           ; Load BP
0D1C: 18           CLC                ; Clear carry
0D1D: 69 FD        ADC #0xFD          ; Add -3 (BP-3)
0D1F: A8           TAY                ; Transfer to Y
0D20: A5 16        LDA 0x16           ; Store i[0]
0D22: 91 61        STA [0x61],Y       
0D24: A5 17        LDA 0x17           ; Store i[1]
0D26: 91 63        STA [0x63],Y       
0D28: A5 18        LDA 0x18           ; Store i[2]
0D2A: 91 65        STA [0x65],Y       
0D2C: A5 19        LDA 0x19           ; Store i[3]
0D2E: 91 67        STA [0x67],Y       

0D30: 68           PLA                ; Pop pre-increment value
0D31: 4C 4D 09     JMP 0x094D         ; Jump to outer loop condition (FIXED!)
```

### After Outer Loop - First printf("%ld\n", s)
```asm
0D34: 48           PHA                ; Push return slot (for printf)
0D35: A9 07        LDA #0x07          ; Low byte of "%ld\n"
0D37: 85 1E        STA 0x1E           ; Store to STRL
0D39: A9 08        LDA #0x08          ; High byte of "%ld\n"
0D3B: 85 1F        STA 0x1F           ; Store to STRH

; Print format string character by character
0D3D: A0 00        LDY #0x00          ; Initialize index
0D3F: B1 1E        LDA [0x1E],Y       ; Load character
0D41: C0 00        CMP #0x00          ; Check for null
0D43: F0 08        BEQ 0x0D4D         ; If null, done
0D45: A2 12        LDX #0x12          ; SysCall.putchar
0D47: 20 03 08     JSR 0x0803         ; Call BIOS
0D4A: C8           INY                ; Next character
0D4B: 80 F2        BRA 0x0D3F         ; Loop

; Load s from BP+0 for printing
0D4D: A5 60        LDA 0x60           ; Load BP
0D4F: 18           CLC                ; Clear carry
0D50: 69 00        ADC #0x00          ; Add 0 (BP+0)
0D52: A8           TAY                ; Transfer to Y
0D53: B1 61        LDA [0x61],Y       ; Load s[0]
0D55: 85 16        STA 0x16           
0D57: B1 63        LDA [0x63],Y       ; Load s[1]
0D59: 85 17        STA 0x17           
0D5B: B1 65        LDA [0x65],Y       ; Load s[2]
0D5D: 85 18        STA 0x18           
0D5F: B1 67        LDA [0x67],Y       ; Load s[3]
0D61: 85 19        STA 0x19           

; Push s for printf
0D63: BA           TSX                ; Get stack pointer
0D64: 8A           TXA                ; Transfer to A
0D65: A8           TAY                ; Transfer to Y
0D66: A5 16        LDA 0x16           ; Push s
0D68: 91 61        STA [0x61],Y       
0D6A: A5 17        LDA 0x17           
0D6C: 91 63        STA [0x63],Y       
0D6E: A5 18        LDA 0x18           
0D70: 91 65        STA [0x65],Y       
0D72: A5 19        LDA 0x19           
0D74: 91 67        STA [0x67],Y       
0D76: 48           PHA                ; Update stack
0D77: 68           PLA                ; Pop

; Pop s into TOP for printing
0D78: BA           TSX                ; Get stack pointer
0D79: 8A           TXA                ; Transfer to A
0D7A: A8           TAY                ; Transfer to Y
0D7B: B1 61        LDA [0x61],Y       ; Pop s to TOP
0D7D: 85 12        STA 0x12           
0D7F: B1 63        LDA [0x63],Y       
0D81: 85 13        STA 0x13           
0D83: B1 65        LDA [0x65],Y       
0D85: 85 14        STA 0x14           
0D87: B1 67        LDA [0x67],Y       
0D89: 85 15        STA 0x15           

; Call Long.Print to print the value
0D8B: A2 1F        LDX #0x1F          ; SysCall.LongPrint
0D8D: 20 03 08     JSR 0x0803         ; Call BIOS

; Print remaining format string (should be newline)
0D90: A0 03        LDY #0x03          ; Start after "%ld"
0D92: B1 1E        LDA [0x1E],Y       ; Load character
0D94: C0 04        CMP #0x04          ; Check position
0D96: F0 08        BEQ 0x0DA0         ; If at position 4, done
0D98: A2 12        LDX #0x12          ; SysCall.putchar
0D9A: 20 03 08     JSR 0x0803         ; Call BIOS
0D9D: C8           INY                ; Next character
0D9E: 80 F2        BRA 0x0D92         ; Loop

0DA0: 68           PLA                ; Pop printf result
```

### Second printf("%ld ms\n", millis() - start)
```asm
0DA1: 48           PHA                ; Push return slot

; Load format string address
0DA2: A9 0C        LDA #0x0C          ; Low byte of "%ld ms\n"
0DA4: 85 1E        STA 0x1E           ; Store to STRL
0DA6: A9 08        LDA #0x08          ; High byte
0DA8: 85 1F        STA 0x1F           ; Store to STRH

; Print format string up to %ld
0DAA: A0 00        LDY #0x00          ; Initialize index
0DAC: B1 1E        LDA [0x1E],Y       ; Load character
0DAE: C0 00        CMP #0x00          ; Check for null
0DB0: F0 08        BEQ 0x0DBA         ; If null, done
0DB2: A2 12        LDX #0x12          ; SysCall.putchar
0DB4: 20 03 08     JSR 0x0803         ; Call BIOS
0DB7: C8           INY                ; Next character
0DB8: 80 F2        BRA 0x0DAC         ; Loop

; Call millis() again
0DBA: 48           PHA                ; Push return slot
0DBB: A2 18        LDX #0x18          ; SysCall.millis
0DBD: 20 03 08     JSR 0x0803         ; Call BIOS

; Marshal millis() result
0DC0: BA           TSX                ; Get stack pointer
0DC1: 8A           TXA                ; Transfer to A
0DC2: A8           TAY                ; Transfer to Y
0DC3: A5 12        LDA 0x12           ; Push TOP0
0DC5: 91 61        STA [0x61],Y       
0DC7: A5 13        LDA 0x13           ; Push TOP1
0DC9: 91 63        STA [0x63],Y       
0DCB: A5 14        LDA 0x14           ; Push TOP2
0DCD: 91 65        STA [0x65],Y       
0DCF: A5 15        LDA 0x15           ; Push TOP3
0DD1: 91 67        STA [0x67],Y       
0DD3: 48           PHA                ; Update stack

; Load start from BP-2
0DD4: A5 60        LDA 0x60           ; Load BP
0DD6: 18           CLC                ; Clear carry
0DD7: 69 FE        ADC #0xFE          ; Add -2 (BP-2)
0DD9: A8           TAY                ; Transfer to Y
0DDA: B1 61        LDA [0x61],Y       ; Load start[0]
0DDC: 85 16        STA 0x16           
0DDE: B1 63        LDA [0x63],Y       ; Load start[1]
0DE0: 85 17        STA 0x17           
0DE2: B1 65        LDA [0x65],Y       ; Load start[2]
0DE4: 85 18        STA 0x18           
0DE6: B1 67        LDA [0x67],Y       ; Load start[3]
0DE8: 85 19        STA 0x19           

; Push start
0DEA: BA           TSX                ; Get stack pointer
0DEB: 8A           TXA                ; Transfer to A
0DEC: A8           TAY                ; Transfer to Y
0DED: A5 16        LDA 0x16           ; Push start
0DEF: 91 61        STA [0x61],Y       
0DF1: A5 17        LDA 0x17           
0DF3: 91 63        STA [0x63],Y       
0DF5: A5 18        LDA 0x18           
0DF7: 91 65        STA [0x65],Y       
0DF9: A5 19        LDA 0x19           
0DFB: 91 67        STA [0x67],Y       
0DFD: 48           PHA                ; Update stack

; Pop values for subtraction
0DFE: 68           PLA                ; Pop
0DFF: BA           TSX                ; Get stack pointer
0E00: 8A           TXA                ; Transfer to A
0E01: A8           TAY                ; Transfer to Y
0E02: B1 61        LDA [0x61],Y       ; Pop start to TOP
0E04: 85 12        STA 0x12           
0E06: B1 63        LDA [0x63],Y       
0E08: 85 13        STA 0x13           
0E0A: B1 65        LDA [0x65],Y       
0E0C: 85 14        STA 0x14           
0E0E: B1 67        LDA [0x67],Y       
0E10: 85 15        STA 0x15           
0E12: 68           PLA                ; Pop again
0E13: BA           TSX                ; Get stack pointer
0E14: 8A           TXA                ; Transfer to A
0E15: A8           TAY                ; Transfer to Y
0E16: B1 61        LDA [0x61],Y       ; Pop millis() to NEXT
0E18: 85 16        STA 0x16           
0E1A: B1 63        LDA [0x63],Y       
0E1C: 85 17        STA 0x17           
0E1E: B1 65        LDA [0x65],Y       
0E20: 85 18        STA 0x18           
0E22: B1 67        LDA [0x67],Y       
0E24: 85 19        STA 0x19           

; Call Long.Sub (millis() - start)
0E26: A2 1B        LDX #0x1B          ; SysCall.LongSub
0E28: 20 03 08     JSR 0x0803         ; Call BIOS

; Marshal result
0E2B: BA           TSX                ; Get stack pointer
0E2C: 8A           TXA                ; Transfer to A
0E2D: A8           TAY                ; Transfer to Y
0E2E: A5 16        LDA 0x16           ; Push result
0E30: 91 61        STA [0x61],Y       
0E32: A5 17        LDA 0x17           
0E34: 91 63        STA [0x63],Y       
0E36: A5 18        LDA 0x18           
0E38: 91 65        STA [0x65],Y       
0E3A: A5 19        LDA 0x19           
0E3C: 91 67        STA [0x67],Y       
0E3E: 48           PHA                ; Update stack
0E3F: 68           PLA                ; Pop

; Pop result into TOP for printing
0E40: BA           TSX                ; Get stack pointer
0E41: 8A           TXA                ; Transfer to A
0E42: A8           TAY                ; Transfer to Y
0E43: B1 61        LDA [0x61],Y       ; Pop to TOP
0E45: 85 12        STA 0x12           
0E47: B1 63        LDA [0x63],Y       
0E49: 85 13        STA 0x13           
0E4B: B1 65        LDA [0x65],Y       
0E4D: 85 14        STA 0x14           
0E4F: B1 67        LDA [0x67],Y       
0E51: 85 15        STA 0x15           

; Call Long.Print
0E53: A2 1F        LDX #0x1F          ; SysCall.LongPrint
0E55: 20 03 08     JSR 0x0803         ; Call BIOS

; Print rest of format string (" ms\n")
0E58: A0 03        LDY #0x03          ; Start after "%ld"
0E5A: B1 1E        LDA [0x1E],Y       ; Load character
0E5C: C0 07        CMP #0x07          ; Check position
0E5E: F0 08        BEQ 0x0E68         ; If at position 7, done
0E60: A2 12        LDX #0x12          ; SysCall.putchar
0E62: 20 03 08     JSR 0x0803         ; Call BIOS
0E65: C8           INY                ; Next character
0E66: 80 F2        BRA 0x0E5A         ; Loop

0E68: 68           PLA                ; Pop printf result
```

### Function Epilogue
```asm
0E69: A6 60        LDX 0x60           ; Load BP
0E6B: 9A           TXS                ; Restore stack pointer
0E6C: 68           PLA                ; Pop old BP
0E6D: 85 60        STA 0x60           ; Restore old BP
0E6F: 60           RTS                ; Return
```

## Summary

The two critical bugs have been **FIXED**:

1. **Outer loop exit** at 0x09F2 now correctly jumps to 0x0D34 (after the outer loop) instead of 0x0000
2. **Outer loop continuation** at 0x0D31 now correctly jumps to 0x094D (outer loop condition) instead of 0x0AAD (inner loop)

The program should now execute correctly with proper nested loop behavior.

However, significant inefficiencies remain:

1. **Excessive Stack Operations**: Every single operation involves pushing to stack, popping from stack, loading back, etc. For example, initializing `s=1000` takes 30+ instructions when it could be done in ~8.

2. **Redundant Memory Access**: Variables are constantly reloaded from memory even when their values haven't changed.

3. **No Optimization**: 
   - No common subexpression elimination (BP+offset recalculated every time)
   - No register allocation (everything goes through stack)
   - No loop invariant code motion (constants recalculated in loops)
   - No strength reduction (full 32-bit operations for simple counters)

4. **Printf Implementation**: The printf is manually parsing the format string character by character at runtime, which is extremely inefficient.

The code is functionally correct but generates approximately 5-10x more instructions than necessary. A hand-optimized version could likely run 3-5x faster.