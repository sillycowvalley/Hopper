# Complete Disassembly of For Loop Program

## Source Code Reference
```c
void main() {
    int i;
    for (i=0; i < 10; i++) {
        putchar('.');
    }
}
```

## Full Disassembly Listing

```asm
; Program Entry and BIOS Dispatcher
0800: 4C 07 08     JMP 0x0807         ; Jump to main entry point
0803: 6C 22 00     JMP [0x0022]       ; BIOS dispatcher (indirect through ZP.BIOSDISPATCH)
0806: 60           RTS                ; Return

; Main Function Entry
0807: A2 00        LDX #0x00          ; Push return slot
0809: 20 03 08     JSR 0x0803         ; Call BIOS (no-op for main)

; Initialize Runtime Stack Pointers
080C: 64 61        STZ 0x61           ; runtimeStack0L = 0
080E: 64 63        STZ 0x63           ; runtimeStack1L = 0
0810: 64 65        STZ 0x65           ; runtimeStack2L = 0
0812: 64 67        STZ 0x67           ; runtimeStack3L = 0
0814: A5 1B        LDA 0x1B           ; Load ZP.IDXH (typically 0x01)
0816: 85 62        STA 0x62           ; runtimeStack0H = 0x01
0818: 1A           INC A              ; A = 0x02
0819: 85 64        STA 0x64           ; runtimeStack1H = 0x02
081B: 1A           INC A              ; A = 0x03
081C: 85 66        STA 0x66           ; runtimeStack2H = 0x03
081E: 1A           INC A              ; A = 0x04
081F: 85 68        STA 0x68           ; runtimeStack3H = 0x04

; Function Prologue
0821: A5 60        LDA 0x60           ; Load current BP
0823: 48           PHA                ; Save old BP on hardware stack
0824: BA           TSX                ; Get stack pointer
0825: 86 60        STX 0x60           ; Set new BP = SP
0827: 48           PHA                ; Reserve space for local variable i

; Initialize i = 0 (For loop init expression)
0828: 64 16        STZ 0x16           ; NEXT0 = 0
082A: 64 17        STZ 0x17           ; NEXT1 = 0
082C: 64 18        STZ 0x18           ; NEXT2 = 0
082E: 64 19        STZ 0x19           ; NEXT3 = 0
0830: BA           TSX                ; Get stack pointer
0831: 8A           TXA                ; Transfer to A
0832: A8           TAY                ; Transfer to Y for indirect addressing
0833: A5 16        LDA 0x16           ; Load NEXT0 (0)
0835: 91 61        STA [0x61],Y       ; Store to stack[0]
0837: A5 17        LDA 0x17           ; Load NEXT1 (0)
0839: 91 63        STA [0x63],Y       ; Store to stack[1]
083B: A5 18        LDA 0x18           ; Load NEXT2 (0)
083D: 91 65        STA [0x65],Y       ; Store to stack[2]
083F: A5 19        LDA 0x19           ; Load NEXT3 (0)
0841: 91 67        STA [0x67],Y       ; Store to stack[3]
0843: 48           PHA                ; Push init expression result
0844: 68           PLA                ; Pop and discard init result

; Load i from stack
0845: BA           TSX                ; Get stack pointer
0846: 8A           TXA                ; Transfer to A
0847: A8           TAY                ; Transfer to Y
0848: B1 61        LDA [0x61],Y       ; Load i[0] from stack
084A: 85 16        STA 0x16           ; Store to NEXT0
084C: B1 63        LDA [0x63],Y       ; Load i[1] from stack
084E: 85 17        STA 0x17           ; Store to NEXT1
0850: B1 65        LDA [0x65],Y       ; Load i[2] from stack
0852: 85 18        STA 0x18           ; Store to NEXT2
0854: B1 67        LDA [0x67],Y       ; Load i[3] from stack
0856: 85 19        STA 0x19           ; Store to NEXT3

; Store i to BP-1 (local variable location)
0858: A5 60        LDA 0x60           ; Load BP
085A: 18           CLC                ; Clear carry
085B: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
085D: A8           TAY                ; Transfer to Y
085E: A5 16        LDA 0x16           ; Load NEXT0
0860: 91 61        STA [0x61],Y       ; Store to [BP-1][0]
0862: A5 17        LDA 0x17           ; Load NEXT1
0864: 91 63        STA [0x63],Y       ; Store to [BP-1][1]
0866: A5 18        LDA 0x18           ; Load NEXT2
0868: 91 65        STA [0x65],Y       ; Store to [BP-1][2]
086A: A5 19        LDA 0x19           ; Load NEXT3
086C: 91 67        STA [0x67],Y       ; Store to [BP-1][3]

; Push i as assignment expression result
086E: BA           TSX                ; Get stack pointer
086F: 8A           TXA                ; Transfer to A
0870: A8           TAY                ; Transfer to Y
0871: A5 16        LDA 0x16           ; Load NEXT0
0873: 91 61        STA [0x61],Y       ; Push to stack[0]
0875: A5 17        LDA 0x17           ; Load NEXT1
0877: 91 63        STA [0x63],Y       ; Push to stack[1]
0879: A5 18        LDA 0x18           ; Load NEXT2
087B: 91 65        STA [0x65],Y       ; Push to stack[2]
087D: A5 19        LDA 0x19           ; Load NEXT3
087F: 91 67        STA [0x67],Y       ; Push to stack[3]
0881: 48           PHA                ; Update stack pointer
0882: 68           PLA                ; Pop assignment result

; *** LOOP START (BACKWARD JUMP TARGET) ***
0883: EA           NOP                ; No operation (padding?)

; Load i from BP-1
0884: A5 60        LDA 0x60           ; Load BP
0886: 18           CLC                ; Clear carry
0887: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
0889: A8           TAY                ; Transfer to Y
088A: B1 61        LDA [0x61],Y       ; Load i[0] from [BP-1]
088C: 85 16        STA 0x16           ; Store to NEXT0
088E: B1 63        LDA [0x63],Y       ; Load i[1] from [BP-1]
0890: 85 17        STA 0x17           ; Store to NEXT1
0892: B1 65        LDA [0x65],Y       ; Load i[2] from [BP-1]
0894: 85 18        STA 0x18           ; Store to NEXT2
0896: B1 67        LDA [0x67],Y       ; Load i[3] from [BP-1]
0898: 85 19        STA 0x19           ; Store to NEXT3

; Push i for comparison
089A: BA           TSX                ; Get stack pointer
089B: 8A           TXA                ; Transfer to A
089C: A8           TAY                ; Transfer to Y
089D: A5 16        LDA 0x16           ; Load NEXT0
089F: 91 61        STA [0x61],Y       ; Push to stack[0]
08A1: A5 17        LDA 0x17           ; Load NEXT1
08A3: 91 63        STA [0x63],Y       ; Push to stack[1]
08A5: A5 18        LDA 0x18           ; Load NEXT2
08A7: 91 65        STA [0x65],Y       ; Push to stack[2]
08A9: A5 19        LDA 0x19           ; Load NEXT3
08AB: 91 67        STA [0x67],Y       ; Push to stack[3]
08AD: 48           PHA                ; Update stack pointer

; Push constant 10 for comparison
08AE: A9 0A        LDA #0x0A          ; Load 10
08B0: 85 16        STA 0x16           ; NEXT0 = 10
08B2: 64 17        STZ 0x17           ; NEXT1 = 0
08B4: 64 18        STZ 0x18           ; NEXT2 = 0
08B6: 64 19        STZ 0x19           ; NEXT3 = 0
08B8: BA           TSX                ; Get stack pointer
08B9: 8A           TXA                ; Transfer to A
08BA: A8           TAY                ; Transfer to Y
08BB: A5 16        LDA 0x16           ; Load NEXT0 (10)
08BD: 91 61        STA [0x61],Y       ; Push to stack[0]
08BF: A5 17        LDA 0x17           ; Load NEXT1 (0)
08C1: 91 63        STA [0x63],Y       ; Push to stack[1]
08C3: A5 18        LDA 0x18           ; Load NEXT2 (0)
08C5: 91 65        STA [0x65],Y       ; Push to stack[2]
08C7: A5 19        LDA 0x19           ; Load NEXT3 (0)
08C9: 91 67        STA [0x67],Y       ; Push to stack[3]
08CB: 48           PHA                ; Update stack pointer

; Pop values for LT comparison
08CC: 68           PLA                ; Pop
08CD: BA           TSX                ; Get stack pointer
08CE: 8A           TXA                ; Transfer to A
08CF: A8           TAY                ; Transfer to Y
08D0: B1 61        LDA [0x61],Y       ; Pop stack[0] to TOP0
08D2: 85 12        STA 0x12           ; Store to TOP0
08D4: B1 63        LDA [0x63],Y       ; Pop stack[1] to TOP1
08D6: 85 13        STA 0x13           ; Store to TOP1
08D8: B1 65        LDA [0x65],Y       ; Pop stack[2] to TOP2
08DA: 85 14        STA 0x14           ; Store to TOP2
08DC: B1 67        LDA [0x67],Y       ; Pop stack[3] to TOP3
08DE: 85 15        STA 0x15           ; Store to TOP3
08E0: 68           PLA                ; Pop again
08E1: BA           TSX                ; Get stack pointer
08E2: 8A           TXA                ; Transfer to A
08E3: A8           TAY                ; Transfer to Y
08E4: B1 61        LDA [0x61],Y       ; Pop stack[0] to NEXT0
08E6: 85 16        STA 0x16           ; Store to NEXT0
08E8: B1 63        LDA [0x63],Y       ; Pop stack[1] to NEXT1
08EA: 85 17        STA 0x17           ; Store to NEXT1
08EC: B1 65        LDA [0x65],Y       ; Pop stack[2] to NEXT2
08EE: 85 18        STA 0x18           ; Store to NEXT2
08F0: B1 67        LDA [0x67],Y       ; Pop stack[3] to NEXT3
08F2: 85 19        STA 0x19           ; Store to NEXT3

; Call Long.LT comparison
08F4: A2 20        LDX #0x20          ; SysCall.LongLT
08F6: 20 03 08     JSR 0x0803         ; Call BIOS dispatch

; Marshal comparison result to stack
08F9: BA           TSX                ; Get stack pointer
08FA: 8A           TXA                ; Transfer to A
08FB: A8           TAY                ; Transfer to Y
08FC: A9 00        LDA #0x00          ; Load 0
08FE: 69 00        ADC #0x00          ; Add carry (result from LT)
0900: 91 61        STA [0x61],Y       ; Store result to stack[0]
0902: A9 00        LDA #0x00          ; Load 0
0904: 91 63        STA [0x63],Y       ; Store to stack[1]
0906: 91 65        STA [0x65],Y       ; Store to stack[2]
0908: 91 67        STA [0x67],Y       ; Store to stack[3]
090A: 48           PHA                ; Push result
090B: 68           PLA                ; Pop result

; Load comparison result to check
090C: BA           TSX                ; Get stack pointer
090D: 8A           TXA                ; Transfer to A
090E: A8           TAY                ; Transfer to Y
090F: B1 61        LDA [0x61],Y       ; Load result[0]
0911: 85 16        STA 0x16           ; Store to NEXT0
0913: B1 63        LDA [0x63],Y       ; Load result[1]
0915: 85 17        STA 0x17           ; Store to NEXT1
0917: B1 65        LDA [0x65],Y       ; Load result[2]
0919: 85 18        STA 0x18           ; Store to NEXT2
091B: B1 67        LDA [0x67],Y       ; Load result[3]
091D: 85 19        STA 0x19           ; Store to NEXT3

; Test if result is zero (exit condition)
091F: A5 16        LDA 0x16           ; Load NEXT0
0921: 05 17        ORA 0x17           ; OR with NEXT1
0923: 05 18        ORA 0x18           ; OR with NEXT2
0925: 05 19        ORA 0x19           ; OR with NEXT3
0927: D0 03        BNE 0x092C         ; If not zero (true), skip exit
0929: 4C D8 09     JMP 0x09D8         ; Exit loop if false

; Loop body - prepare putchar('.')
092C: 48           PHA                ; Push return slot
092D: A9 2E        LDA #0x2E          ; Load '.' (ASCII 0x2E)
092F: 85 16        STA 0x16           ; NEXT0 = 0x2E
0931: 64 17        STZ 0x17           ; NEXT1 = 0
0933: 64 18        STZ 0x18           ; NEXT2 = 0
0935: 64 19        STZ 0x19           ; NEXT3 = 0
0937: BA           TSX                ; Get stack pointer
0938: 8A           TXA                ; Transfer to A
0939: A8           TAY                ; Transfer to Y
093A: A5 16        LDA 0x16           ; Load NEXT0 (0x2E)
093C: 91 61        STA [0x61],Y       ; Push to stack[0]
093E: A5 17        LDA 0x17           ; Load NEXT1 (0)
0940: 91 63        STA [0x63],Y       ; Push to stack[1]
0942: A5 18        LDA 0x18           ; Load NEXT2 (0)
0944: 91 65        STA [0x65],Y       ; Push to stack[2]
0946: A5 19        LDA 0x19           ; Load NEXT3 (0)
0948: 91 67        STA [0x67],Y       ; Push to stack[3]
094A: 48           PHA                ; Update stack pointer
094B: 68           PLA                ; Pop

; Pop argument for putchar
094C: BA           TSX                ; Get stack pointer
094D: 8A           TXA                ; Transfer to A
094E: A8           TAY                ; Transfer to Y
094F: B1 61        LDA [0x61],Y       ; Pop stack[0]
0951: 85 16        STA 0x16           ; Store to NEXT0
0953: B1 63        LDA [0x63],Y       ; Pop stack[1]
0955: 85 17        STA 0x17           ; Store to NEXT1
0957: B1 65        LDA [0x65],Y       ; Pop stack[2]
0959: 85 18        STA 0x18           ; Store to NEXT2
095B: B1 67        LDA [0x67],Y       ; Pop stack[3]
095D: 85 19        STA 0x19           ; Store to NEXT3

; Call putchar
095F: A5 16        LDA 0x16           ; Load character '.'
0961: A2 12        LDX #0x12          ; SysCall.putchar
0963: 20 03 08     JSR 0x0803         ; Call BIOS dispatch

; Store putchar result (not used)
0966: BA           TSX                ; Get stack pointer
0967: 8A           TXA                ; Transfer to A
0968: A8           TAY                ; Transfer to Y
0969: A5 16        LDA 0x16           ; Load NEXT0
096B: 91 61        STA [0x61],Y       ; Store to stack[0]
096D: A5 17        LDA 0x17           ; Load NEXT1
096F: 91 63        STA [0x63],Y       ; Store to stack[1]
0971: A5 18        LDA 0x18           ; Load NEXT2
0973: 91 65        STA [0x65],Y       ; Store to stack[2]
0975: A5 19        LDA 0x19           ; Load NEXT3
0977: 91 67        STA [0x67],Y       ; Store to stack[3]
0979: 48           PHA                ; Push result
097A: 68           PLA                ; Pop and discard result

; Load i from BP-1 for increment
097B: A5 60        LDA 0x60           ; Load BP
097D: 18           CLC                ; Clear carry
097E: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
0980: A8           TAY                ; Transfer to Y
0981: B1 61        LDA [0x61],Y       ; Load i[0]
0983: 85 16        STA 0x16           ; Store to NEXT0
0985: B1 63        LDA [0x63],Y       ; Load i[1]
0987: 85 17        STA 0x17           ; Store to NEXT1
0989: B1 65        LDA [0x65],Y       ; Load i[2]
098B: 85 18        STA 0x18           ; Store to NEXT2
098D: B1 67        LDA [0x67],Y       ; Load i[3]
098F: 85 19        STA 0x19           ; Store to NEXT3

; Push current value of i (for postfix)
0991: BA           TSX                ; Get stack pointer
0992: 8A           TXA                ; Transfer to A
0993: A8           TAY                ; Transfer to Y
0994: A5 16        LDA 0x16           ; Load NEXT0
0996: 91 61        STA [0x61],Y       ; Push to stack[0]
0998: A5 17        LDA 0x17           ; Load NEXT1
099A: 91 63        STA [0x63],Y       ; Push to stack[1]
099C: A5 18        LDA 0x18           ; Load NEXT2
099E: 91 65        STA [0x65],Y       ; Push to stack[2]
09A0: A5 19        LDA 0x19           ; Load NEXT3
09A2: 91 67        STA [0x67],Y       ; Push to stack[3]
09A4: 48           PHA                ; Update stack pointer

; Perform increment (i++)
09A5: 18           CLC                ; Clear carry
09A6: A5 16        LDA 0x16           ; Load NEXT0
09A8: 69 01        ADC #0x01          ; Add 1
09AA: 85 16        STA 0x16           ; Store back to NEXT0
09AC: A5 17        LDA 0x17           ; Load NEXT1
09AE: 69 00        ADC #0x00          ; Add carry
09B0: 85 17        STA 0x17           ; Store back to NEXT1
09B2: A5 18        LDA 0x18           ; Load NEXT2
09B4: 69 00        ADC #0x00          ; Add carry
09B6: 85 18        STA 0x18           ; Store back to NEXT2
09B8: A5 19        LDA 0x19           ; Load NEXT3
09BA: 69 00        ADC #0x00          ; Add carry
09BC: 85 19        STA 0x19           ; Store back to NEXT3

; Store incremented value back to BP-1
09BE: A5 60        LDA 0x60           ; Load BP
09C0: 18           CLC                ; Clear carry
09C1: 69 FF        ADC #0xFF          ; Add -1 (BP-1)
09C3: A8           TAY                ; Transfer to Y
09C4: A5 16        LDA 0x16           ; Store incremented i[0]
09C6: 91 61        STA [0x61],Y       ; Store to [BP-1][0]
09C8: A5 17        LDA 0x17           ; Store incremented i[1]
09CA: 91 63        STA [0x63],Y       ; Store to [BP-1][1]
09CC: A5 18        LDA 0x18           ; Store incremented i[2]
09CE: 91 65        STA [0x65],Y       ; Store to [BP-1][2]
09D0: A5 19        LDA 0x19           ; Store incremented i[3]
09D2: 91 67        STA [0x67],Y       ; Store to [BP-1][3]

; End of loop iteration
09D4: 68           PLA                ; Pop pre-increment value
09D5: 4C 83 08     JMP 0x0883         ; Jump back to loop start

; Function Epilogue
09D8: A6 60        LDX 0x60           ; Load BP
09DA: 9A           TXS                ; Restore stack pointer
09DB: 68           PLA                ; Pop old BP
09DC: 85 60        STA 0x60           ; Restore old BP
09DE: 60           RTS                ; Return
```

## Key Observations

1. **Loop starts at 0x0883** with a NOP instruction
2. **No stack pointer reload** at loop start - the code immediately loads from BP-1
3. **Consistent pattern** of TSX, TXA, TAY before all stack operations except at loop start
4. **Postfix increment** correctly saves pre-increment value and discards it
5. **Loop exit** at 0x09D8 properly restores stack frame

## The Bug

The loop doesn't reload the stack pointer at 0x0883, but this appears to be **intentional** - it always accesses the local variable `i` through BP-relative addressing (BP-1), not through the current stack pointer. So this may not actually be the bug causing only 9 dots.

The real issue might be in the initialization or the comparison logic. Need to trace the actual values through the execution to see why it exits after 9 iterations.