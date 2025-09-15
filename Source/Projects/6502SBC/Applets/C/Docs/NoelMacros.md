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
083B: TSX                  ; BA - PushTOP macro start
083C: TXA                  ; 8A
083D: TAY                  ; A8
083E: LDA 0x12             ; A5 12 - TOP0
0840: STA [0x61], Y        ; 91 61
0842: LDA 0x13             ; A5 13 - TOP1  
0844: STA [0x63], Y        ; 91 63
0846: LDA 0x14             ; A5 14 - TOP2
0848: STA [0x65], Y        ; 91 65
084A: LDA 0x15             ; A5 15 - TOP3
084C: STA [0x67], Y        ; 91 67
084E: PHA                  ; 48
084F: PLA                  ; 68 - PushTOP macro end
```

### Store start time to local variable
```hopper
0850: TSX                  ; BA - PopNEXT macro start  
0851: TXA                  ; 8A
0852: TAY                  ; A8
0853: LDA [0x61], Y        ; B1 61
0855: STA 0x16             ; 85 16 - NEXT0
0857: LDA [0x63], Y        ; B1 63
0859: STA 0x17             ; 85 17 - NEXT1
085B: LDA [0x65], Y        ; B1 65
085D: STA 0x18             ; 85 18 - NEXT2
085F: LDA [0x67], Y        ; B1 67
0861: STA 0x19             ; 85 19 - NEXT3 - PopNEXT macro end

0862: LDA 0x60             ; A5 60 - PutNEXT macro start (store to [BP-2])
0864: CLC                  ; 18
0865: ADC #0xFE            ; 69 FE
0867: TAY                  ; A8
0868: LDA 0x16             ; A5 16
086A: STA [0x61], Y        ; 91 61
086C: LDA 0x17             ; A5 17
086E: STA [0x63], Y        ; 91 63
0870: LDA 0x18             ; A5 18
0872: STA [0x65], Y        ; 91 65
0874: LDA 0x19             ; A5 19
0876: STA [0x67], Y        ; 91 67 - PutNEXT macro end
```

### Get start time back and push to stack
```hopper
0878: TSX                  ; BA - Stack management
0879: TXA                  ; 8A
087A: TAY                  ; A8
087B: LDA 0x16             ; A5 16 - Load NEXT
087D: STA [0x61], Y        ; 91 61
087F: LDA 0x17             ; A5 17
0881: STA [0x63], Y        ; 91 63
0883: LDA 0x18             ; A5 18
0885: STA [0x65], Y        ; 91 65
0887: LDA 0x19             ; A5 19
0889: STA [0x67], Y        ; 91 67
088B: PHA                  ; 48
088C: PLA                  ; 68
```

### Initialize i = 1  
```hopper
088D: PHA                  ; 48
088E: PHA                  ; 48
088F: LDA #0x01            ; A9 01 - LongNEXT #1 macro start
0891: STA 0x16             ; 85 16
0893: STZ 0x17             ; 64 17
0895: STZ 0x18             ; 64 18
0897: STZ 0x19             ; 64 19 - LongNEXT #1 macro end
0899: TSX                  ; BA - PushNEXT macro
089A: TXA                  ; 8A
089B: TAY                  ; A8
089C: LDA 0x16             ; A5 16
089E: STA [0x61], Y        ; 91 61
08A0: LDA 0x17             ; A5 17
08A2: STA [0x63], Y        ; 91 63
08A4: LDA 0x18             ; A5 18
08A6: STA [0x65], Y        ; 91 65
08A8: LDA 0x19             ; A5 19
08AA: STA [0x67], Y        ; 91 67
08AC: PHA                  ; 48
08AD: PLA                  ; 68
```

### Outer loop - get i and store locally
```hopper
08AE: TSX                  ; BA - PopNEXT macro
08AF: TXA                  ; 8A
08B0: TAY                  ; A8
08B1: LDA [0x61], Y        ; B1 61
08B3: STA 0x16             ; 85 16
08B5: LDA [0x63], Y        ; B1 63
08B7: STA 0x17             ; 85 17
08B9: LDA [0x65], Y        ; B1 65
08BB: STA 0x18             ; 85 18
08BD: LDA [0x67], Y        ; B1 67
08BF: STA 0x19             ; 85 19

08C1: LDA 0x60             ; A5 60 - PutNEXT to [BP-3]
08C3: CLC                  ; 18
08C4: ADC #0xFD            ; 69 FD
08C6: TAY                  ; A8
08C7: LDA 0x16             ; A5 16
08C9: STA [0x61], Y        ; 91 61
08CB: LDA 0x17             ; A5 17
08CD: STA [0x63], Y        ; 91 63
08CF: LDA 0x18             ; A5 18
08D1: STA [0x65], Y        ; 91 65
08D3: LDA 0x19             ; A5 19
08D5: STA [0x67], Y        ; 91 67
```

### Inner loop setup - s = 0, j = 1
```hopper
08D7: TSX                  ; BA - Stack operations
08D8: TXA                  ; 8A
08D9: TAY                  ; A8
08DA: LDA 0x16             ; A5 16
08DC: STA [0x61], Y        ; 91 61
08DE: LDA 0x17             ; A5 17
08E0: STA [0x63], Y        ; 91 63
08E2: LDA 0x18             ; A5 18
08E4: STA [0x65], Y        ; 91 65
08E6: LDA 0x19             ; A5 19
08E8: STA [0x67], Y        ; 91 67
08EA: PHA                  ; 48

08EB: LDA 0x60             ; A5 60 - GetNEXT from [BP-3]
08ED: CLC                  ; 18
08EE: ADC #0xFD            ; 69 FD
08F0: TAY                  ; A8
08F1: LDA [0x61], Y        ; B1 61
08F3: STA 0x16             ; 85 16
08F5: LDA [0x63], Y        ; B1 63
08F7: STA 0x17             ; 85 17
08F9: LDA [0x65], Y        ; B1 65
08FB: STA 0x18             ; 85 18
08FD: LDA [0x67], Y        ; B1 67
08FF: STA 0x19             ; 85 19

0901: TSX                  ; BA - PushNEXT
0902: TXA                  ; 8A
0903: TAY                  ; A8
0904: LDA 0x16             ; A5 16
0906: STA [0x61], Y        ; 91 61
0908: LDA 0x17             ; A5 17
090A: STA [0x63], Y        ; 91 63
090C: LDA 0x18             ; A5 18
090E: STA [0x65], Y        ; 91 65
0910: LDA 0x19             ; A5 19
0912: STA [0x67], Y        ; 91 67
0914: PHA                  ; 48

0915: LDA #0x0A            ; A9 0A - LongNEXT #10
0917: STA 0x16             ; 85 16
0919: STZ 0x17             ; 64 17
091B: STZ 0x18             ; 64 18
091D: STZ 0x19             ; 64 19

091F: TSX                  ; BA - PushNEXT
0920: TXA                  ; 8A
0921: TAY                  ; A8
0922: LDA 0x16             ; A5 16
0924: STA [0x61], Y        ; 91 61
0926: LDA 0x17             ; A5 17
0928: STA [0x63], Y        ; 91 63
092A: LDA 0x18             ; A5 18
092C: STA [0x65], Y        ; 91 65
092E: LDA 0x19             ; A5 19
0930: STA [0x67], Y        ; 91 67
0932: PHA                  ; 48
0933: PLA                  ; 68

0934: TSX                  ; BA - PopTOP macro
0935: TXA                  ; 8A
0936: TAY                  ; A8
0937: LDA [0x61], Y        ; B1 61
0939: STA 0x12             ; 85 12
093B: LDA [0x63], Y        ; B1 63
093D: STA 0x13             ; 85 13
093F: LDA [0x65], Y        ; B1 65
0941: STA 0x14             ; 85 14
0943: LDA [0x67], Y        ; B1 67
0945: STA 0x15             ; 85 15
0947: PLA                  ; 68

0948: TSX                  ; BA - PopNEXT macro  
0949: TXA                  ; 8A
094A: TAY                  ; A8
094B: LDA [0x61], Y        ; B1 61
094D: STA 0x16             ; 85 16
094F: LDA [0x63], Y        ; B1 63
0951: STA 0x17             ; 85 17
0953: LDA [0x65], Y        ; B1 65
0955: STA 0x18             ; 85 18
0957: LDA [0x67], Y        ; B1 67
0959: STA 0x19             ; 85 19

095B: LDX #0x24            ; A2 24 - SysCall.LongAdd
095D: JSR 0x0803           ; 20 03 08

095F: TSX                  ; BA - Store result
0960: TXA                  ; 8A
0961: TAY                  ; A8
0962: LDA #0x00            ; A9 00
0964: ADC #0x00            ; 69 00
0966: STA [0x61], Y        ; 91 61
0968: LDA #0x00            ; A9 00
096A: STA [0x63], Y        ; 91 63
096C: STA [0x65], Y        ; 91 65
096E: STA [0x67], Y        ; 91 67
0970: PHA                  ; 48
0971: PLA                  ; 68

0972: TSX                  ; BA - Load result for test
0973: TXA                  ; 8A
0974: TAY                  ; A8
0975: LDA [0x61], Y        ; B1 61
0977: STA 0x16             ; 85 16
0979: LDA [0x63], Y        ; B1 63
097B: STA 0x17             ; 85 17
097D: LDA [0x65], Y        ; B1 65
097F: STA 0x18             ; 85 18
0981: LDA [0x67], Y        ; B1 67
0983: STA 0x19             ; 85 19

0985: LDA 0x16             ; A5 16 - Test all bytes of result
0987: ORA 0x17             ; 05 17
0989: ORA 0x18             ; 05 18
098B: ORA 0x19             ; 05 19
098D: BNE 0x0992           ; D0 03 - Branch if non-zero
098F: JMP 0x0CD5           ; 4C D5 0C - Exit if zero

0992: STZ 0x16             ; 64 16 - LongNEXT #0 (initialize s = 0)
0994: STZ 0x17             ; 64 17
0996: STZ 0x18             ; 64 18
0998: STZ 0x19             ; 64 19

099A: TSX                  ; BA - PushNEXT
099B: TXA                  ; 8A
099C: TAY                  ; A8
099D: LDA 0x16             ; A5 16
099F: STA [0x61], Y        ; 91 61
09A1: LDA 0x17             ; A5 17
09A3: STA [0x63], Y        ; 91 63
09A5: LDA 0x18             ; A5 18
09A7: STA [0x65], Y        ; 91 65
09A9: LDA 0x19             ; A5 19
09AB: STA [0x67], Y        ; 91 67
09AD: PHA                  ; 48
09AE: PLA                  ; 68

09AF: TSX                  ; BA - Stack operations
09B0: TXA                  ; 8A
09B1: TAY                  ; A8
09B2: LDA [0x61], Y        ; B1 61
09B4: STA 0x16             ; 85 16
09B6: LDA [0x63], Y        ; B1 63
09B8: STA 0x17             ; 85 17
09BA: LDA [0x65], Y        ; B1 65
09BC: STA 0x18             ; 85 18
09BE: LDA [0x67], Y        ; B1 67
09C0: STA 0x19             ; 85 19

09C2: LDA 0x60             ; A5 60 - Store to local variable
09C4: CLC                  ; 18
09C5: ADC #0x00            ; 69 00
09C7: TAY                  ; A8
09C8: LDA 0x16             ; A5 16
09CA: STA [0x61], Y        ; 91 61
09CC: LDA 0x17             ; A5 17
09CE: STA [0x63], Y        ; 91 63
09D0: LDA 0x18             ; A5 18
09D2: STA [0x65], Y        ; 91 65
09D4: LDA 0x19             ; A5 19
09D6: STA [0x67], Y        ; 91 67

09D8: TSX                  ; BA - More stack operations
09D9: TXA                  ; 8A
09DA: TAY                  ; A8
09DB: LDA 0x16             ; A5 16
09DD: STA [0x61], Y        ; 91 61
09DF: LDA 0x17             ; A5 17
09E1: STA [0x63], Y        ; 91 63
09E3: LDA 0x18             ; A5 18
09E5: STA [0x65], Y        ; 91 65
09E7: LDA 0x19             ; A5 19
09E9: STA [0x67], Y        ; 91 67
09EB: PHA                  ; 48
09EC: PLA                  ; 68

09ED: LDA #0x01            ; A9 01 - LongNEXT #1 (j = 1)
09EF: STA 0x16             ; 85 16
09F1: STZ 0x17             ; 64 17
09F3: STZ 0x18             ; 64 18
09F5: STZ 0x19             ; 64 19

09F7: TSX                  ; BA - PushNEXT
09F8: TXA                  ; 8A
09F9: TAY                  ; A8
09FA: LDA 0x16             ; A5 16
09FC: STA [0x61], Y        ; 91 61
09FE: LDA 0x17             ; A5 17
0A00: STA [0x63], Y        ; 91 63
0A02: LDA 0x18             ; A5 18
0A04: STA [0x65], Y        ; 91 65
0A06: LDA 0x19             ; A5 19
0A08: STA [0x67], Y        ; 91 67
0A0A: PHA                  ; 48
0A0B: PLA                  ; 68

0A0C: TSX                  ; BA - PopNEXT
0A0D: TXA                  ; 8A
0A0E: TAY                  ; A8
0A0F: LDA [0x61], Y        ; B1 61
0A11: STA 0x16             ; 85 16
0A13: LDA [0x63], Y        ; B1 63
0A15: STA 0x17             ; 85 17
0A17: LDA [0x65], Y        ; B1 65
0A19: STA 0x18             ; 85 18
0A1B: LDA [0x67], Y        ; B1 67
0A1D: STA 0x19             ; 85 19

0A1F: LDA 0x60             ; A5 60 - PutNEXT to [BP-4]
0A21: CLC                  ; 18
0A22: ADC #0xFC            ; 69 FC
0A24: TAY                  ; A8
0A25: LDA 0x16             ; A5 16
0A27: STA [0x61], Y        ; 91 61
0A29: LDA 0x17             ; A5 17
0A2B: STA [0x63], Y        ; 91 63
0A2D: LDA 0x18             ; A5 18
0A2F: STA [0x65], Y        ; 91 65
0A31: LDA 0x19             ; A5 19
0A33: STA [0x67], Y        ; 91 67

0A35: TSX                  ; BA - Stack management
0A36: TXA                  ; 8A
0A37: TAY                  ; A8
0A38: LDA 0x16             ; A5 16
0A3A: STA [0x61], Y        ; 91 61
0A3C: LDA 0x17             ; A5 17
0A3E: STA [0x63], Y        ; 91 63
0A40: LDA 0x18             ; A5 18
0A42: STA [0x65], Y        ; 91 65
0A44: LDA 0x19             ; A5 19
0A46: STA [0x67], Y        ; 91 67
0A48: PHA                  ; 48

0A49: LDA 0x60             ; A5 60 - GetNEXT from [BP-4]
0A4B: CLC                  ; 18
0A4C: ADC #0xFC            ; 69 FC
0A4E: TAY                  ; A8
0A4F: LDA [0x61], Y        ; B1 61
0A51: STA 0x16             ; 85 16
0A53: LDA [0x63], Y        ; B1 63
0A55: STA 0x17             ; 85 17
0A57: LDA [0x65], Y        ; B1 65
0A59: STA 0x18             ; 85 18
0A5B: LDA [0x67], Y        ; B1 67
0A5D: STA 0x19             ; 85 19

0A5F: TSX                  ; BA - PushNEXT
0A60: TXA                  ; 8A
0A61: TAY                  ; A8
0A62: LDA 0x16             ; A5 16
0A64: STA [0x61], Y        ; 91 61
0A66: LDA 0x17             ; A5 17
0A68: STA [0x63], Y        ; 91 63
0A6A: LDA 0x18             ; A5 18
0A6C: STA [0x65], Y        ; 91 65
0A6E: LDA 0x19             ; A5 19
0A70: STA [0x67], Y        ; 91 67
0A72: PHA                  ; 48

0A73: LDA #0xE8            ; A9 E8 - LongNEXT #1000 (0x03E8)
0A75: STA 0x16             ; 85 16
0A77: LDA #0x03            ; A9 03
0A79: STA 0x17             ; 85 17
0A7B: STZ 0x18             ; 64 18
0A7D: STZ 0x19             ; 64 19

0A7F: TSX                  ; BA - PushNEXT
0A80: TXA                  ; 8A
0A81: TAY                  ; A8
0A82: LDA 0x16             ; A5 16
0A84: STA [0x61], Y        ; 91 61
0A86: LDA 0x17             ; A5 17
0A88: STA [0x63], Y        ; 91 63
0A8A: LDA 0x18             ; A5 18
0A8C: STA [0x65], Y        ; 91 65
0A8E: LDA 0x19             ; A5 19
0A90: STA [0x67], Y        ; 91 67
0A92: PLA                  ; 68

0A93: TSX                  ; BA - PopTOP
0A94: TXA                  ; 8A
0A95: TAY                  ; A8
0A96: LDA [0x61], Y        ; B1 61
0A98: STA 0x12             ; 85 12
0A9A: LDA [0x63], Y        ; B1 63
0A9C: STA 0x13             ; 85 13
0A9E: LDA [0x65], Y        ; B1 65
0AA0: STA 0x14             ; 85 14
0AA2: LDA [0x67], Y        ; B1 67
0AA4: STA 0x15             ; 85 15
0AA6: PLA                  ; 68

0AA7: TSX                  ; BA - PopNEXT
0AA8: TXA                  ; 8A
0AA9: TAY                  ; A8
0AAA: LDA [0x61], Y        ; B1 61
0AAC: STA 0x16             ; 85 16
0AAE: LDA [0x63], Y        ; B1 63
0AB0: STA 0x17             ; 85 17
0AB2: LDA [0x65], Y        ; B1 65
0AB4: STA 0x18             ; 85 18
0AB6: LDA [0x67], Y        ; B1 67
0AB8: STA 0x19             ; 85 19

0ABA: LDX #0x24            ; A2 24 - SysCall.LongAdd
0ABC: JSR 0x0803           ; 20 03 08

0ABE: TSX                  ; BA - Store result
0ABF: TXA                  ; 8A
0AC0: TAY                  ; A8
0AC1: LDA #0x00            ; A9 00
0AC3: ADC #0x00            ; 69 00
0AC5: STA [0x61], Y        ; 91 61
0AC7: LDA #0x00            ; A9 00
0AC9: STA [0x63], Y        ; 91 63
0ACB: STA [0x65], Y        ; 91 65
0ACD: STA [0x67], Y        ; 91 67
0ACF: PHA                  ; 48
0AD0: PLA                  ; 68

0AD1: TSX                  ; BA - Load result for test
0AD2: TXA                  ; 8A
0AD3: TAY                  ; A8
0AD4: LDA [0x61], Y        ; B1 61
0AD6: STA 0x16             ; 85 16
0AD8: LDA [0x63], Y        ; B1 63
0ADA: STA 0x17             ; 85 17
0ADC: LDA [0x65], Y        ; B1 65
0ADE: STA 0x18             ; 85 18
0AE0: LDA [0x67], Y        ; B1 67
0AE2: STA 0x19             ; 85 19

0AE4: LDA 0x16             ; A5 16 - Test result
0AE6: ORA 0x17             ; 05 17
0AE8: ORA 0x18             ; 05 18
0AEA: ORA 0x19             ; 05 19
0AEC: BNE 0x0AF1           ; D0 03 - Branch if non-zero
0AEE: JMP 0x0C29           ; 4C 29 0C - Jump if zero

0AF1: LDA 0x60             ; A5 60 - GetNEXT from [BP+0]
0AF3: CLC                  ; 18
0AF4: ADC #0x00            ; 69 00
0AF6: TAY                  ; A8
0AF7: LDA [0x61], Y        ; B1 61
0AF9: STA 0x16             ; 85 16
0AFB: LDA [0x63], Y        ; B1 63
0AFD: STA 0x17             ; 85 17
0AFF: LDA [0x65], Y        ; B1 65
0B01: STA 0x18             ; 85 18
0B03: LDA [0x67], Y        ; B1 67
0B05: STA 0x19             ; 85 19

0B07: TSX                  ; BA - PushNEXT
0B08: TXA                  ; 8A
0B09: TAY                  ; A8
0B0A: LDA 0x16             ; A5 16
0B0C: STA [0x61], Y        ; 91 61
0B0E: LDA 0x17             ; A5 17
0B10: STA [0x63], Y        ; 91 63
0B12: LDA 0x18             ; A5 18
0B14: STA [0x65], Y        ; 91 65
0B16: LDA 0x19             ; A5 19
0B18: STA [0x67], Y        ; 91 67
0B1A: PHA                  ; 48

0B1B: LDA 0x60             ; A5 60 - GetNEXT from [BP-4]
0B1D: CLC                  ; 18
0B1E: ADC #0xFC            ; 69 FC
0B20: TAY                  ; A8
0B21: LDA [0x61], Y        ; B1 61
0B23: STA 0x16             ; 85 16
0B25: LDA [0x63], Y        ; B1 63
0B27: STA 0x17             ; 85 17
0B29: LDA [0x65], Y        ; B1 65
0B2B: STA 0x18             ; 85 18
0B2D: LDA [0x67], Y        ; B1 67
0B2F: STA 0x19             ; 85 19

0B31: TSX                  ; BA - PushNEXT
0B32: TXA                  ; 8A
0B33: TAY                  ; A8
0B34: LDA 0x16             ; A5 16
0B36: STA [0x61], Y        ; 91 61
0B38: LDA 0x17             ; A5 17
0B3A: STA [0x63], Y        ; 91 63
0B3C: LDA 0x18             ; A5 18
0B3E: STA [0x65], Y        ; 91 65
0B40: LDA 0x19             ; A5 19
0B42: STA [0x67], Y        ; 91 67
0B44: PLA                  ; 68
0B45: PLA                  ; 68

0B46: TSX                  ; BA - PopTOP
0B47: TXA                  ; 8A
0B48: TAY                  ; A8
0B49: LDA [0x61], Y        ; B1 61
0B4B: STA 0x12             ; 85 12
0B4D: LDA [0x63], Y        ; B1 63
0B4F: STA 0x13             ; 85 13
0B51: LDA [0x65], Y        ; B1 65
0B53: STA 0x14             ; 85 14
0B55: LDA [0x67], Y        ; B1 67
0B57: STA 0x15             ; 85 15
0B59: PLA                  ; 68

0B5A: TSX                  ; BA - PopNEXT
0B5B: TXA                  ; 8A
0B5C: TAY                  ; A8
0B5D: LDA [0x61], Y        ; B1 61
0B5F: STA 0x16             ; 85 16
0B61: LDA [0x63], Y        ; B1 63
0B63: STA 0x17             ; 85 17
0B65: LDA [0x65], Y        ; B1 65
0B67: STA 0x18             ; 85 18
0B69: LDA [0x67], Y        ; B1 67
0B6B: STA 0x19             ; 85 19

0B6D: LDX #0x1A            ; A2 1A - Different syscall (Long subtract?)
0B6F: JSR 0x0803           ; 20 03 08

0B71: TSX                  ; BA - Store result
0B72: TXA                  ; 8A
0B73: TAY                  ; A8
0B74: LDA 0x16             ; A5 16
0B76: STA [0x61], Y        ; 91 61
0B78: LDA 0x17             ; A5 17
0B7A: STA [0x63], Y        ; 91 63
0B7C: LDA 0x18             ; A5 18
0B7E: STA [0x65], Y        ; 91 65
0B80: LDA 0x19             ; A5 19
0B82: STA [0x67], Y        ; 91 67
0B84: PHA                  ; 48
0B85: PLA                  ; 68

0B86: TSX                  ; BA - Load result
0B87: TXA                  ; 8A
0B88: TAY                  ; A8
0B89: LDA [0x61], Y        ; B1 61
0B8B: STA 0x16             ; 85 16
0B8D: LDA [0x63], Y        ; B1 63
0B8F: STA 0x17             ; 85 17
0B91: LDA [0x65], Y        ; B1 65
0B93: STA 0x18             ; 85 18
0B95: LDA [0x67], Y        ; B1 67
0B97: STA 0x19             ; 85 19

0B99: LDA 0x60             ; A5 60 - PutNEXT to [BP+0]
0B9B: CLC                  ; 18
0B9C: ADC #0x00            ; 69 00
0B9E: TAY                  ; A8
0B9F: LDA 0x16             ; A5 16
0BA1: STA [0x61], Y        ; 91 61
0BA3: LDA 0x17             ; A5 17
0BA5: STA [0x63], Y        ; 91 63
0BA7: LDA 0x18             ; A5 18
0BA9: STA [0x65], Y        ; 91 65
0BAB: LDA 0x19             ; A5 19
0BAD: STA [0x67], Y        ; 91 67

0BAF: TSX                  ; BA - Stack management
0BB0: TXA                  ; 8A
0BB1: TAY                  ; A8
0BB2: LDA 0x16             ; A5 16
0BB4: STA [0x61], Y        ; 91 61
0BB6: LDA 0x17             ; A5 17
0BB8: STA [0x63], Y        ; 91 63
0BBA: LDA 0x18             ; A5 18
0BBC: STA [0x65], Y        ; 91 65
0BBE: LDA 0x19             ; A5 19
0BC0: STA [0x67], Y        ; 91 67
0BC2: PHA                  ; 48
0BC3: PLA                  ; 68

0BC4: LDA 0x60             ; A5 60 - GetNEXT from [BP-4]
0BC6: CLC                  ; 18
0BC7: ADC #0xFC            ; 69 FC
0BC9: TAY                  ; A8
0BCA: LDA [0x61], Y        ; B1 61
0BCC: STA 0x16             ; 85 16
0BCE: LDA [0x63], Y        ; B1 63
0BD0: STA 0x17             ; 85 17
0BD2: LDA [0x65], Y        ; B1 65
0BD4: STA 0x18             ; 85 18
0BD6: LDA [0x67], Y        ; B1 67
0BD8: STA 0x19             ; 85 19

0BDA: TSX                  ; BA - PushNEXT
0BDB: TXA                  ; 8A
0BDC: TAY                  ; A8
0BDD: LDA 0x16             ; A5 16
0BDF: STA [0x61], Y        ; 91 61
0BE1: LDA 0x17             ; A5 17
0BE3: STA [0x63], Y        ; 91 63
0BE5: LDA 0x18             ; A5 18
0BE7: STA [0x65], Y        ; 91 65
0BE9: LDA 0x19             ; A5 19
0BEB: STA [0x67], Y        ; 91 67
0BED: PHA                  ; 48

0BEE: CLC                  ; 18 - Manual 32-bit increment
0BEF: LDA 0x16             ; A5 16
0BF1: ADC #0x01            ; 69 01
0BF3: STA 0x16             ; 85 16
0BF5: LDA 0x17             ; A5 17
0BF7: ADC #0x00            ; 69 00
0BF9: STA 0x17             ; 85 17
0BFB: LDA 0x18             ; A5 18
0BFD: ADC #0x00            ; 69 00
0BFF: STA 0x18             ; 85 18
0C01: LDA 0x19             ; A5 19
0C03: ADC #0x00            ; 69 00
0C05: STA 0x19             ; 85 19

0C07: LDA 0x60             ; A5 60 - PutNEXT to [BP-4]
0C09: CLC                  ; 18
0C0A: ADC #0xFC            ; 69 FC
0C0C: TAY                  ; A8
0C0D: LDA 0x16             ; A5 16
0C0F: STA [0x61], Y        ; 91 61
0C11: LDA 0x17             ; A5 17
0C13: STA [0x63], Y        ; 91 63
0C15: LDA 0x18             ; A5 18
0C17: STA [0x65], Y        ; 91 65
0C19: LDA 0x19             ; A5 19
0C1B: STA [0x67], Y        ; 91 67
0C1D: PLA                  ; 68

0C1E: JMP 0x0A4E           ; 4C 4E 0A - Jump back to inner loop

0C21: PHA                  ; 48
0C22: LDA #0x2E            ; A9 2E - ASCII '.'
0C24: STA 0x16             ; 85 16
0C26: STZ 0x17             ; 64 17
0C28: STZ 0x18             ; 64 18
0C2A: STZ 0x19             ; 64 19

0C2C: TSX                  ; BA - PushNEXT
0C2D: TXA                  ; 8A
0C2E: TAY                  ; A8
0C2F: LDA 0x16             ; A5 16
0C31: STA [0x61], Y        ; 91 61
0C33: LDA 0x17             ; A5 17
0C35: STA [0x63], Y        ; 91 63
0C37: LDA 0x18             ; A5 18
0C39: STA [0x65], Y        ; 91 65
0C3B: LDA 0x19             ; A5 19
0C3D: STA [0x67], Y        ; 91 67
0C3F: PHA                  ; 48
0C40: PLA                  ; 68

0C41: TSX                  ; BA - PopNEXT
0C42: TXA                  ; 8A
0C43: TAY                  ; A8
0C44: LDA [0x61], Y        ; B1 61
0C46: STA 0x16             ; 85 16
0C48: LDA [0x63], Y        ; B1 63
0C4A: STA 0x17             ; 85 17
0C4C: LDA [0x65], Y        ; B1 65
0C4E: STA 0x18             ; 85 18
0C50: LDA [0x67], Y        ; B1 67
0C52: STA 0x19             ; 85 19

0C54: LDA 0x16             ; A5 16 - Load character
0C56: LDX #0x12            ; A2 12 - SysCall.SerialWriteChar
0C58: JSR 0x0803           ; 20 03 08

0C5A: TSX                  ; BA - PushNEXT
0C5B: TXA                  ; 8A
0C5C: TAY                  ; A8
0C5D: LDA 0x16             ; A5 16
0C5F: STA [0x61], Y        ; 91 61
0C61: LDA 0x17             ; A5 17
0C63: STA [0x63], Y        ; 91 63
0C65: LDA 0x18             ; A5 18
0C67: STA [0x65], Y        ; 91 65
0C69: LDA 0x19             ; A5 19
0C6B: STA [0x67], Y        ; 91 67
0C6D: PHA                  ; 48
0C6E: PLA                  ; 68

0C6F: LDA 0x60             ; A5 60 - GetNEXT from [BP-3]
0C71: CLC                  ; 18
0C72: ADC #0xFD            ; 69 FD
0C74: TAY                  ; A8
0C75: LDA [0x61], Y        ; B1 61
0C77: STA 0x16             ; 85 16
0C79: LDA [0x63], Y        ; B1 63
0C7B: STA 0x17             ; 85 17
0C7D: LDA [0x65], Y        ; B1 65
0C7F: STA 0x18             ; 85 18
0C81: LDA [0x67], Y        ; B1 67
0C83: STA 0x19             ; 85 19

0C85: TSX                  ; BA - PushNEXT
0C86: TXA                  ; 8A
0C87: TAY                  ; A8
0C88: LDA 0x16             ; A5 16
0C8A: STA [0x61], Y        ; 91 61
0C8C: LDA 0x17             ; A5 17
0C8E: STA [0x63], Y        ; 91 63
0C90: LDA 0x18             ; A5 18
0C92: STA [0x65], Y        ; 91 65
0C94: LDA 0x19             ; A5 19
0C96: STA [0x67], Y        ; 91 67
0C98: PHA                  ; 48

0C99: CLC                  ; 18 - Manual increment (i++)
0C9A: LDA 0x16             ; A5 16
0C9C: ADC #0x01            ; 69 01
0C9E: STA 0x16             ; 85 16
0CA0: LDA 0x17             ; A5 17
0CA2: ADC #0x00            ; 69 00
0CA4: STA 0x17             ; 85 17
0CA6: LDA 0x18             ; A5 18
0CA8: ADC #0x00            ; 69 00
0CAA: STA 0x18             ; 85 18
0CAC: LDA 0x19             ; A5 19
0CAE: ADC #0x00            ; 69 00
0CB0: STA 0x19             ; 85 19

0CB2: LDA 0x60             ; A5 60 - PutNEXT to [BP-3]
0CB4: CLC                  ; 18
0CB5: ADC #0xFD            ; 69 FD
0CB7: TAY                  ; A8
0CB8: LDA 0x16             ; A5 16
0CBA: STA [0x61], Y        ; 91 61
0CBC: LDA 0x17             ; A5 17
0CBE: STA [0x63], Y        ; 91 63
0CC0: LDA 0x18             ; A5 18
0CC2: STA [0x65], Y        ; 91 65
0CC4: LDA 0x19             ; A5 19
0CC6: STA [0x67], Y        ; 91 67
0CC8: PLA                  ; 68

0CC9: JMP 0x08EE           ; 4C EE 08 - Jump back to outer loop

0CCC: PHA                  ; 48
0CCD: LDA #0x07            ; A9 07 - String pointer setup
0CCF: STA 0x1E             ; 85 1E
0CD1: LDA #0x08            ; A9 08
0CD3: STA 0x1F             ; 85 1F

0CD5: LDY #0x00            ; A0 00 - Print "%ld\n" string
0CD7: LDA [0x1E], Y        ; B1 1E
0CD9: CMP #0x00            ; C0 00
0CDB: BEQ 0x0CE3           ; F0 08
0CDD: LDX #0x12            ; A2 12 - SysCall.SerialWriteChar
0CDF: JSR 0x0803           ; 20 03 08
0CE1: INY                  ; C8
0CE2: BRA 0x0CD7           ; 80 F2

0CE3: LDA 0x60             ; A5 60 - Get final sum for printing
0CE5: CLC                  ; 18
0CE6: ADC #0x00            ; 69 00
0CE8: TAY                  ; A8
0CE9: LDA [0x61], Y        ; B1 61
0CEB: STA 0x16             ; 85 16
0CED: LDA [0x63], Y        ; B1 63
0CEF: STA 0x17             ; 85 17
0CF1: LDA [0x65], Y        ; B1 65
0CF3: STA 0x18             ; 85 18
0CF5: LDA [0x67], Y        ; B1 67
0CF7: STA 0x19             ; 85 19

0CF9: TSX                  ; BA - PushNEXT
0CFA: TXA                  ; 8A
0CFB: TAY                  ; A8
0CFC: LDA 0x16             ; A5 16
0CFE: STA [0x61], Y        ; 91 61
0D00: LDA 0x17             ; A5 17
0D02: STA [0x63], Y        ; 91 63
0D04: LDA 0x18             ; A5 18
0D06: STA [0x65], Y        ; 91 65
0D08: LDA 0x19             ; A5 19
0D0A: STA [0x67], Y        ; 91 67
0D0C: PHA                  ; 48
0D0D: PLA                  ; 68

0D0E: TSX                  ; BA - PopTOP
0D0F: TXA                  ; 8A
0D10: TAY                  ; A8
0D11: LDA [0x61], Y        ; B1 61
0D13: STA 0x12             ; 85 12
0D15: LDA [0x63], Y        ; B1 63
0D17: STA 0x13             ; 85 13
0D19: LDA [0x65], Y        ; B1 65
0D1B: STA 0x14             ; 85 14
0D1D: LDA [0x67], Y        ; B1 67
0D1F: STA 0x15             ; 85 15

0D21: LDX #0x1F            ; A2 1F - SysCall.Printf
0D23: JSR 0x0803           ; 20 03 08

0D25: LDY #0x03            ; A0 03 - Print remaining part of string
0D27: LDA [0x1E], Y        ; B1 1E
0D29: CMP #0x04            ; C0 04
0D2B: BEQ 0x0D33           ; F0 08
0D2D: LDX #0x12            ; A2 12 - SysCall.SerialWriteChar
0D2F: JSR 0x0803           ; 20 03 08
0D31: INY                  ; C8
0D32: BRA 0x0D27           ; 80 F2

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
0D46: LDX #0x12            ; A2 12 - SysCall.SerialWriteChar
0D48: JSR 0x0803           ; 20 03 08
0D4A: INY                  ; C8
0D4B: BRA 0x0D40           ; 80 F2

0D4C: PHA                  ; 48
0D4D: LDX #0x18            ; A2 18 - SysCall.TimeMillis (get end time)
0D4F: JSR 0x0803           ; 20 03 08

0D51: TSX                  ; BA - PushTOP
0D52: TXA                  ; 8A
0D53: TAY                  ; A8
0D54: LDA 0x12             ; A5 12
0D56: STA [0x61], Y        ; 91 61
0D58: LDA 0x13             ; A5 13
0D5A: STA [0x63], Y        ; 91 63
0D5C: LDA 0x14             ; A5 14
0D5E: STA [0x65], Y        ; 91 65
0D60: LDA 0x15             ; A5 15
0D62: STA [0x67], Y        ; 91 67
0D64: PHA                  ; 48

0D65: LDA 0x60             ; A5 60 - Get start time from [BP-2]
0D67: CLC                  ; 18
0D68: ADC #0xFE            ; 69 FE
0D6A: TAY                  ; A8
0D6B: LDA [0x61], Y        ; B1 61
0D6D: STA 0x16             ; 85 16
0D6F: LDA [0x63], Y        ; B1 63
0D71: STA 0x17             ; 85 17
0D73: LDA [0x65], Y        ; B1 65
0D75: STA 0x18             ; 85 18
0D77: LDA [0x67], Y        ; B1 67
0D79: STA 0x19             ; 85 19

0D7B: TSX                  ; BA - PushNEXT
0D7C: TXA                  ; 8A
0D7D: TAY                  ; A8
0D7E: LDA 0x16             ; A5 16
0D80: STA [0x61], Y        ; 91 61
0D82: LDA 0x17             ; A5 17
0D84: STA [0x63], Y        ; 91 63
0D86: LDA 0x18             ; A5 18
0D88: STA [0x65], Y        ; 91 65
0D8A: LDA 0x19             ; A5 19
0D8C: STA [0x67], Y        ; 91 67
0D8E: PLA                  ; 68
0D8F: PLA                  ; 68

0D90: TSX                  ; BA - PopTOP
0D91: TXA                  ; 8A
0D92: TAY                  ; A8
0D93: LDA [0x61], Y        ; B1 61
0D95: STA 0x12             ; 85 12
0D97: LDA [0x63], Y        ; B1 63
0D99: STA 0x13             ; 85 13
0D9B: LDA [0x65], Y        ; B1 65
0D9D: STA 0x14             ; 85 14
0D9F: LDA [0x67], Y        ; B1 67
0DA1: STA 0x15             ; 85 15
0DA3: PLA                  ; 68

0DA4: TSX                  ; BA - PopNEXT
0DA5: TXA                  ; 8A
0DA6: TAY                  ; A8
0DA7: LDA [0x61], Y        ; B1 61
0DA9: STA 0x16             ; 85 16
0DAB: LDA [0x63], Y        ; B1 63
0DAD: STA 0x17             ; 85 17
0DAF: LDA [0x65], Y        ; B1 65
0DB1: STA 0x18             ; 85 18
0DB3: LDA [0x67], Y        ; B1 67
0DB5: STA 0x19             ; 85 19

0DB7: LDX #0x1B            ; A2 1B - SysCall.LongSubtract
0DB9: JSR 0x0803           ; 20 03 08

0DBB: TSX                  ; BA - PushNEXT (elapsed time)
0DBC: TXA                  ; 8A
0DBD: TAY                  ; A8
0DBE: LDA 0x16             ; A5 16
0DC0: STA [0x61], Y        ; 91 61
0DC2: LDA 0x17             ; A5 17
0DC4: STA [0x63], Y        ; 91 63
0DC6: LDA 0x18             ; A5 18
0DC8: STA [0x65], Y        ; 91 65
0DCA: LDA 0x19             ; A5 19
0DCC: STA [0x67], Y        ; 91 67
0DCE: PHA                  ; 48
0DCF: PLA                  ; 68

0DD0: TSX                  ; BA - PopTOP (prepare for printf)
0DD1: TXA                  ; 8A
0DD2: TAY                  ; A8
0DD3: LDA [0x61], Y        ; B1 61
0DD5: STA 0x12             ; 85 12
0DD7: LDA [0x63], Y        ; B1 63
0DD9: STA 0x13             ; 85 13
0DDB: LDA [0x65], Y        ; B1 65
0DDD: STA 0x14             ; 85 14
0DDF: LDA [0x67], Y        ; B1 67
0DE1: STA 0x15             ; 85 15

0DE3: LDX #0x1F            ; A2 1F - SysCall.Printf
0DE5: JSR 0x0803           ; 20 03 08

0DE7: LDY #0x03            ; A0 03 - Print remaining string
0DE9: LDA [0x1E], Y        ; B1 1E
0DEB: CMP #0x07            ; C0 07
0DED: BEQ 0x0DF5           ; F0 08
0DEF: LDX #0x12            ; A2 12 - SysCall.SerialWriteChar
0DF1: JSR 0x0803           ; 20 03 08
0DF3: INY                  ; C8
0DF4: BRA 0x0DE9           ; 80 F2

0DF6: PLA                  ; 68

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
- **0x00**: Memory allocation
- **0x12**: Serial character output (putchar)  
- **0x18**: Get millisecond timer (millis)
- **0x1A**: Long comparison/subtraction operation (at 0xB6D)
- **0x1B**: Long subtraction 
- **0x1F**: Printf formatting
- **0x24**: Long addition

### Key Observations:

1. **Comparison Implementation**: The code uses syscall **0x1A** at address 0xB6D, which appears to be a comparison operation distinct from addition (0x24).

2. **Manual 32-bit Arithmetic**: At 0xBEE and 0xC99, the compiler generates inline 32-bit increment operations using CLC/ADC sequences instead of calling syscalls.

3. **Proper Loop Structure**: The loops use subtraction (0x1A) for comparison, then test the result with OR operations to check if all bytes are zero.

4. **Runtime Stack System**: Sophisticated 4-page parallel stack implementation using indexed indirect addressing [0x61], Y through [0x67], Y.

5. **String Handling**: Character-by-character output for printf strings, with proper null termination checks.

The code appears to be a correctly functioning implementation with proper comparison operations via syscall 0x1A.