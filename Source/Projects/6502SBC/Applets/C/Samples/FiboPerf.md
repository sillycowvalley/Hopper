# Complete Disassembly of Fibonacci Program

## Memory Map
```
0800-0806: Entry point and BIOS dispatch
0807-0821: String literals
0822-0ABF: fibo() function
0AC0-0E02: main() function
```

## Full Listing with Annotations

```assembly
; Entry point and BIOS dispatch
0800: 4C C3 0A    JMP $0AC3         ; Jump to main
0803: 6C 22 00    JMP ($0022)       ; BIOS dispatch vector
0806: 60          RTS               ; Return from BIOS call

; String literals
0807: 46 69 62 6F 28 25 64 29 3D 25 64 0A 00   ; "Fibo(%d)=%d\n\0"
0814: 45 6C 61 70 73 65 64 3A 20 25 6C 64 20 6D 73 0A 00  ; "Elapsed: %ld ms\n\0"

; ==================== fibo() function ====================
; int fibo(int n) {
;     if (n <= 1) return n;
;     return fibo(n-1) + fibo(n-2);
; }

; Function prologue
0822: A5 60       LDA $60           ; Load current BP
0824: 48          PHA               ; Save old BP
0825: BA          TSX               ; Get stack pointer
0826: 86 60       STX $60           ; New BP = SP

; Load parameter n from [BP+4]
0828: A5 60       LDA $60           ; Load BP
082A: 18          CLC               ; Clear carry for addition
082B: 69 04       ADC #$04          ; BP + 4 (parameter location)
082D: A8          TAY               ; Transfer to Y for indirect addressing

; Load n to NEXT (4 bytes)
082E: B1 61       LDA ($61),Y       ; Load byte 0 from stack page 0
0830: 85 16       STA $16           ; Store in NEXT0
0832: B1 63       LDA ($63),Y       ; Load byte 1 from stack page 1
0834: 85 17       STA $17           ; Store in NEXT1
0836: B1 65       LDA ($65),Y       ; Load byte 2 from stack page 2
0838: 85 18       STA $18           ; Store in NEXT2
083A: B1 67       LDA ($67),Y       ; Load byte 3 from stack page 3
083C: 85 19       STA $19           ; Store in NEXT3

; Push n onto stack
083E: BA          TSX               ; Get stack pointer
083F: 8A          TXA               ; Transfer to A
0840: A8          TAY               ; Transfer to Y for indexing
0841: A5 16       LDA $16           ; Load NEXT0
0843: 91 61       STA ($61),Y       ; Store to stack page 0
0845: A5 17       LDA $17           ; Load NEXT1
0847: 91 63       STA ($63),Y       ; Store to stack page 1
0849: A5 18       LDA $18           ; Load NEXT2
084B: 91 65       STA ($65),Y       ; Store to stack page 2
084D: A5 19       LDA $19           ; Load NEXT3
084F: 91 67       STA ($67),Y       ; Store to stack page 3
0851: 48          PHA               ; ⚠️ OPTIMIZATION: Unnecessary push

; Push literal 1
0852: A9 01       LDA #$01          ; Load immediate 1
0854: 85 16       STA $16           ; Store in NEXT0
0856: 64 17       STZ $17           ; Clear NEXT1
0858: 64 18       STZ $18           ; Clear NEXT2
085A: 64 19       STZ $19           ; Clear NEXT3

; Push 1 onto stack
085C: BA          TSX               ; Get stack pointer
085D: 8A          TXA               ; Transfer to A
085E: A8          TAY               ; Transfer to Y
085F: A5 16       LDA $16           ; Load NEXT0
0861: 91 61       STA ($61),Y       ; Store to stack page 0
0863: A5 17       LDA $17           ; Load NEXT1
0865: 91 63       STA ($63),Y       ; Store to stack page 1
0867: A5 18       LDA $18           ; Load NEXT2
0869: 91 65       STA ($65),Y       ; Store to stack page 2
086B: A5 19       LDA $19           ; Load NEXT3
086D: 91 67       STA ($67),Y       ; Store to stack page 3
086F: 48          PHA               ; Push to hardware stack

; Pop first value to TOP
0870: 68          PLA               ; Pull from hardware stack
0871: BA          TSX               ; Get stack pointer
0872: 8A          TXA               ; Transfer to A
0873: A8          TAY               ; Transfer to Y
0874: B1 61       LDA ($61),Y       ; Load from stack page 0
0876: 85 12       STA $12           ; Store in TOP0
0878: B1 63       LDA ($63),Y       ; Load from stack page 1
087A: 85 13       STA $13           ; Store in TOP1
087C: B1 65       LDA ($65),Y       ; Load from stack page 2
087E: 85 14       STA $14           ; Store in TOP2
0880: B1 67       LDA ($67),Y       ; Load from stack page 3
0882: 85 15       STA $15           ; Store in TOP3

; Pop second value to NEXT
0884: 68          PLA               ; Pull from hardware stack
0885: BA          TSX               ; Get stack pointer
0886: 8A          TXA               ; Transfer to A
0887: A8          TAY               ; Transfer to Y
0888: B1 61       LDA ($61),Y       ; Load from stack page 0
088A: 85 16       STA $16           ; Store in NEXT0
088C: B1 63       LDA ($63),Y       ; Load from stack page 1
088E: 85 17       STA $17           ; Store in NEXT1
0890: B1 65       LDA ($65),Y       ; Load from stack page 2
0892: 85 18       STA $18           ; Store in NEXT2
0894: B1 67       LDA ($67),Y       ; Load from stack page 3
0896: 85 19       STA $19           ; Store in NEXT3

; Call Long.LE (n <= 1)
0898: A2 24       LDX #$24          ; SysCall.LongLE
089A: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push comparison result (C flag as 0/1)
089D: BA          TSX               ; Get stack pointer
089E: 8A          TXA               ; Transfer to A
089F: A8          TAY               ; Transfer to Y
08A0: A9 00       LDA #$00          ; Default to 0
08A2: 69 00       ADC #$00          ; Add carry (0 or 1)
08A4: 91 61       STA ($61),Y       ; Store to stack page 0
08A6: A9 00       LDA #$00          ; Zero for upper bytes
08A8: 91 63       STA ($63),Y       ; Store to stack page 1
08AA: 91 65       STA ($65),Y       ; Store to stack page 2
08AC: 91 67       STA ($67),Y       ; Store to stack page 3
08AE: 48          PHA               ; Push to hardware stack

; Pop condition result to NEXT
08AF: 68          PLA               ; Pull from hardware stack
08B0: BA          TSX               ; Get stack pointer
08B1: 8A          TXA               ; Transfer to A
08B2: A8          TAY               ; Transfer to Y
08B3: B1 61       LDA ($61),Y       ; Load from stack page 0
08B5: 85 16       STA $16           ; Store in NEXT0
08B7: B1 63       LDA ($63),Y       ; Load from stack page 1
08B9: 85 17       STA $17           ; Store in NEXT1
08BB: B1 65       LDA ($65),Y       ; Load from stack page 2
08BD: 85 18       STA $18           ; Store in NEXT2
08BF: B1 67       LDA ($67),Y       ; Load from stack page 3
08C1: 85 19       STA $19           ; Store in NEXT3

; Test if condition is true (n <= 1)
08C3: A5 16       LDA $16           ; Load NEXT0
08C5: 05 17       ORA $17           ; OR with NEXT1
08C7: 05 18       ORA $18           ; OR with NEXT2
08C9: 05 19       ORA $19           ; OR with NEXT3
08CB: D0 03       BNE $08D0         ; Branch if true (n <= 1)
08CD: 4C 2E 09    JMP $092E         ; Jump to recursive case

; === Then block: return n ===
08D0: A5 60       LDA $60           ; Load BP
08D2: 18          CLC               ; Clear carry
08D3: 69 04       ADC #$04          ; BP + 4 (parameter n)
08D5: A8          TAY               ; Transfer to Y

; Load n to NEXT
08D6: B1 61       LDA ($61),Y       ; Load byte 0
08D8: 85 16       STA $16           ; Store in NEXT0
08DA: B1 63       LDA ($63),Y       ; Load byte 1
08DC: 85 17       STA $17           ; Store in NEXT1
08DE: B1 65       LDA ($65),Y       ; Load byte 2
08E0: 85 18       STA $18           ; Store in NEXT2
08E2: B1 67       LDA ($67),Y       ; Load byte 3
08E4: 85 19       STA $19           ; Store in NEXT3

; Push n onto stack
08E6: BA          TSX               ; Get stack pointer
08E7: 8A          TXA               ; Transfer to A
08E8: A8          TAY               ; Transfer to Y
08E9: A5 16       LDA $16           ; Load NEXT0
08EB: 91 61       STA ($61),Y       ; Store to stack page 0
08ED: A5 17       LDA $17           ; Load NEXT1
08EF: 91 63       STA ($63),Y       ; Store to stack page 1
08F1: A5 18       LDA $18           ; Load NEXT2
08F3: 91 65       STA ($65),Y       ; Store to stack page 2
08F5: A5 19       LDA $19           ; Load NEXT3
08F7: 91 67       STA ($67),Y       ; Store to stack page 3
08F9: 48          PHA               ; Push to hardware stack

; Pop return value to NEXT
08FA: 68          PLA               ; Pull from hardware stack
08FB: BA          TSX               ; Get stack pointer
08FC: 8A          TXA               ; Transfer to A
08FD: A8          TAY               ; Transfer to Y
08FE: B1 61       LDA ($61),Y       ; Load from stack page 0
0900: 85 16       STA $16           ; Store in NEXT0
0902: B1 63       LDA ($63),Y       ; Load from stack page 1
0904: 85 17       STA $17           ; Store in NEXT1
0906: B1 65       LDA ($65),Y       ; Load from stack page 2
0908: 85 18       STA $18           ; Store in NEXT2
090A: B1 67       LDA ($67),Y       ; Load from stack page 3
090C: 85 19       STA $19           ; Store in NEXT3

; Store return value at [BP+5]
090E: A5 60       LDA $60           ; Load BP
0910: 18          CLC               ; Clear carry
0911: 69 05       ADC #$05          ; BP + 5 (return slot)
0913: A8          TAY               ; Transfer to Y
0914: A5 16       LDA $16           ; Load NEXT0
0916: 91 61       STA ($61),Y       ; Store to return slot page 0
0918: A5 17       LDA $17           ; Load NEXT1
091A: 91 63       STA ($63),Y       ; Store to return slot page 1
091C: A5 18       LDA $18           ; Load NEXT2
091E: 91 65       STA ($65),Y       ; Store to return slot page 2
0920: A5 19       LDA $19           ; Load NEXT3
0922: 91 67       STA ($67),Y       ; Store to return slot page 3

; Function epilogue
0924: A6 60       LDX $60           ; Load BP
0926: 9A          TXS               ; Restore SP
0927: 68          PLA               ; Pull saved BP
0928: 85 60       STA $60           ; Restore BP
092A: 60          RTS               ; Return

; === Recursive case: return fibo(n-1) + fibo(n-2) ===
092B: 48          PHA               ; ⚠️ OPTIMIZATION: Unnecessary push

; Load n from [BP+4]
092C: A5 60       LDA $60           ; Load BP
092E: 18          CLC               ; Clear carry
092F: 69 04       ADC #$04          ; BP + 4
0931: A8          TAY               ; Transfer to Y
0932: B1 61       LDA ($61),Y       ; Load byte 0
0934: 85 16       STA $16           ; Store in NEXT0
0936: B1 63       LDA ($63),Y       ; Load byte 1
0938: 85 17       STA $17           ; Store in NEXT1
093A: B1 65       LDA ($65),Y       ; Load byte 2
093C: 85 18       STA $18           ; Store in NEXT2
093E: B1 67       LDA ($67),Y       ; Load byte 3
0940: 85 19       STA $19           ; Store in NEXT3

; Push n onto stack
0942: BA          TSX               ; Get stack pointer
0943: 8A          TXA               ; Transfer to A
0944: A8          TAY               ; Transfer to Y
0945: A5 16       LDA $16           ; Load NEXT0
0947: 91 61       STA ($61),Y       ; Store to stack page 0
0949: A5 17       LDA $17           ; Load NEXT1
094B: 91 63       STA ($63),Y       ; Store to stack page 1
094D: A5 18       LDA $18           ; Load NEXT2
094F: 91 65       STA ($65),Y       ; Store to stack page 2
0951: A5 19       LDA $19           ; Load NEXT3
0953: 91 67       STA ($67),Y       ; Store to stack page 3
0955: 48          PHA               ; Push to hardware stack

; Push literal 1
0956: A9 01       LDA #$01          ; Load immediate 1
0958: 85 16       STA $16           ; Store in NEXT0
095A: 64 17       STZ $17           ; Clear NEXT1
095C: 64 18       STZ $18           ; Clear NEXT2
095E: 64 19       STZ $19           ; Clear NEXT3

; Push 1 onto stack
0960: BA          TSX               ; Get stack pointer
0961: 8A          TXA               ; Transfer to A
0962: A8          TAY               ; Transfer to Y
0963: A5 16       LDA $16           ; Load NEXT0
0965: 91 61       STA ($61),Y       ; Store to stack page 0
0967: A5 17       LDA $17           ; Load NEXT1
0969: 91 63       STA ($63),Y       ; Store to stack page 1
096B: A5 18       LDA $18           ; Load NEXT2
096D: 91 65       STA ($65),Y       ; Store to stack page 2
096F: A5 19       LDA $19           ; Load NEXT3
0971: 91 67       STA ($67),Y       ; Store to stack page 3
0973: 48          PHA               ; Push to hardware stack

; Pop values for subtraction
0974: 68          PLA               ; Pull from hardware stack
0975: BA          TSX               ; Get stack pointer
0976: 8A          TXA               ; Transfer to A
0977: A8          TAY               ; Transfer to Y
0978: B1 61       LDA ($61),Y       ; Load from stack page 0
097A: 85 12       STA $12           ; Store in TOP0
097C: B1 63       LDA ($63),Y       ; Load from stack page 1
097E: 85 13       STA $13           ; Store in TOP1
0980: B1 65       LDA ($65),Y       ; Load from stack page 2
0982: 85 14       STA $14           ; Store in TOP2
0984: B1 67       LDA ($67),Y       ; Load from stack page 3
0986: 85 15       STA $15           ; Store in TOP3

0988: 68          PLA               ; Pull from hardware stack
0989: BA          TSX               ; Get stack pointer
098A: 8A          TXA               ; Transfer to A
098B: A8          TAY               ; Transfer to Y
098C: B1 61       LDA ($61),Y       ; Load from stack page 0
098E: 85 16       STA $16           ; Store in NEXT0
0990: B1 63       LDA ($63),Y       ; Load from stack page 1
0992: 85 17       STA $17           ; Store in NEXT1
0994: B1 65       LDA ($65),Y       ; Load from stack page 2
0996: 85 18       STA $18           ; Store in NEXT2
0998: B1 67       LDA ($67),Y       ; Load from stack page 3
099A: 85 19       STA $19           ; Store in NEXT3

; Call Long.Sub (n - 1)
099C: A2 1B       LDX #$1B          ; SysCall.LongSub
099E: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push result (n-1)
09A1: BA          TSX               ; Get stack pointer
09A2: 8A          TXA               ; Transfer to A
09A3: A8          TAY               ; Transfer to Y
09A4: A5 16       LDA $16           ; Load NEXT0 (result)
09A6: 91 61       STA ($61),Y       ; Store to stack page 0
09A8: A5 17       LDA $17           ; Load NEXT1
09AA: 91 63       STA ($63),Y       ; Store to stack page 1
09AC: A5 18       LDA $18           ; Load NEXT2
09AE: 91 65       STA ($65),Y       ; Store to stack page 2
09B0: A5 19       LDA $19           ; Load NEXT3
09B2: 91 67       STA ($67),Y       ; Store to stack page 3
09B4: 48          PHA               ; Push to hardware stack

; Recursive call fibo(n-1)
09B5: 20 25 08    JSR $0825         ; Call fibo()
09B8: 68          PLA               ; ⚠️ OPTIMIZATION: Unnecessary
09B9: 48          PHA               ; ⚠️ OPTIMIZATION: Unnecessary

; Load n again for second recursive call
09BA: A5 60       LDA $60           ; Load BP
09BC: 18          CLC               ; Clear carry
09BD: 69 04       ADC #$04          ; BP + 4
09BF: A8          TAY               ; Transfer to Y
09C0: B1 61       LDA ($61),Y       ; Load byte 0
09C2: 85 16       STA $16           ; Store in NEXT0
09C4: B1 63       LDA ($63),Y       ; Load byte 1
09C6: 85 17       STA $17           ; Store in NEXT1
09C8: B1 65       LDA ($65),Y       ; Load byte 2
09CA: 85 18       STA $18           ; Store in NEXT2
09CC: B1 67       LDA ($67),Y       ; Load byte 3
09CE: 85 19       STA $19           ; Store in NEXT3

; Push n onto stack
09D0: BA          TSX               ; Get stack pointer
09D1: 8A          TXA               ; Transfer to A
09D2: A8          TAY               ; Transfer to Y
09D3: A5 16       LDA $16           ; Load NEXT0
09D5: 91 61       STA ($61),Y       ; Store to stack page 0
09D7: A5 17       LDA $17           ; Load NEXT1
09D9: 91 63       STA ($63),Y       ; Store to stack page 1
09DB: A5 18       LDA $18           ; Load NEXT2
09DD: 91 65       STA ($65),Y       ; Store to stack page 2
09DF: A5 19       LDA $19           ; Load NEXT3
09E1: 91 67       STA ($67),Y       ; Store to stack page 3
09E3: 48          PHA               ; Push to hardware stack

; Push literal 2
09E4: A9 02       LDA #$02          ; Load immediate 2
09E6: 85 16       STA $16           ; Store in NEXT0
09E8: 64 17       STZ $17           ; Clear NEXT1
09EA: 64 18       STZ $18           ; Clear NEXT2
09EC: 64 19       STZ $19           ; Clear NEXT3

; Push 2 onto stack
09EE: BA          TSX               ; Get stack pointer
09EF: 8A          TXA               ; Transfer to A
09F0: A8          TAY               ; Transfer to Y
09F1: A5 16       LDA $16           ; Load NEXT0
09F3: 91 61       STA ($61),Y       ; Store to stack page 0
09F5: A5 17       LDA $17           ; Load NEXT1
09F7: 91 63       STA ($63),Y       ; Store to stack page 1
09F9: A5 18       LDA $18           ; Load NEXT2
09FB: 91 65       STA ($65),Y       ; Store to stack page 2
09FD: A5 19       LDA $19           ; Load NEXT3
09FF: 91 67       STA ($67),Y       ; Store to stack page 3
0A01: 48          PHA               ; Push to hardware stack

; Pop values for subtraction
0A02: 68          PLA               ; Pull from hardware stack
0A03: BA          TSX               ; Get stack pointer
0A04: 8A          TXA               ; Transfer to A
0A05: A8          TAY               ; Transfer to Y
0A06: B1 61       LDA ($61),Y       ; Load from stack page 0
0A08: 85 12       STA $12           ; Store in TOP0
0A0A: B1 63       LDA ($63),Y       ; Load from stack page 1
0A0C: 85 13       STA $13           ; Store in TOP1
0A0E: B1 65       LDA ($65),Y       ; Load from stack page 2
0A10: 85 14       STA $14           ; Store in TOP2
0A12: B1 67       LDA ($67),Y       ; Load from stack page 3
0A14: 85 15       STA $15           ; Store in TOP3

0A16: 68          PLA               ; Pull from hardware stack
0A17: BA          TSX               ; Get stack pointer
0A18: 8A          TXA               ; Transfer to A
0A19: A8          TAY               ; Transfer to Y
0A1A: B1 61       LDA ($61),Y       ; Load from stack page 0
0A1C: 85 16       STA $16           ; Store in NEXT0
0A1E: B1 63       LDA ($63),Y       ; Load from stack page 1
0A20: 85 17       STA $17           ; Store in NEXT1
0A22: B1 65       LDA ($65),Y       ; Load from stack page 2
0A24: 85 18       STA $18           ; Store in NEXT2
0A26: B1 67       LDA ($67),Y       ; Load from stack page 3
0A28: 85 19       STA $19           ; Store in NEXT3

; Call Long.Sub (n - 2)
0A2A: A2 1B       LDX #$1B          ; SysCall.LongSub
0A2C: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push result (n-2)
0A2F: BA          TSX               ; Get stack pointer
0A30: 8A          TXA               ; Transfer to A
0A31: A8          TAY               ; Transfer to Y
0A32: A5 16       LDA $16           ; Load NEXT0 (result)
0A34: 91 61       STA ($61),Y       ; Store to stack page 0
0A36: A5 17       LDA $17           ; Load NEXT1
0A38: 91 63       STA ($63),Y       ; Store to stack page 1
0A3A: A5 18       LDA $18           ; Load NEXT2
0A3C: 91 65       STA ($65),Y       ; Store to stack page 2
0A3E: A5 19       LDA $19           ; Load NEXT3
0A40: 91 67       STA ($67),Y       ; Store to stack page 3
0A42: 48          PHA               ; Push to hardware stack

; Recursive call fibo(n-2)
0A43: 20 25 08    JSR $0825         ; Call fibo()
0A46: 68          PLA               ; ⚠️ OPTIMIZATION: Unnecessary

; Pop both recursive results
0A47: 68          PLA               ; Pull from hardware stack
0A48: BA          TSX               ; Get stack pointer
0A49: 8A          TXA               ; Transfer to A
0A4A: A8          TAY               ; Transfer to Y
0A4B: B1 61       LDA ($61),Y       ; Load fibo(n-2) result byte 0
0A4D: 85 12       STA $12           ; Store in TOP0
0A4F: B1 63       LDA ($63),Y       ; Load byte 1
0A51: 85 13       STA $13           ; Store in TOP1
0A53: B1 65       LDA ($65),Y       ; Load byte 2
0A55: 85 14       STA $14           ; Store in TOP2
0A57: B1 67       LDA ($67),Y       ; Load byte 3
0A59: 85 15       STA $15           ; Store in TOP3

0A5B: 68          PLA               ; Pull from hardware stack
0A5C: BA          TSX               ; Get stack pointer
0A5D: 8A          TXA               ; Transfer to A
0A5E: A8          TAY               ; Transfer to Y
0A5F: B1 61       LDA ($61),Y       ; Load fibo(n-1) result byte 0
0A61: 85 16       STA $16           ; Store in NEXT0
0A63: B1 63       LDA ($63),Y       ; Load byte 1
0A65: 85 17       STA $17           ; Store in NEXT1
0A67: B1 65       LDA ($65),Y       ; Load byte 2
0A69: 85 18       STA $18           ; Store in NEXT2
0A6B: B1 67       LDA ($67),Y       ; Load byte 3
0A6D: 85 19       STA $19           ; Store in NEXT3

; Call Long.Add
0A6F: A2 1A       LDX #$1A          ; SysCall.LongAdd
0A71: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push sum
0A74: BA          TSX               ; Get stack pointer
0A75: 8A          TXA               ; Transfer to A
0A76: A8          TAY               ; Transfer to Y
0A77: A5 16       LDA $16           ; Load NEXT0 (sum)
0A79: 91 61       STA ($61),Y       ; Store to stack page 0
0A7B: A5 17       LDA $17           ; Load NEXT1
0A7D: 91 63       STA ($63),Y       ; Store to stack page 1
0A7F: A5 18       LDA $18           ; Load NEXT2
0A81: 91 65       STA ($65),Y       ; Store to stack page 2
0A83: A5 19       LDA $19           ; Load NEXT3
0A85: 91 67       STA ($67),Y       ; Store to stack page 3
0A87: 48          PHA               ; Push to hardware stack

; Pop sum to NEXT
0A88: 68          PLA               ; Pull from hardware stack
0A89: BA          TSX               ; Get stack pointer
0A8A: 8A          TXA               ; Transfer to A
0A8B: A8          TAY               ; Transfer to Y
0A8C: B1 61       LDA ($61),Y       ; Load from stack page 0
0A8E: 85 16       STA $16           ; Store in NEXT0
0A90: B1 63       LDA ($63),Y       ; Load from stack page 1
0A92: 85 17       STA $17           ; Store in NEXT1
0A94: B1 65       LDA ($65),Y       ; Load from stack page 2
0A96: 85 18       STA $18           ; Store in NEXT2
0A98: B1 67       LDA ($67),Y       ; Load from stack page 3
0A9A: 85 19       STA $19           ; Store in NEXT3

; Store return value at [BP+5]
0A9C: A5 60       LDA $60           ; Load BP
0A9E: 18          CLC               ; Clear carry
0A9F: 69 05       ADC #$05          ; BP + 5 (return slot)
0AA1: A8          TAY               ; Transfer to Y
0AA2: A5 16       LDA $16           ; Load NEXT0
0AA4: 91 61       STA ($61),Y       ; Store to return slot page 0
0AA6: A5 17       LDA $17           ; Load NEXT1
0AA8: 91 63       STA ($63),Y       ; Store to return slot page 1
0AAA: A5 18       LDA $18           ; Load NEXT2
0AAC: 91 65       STA ($65),Y       ; Store to return slot page 2
0AAE: A5 19       LDA $19           ; Load NEXT3
0AB0: 91 67       STA ($67),Y       ; Store to return slot page 3

; Function epilogue
0AB2: A6 60       LDX $60           ; Load BP
0AB4: 9A          TXS               ; Restore SP
0AB5: 68          PLA               ; Pull saved BP
0AB6: 85 60       STA $60           ; Restore BP
0AB8: 60          RTS               ; Return

; ⚠️ DUPLICATE EPILOGUE (unreachable code)
0AB9: A6 60       LDX $60           ; Load BP
0ABB: 9A          TXS               ; Restore SP
0ABC: 68          PLA               ; Pull saved BP
0ABD: 85 60       STA $60           ; Restore BP
0ABF: 60          RTS               ; Return

; ==================== main() function ====================
; Stack initialization
0AC0: A2 00       LDX #$00          ; SysCall.MemAllocate
0AC2: 20 03 08    JSR $0803         ; Call BIOS dispatch
0AC5: 64 61       STZ $61           ; Clear stack pointer LSB page 0
0AC7: 64 63       STZ $63           ; Clear stack pointer LSB page 1
0AC9: 64 65       STZ $65           ; Clear stack pointer LSB page 2
0ACB: 64 67       STZ $67           ; Clear stack pointer LSB page 3
0ACD: A5 1B       LDA $1B           ; Load IDXH (allocated page)
0ACF: 85 62       STA $62           ; Set stack pointer MSB page 0
0AD1: 1A          INC A             ; Increment for next page
0AD2: 85 64       STA $64           ; Set stack pointer MSB page 1
0AD4: 1A          INC A             ; Increment for next page
0AD5: 85 66       STA $66           ; Set stack pointer MSB page 2
0AD7: 1A          INC A             ; Increment for next page
0AD8: 85 68       STA $68           ; Set stack pointer MSB page 3

; Function prologue
0ADA: A5 60       LDA $60           ; Load current BP
0ADC: 48          PHA               ; Save old BP
0ADD: BA          TSX               ; Get stack pointer
0ADE: 86 60       STX $60           ; New BP = SP

; Reserve space for locals
0AE0: 48          PHA               ; ⚠️ OPTIMIZATION: Unnecessary
0AE1: 48          PHA               ; ⚠️ OPTIMIZATION: Unnecessary

; long start = millis()
0AE2: A2 18       LDX #$18          ; SysCall.TimeMillis
0AE4: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push millis() result (in TOP)
0AE7: BA          TSX               ; Get stack pointer
0AE8: 8A          TXA               ; Transfer to A
0AE9: A8          TAY               ; Transfer to Y
0AEA: A5 12       LDA $12           ; Load TOP0
0AEC: 91 61       STA ($61),Y       ; Store to stack page 0
0AEE: A5 13       LDA $13           ; Load TOP1
0AF0: 91 63       STA ($63),Y       ; Store to stack page 1
0AF2: A5 14       LDA $14           ; Load TOP2
0AF4: 91 65       STA ($65),Y       ; Store to stack page 2
0AF6: A5 15       LDA $15           ; Load TOP3
0AF8: 91 67       STA ($67),Y       ; Store to stack page 3
0AFA: 48          PHA               ; Push to hardware stack

; Pop millis() to NEXT
0AFB: 68          PLA               ; Pull from hardware stack
0AFC: BA          TSX               ; Get stack pointer
0AFD: 8A          TXA               ; Transfer to A
0AFE: A8          TAY               ; Transfer to Y
0AFF: B1 61       LDA ($61),Y       ; Load from stack page 0
0B01: 85 16       STA $16           ; Store in NEXT0
0B03: B1 63       LDA ($63),Y       ; Load from stack page 1
0B05: 85 17       STA $17           ; Store in NEXT1
0B07: B1 65       LDA ($65),Y       ; Load from stack page 2
0B09: 85 18       STA $18           ; Store in NEXT2
0B0B: B1 67       LDA ($67),Y       ; Load from stack page 3
0B0D: 85 19       STA $19           ; Store in NEXT3

; Store start at [BP-1]
0B0F: A5 60       LDA $60           ; Load BP
0B11: 18          CLC               ; Clear carry
0B12: 69 FF       ADC #$FF          ; BP - 1
0B14: A8          TAY               ; Transfer to Y
0B15: A5 16       LDA $16           ; Load NEXT0
0B17: 91 61       STA ($61),Y       ; Store to local variable
0B19: A5 17       LDA $17           ; Load NEXT1
0B1B: 91 63       STA ($63),Y       ; Store to local variable
0B1D: A5 18       LDA $18           ; Load NEXT2
0B1F: 91 65       STA ($65),Y       ; Store to local variable
0B21: A5 19       LDA $19           ; Load NEXT3
0B23: 91 67       STA ($67),Y       ; Store to local variable

; Push start again (redundant)
0B25: BA          TSX               ; Get stack pointer
0B26: 8A          TXA               ; Transfer to A
0B27: A8          TAY               ; Transfer to Y
0B28: A5 16       LDA $16           ; Load NEXT0
0B2A: 91 61       STA ($61),Y       ; Store to stack page 0
0B2C: A5 17       LDA $17           ; Load NEXT1
0B2E: 91 63       STA ($63),Y       ; Store to stack page 1
0B30: A5 18       LDA $18           ; Load NEXT2
0B32: 91 65       STA ($65),Y       ; Store to stack page 2
0B34: A5 19       LDA $19           ; Load NEXT3
0B36: 91 67       STA ($67),Y       ; Store to stack page 3
0B38: 48          PHA               ; Push to hardware stack
0B39: 68          PLA               ; ⚠️ OPTIMIZATION: Immediate pop
0B3A: 48          PHA               ; ⚠️ OPTIMIZATION: Re-push

; int f = 12
0B3B: A9 18       LDA #$18          ; Load immediate 24 (⚠️ BUG: Should be 12/$0C)
0B3D: 85 16       STA $16           ; Store in NEXT0
0B3F: 64 17       STZ $17           ; Clear NEXT1
0B41: 64 18       STZ $18           ; Clear NEXT2
0B43: 64 19       STZ $19           ; Clear NEXT3

; Push 12 onto stack
0B45: BA          TSX               ; Get stack pointer
0B46: 8A          TXA               ; Transfer to A
0B47: A8          TAY               ; Transfer to Y
0B48: A5 16       LDA $16           ; Load NEXT0
0B4A: 91 61       STA ($61),Y       ; Store to stack page 0
0B4C: A5 17       LDA $17           ; Load NEXT1
0B4E: 91 63       STA ($63),Y       ; Store to stack page 1
0B50: A5 18       LDA $18           ; Load NEXT2
0B52: 91 65       STA ($65),Y       ; Store to stack page 2
0B54: A5 19       LDA $19           ; Load NEXT3
0B56: 91 67       STA ($67),Y       ; Store to stack page 3
0B58: 48          PHA               ; Push to hardware stack

; Pop 12 to NEXT
0B59: 68          PLA               ; Pull from hardware stack
0B5A: BA          TSX               ; Get stack pointer
0B5B: 8A          TXA               ; Transfer to A
0B5C: A8          TAY               ; Transfer to Y
0B5D: B1 61       LDA ($61),Y       ; Load from stack page 0
0B5F: 85 16       STA $16           ; Store in NEXT0
0B61: B1 63       LDA ($63),Y       ; Load from stack page 1
0B63: 85 17       STA $17           ; Store in NEXT1
0B65: B1 65       LDA ($65),Y       ; Load from stack page 2
0B67: 85 18       STA $18           ; Store in NEXT2
0B69: B1 67       LDA ($67),Y       ; Load from stack page 3
0B6B: 85 19       STA $19           ; Store in NEXT3

; Store f at [BP-2]
0B6D: A5 60       LDA $60           ; Load BP
0B6F: 18          CLC               ; Clear carry
0B70: 69 FE       ADC #$FE          ; BP - 2
0B72: A8          TAY               ; Transfer to Y
0B73: A5 16       LDA $16           ; Load NEXT0
0B75: 91 61       STA ($61),Y       ; Store to local variable f
0B77: A5 17       LDA $17           ; Load NEXT1
0B79: 91 63       STA ($63),Y       ; Store to local variable f
0B7B: A5 18       LDA $18           ; Load NEXT2
0B7D: 91 65       STA ($65),Y       ; Store to local variable f
0B7F: A5 19       LDA $19           ; Load NEXT3
0B81: 91 67       STA ($67),Y       ; Store to local variable f

; Push f again (redundant)
0B83: BA          TSX               ; Get stack pointer
0B84: 8A          TXA               ; Transfer to A
0B85: A8          TAY               ; Transfer to Y
0B86: A5 16       LDA $16           ; Load NEXT0
0B88: 91 61       STA ($61),Y       ; Store to stack page 0
0B8A: A5 17       LDA $17           ; Load NEXT1
0B8C: 91 63       STA ($63),Y       ; Store to stack page 1
0B8E: A5 18       LDA $18           ; Load NEXT2
0B90: 91 65       STA ($65),Y       ; Store to stack page 2
0B92: A5 19       LDA $19           ; Load NEXT3
0B94: 91 67       STA ($67),Y       ; Store to stack page 3
0B96: 48          PHA               ; Push to hardware stack
0B97: 68          PLA               ; ⚠️ OPTIMIZATION: Immediate pop
0B98: 48          PHA               ; ⚠️ OPTIMIZATION: Re-push
0B99: 48          PHA               ; ⚠️ OPTIMIZATION: Extra push

; Load f from [BP-2] for function call
0B9A: A5 60       LDA $60           ; Load BP
0B9C: 18          CLC               ; Clear carry
0B9D: 69 FE       ADC #$FE          ; BP - 2
0B9F: A8          TAY               ; Transfer to Y
0BA0: B1 61       LDA ($61),Y       ; Load f byte 0
0BA2: 85 16       STA $16           ; Store in NEXT0
0BA4: B1 63       LDA ($63),Y       ; Load f byte 1
0BA6: 85 17       STA $17           ; Store in NEXT1
0BA8: B1 65       LDA ($65),Y       ; Load f byte 2
0BAA: 85 18       STA $18           ; Store in NEXT2
0BAC: B1 67       LDA ($67),Y       ; Load f byte 3
0BAE: 85 19       STA $19           ; Store in NEXT3

; Push f as argument
0BB0: BA          TSX               ; Get stack pointer
0BB1: 8A          TXA               ; Transfer to A
0BB2: A8          TAY               ; Transfer to Y
0BB3: A5 16       LDA $16           ; Load NEXT0
0BB5: 91 61       STA ($61),Y       ; Store to stack page 0
0BB7: A5 17       LDA $17           ; Load NEXT1
0BB9: 91 63       STA ($63),Y       ; Store to stack page 1
0BBB: A5 18       LDA $18           ; Load NEXT2
0BBD: 91 65       STA ($65),Y       ; Store to stack page 2
0BBF: A5 19       LDA $19           ; Load NEXT3
0BC1: 91 67       STA ($67),Y       ; Store to stack page 3
0BC3: 48          PHA               ; Push to hardware stack

; Call fibo(f)
0BC4: 20 25 08    JSR $0825         ; Call fibo()
0BC7: 68          PLA               ; Clean up argument

; Pop fibo result to NEXT
0BC8: BA          TSX               ; Get stack pointer
0BC9: 8A          TXA               ; Transfer to A
0BCA: A8          TAY               ; Transfer to Y
0BCB: B1 61       LDA ($61),Y       ; Load result byte 0
0BCD: 85 16       STA $16           ; Store in NEXT0
0BCF: B1 63       LDA ($63),Y       ; Load result byte 1
0BD1: 85 17       STA $17           ; Store in NEXT1
0BD3: B1 65       LDA ($65),Y       ; Load result byte 2
0BD5: 85 18       STA $18           ; Store in NEXT2
0BD7: B1 67       LDA ($67),Y       ; Load result byte 3
0BD9: 85 19       STA $19           ; Store in NEXT3

; Store total at [BP-3]
0BDB: A5 60       LDA $60           ; Load BP
0BDD: 18          CLC               ; Clear carry
0BDE: 69 FD       ADC #$FD          ; BP - 3
0BE0: A8          TAY               ; Transfer to Y
0BE1: A5 16       LDA $16           ; Load NEXT0
0BE3: 91 61       STA ($61),Y       ; Store to local variable total
0BE5: A5 17       LDA $17           ; Load NEXT1
0BE7: 91 63       STA ($63),Y       ; Store to local variable total
0BE9: A5 18       LDA $18           ; Load NEXT2
0BEB: 91 65       STA ($65),Y       ; Store to local variable total
0BED: A5 19       LDA $19           ; Load NEXT3
0BEF: 91 67       STA ($67),Y       ; Store to local variable total

; Push total (unnecessary)
0BF1: BA          TSX               ; Get stack pointer
0BF2: 8A          TXA               ; Transfer to A
0BF3: A8          TAY               ; Transfer to Y
0BF4: A5 16       LDA $16           ; Load NEXT0
0BF6: 91 61       STA ($61),Y       ; Store to stack page 0
0BF8: A5 17       LDA $17           ; Load NEXT1
0BFA: 91 63       STA ($63),Y       ; Store to stack page 1
0BFC: A5 18       LDA $18           ; Load NEXT2
0BFE: 91 65       STA ($65),Y       ; Store to stack page 2
0C00: A5 19       LDA $19           ; Load NEXT3
0C02: 91 67       STA ($67),Y       ; Store to stack page 3
0C04: 48          PHA               ; Push to hardware stack
0C05: 68          PLA               ; ⚠️ OPTIMIZATION: Immediate pop
0C06: 48          PHA               ; ⚠️ OPTIMIZATION: Re-push
0C07: 48          PHA               ; ⚠️ OPTIMIZATION: Extra push

; long elapsed = millis() - start
0C08: A2 18       LDX #$18          ; SysCall.TimeMillis
0C0A: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push millis() result
0C0D: BA          TSX               ; Get stack pointer
0C0E: 8A          TXA               ; Transfer to A
0C0F: A8          TAY               ; Transfer to Y
0C10: A5 12       LDA $12           ; Load TOP0
0C12: 91 61       STA ($61),Y       ; Store to stack page 0
0C14: A5 13       LDA $13           ; Load TOP1
0C16: 91 63       STA ($63),Y       ; Store to stack page 1
0C18: A5 14       LDA $14           ; Load TOP2
0C1A: 91 65       STA ($65),Y       ; Store to stack page 2
0C1C: A5 15       LDA $15           ; Load TOP3
0C1E: 91 67       STA ($67),Y       ; Store to stack page 3
0C20: 48          PHA               ; Push to hardware stack

; Load start from [BP-1]
0C21: A5 60       LDA $60           ; Load BP
0C23: 18          CLC               ; Clear carry
0C24: 69 FF       ADC #$FF          ; BP - 1
0C26: A8          TAY               ; Transfer to Y
0C27: B1 61       LDA ($61),Y       ; Load start byte 0
0C29: 85 16       STA $16           ; Store in NEXT0
0C2B: B1 63       LDA ($63),Y       ; Load start byte 1
0C2D: 85 17       STA $17           ; Store in NEXT1
0C2F: B1 65       LDA ($65),Y       ; Load start byte 2
0C31: 85 18       STA $18           ; Store in NEXT2
0C33: B1 67       LDA ($67),Y       ; Load start byte 3
0C35: 85 19       STA $19           ; Store in NEXT3

; Push start
0C37: BA          TSX               ; Get stack pointer
0C38: 8A          TXA               ; Transfer to A
0C39: A8          TAY               ; Transfer to Y
0C3A: A5 16       LDA $16           ; Load NEXT0
0C3C: 91 61       STA ($61),Y       ; Store to stack page 0
0C3E: A5 17       LDA $17           ; Load NEXT1
0C40: 91 63       STA ($63),Y       ; Store to stack page 1
0C42: A5 18       LDA $18           ; Load NEXT2
0C44: 91 65       STA ($65),Y       ; Store to stack page 2
0C46: A5 19       LDA $19           ; Load NEXT3
0C48: 91 67       STA ($67),Y       ; Store to stack page 3
0C4A: 48          PHA               ; Push to hardware stack

; Pop values for subtraction
0C4B: 68          PLA               ; Pull from hardware stack
0C4C: BA          TSX               ; Get stack pointer
0C4D: 8A          TXA               ; Transfer to A
0C4E: A8          TAY               ; Transfer to Y
0C4F: B1 61       LDA ($61),Y       ; Load from stack page 0
0C51: 85 12       STA $12           ; Store in TOP0
0C53: B1 63       LDA ($63),Y       ; Load from stack page 1
0C55: 85 13       STA $13           ; Store in TOP1
0C57: B1 65       LDA ($65),Y       ; Load from stack page 2
0C59: 85 14       STA $14           ; Store in TOP2
0C5B: B1 67       LDA ($67),Y       ; Load from stack page 3
0C5D: 85 15       STA $15           ; Store in TOP3

0C5F: 68          PLA               ; Pull from hardware stack
0C60: BA          TSX               ; Get stack pointer
0C61: 8A          TXA               ; Transfer to A
0C62: A8          TAY               ; Transfer to Y
0C63: B1 61       LDA ($61),Y       ; Load from stack page 0
0C65: 85 16       STA $16           ; Store in NEXT0
0C67: B1 63       LDA ($63),Y       ; Load from stack page 1
0C69: 85 17       STA $17           ; Store in NEXT1
0C6B: B1 65       LDA ($65),Y       ; Load from stack page 2
0C6D: 85 18       STA $18           ; Store in NEXT2
0C6F: B1 67       LDA ($67),Y       ; Load from stack page 3
0C71: 85 19       STA $19           ; Store in NEXT3

; Call Long.Sub (millis() - start)
0C73: A2 1B       LDX #$1B          ; SysCall.LongSub
0C75: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Push result
0C78: BA          TSX               ; Get stack pointer
0C79: 8A          TXA               ; Transfer to A
0C7A: A8          TAY               ; Transfer to Y
0C7B: A5 16       LDA $16           ; Load NEXT0 (result)
0C7D: 91 61       STA ($61),Y       ; Store to stack page 0
0C7F: A5 17       LDA $17           ; Load NEXT1
0C81: 91 63       STA ($63),Y       ; Store to stack page 1
0C83: A5 18       LDA $18           ; Load NEXT2
0C85: 91 65       STA ($65),Y       ; Store to stack page 2
0C87: A5 19       LDA $19           ; Load NEXT3
0C89: 91 67       STA ($67),Y       ; Store to stack page 3
0C8B: 48          PHA               ; Push to hardware stack

; Pop elapsed to NEXT
0C8C: 68          PLA               ; Pull from hardware stack
0C8D: BA          TSX               ; Get stack pointer
0C8E: 8A          TXA               ; Transfer to A
0C8F: A8          TAY               ; Transfer to Y
0C90: B1 61       LDA ($61),Y       ; Load from stack page 0
0C92: 85 16       STA $16           ; Store in NEXT0
0C94: B1 63       LDA ($63),Y       ; Load from stack page 1
0C96: 85 17       STA $17           ; Store in NEXT1
0C98: B1 65       LDA ($65),Y       ; Load from stack page 2
0C9A: 85 18       STA $18           ; Store in NEXT2
0C9C: B1 67       LDA ($67),Y       ; Load from stack page 3
0C9E: 85 19       STA $19           ; Store in NEXT3

; Store elapsed at [BP-4]
0CA0: A5 60       LDA $60           ; Load BP
0CA2: 18          CLC               ; Clear carry
0CA3: 69 FC       ADC #$FC          ; BP - 4
0CA5: A8          TAY               ; Transfer to Y
0CA6: A5 16       LDA $16           ; Load NEXT0
0CA8: 91 61       STA ($61),Y       ; Store to local variable elapsed
0CAA: A5 17       LDA $17           ; Load NEXT1
0CAC: 91 63       STA ($63),Y       ; Store to local variable elapsed
0CAE: A5 18       LDA $18           ; Load NEXT2
0CB0: 91 65       STA ($65),Y       ; Store to local variable elapsed
0CB2: A5 19       LDA $19           ; Load NEXT3
0CB4: 91 67       STA ($67),Y       ; Store to local variable elapsed

; Push elapsed (unnecessary)
0CB6: BA          TSX               ; Get stack pointer
0CB7: 8A          TXA               ; Transfer to A
0CB8: A8          TAY               ; Transfer to Y
0CB9: A5 16       LDA $16           ; Load NEXT0
0CBB: 91 61       STA ($61),Y       ; Store to stack page 0
0CBD: A5 17       LDA $17           ; Load NEXT1
0CBF: 91 63       STA ($63),Y       ; Store to stack page 1
0CC1: A5 18       LDA $18           ; Load NEXT2
0CC3: 91 65       STA ($65),Y       ; Store to stack page 2
0CC5: A5 19       LDA $19           ; Load NEXT3
0CC7: 91 67       STA ($67),Y       ; Store to stack page 3
0CC9: 48          PHA               ; Push to hardware stack
0CCA: 68          PLA               ; ⚠️ OPTIMIZATION: Immediate pop
0CCB: 48          PHA               ; ⚠️ OPTIMIZATION: Re-push

; printf("Fibo(%d)=%d\n", f, total)
0CCC: A9 07       LDA #$07          ; String address low
0CCE: 85 1E       STA $1E           ; Store in STR.L
0CD0: A9 08       LDA #$08          ; String address high
0CD2: 85 1F       STA $1F           ; Store in STR.H

; Print format string characters
0CD4: A0 00       LDY #$00          ; Start at offset 0
0CD6: B1 1E       LDA ($1E),Y       ; Load character
0CD8: C0 05       CPY #$05          ; Check if at %d position
0CDA: F0 08       BEQ $0CE4         ; Jump to format handler
0CDC: A2 12       LDX #$12          ; SysCall.PrintChar
0CDE: 20 03 08    JSR $0803         ; Call BIOS dispatch
0CE1: C8          INY               ; Next character
0CE2: 80 F2       BRA $0CD6         ; Loop

; Load f from [BP-2] for printf
0CE4: A5 60       LDA $60           ; Load BP
0CE6: 18          CLC               ; Clear carry
0CE7: 69 FE       ADC #$FE          ; BP - 2
0CE9: A8          TAY               ; Transfer to Y
0CEA: B1 61       LDA ($61),Y       ; Load f byte 0
0CEC: 85 16       STA $16           ; Store in NEXT0
0CEE: B1 63       LDA ($63),Y       ; Load f byte 1
0CF0: 85 17       STA $17           ; Store in NEXT1
0CF2: B1 65       LDA ($65),Y       ; Load f byte 2
0CF4: 85 18       STA $18           ; Store in NEXT2
0CF6: B1 67       LDA ($67),Y       ; Load f byte 3
0CF8: 85 19       STA $19           ; Store in NEXT3

; Push f
0CFA: BA          TSX               ; Get stack pointer
0CFB: 8A          TXA               ; Transfer to A
0CFC: A8          TAY               ; Transfer to Y
0CFD: A5 16       LDA $16           ; Load NEXT0
0CFF: 91 61       STA ($61),Y       ; Store to stack page 0
0D01: A5 17       LDA $17           ; Load NEXT1
0D03: 91 63       STA ($63),Y       ; Store to stack page 1
0D05: A5 18       LDA $18           ; Load NEXT2
0D07: 91 65       STA ($65),Y       ; Store to stack page 2
0D09: A5 19       LDA $19           ; Load NEXT3
0D0B: 91 67       STA ($67),Y       ; Store to stack page 3
0D0D: 48          PHA               ; Push to hardware stack

; Pop f to TOP for printing
0D0E: 68          PLA               ; Pull from hardware stack
0D0F: BA          TSX               ; Get stack pointer
0D10: 8A          TXA               ; Transfer to A
0D11: A8          TAY               ; Transfer to Y
0D12: B1 61       LDA ($61),Y       ; Load from stack page 0
0D14: 85 12       STA $12           ; Store in TOP0
0D16: B1 63       LDA ($63),Y       ; Load from stack page 1
0D18: 85 13       STA $13           ; Store in TOP1
0D1A: B1 65       LDA ($65),Y       ; Load from stack page 2
0D1C: 85 14       STA $14           ; Store in TOP2
0D1E: B1 67       LDA ($67),Y       ; Load from stack page 3
0D20: 85 15       STA $15           ; Store in TOP3

; Print integer f
0D22: A2 1F       LDX #$1F          ; SysCall.LongPrint
0D24: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Continue format string
0D27: A0 07       LDY #$07          ; Continue at position 7
0D29: B1 1E       LDA ($1E),Y       ; Load character
0D2B: C0 09       CPY #$09          ; Check if at second %d
0D2D: F0 08       BEQ $0D37         ; Jump to format handler
0D2F: A2 12       LDX #$12          ; SysCall.PrintChar
0D31: 20 03 08    JSR $0803         ; Call BIOS dispatch
0D34: C8          INY               ; Next character
0D35: 80 F2       BRA $0D29         ; Loop

; Load total from [BP-3] for printf
0D37: A5 60       LDA $60           ; Load BP
0D39: 18          CLC               ; Clear carry
0D3A: 69 FD       ADC #$FD          ; BP - 3
0D3C: A8          TAY               ; Transfer to Y
0D3D: B1 61       LDA ($61),Y       ; Load total byte 0
0D3F: 85 16       STA $16           ; Store in NEXT0
0D41: B1 63       LDA ($63),Y       ; Load total byte 1
0D43: 85 17       STA $17           ; Store in NEXT1
0D45: B1 65       LDA ($65),Y       ; Load total byte 2
0D47: 85 18       STA $18           ; Store in NEXT2
0D49: B1 67       LDA ($67),Y       ; Load total byte 3
0D4B: 85 19       STA $19           ; Store in NEXT3

; Push total
0D4D: BA          TSX               ; Get stack pointer
0D4E: 8A          TXA               ; Transfer to A
0D4F: A8          TAY               ; Transfer to Y
0D50: A5 16       LDA $16           ; Load NEXT0
0D52: 91 61       STA ($61),Y       ; Store to stack page 0
0D54: A5 17       LDA $17           ; Load NEXT1
0D56: 91 63       STA ($63),Y       ; Store to stack page 1
0D58: A5 18       LDA $18           ; Load NEXT2
0D5A: 91 65       STA ($65),Y       ; Store to stack page 2
0D5C: A5 19       LDA $19           ; Load NEXT3
0D5E: 91 67       STA ($67),Y       ; Store to stack page 3
0D60: 48          PHA               ; Push to hardware stack

; Pop total to TOP for printing
0D61: 68          PLA               ; Pull from hardware stack
0D62: BA          TSX               ; Get stack pointer
0D63: 8A          TXA               ; Transfer to A
0D64: A8          TAY               ; Transfer to Y
0D65: B1 61       LDA ($61),Y       ; Load from stack page 0
0D67: 85 12       STA $12           ; Store in TOP0
0D69: B1 63       LDA ($63),Y       ; Load from stack page 1
0D6B: 85 13       STA $13           ; Store in TOP1
0D6D: B1 65       LDA ($65),Y       ; Load from stack page 2
0D6F: 85 14       STA $14           ; Store in TOP2
0D71: B1 67       LDA ($67),Y       ; Load from stack page 3
0D73: 85 15       STA $15           ; Store in TOP3

; Print integer total
0D75: A2 1F       LDX #$1F          ; SysCall.LongPrint
0D77: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Continue format string to end
0D7A: A0 0B       LDY #$0B          ; Continue at position 11
0D7C: B1 1E       LDA ($1E),Y       ; Load character
0D7E: C0 0C       CPY #$0C          ; Check if at null terminator
0D80: F0 08       BEQ $0D8A         ; Exit if done
0D82: A2 12       LDX #$12          ; SysCall.PrintChar
0D84: 20 03 08    JSR $0803         ; Call BIOS dispatch
0D87: C8          INY               ; Next character
0D88: 80 F2       BRA $0D7C         ; Loop
0D8A: 68          PLA               ; ⚠️ OPTIMIZATION: Unnecessary
0D8B: 48          PHA               ; ⚠️ OPTIMIZATION: Unnecessary

; printf("Elapsed: %ld ms\n", elapsed)
0D8C: A9 14       LDA #$14          ; String address low
0D8E: 85 1E       STA $1E           ; Store in STR.L
0D90: A9 08       LDA #$08          ; String address high
0D92: 85 1F       STA $1F           ; Store in STR.H

; Print format string characters
0D94: A0 00       LDY #$00          ; Start at offset 0
0D96: B1 1E       LDA ($1E),Y       ; Load character
0D98: C0 09       CPY #$09          ; Check if at %ld position
0D9A: F0 08       BEQ $0DA4         ; Jump to format handler
0D9C: A2 12       LDX #$12          ; SysCall.PrintChar
0D9E: 20 03 08    JSR $0803         ; Call BIOS dispatch
0DA1: C8          INY               ; Next character
0DA2: 80 F2       BRA $0D96         ; Loop

; Load elapsed from [BP-4] for printf
0DA4: A5 60       LDA $60           ; Load BP
0DA6: 18          CLC               ; Clear carry
0DA7: 69 FC       ADC #$FC          ; BP - 4
0DA9: A8          TAY               ; Transfer to Y
0DAA: B1 61       LDA ($61),Y       ; Load elapsed byte 0
0DAC: 85 16       STA $16           ; Store in NEXT0
0DAE: B1 63       LDA ($63),Y       ; Load elapsed byte 1
0DB0: 85 17       STA $17           ; Store in NEXT1
0DB2: B1 65       LDA ($65),Y       ; Load elapsed byte 2
0DB4: 85 18       STA $18           ; Store in NEXT2
0DB6: B1 67       LDA ($67),Y       ; Load elapsed byte 3
0DB8: 85 19       STA $19           ; Store in NEXT3

; Push elapsed
0DBA: BA          TSX               ; Get stack pointer
0DBB: 8A          TXA               ; Transfer to A
0DBC: A8          TAY               ; Transfer to Y
0DBD: A5 16       LDA $16           ; Load NEXT0
0DBF: 91 61       STA ($61),Y       ; Store to stack page 0
0DC1: A5 17       LDA $17           ; Load NEXT1
0DC3: 91 63       STA ($63),Y       ; Store to stack page 1
0DC5: A5 18       LDA $18           ; Load NEXT2
0DC7: 91 65       STA ($65),Y       ; Store to stack page 2
0DC9: A5 19       LDA $19           ; Load NEXT3
0DCB: 91 67       STA ($67),Y       ; Store to stack page 3
0DCD: 48          PHA               ; Push to hardware stack

; Pop elapsed to TOP for printing
0DCE: 68          PLA               ; Pull from hardware stack
0DCF: BA          TSX               ; Get stack pointer
0DD0: 8A          TXA               ; Transfer to A
0DD1: A8          TAY               ; Transfer to Y
0DD2: B1 61       LDA ($61),Y       ; Load from stack page 0
0DD4: 85 12       STA $12           ; Store in TOP0
0DD6: B1 63       LDA ($63),Y       ; Load from stack page 1
0DD8: 85 13       STA $13           ; Store in TOP1
0DDA: B1 65       LDA ($65),Y       ; Load from stack page 2
0DDC: 85 14       STA $14           ; Store in TOP2
0DDE: B1 67       LDA ($67),Y       ; Load from stack page 3
0DE0: 85 15       STA $15           ; Store in TOP3

; Print long elapsed
0DE2: A2 1F       LDX #$1F          ; SysCall.LongPrint
0DE4: 20 03 08    JSR $0803         ; Call BIOS dispatch

; Continue format string to end
0DE7: A0 0C       LDY #$0C          ; Continue at position 12
0DE9: B1 1E       LDA ($1E),Y       ; Load character
0DEB: C0 10       CPY #$10          ; Check if at null terminator
0DED: F0 08       BEQ $0DF7         ; Exit if done
0DEF: A2 12       LDX #$12          ; SysCall.PrintChar
0DF1: 20 03 08    JSR $0803         ; Call BIOS dispatch
0DF4: C8          INY               ; Next character
0DF5: 80 F2       BRA $0DE9         ; Loop
0DF7: 68          PLA               ; Clean up stack

; Function epilogue
0DF8: A6 60       LDX $60           ; Load BP
0DFA: 9A          TXS               ; Restore SP
0DFB: 68          PLA               ; Pull saved BP
0DFC: 85 60       STA $60           ; Restore BP
0DFE: 60          RTS               ; Return
```

## Summary of Major Optimization Opportunities

1. **Excessive PHA/PLA pairs**: ~150 unnecessary bytes
2. **Duplicate variable loads**: ~80 unnecessary bytes
3. **Redundant epilogue**: 6 unnecessary bytes
4. **32-bit operations for small values**: Could save ~40% on arithmetic
5. **Wrong constant at 0B3B**: Should be #$0C (12) not #$18 (24)

**Total potential savings: ~240 bytes (approximately 40% of code size)**