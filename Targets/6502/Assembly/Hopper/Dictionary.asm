; ######################## Dictionary syscalls ########################

; Dictionary memory map:
;   0000 heap allocator size
;   13   type = tDictionary
;   00   GC reference count
;   xx   kType: key type tString or tUint
;   xx   vType: value type
;   xxxx count of entries
;   xxxx capacity (number of slots in block of entries)
;   xxxx pEntries (memory block of HashStringEntry or HashUIntEntry entries)

; HashStringEntry memory block: (8 bytes per entry)
;   0000 heap allocator size
;   xxxx pKey (string)
;   xxxxxxxx hash; (32 bits)
;   xxxx pValue variant
;   ...
;   xxxx pKey (string)
;   xxxxxxxx hash; (32 bits)
;   xxxx pValue variant

; HashUIntEntry memory block:  (8 bytes per entry)
;   0000 heap allocator size
;   xxxx key
;   XXxxxxxx isOccupied; ; like key != nullptr in HashEntry (zero initialization means all !isOccupied == isEmpty to start)
;   xxxx pValue variant
;   ...
;   xxxx key
;   XXxxxxxx isOccupied;
;   xxxx pValue variant

; Clear(<K,V> this) system;


; const char * str = "Hello World!";
; Key:               0xB1EA4872
; const char* str2 = "Hopper hop hop hopping along.";
; Key:               0x4C282641

;fSIZE = F1
;fSIZEL = F1
;fSIZEH = F2

;fSOURCEADDRESS  = F3
;fSOURCEADDRESSL = F3
;fSOURCEADDRESSH = F4

;fVALUE = F10
;fVALUEL = F10
;fVALUEH = F11

;lCOUNT  = F14
;lCOUNTL = F14
;lCOUNTH = F15

dVTYPE = D0
dKTYPE = D1

dHASHENTRIES = D2
dHASHENTRIESL = D2
dHASHENTRIESH = D3

dCAPACITY = D4
dCAPACITYL = D4
dCAPACITYH = D5

dKEY = D6
dKEYL = D6
dKEYH = D7

dHASHENTRY = D8
dHASHENTRYL = D8
dHASHENTRYH = D9

dHASH0 = D10
dHASH1 = D11
dHASH2 = D12
dHASH3 = D13

dITERATOR  = D14
dITERATORL = D14
dITERATORH = D15

; zero page slots U0..U7:
hSRC0 = lRESULT0
hSRC1 = lRESULT1
hSRC2 = lRESULT2
hSRC3 = lRESULT3

; F5..F8: clashes with fDESTINATIONADDRESS, fTYPE, fLENGTH, lPREVIOUS, lNEXT
hDST0 = lTOP0 
hDST1 = lTOP1
hDST2 = lTOP2
hDST3 = lTOP3

  .ifdef MEMDEBUG2
decCURRENT:
  pha
  lda uCURRENTL
  bne decCURRENTSkip
  dec uCURRENTH
decCURRENTSkip:
  dec uCURRENTL
  pla
  rts
  .endif

  .ifdef UNUSED
  
  .ifdef LONGS
syscallHashKey:

  ; this -> dKEY
  jsr decSP          ; MSB
  lda (SP)
  sta dKEYH
  jsr decSP          ; LSB
  lda (SP)
  sta dKEYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif
  
  jsr hashKey
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  
  lda #4
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tLong
  jsr gcCreate
  
  ; make sure it is zero initialized
  ldy #2
  lda dHASH0
  sta (IDX), Y
  iny
  lda dHASH1
  sta (IDX), Y
  iny
  lda dHASH2
  sta (IDX), Y
  iny
  lda dHASH3
  sta (IDX), Y
  
  lda #tLong
  jmp pushIDXExit
  .endif
  .endif

hashKey:
; string to hash in dKEY
; uses IDX, IDY, 
;      lRESULT0..7           : U0..U7
;      hDST0..hDST3,         : F5..F8
;      hSRC0..hSRC3          : U0..U3
;   result in dHASH0..dHASH3 : D10..D13

  lda IDXH
  pha
  lda IDXL
  pha
  lda IDYH
  pha
  lda IDYL
  pha
  
  lda dKEYL
  sta IDXL
  lda dKEYH
  sta IDXH

  ; skip past type and reference count
  jsr incIDX
  jsr incIDX
  
  ; string length -> IDY
  ldy #1
  lda (IDX)
  sta IDYL
  lda (IDX), Y
  sta IDYH
  
  ; skip past length
  jsr incIDX
  jsr incIDX
  
; DST = 0x811C9DC5;
  lda #$C5
  sta hDST0
  lda #$9D
  sta hDST1
  lda #$1C
  sta hDST2
  lda #$81
  sta hDST3

hashLoop:  
  lda IDYL
  bne hashNextCharacter
  lda IDYH
  bne hashNextCharacter
  jmp hashDone
hashNextCharacter:
  
  ; load next character from string
  lda (IDX)
  sta hSRC0
  stz hSRC1
  stz hSRC2
  stz hSRC3
  
  ;32 bit XOR: DST = DST ^ SRC
  lda hSRC0
  eor hDST0 
  sta hDST0
  
  ; 32 bit unsigned multiply: DST = DST * 0x01000193;
  stz lRESULT4   ;Clear upper half of
  stz lRESULT5   ;product
  stz lRESULT6
  stz lRESULT7
  ldx #$20     ; set binary count to 32
hashLongMULShiftR:   
  lsr hDST3   ; shift multiplyer right
  ror hDST2
  ror hDST1
  ror hDST0
  bcc hashLongMULRotateR ;Go rotate right if c = 0
  lda lRESULT4   ; get upper half of product and add multiplicand to it
  clc               
  adc #$93
  sta lRESULT4
  lda lRESULT5
  adc #$01
  sta lRESULT5
  lda lRESULT6
  adc #$00
  sta lRESULT6
  lda lRESULT7
  adc #$01
hashLongMULRotateR:
  ror a    ; rotate partial product
  sta lRESULT7   ; right
  ror lRESULT6
  ror lRESULT5
  ror lRESULT4
  ror lRESULT3
  ror lRESULT2
  ror lRESULT1
  ror lRESULT0
  dex              ; decrement bit count and
  bne hashLongMULShiftR  ; loop until 32 bits are done
  
  ; least significant 4 bytes of the result are in lRESULT0..lRESULT3
  lda lRESULT0
  sta hDST0
  lda lRESULT1
  sta hDST1
  lda lRESULT2
  sta hDST2
  lda lRESULT3
  sta hDST3
  
  jsr incIDX
  jsr decIDY
  jmp hashLoop
hashDone:

  lda lRESULT0
  sta dHASH0
  lda lRESULT1
  sta dHASH1
  lda lRESULT2
  sta dHASH2
  lda lRESULT3
  sta dHASH3

  pla
  sta IDYL
  pla
  sta IDYH
  pla
  sta IDXL
  pla
  sta IDXH

  rts


  

  .ifdef UNUSED
keyModCapacity:
  ; key in dKEY
  ; capacity in dCapacity
  ;   result in ACC
  
  lda dKEYL
  sta NEXTL
  lda dKEYH
  sta NEXTH
  
  lda dCAPACITYL
  sta TOPL
  lda dCAPACITYH
  sta TOPH
  
  ; TOP = NEXT / TOP (divisor)
  ; ACC (remainder)
  
  ; https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
  ; https://llx.com/Neil/a2/mult.html

  stz ACCL
  stz ACCH
  ldx #16
  
keyModCapacityLoop:
  asl NEXTL
  rol NEXTH
  rol ACCL
  rol ACCH
  lda ACCL
  sec
  sbc TOPL
  tay
  lda ACCH
  sbc TOPH
  bcc keyModCapacitySkip
  
  sta ACCH
  sty ACCL
  inc NEXTL
  
keyModCapacitySkip:
  dex
  bne keyModCapacityLoop
  rts
  .endif ; UNUSED
  
hashModCapacity:
  ; hash in dHASH
  ; capacity in dCapacity
  ; uses IDX, IDY
  ;      hDST0..hDST3,         : F5..F8
  ;   result in lRESULT0..lRESULT4
  
  ; #### https://llx.com/Neil/a2/mult.html ####
  ; http://www.6502.org/source/integers/32muldiv.htm
  
  lda dHASH0
  sta hDST0
  lda dHASH1
  sta hDST1
  lda dHASH2
  sta hDST2
  lda dHASH3
  sta hDST3
  
  ;NUM1 = NUM1 / NUM2 + REM
  ;hDST = hDST / dCAPACITY + lRESULT
  
  ;Initialize remainder to 0
  stz lRESULT0
  stz lRESULT1
  stz lRESULT2
  stz lRESULT3
  ldx #32   ; there are 16 bits in N
hashModCapacityNextBit:
  asl hDST0    ; shift hi bit of N into R
  rol hDST1    ; (vacating the lo bit, which will be used for the quotient)
  rol hDST2
  rol hDST3
  rol lRESULT0
  rol lRESULT1
  rol lRESULT2
  rol lRESULT3
  
  sec       ; trial subtraction
  lda lRESULT0
  sbc dCAPACITYL
  sta lRESULT4
  lda lRESULT1
  sbc dCAPACITYH
  sta lRESULT5
  lda lRESULT2
  sbc #0
  sta lRESULT6
  lda lRESULT3
  sbc #0
  ;sta lRESULT7
  
  bcc hashModCapacityNextNo      ; did subtraction succeed?
  ;lda lRESULT7
  sta lRESULT3   ; if yes, save it
  lda lRESULT6
  sta lRESULT2
  lda lRESULT5 
  sta lRESULT1
  lda lRESULT4
  sta lRESULT0
  inc hDST0   ; and record a 1 in the quotient
hashModCapacityNextNo:
  dex
  bne hashModCapacityNextBit
  rts
  
  
hashTableFindEntry:
  ; 'entries' in dHASHENTRIES
  ; 'key' string in dKEY
  ; 'hash' in dHASH0..dHASH3
  ; 'capacity' in dCAPACITY
  ; uses dHASHENTRY, ACC
  ;    returns 'hashEntry' in dHASHENTRY
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha

  ; unsigned int index = hash % capacity;
  jsr hashModCapacity
  
  ; low 16 bits of mod result -> index IDX
  lda lRESULT0
  sta IDXL
  iny
  lda lRESULT1
  sta IDXH
  
  ;jsr diagnosticOutNewLine
  ;lda #"B"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRIESH
  ;jsr diagnosticOutHex   
  ;lda dHASHENTRIESL
  ;jsr diagnosticOutHex   
  ;lda #"H"
  ;jsr diagnosticOutChar
  ;lda dHASH3
  ;jsr diagnosticOutHex   
  ;lda dHASH2
  ;jsr diagnosticOutHex   
  ;lda dHASH1
  ;jsr diagnosticOutHex   
  ;lda dHASH0
  ;jsr diagnosticOutHex   
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda dCAPACITYH
  ;jsr diagnosticOutHex   
  ;lda dCAPACITYL
  ;jsr diagnosticOutHex   
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"I"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex   
  ;lda IDXL
  ;jsr diagnosticOutHex   
  
  ; tombstone = nullptr;
  stz IDYL
  stz IDYH
hashTableFindEntryNext:
  ; HashEntry* hashEntry = &entries[index];
  ; dHASHENTRY = dHASHENTRIES + (IDX << 3)
  
  lda IDXL
  sta ACCL
  lda IDXH
  sta ACCH
  
  ; ACC = ACC << 3
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"8"
  ;jsr diagnosticOutChar
  ;lda ACCH
  ;jsr diagnosticOutHex   
  ;lda ACCL
  ;jsr diagnosticOutHex   
  
  clc
  lda dHASHENTRIESL
  adc ACCL
  sta dHASHENTRYL
  lda dHASHENTRIESH
  adc ACCH
  sta dHASHENTRYH
  
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRYH
  ;jsr diagnosticOutHex   
  ;lda dHASHENTRYL
  ;jsr diagnosticOutHex   
  
  ; hashEntry->key == nullptr ?
  ldy #0
  lda (dHASHENTRY), Y
  bne hashTableFindEntryKeyNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashTableFindEntryKeyNotNull
  ; hashEntry->key == nullptr
  
  ;lda #"a"
  ;jsr diagnosticOutChar
  
  ; hashEntry->value == nullptr?
  ldy #6
  lda (dHASHENTRY), Y
  bne hashTableFindEntryValueNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashTableFindEntryValueNotNull
  
  ; hashEntry->value == nullptr
  
  ;lda #"b"
  ;jsr diagnosticOutChar
  
  ; empty entry
  
  ; tombstone == nullptr?
  lda IDYL
  bne hashTableFindEntryTombstoneNotNull2
  lda IDYH
  bne hashTableFindEntryTombstoneNotNull2
  
  ;lda #"c"
  ;jsr diagnosticOutChar
  
  ; return hashEntry
  bra hashTableFindEntryExit
hashTableFindEntryTombstoneNotNull2:

  ;lda #"d"
  ;jsr diagnosticOutChar
  
  ; return tombstone
  lda IDYL
  sta dHASHENTRYL
  lda IDYH
  sta dHASHENTRYH
  bra hashTableFindEntryExit
hashTableFindEntryValueNotNull:
  ; hashEntry->value != nullptr
  ; tombstone
  
  ;lda #"e"
  ;jsr diagnosticOutChar
  
  ; tombstone == nullptr?
  lda IDYL
  bne hashTableFindEntryTombstoneNotNull
  lda IDYH
  bne hashTableFindEntryTombstoneNotNull
  ; tombstone == nullptr
  
  ;lda #"f"
  ;jsr diagnosticOutChar
  
  ; tombstone = hashEntry;
  lda dHASHENTRYL
  sta IDYL
  lda dHASHENTRYH
  sta IDYH
  
hashTableFindEntryTombstoneNotNull:
  bra hashTableFindEntryNextIndex
hashTableFindEntryKeyNotNull:
  ; hashEntry->key != nullptr
  
  ;lda #"g"
  ;jsr diagnosticOutChar
  
  ; hashEntry->key
  ldy #0
  lda (dHASHENTRY), Y
  sta TOPL
  iny
  lda (dHASHENTRY), Y
  sta TOPH
  lda dKEYL
  sta NEXTL
  lda dKEYH
  sta NEXTH
  
  ; compare strings TOP and NEXT
  ; result Z set if equal
  ;   uses ACC
  jsr stringsEqual
  beq hashTableFindEntryExit

hashTableFindEntryNextIndex:

  ; index = (index + 1) % capacity;
  
  inc IDXL
  bne incIDXEnd3
  inc IDXH
incIDXEnd3:
  
  ; dCAPACITY is always a power of 2 (typically $0010, $0020, $0040 ..)
  lda dCAPACITYL
  sta ACCL
  lda dCAPACITYH
  sta ACCH
  
  ; $000F, $001F, $003F ..
  lda ACCL
  bne hashValueTableFindEntrySkipMSB3
  dec ACCH
hashValueTableFindEntrySkipMSB3:
  dec ACCL
  
  lda IDXH
  and ACCH
  sta IDXH
  lda IDXL
  and ACCL
  sta IDXL
  
  jmp hashTableFindEntryNext
hashTableFindEntryExit:

  pla 
  sta IDYH
  pla 
  sta IDYL
  pla 
  sta IDXH
  pla 
  sta IDXL
  
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"E"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRYH
  ;jsr diagnosticOutHex   
  ;lda dHASHENTRYL
  ;jsr diagnosticOutHex   
  
  rts
  
hashValueTableFindEntry:
  ; 'entries' in dHASHENTRIES
  ; 'key' string in dKEY
  ; 'capacity' in dCAPACITY
  ; uses dHASHENTRY, ACC
  ;    returns 'hashEntry' in dHASHENTRY
  lda IDXL
  pha
  lda IDXH
  pha
  lda IDYL
  pha
  lda IDYH
  pha

  ; unsigned int index = key % capacity;
  
  ; dCAPACITY is always a power of 2 (typically $0010, $0020, $0040 ..)
  lda dCAPACITYL
  sta ACCL
  lda dCAPACITYH
  sta ACCH
  
  ; $000F, $001F, $003F ..
  lda ACCL
  bne keyModCapacitySkipMSB
  dec ACCH
keyModCapacitySkipMSB:
  dec ACCL
  
  lda dKEYH
  and ACCH
  sta IDXH
  lda dKEYL
  and ACCL
  sta IDXL
  
  ;jsr diagnosticOutNewLine
  ;lda #"B"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRIESH
  ;jsr diagnosticOutHex   
  ;lda dHASHENTRIESL
  ;jsr diagnosticOutHex   
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda dCAPACITYH
  ;jsr diagnosticOutHex   
  ;lda dCAPACITYL
  ;jsr diagnosticOutHex   
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"I"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex   
  ;lda IDXL
  ;jsr diagnosticOutHex
  
  ; tombstone = nullptr;
  stz IDYL
  stz IDYH
hashValueTableFindEntryNext:
  
  ; hashValueEntry = &entries[index];
  ; dHASHENTRY = dHASHENTRIES + (IDX << 3)
  
  lda IDXL
  sta ACCL
  lda IDXH
  sta ACCH
  
  ; ACC = ACC << 3
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"8"
  ;jsr diagnosticOutChar
  ;lda ACCH
  ;jsr diagnosticOutHex   
  ;lda ACCL
  ;jsr diagnosticOutHex   
  
  clc
  lda dHASHENTRIESL
  adc ACCL
  sta dHASHENTRYL
  lda dHASHENTRIESH
  adc ACCH
  sta dHASHENTRYH
  
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRYH
  ;jsr diagnosticOutHex   
  ;lda dHASHENTRYL
  ;jsr diagnosticOutHex   	

  ; HashUIntEntry
  ;   xxxx key
  ;   XXxxxxxx isOccupied; ; like key != nullptr in HashEntry (zero initialization means all !isOccupied == isEmpty to start)
  ;   xxxx pValue variant  
  ldy #2
  lda (dHASHENTRY), Y
  bne hashValueTableFindEntryOccupied
  
  ; !hashEntry->isOccupied
  ;lda #"a"
  ;jsr diagnosticOutChar
  
  ; hashEntry->value == nullptr ? 
  lda #6
  lda (dHASHENTRY), Y
  bne hashValueTableFindEntryValueNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashValueTableFindEntryValueNotNull
  
  ; hashEntry->value == nullptr
  ;lda #"b"
  ;jsr diagnosticOutChar
  
  ; empty entry
  
  ; tombstone == nullptr?
  lda IDYH
  bne hashValueTableFindEntryValueTombstoneNotNull
  lda IDYL
  bne hashValueTableFindEntryValueTombstoneNotNull
  ; tombstone == nullptr
  ;lda #"c"
  ;jsr diagnosticOutChar
  
  ; return hashEntry
  bra hashValueTableFindEntryExit
hashValueTableFindEntryValueTombstoneNotNull:  
  ; tombstone != nullptr
  ;lda #"d"
  ;jsr diagnosticOutChar
  
  ; return tombstone
  lda IDYH
  sta dHASHENTRYH
  lda IDYL
  sta dHASHENTRYL
  bra hashValueTableFindEntryExit
hashValueTableFindEntryValueNotNull:
  ; tombstone == nullptr?
  lda IDYH
  bne hashValueTableFindEntryAfterOccupied
  lda IDYL
  bne hashValueTableFindEntryAfterOccupied
  
  ; tombstone == nullptr
  ;lda #"e"
  ;jsr diagnosticOutChar
  
  ; tombstone = hashEntry;
  lda dHASHENTRYH
  sta IDYH
  lda dHASHENTRYL
  sta IDYL
  bra hashValueTableFindEntryAfterOccupied			
hashValueTableFindEntryOccupied:
  ; hashEntry->isOccupied
  
  ;lda #"f"
  ;jsr diagnosticOutChar
  
  
  ; hashEntry->key == key?
  ldy #0
  lda (dHASHENTRY), Y
  cmp dKEYL
  bne hashValueTableFindEntryAfterOccupied
  iny
  lda (dHASHENTRY), Y
  cmp dKEYH
  bne hashValueTableFindEntryAfterOccupied
  
  ; hashEntry->key == key
  
  ;lda #"g"
  ;jsr diagnosticOutChar
        
  ; return hashEntry
  bra hashValueTableFindEntryExit
        
hashValueTableFindEntryAfterOccupied:

  ;lda #"h"
  ;jsr diagnosticOutChar
		
  ; index = (index + 1) % capacity;
  
  inc IDXL
  bne incIDXEnd2
  inc IDXH
incIDXEnd2:
  
  ; dCAPACITY is always a power of 2 (typically $0010, $0020, $0040 ..)
  lda dCAPACITYL
  sta ACCL
  lda dCAPACITYH
  sta ACCH
  
  ; $000F, $001F, $003F ..
  lda ACCL
  bne hashValueTableFindEntrySkipMSB2
  dec ACCH
hashValueTableFindEntrySkipMSB2:
  dec ACCL
  
  lda IDXH
  and ACCH
  sta IDXH
  lda IDXL
  and ACCL
  sta IDXL
  
  jmp hashValueTableFindEntryNext
  
hashValueTableFindEntryExit:
  
  pla 
  sta IDYH
  pla 
  sta IDYL
  pla 
  sta IDXH
  pla 
  sta IDXL
  
  ;lda #" "
  ;jsr diagnosticOutChar
  ;lda #"E"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRYH
  ;jsr diagnosticOutHex   
  ;lda dHASHENTRYL
  ;jsr diagnosticOutHex   
  
  rts

; dictionary New(type kType, type vType)
syscallDictionaryNew:

  .ifdef STACK8
  
  ; value type -> stack
  ldx SP8
  dex
  dex
  lda HopperValueStack, X
  sta dVTYPE
  
  ; key type -> stack
  dex
  dex
  lda HopperValueStack, X
  sta dKTYPE
  stx SP8
  
  .else
  
  ; value type -> stack
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta dVTYPE
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; key type -> stack
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta dKTYPE
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif
  
  .ifdef CHECKED
  
  ; verify that kType is tString or a value type
  lda dKTYPE
  cmp #tString
  beq syscallDictionaryNewKeyTypeOk ; kType is string
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallDictionaryNewKeyTypeOk ; kType is value type
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?New kType", 0
  .endif
  jmp throwToys
  
syscallDictionaryNewKeyTypeOk
  
  .endif
  
  jsr utilityDictionaryNew
    
  lda #tDictionary
  jmp pushIDXExit
  
utilityDictionaryNew:
  ; dKTYPE, dVTYPE -> IDX
  lda #8
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tDictionary
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ldy #2
  lda dKTYPE
  sta (IDX), Y  ; key type
  iny
  lda dVTYPE
  sta (IDX), Y  ; value type
  iny
  lda #0
  sta (IDX), Y  ; count of entries
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y  ; capacity (number of slots in pEntries block)
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y  ; pEntries (memory block of HashStringEntry or HashUIntEntry entries)
  iny
  sta (IDX), Y
  rts
  
; uint Count { get system; }
syscallDictionaryCountGet:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  ; this -> IDX
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertDictionary
  .endif
  .endif
  
  ldy #4
  lda (IDX), Y       ; LSB of count
  sta TOPL
  iny
  lda (IDX), Y       ; MSB of count
  sta TOPH
  
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  
  lda #tUInt
  jmp pushTOPExit
  
; V Get(<K,V> this, K key) system;
syscallDictionaryGet:

  .ifdef STACK8
  
  ; key -> fKEY
  ldx SP8
  dex
  lda HopperValueStack, X
  sta dKEYH
  dex
  lda HopperValueStack, X
  sta dKEYL
  lda HopperTypeStack, X
  sta dKTYPE
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x4
  sec
  lda SPL
  sbc #4
  sta SPL
  bcs syscallDictionaryGetSkipMSB
  dec SPH
syscallDictionaryGetSkipMSB:
  
  .endif
  
  ; key -> fKEY
  ldy #3
  lda (SP), Y        ; MSB
  sta dKEYH
  dey
  lda (SP), Y        ; LSB
  sta dKEYL
  
  jsr decTSP         ; INLINE
  lda (TSP)
  sta dKTYPE
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  jsr decTSP         ; INLINE
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertDictionary
  .endif
  
  .endif
  
  .ifdef CHECKED
  ; assert that the type of 'key' is the same as kType
  ldy #2
  lda (IDX), Y
  cmp dKTYPE
  beq syscallDictionaryGetKTypeMatches ; the only reference type allowed is tString which must match here
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallDictionaryGetKTypeMatches ; otherwise all value types are ok
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Get kType", 0
  .endif
  jmp throwToys
syscallDictionaryGetKTypeMatches:
  .endif
  
  lda dKTYPE
  cmp #tString
  beq syscallDictionaryGetValueString
  jmp syscallDictionaryGetValue
syscallDictionaryGetValueString:  
  ; kType = tString
  
  ; dictionary is in IDX
  ; key string is in dKEY
  ;   result is returned in fVALUE
  ;   X contains 1 is found, 0 if nullptr
  jsr hashTableGet   ; 1s / 77s = 1%
  txa ; bool result
  bne syscallDictionaryGetFound
  
  stz ACCH
  lda #$03 ; no entry for key in dictionary
  sta ACCL
  jmp utilityDiagnosticsDie
  
syscallDictionaryGetFound:
  
  jsr releaseSPandSPNEXT ; release 'this' and 'key' string
  
syscallDictionaryGetProcessResult:
  ; clone fVALUE
  lda fVALUEL
  sta IDYL
  lda fVALUEH
  sta IDYH
  lda (IDY)
  sta dVTYPE
  
  cmp #tVariant
  bne syscallDictionaryGetCloneReference
  ; variant
  
  ; variant.type
  ldy #2
  lda (IDY), Y
  sta dVTYPE 
  iny
  ; variant.pData
  lda (IDY), Y
  tax
  iny
  lda (IDY), Y
  sta IDYH
  stx IDYL
  
  lda dVTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcs syscallDictionaryGetCloneReference
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  
  jmp syscallDictionaryGetExit
syscallDictionaryGetCloneReference:
  ; reference type
  ; type is in A
  ; reference type to clone is at IDY
  
  lda dVTYPE
  jsr cloneIDY
  
  jmp syscallDictionaryGetExit
  
  
syscallDictionaryGetValue:
  ; kType = value type
  
  ; dictionary is in IDX
  ; key string is in dKEY
  ;   result is returned in fVALUE
  ;   X contains 1 is found, 0 if nullptr
  jsr hashValueTableGet
  
  txa ; bool result
  bne syscallDictionaryGetValueFound
  
  stz ACCH
  lda #$03 ; no entry for key in dictionary
  sta ACCL
  jmp utilityDiagnosticsDie
  
syscallDictionaryGetValueFound:  
  
  .ifdef CHECKED
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  .else
  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
  bne syscallDictionaryGetNoRelease
  jsr memoryFree
syscallDictionaryGetNoRelease:
  .endif
  
  jmp syscallDictionaryGetProcessResult
  
syscallDictionaryGetExit:

  lda dVTYPE
  jmp pushIDXExit
  
; bool Contains(<K,V> this, K key) system;
syscallDictionaryContains:

  .ifdef STACK8
  
  ; key -> dKEY
  ldx SP8
  dex
  lda HopperValueStack, X
  sta dKEYH
  dex
  lda HopperValueStack, X
  sta dKEYL
  lda HopperTypeStack, X
  sta dKTYPE
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x4
  sec
  lda SPL
  sbc #4
  sta SPL
  bcs syscallDictionaryContainsSkipMSB
  dec SPH
syscallDictionaryContainsSkipMSB:
  
  .endif
  
  ; key -> dKEY
  ldy #3
  lda (SP), Y        ; MSB
  sta dKEYH
  dey
  lda (SP), Y        ; LSB
  sta dKEYL
  
  jsr decTSP
  lda (TSP)
  sta dKTYPE
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertDictionary
  .endif
  .endif
  
  .ifdef CHECKED
  ; assert that the type of 'key' is the same as kType
  ldy #2
  lda (IDX), Y
  cmp dKTYPE
  beq syscallDictionaryContainsKTypeMatches ; the only reference type allowed is tString which must match here
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallDictionaryContainsKTypeMatches ; otherwise all value types are ok
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Contains kType", 0
  .endif
  jsr throwToysNoStack
  
syscallDictionaryContainsKTypeMatches:
  .endif
  
  lda dKTYPE
  cmp #tString
  bne syscallDictionaryContainsValue
  ; kType = tString
  
  ; dictionary is in IDX
  ; key string is in dKEY
  ;   result is returned in fVALUE
  ;   X contains 1 is found, 0 if nullptr
  jsr hashTableGet
  phx ; bool result
  
  jsr releaseSPandSPNEXT ; release 'this' and 'key' string
  jmp syscallDictionaryContainsExit
syscallDictionaryContainsValue:
  ; kType = value type
  
  ; dictionary is in IDX
  ; key string is in dKEY
  ;   X contains 1 if found, 0 if nullptr
  ;   X contains 1 if found, 0 if nullptr

  ldy #4
  lda (IDX), Y
  bne syscallDictionaryContainsValueNotZero
  iny
  lda (IDX), Y
  bne syscallDictionaryContainsValueNotZero
  
  ldx #0 ; dictionary is empty
  bra syscallDictionaryContainsNotOccupied
  
syscallDictionaryContainsValueNotZero:

  ldy #6
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH
  
  ; entries in dHASHENTRIES
  ; capacity in dCAPACITY
  ; key string in dKEY
  ;   result 'entry' in dHASHENTRY

  jsr hashValueTableFindEntry ; from hashValueTableGet
  
  ldx #0 ; !IsOccupied
  
  ; hashValueEntry->isOccupied?
  ldy #2
  lda (dHASHENTRY), Y
  beq syscallDictionaryContainsNotOccupied
  
  ldx #1 ; IsOccupied
  
syscallDictionaryContainsNotOccupied:  
  phx ; bool result
  
  .ifdef CHECKED
  jsr rawReleaseSP ; we popped 'this', decrease reference count
  .else
  ; inline version of rawReleaseSP since we know:
  ;    - this is an Array (reference type)
  ;    - it is in IDX (as well as (SP)
  ldy #1
  lda (IDX), Y ; reference count
  dec
  sta (IDX), Y
  ; if zero, free
  bne syscallDictionaryContainsNoRelease
  jsr memoryFree
syscallDictionaryContainsNoRelease:
  .endif
  
syscallDictionaryContainsExit:

  .ifdef STACK8
  
  ldx SP8
  pla ; bool result
  sta HopperValueStack, X
  lda #tBool
  sta HopperTypeStack, X
  inx
  stz HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  
  .else
  
  pla ; bool result
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tBool
  sta (TSP)
  jsr incTSP
  
  .endif
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  jmp nextInstruction

; Set (<K, V> this, K key, V value)
syscallDictionarySet:

  .ifdef STACK8
  
  ; value -> fVALUE
  ldx SP8
  dex
  lda HopperValueStack, X
  sta fVALUEH
  dex
  lda HopperValueStack, X
  sta fVALUEL
  lda HopperTypeStack, X
  sta dVTYPE
  
  ; key -> fKEY
  
  dex
  lda HopperValueStack, X
  sta dKEYH
  dex
  lda HopperValueStack, X
  sta dKEYL
  lda HopperTypeStack, X
  sta dKTYPE
  
  ; this -> IDX
  
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x6
  sec
  lda SPL
  sbc #6
  sta SPL
  bcs syscallDictionarySetSkipMSB
  dec SPH
syscallDictionarySetSkipMSB:
  
  .endif
  
  ; value -> fVALUE
  ldy #5
  lda (SP), Y        ; MSB
  sta fVALUEH
  dey
  lda (SP), Y        ; LSB
  sta fVALUEL
  
  jsr decTSP
  lda (TSP)
  sta dVTYPE
  
  ; key -> fKEY
  ldy #3
  lda (SP), Y        ; MSB
  sta dKEYH
  dey
  lda (SP), Y        ; LSB
  sta dKEYL
  
  jsr decTSP
  lda (TSP)
  sta dKTYPE
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertDictionary
  .endif
  .endif
  
  ; dVTYPE is tricky:
  ; - if the dictionary vType is tVariant, then use the argument vType
  ; - if not, then use the dictionary vType
  ldy #3
  lda (IDX), Y ; dictionary vType
  cmp #tVariant
  beq syscallDictionarySetUseArgumentType  ; use argument vType
  sta dVTYPE                               ; use dictionary vType
syscallDictionarySetUseArgumentType:
  
  
  .ifdef CHECKED
  ; assert that the type of 'key' is the same as kType
  ldy #2
  lda (IDX), Y
  cmp dKTYPE
  beq syscallDictionarySetKTypeMatches ; the only reference type allowed is tString which must match here
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallDictionarySetKTypeMatches ; otherwise all value types are ok
  
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte $0D, "?Set kType", 0
  .endif
  
  jmp throwToys
syscallDictionarySetKTypeMatches:

  .endif

  lda dKTYPE
  cmp #tString
  bne syscallDictionarySetValue

  ; kType = tString
  jsr hashTableAdd
  
  lda dVTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallDictionarySetDontReleaseValue1
  
  ;lda #"R"
  ;jsr diagnosticOutChar
  ;lda #"1"
  ;jsr diagnosticOutChar
  
  jsr releaseSPandSPNEXTandSPNEXTNEXT ; release 'this', 'key' string and 'value'
  
  jmp syscallDictionarySetExit
syscallDictionarySetDontReleaseValue1:
  
  ;lda #"R"
  ;jsr diagnosticOutChar
  ;lda #"2"
  ;jsr diagnosticOutChar
  
  jsr releaseSPandSPNEXT ; release 'this' and 'key' string
  jmp syscallDictionarySetExit
  
syscallDictionarySetValue:
  
  ; kType = value type
  jsr hashValueTableAdd
  
  lda dVTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc syscallDictionarySetDontReleaseValue2
  
  ;lda #"R"
  ;jsr diagnosticOutChar
  ;lda #"3"
  ;jsr diagnosticOutChar
  
  jsr releaseSPandSPNEXTNEXT ; release 'this' and 'value'
  
  jmp syscallDictionarySetExit
syscallDictionarySetDontReleaseValue2:  

  ;lda #"R"
  ;jsr diagnosticOutChar
  ;lda #"4"
  ;jsr diagnosticOutChar
  
  jsr releaseSP ; release 'this'
  
syscallDictionarySetExit:

  jmp nextInstruction
  
checkCapacity:
  ; dictionary is in IDX
  ;  uses TOP and NEXT
  ;  returns #1 in TOP if need more capacity
  ; if (hashTable->count +1 > hashTable->capacity * 0.75)
  ; we do count * 1.25 > capacity
  
  ; NEXT = count * 1.25
  
  ldy #4 ; count
  lda (IDX), Y
  sta NEXTL
  iny
  lda (IDX), Y
  sta NEXTH
  
  ; divide count by 4
  lsr NEXTH
  ror NEXTL
  lsr NEXTH
  ror NEXTL
  
  ; +1
  inc NEXTL
  bne checkCapacityIncEnd
  inc NEXTH
checkCapacityIncEnd:
  
  ; + count
  ldy #4
  clc
  lda (IDX), Y
  adc NEXTL
  sta NEXTL
  iny
  lda (IDX), Y
  adc NEXTH
  sta NEXTH
  iny
  ;ldy #6 ; capacity
  lda (IDX), Y
  sta TOPL
  iny
  lda (IDX), Y
  sta TOPH
  
  ; TOP = (NEXT > TOP)
  jsr utilityUIntGT
  rts
  
calculateDesiredCapacity:
  ; dictionary is in IDX
  ;   result is in dCAPACITY
  
  ; Calculated desired capacity is:
  ; - initially 16 slots
  ; - then 2x existing capacity
  
  ldy #6 ; dictionary->capacity
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  
  lda dCAPACITYL
  bne calculateDesiredCapacityNotZero
  lda dCAPACITYH
  bne calculateDesiredCapacityNotZero
  
  ; minimum capacity = 8 x2 = 16
  lda #8
  sta dCAPACITYL
  stz dCAPACITYH
calculateDesiredCapacityNotZero:
  
  ; capacity *= 2
  asl dCAPACITYL
  rol dCAPACITYH
  
  rts
  
hashTableGet:
  ; dictionary is in IDX
  ; key string is in dKEY
  ;   result is returned in fVALUE
  ;   X contains 1 is found, 0 if nullptr
  
  ; Dictionary memory map:
;   0000 heap allocator size
;   13   type = tDictionary
;   00   GC reference count
;   xx   kType: key type tString or tUint
;   xx   vType: value type
;   xxxx count of entries
;   xxxx capacity (number of slots in block of entries)
;   xxxx pEntries (memory block of HashStringEntry or HashUIntEntry entries)

  ldy #4
  lda (IDX), Y
  bne hashTableGetCountNotZero
  iny
  lda (IDX), Y
  bne hashTableGetCountNotZero
  
  ; empty
  ldx #0
  bra hashTableGetExit
  
hashTableGetCountNotZero:

  ; string to hash in dKEY
  ;   result in dHASH0..dHASH3
  jsr hashKey
  
  ldy #6
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH
  
  ; entries in dHASHENTRIES
  ; capacity in dCAPACITY
  ; key string in dKEY
  ; hash in dHASH0..dHASH3
  ;   result 'entry' in dHASHENTRY

  jsr hashTableFindEntry
  
  ; hashEntry->key == nullptr ?
  ldy #0
  lda (dHASHENTRY), Y
  bne hashTableGetKeyNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashTableGetKeyNotNull
  
  ; hashEntry->key == nullptr

  ldx #0
  bra hashTableGetExit

hashTableGetKeyNotNull:

  ldy #6
  lda (dHASHENTRY), Y
  sta fVALUEL
  iny
  lda (dHASHENTRY), Y
  sta fVALUEH

  ldx #1
hashTableGetExit:  
  rts
  
hashValueTableGet:
  ; dictionary is in IDX
  ; key string is in dKEY
  ;   result is returned in fVALUE
  ;   X contains 1 is found, 0 if nullptr

  ldy #4
  lda (IDX), Y
  bne hashValueTableGetCountNotZero
  iny
  lda (IDX), Y
  bne hashValueTableGetCountNotZero
  
  ; empty
  ldx #0
  bra hashValueTableGetExit
  
hashValueTableGetCountNotZero:

  ldy #6
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH
  
  ; entries in dHASHENTRIES
  ; capacity in dCAPACITY
  ; key string in dKEY
  ;   result 'entry' in dHASHENTRY

  jsr hashValueTableFindEntry ; from hashValueTableGet
  
  ; hashValueEntry->isOccupied?
  ldy #2
  lda (dHASHENTRY), Y
  bne hashValueTableGetKeyIsOccupied
  
  ldx #0
  bra hashValueTableGetExit

hashValueTableGetKeyIsOccupied:

  ldy #6
  lda (dHASHENTRY), Y
  sta fVALUEL
  iny
  lda (dHASHENTRY), Y
  sta fVALUEH

  ldx #1
hashValueTableGetExit:  
  rts
  
hashTableAdd:
  ; dictionary is in IDX
  ; key string is in dKEY
  ; value is in fVALUE
  ; dKTYPE are dVTYPE set already 
  
  lda IDYH
  pha
  lda IDYL
  pha
  
  jsr checkCapacity
  
  lda TOPL
  beq hashTableAddCapacityDone
  
  jsr calculateDesiredCapacity
  
  jsr hashTableAdjustCapacity
  
hashTableAddCapacityDone:


  ; string to hash in dKEY
  ;   result in dHASH0..dHASH3
  jsr hashKey
  
  ldy #6
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH

  ; entries in dHASHENTRIES
  ; capacity in dCAPACITY
  ; key string in dKEY
  ; hash in dHASH0..dHASH3
  ;   result 'entry' in dHASHENTRY

  ;HashEntry * entry = findEntry(hashTable->entries, hashTable->capacity, key, hash);
  jsr hashTableFindEntry
  
  ; hashEntry->key == nullptr ?
  ldy #0
  lda (dHASHENTRY), Y
  bne hashTableAddEntryKeyNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashTableAddEntryKeyNotNull
  
  ; hashEntry->key == nullptr
  
  ; entry->value == nullptr ?
  ldy #6
  lda (dHASHENTRY), Y
  bne hashTableAddEntryKeyNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashTableAddEntryKeyNotNull
  
  ; entry->value == nullptr
  
  ; hashTable->count++;
  ldy #4
  clc
  lda (IDX), Y
  adc #1
  sta (IDX), Y
  iny
  lda (IDX), Y
  adc #0
  sta (IDX), Y
  
  bra hashTableAddEntryAfterKeyFree
hashTableAddEntryKeyNotNull:

  .ifdef MEMDEBUG2
  
  lda #"r"
  jsr diagnosticOutChar
  lda #"k"
  jsr diagnosticOutChar
  
  .endif
  
  ; munt IDX (dictionary) here:
  ldy #0
  lda (dHASHENTRY), Y
  sta IDXL
  iny
  lda (dHASHENTRY), Y
  sta IDXH
  jsr gcRelease
hashTableAddEntryAfterKeyFree:

  ;entry->key = clone(key)
  lda dKEYL
  sta IDYL
  lda dKEYH
  sta IDYH
  jsr cloneString
  ldy #0
  lda IDXL
  sta (dHASHENTRY), Y
  iny
  lda IDXH
  sta (dHASHENTRY), Y
  iny
  
  ;entry->hash = hash;
  lda dHASH0
  sta (dHASHENTRY), Y
  iny
  lda dHASH1
  sta (dHASHENTRY), Y
  iny
  lda dHASH2
  sta (dHASHENTRY), Y
  iny
  lda dHASH3
  sta (dHASHENTRY), Y
  
  ldy #6
  lda (dHASHENTRY), Y
  bne hashTableAddEntryValueNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashTableAddEntryValueNotNull
  
  bra hashTableAddEntryValueAfterFree

hashTableAddEntryValueNotNull:

  .ifdef MEMDEBUG2
  
  lda #"r"
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  
  .endif
  
  ldy #6
  lda (dHASHENTRY), Y
  sta IDXL
  iny
  lda (dHASHENTRY), Y
  sta IDXH
  jsr gcRelease

hashTableAddEntryValueAfterFree:

  ;entry->value = clone(value);
  
  lda dVTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc hashTableAddEntryValueType
  
  lda fVALUEL
  sta IDYL
  lda fVALUEH
  sta IDYH
  
  ; reference type
  ; type is in A
  ; reference type to clone is at IDY
  lda dVTYPE
  jsr cloneIDY
  
  bra hashTableAddEntryStoreValue  
hashTableAddEntryValueType:

  ; type in A
  ; value in fVALUE
  ; uses fSIZE
  ; return tVariant in IDX
  lda dVTYPE
  jsr createValueVariant
  
hashTableAddEntryStoreValue:  

  ldy #6
  lda IDXL
  sta (dHASHENTRY), Y
  iny
  lda IDXH
  sta (dHASHENTRY), Y
  
  pla
  sta IDYL
  pla
  lda IDYH
  
  rts
  
  
hashValueTableAdd:
  ; dictionary is in IDX
  ; key value is in fKEY
  ; value is in fVALUEH
  ; dKTYPE are dVTYPE set already
  
  lda IDYH
  pha
  lda IDYL
  pha
  
  jsr checkCapacity
  
  ;jsr diagnosticOutNewLine
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda TOPL
  ;jsr diagnosticOutHex   
  
  lda TOPL
  beq hashValueTableAddCapacityDone
  
  jsr calculateDesiredCapacity
  
  ;jsr diagnosticOutNewLine
  ;lda #"D"
  ;jsr diagnosticOutChar
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda dCAPACITYH
  ;jsr diagnosticOutHex   
  ;lda dCAPACITYL
  ;jsr diagnosticOutHex   
  
  jsr hashValueTableAdjustCapacity

hashValueTableAddCapacityDone:

  ldy #6
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH

  ; entries in dHASHENTRIES
  ; newCapacity in dCAPACITY
  ; key value in dKEY
  ;   result 'newEntry' in dHASHENTRY
  jsr hashValueTableFindEntry ; from hashValueTableAdd
  
  ; entry->value == nullptr ?
  ldy #6
  lda (dHASHENTRY), Y
  bne hashValueTableAddEntryValueNotNull
  iny
  lda (dHASHENTRY), Y
  bne hashValueTableAddEntryValueNotNull
  
  ; entry->value == nullptr
  
  ldy #2
  lda (dHASHENTRY), Y
  bne hashValueTableAddEntryValueAfterNotNull ; hashValueEntry->isOccupied
  
  ; !entry->isOccupied && (entry->value == nullptr)

  ;lda "D"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex
  ;lda IDXL
  ;jsr diagnosticOutHex
  
  ;	hashValueTable->count++;
  ldy #4
  clc
  lda (IDX), Y
  adc #1
  sta (IDX), Y
  iny
  lda (IDX), Y
  adc #0
  sta (IDX), Y
  
  bra hashValueTableAddEntryValueAfterNotNull
hashValueTableAddEntryValueNotNull:
  ; entry->value != nullptr
  
  ;lda #"3"
  ;jsr diagnosticOutChar
  
  ldy #2
  lda (dHASHENTRY), Y
  beq hashValueTableAddEntryValueAfterNotNull ; !hashValueEntry->isOccupied
  
  .ifdef MEMDEBUG2
  
  lda #"r"
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  
  .endif
  
  ldy #6
  lda (dHASHENTRY), Y
  sta IDXL
  iny
  lda (dHASHENTRY), Y
  sta IDXH
  jsr gcRelease
  
hashValueTableAddEntryValueAfterNotNull:

  ; newEntry->key = key;
  ldy #0
  lda dKEYL
  sta (dHASHENTRY), Y
  iny
  lda dKEYH
  sta (dHASHENTRY), Y
  iny
  ; newEntry->isOccupied = true;
  lda #1
  sta (dHASHENTRY), Y
  iny
  lda #0
  sta (dHASHENTRY), Y
  iny
  sta (dHASHENTRY), Y
  iny
  sta (dHASHENTRY), Y
  
  ;newEntry->value = clone(value);
  
  lda dVTYPE
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc hashValueTableAddEntryValueType
  
  lda fVALUEL
  sta IDYL
  lda fVALUEH
  sta IDYH
  
  ; reference type
  ; type is in A
  ; reference type to clone is at IDY
  lda dVTYPE
  jsr cloneIDY
  
  bra hashValueTableAddEntryStoreValue  
hashValueTableAddEntryValueType:

  ; type in A
  ; value in fVALUE
  ; uses fSIZE
  ; return tVariant in IDX
  lda dVTYPE
  jsr createValueVariant
  
hashValueTableAddEntryStoreValue:  

  ldy #6
  lda IDXL
  sta (dHASHENTRY), Y
  iny
  lda IDXH
  sta (dHASHENTRY), Y
  
  pla
  sta IDYL
  pla
  lda IDYH
  
  rts
  
freeHashTableEntries:
  ; dictionary is in IDX
  lda IDXH
  pha
  lda IDXL
  pha
  
  ; dictionary->pEntries
  ldy #8
  lda (IDX), Y
  tax
  iny
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  jsr memoryFree

  pla
  sta IDXL
  pla
  sta IDXH
  rts
  
  
hashTableAdjustCapacity:
  ; dictionary is in IDX
  ; desired newCapacity is in dCAPACITY
  
  ;jsr diagnosticOutNewLine
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda #"H"
  ;jsr diagnosticOutChar
  
  
  lda dKEYL
  pha
  lda dKEYH
  pha

  ; sizeRequired = sizeof(HashEntry) * newCapacity;  
  ; 8 byte entries : ACC = dCAPACITYL << 3
  lda dCAPACITYL
  sta ACCL
  lda dCAPACITYH
  sta ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  
  ;jsr diagnosticOutNewLine
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda ACCH
  ;jsr diagnosticOutHex   
  ;lda ACCL
  ;jsr diagnosticOutHex
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; sizeRequired in ACC
  ; new 'entries' block in IDX
  jsr memoryAllocate
  
  lda IDXL
  sta dHASHENTRIESL
  lda IDXH
  sta dHASHENTRIESH
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  ; hashTable->count = 0;
  ldy #4
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  ; dictionary->capacity
  ldy #6
  lda (IDX), Y  
  sta IDYL
  iny
  lda (IDX), Y
  sta IDYH
  
  ; dictionary->pEntries
  ldy #8
  lda (IDX), Y
  sta fSOURCEADDRESSL
  iny
  lda (IDX), Y
  sta fSOURCEADDRESSH
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  stz lCOUNTL
  stz lCOUNTH
  
hashTableAdjustCapacityNextEntry:
  lda IDYL
  bne hashTableAdjustCapacityNextEntryDo
  lda IDYH
  bne hashTableAdjustCapacityNextEntryDo
  bra hashTableAdjustCapacityDoneCopying
hashTableAdjustCapacityNextEntryDo:
  jsr decIDY
  
  ; fSOURCEADDRESSL = hashEntry

  ; hashEntry->key == nullptr?
  lda (fSOURCEADDRESS)
  bne hashTableAdjustCapacityNextEntryDoCopy
  ldy #1
  lda (fSOURCEADDRESS), Y
  bne hashTableAdjustCapacityNextEntryDoCopy
  bra hashTableAdjustCapacityNextEntrySkipCopy
hashTableAdjustCapacityNextEntryDoCopy:
  
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta dKEYL
  iny
  lda (fSOURCEADDRESS), Y
  sta dKEYH
  iny
  lda (fSOURCEADDRESS), Y
  sta dHASH0
  iny
  lda (fSOURCEADDRESS), Y
  sta dHASH1
  iny
  lda (fSOURCEADDRESS), Y
  sta dHASH2
  iny
  lda (fSOURCEADDRESS), Y
  sta dHASH3
  
  ; newEntry = findEntry(entries, newCapacity, hashEntry->key, hashEntry->hash);
  ; entries in dHASHENTRIES
  ; newCapacity in dCAPACITY
  ; key string in dKEY
  ; hash in dHASH0..dHASH3
  ;   result 'newEntry' in dHASHENTRY
  jsr hashTableFindEntry
  
  ; newEntry->key = hashEntry->key;
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  ; newEntry->hash = hashEntry->hash;
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  ; newEntry->value = hashEntry->value;
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  
  ; hashTable->count++;
  jsr incCOUNT
hashTableAdjustCapacityNextEntrySkipCopy:
  clc
  lda fSOURCEADDRESSL
  adc #8
  sta fSOURCEADDRESSL
  lda fSOURCEADDRESSH
  adc #0
  sta fSOURCEADDRESSH
  
  bra hashTableAdjustCapacityNextEntry
hashTableAdjustCapacityDoneCopying:

  pla
  sta IDXL
  pla
  sta IDXH
  
  ; if (nullptr != hashTable->entries)
  ldy #8
  lda (IDX), Y
  bne freePreviousEntries
  iny
  lda (IDX), Y
  bne freePreviousEntries
  bra afterFreePreviousEntries
freePreviousEntries:
  jsr freeHashTableEntries
afterFreePreviousEntries:

  ; hashTable->count = count
  ldy #4
  lda lCOUNTL
  sta (IDX), Y  
  iny
  lda lCOUNTH
  sta (IDX), Y
  iny
  
  ; hashTable->capacity = newCapacity;
  lda dCAPACITYL
  sta (IDX), Y  
  iny
  lda dCAPACITYH
  sta (IDX), Y
  iny
  
  ; hashTable->entries = entries;
  lda dHASHENTRIESL
  sta (IDX), Y
  iny
  lda dHASHENTRIESH
  sta (IDX), Y
  
  pla
  sta dKEYH
  pla
  sta dKEYL
  
  ;lda #"."
  ;jsr diagnosticOutChar
  
  rts
  
hashValueTableAdjustCapacity:
  ; dictionary is in IDX
  ; desired newCapacity is in dCAPACITY
  
  lda dKEYL
  pha
  lda dKEYH
  pha

  ;jsr diagnosticOutNewLine
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda #"V"
  ;jsr diagnosticOutChar
  ;lda dCAPACITYH
  ;jsr diagnosticOutHex   
  ;lda dCAPACITYL
  ;jsr diagnosticOutHex
  
  ; sizeRequired = sizeof(HashEntry) * newCapacity;  
  ; 8 byte entries : ACC = dCAPACITYL << 3
  lda dCAPACITYL
  sta ACCL
  lda dCAPACITYH
  sta ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda ACCH
  ;jsr diagnosticOutHex   
  ;lda ACCL
  ;jsr diagnosticOutHex
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; sizeRequired in ACC
  ; new 'entries' block in IDX
  jsr memoryAllocate
  
  lda IDXL
  sta dHASHENTRIESL
  lda IDXH
  sta dHASHENTRIESH
  
  pla
  sta IDXH
  pla
  sta IDXL
  ; dictionary is in IDX
  
  ; hashTable->count = 0;
  ldy #4
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  ; dictionary->capacity (old)
  ldy #6
  lda (IDX), Y  
  sta IDYL
  iny
  lda (IDX), Y
  sta IDYH
  
  ; dictionary->pEntries (old)
  ldy #8
  lda (IDX), Y
  sta fSOURCEADDRESSL
  iny
  lda (IDX), Y
  sta fSOURCEADDRESSH
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  stz lCOUNTL
  stz lCOUNTH
  
hashValueTableAdjustCapacityNextEntry:
  lda IDYL
  bne hashValueTableAdjustCapacityNextEntryDo
  lda IDYH
  bne hashValueTableAdjustCapacityNextEntryDo
  bra hashValueTableAdjustCapacityDoneCopying
hashValueTableAdjustCapacityNextEntryDo:
  jsr decIDY
  
  ; fSOURCEADDRESS = hashValueEntry
  
  ldy #2
  lda (fSOURCEADDRESS), Y
  beq hashValueTableAdjustCapacityNextEntrySkipCopy ; !hashValueEntry->isOccupied
  
  ; hashValueEntry->key (old)
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta dKEYL
  iny
  lda (fSOURCEADDRESS), Y
  sta dKEYH
  
  ;jsr diagnosticOutNewLine
  ;lda #"K"
  ;jsr diagnosticOutChar
  ;lda dKEYH
  ;jsr diagnosticOutHex   
  ;lda dKEYL
  ;jsr diagnosticOutHex

  ; entries in dHASHENTRIES
  ; newCapacity in dCAPACITY
  ; key value in dKEY
  ;   result 'newEntry' in dHASHENTRY
  jsr hashValueTableFindEntry; from hashValueTableAdjustCapacity

  ; newEntry->key = hashValueEntry->key;
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  ; newEntry->isOccupied = true;
  lda #1
  sta (dHASHENTRY), Y
  iny
  lda #0
  sta (dHASHENTRY), Y
  iny
  sta (dHASHENTRY), Y
  iny
  sta (dHASHENTRY), Y
  iny
  ; newEntry->value = hashValueEntry->value;
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRY), Y

  ; hashValueTable->count++;
  jsr incCOUNT
hashValueTableAdjustCapacityNextEntrySkipCopy:
  clc
  lda fSOURCEADDRESSL
  adc #8
  sta fSOURCEADDRESSL
  lda fSOURCEADDRESSH
  adc #0
  sta fSOURCEADDRESSH
  
  bra hashValueTableAdjustCapacityNextEntry
hashValueTableAdjustCapacityDoneCopying:

  pla
  sta IDXL
  pla
  sta IDXH
  
  ; if (nullptr != hashTable->entries)
  ldy #8
  lda (IDX), Y
  bne freePreviousEntries2
  iny
  lda (IDX), Y
  bne freePreviousEntries2
  bra afterFreePreviousEntries2
freePreviousEntries2:
  jsr freeHashTableEntries
afterFreePreviousEntries2:

  ; hashTable->count = count
  ldy #4
  lda lCOUNTL
  sta (IDX), Y  
  iny
  lda lCOUNTH
  sta (IDX), Y
  iny
  
  ; hashTable->capacity = newCapacity;
  lda dCAPACITYL
  sta (IDX), Y  
  iny
  lda dCAPACITYH
  sta (IDX), Y
  iny
  
  ; hashTable->entries = entries;
  lda dHASHENTRIESL
  sta (IDX), Y
  iny
  lda dHASHENTRIESH
  sta (IDX), Y
  
  pla
  sta dKEYH
  pla
  sta dKEYL
  
  ;lda #"."
  ;jsr diagnosticOutChar
  
  rts
  
syscallDictionaryClear:

  .ifdef STACK8
  
  ; this -> IDX
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  stx SP8
  
  .else
  
  ; this -> IDX
  jsr decSP
  lda (SP)
  sta IDXH
  jsr decSP
  lda (SP)
  sta IDXL
  
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertDictionary
  .endif
  .endif
  
  jsr utilityDictionaryClear
  
  jsr releaseSP ; release 'this'
  
  jmp nextInstruction
  
syscallDictionaryNext:

  .ifdef STACK8
  
  ; iterator
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta dITERATORH
  dex
  lda HopperValueStack, X
  sta dITERATORL
  
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  
  stx SP8
  .else
  
  .ifdef CHECKED
  
  jsr decSP
  jsr decSP
  jsr decSP
  jsr decSP
  
  .else
  
  ; decSP x4
  sec
  lda SPL
  sbc #4
  sta SPL
  bcs syscallDictionaryNextSkipMSB
  dec SPH
syscallDictionaryNextSkipMSB:
  
  .endif
  
  ; iterator
  ldy #3
  lda (SP), Y        ; MSB
  sta dITERATORH
  dey
  lda (SP), Y        ; LSB
  sta dITERATORL
  
  jsr decTSP
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; this -> IDX
  dey
  lda (SP), Y        ; MSB
  sta IDXH
  dey
  lda (SP), Y        ; LSB
  sta IDXL
  
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertDictionary
  .endif
  .endif
  
  ldy #2
  lda (IDX), Y
  sta dKTYPE
  iny
  lda (IDX), Y
  sta dVTYPE
  iny
  lda (IDX), Y
  sta lCOUNTL
  iny
  lda (IDX), Y
  sta lCOUNTH
  iny
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH

  ;Pair* pair = Pair_New(_TYPE_(dictionary->kType), _TYPE_(dictionary->vType));  
  jsr utilityPairNew
  
  jsr dictionaryNext
  
  jsr releaseSP ; release 'this'


  .ifdef STACK8
  
  ; found?
  ldx SP8
  lda ACCL
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  inx
  lda ACCH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  
  ; pair
  lda IDXL
  sta HopperValueStack, X
  lda #tPair
  sta HopperTypeStack, X
  inx
  lda IDXH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  
  ; iterator
  lda dITERATORL
  sta HopperValueStack, X
  lda #tUInt
  sta HopperTypeStack, X
  inx
  lda dITERATORH
  sta HopperValueStack, X
  .ifdef CHECKED
  lda #$AA
  sta HopperTypeStack, X ; error marker
  .endif
  inx
  stx SP8
  
  .else
  
  ; found?
  lda ACCL ; found?
  sta (SP)
  jsr incSP          ; LSB
  lda ACCH
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP

  ; pair
  lda IDXL
  sta (SP)
  jsr incSP          ; LSB
  lda IDXH
  sta (SP)
  jsr incSP          ; MSB
  lda #tPair
  sta (TSP)
  jsr incTSP
  
  ; iterator
  lda dITERATORL
  sta (SP)
  jsr incSP          ; LSB
  lda dITERATORH
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  
  .endif
  
  jmp nextInstruction
  
dictionaryNext:
  ; 'count' is in lCOUNT
  ; 'entries' in dHASHENTRIES
  ; 'capacity' in dCAPACITY
  ; pair in IDX
  ; interator in dITERATOR
  ;   return 'found' in ACC
  
  .ifdef MEMDEBUG2
  
  jsr diagnosticOutNewLine
  lda #"D"
  jsr diagnosticOutChar
  lda #"N"
  jsr diagnosticOutChar
  lda (IDX)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  sta uCURRENTH
  jsr diagnosticOutHex
  lda IDXL
  sta uCURRENTL
  jsr diagnosticOutHex
  jsr decCURRENT
  jsr decCURRENT
  jsr memoryHeapWalkPair
  
  .endif
  
  stz ACCL
  stz ACCH
  
  ; count == ?
  lda lCOUNTL
  bne dictionaryNextNotEmpty
  lda lCOUNTH
  bne dictionaryNextNotEmpty
  jmp dictionaryNextExit
  
dictionaryNextNotEmpty:
  lda dITERATORL
  cmp #$FF
  bne dictionaryNextLoop
  lda dITERATORH
  cmp #$FF
  bne dictionaryNextLoop
  jmp dictionaryNextExit
  
dictionaryNextLoop:
  ; dHASHENTRY = dHASHENTRIES + (dITERATOR << 3)
  
  lda dITERATORL
  sta dHASHENTRYL
  lda dITERATORH
  sta dHASHENTRYH
  
  ; dHASHENTRY = dHASHENTRY << 3
  asl dHASHENTRYL
  rol dHASHENTRYH
  asl dHASHENTRYL
  rol dHASHENTRYH
  asl dHASHENTRYL
  rol dHASHENTRYH
  
  clc
  lda dHASHENTRIESL
  adc dHASHENTRYL
  sta dHASHENTRYL
  lda dHASHENTRIESH
  adc dHASHENTRYH
  sta dHASHENTRYH
  
  inc dITERATORL
  bne incDictionaryNextEnd
  inc dITERATORH
incDictionaryNextEnd:
  
  ; dITERATOR++
  lda dITERATORL
  cmp dCAPACITYL
  bne incDictionaryNextNotEqual
  lda dITERATORH
  cmp dCAPACITYH
  bne incDictionaryNextNotEqual
  stz dITERATORL
  stz dITERATORH
incDictionaryNextNotEqual:
  
  lda dKTYPE
  cmp #tString
  bne dictionaryNextNotStringKey
  
  ; hashEntry->key == nullptr ?
  ldy #0
  lda (dHASHENTRY), Y
  bne dictionaryNextFound
  iny
  lda (dHASHENTRY), Y
  bne dictionaryNextFound
  ; hashEntry->key == nullptr
  
  ; empty entry or tombstone
  jmp dictionaryNextCheckZero
  
dictionaryNextNotStringKey:
  ; !hashValueEntry->isOccupied?
  ldy #2
  lda (dHASHENTRY), Y
  bne dictionaryNextFound
  jmp dictionaryNextCheckZero

dictionaryNextFound:
  
  lda dITERATORL
  bne dictionaryNextFoundNotDone
  lda dITERATORH
  bne dictionaryNextFoundNotDone
  lda #$FF
  sta dITERATORL
  sta dITERATORH
  
dictionaryNextFoundNotDone

  lda dKTYPE
  cmp #tString
  bne dictionaryNextKeyNotString
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ldy #0
  lda (dHASHENTRY), Y
  sta IDYL
  iny
  lda (dHASHENTRY), Y
  sta IDYH
  
  jsr cloneString  ; IDY -> IDX
  
  bra dictionaryNextProcessResult
dictionaryNextKeyNotString:

  lda IDXL
  pha
  lda IDXH
  pha
  
  ldy #0
  lda (dHASHENTRY), Y
  sta fVALUEL
  iny
  lda (dHASHENTRY), Y
  sta fVALUEH
  
  lda dKTYPE
  jsr createValueVariant
  ; type in A
  ; value in fVALUE
  ; uses fSIZE
  ;   return tVariant in IDX
  
dictionaryNextProcessResult:

  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  .ifdef MEMDEBUG2
  lda #"["
  jsr diagnosticOutChar
  lda (IDX)
  jsr diagnosticOutHex
  lda #":"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  .endif
  
  jsr utilityPairClear
  
  .ifdef MEMDEBUG2
  lda #"]"
  jsr diagnosticOutChar
  .endif
  
  ; pair->key = IDYL
  ldy #2
  lda dKTYPE
  sta (IDX), Y
  ldy #4
  lda IDYL
  sta (IDX), Y
  iny
  lda IDYH
  sta (IDX), Y
  
  ldy #6
  lda (dHASHENTRY), Y
  sta IDYL
  iny
  lda (dHASHENTRY), Y
  sta IDYH
  
  lda (IDY)
  sta dVTYPE
  
  .ifdef MEMDEBUG2
  lda #"T"
  jsr diagnosticOutChar
  lda dVTYPE
  jsr diagnosticOutHex
  .endif
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; type is in A
  ; reference type to clone is at IDY
  lda dVTYPE
  jsr cloneIDY ; IDY -> IDX
  
  
  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  ; pair->Value = IDY
  ldy #3
  lda dVTYPE
  sta (IDX), Y
  ldy #6
  lda IDYL
  sta (IDX), Y
  iny
  lda IDYH
  sta (IDX), Y
  
  lda #1
  sta ACCL
  stz ACCH
  bra dictionaryNextExit
    
dictionaryNextCheckZero:
  lda dITERATORL
  bne dictionaryNextLoopJmp
  lda dITERATORH
  bne dictionaryNextLoopJmp
  bra dictionaryNextExit
  
dictionaryNextLoopJmp:
  jmp dictionaryNextLoop
  
dictionaryNextExit:
  rts
  
utilityDictionaryClear:
  ; dictionary is in IDX
  
  ; utilityDictionaryClear can go recursive: 
  lda dITERATORH
  pha
  lda dITERATORL
  pha
  lda dCAPACITYH
  pha
  lda dCAPACITYL
  pha
  lda dHASHENTRIESH
  pha
  lda dHASHENTRIESL
  pha
  lda dHASHENTRYH
  pha
  lda dHASHENTRYL
  pha
  lda dKTYPE
  pha
  lda dVTYPE
  pha
  
  ldy #2
  lda (IDX), Y
  sta dKTYPE
  iny
  lda (IDX), Y
  sta dVTYPE
  iny
  lda (IDX), Y
  sta lCOUNTL
  iny
  lda (IDX), Y
  sta lCOUNTH
  iny
  lda (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDX), Y
  sta dHASHENTRIESL
  iny
  lda (IDX), Y
  sta dHASHENTRIESH
  
  stz dITERATORL
  stz dITERATORH
  
  ; save dictionary
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; count == ?
  lda lCOUNTL
  bne utilityDictionaryClearLoop
  lda lCOUNTH
  bne utilityDictionaryClearLoop
  jmp utilityDictionaryClearExitNoFree
  
  
utilityDictionaryClearLoop:
  ; dHASHENTRY = dHASHENTRIES + (dITERATOR << 3)
  
  lda dITERATORL
  sta dHASHENTRYL
  lda dITERATORH
  sta dHASHENTRYH
  
  ; dHASHENTRY = dHASHENTRY << 3
  asl dHASHENTRYL
  rol dHASHENTRYH
  asl dHASHENTRYL
  rol dHASHENTRYH
  asl dHASHENTRYL
  rol dHASHENTRYH
  
  clc
  lda dHASHENTRIESL
  adc dHASHENTRYL
  sta dHASHENTRYL
  lda dHASHENTRIESH
  adc dHASHENTRYH
  sta dHASHENTRYH
  
  inc dITERATORL
  bne incUtilityDictionaryClearEnd
  inc dITERATORH
incUtilityDictionaryClearEnd:
  
  ; dITERATOR++
  lda dITERATORL
  cmp dCAPACITYL
  bne incUtilityDictionaryClearNotEqual
  lda dITERATORH
  cmp dCAPACITYH
  bne incUtilityDictionaryClearNotEqual
  
  ; loop exit condition below
  stz dITERATORL
  stz dITERATORH
incUtilityDictionaryClearNotEqual:
  
  lda dKTYPE
  cmp #tString
  bne utilityDictionaryClearNotStringKey
  
  ; hashEntry->key == nullptr ?
  ldy #0
  lda (dHASHENTRY), Y
  bne utilityDictionaryClearFound
  iny
  lda (dHASHENTRY), Y
  bne utilityDictionaryClearFound
  ; hashEntry->key == nullptr
  
  ; empty entry or tombstone
  jmp utilityDictionaryClearCheckZero
  
utilityDictionaryClearNotStringKey:
  ; !hashValueEntry->isOccupied?
  ldy #2
  lda (dHASHENTRY), Y
  bne utilityDictionaryClearFound
  jmp utilityDictionaryClearCheckZero

utilityDictionaryClearFound:
  
  lda dKTYPE
  cmp #tString
  bne utilityDictionaryClearKeyNotString
  
  
  ldy #0
  lda (dHASHENTRY), Y
  sta IDXL
  iny
  lda (dHASHENTRY), Y
  sta IDXH
  
  .ifdef MEMDEBUG2
  
  lda #"c"
  jsr diagnosticOutChar
  lda #"k"
  jsr diagnosticOutChar
  
  .endif
  
  jsr gcRelease ; IDX
  
utilityDictionaryClearKeyNotString:

  ; release the value
  ldy #6
  lda (dHASHENTRY), Y
  sta IDXL
  iny
  lda (dHASHENTRY), Y
  sta IDXH
  
  .ifdef MEMDEBUG2
  
  lda #"c"
  jsr diagnosticOutChar
  lda #"v"
  jsr diagnosticOutChar
  
  .endif
  
  jsr gcRelease ; IDX
  
  
utilityDictionaryClearCheckZero:
  lda dITERATORL
  bne utilityDictionaryClearLoopJmp
  lda dITERATORH
  bne utilityDictionaryClearLoopJmp
  
  lda dHASHENTRIESL
  sta IDXL
  lda dHASHENTRIESH
  sta IDXH
  
  jsr memoryFree
utilityDictionaryClearExitNoFree:
  
  ; restore dictionary
  pla
  sta IDXH
  pla
  sta IDXL
  
  ldy #4
  lda #0
  sta (IDX), Y  ; count of entries
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y  ; capacity (number of slots in pEntries block)
  iny
  sta (IDX), Y
  iny
  sta (IDX), Y  ; pEntries (memory block of HashStringEntry or HashUIntEntry entries)
  iny
  sta (IDX), Y
  
  pla
  sta dVTYPE
  pla
  sta dKTYPE
  pla
  sta dHASHENTRYL
  pla
  sta dHASHENTRYH
  pla
  sta dHASHENTRIESL
  pla
  sta dHASHENTRIESH
  pla
  sta dCAPACITYL
  pla
  sta dCAPACITYH
  pla
  sta dITERATORL
  pla
  sta dITERATORH
  
  rts
  
utilityDictionaryClearLoopJmp:
  jmp utilityDictionaryClearLoop
  
cloneDictionary:
  
  ; cloneDictionary can go recursive: 
  lda fSOURCEADDRESSH
  pha
  lda fSOURCEADDRESSL
  pha
  lda dCAPACITYH
  pha
  lda dCAPACITYL
  pha
  lda dHASHENTRIESH
  pha
  lda dHASHENTRIESL
  pha
  lda dKTYPE
  pha
  lda dVTYPE
  pha
  
  ; dictionary to clone is at IDY
  ;   result is in IDX
  ldy #2
  lda (IDY), Y
  sta dKTYPE
  iny
  lda (IDY), Y
  sta dVTYPE
  iny
  jsr utilityDictionaryNew ; dKTYPE, dVTYPE -> IDX
  
  ldy #2
  lda (IDY), Y ; dKTYPE
  sta (IDX), Y
  iny
  lda (IDY), Y ; dVTYPE
  sta (IDX), Y
  iny
  lda (IDY), Y ; lCOUNT
  sta (IDX), Y
  iny
  lda (IDY), Y
  sta (IDX), Y
  iny
  lda (IDY), Y ; dCAPACITY
  sta (IDX), Y
  sta dCAPACITYL
  iny
  lda (IDY), Y 
  sta (IDX), Y
  sta dCAPACITYH
  iny
  lda (IDY), Y ; dHASHENTRIES
  sta fSOURCEADDRESSL
  iny
  lda (IDY), Y
  sta fSOURCEADDRESSH
  
  lda dCAPACITYL
  sta ACCL
  lda dCAPACITYH
  sta ACCH
  
  ; ACC = ACC << 3
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  asl ACCL
  rol ACCH
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; sizeRequired in ACC
  ; new 'entries' block in IDX
  jsr memoryAllocate
  
  lda IDXL
  sta dHASHENTRIESL
  lda IDXH
  sta dHASHENTRIESH
  
  pla
  sta IDXH
  pla
  sta IDXL
  
  ldy #8
  lda dHASHENTRIESL
  sta (IDX), Y
  iny
  lda dHASHENTRIESH
  sta (IDX), Y
  
  lda IDXL
  pha
  lda IDXH
  pha
  
  
  ; existing entries at fSOURCEADDRESS
  ; new entries      at dHASHENTRIES
  
  ;jsr diagnosticOutNewLine
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda fSOURCEADDRESSH
  ;jsr diagnosticOutHex
  ;lda fSOURCEADDRESSL
  ;jsr diagnosticOutHex
  ;lda #"D"
  ;jsr diagnosticOutChar
  ;lda dHASHENTRIESH
  ;jsr diagnosticOutHex
  ;lda dHASHENTRIESL
  ;jsr diagnosticOutHex
  
cloneDictionaryNext:
  lda dCAPACITYL
  bne cloneDictionaryNextNotZero
  lda dCAPACITYH
  bne cloneDictionaryNextNotZero
  jmp cloneDictionaryDone
cloneDictionaryNextNotZero:

  lda dKTYPE
  cmp #tString
  beq cloneDictionaryStringKeys
  ; value keys : check isOccupied
  ldy #2
  lda (fSOURCEADDRESS), Y
  beq cloneDictionarySkipEntry
  
  ; copy the key and isOccupied
  ldy #0
  lda (fSOURCEADDRESS), Y ; key LSB
  sta (dHASHENTRIES), Y
  iny
  lda (fSOURCEADDRESS), Y ; key MSB
  sta (dHASHENTRIES), Y
  iny
  lda (fSOURCEADDRESS), Y ; isOccupied
  sta (dHASHENTRIES), Y
  lda #0
  iny
  sta (dHASHENTRIES), Y
  iny
  sta (dHASHENTRIES), Y
  iny
  sta (dHASHENTRIES), Y
  bra cloneDictionaryCloneData  
  
cloneDictionaryStringKeys:
  ; string keys
  ldy #0
  lda (fSOURCEADDRESS), Y
  bne cloneDictionaryString
  iny
  lda (fSOURCEADDRESS), Y
  bne cloneDictionaryString
  bra cloneDictionarySkipEntry
cloneDictionaryString:
  ; key != nullptr
  
  ; clone the key string
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta IDYL
  iny
  lda (fSOURCEADDRESS), Y
  sta IDYH
  jsr cloneString  ; IDY -> IDX (preserves fSOURCEADDRESS)
  
  ldy #0
  lda IDXL
  sta (dHASHENTRIES), Y
  iny
  lda IDXH
  sta (dHASHENTRIES), Y
  iny
  ; copy the hash
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRIES), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRIES), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRIES), Y
  iny
  lda (fSOURCEADDRESS), Y
  sta (dHASHENTRIES), Y
  
cloneDictionaryCloneData:
  ldy #6
  lda (fSOURCEADDRESS), Y
  sta IDYL
  iny
  lda (fSOURCEADDRESS), Y
  sta IDYH
  lda (IDY) ; type in A
  jsr cloneIDY  ; IDY -> IDX
  
  ldy #6
  lda IDXL
  sta (dHASHENTRIES), Y
  iny
  lda IDXH
  sta (dHASHENTRIES), Y

cloneDictionarySkipEntry:  

  ; dHASHENTRIES += 8
  clc
  lda dHASHENTRIESL
  adc #8
  sta dHASHENTRIESL
  lda dHASHENTRIESH
  adc #0
  sta dHASHENTRIESH
  
  ; fSOURCEADDRESS += 8
  clc
  lda fSOURCEADDRESSL
  adc #8
  sta fSOURCEADDRESSL
  lda fSOURCEADDRESSH
  adc #0
  sta fSOURCEADDRESSH
  
  ; CAPACITY--
  lda dCAPACITYL
  bne decCAPACITYSkip
  dec dCAPACITYH
decCAPACITYSkip:
  dec dCAPACITYL
  
  jmp cloneDictionaryNext

cloneDictionaryDone:

  pla
  sta IDXH
  pla
  sta IDXL
  
  pla
  sta dVTYPE
  pla
  sta dKTYPE
  pla
  sta dHASHENTRIESL
  pla
  sta dHASHENTRIESH
  pla
  sta dCAPACITYL
  pla
  sta dCAPACITYH
  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  
  rts