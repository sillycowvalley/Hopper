; ######################## String syscalls ########################

; String memory map:
;   0000 heap allocator size
;   0F   type = tString
;   00   GC reference count
;   0000 string length n
;   00   first char in string
;   ..
;   <n>  last char in string

; APIs:
;
;   string String.New()
;syscallStringNew:
;
;   string String.NewFromConstant(uint constantOffset, uint length)
;syscallStringNewFromConstant:
;
;   uint String.LengthGet(string this)
;syscallStringLengthGet:
;
;   string InsertChar(string this, uint index, char append)
;syscallStringInsertChar:
;
;   string Append(string this, char appendChar)
;syscallStringAppend1:
;
;   String.Build(ref string build, string appendString) : build = build + append
;syscallStringBuild: --- not implemented?!
;
;   String.BuildFront(ref string build, char insertChar) : build = insertChar + build
;syscallStringBuildFront:
;
;   String.Build(ref string build, char appendChar) : build = build + appendChar
;syscallStringBuild1:
;
;   String.Build(ref string build) : set length = 0
;syscallStringBuild2:
;
;   string Append(string this, string appendString)
;syscallStringAppend:
;  
;    char GetChar(string this, uint index) system;
;syscallStringGetChar:
;
;
; Utility:
;
;   compare strings TOP and NEXT
;   result Z set if equal
;     uses ACC
;stringsEqual:
;
;   string in IDX, munts fITEM
;     return capacity in fSIZE
;getStringCapacity:
;
;cloneString:



; compare strings TOP and NEXT
; result Z set if equal
;   uses ACC
stringsEqual:

  ; skip past types and reference counts
  jsr incTOP   ; TODO PERF
  jsr incTOP
  jsr incNEXT
  jsr incNEXT
  
  ; length of TOP == length of NEXT?
  lda (TOP)
  cmp (NEXT)
  bne stringsEqualExit ; not equal
  sta ACCL
  jsr incTOP   ; TODO PERF
  jsr incNEXT
  lda (TOP)
  cmp (NEXT)
  bne stringsEqualExit ; not equal
  sta ACCH
  
  jsr incTOP   ; TODO PERF
  jsr incNEXT
  
  ;lda #"L"
  ;jsr diagnosticOutChar
  ;lda ACCH
  ;jsr diagnosticOutHex
  ;lda ACCL
  ;jsr diagnosticOutHex
  ;lda #" "
  ;jsr diagnosticOutChar
  
stringsEqualLoop
  lda ACCL
  bne stringsEqualNext
  lda ACCH
  bne stringsEqualNext
  
  ; must have been equal, Z set
  bra stringsEqualExit
stringsEqualNext:
  lda (TOP)
  cmp (NEXT)
  bne stringsEqualExit ; not equal
  
  ;lda (TOP)
  ;jsr diagnosticOutChar
  ;lda (NEXT)
  ;jsr diagnosticOutChar
  
  jsr incTOP
  jsr incNEXT
  jsr decACC
  bra stringsEqualLoop
  
stringsEqualExit:
  rts

; string New()
syscallStringNew:

  stz ACCL
  stz ACCH
  stz IDXL
  stz IDXH
  jmp stringNewFromConstant

; string NewFromConstant(uint constantOffset, uint length)
syscallStringNewFromConstant:
  
  ; length  
  .ifndef STACK8
  jsr decTSP
  .endif
  
  .ifdef CHECKED
  .ifndef STACK8
  lda (TSP)
  jsr assertUInt
  .endif
  .endif

  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB
  lda (SP)
  .endif
  
  sta ACCH
  
  
  .ifdef STACK8 
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; LSB
  lda (SP)
  .endif
  
  sta ACCL
  
  ; offset
  .ifndef STACK8
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif
  
  .ifdef STACK8 
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB
  lda (SP)
  .endif
  sta IDXH
  
  .ifdef STACK8 
  dex
  lda HopperValueStack, X
  stx SP8
  .else
  jsr decSP          ; LSB
  lda (SP)
  .endif
  sta IDXL
  
stringNewFromConstant:
  ; ACC: constant length
  ; IDX: constant offset
  lda ACCL
  sta fSIZEL
  lda ACCH
  sta fSIZEH
  
  ;uint constantStart = ReadWord(hopperStart + uint(2));
  clc
  lda #<HopperData  ; LSB
  adc #2
  sta IDYL
  lda #>HopperData  ; MSB
  adc #0
  sta IDYH
  
  ldy #0
  lda (IDY), Y
  sta fSOURCEADDRESSL
  iny
  lda (IDY), Y
  sta fSOURCEADDRESSH
    
  ;uint constantAddress = constantStart + constantOffset + hopperStart;
  clc
  lda #<HopperData  ; LSB
  adc fSOURCEADDRESSL
  sta fSOURCEADDRESSL
  lda #>HopperData  ; MSB
  adc fSOURCEADDRESSH
  sta fSOURCEADDRESSH
  
  clc
  lda IDXL  ; LSB
  adc fSOURCEADDRESSL
  sta fSOURCEADDRESSL
  lda IDXH  ; MSB
  adc fSOURCEADDRESSH
  sta fSOURCEADDRESSH
  
  ; store the absolute address of the constant
  lda fSOURCEADDRESSL ; LSB
  pha
  lda fSOURCEADDRESSH ; MSB
  pha

  ; store the string length  
  lda fSIZEL  ; LSB
  pha
  lda fSIZEH  ; MSB
  pha
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ; length
  pla 
  sta IDYH
  pla
  sta IDYL
  
  ; constant start address
  pla 
  sta fSOURCEADDRESSH
  pla 
  sta fSOURCEADDRESSL
  
  ;WriteWord(stringAddress+2, length);
  ldy #2
  lda IDYL
  sta (IDX), Y
  iny
  lda IDYH
  sta (IDX), Y
  iny
  
  ;uint destination = stringAddress + 4;
  clc
  lda IDXL  ; LSB
  adc #4
  sta fDESTINATIONADDRESSL
  lda IDXH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  ; SOURCEADDRESS -> DESTINATIONADDRESS
copyCharacters:
  lda #0
  cmp IDYL
  bne nextCharacterToCopy
  cmp IDYH
  beq stringInitialized
nextCharacterToCopy:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS;
  jsr decIDY
  
  bra copyCharacters
stringInitialized:
  
  lda #tString
  jmp pushIDXExit
  
  
; string NewFromConstant(char single)
syscallStringNewFromConstant1:
  
  ; single  
  .ifndef STACK8
  jsr decTSP
  .endif
  
  .ifdef CHECKED
  .ifndef STACK8
  lda (TSP)
  jsr assertUInt
  .endif
  .endif

  .ifdef STACK8 
  ldx SP8
  dex
  lda HopperValueStack, X
  .else
  jsr decSP          ; MSB
  lda (SP)
  .endif
  
  sta fVALUEH
  
  .ifdef STACK8 
  dex
  lda HopperValueStack, X
  stx SP8
  .else
  jsr decSP          ; LSB
  lda (SP)
  .endif
  
  sta fVALUEL
  
  ; ACCL: single char
  
  ; add 2 bytes for string length field
  lda fVALUEH
  beq singleChar
  lda #4
  bra doubleChar
singleChar:
  lda #3
doubleChar:
  sta fSIZEL  ; LSB
  stz fSIZEH  ; MSB
  
  dec
  dec
  pha
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ;WriteWord(stringAddress+2, length);
  ldy #2
  pla
  sta (IDX), Y
  iny
  lda #0
  sta (IDX), Y
  iny
  
  lda fVALUEL
  sta (IDX), Y
  lda fVALUEH
  beq singleChar2
  iny
  sta (IDX), Y
singleChar2:

  
  lda #tString
  jmp pushIDXExit
  
syscallStringLengthGet:
  
  .ifdef STACK8
  
  ; this -> IDX
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ; skip past block header (type and reference count)
  jsr incIDX ; TODO PERF
  jsr incIDX
  
  ldy #0
  lda (IDX), Y       ; LSB of length
  sta TOPL
  iny
  lda (IDX), Y       ; MSB of length
  sta TOPH
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  lda #tUInt
  jmp pushTOPExit
  
; string InsertChar(string this, uint index, char append)
syscallStringInsertChar:

  .ifdef STACK8
  
  ; append
  ldx SP8
  dex
  dex
  lda HopperValueStack, X ; LSB
  pha
  
  ; index -> IDY
  dex
  lda HopperValueStack, X ; MSB
  sta IDYH
  dex
  lda HopperValueStack, X ; LSB
  sta IDYL
  
  ; this -> IDX
  
  dex
  lda HopperValueStack, X ; MSB
  sta IDXH
  dex
  lda HopperValueStack, X ; LSB
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  
  .else

  ; append
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  pha
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
  ; index -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ; skip past block header (type and reference count)
  jsr incIDX  ; TODO PERF
  jsr incIDX
  
  ldy #0
  lda (IDX), Y       ; LSB of length
  sta fSIZEL
  iny
  lda (IDX), Y       ; MSB of length
  sta fSIZEH
  
  ; skip past length
  jsr incIDX ; TODO PERF
  jsr incIDX
  
  .ifdef CHECKED
  
  ; make sure index <= length (IDY <= LENGTH)
  lda fSIZEL
  sta TOPL
  lda fSIZEH
  sta TOPH
  lda IDYL
  sta NEXTL
  lda IDYH
  sta NEXTH
  jsr utilityUIntLE ; TOPL = (NEXT <= TOP)
  lda TOPL
  ;cmp #0
  bne syscallStringInsertCharRangeOk
  
  stz ACCH
  lda #$05 ; string index out of range
  sta ACCL
  jmp utilityDiagnosticsDie
syscallStringInsertCharRangeOk:
  .endif
  
  ; push source address
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; add 1 byte for the inserted character, push new length
  clc
  lda fSIZEL  ; LSB
  adc #1
  sta fSIZEL
  pha
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  pha
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  lda IDYL
  pha
  lda IDYH
  pha
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  pla
  sta IDYH
  pla
  sta IDYL
  
  pla
  sta fLENGTHH
  pla
  sta fLENGTHL
  
  pla
  sta fSOURCEADDRESSH
  pla
  sta fSOURCEADDRESSL
  pla
  sta ACCL
  stz ACCH
  
  ; push new string memory address to return
  lda IDXL
  pha
  lda IDXH
  pha
  
  ; skip past type and reference
  jsr incIDX ; TODO PERF
  jsr incIDX
  
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH
  
  ; set length field
  ldy #0
  lda fLENGTHL
  sta (fDESTINATIONADDRESS), Y
  iny
  lda fLENGTHH
  sta (fDESTINATIONADDRESS), Y
  
  ; skip past length field
  jsr incDESTINATIONADDRESS
  jsr incDESTINATIONADDRESS
  
  ; counter
  stz IDXL
  stz IDXH
  
  ; while IDX != LENGTH
nextInsertLoop:  

  lda IDXL
  cmp fLENGTHL
  bne nextInsertChar
  lda IDXH
  cmp fLENGTHH
  beq doneInsertingChar
nextInsertChar:

  ; IDX == IDY
  lda IDXL
  cmp IDYL
  bne copyInsertChar
  lda IDXH
  cmp IDYH
  bne copyInsertChar
  ; CHAR -> (DESTINATIONADDRESS)
  
  lda ACCL
  bra doCharInsert
copyInsertChar:
  
  ; (SOURCEADDRESS) -> (DESTINATIONADDRESS)
  lda (fSOURCEADDRESS)
  jsr incSOURCEADDRESS
doCharInsert:  
  sta (fDESTINATIONADDRESS)
  
  jsr incDESTINATIONADDRESS
  jsr incIDX
  bra nextInsertLoop
  
doneInsertingChar:

  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  ; push result string object onto stack
  pla
  sta IDXH
  pla
  sta IDXL
  
  lda #tString
  jmp pushIDXExit

; string Append(string this, char appendChar)
syscallStringAppend1:

  .ifdef STACK8
  
  ; appendChar -> IDY
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  .else
  
  ; appendChar -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  
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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ; skip type and reference
  jsr incIDX ; TODO PERF
  jsr incIDX
  
  ; length of 'this'
  ldy #0
  lda (IDX), Y
  sta fSIZEL
  iny
  lda (IDX), Y
  sta fSIZEH
  
  ; add length of 'appendChar'
  jsr incSIZE
  
  ; 'appendChar'
  lda IDYL
  pha
  
  ; 'this' (pointing at length)
  lda IDXH
  pha
  lda IDXL
  pha
  
  ; 'length'
  lda fSIZEH
  pha
  lda fSIZEL
  pha
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ;jsr diagnosticOutNewLine
  
  ;jsr diagnosticOutNewLine
  
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH
  
  ; skip past type and reference
  jsr incDESTINATIONADDRESS
  jsr incDESTINATIONADDRESS
  
  
  pla
  sta fLENGTHL
  pla
  sta fLENGTHH
  
  ; set length field
  ldy #0
  lda fLENGTHL
  sta (fDESTINATIONADDRESS), Y
  iny
  lda fLENGTHH
  sta (fDESTINATIONADDRESS), Y
  
  ; skip past length field
  jsr incDESTINATIONADDRESS
  jsr incDESTINATIONADDRESS

  ; 'this' (pointing at length)  
  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta IDYL
  iny
  lda (fSOURCEADDRESS), Y
  sta IDYH
  
  ; skip past length field
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS

nextAppendChar3  
  lda #0
  cmp IDYL
  bne nextAppendChar1
  cmp IDYH
  beq nextAppendChar2
nextAppendChar1:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  jsr incSOURCEADDRESS
  jsr incDESTINATIONADDRESS
  jsr decIDY
  bra nextAppendChar3
nextAppendChar2:
  ; 'appendChar'
  pla
  sta ACCL
  
  sta (fDESTINATIONADDRESS)
  
  jsr releaseSP ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  
  lda #tString
  jmp pushIDXExit
  
getStringCapacity:
  ; string in IDX, munts fITEM
  ;   return capacity in fSIZE
  
  sec
  lda IDXL
  sbc #2
  sta fITEML
  lda IDXH
  sbc #0
  sta fITEMH

  ldy #1
  lda (fITEM)
  sta fSIZEL
  lda (fITEM), Y
  sta fSIZEH
  
  ; subtract 6
  ;   0000 heap allocator size
  ;   0F   type = tString
  ;   00   GC reference count
  ;   0000 string length n

  sec
  lda fSIZEL
  sbc #6
  sta fSIZEL
  lda fSIZEH
  sbc #0
  sta fSIZEH
  
  rts

  .ifdef STACK8
; String.Build(ref string build, string appendString) : build = build + append
syscallStringBuild:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutString
  .byte "?String.Build", 0
  .endif
  jmp throwToysNoStack ; also not implemented/tested for 16 bit stack pointer (see below)
  
  jsr releaseSPNEXT ; appendString
  jsr throwToys
  jmp nextInstruction
  
  .else
; String.Build(ref string build, string appendString) : build = build + append
syscallStringBuild:

  ; appendString -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif
  
  ; ref this -> IDX
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  
  ; ref string -> IDX
  ldy #1
  lda (IDX)
  tax
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  lda #"S"
  jsr diagnosticOutChar
  lda IDXH
  jsr diagnosticOutHex
  lda IDXL
  jsr diagnosticOutHex
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  jsr getStringCapacity
  
  
  ldy #2
  lda (IDX), Y
  sta fLENGTHL
  iny
  lda (IDX), Y
  sta fLENGTHH
  
  lda #"C"
  jsr diagnosticOutChar
  lda fSIZEH
  jsr diagnosticOutHex
  lda fSIZEL
  jsr diagnosticOutHex
  lda #"L"
  jsr diagnosticOutChar
  lda fLENGTHH
  jsr diagnosticOutHex
  lda fLENGTHL
  jsr diagnosticOutHex
  
  
  jsr releaseSPNEXT ; appendString
  
  
  jsr throwToys
  jmp nextInstruction
  .endif
  
; String.BuildFront(ref string build, char insertChar) : build = insertChar + build
syscallStringBuildFront:

  ; insertChar -> fVALUE
  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta fVALUEH
  dex
  lda HopperValueStack, X
  sta fVALUEL
  
  .else
  jsr decSP          ; MSB
  lda (SP)
  sta fVALUEH
  jsr decSP          ; LSB
  lda (SP)
  sta fVALUEL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif
  .endif

  ; ref this -> IDX
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  stx SP8
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertReference
  .endif
  .else
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  .endif
  
  ; ref string -> IDX
  ldy #1
  lda (IDX)
  tax
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  jsr getStringCapacity
  
  ldy #2
  lda (IDX), Y
  sta fLENGTHL
  iny
  lda (IDX), Y
  sta fLENGTHH
  
  ;jsr diagnosticOutNewLine
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex
  ;lda IDXL
  ;jsr diagnosticOutHex
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda fSIZEH
  ;jsr diagnosticOutHex
  ;lda fSIZEL
  ;jsr diagnosticOutHex
  ;lda #"L"
  ;jsr diagnosticOutChar
  ;lda fLENGTHH
  ;jsr diagnosticOutHex
  ;lda fLENGTHL
  ;jsr diagnosticOutHex
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda fVALUEL
  ;jsr diagnosticOutHex
  
  ; fLENGTH >= fSIZE?
  lda fLENGTHH
  cmp fSIZEH
  bne donesyscallStringBuildFrontGE
  lda fLENGTHL
  cmp fSIZEL
donesyscallStringBuildFrontGE:
  bcs syscallStringBuildFrontLT 
  jmp syscallStringBuildFrontGE ; fLENGTH < fSIZE
  
syscallStringBuildFrontLT:
  ; fLENGTH >= fSIZE
  
  
  
  ;lda #"+"
  ;jsr diagnosticOutChar
  
  ; add 2 bytes for string length field and 1 for the extra character (bumps block size up by 16)
  clc
  lda fLENGTHL  ; LSB
  adc #3
  sta fSIZEL
  lda fLENGTHH  ; MSB
  adc #0
  sta fSIZEH
  
  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ; clone size: string length + 2 bytes for length field + 2 bytes for type and reference count
  jsr incSIZE 
  
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda fSIZEH
  ;jsr diagnosticOutHex
  ;lda fSIZEL
  ;jsr diagnosticOutHex
  
  lda IDYL
  sta fSOURCEADDRESSL
  lda IDYH
  sta fSOURCEADDRESSH
  
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH

syscallStringBuildFrontNext:  

  lda (fSOURCEADDRESS);
  sta (fDESTINATIONADDRESS)

  jsr decSIZE
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS
  
  lda fSIZEL
  bne syscallStringBuildFrontNext
  lda fSIZEH
  bne syscallStringBuildFrontNext
  
  ; IDY -> IDX
  lda #tString
  sta fTYPE
  jsr replaceStackReferences
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  jsr memoryFree ; free the original from under the nose of the GC
  
  pla
  sta IDXL
  pla
  sta IDXH
  
syscallStringBuildFrontGE:
  
  ; shift string forward by one in reverse
  
  lda IDXL
  sta fSOURCEADDRESSL
  lda IDXH
  sta fSOURCEADDRESSH
  
  ; first character
  clc
  lda IDXL
  adc #4
  sta fSOURCEADDRESSL
  lda IDXH
  adc #0
  sta fSOURCEADDRESSH
  
  clc
  lda fSOURCEADDRESSL
  adc fLENGTHL
  sta fSOURCEADDRESSL
  sta fDESTINATIONADDRESSL
  lda fSOURCEADDRESSH
  adc fLENGTHH
  sta fSOURCEADDRESSH
  sta fDESTINATIONADDRESSH
  
  jsr decSOURCEADDRESS
  
  lda fLENGTHL
  sta fSIZEL
  lda fLENGTHH
  sta fSIZEH
  
syscallStringBuildFrontCopyLoop:
  lda fSIZEL
  bne syscallStringBuildFrontCopyNext
  lda fSIZEH
  bne syscallStringBuildFrontCopyNext
  bra syscallStringBuildFrontCopyDone
syscallStringBuildFrontCopyNext:

  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  
  jsr decSOURCEADDRESS
  jsr decDESTINATIONADDRESS
  jsr decSIZE
  bra syscallStringBuildFrontCopyLoop
  
syscallStringBuildFrontCopyDone:
  
  ; build[0] = insertChar
  lda fVALUEL
  sta (fDESTINATIONADDRESS)
  
  ; length++
  jsr incLENGTH
  
  ldy #2
  lda fLENGTHL
  sta (IDX), Y
  iny
  lda fLENGTHH
  sta (IDX), Y
  
  jmp nextInstruction
  
; String.Build(ref string build, char appendChar) : build = build + appendChar
syscallStringBuild1:

  .ifdef STACK8
  
  ; appendChar -> fVALUE
  ldx SP8
  dex
  lda HopperValueStack, X
  sta fVALUEH
  dex
  lda HopperValueStack, X
  sta fVALUEL
  
  ; ref this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertReference
  .endif
  stx SP8
  
  .else
  
  ; appendChar -> fVALUE
  jsr decSP          ; MSB
  lda (SP)
  sta fVALUEH
  jsr decSP          ; LSB
  lda (SP)
  sta fVALUEL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif

  ; ref this -> IDX
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  .endif
  
  ; ref string -> IDX
  ldy #1
  lda (IDX)
  tax
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  
  ; string in IDX, munts fITEM
  ;   return capacity in fSIZE (heap allocator size - 6)
  jsr getStringCapacity
  
  ldy #2
  lda (IDX), Y
  sta fLENGTHL
  iny
  lda (IDX), Y
  sta fLENGTHH
  
  ;jsr diagnosticOutNewLine
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda IDXH
  ;jsr diagnosticOutHex
  ;lda IDXL
  ;jsr diagnosticOutHex
  ;lda #"C"
  ;jsr diagnosticOutChar
  ;lda fSIZEH
  ;jsr diagnosticOutHex
  ;lda fSIZEL
  ;jsr diagnosticOutHex
  ;lda #"L"
  ;jsr diagnosticOutChar
  ;lda fLENGTHH
  ;jsr diagnosticOutHex
  ;lda fLENGTHL
  ;jsr diagnosticOutHex
  ;lda #"A"
  ;jsr diagnosticOutChar
  ;lda fVALUEL
  ;jsr diagnosticOutHex
  
  ; fLENGTH >= fSIZE?
  lda fLENGTHH
  cmp fSIZEH
  bne donesyscallStringBuild1GE
  lda fLENGTHL
  cmp fSIZEL
donesyscallStringBuild1GE:
  bcs syscallStringBuild1LT 
  jmp syscallStringBuild1GE ; fLENGTH < fSIZE
  
syscallStringBuild1LT:
  ; fLENGTH >= fSIZE
  
  ; add 2 bytes for string length field and 1 for the extra character (bumps to next block size)
  clc
  lda fLENGTHL  ; LSB
  adc #3
  sta fSIZEL
  lda fLENGTHH  ; MSB
  adc #0
  sta fSIZEH
  
  lda IDXH
  sta IDYH
  lda IDXL
  sta IDYL
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ; clone size: string length + 2 bytes for length field + 2 bytes for type and reference count
  jsr incSIZE 
  
  ;lda #"S"
  ;jsr diagnosticOutChar
  ;lda fSIZEH
  ;jsr diagnosticOutHex
  ;lda fSIZEL
  ;jsr diagnosticOutHex
  
  lda IDYL
  sta fSOURCEADDRESSL
  lda IDYH
  sta fSOURCEADDRESSH
  
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH

syscallStringBuild1Next:

  lda (fSOURCEADDRESS);
  sta (fDESTINATIONADDRESS)

  jsr decSIZE
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS
  
  lda fSIZEL
  bne syscallStringBuild1Next
  lda fSIZEH
  bne syscallStringBuild1Next
  
  ; IDY -> IDX
  lda #tString
  sta fTYPE
  jsr replaceStackReferences
  
  lda IDXH
  pha
  lda IDXL
  pha
  
  lda IDYL
  sta IDXL
  lda IDYH
  sta IDXH
  jsr memoryFree ; free the original from under the nose of the GC
  
  pla
  sta IDXL
  pla
  sta IDXH
  
syscallStringBuild1GE:
  
  ; build[length] = appendChar
  clc
  lda IDXL
  adc #4
  sta fDESTINATIONADDRESSL
  lda IDXH
  adc #0
  sta fDESTINATIONADDRESSH
  
  clc
  lda fDESTINATIONADDRESSL
  adc fLENGTHL
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH
  adc fLENGTHH
  sta fDESTINATIONADDRESSH
  
  lda fVALUEL
  sta (fDESTINATIONADDRESS)
  
  ; length++
  jsr incLENGTH
  
  ldy #2
  lda fLENGTHL
  sta (IDX), Y
  iny
  lda fLENGTHH
  sta (IDX), Y
  
  jmp nextInstruction
  
; String.Build(ref string build) : set length = 0
syscallStringBuild2:

  .ifdef STACK8
  
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertReference
  .endif
  stx SP8
  
  .else
  
  ; ref this -> IDX
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  .endif
  
  ; ref string -> IDX
  ldy #1
  lda (IDX)
  tax
  lda (IDX), Y
  sta IDXH
  stx IDXL
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ; set length = 0
  ldy #2
  lda #0
  sta (IDX), Y
  iny
  sta (IDX), Y
  
  jmp nextInstruction
  
; string Append(string this, string appendString)
syscallStringAppend:

  ; appendString -> IDY
  .ifdef STACK8
  ldx SP8
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  .else
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif
  .endif
  
  ; this -> IDX
  .ifdef STACK8
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  .else
  jsr decSP          ; MSB
  lda (SP)
  sta IDXH
  jsr decSP          ; LSB
  lda (SP)
  sta IDXL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  lda (IDY)
  jsr assertString
  .endif
  ; skip type and reference
  jsr incIDX ; TODO PERF
  jsr incIDX
  jsr incIDY
  jsr incIDY
  
  ; length of 'this'
  ldy #0
  lda (IDX), Y
  sta fSIZEL
  iny
  lda (IDX), Y
  sta fSIZEH
  
  ; add length of 'appendString'
  ldy #0
  lda (IDY), Y
  clc
  adc fSIZEL
  sta fSIZEL
  iny
  lda (IDY), Y
  adc fSIZEH
  sta fSIZEH
  
  ; 'appendString' (pointing at length)
  lda IDYH
  pha
  lda IDYL
  pha
  
  ; 'this' (pointing at length)
  lda IDXH
  pha
  lda IDXL
  pha
  
  ; 'length'
  lda fSIZEH
  pha
  lda fSIZEL
  pha
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tString
  jsr gcCreate ; destroys Nx variables in memoryAllocate
  
  ;jsr diagnosticOutNewLine
  
  ;jsr diagnosticOutNewLine
  
  lda IDXL
  sta fDESTINATIONADDRESSL
  lda IDXH
  sta fDESTINATIONADDRESSH
  
  ; skip past type and reference
  jsr incDESTINATIONADDRESS
  jsr incDESTINATIONADDRESS
  
  
  pla
  sta fLENGTHL
  pla
  sta fLENGTHH
  
  ; set length field
  ldy #0
  lda fLENGTHL
  sta (fDESTINATIONADDRESS), Y
  iny
  lda fLENGTHH
  sta (fDESTINATIONADDRESS), Y
  
  ; skip past length field
  jsr incDESTINATIONADDRESS
  jsr incDESTINATIONADDRESS

  ; 'this' (pointing at length)  
  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta IDYL
  iny
  lda (fSOURCEADDRESS), Y
  sta IDYH
  
  ; skip past length field
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS

nextAppend3  
  lda #0
  cmp IDYL
  bne nextAppend1
  cmp IDYH
  beq nextAppend2
nextAppend1:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  jsr incSOURCEADDRESS
  jsr incDESTINATIONADDRESS
  jsr decIDY
  bra nextAppend3
nextAppend2:
  ; 'appendString' (pointing at length)
  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  
  ldy #0
  lda (fSOURCEADDRESS), Y
  sta IDYL
  iny
  lda (fSOURCEADDRESS), Y
  sta IDYH
  
  ; skip past length field
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
nextAppend6
  lda #0
  cmp IDYL
  bne nextAppend4
  cmp IDYH
  beq nextAppend5
nextAppend4:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  jsr incSOURCEADDRESS
  jsr incDESTINATIONADDRESS
  jsr decIDY
  bra nextAppend6
nextAppend5:
  
  
   ; we popped 'appendString', decrease reference count (munts all Nx variables if memoryFree is called)
  ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  jsr releaseSPandSPNEXT
  
  lda #tString
  jmp pushIDXExit
  
  
; char GetChar(string this, uint index) system;
syscallStringGetChar:

  .ifdef STACK8
  
  ldx SP8
  ; index -> IDY
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  
  .else
  
  ; index -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif

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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ; length of 'this'
  ldy #2
  lda (IDX), Y
  sta fLENGTHL
  iny
  lda (IDX), Y
  sta fLENGTHH
  
  ; skip type, reference and length fields
  clc
  lda #4
  adc IDXL
  sta IDXL
  lda #0
  adc IDXH
  sta IDXH
  
  .ifdef CHECKED
  ; make sure index < length (IDY < LENGTH)
  lda fLENGTHL
  sta TOPL
  lda fLENGTHH
  sta TOPH
  lda IDYL
  sta NEXTL
  lda IDYH
  sta NEXTH
  jsr utilityUIntLT ; TOPL = (NEXT < TOP)
  lda TOPL
  ;cmp #0
  bne syscallStringGetCharRangeOk
  stz ACCH
  lda #$05 ; string index out of range
  sta ACCL
  jmp utilityDiagnosticsDie
syscallStringGetCharRangeOk:
  .endif
  
  ; index <= 255 ?
  lda IDYH                      ;  +3
  bne syscallStringGetCharLong  ;  +3
  ldy IDYL                      ;  3
  lda (IDX), Y                  ;  5
  bra syscallStringGetCharShort ;  3
                                ; 17
syscallStringGetCharLong:
  clc                           ;  2
  lda IDXL                      ;  3
  adc IDYL                      ;  3
  sta IDXL
  lda IDXH                      ;  3
  adc IDYH                      ;  3
  sta IDXH
  lda (IDX)                     ;  5
                                ; 19 + 6
syscallStringGetCharShort:
  pha
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  .ifdef STACK8
  
  ldx SP8
  pla
  sta HopperValueStack, X
  lda #tUInt
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
  
  pla
  sta (SP)
  jsr incSP          ; LSB
  lda #0
  sta (SP)
  jsr incSP          ; MSB
  lda #tUInt
  sta (TSP)
  jsr incTSP
  
  .endif
  
  jmp nextInstruction


cloneString:

  ; used by cloneDictionary, cloneString and cloneLong
  lda fSOURCEADDRESSH
  pha
  lda fSOURCEADDRESSL
  pha
  
  ; initialized from string at IDY
  ldy #1
  lda IDYH
  sta fSOURCEADDRESSH
  lda IDYL
  sta fSOURCEADDRESSL
  
  ldy #2
  lda (fSOURCEADDRESS), Y
  sta fSIZEL
  iny
  lda (fSOURCEADDRESS), Y
  sta fSIZEH
  
  ; store the string length  
  lda fSIZEL  ; LSB
  sta fLENGTHL
  lda fSIZEH  ; MSB
  sta fLENGTHH
  
  ; add 2 bytes for string length field
  clc
  lda fSIZEL  ; LSB
  adc #2
  sta fSIZEL
  lda fSIZEH  ; MSB
  adc #0
  sta fSIZEH
  
  ; type in A
  ; size is in fSIZE = length in characters + 2 bytes for string length field
  ; return address in IDX
  lda #tString
  jsr gcCreate
  
  ;WriteWord(stringAddress+2, length);
  ldy #2
  lda fLENGTHL
  sta (IDX), Y
  iny
  lda fLENGTHH
  sta (IDX), Y
  iny
  
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
  jsr incSOURCEADDRESS
  
  clc
  lda IDXL  ; LSB
  adc #4
  sta fDESTINATIONADDRESSL
  lda IDXH  ; MSB
  adc #0
  sta fDESTINATIONADDRESSH
  
  ; SOURCEADDRESS -> DESTINATIONADDRESS
copyCharactersClone:
  lda #0
  cmp fLENGTHL
  bne nextCharacterToCopyClone
  cmp fLENGTHH
  beq stringInitializedClone
nextCharacterToCopyClone:  
  lda (fSOURCEADDRESS)
  sta (fDESTINATIONADDRESS)
  
  jsr incDESTINATIONADDRESS
  jsr incSOURCEADDRESS;
  jsr decLENGTH
  
  bra copyCharactersClone
stringInitializedClone:

  pla
  sta fSOURCEADDRESSL
  pla
  sta fSOURCEADDRESSH
  
  rts

; bool StartsWith(string this, char pattern)
syscallStringStartsWith:
  .ifdef STACK8
  
  ldx SP8
  ; pattern -> ACCL
  dex
  dex
  lda HopperValueStack, X
  sta ACCL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  
  .else
  
  ; pattern -> ACCL
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  jsr decTSP
  
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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ldy #2
  lda (IDX), Y       ; LSB of length
  sta fLENGTHL
  iny
  lda (IDX), Y       ; MSB of length
  sta fLENGTHH
  
  ; skip past type, reference count and length
  clc
  lda #4
  adc IDXL
  sta IDXL
  lda #0
  adc IDXH
  sta IDXH
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  stz TOPL
  stz TOPH
  
  lda fLENGTHL
  bne syscallStringStartsWithNotEmpty
  lda fLENGTHH
  bne syscallStringStartsWithNotEmpty
  ; empty string
  lda #tBool
  bra syscallStringStartsWithDone
  
syscallStringStartsWithNotEmpty:
  
  lda (IDX) ; first character
  cmp ACCL
  bne syscallStringStartsWithDone
  
  lda #1
  sta TOPL
  
syscallStringStartsWithDone:
  lda #tBool
  jmp pushTOPExit
  
; bool Contains(string this, char needle)
syscallStringContains:
  .ifdef STACK8
  
  ldx SP8
  
  ; needle -> ACCL
  dex
  dex
  lda HopperValueStack, X
  sta ACCL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  
  .else
  ; needle -> ACCL
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  jsr decTSP
  
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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ldy #2
  lda (IDX), Y       ; LSB of length
  sta fSIZEL
  iny
  lda (IDX), Y       ; MSB of length
  sta fSIZEH
  
  ; skip past type, reference count and length
  clc
  lda #4
  adc IDXL
  sta IDXL
  lda #0
  adc IDXH
  sta IDXH
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  stz TOPL
  stz TOPH
  
; length <= 255 ?
  lda fSIZEH
  bne syscallStringContainsLong
  
  ; length <= 255
  
  ldy #0
syscallStringContainsNextShort:
  cpy fSIZEL ; index == length?
  beq syscallStringContainsDone
  lda (IDX), Y
  cmp ACCL
  beq syscallStringContainsFound
  iny
  bra syscallStringContainsNextShort
  
syscallStringContainsLong:
  
syscallStringContainsNext:
  lda fSIZEL
  bne syscallStringContainsCompare
  lda fSIZEH
  bne syscallStringContainsCompare
  ; empty string
  bra syscallStringContainsDone
  
syscallStringContainsCompare:
  
  lda (IDX) ; first character
  cmp ACCL
  beq syscallStringContainsFound
  
  jsr incIDX
  jsr decSIZE
  bra syscallStringContainsNext
  
syscallStringContainsFound:
  lda #1
  sta TOPL
  
syscallStringContainsDone:
  lda #tBool
  jmp pushTOPExit
  
; bool IndexOf(string this, char needle, ref uint index)
syscallStringIndexOf:
  .ifdef STACK8
  
  ldx SP8
  
  ; ref index -> IDY
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertReference
  .endif
  
  .else
  
  ; ref index -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  .endif
  
  stz fVALUEL
  stz fVALUEH
  jmp syscallStringIndexOfShared
  
; bool IndexOf(string this, char pattern, uint searchIndex, ref uint index)
syscallStringIndexOf1:
  .ifdef STACK8
  
  ldx SP8
  
  ; ref index -> IDY
  dex
  lda HopperValueStack, X
  sta IDYH
  dex
  lda HopperValueStack, X
  sta IDYL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertReference
  .endif
  
  ; searchIndex -> fVALUE
  dex
  lda HopperValueStack, X
  sta fVALUEH
  dex
  lda HopperValueStack, X
  sta fVALUEL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertUInt
  .endif

syscallStringIndexOfShared:

  ; pattern -> ACCL
  dex
  dex
  lda HopperValueStack, X
  sta ACCL
  
  ; this -> IDX
  dex
  lda HopperValueStack, X
  sta IDXH
  dex
  lda HopperValueStack, X
  sta IDXL
  .ifdef CHECKED
  lda HopperTypeStack, X
  jsr assertString
  .endif
  stx SP8
  
  .else
  
  ; ref index -> IDY
  jsr decSP          ; MSB
  lda (SP)
  sta IDYH
  jsr decSP          ; LSB
  lda (SP)
  sta IDYL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertReference
  .endif
  
  ; searchIndex -> fVALUE
  jsr decSP          ; MSB
  lda (SP)
  sta fVALUEH
  jsr decSP          ; LSB
  lda (SP)
  sta fVALUEL
  jsr decTSP
  
  .ifdef CHECKED
  lda (TSP)
  jsr assertUInt
  .endif

syscallStringIndexOfShared:
  
  ; needle -> ACCL
  jsr decSP          ; MSB
  jsr decSP          ; LSB
  lda (SP)
  sta ACCL
  jsr decTSP
  
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
  jsr assertString
  .endif
  .endif
  
  .ifdef CHECKED
  lda (IDX)
  jsr assertString
  .endif
  
  ldy #2
  lda (IDX), Y       ; LSB of length
  sta fSIZEL
  iny
  lda (IDX), Y       ; MSB of length
  sta fSIZEH
  
  ; skip past type, reference count and length
  clc
  lda #4
  adc IDXL
  sta IDXL
  lda #0
  adc IDXH
  sta IDXH
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  
  ; make sure searchIndex < length (fVALUE < fSIZE)
  lda fSIZEL
  sta TOPL
  lda fSIZEH
  sta TOPH
  lda fVALUEL
  sta NEXTL
  lda fVALUEH
  sta NEXTH
  jsr utilityUIntLT ; TOPL = (NEXT < TOP)
  lda TOPL
  ;cmp #0
  bne syscallStringIndexOfRangeOk
  
  ; result = false
  stz TOPL
  stz TOPH
  
  ; searchIndex >= length
  bra syscallStringIndexOfDone2
  
syscallStringIndexOfRangeOk:

  ; result = false
  stz TOPL
  stz TOPH
  
  stz lCOUNTH
  
  ; length <= 255 ?
  lda fSIZEH
  bne syscallStringIndexOfLong2
  
  ; length <= 255
  
  ldy fVALUEL
syscallStringIndexOfNextShort2:
  cpy fSIZEL ; index == length?
  beq syscallStringIndexOfDone2
  lda (IDX), Y
  cmp ACCL
  beq syscallStringIndexOfFoundShort2
  iny
  bra syscallStringIndexOfNextShort2
  
syscallStringIndexOfLong2:
  
  lda fVALUEL
  sta lCOUNTL
  
syscallStringIndexOfNext2:
  lda fSIZEL
  bne syscallStringIndexOfCompare2
  lda fSIZEH
  bne syscallStringIndexOfCompare2
  ; empty string
  bra syscallStringIndexOfDone2
  
syscallStringIndexOfCompare2:
  
  lda (IDX) ; first character
  cmp ACCL
  beq syscallStringIndexOfFound2
  
  jsr incIDX
  jsr decSIZE
  jsr incCOUNT
  bra syscallStringIndexOfNext2
  
syscallStringIndexOfFoundShort2:
  sty lCOUNTL
  
syscallStringIndexOfFound2:
  
  ldy #1
  lda lCOUNTL
  sta (IDY)
  lda lCOUNTH
  sta (IDY), Y
  
  lda #1
  sta TOPL
  
syscallStringIndexOfDone2:
  lda #tBool
  jmp pushTOPExit
  