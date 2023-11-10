; ######################## String syscalls ########################

  .include StringUtilities.asm

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
; syscallCharToString:               string ToString(char this)
;
; syscallStringAppend:               string Append(string this, string appendString)
; syscall_1_StringAppend:            string Append(string this, char appendChar)
; syscallStringBuild:                String.Build(ref string build, string appendString) : build = build + append
; syscall_1_StringBuild:             String.Build(ref string build, char appendChar) : build = build + appendChar
; syscall_2_StringBuild:             String.Build(ref string build) : set length = 0
; syscallStringBuildFront:           String.BuildFront(ref string build, char insertChar) : build = insertChar + build
; syscallStringCompare:              int Compare(string left, string right) // returns -1, 0, +1
; syscallStringContains:             bool Contains(string this, char needle) system;
; syscallStringGetChar:              char GetChar(string this, uint index) system;
; syscallStringIndexOf:              bool IndexOf(string this, char needle, ref uint index)
; syscall_1_StringIndexOf:           bool IndexOf(string this, char needle, uint searchIndex, ref uint index)
; syscallStringInsertChar:           string InsertChar(string this, uint index, char append)
; syscallStringLengthGet:            uint Length { get system; }
; syscallStringNew:                  string New()
; syscall_1_StringNewFromConstant:   string NewFromConstant(uint twinChar)
; syscallStringNewFromConstant:      string NewFromConstant(uint constantOffset, uint length)
; syscallStringPushImmediate:        pops uints from the stack until LSB or MSB is null, builds a string and pushes it
; syscallStringStartsWith:           bool StartsWith(string this, char pattern) system;
; syscall_1_StringSubstring:         string Substring(string original, uint start)
; syscall_2_StringSubstring:         Substring(ref string build, uint start) system;
; syscall_1_StringToLower:           ToLower(ref string build) system;
; syscallStringToLower:              string ToLower(string this) system;
; syscallStringToUpper:              string ToUpper(string this) system;
; syscall_1_StringToUpper:           ToUpper(ref string build) system;
; syscallStringTrim:                 string Trim(string this) system;
; syscall_1_StringTrim:              Trim(ref string build) system;
; syscallStringTrimLeft:             string TrimLeft(string this) system;
; syscall_1_StringTrimLeft:          TrimLeft(ref string build) system;
; syscallStringTrimRight:            TrimRight(ref string build) system;
;
;
;


; bool StartsWith(string this, char pattern)
syscallStringStartsWith:
  jsr stringUtilityPopCharToACCL
  jsr stringUtilityPopStringToIDX
  
  jsr stringUtilityLoadLengthFromIDX
  jsr stringUtilityAdvanceIDXToFirstChar
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  ldx #0 ; false result
  
  lda fLENGTHL
  bne syscallStringStartsWithNotEmpty
  lda fLENGTHH
  bne syscallStringStartsWithNotEmpty
  
  ; empty string
  bra syscallStringStartsWithDone
  
syscallStringStartsWithNotEmpty:
  
  lda (IDX) ; first character
  cmp ACCL
  bne syscallStringStartsWithDone
  
  ldx #1 ; true result
  
syscallStringStartsWithDone:
  jmp pushXBoolExit


; bool Contains(string this, char needle)
syscallStringContains:

  jsr stringUtilityPopCharToACCL
  jsr stringUtilityPopStringToIDX
  
  jsr stringUtilityLoadLengthFromIDX
  jsr stringUtilityAdvanceIDXToFirstChar
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  ldx #0 ; false result
  
  ; length <= 255 ?
  lda fLENGTH
  bne syscallStringContainsLong
  
  ; length <= 255
  
  ldy #0
syscallStringContainsNextShort:
  cpy fLENGTHL ; index == length?
  beq syscallStringContainsDone
  lda (IDX), Y
  cmp ACCL
  beq syscallStringContainsFound
  iny
  bra syscallStringContainsNextShort
  
syscallStringContainsLong:
  
syscallStringContainsNext:
  lda fLENGTHL
  bne syscallStringContainsCompare
  lda fLENGTHH
  bne syscallStringContainsCompare
  ; empty string
  bra syscallStringContainsDone
  
syscallStringContainsCompare:
  
  lda (IDX) ; first character
  cmp ACCL
  beq syscallStringContainsFound
  
  jsr incIDX
  jsr decLENGTH
  bra syscallStringContainsNext
  
syscallStringContainsFound:
  ldx #1 ; true result
  
syscallStringContainsDone:
  jmp pushXBoolExit


; char GetChar(string this, uint index) system;
syscallStringGetChar:

  jsr stringUtilityPopUIntToIndex
  jsr stringUtilityPopStringToIDX
  
  jsr stringUtilityLoadLengthFromIDX
  jsr stringUtilityAdvanceIDXToFirstChar
  
  .ifdef CHECKED
  jsr stringUtilityVerifyIndexLTLength
  .endif
  
  clc
  lda IDXL
  adc IDYL
  sta IDXL
  lda IDXH
  adc IDYH
  sta IDXH
  lda (IDX)

  pha
  jsr releaseSP ; we popped 'this', decrease reference count
  pla
  
  ; push A as UInt
  sta TOPL
  stz TOPH
  
  lda #tUInt
  jmp pushTOPExit


; uint Length
syscallStringLengthGet:
  jsr stringUtilityPopStringToIDX
  jsr stringUtilityLoadLengthFromIDX
  lda fLENGTHL
  sta TOPL
  lda fLENGTHH
  sta TOPH
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  lda #tUInt
  jmp pushTOPExit


; bool IndexOf(string this, char needle, ref uint index)
syscallStringIndexOf:
  ; index
  jsr stringUtilityPopRefToIndex
  ; searchIndex = 0
  stz fVALUEL
  stz fVALUEH
  jmp syscallStringIndexOfShared


; bool IndexOf(string this, char needle, uint searchIndex, ref uint index)
syscall_1_StringIndexOf:
  ; index
  jsr stringUtilityPopRefToIndex
  ; searchIndex -> fVALUE
  jsr stringUtilityPopUIntToValue
  ; fall through
syscallStringIndexOfShared:

  ; needle -> ACCL
  jsr stringUtilityPopCharToACCL
  ; this -> IDX
  jsr stringUtilityPopStringToIDX
  
  jsr stringUtilityLoadLengthFromIDX
  jsr stringUtilityAdvanceIDXToFirstChar
  
  jsr releaseSP ; we popped 'this', decrease reference count
  
  ldx #0 ; false result
  
  ; make sure searchIndex < length (fVALUE < fLENGTH)
  lda fLENGTHL
  sta TOPL
  lda fLENGTHH
  sta TOPH
  lda fVALUEL
  sta NEXTL
  lda fVALUEH
  sta NEXTH
  jsr utilityUIntLT ; TOPL = (NEXT < TOP)
  lda TOPL
  ;cmp #0
  bne syscallStringIndexOfRangeOk
  
  ; searchIndex >= length
  bra syscallStringIndexOfDoneShort
  
syscallStringIndexOfRangeOk:

  stz lCOUNTH
  
  ; length <= 255 ?
  lda fLENGTHH
  bne syscallStringIndexOfLong
  
  ; length <= 255
  ldy fVALUEL
syscallStringIndexOfNextShort:
  cpy fLENGTHL ; index == length?
  beq syscallStringIndexOfDoneShort
  lda (IDX), Y
  cmp ACCL
  beq syscallStringIndexOfFoundShort
  iny
  bra syscallStringIndexOfNextShort
  
syscallStringIndexOfFoundShort:
  ; lCOUNT to ref index: lCOUNT -> (IDY)
  sty lCOUNTL
  ldy #1
  lda lCOUNTL
  sta (IDY)
  lda lCOUNTH
  sta (IDY), Y
  ldx #1 ; true result
  
syscallStringIndexOfDoneShort:
  jmp pushXBoolExit


syscallStringIndexOfLong:
  lda fVALUEL
  sta lCOUNTL
syscallStringIndexOfNext:
  lda fLENGTHL
  bne syscallStringIndexOfCompare
  lda fLENGTHH
  bne syscallStringIndexOfCompare
  ; empty string
  bra syscallStringIndexOfDone
  
syscallStringIndexOfCompare:
  lda (IDX) ; first character
  cmp ACCL
  beq syscallStringIndexOfFound
  jsr incIDX
  jsr decLENGTH
  jsr incCOUNT
  bra syscallStringIndexOfNext
  
syscallStringIndexOfFound:
  ; lCOUNT to ref index: lCOUNT -> (IDY)
  ldy #1
  lda lCOUNTL
  sta (IDY)
  lda lCOUNTH
  sta (IDY), Y
  
  ldx #1 ; true result

syscallStringIndexOfDone:
  jmp pushXBoolExit



; TODO: merge String.New(), Char.ToString(char this) and String.NewFromConstant(uint twinChar) into a single syscall in future

; string New()
syscallStringNew:
  stz fVALUEL
  stz fVALUEH
  lda #0
  bra zeroCharNewString

; string ToString(char this)
syscallCharToString:
  ; exactly the same as syscall_1_StringNewFromConstant (with MSB = 0)

  ; fall through
  
; string NewFromConstant(uint twinChar)
syscall_1_StringNewFromConstant:
  jsr stringUtilityPopUIntToValue

  ; add 2 bytes for string length field
  lda fVALUEH
  beq singleChar
  lda #2
  bra doubleChar
singleChar:
  lda #1
doubleChar:
zeroCharNewString:
  sta fSIZEL  ; LSB
  stz fSIZEH  ; MSB
  
  jsr stringUtilityCreateString                 ; fSIZE is number of characters needed in string, returns string as IDX (sets default length value)
  
  lda fVALUEL
  beq singleChar2
  ldy #4
  sta (IDX), Y
  lda fVALUEH
  beq singleChar2
  iny
  sta (IDX), Y
singleChar2:

  lda #tString
  jmp pushIDXExit


; string NewFromConstant(uint constantOffset, uint length)
syscallStringNewFromConstant:
  
  ; length -> fCOUNT
  jsr stringUtilityPopUIntToCount
  
  ; offset -> IDY
  jsr stringUtilityPopUIntToIndex
  
  ;uint constantStart = ReadWord(hopperStart + uint(2));
  clc
  lda #<HopperData  ; LSB
  adc #2
  sta IDXL
  lda #>HopperData  ; MSB
  adc #0
  sta IDXH
  
  ldy #0
  lda (IDX), Y
  sta fSOURCEADDRESSL
  iny
  lda (IDX), Y
  sta fSOURCEADDRESSH
    
  ; constantAddress = constantStart + hopperStart;
  clc
  lda #<HopperData  ; LSB
  adc fSOURCEADDRESSL
  sta fSOURCEADDRESSL
  lda #>HopperData  ; MSB
  adc fSOURCEADDRESSH
  sta fSOURCEADDRESSH
  
  ; constantAddress = constantAddress + constantOffset
  clc
  lda IDYL  ; LSB
  adc fSOURCEADDRESSL
  sta fSOURCEADDRESSL
  lda IDYH  ; MSB
  adc fSOURCEADDRESSH
  sta fSOURCEADDRESSH
  
  lda lCOUNTL  ; LSB
  sta fSIZEL
  lda lCOUNTH  ; MSB
  sta fSIZEH
  jsr stringUtilityCreateString                 ; fSIZE is number of characters needed in string, returns string as IDX (sets default length value)
  
  ; copy lCOUNT chars from fSOURCEADDRESS to string at IDY
  jsr stringUtilityCopyCharsToIDX
  
  lda #tString
  jmp pushIDXExit


; String.Build(ref string build, string appendString) : build = build + append
syscallStringBuild:
  ; appendString
  jsr stringUtilityPopStringToIDX    ; appendString -> IDX
  jsr stringUtilityLoadLengthFromIDX ; (IDX), 2 -> fLENGTH
  lda fLENGTHL                       ; number of characters to append -> lCOUNT
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  jsr stringUtilityLoadSourceFromIDX ; (IDX), 4 -> fSOURCEADDRESS
  
  ; build
  jsr stringUtilityPopRefToIndex     ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX       ; ref string IDY -> IDX
  jsr stringUtilityGetCapacity       ; IDX -> string, returns capacity in fSIZE
  jsr stringUtilityLoadLengthFromIDX ; (IDX), 2 -> fLENGTH
  
  clc                                ; fVALUE = fLENGTH (length of build) + lCOUNT (length of appendString)
  lda fLENGTHL 
  adc lCOUNTL
  sta fVALUEL
  lda fLENGTHH
  adc lCOUNTH
  sta fVALUEH
  
  ; fVALUE >= fSIZE?
  lda fVALUEH
  cmp fSIZEH
  bne donesyscallStringBuild1CheckGT1
  lda fVALUEL
  cmp fSIZEL
donesyscallStringBuild1CheckGT1:
  bcs syscallStringBuild1Resize1 
  jmp syscallStringBuild1LengthOk1 ; fVALUE < fSIZE

syscallStringBuild1Resize1:

  ; fVALUE >= fSIZE
  lda fVALUEL
  sta fSIZEL
  lda fVALUEH
  sta fSIZEH
  
  ; string in IDX, required new length in fSIZE - new string returned in IDX (stack references updated)
  jsr utilityStringEnlarge
  
syscallStringBuild1LengthOk1:

  jsr stringUtilityLoadDestFromIDX
  clc
  lda fDESTINATIONADDRESSL
  adc fLENGTHL
  sta fDESTINATIONADDRESSL
  lda fDESTINATIONADDRESSH
  adc fLENGTHH
  sta fDESTINATIONADDRESSH
  
  jsr stringUtilityCopyChars   ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  
  lda fVALUEL
  sta fLENGTHL
  lda fVALUEH
  sta fLENGTHH
  jsr stringUtilityStoreLengthToIDX
  
  jsr releaseSPNEXT            ; release appendString
  
  jmp nextInstruction


; String.Build(ref string build) : set length = 0
syscall_2_StringBuild:
  jsr stringUtilityPopRefToIndex     ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX       ; ref string IDY -> IDX
  
  stz fLENGTHL                       ; set length = 0
  stz fLENGTHH
  jsr stringUtilityStoreLengthToIDX  ; fLENGTH -> (IDX), 2
  jmp nextInstruction

; String.Build(ref string build, char appendChar) : build = build + appendChar
syscall_1_StringBuild:
  jsr stringUtilityPopCharToACCL     ; appendChar -> ACCL
  jsr stringUtilityPopRefToIndex     ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX       ; ref string IDY -> IDX
  
  jsr stringUtilityAppendChar        ; string in IDX, appendChar in ACCL, returned IDX may have changed
  jmp nextInstruction


; String.BuildFront(ref string build, char insertChar) : build = insertChar + build
syscallStringBuildFront:
  jsr stringUtilityPopCharToACCL     ; appendChar -> ACCL
  jsr stringUtilityPopRefToIndex     ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX       ; ref string IDY -> IDX
  
  jsr stringUtilityInsertCharFront   ; string in IDX, insertChar in ACCL, returned IDX may have changed
  jmp nextInstruction


; string InsertChar(string this, uint index, char append)
syscallStringInsertChar:
  jsr stringUtilityPopCharToACCL      ; appendChar -> ACCL
  jsr stringUtilityPopUIntToIndex     ; pop uint argument -> IDY
  jsr stringUtilityPopStringToIDX     ; pop string argument -> IDX
  
  jsr stringUtilityLoadLengthFromIDX  ; (IDX), 2 -> fLENGTH
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  
  lda fLENGTHL                        ; how many characters for stringUtilityCopyChars to copy
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  
  jsr incLENGTH                       ; one more for 'appendChar'
  lda fLENGTHL
  sta fSIZEL
  lda fLENGTHH
  sta fSIZEH
  jsr stringUtilityCreateString       ; fSIZE is number of characters needed in string, returns string as IDX (sets default length value)
  
  jsr stringUtilityLoadDestFromIDX    ; string IDX, 4 -> fDESTINATIONADDRESS
  jsr stringUtilityCopyCharsWithInsert; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS, but insert ACCL at position IDY
  
  jsr releaseSP ; we popped 'this', decrease reference count
  lda #tString
  jmp pushIDXExit


; string Append(string this, char appendChar)
syscall_1_StringAppend:
  jsr stringUtilityPopCharToACCL      ; appendChar -> ACCL
  jsr stringUtilityPopStringToIDX     ; pop string argument -> IDX
  
  jsr stringUtilityLoadLengthFromIDX  ; (IDX), 2 -> fLENGTH
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  
  lda fLENGTHL                        ; how many characters for stringUtilityCopyChars to copy
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  
  jsr incLENGTH                       ; one more for 'appendChar'
  lda fLENGTHL
  sta fSIZEL
  lda fLENGTHH
  sta fSIZEH
  jsr stringUtilityCreateString       ; fSIZE is number of characters needed in string, returns string as IDX (sets default length value)
  
  jsr stringUtilityLoadDestFromIDX    ; string IDX, 4 -> fDESTINATIONADDRESS
  jsr stringUtilityCopyChars          ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  lda ACCL
  sta (fDESTINATIONADDRESS)           ; after stringUtilityCopyChars, fDESTINATIONADDRESS points to correct location to write appendChar
  
  jsr releaseSP ; we popped 'this', decrease reference count
  lda #tString
  jmp pushIDXExit


; string Append(string this, string appendString)
syscallStringAppend:
  ; appendString
  jsr stringUtilityPopStringToIDX     ; appendString -> IDX
  jsr stringUtilityLoadLengthFromIDX  ; (IDX), 2 -> fLENGTH
  lda fLENGTHL                        ; number of characters to append -> lCOUNT
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  jsr stringUtilityLoadSourceFromIDX  ; (IDX), 4 -> fSOURCEADDRESS
  lda fSOURCEADDRESSL
  pha
  lda fSOURCEADDRESSH
  pha
  
  ; this
  jsr stringUtilityPopStringToIDX     ; pop string argument -> IDX
  jsr stringUtilityLoadLengthFromIDX  ; (IDX), 2 -> fLENGTH
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  
  ; add length of 'appendString':  SIZE = LENGTH(this) + COUNT(append)
  clc
  lda lCOUNTL
  pha
  adc fLENGTHL
  sta fSIZEL
  iny
  lda lCOUNTH
  pha
  adc fLENGTHH
  sta fSIZEH
  
  lda fLENGTHL
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  
  jsr stringUtilityCreateString                 ; fSIZE is number of characters needed in string, returns string as IDX (sets default length value)

  jsr stringUtilityLoadDestFromIDX    ; string IDX, 4 -> fDESTINATIONADDRESS
  jsr stringUtilityCopyChars          ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  
  pla
  sta lCOUNTH
  pla
  sta lCOUNTL
  pla
  sta fSOURCEADDRESSH
  pla
  sta fSOURCEADDRESSL
  jsr stringUtilityCopyChars          ; copy lCOUNT chars from fSOURCEADDRESS to fDESTINATIONADDRESS
  
   ; we popped 'appendString', decrease reference count (munts all Nx variables if memoryFree is called)
  ; we popped 'this', decrease reference count (munts all Nx variables if memoryFree is called)
  jsr releaseSPandSPNEXT
  
  lda #tString
  jmp pushIDXExit


syscallStringPushImmediate:
  ; create and empty string
  stz fSIZEL
  stz fSIZEH
  jsr stringUtilityCreateString         ; fSIZE is number of characters needed in string, returns string as IDX (sets default length value)

syscallStringPushImmediateNotNull:
  jsr stringUtilityPopUIntToValue        ; appendChars -> fVALUE
  
  lda fVALUEL
  beq syscallStringPushImmediateTryMSB
  sta ACCL
  jsr stringUtilityAppendChar
  
syscallStringPushImmediateTryMSB:
  lda fVALUEH
  beq syscallStringPushImmediateExit
  sta ACCL
  jsr stringUtilityAppendChar
  
  bra syscallStringPushImmediateNotNull
syscallStringPushImmediateExit:
  
  lda #tString                         ; push resulting string
  jmp pushIDXExit




; string TrimLeft(string this) system;
syscallStringTrimLeft:
  jsr stringUtilityPopStringToIDY     ; this -> IDY
  
  ; IDY -> sourceString, returns cloned string in IDX
  jsr stringUtilityClone
  jsr stringUtilityTrimLeft
  
  jsr releaseSP
  lda #tString                         ; push resulting string
  jmp pushIDXExit


; TrimLeft(ref string build) system;
syscall_1_StringTrimLeft:
  jsr stringUtilityPopRefToIndex      ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX        ; ref string IDY -> IDX
  jsr stringUtilityTrimLeft
  jmp nextInstruction


; TrimRight(ref string build) system;
syscallStringTrimRight:
  jsr stringUtilityPopRefToIndex      ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX        ; ref string IDY -> IDX
  jsr stringUtilityTrimRight
  jmp nextInstruction


; Trim(ref string build) system;
syscall_1_StringTrim:
  jsr stringUtilityPopRefToIndex      ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX        ; ref string IDY -> IDX
  
  jsr stringUtilityTrimRight
  jsr stringUtilityTrimLeft
  jmp nextInstruction


; string Trim(string this) system;
syscallStringTrim:
  jsr stringUtilityPopStringToIDY     ; this -> IDY
  
  ; IDY -> sourceString, returns cloned string in IDX
  jsr stringUtilityClone

  jsr stringUtilityTrimRight
  jsr stringUtilityTrimLeft
  
  jsr releaseSP
  lda #tString                         ; push resulting string
  jmp pushIDXExit


; string Substring(string original, uint start)
syscallStringSubstring:
  jsr stringUtilityPopUIntToValue     ; start -> fVALUE
  jsr stringUtilityPopStringToIDY     ; this -> IDY
  
  ; IDY -> sourceString, returns cloned string in IDX
  jsr stringUtilityClone

  ; IDX source string, start position in fVALUE
  jsr stringUtilitySubstringStart
  
  jsr releaseSP
  lda #tString                         ; push resulting string
  jmp pushIDXExit


; Substring(ref string build, uint start) system;
syscall_2_StringSubstring:
  jsr stringUtilityPopUIntToValue     ; start -> fVALUE
  jsr stringUtilityPopRefToIndex      ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX        ; ref string IDY -> IDX
  
  ; IDX source string, start position in fVALUE
  jsr stringUtilitySubstringStart
  jmp nextInstruction


; string Substring(string original, uint start, uint length)
syscall_1_StringSubstring:
  jsr stringUtilityPopUIntToCount     ; length -> lCOUNT
  jsr stringUtilityPopUIntToValue     ; start -> fVALUE
  jsr stringUtilityPopStringToIDY     ; this -> IDY
  
  lda lCOUNTL
  pha
  lda lCOUNTH
  pha
  
  ; IDY -> sourceString, returns cloned string in IDX
  jsr stringUtilityClone

  ; IDX source string, start position in fVALUE
  jsr stringUtilitySubstringStart
  jsr stringUtilityLoadLengthFromIDX
  
  pla
  sta lCOUNTH
  pla
  sta lCOUNTL

  sec                                  ; fLENGTH = fLENGTH - lCOUNT
  lda fLENGTHL
  sbc lCOUNTL
  sta fLENGTHL
  lda fLENGTHH
  sbc lCOUNTH
  sta fLENGTHH
  bmi syscall_2_StringSubstringExit  ; -ve means the string length is already shorter than requested (so leave it unchanged)
  
  ; +ve means lCOUNT >= fLENGTH so take lCOUNT
  lda lCOUNTL
  sta fLENGTHL
  lda lCOUNTH
  sta fLENGTHH
  jsr stringUtilityStoreLengthToIDX
  
syscall_2_StringSubstringExit:
  
  jsr releaseSP
  lda #tString                         ; push resulting string
  jmp pushIDXExit


; ToUpper(ref string build) system;
syscall_1_StringToUpper:
  jsr stringUtilityPopRefToIndex      ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX        ; ref string IDY -> IDX
  
  jsr stringUtilityToUpper
  jmp nextInstruction


; string ToUpper(string this) system;
syscallStringToUpper:
  jsr stringUtilityPopStringToIDY     ; this -> IDY
  
  ; IDY -> sourceString, returns cloned string in IDX
  jsr stringUtilityClone

  jsr stringUtilityToUpper
  
  jsr releaseSP
  lda #tString                         ; push resulting string
  jmp pushIDXExit


; ToLower(ref string build) system;
syscall_1_StringToLower:
  jsr stringUtilityPopRefToIndex      ; ref build -> IDY
  jsr stringUtilityRefIDYtoIDX        ; ref string IDY -> IDX
  
  jsr stringUtilityToLower
  jmp nextInstruction


; string ToLower(string this) system;
syscallStringToLower:
  jsr stringUtilityPopStringToIDY     ; this -> IDY
  
  ; IDY -> sourceString, returns cloned string in IDX
  jsr stringUtilityClone

  jsr stringUtilityToLower
  
  jsr releaseSP
  lda #tString                         ; push resulting string
  jmp pushIDXExit

; int Compare(string left, string right) // returns -1, 0, +1
syscallStringCompare:
  ;
  ; int __cdecl strcmp (
  ;         const char * src,
  ;         const char * dst
  ;         )
  ; {
  ;         int ret = 0 ;
  ; 
  ;         while((ret = *(unsigned char *)src - *(unsigned char *)dst) == 0 && *dst)
  ;                 {
  ;                 ++src, ++dst;
  ;                 }
  ; 
  ;         return ((-ret) < 0) - (ret < 0); // (if positive) - (if negative) generates branchless code
  ; }
  jsr stringUtilityPopStringToIDY     ; right -> IDY
  jsr stringUtilityPopStringToIDX     ; left  -> IDX
  jsr stringUtilityLoadLengthFromIDX  ; IDX -> string, load fLENGTH
  lda fLENGTHL
  sta lCOUNTL
  lda fLENGTHH
  sta lCOUNTH
  jsr stringUtilityLoadLengthFromIDY  ; IDY -> string, load fLENGTH
  
  stz TOPL
  stz TOPH
  
  jsr stringUtilityLoadSourceFromIDX  ; string IDX, 4 -> fSOURCEADDRESS
  jsr stringUtilityLoadDestFromIDY    ; string IDY, 4 -> fDESTINATIONADDRESS

syscallStringCompareLoop:
  
  stz ACCL
  lda #0
  cmp lCOUNTL
  bne syscallStringCompareLeftNotNull
  cmp lCOUNTH
  beq syscallStringCompareLeftNull 
syscallStringCompareLeftNotNull:
  lda (fSOURCEADDRESS)
  sta ACCL
syscallStringCompareLeftNull:
  
  stz ACCH
  lda #0
  cmp fLENGTHL
  bne syscallStringCompareRightNotNull
  cmp fLENGTHH
  beq syscallStringCompareRightNull 
syscallStringCompareRightNotNull:
  lda (fDESTINATIONADDRESS)
  sta ACCH
syscallStringCompareRightNull:
  
  sec
  lda ACCL
  sbc ACCH
  sta TOPL
  bne syscallStringCompareLoopDone ; != 0
  ; == 0
  
  jsr decCOUNT
  jsr decLENGTH
  
  lda ACCH
  beq syscallStringCompareLoopDone 

  jsr incSOURCEADDRESS
  jsr incDESTINATIONADDRESS
  
  bra syscallStringCompareLoop

syscallStringCompareLoopDone:
  lda TOPL
  beq syscallStringCompareExit ; == 0
  bmi syscallStringCompareNeg  ; < 0
  
syscallStringComparePos:
  ; > 0 = 1
  lda #1
  sta TOPL
  bra syscallStringCompareExit

syscallStringCompareNeg:
  ; < 0 = -1
  lda #$FF
  sta TOPL
  sta TOPH

syscallStringCompareExit:
  jsr releaseSPandSPNEXT
  lda #tInt
  jmp pushTOPExit
  