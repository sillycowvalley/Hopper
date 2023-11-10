
ACIASERIAL    = 1
ACIAMOTOROLLA = 1
VIALCD        = 1
VIALED        = 1
;VIAKEYBOARD   = 1 ; TODO : implement keyboard syscalls
VIATIMER      = 1

; ######################## firmware constants (zero size) ########################
  .include constants.asm

; option features

HEAP = 1
  .ifdef HEAP
STRINGS = 1
LONGS = 1
ARRAYS = 1
LISTS = 1
  .ifdef STRINGS
DICTIONARIES = 1 ; Dictionaries are kinda pointless without Strings
  .endif
  .endif
  
FASTINTS = 1    ; faster 'int' and 'uint' multiply and divide (at a cost of 438 bytes)
                ; - multiply optimized for: 0, 1, 2, 4, 8, 16
                ; - divide optimized for:   1, 2, 4, 10, 50, 100

;PROFILE = 1     ; count opCode and sysCall hits
;CHECKED = 1     ; checked build (stack range checks, ..)
PERMISSIVE=1    ; additional level of CHECKED: allows uint for int or delegates
;MEMDEBUG = 1    ; memoryAllocate and memoryFree diagnostics
;MEMDEBUG2 = 1   ; for debugging gcRelease
;DICTDIAG = 1;
;VERIFYSTACK = 1 ; diagnostics for the 6502 SP
;NODIAGNOSTICS=1 ; selective removal of diagnostic code (for reduced size) - use for smaller builds!
;TESTINGHWM = 1  ; debugging maxing out the value stack
STACK8 = 1      ; 8 bit value stack pointer

VALUES = 1 ; testing value type direct storage in dictionaries and pairs

; .org $8000  ; physical ROM starts at $8000
; .org $A000  ; addressable ROM starts at $A000

  .org $8000  ; 32K EEPROM starts at $8000
  ;.org $E000  ; 8K EEPROM starts at $E000
               ; firmware is at $E000

  .org $8400    ; Hopper code, Hopper VM and Monitor is at $8800           

;HopperEntryPoint:
  jmp HopperStart

; ######################## Hopper source code ########################
  .include Defines.asm
  
  .ifdef HEAP
  .include Memory.asm
  .include GC.asm
  .endif
  .include Stacks.asm
  .include IncDec.asm
  .include Diagnostics.asm
  
  .include Shared.asm
  .include SysCalls.asm
  .include OpCodes.asm

HopperInitPC:
  ; initialize PC (X) with start index of program
  lda #<HopperData  ; LSB
  clc
  adc HopperData+4 
  sta PCL
  lda #>HopperData  ; MSB
  adc HopperData+5
  sta PCH
  rts

HopperInit:
  jsr stackInit
  .ifdef HEAP
  jsr memoryInit
  .endif
  
  lda #>HopperData
  sta CODESTART ; code page : first page where programs are loaded
  
  .ifdef HEAP
  stz COPYNEXTPOP
  .endif
  stz FLAGS
  
  .ifdef CHECKED
  smb2 FLAGS ; this is the CHECKED build
  .endif
  .ifdef STACK8
  smb3 FLAGS ; 8 bit stack pointer and base pointer
  .endif
  .ifdef PROFILE
  smb4 FLAGS ; this is a PROFILE build
  .endif
  
  .ifdef TESTINGHWM  
  stz HWML
  stz HWMH
  .endif
  
  .ifdef PROFILE
  
  stz IDXL
  lda #>HopperOpProfile
  sta IDXH
  ldx #2
  jsr clearPages ; with IDX (memory location) and X (number of pages) initialized
  
  stz IDXL
  lda #>HopperSysProfile
  sta IDXH
  ldx #2
  jsr clearPages ; with IDX (memory location) and X (number of pages) initialized
  
  .endif
  
  jsr HopperInitPC
  
  rts
  
HopperStart:
  jmp nextInstructionNoInc
 
nextInstruction:
  
  inc PCL                      ; 5
  bne incPCnextInstructionEnd  ; 2-4
  inc PCH                      ; 5
incPCnextInstructionEnd:
  
nextInstructionNoInc:

  .ifdef VERIFYSTACK
  jsr diagnosticsVerifyStack
  .endif

  bbr1 FLAGS, breakCheck       ; 5 - warp speed - ignore <ctrl><C> and trace, don't check for illegal opcodes, ignore breakpoints

HopperStartNoBreak:            ; in case <F5> is sitting at a breakpoint

  ; warp speed:
  lda (PC)                     ; 5 - get next instruction
  
  .ifdef STACK8
  
  .ifdef PROFILE
  jsr incHopperOpProfile
  .endif
  
  asl                          ; 2 - 2 byte jump table entries
  tax                          ; 2
  jmp ( opCodeJumpTable, X)    ; 6
  
  .else
  
  jmp traceFlagOff             ; 3 - skip <ctrl><C> check and trace check
  
  .endif
  
breakCheck:
  lda SerialBreakFlag          ; 3
  beq breakCheckExit           ; 3  - if not, exit
  
  ; it was non-zero so <ctrl><C>, consume it
  sei
  dec SerialBreakFlag
  cli
  
  jmp monitorReEntry

breakCheckExit:

  bbr5 FLAGS, noBreakPointsAreSet ; 6 
  
  ; see if PC is at a breakpoint or not
  ldx #0
nextBreakPoint:
  lda BRKH, X
  cmp PCH
  bne notAtThisBreakPoint
  lda BRKL, X
  cmp PCL
  bne notAtThisBreakPoint
  ; we hit a breakpoint
  cpx #0
  bne notNextStepBreak
  ; if it was BRK 0, we need to clear it (next step)
  stz BRKL
  stz BRKH
  jsr checkForBreakPoints ; reset FLAG bit 5
notNextStepBreak:

  jmp monitorReEntry
  
notAtThisBreakPoint:
  inx
  cpx #$10
  bne nextBreakPoint

noBreakPointsAreSet:

  bbr6 FLAGS, afterSingleStepCheck
  rmb6 FLAGS ; clear the single step bit
  
  jmp monitorReEntry
  
afterSingleStepCheck:

  lda (PC)                     ; 5 - get next instruction
  
  .ifdef PROFILE
  jsr incHopperOpProfile
  .endif

  bbr0 FLAGS, traceFlagOff     ; 6 
  ;jsr diagnosticOutMini
  jsr diagnosticOut
  ;jsr diagnosticCheckStack
  ;jsr diagnosticOutStack
  ;jsr diagnosticOutCallStack
traceFlagOff:

  
; ######################## OpCode lookups - first level ########################

  ; http://6502.org/tutorials/compare_instructions.html
  .ifdef CHECKED
  cmp #$6A                     ; C will be set if A >= $6A (unknown OpCode)
  bcs unknownOpCode            ; 0x6A..0xFF
  .endif
  
  .ifdef STACK8
  
  asl                          ; 2 - 2 byte jump table entries
  tax                          ; 2
  jmp ( opCodeJumpTable, X)    ; 6
  
  .else
  
  
  cmp #$1A                 ; 2 -   C will be set if A >= $1A
  bcs jumpOpCode           ; 2-4 - 0x1A..0x69: byteOperand, wordOperand & noOperand 
  cmp #$11                 ; 2 -   C will be clear if A < $11
  bcc pop2Push1Unsigned    ; 2-4 - 0x00..0x10
  ; bra pop2Push1Signed    ;       0x11..0x19
  
  ; fall through

;pop2Push1Signed:   ; A = $11..$19
  asl
  tax

  ; top
  jsr popTOPInt
  
  ; next
  jsr popNEXTInt
  
  jmp ( opCodeJumpTable, X)


pop2Push1Unsigned: ; A = $00..$10
  
  asl
  tax
  
  ; top
  .ifdef CHECKED
  
  jsr decTSPnoA
  
; too many operations are ok with tInt like: EQ, NE, BITAND, BITOR, BITSHL, BITSHR  
;  lda (TSP)
;  cpx #$0A ; EQ<<1
;  beq skipUintAssert1
;  cpx #$0C ; NE<<1
;  beq skipUintAssert1
;  jsr assertUInt
;skipUintAssert1:
  
  jsr decSPnoA       ; MSB
  lda (SP)
  sta TOPH
  
  jsr decSPnoA       ; LSB
  lda (SP)
  sta TOPL
    
  ; next
  jsr decTSPnoA
  
; too many operations are ok with tInt like: EQ, NE, BITAND, BITOR, BITSHL, BITSHR
;  lda (TSP)
;  cpx #$0A ; EQ<<1
;  beq skipUintAssert2
;  cpx #$0C ; NE<<1
;  beq skipUintAssert2
;  cpx #$20 ; BITSHR<<1
;  beq skipUintAssert2
;  jsr assertUInt
;skipUintAssert2:

  jsr decSPnoA       ; MSB
  lda (SP)
  sta NEXTH
  jsr decSPnoA       ; LSB
  lda (SP)
  sta NEXTL
  
  .else
  
  ; decTSP x2
  sec
  lda TSPL
  sbc #2
  sta TSPL
  bcs decTSPpop2Push1Unsigned
  dec TSPH
decTSPpop2Push1Unsigned:

  ; decSP x4
  sec
  lda SPL
  sbc #4
  sta SPL
  bcs decTSPpop2Push1Unsigned2
  dec SPH
decTSPpop2Push1Unsigned2:

  ldy #3
  lda (SP), Y
  sta TOPH
  dey
  lda (SP), Y
  sta TOPL
  dey
  lda (SP), Y
  sta NEXTH
  lda (SP)
  sta NEXTL
  
  .endif
  
  jmp ( opCodeJumpTable, X)
  
jumpOpCode:  
  asl
  tax
  
  jmp ( opCodeJumpTable, X)
  .endif

  
unknownOpCode:
  .ifndef NODIAGNOSTICS
  jsr diagnosticOutNewLine
  jsr diagnosticOutHex
  jsr diagnosticOutString
  .byte "?opCode", 0
  .endif
  jmp throwToys
  
; ######################## end of OpCode lookups ########################

clearBreakPoints:
  ldx #1
clearNextBreakPoint:
  stz BRKH, X
  stz BRKL, X
  inx
  cpx #$10
  bne clearNextBreakPoint
  
  ; fall through to check if breakpoint #0 is set

checkForBreakPoints:
  ldx #0
checkNextBreakPoint:
  lda BRKH, X
  bne breakpointsExist
  lda BRKL, X
  bne breakpointsExist
  inx
  cpx #$10
  bne checkNextBreakPoint
  
  rmb5 FLAGS ; no breakpoints
  rts
  
breakpointsExist:
  
  smb5 FLAGS ; at least one breakpoint
  rts
  


sendSlash:
  lda #$5C ; "\"
  jsr SerialOut ; sends the character in A out the RS232 port
  rts
  
  
waitForChar:
  jsr SerialInAvailable ; is there a character available in the buffer?
  beq waitForChar
  jsr SerialInConsume  ; consume it returning the character in A
  rts
  
emptySerialBuffer:
  jsr SerialInAvailable ; is there a character available in the buffer?
  beq bufferEmptied
  jsr SerialInConsume
  bra emptySerialBuffer
bufferEmptied:
  rts

monitorReEntryResetPC:
  jsr HopperInitPC
  ; fall through
monitorReEntry:
  ; soft monitor restart when coming back from Hopper VM (either from end of program or from <ctrl><C>)
  jsr sendSlash
  
  ldx #$ff         ; reset 6502 stack pointer - assume we may have had a forced exit
  txs
  
  jmp monitorEntry

resetVector: ; main entry point from RESET vector

  sei        ; disable interrupts
  cld        ; clear decimal arithmetic mode.
  
  
  ; clear zero page to zeros:
  lda #0
  ldy #0
clearZeroPage:
  sta $0000, Y
  dey
  bne clearZeroPage
  
  ; clear system stack to zeros:
  ldy #0
clearSystemStack:
  sta $0100, Y
  dey
  bne clearSystemStack
  
  ; clear key memory pages to zeros:
  ;   0x0200: keyboard buffer,
  ;   0x0300: serial buffer,
  ;   0x0400: call stack, 
  ;   0x0500: type stack, 
  ;   0x0600 (and possibly 0x700): value stack
  
  stz IDXL
  lda #$02
  sta IDXH
  
  .ifdef STACK8
  ldx #5
  .else
  ldx #6
  .endif
  jsr clearPages ; with IDX (memory location) and X (number of pages) initialized
    
  ldx #$ff         ; reset stack pointer
  txs
  
  ; good default heap until we load a program
  .ifdef HEAP
  lda #$60
  sta HEAPSTART
  lda #$20
  sta HEAPSIZE
  .endif
  
  lda #%10000000
  sta DDRA   ; LCD: set all pins on port A to input (good default in case we don't use it)
  smb7 PORTA ; turn the built-in LED on
  
  jsr Initialize ; initialize firmware (cold and warm starts)
  
  .ifdef VIALCD
  jsr LCDString
  .byte "HopperMon:"
  .byte 0
  jsr LCDNewLine
  .endif
  
  jsr HopperInit ; good defaults for HopperVM
  
  ; cold restart - try to free up HopperMon
  jsr sendSlash
  
  ;lda #$5C
  ;jsr LCDCharacter

monitorEntry:
  ; soft monitor restart after each monitor command
  jsr waitForChar
  
  cmp #$1B
  bne notConfirmEscapeSignal
  jmp confirmEscapeSignal
notConfirmEscapeSignal:
  
  cmp #"L"
  bne notLoadCommand
  jmp loadCommand
notLoadCommand:
  
  cmp #"M"
  bne notMemoryCommand
  jmp memoryCommand
notMemoryCommand:

  cmp #"F"
  bne notFastMemoryCommand
  jmp fastMemoryCommand
notFastMemoryCommand:

  cmp #"B"
  bne notBreakpointCommand
  jmp breakpointCommand
notBreakpointCommand:
  
  cmp #"X"
  bne notExecuteCommand
  smb1 FLAGS ; turn Warp on
  jmp executeCommand
notExecuteCommand:

  cmp #"D"
  bne notDebugCommand
  rmb1 FLAGS ; turn Warp off (respond to <ctrl><C> and breakpoints)
  jmp executeCommand
notDebugCommand:

  cmp #"I"
  bne notStepIntoCommand
  rmb1 FLAGS ; turn Warp off (respond to <ctrl><C> and breakpoints)
  jmp singleStepCommand
notStepIntoCommand:

  cmp #"O"
  bne notStepOverCommand
  rmb1 FLAGS ; turn Warp off (respond to <ctrl><C> and breakpoints)
  jmp nextStepCommand
notStepOverCommand:
  
  cmp #"W"
  bne notWarmrestartCommand
  jmp warmrestartCommand
notWarmrestartCommand:
  
  cmp #"R"
  bne notRegistersCommand
  jmp registersCommand
notRegistersCommand:

  cmp #"P"
  bne notPCCommand
  jmp pcCommand
notPCCommand:
  
  cmp #"V"
  bne notStackCommand
  jmp stackCommand
notStackCommand:
  cmp #"C"
  bne notCallStackCommand
  jmp callStackCommand
notCallStackCommand:

  ifdef HEAP
  cmp #"H"
  bne notHeapCommand
  jmp heapCommand
notHeapCommand:
  .endif ; HEAP

  bra monitorEntry
  
confirmEscapeSignal:
  jsr sendSlash
  jmp monitorEntry
  
  .ifdef HEAP
initializeHeapSize:
  ; assumes that:
  ; - entire program was loaded at HopperData (typically $1000)
  ; - size in bytes of loaded program is in IDY
  
  clc
  lda #>HopperData
  adc IDYH
  sta HEAPSTART
  inc HEAPSTART ; next page after program
  
  ; heap start
  lda #" "
  jsr diagnosticOutChar
  lda HEAPSTART
  jsr diagnosticOutHex
  lda #0
  jsr diagnosticOutHex
  
  sec
  lda #$80
  sbc HEAPSTART
  sta HEAPSIZE
  
  ; heap size
  lda #" "
  jsr diagnosticOutChar
  lda HEAPSIZE
  jsr diagnosticOutHex
  lda #0
  jsr diagnosticOutHex
  
  rts
  .endif
  
; http://www.6502.org/source/monitors/intelhex/intelhex.htm
makeNibble:
  ; only touches A
  cmp     #'9'+1  	  ; See if it's 0-9 or 'A'..'F' (no lowercase yet)
  bcc     makeNibbleH ; If we borrowed, we lost the carry so 0..9
  sbc     #7+1    	  ; Subtract off extra 7 (sbc subtracts off one less)
; If we fall through, carry is set unlike direct entry at MKNNH
makeNibbleH:
  sbc     #'0'-1  	; subtract off '0' (if carry clear coming in)
  and     #$0F    	; no upper nibble no matter what
  rts             	; and return the nibble

loadNextCharacter:
  jsr waitForChar
  ;jsr SerialOut ; echo the data back for now
  rts
  
loadRecord:

  jsr loadNextCharacter
  cmp #":"
  beq loadGoodPrefix
  jmp loadRecordFailure
  
loadGoodPrefix:
  jsr loadNextCharacter
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCL
  jsr loadNextCharacter
  jsr makeNibble
  ora ACCL      
  sta ACCL ; ACCL contains record length
  
  jsr loadNextCharacter
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta IDXH
  jsr loadNextCharacter
  jsr makeNibble
  ora IDXH
  sta IDXH
  
  jsr loadNextCharacter
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta IDXL
  jsr loadNextCharacter
  jsr makeNibble
  ora IDXL      
  sta IDXL 
  
  clc
  lda IDXL
  adc #<HopperData
  sta IDXL
  lda IDXH
  adc #>HopperData
  sta IDXH
  
  ; IDX contains the destination address
  
  ;jsr diagnosticOutNewLine
  ;lda IDXH
  ;jsr diagnosticOutHex
  ;lda IDXL
  ;jsr diagnosticOutHex
  
  jsr loadNextCharacter
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCH
  jsr loadNextCharacter
  jsr makeNibble
  ora ACCH ; A contains record type    

  cmp #$01 ; EOF record
  beq loadRecordSuccessEOF
  cmp #$00 ; data record
  bne loadRecordFailure ; what's this?
  
  ; load the data
  ldy #0
  ldx ACCL
loadRecordNext:
  cpx #0
  beq loadRecordDone
  
  jsr loadNextCharacter
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCL
  jsr loadNextCharacter
  jsr makeNibble
  ora ACCL ; A contains data byte
  
  sta (IDX), Y
  
  jsr incIDY
  
  iny
  dex
  bra loadRecordNext
loadRecordDone:
  ; ignore data checksum and EOL for now : TODO
  jsr loadNextCharacter
  jsr loadNextCharacter
  jsr loadNextCharacter
  lda #$00 ; not '!' or '*'
  rts
loadRecordSuccessEOF:  
  ; ignore EOF checksum and EOL : TODO
  jsr loadNextCharacter
  jsr loadNextCharacter
  jsr loadNextCharacter
  
  jsr diagnosticOutNewLine
  lda IDYH
  jsr diagnosticOutHex
  lda IDYL
  jsr diagnosticOutHex
  
  lda #$2A ; '*' success terminator
  rts
loadRecordFailure:
  jsr emptySerialBuffer ; discard input after the error
  lda #$21 ; '!' failure terminator
  rts

loadCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  stz IDYH
  stz IDYL
loadNextRecord:
  jsr loadRecord
  cmp #$2A ; '*' success terminator
  beq loadCommandDoneGood
  cmp #$21 ; '!' failure terminator
  beq loadCommandDone
  bra loadNextRecord
  
loadCommandDoneGood:

  .ifdef HEAP
  jsr initializeHeapSize
  .endif
  
  jsr HopperInit ; good defaults for HopperVM
  jsr diagnosticOutNewLine
  lda #$2A       ; munted by HopperInit
loadCommandDone:
  jsr SerialOut ; success or failure ('*' or '!')?
  jsr sendSlash ; confirm the data
  
  jmp monitorEntry
  
  
pcCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  lda #$0D
  jsr SerialOut
  lda PCH
  jsr SerialHexOut
  lda PCL
  jsr SerialHexOut
  
  jsr sendSlash ; confirm the data
  jmp monitorEntry
  
registersCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  jsr diagnosticOut
  jsr sendSlash ; confirm the data
  jmp monitorEntry
  
warmrestartCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  jsr HopperInit ; good defaults for HopperVM (but don't clear program)
  
  jmp monitorEntry

executeCommand:   ; "X" | <ctrl><F5> (Warp)    and    "D" | <F5>  (no Warp)
  jsr waitForChar ; consume <enter>
  jsr sendSlash   ; confirm the command
  
  jmp HopperStartNoBreak

singleStepCommand: ; "I" | <F11>
  jsr waitForChar  ; consume <enter>
  jsr sendSlash    ; confirm the command
  
useSingleStep:
  smb6 FLAGS ; set the single step bit
  
  jmp afterSingleStepCheck
  
nextStepCommand:  ; "O" | <F10>
  jsr waitForChar ; consume <enter>
  jsr sendSlash   ; confirm the command
  
  lda (PC)
  cmp #0x34 ; CALLW
  bne useSingleStep
  
  clc
  lda #03
  adc PCL
  sta BRKL
  lda PCH
  adc #0
  sta BRKH
  
  smb5 FLAGS ; breakpoint/s exist
  
  jmp nextInstructionNoInc
  
stackCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  jsr diagnosticOutStack
  
  jsr sendSlash ; confirm the data
  jmp monitorEntry

callStackCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  jsr diagnosticOutCallStack
  
  jsr sendSlash ; confirm the data
  jmp monitorEntry

  .ifdef HEAP
heapCommand:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  jsr memoryHeapWalk
  
  jsr sendSlash ; confirm the data
  jmp monitorEntry
  .endif
  
breakpointCommand:
  jsr waitForChar ; consume hex character, , only touches A
  cmp #"X"
  bne setBreakpoint
  jsr clearBreakPoints
  bra breakpointsDone
setBreakpoint:
  jsr makeNibble ; only touches A
  tax
  
  jsr waitForChar ; consume hex character, only touches A
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCL
  jsr waitForChar ; consume hex character
  jsr makeNibble
  ora ACCL
  sta BRKH, X
  
  jsr waitForChar ; consume hex character, only touches A
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCL
  jsr waitForChar ; consume hex character
  jsr makeNibble
  ora ACCL
  sta BRKL, X
  
  smb5 FLAGS ; at least one breakpoint now
  
  ; fall through
breakpointsDone:
  jsr waitForChar ; consume <enter>
  jsr sendSlash ; confirm the command
  
  jmp monitorEntry
  
memoryCommand:
  
  jsr waitForChar ; consume hex character
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCL
  
  jsr waitForChar ; consume hex character
  jsr makeNibble
  ora ACCL      ; A now contains the page number for the memory dump
  pha
  
  jsr waitForChar ; consume <enter>
  
  jsr sendSlash ; confirm the command
  
  pla
  jsr diagnosticPageMemory
  
  jsr sendSlash ; confirm the data
  
  jmp monitorEntry

fastMemoryCommand:
  
  jsr waitForChar ; consume hex character
  jsr makeNibble
  asl
  asl
  asl
  asl
  and #$F0
  sta ACCL
  
  jsr waitForChar ; consume hex character
  jsr makeNibble
  ora ACCL      ; A now contains the page number for the memory dump
  pha
  
  jsr waitForChar ; consume <enter>
  
  jsr sendSlash ; confirm the command
  
  pla
  jsr diagnosticFastPageMemory
  
  jsr sendSlash ; confirm the data
  
  jmp monitorEntry


  .org $FFFA


; ######################## firmware code (includes code, data tables, ISRs and vectors) ########################
  
  .include firmware.asm
