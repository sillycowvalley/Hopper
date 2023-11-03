
; Dictionary slots
D0 = $70;
D1 = $71;
D2 = $72;
D3 = $73;
D4 = $74;
D5 = $75;
D6 = $76;
D7 = $77;
D8 = $78;
D9 = $79;
D10 = $7A;
D11 = $7B;
D12 = $7C;
D13 = $7D;
D14 = $7E;
D15 = $7F;

; Heap manager slots (memoryAllocate and memoryFree)
M0   = $80
M1   = $81
M2   = $82
M3   = $83
M4   = $84
M5   = $85
M6   = $86
M7   = $87
M8   = $88
M9   = $89
M10  = $8A 
M11  = $8B
M12  = $8C
M13  = $8D
M14  = $8E
M15  = $8F

; General slots used by opCode and sysCall functions only.
; Illegal to modify these outside of opCode and sysCall functions and their helpers
F0 = $90
F1 = $91
F2 = $92
F3 = $93
F4 = $94
F5 = $95
F6 = $96
F7 = $97
F8 = $98
F9 = $99
F10 = $9A
F11 = $9B
F12 = $9C
F13 = $9D
F14 = $9E
F15 = $9F

; Extra slots only used by utilityMUL (Dictionaries, opcodeMUL, opcodeMULI), syscallLongDiv, 
;                                      syscallLongMod and syscallLongMul so far
; memoryHeapWalk also uses these slots so it can be safely called from anywhere (except
; math functions mentioned above)
U0  = $A0
U1  = $A1
U2  = $A2
U3  = $A3
U4  = $A4
U5  = $A5
U6  = $A6
U7  = $A7
U8  = $A8
U9  = $A9
U10 = $AA
U11 = $AB

; program counter: 8 bit slots
PC   = $B0
PCL  = $B0
PCH  = $B1

  .ifdef STACK8
; value stack: 16 bit slots
SP8 = $B2

; stack base pointer for current method  
BP8 = $B6

  .else
; value stack: 16 bit slots
SP   = $B2
SPL  = $B2
SPH  = $B3

; type stack: 8 bit slots (shadow the value stack)
TSP   = $B4
TSPL  = $B4
TSPH  = $B5

; stack base pointer for current method
BP   = $B6
BPL  = $B6
BPH  = $B7
  .endif

; call stack: 16 bit return address slots
CSP  = $B8

; copy on assignment for reference types
  .ifdef HEAP
COPYNEXTPOP  = $BA
  .endif
  


; breakpoints (0..15)
BRKL = $50 ; 0x50..0x5F
BRKH = $60 ; 0x60..0x6F

; system settings
; bit 0 = Trace On/Off
; bit 1 = WarpSpeed On/Off (ignore Trace and <ctrl><C>)
; bit 2 = this is the CHECKED build
; bit 3 = we are running on an 8 bit stack (8 bit SP and 8 bit BP)
; bit 4 = this is a PROFILE build
; bit 5 = breakpoints exist
; bit 6 = single step mode
; bit 7 = MCU device
FLAGS  = $BB 

; location to store PC to when running user code on the VM
SPC   = $BC
SPCL  = $BD
SPCH  = $BE

; 16 bit accumulator
ACC  = $C0
ACCL = $C0
ACCH = $C1

; 16 bit popped stack argument
TOP   = $C2
TOPL  = $C2
TOPH  = $C3

; 16 bit popped stack argument
NEXT  = $C4
NEXTL = $C4
NEXTH = $C5

; 16 bit index register
IDX  = $C6
IDXL = $C6
IDXH = $C7

; 16 bit index register
IDY  = $C8
IDYL = $C8
IDYH = $C9

; start address page of Hopper programs
; (this doesn't need to be on zero page - only used by "Z" command
;  to pass #>HopperData back to monitor)
CODESTART = $CA

  .ifdef TESTINGHWM
HWM  = $CB
HWML = $CB
HWMH = $CC
  .endif
  
  .ifdef VERIFYSTACK
DIAGSP = $DF ; only used for diagnostics on the 6502 SP
  .endif
  
  .ifdef HEAP
; 16 bit memory allocator free list address
FREELIST  = $E8
FREELISTL = $E8
FREELISTH = $E9

HEAPSTART = $EA ; first page of Hopper heap
HEAPSIZE  = $EB ; size in pages of Hopper heap 
  .endif


; ######################## Hopper Types ########################

tChar = 1 ; token tUInt at runtime (except perhaps for array elements)
tInt  = 2
tByte = 3 ; token tUInt at runtime (except perhaps for array elements)
tUInt = 4
tReference = 5
tBool = 6 ; token tUInt at runtime (except perhaps for array elements)
tEnum = 7
tFlags = 8
tDelegate = 11   ; 0x0B
tType = 12       ; 0x0C

; types above here are not GCd (no reference counting)

tHeapTypes = 13  ; 0x0D
  .ifdef UNUSED
tFloat = 13
  .endif
  .ifdef LONGS
tLong = 14       ; 0x0E
  .endif
  .ifdef STRINGS
tString = 15     ; 0x0F
  .endif
  .ifdef DICTIONARIES
tPair = 16       ; 0x10
  .endif

  .ifdef ARRAYS
tArray = 18      ; 0x12
  .endif
  .ifdef DICTIONARIES
tDictionary = 19 ; 0x13
  .endif
  .ifdef HEAP
tVariant    = 20 ; 0x14
  .endif
;tFile = 21
;tDirectory = 22
  .ifdef LISTS
tList = 25       ; 0x19
  .endif
