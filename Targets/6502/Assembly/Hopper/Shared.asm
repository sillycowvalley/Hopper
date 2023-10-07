; ######################## Shared across type functions ########################

  .include Int.asm
  .include UInt.asm
  .include Char.asm
  .ifdef HEAP
  .include Variant.asm
  .endif
  
  .ifdef STRINGS
  .include String.asm
  
  .ifdef DICTIONARIES
  .include Dictionary.asm
  .include Pair.asm
  .endif  
  
  .endif
  
  .ifdef LONGS
  .include Long.asm
  .endif
  
  .ifdef ARRAYS
  .include Array.asm
  .endif
  
  .ifdef LISTS
  .include List.asm
  .endif
  
  
  

; These are shared across type sysCalls and opCodes that don't call each other:

fSIGN = F0

fSIZE = F1
fSIZEL = F1
fSIZEH = F2

; used by strings, long, dictionaries and arrays

fSOURCEADDRESS  = F3
fSOURCEADDRESSL = F3
fSOURCEADDRESSH = F4

fDESTINATIONADDRESS  = F5
fDESTINATIONADDRESSL = F5
fDESTINATIONADDRESSH = F6

fTYPE = F7

fLENGTH = F8
fLENGTHL = F8
fLENGTHH = F9

fVALUE = F10
fVALUEL = F10
fVALUEH = F11

; used by long:
; long uses fSIGN, fSIZE and fSOURCEADDRESS: F0..F4
lTOP0 = F5
lTOP1 = F6
lTOP2 = F7
lTOP3 = F8

lNEXT0 = F9
lNEXT1 = F10
lNEXT2 = F11
lNEXT3 = F12

; used by syscallLongDiv, syscallLongMod, syscallLongMul:
lRESULT0 = U0
lRESULT1 = U1
lRESULT2 = U2
lRESULT3 = U3
lRESULT4 = U4
lRESULT5 = U5
lRESULT6 = U6
lRESULT7 = U7

; used in Diagnostics.asm
dSCRATCH = U0
dSCRATCHL = U0
dSCRATCHH = U1

; used by lists:

lPREVIOUS  = F6
lPREVIOUSL = F6
lPREVIOUSH = F7

; preserved during recursive clone calls
lNEXT  = F8
lNEXTL = F8
lNEXTH = F9

; preserved during recursive clone calls
lCURRENT  = F10 
lCURRENTL = F10
lCURRENTH = F11

fITEM  = F12
fITEML = F12
fITEMH = F13

lCOUNT  = F14
lCOUNTL = F14
lCOUNTH = F15

; used by arrays

aCARRY   = F14
aBITMASK = F15


