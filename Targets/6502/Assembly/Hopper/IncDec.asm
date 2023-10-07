incPC:
  inc PCL
  bne incPCEnd
  inc PCH
incPCEnd:
  rts

decPC:
  pha
  lda PCL
  bne decPCSkipMSB
  dec PCH
decPCSkipMSB:
  dec PCL
  pla
  rts






decIDX:
  pha
  lda IDXL
  bne decIDXSkipMSB
  dec IDXH
decIDXSkipMSB
  dec IDXL
  pla
  rts

incIDY:
  inc IDYL
  bne incIDYEnd
  inc IDYH
incIDYEnd:
  rts

decIDY:
  pha
  lda IDYL
  bne decIDYSkip
  dec IDYH
decIDYSkip:
  dec IDYL
  pla
  rts

  
  .ifdef HEAP ; used in STRINGS, LONGS, etc

incACC:
  inc ACCL
  bne incACCEnd
  inc ACCH
incACCEnd:
  rts

incSIZE:
  inc fSIZEL
  bne incSIZEEnd
  inc fSIZEH
incSIZEEnd:
  rts
  


decSIZE:
  pha
  lda fSIZEL
  bne decSIZESkipMSB
  dec fSIZEH
decSIZESkipMSB
  dec fSIZEL
  pla
  rts


incLENGTH:
  inc fLENGTHL
  bne incLENGTHEnd
  inc fLENGTHH
incLENGTHEnd:
  rts

decLENGTH:
  pha
  lda fLENGTHL
  bne decLENGTHSkip
  dec fLENGTHH
decLENGTHSkip:
  dec fLENGTHL
  pla
  rts
  
decACC:
  pha
  lda ACCL
  bne decACCSkip
  dec ACCH
decACCSkip:
  dec ACCL
  pla
  rts
  
incIDX:
  inc IDXL       ; 5
  bne incIDXEnd  ; 2-4
  inc IDXH       ; 5
incIDXEnd:
  rts            ; 6
  
incDESTINATIONADDRESS:
  inc fDESTINATIONADDRESSL
  bne incDESTINATIONADDRESSEnd
  inc fDESTINATIONADDRESSH
incDESTINATIONADDRESSEnd:
  rts

decDESTINATIONADDRESS:
  pha
  lda fDESTINATIONADDRESSL
  bne decDESTINATIONADDRESSSkip
  dec fDESTINATIONADDRESSH
decDESTINATIONADDRESSSkip:
  dec fDESTINATIONADDRESSL
  pla
  rts

incSOURCEADDRESS:
  inc fSOURCEADDRESSL
  bne incSOURCEADDRESSEnd
  inc fSOURCEADDRESSH
incSOURCEADDRESSEnd:
  rts
  
decSOURCEADDRESS:
  pha
  lda fSOURCEADDRESSL
  bne decSOURCEADDRESSSkip
  dec fSOURCEADDRESSH
decSOURCEADDRESSSkip:
  dec fSOURCEADDRESSL
  pla
  rts
  .endif

  .ifdef STRINGS 
incCOUNT: ; also DICTIONARIES but they use STRINGS too so ..
  inc lCOUNTL
  bne incCOUNTEnd
  inc lCOUNTH
incCOUNTEnd:
  rts
  
incNEXT: ; STRINGS
  inc NEXTL
  bne incNEXTEnd
  inc NEXTH
incNEXTEnd:
  rts
  
incTOP: ; STRINGS
  inc TOPL
  bne incTOPEnd
  inc TOPH
incTOPEnd:
  rts
  .endif ; STRINGS
