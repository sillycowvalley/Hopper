; ######################## Char syscalls ########################

utilityGetCharArgument:
  ; gets the char in A and points X / (IDX/IDY) to the stack slot
  .ifdef STACK8
  ldx SP8
  dex
  dex
  lda HopperValueStack, X
  .else
  lda SPL
  sta IDXL
  lda SPH
  sta IDXH
  jsr decIDX
  jsr decIDX
  
  lda TSPL
  sta IDYL
  lda TSPH
  sta IDYH
  jsr decIDY
  
  lda (IDX)
  .endif
  rts
  
utilityBoolTrueExit:
  ; assumes that X / (IDX/IDY) are pointing to the stack slot
  lda #1
  
  .ifdef STACK8
  sta HopperValueStack, X
  lda #tBool
  sta HopperTypeStack, X
  .else
  sta (IDX)
  lda #tBool
  sta (IDY)
  .endif
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
utilityBoolFalseExit:
  ; assumes that X / (IDX/IDY) are pointing to the stack slot
  ifdef STACK8
  stz HopperValueStack, X
  lda #tBool
  sta HopperTypeStack, X
  .else
  lda #0
  sta (IDX)
  lda #tBool
  sta (IDY)
  .endif
  
  .ifdef CHECKED
  jsr verifyBoolStackTop
  .endif
  
  jmp nextInstruction
  
utilityCharExit:
  ; assumes that X / (IDX/IDY) are pointing to the stack slot
  ifdef STACK8
  sta HopperValueStack, X
  lda #tChar
  sta HopperTypeStack, X
  .else
  sta (IDX)
  lda #tChar
  sta (IDY)
  .endif
  
  jmp nextInstruction

syscallCharIsUpper:
  jsr utilityGetCharArgument
  cmp #'A'
  bcc notUpper
  cmp #'Z'+1
  bcs notUpper
  ; isUpper
  jmp utilityBoolTrueExit
notUpper:
  jmp utilityBoolFalseExit
  
syscallCharIsLower:
  jsr utilityGetCharArgument
  cmp #'a'
  bcc notLower
  cmp #'z'+1
  bcs notLower
  ; isLower
  jmp utilityBoolTrueExit
notLower:
  jmp utilityBoolFalseExit
  
syscallCharIsDigit:
  jsr utilityGetCharArgument
  cmp #'0'
  bcc notDigit ; <
  cmp #'9'+1
  bcs notDigit ; >=
  ; isDigit
  jmp utilityBoolTrueExit
notDigit:
  jmp utilityBoolFalseExit
  
  
syscallCharIsHexDigit:
  jsr utilityGetCharArgument
  cmp #'f'+1
  bcs notHexDigit ; >=
  cmp #'a'
  bcs isHexDigit  ; >=
  cmp #'F'+1
  bcs notHexDigit ; >=
  cmp #'A'
  bcs isHexDigit  ; >=
  cmp #'9'+1
  bcs notHexDigit ; >=
  cmp #'0'
  bcc notHexDigit ; <
isHexDigit:
  jmp utilityBoolTrueExit
notHexDigit:
  jmp utilityBoolFalseExit

syscallCharIsLetterOrDigit:
  jsr utilityGetCharArgument
  cmp #'z'+1
  bcs notLetterOrDigit ; >=
  cmp #'a'
  bcs isLetterOrDigit  ; >=
  cmp #'Z'+1
  bcs notLetterOrDigit ; >=
  cmp #'A'
  bcs isLetterOrDigit  ; >=
  cmp #'9'+1
  bcs notLetterOrDigit ; >=
  cmp #'0'
  bcc notLetterOrDigit ; <
isLetterOrDigit:
  jmp utilityBoolTrueExit
notLetterOrDigit:
  jmp utilityBoolFalseExit
  
syscallCharToUpper:
  jsr utilityGetCharArgument
  
  cmp #'a'
  bcc notLowerToUpper
  cmp #'z'+1
  bcs notLowerToUpper
  ; isLower
  and #$DF ; lower to upper ASCII
  jmp utilityCharExit
notLowerToUpper
  jmp nextInstruction ; NOP
  
syscallCharToLower:
  jsr utilityGetCharArgument
  
  cmp #'A'
  bcc notUpperToLower
  cmp #'Z'+1
  bcs notUpperToLower
  ; isUpper
  ora #$20 ; upper to lower ASCII
  jmp utilityCharExit
notUpperToLower
  jmp nextInstruction ; NOP
  
syscallCharToDigit:
  jsr utilityGetCharArgument
  clc
  adc #48 ; d = d + 48;
  jmp utilityCharExit
  
syscallCharToHex:
  jsr utilityGetCharArgument
  cmp #10
  bcc hexDigit ; < 10
  ; hexLetter
  clc
  adc #55 ; d = d + 55; (+'A' - 10)
  jmp utilityCharExit
hexDigit:
  clc
  adc #48 ; d = d + 48; (+'0')
  jmp utilityCharExit
  