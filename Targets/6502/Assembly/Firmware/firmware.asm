; ######################## firmware initialization ########################
;
; Note about clock speed: several drivers have hard coded delays in them
; that assume a clock speed of 1 mHz. These include:
; - LCDInitialize (waiting for 4 bit mode to kick in)
; - TimerInitialize (VIA interval timer to tick every 1ms or 1000 clock cycles)
; - SerialOut (delay to overcome the WDC Tx bug that makes assumptions about clock speed)

  ;.org $E000  ; firmware at $E000 for now (last 8K)
  .org $8000  ; firmware at $8000 for 32K

Initialize:
  
  .ifdef UNOSERIAL
  sta UNOSTATUS    ; soft reset (value not important)
  
  lda #$0A       ; new line
  sta UNODATA
  lda #"O"
  sta UNODATA
  lda #"K"
  sta UNODATA
  lda #$0A       ; new line
  sta UNODATA
  
  .endif
  
  
  ; VIA initialization assuming the RESB pulse was too short
  ; "Reset clears all internal registers (except T1 and T2 counters and latches, and the SR"
  
  lda #%10000000
  sta DDRA ; LCD: set all pins on port A to input (good default in case we don't use it)
           ;      except for PA7 which is the built-in LED
  lda #%00000000
  sta DDRB ; PS/2 Keyboard: set all pins on port B to input
  
  rmb7 PORTA ; turn the built-in LED off
  nop ; we fail to call LCDInitialize without this nop!?!
  
  .ifdef VIALCD 
  jsr LCDInitialize
  jsr LCDClear ; initializes LCDColumn and LCDRow to zero and clears the screen
  .else
  .ifdef VIALED
  jsr LEDInitialize
  .endif
  .endif
  
  .ifdef VIAKEYBOARD
  jsr KeyboardInitialize
  .endif
  
  
  .ifdef ACIASERIAL
  jsr SerialInitialize
  .endif
  
  .ifdef VIATIMER
  jsr TimerInitialize ; start timer last since it generates interrupts
  .endif
  
  cli            ; enable interrupts
  
  rts

; ######################## UNO serial utilities ########################
  .ifdef UNOSERIAL

UnoNewLine:
  pha
  lda #$0D
  sta UNODATA
  lda #$0A
  sta UNODATA
  pla
  rts
  
UnoHexCharacters:
  pha
  
  pha
  lsr
  lsr
  lsr
  lsr
  and #$0F
  cmp #$0A
  bcc notHex1      ; if it is 0..9, add '0' else also add 7
  adc #6           ; add 7 (6+C=1), result will be with C=0
notHex1:
  adc #"0"        ; if C=0, we're at 0..9
  nop
  nop
  sta UNODATA
  nop
  nop
  
  pla
  and #$0F
  cmp #$0A
  bcc notHex2      ; if it is 0..9, add '0' else also add 7
  adc #6           ; add 7 (6+C=1), result will be with C=0
notHex2:
  adc #"0"        ; if C=0, we're at 0..9
  nop
  nop
  sta UNODATA
  nop
  nop
  pla
  rts
  .endif

; ######################## firmware libraries ########################

  .ifdef VIALCD
  .include lcddriver.asm
  .else
  .ifdef VIALED
  .include lcddriver.asm
  .endif
  .endif
  .ifdef VIAKEYBOARD
  .include keyboarddriver.asm
  .endif
   .ifdef VIATIMER
  .include timerdriver.asm
  .endif
  .ifdef ACIASERIAL  
  .include serialdriver.asm
  .endif
  
; ######################## Timer firmware ISR ########################
  .ifdef VIATIMER
timerISR:

  bit  T1CL   ; clear the interrupt by reading T1 Counter L
  inc  Ticks
  bne  endISR
  inc  Ticks+1
  bne  endISR
  inc  Ticks+2
  bne  endISR
  inc  Ticks+3
  
endISR:
  rti
  .endif

; ######################## Keyboard firmware ISR ########################
  .ifdef VIAKEYBOARD
keyboardISR:
  pha
  phx
  
  lda  PORTB                   ; read key code (reading PORTB also clears the interrupt)
  ldx  KeyboardWritePointer    ; push it into key code buffer
  sta  KeyboardBuffer, X
  inc  KeyboardWritePointer

  plx
  pla
  rti
  .endif

; ######################## ACIA serial firmware ISR ########################
   .ifdef ACIASERIAL
serialInISR:
  pha
  phx
  
  lda  ACIADATA                ; read serial byte
  cmp  #$03                    ; is it break? (<ctrl><C>)
  beq  serialInBreak
  ldx  SerialInWritePointer    ; push it into serial input buffer
  sta  SerialInBuffer, X
  inc  SerialInWritePointer

  plx
  pla
  rti
  
serialInBreak:
  inc SerialBreakFlag
  plx
  pla
  rti
  .endif
  
; IRQ vector points here
irqVector:
  
  bbr7 IFR, notVIA
  .ifdef VIATIMER
  bbs6 IFR, timerISR     ; isr does rti
  .endif
  .ifdef VIAKEYBOARD
  bbs4 IFR, keyboardISR; ; isr does rti
  .endif
  
  ; other VIA interrupts?
  rti
  
notVIA
  .ifdef ACIASERIAL
  bbr7 ACIASTATUS, notACIA
  
  .ifdef ACIAMOTOROLLA
  bbs0 ACIASTATUS, serialInISR ; 6850: isr does rti
  .else
  bbs3 ACIASTATUS, serialInISR ; 6551: isr does rti
  .endif
  .endif
  
  ; other ACIA interrupts?
  rti
  
notACIA:
  ; interrupts from sources other than VIA and ACIA?
  rti
  

nmiVector:
  rti

   ; tables must come after code, tables are page aligned
  .ifdef VIALCD
  .include lcdconstants.asm
  .endif
  .ifdef VIAKEYBOARD
  .include keyboardconstants.asm
  .endif

; Reset/IRQ vectors
  .org $fffa
  .word nmiVector
  .word resetVector
  .word irqVector