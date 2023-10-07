; ######################## Acknowledgements ########################
;  Obviously there is a lot of great material online
;  to learn from or borrow (like that at https://6502.org/ 
;  for example.
;  However, I must mention that a major source of source
;  material and inspiration for me has been https://eater.net/6502
;

; ######################## Timer firmware APIs ########################
;
; TimerInitialize    : initialize or reset the VIA timer firmware
; TimerDelay         : value in X / Y (LSB / MSB) in milliseconds, returns after delay

; ######################## start of Timer firmware ########################

TimerInitialize:
  
  stz Ticks
  stz Ticks1
  stz Ticks2
  stz Ticks3
  
  lda #%01000000 ; put the timer into free run mode
  sta ACR
  
  ; 1 millisecond intervals = 1000 cycles - 2 = 998 / $03E6
  ;   This assumes a CPU clock of 1 mHz.
  ;   A CPU clock of  4.9152 mHz would be 4.9152x faster so this count would need to be 4915 cycles - 2 = 4913 / 0x1061: 
  ;   A CPU clock of 10 kHz would be  100x slower so this would be a 100ms interval
  ;   A CPU clock of  1 kHz would be 1000x slower so this would be a 1000ms interval (1 second)
  lda #$61
  sta T1CL
  lda #$10
  sta T1CH
  lda #%11000000 ; set Timer1 bit in IER to put Timer1 into free run mode
  sta IER
  rts
  
  
TimerDelay:
  pha
  phx
  phy
  
  ; add ArgumentWord to Ticks0..3 and store in Target0..3
  clc
  txa
  adc Ticks
  sta Target
  tya
  adc Ticks1
  sta Target1
  lda Ticks2
  adc #0 ; to collect the carry
  sta Target2
  lda Ticks3
  adc #0 ; to collect the carry
  sta Target3
  
  ; while Ticks0..3 < Target0..3, loop here
isLower:
  lda Ticks+3
  cmp Target+3
  bcc isLower
  lda Ticks+2
  cmp Target+2
  bcc isLower
  lda Ticks+1
  cmp Target+1
  bcc isLower
  lda Ticks
  cmp Target
  bcc isLower
  
  ply
  plx
  pla
  rts
  
