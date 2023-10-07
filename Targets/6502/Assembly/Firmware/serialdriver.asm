; ######################## Acknowledgements ########################
;  Obviously there is a lot of great material online
;  to learn from or borrow (like that at https://6502.org/ 
;  for example.
;  However, I must mention that a major source of source
;  material and inspiration for me has been https://eater.net/6502
;

; ######################## Serial ACIA firmware APIs ########################
;
; SerialInitialize   : initialize or reset the serial ACIA firmware
; SerialInAvailable  : returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
; SerialInConsume    : consumes the next character from the buffer and returns value in A
; SerialOut          : transmits A

; ######################## start of Keyboard VIA firmware ########################


; ######################## start of Serial ACIA firmware ########################

SerialInitialize: ; initialise 6551 ACIA
  pha
  
  ; reset buffer so at least start and end are the same
  sei              ; disable interrupts
  stz SerialInWritePointer
  stz SerialInReadPointer
  stz SerialBreakFlag
  cli              ; enable interrupts
  
  .ifdef ACIAMOTOROLLA
  
  lda #%00000011        ; reset the 6850
  sta ACIACONTROL
  
  ;lda #%00010101        ; 8-N-1, 115200 baud (/16 for  1.8432 mHz), no rx interrupt
  ;lda #%00010110        ; 8-N-1,  28800 baud (/64 for  1.8432 mHz), no rx interrupt
  lda #%10010110         ; 8-N-1,  28800 baud (/64 for  1.8432 mHz), rx interrupt
  sta ACIACONTROL
  
  .else
  
  lda #$00       ; soft reset (value not important)
  sta ACIASTATUS 
  
  lda #%00011110         ; 8-N-1, 9600 baud
  ;lda #%00011111        ; 8-N-1, 19200 baud
  sta ACIACONTROL
  
  ;lda #%00001011  ; set specific modes and functions
  ;                ; no parity, no echo, no Tx interrupt
  ;                ; no Rx interrupt, enable Tx/Rx
  lda #%00001001   ; same as above but with Rx interrupt enabled
                  
                  
  sta ACIACOMMAND ; save to command register
  
  .endif
  
  pla
  rts


SerialInAvailable:
                            ;  6 - jsr
  sei                       ;  2 - disable interrupts
  lda SerialInReadPointer   ;  3
  cmp SerialInWritePointer  ;  3
  cli                       ;  2 - enable interrupts
  rts                       ;  6
                            ; 22
SerialInConsume:
  phx
  
  ldx SerialInReadPointer
  lda SerialInBuffer, X
  inc SerialInReadPointer

  plx
  rts

SerialOut:
  .ifdef ACIAMOTOROLLA

  ; 6850:
  
  pha
loopSerialOut:
  lda ACIASTATUS  
  
  and #%00000010     ; Bit 1 - Transmit Data Register Empty (TDRE)
  beq loopSerialOut  ; loop if not ready (bit set means TDRE is empty and ready)
  
  ; reading ACIASTATUS clears the interrupt bit so don't do it here if there is a character waiting in the TDRE
  ;and #%00000001     ; Bit 0 - Receiver Data Register Full (RDRF)
  ;bne loopSerialOut  ; bit set means RDRF is full - loop to wait for ISR to clear it
    
  pla
                      
  sta ACIADATA       ; output character to TDRE
  
  .else
  
  ; 6551:
  
  phx
  ; delay to deal with the 6551 Transmitter Data Register Empty bug
  ; 9600 baud / 10 bits per character =960 characters per second
  ; 1 second = 1000 ms = 1000000 µs
  ; 1000000 / 960 = 1042µs per character
  ; at 1 mHz 1:1 ratio between µs and clock cycles so delay for 1042 cycles
  ldx #208 ;#104 ; 19200 bps : 1 bit every 52 clock cycles x 10 bits 
           ; per character = 520 clock cycles (as per Ben's description)
serialOutDelay:
  dex                 ; 2x208 = 416
  bne serialOutDelay  ; 3x208 = 624
                      ;         ----
                      ;         1040 clock cycles

  sta ACIADATA
  plx
  
  .endif
  rts
  
SerialHexOut:
  pha
  jsr SerialHexOutMuntsA
  pla
  rts
  
SerialHexOutMuntsA:
  ; Inspired by Ross Archer's HEX file loader : http://www.6502.org/source/monitors/intelhex/intelhex.htm
  ; and by https://beebwiki.mdfs.net/Number_output_in_6502_machine_code
  ; Note: trashes A
  pha
  lsr
  lsr
  lsr
  lsr
  jsr outNibbleSerial
  pla
outNibbleSerial:
  and #$0F
  cmp #$0A
  bcc notHexSerial ; if it is 0..9, add '0' else also add 7
  adc #6           ; add 7 (6+C=1), result will be with C=0
notHexSerial:
  adc #"0"         ; if C=0, we're at 0..9
  jmp SerialOut    ; includes rts

