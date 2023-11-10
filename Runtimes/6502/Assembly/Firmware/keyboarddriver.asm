; ######################## Acknowledgements ########################
;  Obviously there is a lot of great material online
;  to learn from or borrow (like that at https://6502.org/ 
;  for example.
;  However, I must mention that a major source of source
;  material and inspiration for me has been https://eater.net/6502
;

; ######################## Keyboard VIA firmware APIs ########################
;
; KeyboardInitialize : no arguments, initilizes the keyboard interface
; KeyboardAvailable  : returns Z flag set if there is a key code available in the buffer, Z clear if not (disables and enables interrupts)
; KeyboardConsume    : consumes the next key code from the buffer and returns value in A (zero means control character or release code)

; ######################## start of Keyboard VIA firmware ########################

RELEASING    = %00000001
LEFTSHIFT    = %00000010
RIGHTSHIFT   = %00000100
LEFTCONTROL  = %00001000

KeyboardInitialize:

  
  lda  #%00010000  ; set positive active edge to trigger for interrupt on transition to high for CB1 ($0 would trigger on transition to low)
  sta PCR
  lda  #%10010000  ; set CB1 bit in IER to enable CB1 as an interrupt pin
  sta IER
    
  lda #$00
  sta KeyboardWritePointer
  sta KeyboardReadPointer
  sta KeyboardFlags
  
  rts
  
KeyboardAvailable:
  sei              ; disable interrupts
  lda KeyboardReadPointer
  cmp KeyboardWritePointer
  cli              ; enable interrupts
  rts
  
KeyboardConsume:
  phx
  
  ldx KeyboardReadPointer
  lda KeyboardBuffer, x
  inc KeyboardReadPointer
  
  cmp #$F0 ; release code?
  bne notreleasenext
  smb0 KeyboardFlags ; #RELEASING
  lda #0
  jmp loopexit
  
 notreleasenext:
  ; not release code
  bbs0 KeyboardFlags, releasecode ; #RELEASING
  
  ; regular code, not release code
  cmp #$14 ; left control?
  bne notleftcontrol
  smb3 KeyboardFlags ; #LEFTCONTROL
  lda #0
  jmp loopexit
notleftcontrol
  cmp #$12 ; left shift?
  bne notshiftleft
  smb1 KeyboardFlags ; #LEFTSHIFT
  lda #0
  jmp loopexit
notshiftleft:
  cmp #$59 ; right shift?
  bne notshiftright
  smb2 KeyboardFlags ; #RIGHTSHIFT
  lda #0
  jmp loopexit
notshiftright:
  
  ; decode code here
  tax
  lda KeyboardFlags
  and #(LEFTSHIFT | RIGHTSHIFT)
  beq notshifted
  lda keymap_shifted, x   ; map to shifted character code
  jmp testunknown
notshifted:
  bbr3 KeyboardFlags, regular ; #LEFTCONTROL
  lda keymap_controlled, x   ; map to controlled character code
  jmp testunknown
regular:
  lda keymap, x           ; map to character code
testunknown:
  
  ; debugging unknown codes
  cmp #"?"
  bne notunknown
  cpx #$4A                ; actual question key?
  beq notunknown
  .ifdef VIALCD
  jsr LCDHexCharacters
  .endif
  
notunknown:
  jmp loopexit
  
releasecode: 
  rmb0 KeyboardFlags ; RELEASING

  cmp #$14 ; left control for RC2014 keyboard
  bne notleftcontrol2
  rmb3 KeyboardFlags ; #LEFTCONTROL
  lda #0
  jmp loopexit
notleftcontrol2:
  cmp #$12 ; left shift
  bne notshiftleft2
  rmb1 KeyboardFlags ; #LEFTSHIFT
  lda #0
  jmp loopexit
notshiftleft2:
  cmp #$59 ; right shift
  bne notshiftright2
  rmb2 KeyboardFlags ; #RIGHTSHIFT
  lda #0
  jmp loopexit
notshiftright2:  
  lda #0
  
  ; ignore released code
  jmp loopexit
  
loopexit:
  plx
  rts

