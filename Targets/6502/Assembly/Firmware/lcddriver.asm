; ######################## Acknowledgements ########################
;  Obviously there is a lot of great material online
;  to learn from or borrow (like that at https://6502.org/ 
;  for example.
;  However, I must mention that a major source of source
;  material and inspiration for me has been https://eater.net/6502
;

; ######################## LCD VIA firmware constants ########################

E      = %01000000
RW     = %00100000
RS     = %00010000

  .ifdef VIALCD

; 20x4
LCDROWS    = 4
LCDCOLUMNS = 20

; 16x2
;LCDROWS    = 2
;LCDCOLUMNS = 16

LCDROWSMINUSONE = LCDROWS-1
  .endif

; ######################## LCD VIA firmware APIs ########################
;
; LCDInitialize   : no arguments, initilizes the LCD into 4 bit mode
; LCDClear        : no arguments, sets LCDColumn and LCDRow to zero and clears the screen
; LCDCharacter    : ASCII character in A, outputs character and advances cursor, scrolls if at the end of the last row
; LCDString       : ASCII string follows subroutine call inline, trashes A
; LCDHexCharacters: print two uppercase hex digits for the 8 bit value in A and advance cursor, trashes A
; LCDNewLine      : advances cursor to the beginning of the next row, scrolls if last row
; LCDSetCursor    : moves the cursor to cell (LCDColumn, LCDRow)
; LCDBackspace    : move the cursor back by one cell, does nothing if at (0,0)
; LCDScroll       : scroll the top row off display, clear bottom row and place cursor in first cell of bottom row

; LEDWrite        : non-zero A means ON, zero A means off (not LCD but uses same port)

; ######################## start of LCD VIA firmware ########################

  .ifdef VIALED
  .ifndef VIALCD
LEDInitialize:
  lda #%10000000
  sta DDRA ; set LED port to write and the others to read (no LCD)
  lda #0
  jsr LEDWrite ; start with LED off
  rts
  .endif
  .endif
  
LEDWrite:
  nop ; pha
  ; we assume DDRA is left correctly configured by the LCD APIs
  cmp #0
  beq ledValueOk ; storing #0 is fine for off
  lda #1     ; store   #1 for on
ledValueOk:
  sta LEDState
  ; fall through

ledSet: ; helper function used to restore LED from LCD APIs
  bbs0 LEDState, ledOn
  rmb7 PORTA ; turn the built-in LED off
  nop ; pla
  rts
ledOn:
  smb7 PORTA ; turn the built-in LED on
  nop ; pla
  rts
  
  .ifdef VIALCD

LCDClear:
  pha
  lda #$00
  sta LCDColumn
  sta LCDRow
  lda #%00000001 ; clear screen
  jsr lcdInstruction
  jsr LCDSetCursor
  pla
  rts



LCDNewLine:
  phx
  
  ldx LCDRow
  inx
  cpx #LCDROWS
  beq lcdScrollLast ; don't enter on last row
  stx LCDRow
  ldx #$00
  stx LCDColumn
  plx
  
  jsr LCDSetCursor
  rts
  
lcdScrollLast:
  plx
  jsr LCDScroll
  rts



LCDScroll:
; Currently optimal in terms of number of display IO reads and writes: only the cells that change are written to
; and in typical scenarios a lot of the screen is blank : scrolling blank is fast and efficient.
; - in the best case scenario the entire screen is blank which would result in ROWSxCOLUMNS reads and zero writes
; - in the worst case scenario, ROWSxCOLUMNS reads and ROWSxCOLUMNS writes
  phy
  phx
  pha
  
  ldy #0
nextColumn3:
  sty LCDColumn
  
  ldx #$0 ; start on the first row
  lda lcdRowStart, x
  clc
  adc LCDColumn
  ora #%10000000 ; set source DDRAM address
  sta ScratchPrevAddress
  jsr lcdInstruction
  jsr lcdDataRead
  sta ScratchActiveChar ; to remember character on previous row
nextRow3:
  inx
  
  ; load cell (LCDColumn, x) into ScratchActiveChar
  lda lcdRowStart, x
  clc
  adc LCDColumn
  ora #%10000000 ; set source DDRAM address (current row)
  sta ScratchCurrAddress
  jsr lcdInstruction
  jsr lcdDataRead
  cmp ScratchActiveChar ; compare to previous row
  beq noNeedToWrite
  
  sta ScratchActiveChar  ; not the same, update ActiveChar
  lda ScratchPrevAddress ; set destination DDRAM address (previous row)
  jsr lcdInstruction
  lda ScratchActiveChar
  jsr lcdDataWrite
  
noNeedToWrite:
  
  cpx #LCDROWSMINUSONE
  bne notLastRow
  lda ScratchActiveChar ; surely it is already in A?
  cmp #" " ; is ActiveChar a space?
  beq notLastRow
  
  ; last row and not already a " " so blank it
  lda ScratchCurrAddress
  jsr lcdInstruction
  lda #" "
  jsr lcdDataWrite
  
notLastRow:
  lda ScratchCurrAddress
  sta ScratchPrevAddress
  cpx #LCDROWSMINUSONE ; did we just read the last row?
  bne nextRow3
  
  iny
  cpy #LCDCOLUMNS
  bne nextColumn3
  
  ; move cursor to start of last row
  ldy #LCDROWSMINUSONE
  sty LCDRow
  ldx #0
  stx LCDColumn
  jsr LCDSetCursor
  
  pla
  plx
  ply
  rts



lcdCursorBack:
  pha
  phx
  
  lda LCDColumn
  beq columnZero
  dec
  sta LCDColumn
  jmp backspaceExit
  
columnZero:
  ldx LCDRow
  beq backspaceExit
  dex
  stx LCDRow
  lda #LCDCOLUMNS
  dec
  sta LCDColumn
backspaceExit:
  plx
  pla
  jsr LCDSetCursor
  rts



LCDBackspace
  pha
  jsr lcdCursorBack
  lda #" "
  jsr LCDCharacter
  jsr lcdCursorBack
  pla
  rts



LCDSetCursor: ; (LCDColumn, LCDRow)
  
  pha
  phx
  
  ldx LCDRow
  cpx #LCDROWS
  beq lcdSkipSetCursor ; don't wrap around if (col,row) out of range (less confusion)
  
  lda lcdRowStart, x
  clc
  adc LCDColumn
  ora #%10000000 ; Set DDRAM address
  jsr lcdInstruction
  
lcdSkipSetCursor:
  plx
  pla
  rts



lcdWait:
  pha
  lda #%11110000  ; LCD control lines as output and data lines as input
  sta DDRA
  
lcdBusy:
  lda #RW    ; set RW bit (%00100000) and clear RS bit (%00010000) to send instruction
  sta PORTA
  lda #(RW | E)
  sta PORTA
  lda PORTA       ; Read high nibble
  sta ScratchByte
  lda #RW
  sta PORTA
  lda #(RW | E)
  sta PORTA
  lda PORTA       ; Read low nibble (and discard)
  lda ScratchByte
  and #%00001000
  bne lcdBusy    ; wait if busy bit is still set

  lda #%11111111  ; LCD data is output
  sta DDRA
   
  pla
  rts



lcdInstruction:
  jsr lcdWait
  pha
  pha
  lsr
  lsr
  lsr
  lsr            ; Send high 4 bits
  sta PORTA
  ora #E         ; Set E bit to send instruction
  sta PORTA
  eor #E         ; Clear E bit
  sta PORTA
  pla
  and #%00001111 ; Send low 4 bits
  sta PORTA
  ora #E         ; Set E bit to send instruction
  sta PORTA
  eor #E         ; Clear E bit
  sta PORTA
  pla
  jsr lcdWait
  jsr ledSet ; restore state of built-in LED
  rts



lcdDataRead: 
  jsr lcdWait
  lda #%11110000
  sta DDRA
  
  lda #(RW | RS)
  sta PORTA
  ora #E
  sta PORTA
  lda PORTA       ; read high nibble
  and #%00001111
  clc
  rol
  rol
  rol
  rol
  sta ScratchByte     ; and store in ScratchByte
  
  lda #(RW | RS)
  sta PORTA
  ora #E
  sta PORTA
  
  lda PORTA       ; read low nibble
  and #%00001111
  ora ScratchByte
  pha

  ; clear E bit (%01000000) to send instruction
  lda #(RW | RS)
  sta PORTA
  
  lda #%11111111  ; LCD data is output
  sta DDRA
  
  pla
  jsr ledSet ; restore state of built-in LED
  rts




lcdWrite4DataBits: 
  ; RS (%00010000) is set and RW (%00100000) is clear for data and E (%01000000) starts low
  pha
  ora #RS  ; set RS
  sta PORTA
  ora #E ; toggle E bit (%01000000) for write
  sta PORTA
  eor #E
  sta PORTA
  pla
  rts



lcdDataWrite:
  jsr lcdWait
  pha
  pha
  lsr
  lsr
  lsr
  lsr             ; Send high 4 bits
  jsr lcdWrite4DataBits
  pla
  and #%00001111  ; Send low 4 bits
  jsr lcdWrite4DataBits
  pla
  jsr ledSet ; restore state of built-in LED
  rts



LCDInitialize:
  
  phy
  pha
  
  lda #%11111111
  sta DDRA ; LCD: set all pins on port A to output (which will be their default state if we are using the LCD)
  
  .ifdef SLOWCLOCK_1_25
  
  ; As per Ben Eater's code using %00000010 rather than %00000011 works for the cold start (at any speed) but not the warm restart
  ; https://eater.net/downloads/keyboard.s
  
  lda #%00000010         ; Set 4-bit mode (this also clears RS, RW and E since we are writing the entire port)
  sta PORTA
  ora #E ; toggle E to send instruction
  sta PORTA
  and #%00001111
  sta PORTA
  
  .else
  
  ; delay 50000 us ; > 40ms for Vcc to rise above 2.7V (for cold start)
  ;lda #21       ; 1 mHz
  ;ldy #178
  lda #86       ; 4 mHz
  ldy #205
delayLoop0:
  cpy #1         ; 2
  dey            ; 2
  sbc #0         ; 2
  bcs delayLoop0 ; 3
  
  ; This approach as per figure 24 ("4-Bit Interface") of the spec, uses %00000011 rather than %00000010 and works for 
  ; both cold and warm starts but only at higher speeds (>= 1 mHz) (even if I compensate for the delay differences).
  
  lda #%00000011         ; Set 4-bit mode (this also clears RS, RW and E since we are writing the entire port)
  sta PORTA
  ora #E ; toggle E to send instruction
  sta PORTA
  and #%00001111
  sta PORTA
  
  ; delay 4500 us (> 4.1ms)
  pha
  ;lda #1         ; 1 mHz
  ;ldy #243
  lda #7          ; 4 mHz
  ldy #206
delayLoop1:
  cpy #1         ; 2
  dey            ; 2
  sbc #0         ; 2
  bcs delayLoop1 ; 3
  pla
  
  ; Set 4-bit mode
  sta PORTA
  ora #E ; toggle E to send instruction
  sta PORTA
  and #%00001111
  sta PORTA
  
  ; delay 150 us (> 100us)
  pha
  ;lda #0  ; 1 mHz
  ;ldy #6
  lda #0  ; 4 mHz
  ldy #27
delayLoop2:
  cpy #1         ; 2
  dey            ; 2
  sbc #0         ; 2
  bcs delayLoop2 ; 3
  pla
  
  ; Set 4-bit mode
  sta PORTA
  ora #E ; toggle E to send instruction
  sta PORTA
  and #%00001111
  sta PORTA
  
  ; delay 150 us (> 100us)
  pha
  ;lda #0  ; 1 mHZ
  ;ldy #6 
  lda #0  ; 4 mHz
  ldy #27
delayLoop3:
  cpy #1         ; 2
  dey            ; 2
  sbc #0         ; 2
  bcs delayLoop3 ; 3
  pla
  
  ; Set 4-bit mode (even number of times in case we are already in 4-bit mode)
  sta PORTA
  ora #E ; toggle E to send instruction
  sta PORTA
  and #%00001111
  sta PORTA

  .endif
  
  ; at this point we switch to regular programming under the assumption we succeeded in getting to 4-bit mode:
  
  lda #%00101000 ; Set 4-bit mode; 2-line display; 5x8 font
  jsr lcdInstruction
  
  lda #%00001110 ; Display on; cursor on; blink off
  jsr lcdInstruction
  
  lda #%00000110 ; Increment and shift cursor; don't shift display
  jsr lcdInstruction
  
  lda #%00000001 ; Clear screen
  jsr lcdInstruction
  
  ; initialize the built-in LED to off
  lda #0
  jsr LEDWrite 
  
  pla
  ply
  rts



LCDCharacter: ; (A) (preserved)
  pha
  phx
  ldx LCDRow
  cpx #LCDROWS
  bne noScrollNeeded
  
  jsr LCDScroll
noScrollNeeded:
  jsr LCDSetCursor
  jsr lcdDataWrite
  
  ; move cursor to next cell
  inc LCDColumn
  lda #LCDCOLUMNS
  cmp LCDColumn
  bne exitLCDCharacter ; still room on this line?
  
  lda #0
  sta LCDColumn
  inc LCDRow 
  
  ; was it the last row?
  lda #LCDROWS
  cmp LCDRow 
  bne exitLCDCharacter
  
  jsr LCDScroll
  
exitLCDCharacter:
  jsr LCDSetCursor ; to display next cell position

  plx
  pla
  rts



LCDHexCharacters:
  pha
  jsr LCDHexCharactersMuntsA
  pla
  rts
  
LCDHexCharactersMuntsA:
  ; Inspired by Ross Archer's HEX file loader : http://www.6502.org/source/monitors/intelhex/intelhex.htm
  ; and by https://beebwiki.mdfs.net/Number_output_in_6502_machine_code
  ; Note: trashes A
  pha
  lsr
  lsr
  lsr
  lsr
  jsr outNibble
  pla
outNibble:
  and #$0F
  cmp #$0A
  bcc notHex       ; if it is 0..9, add '0' else also add 7
  adc #6           ; add 7 (6+C=1), result will be with C=0
notHex:
  adc #"0"         ; if C=0, we're at 0..9
  jmp LCDCharacter ; includes rts



LCDString:
  ; Inspired by Ross Archer's HEX file loader : http://www.6502.org/source/monitors/intelhex/intelhex.htm
  ; and by https://beebwiki.mdfs.net/6502_routines
  ; Note: trashes A
  pla                           ; Get the low part of "return" address (data start address)
  sta     ScratchPointerL
  pla                           ; Get the high part of "return" address
  sta     ScratchPointerH       ; (data start address)
  phy
  ; Note: actually we're pointing one short
stringByte:
  ldy     #1
  lda     (ScratchPointerL),y         ; Get the next string character
  inc     ScratchPointerL             ; update the pointer
  bne     nextCharacter          ; if not, we're pointing to next character
  inc     ScratchPointerH             ; account for page crossing
nextCharacter:
  ora     #0              ; Set flags according to contents of Accumulator
  beq     stringExit1     ; don't print the final NULL
  jsr     LCDCharacter    ; write it out
  jmp     stringByte      ; back around
stringExit1:
  inc     ScratchPointerL
  bne     stringExit2
  inc     ScratchPointerH             ; account for page crossing
stringExit2:
  ply
  jmp     (ScratchPointerL)           ; return to byte following final NULL

  .endif ; .ifdef VIALCD

; http://forum.6502.org/viewtopic.php?p=62581#p62581
  ; delay 9*(256*A+Y)+8 cycles
  ; assumes that the BCS does not cross a page boundary
  ; A and Y are the high and low bytes (respectively) of a 16-bit value; multiply that 16-bit value by 9, then add 8 and you get the cycle count
  ;
  ; Note: I didn't bother counting the jsr and rts in the delay time for now.
  ;
  ;            1Mhz Clock     800Hz Clock
  ;            A      Y       A     Y
  ; 500ms      217    43      0     43
  ;  50ms      21    178      0     4
  ; 4.5ms       1    243      0     0
  ; 150us       0     16      0     0
  ;  
  ;  delayLoop:
  ;  cpy #1        ; 2
  ;  dey           ; 2
  ;  sbc #0        ; 2
  ;  bcs delayLoop ; 3