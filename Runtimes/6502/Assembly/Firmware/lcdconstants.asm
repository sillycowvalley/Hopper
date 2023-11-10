; ######################## LCD firmware table ########################

  .org $fff0
lcdRowStart:
  .byte $00 ; 20x4 and 16x2
  .byte $40 ; 20x4 and 16x2
  .byte $14 ; 20x4
  .byte $54 ; 20x4
