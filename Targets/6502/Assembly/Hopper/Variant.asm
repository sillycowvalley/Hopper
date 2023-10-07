; ######################## Variant functionality ########################

; Variant memory map:
;   0000 heap allocator size
;   14   type = tVariant
;   00   reference count
;   xx   actual type for item
;   xxxx data for value types, pData for reference types


createValueVariant:
  ; type in A
  ; value in fVALUE
  ; uses fSIZE
  ; return tVariant in IDX
  
  
  .ifdef CHECKED
  cmp #tHeapTypes ; C set if heap type, C clear if value type
  bcc createValueVariantGood
  
  lda #$0B ; internal error : should never arrive here for a heap type
  sta ACCL
  stz ACCH
  jmp utilityDiagnosticsDie
  
createValueVariantGood:
  .endif
  
  pha
  
  lda #3
  sta fSIZEL
  stz fSIZEH
  
  ; type in A
  ; size is in fSIZE
  ; return address in IDX
  lda #tVariant
  jsr gcCreate
  
  ; variantItem = Variant_Box(type, value);
  ldy #2
  pla ; type
  sta (IDX), Y
  iny
  lda fVALUEL
  sta (IDX), Y
  iny
  lda fVALUEH
  sta (IDX), Y
  
  rts
  
cloneVariant:
  ; variant to clone is in IDY
  ; return clone in IDX
  ldy #4
  lda (IDY), Y
  sta fVALUEH
  dey
  lda (IDY), Y
  sta fVALUEL
  dey
  lda (IDY), Y ; type
  
  ; type in A
  ; value in fVALUE
  ; uses fSIZE
  ; return tVariant in IDX
  jmp createValueVariant
  