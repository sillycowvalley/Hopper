unit Diagnostic
{
    // used by 'F' command
    PageMemory()
    {
        // page # is in A
        STA ZP.IDXH
        STZ ZP.IDXL
        
        LDY #0
        loop
        {
            TYA
            AND #0x0F
            if (Z)
            {
                LDA #Utilities.Enter
                Serial.WriteChar();
            }
            LDA (ZP.IDX), Y
            Serial.HexOut();
            INY
            CPY #0x00 // ; after $FF
            if (Z) { break; }
        }
        LDA #Utilities.Enter
        Serial.WriteChar();
    }
}
