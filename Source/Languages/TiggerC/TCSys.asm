unit TCSys
{
    WriteChar()
    {
        TSX
        
        // return address
        INX
        INX
        
        // char c : SP - 1
        INX
        LDA 0x0100, X
        
        Serial.WriteChar();
        
    }
    Millis()
    {
        LDA # 4
        STA ZP.ACCL
        STZ ZP.ACCH
        Allocate.Allocate();
        
        // return the 16 bit address of the start of the 4 timer tick bytes -> word[2]
        
        LDY # 3
        LDA ZP.TICK3 // all 4 get updated when you read TICK3 (on the emulator)
        STA [IDX], Y
        DEY
        LDA ZP.TICK2
        STA [IDX], Y
        DEY
        LDA ZP.TICK1
        STA [IDX], Y
        DEY
        LDA ZP.TICK0
        STA [IDX], Y
        
        LDA IDXL
        STA ZP.TOPL
        LDA IDXH
        STA ZP.TOPH
    }
    Delay()
    {
        TSX
        
        // return address
        INX
        INX
        
        // word milliseconds : SP - 1
        INX
        LDA 0x0100, X
        STA TOPH
        INX
        LDA 0x0100, X
        STA TOPL
        Time.DelayTOP();
    }
    Malloc()
    {
        TSX
        
        // return address
        INX
        INX
        
        // word size : SP - 1
        INX
        LDA 0x0100, X
        STA ACCH
        INX
        LDA 0x0100, X
        STA ACCL
        Allocate.Allocate();
        
        LDA ZP.IDXL
        STA ZP.TOPL
        LDA ZP.IDXH
        STA ZP.TOPH
    }
    Free()
    {
        TSX
        
        // return address
        INX
        INX
        
        // word ptr : SP - 1
        INX
        LDA 0x0100, X
        STA IDXH
        INX
        LDA 0x0100, X
        STA IDXL
        Free.Free();
    }
}
