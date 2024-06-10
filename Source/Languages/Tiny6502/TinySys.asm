unit TinySys
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
        // return the 16 bit address of the start of the 4 timer tick bytes -> word[2]
        LDA # ZP.TICK0
        STA ZP.TOPL
        STZ ZP.TOPH
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
}
