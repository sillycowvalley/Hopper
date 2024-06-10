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
}