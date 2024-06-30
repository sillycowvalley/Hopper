unit TCSys
{
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
    }
    
    I2CScan()
    {
        TSX
        
        // return address
        INX
        INX
        
        // byte address : SP - 1
        INX
        LDA 0x0100, X
    
        // I2C address in A
        I2C.Scan();
        if (Z)
        {
            LDA # 1 // found = true
            STA ZP.TOPL
        }
        else
        {
            // not found
            STZ ZP.TOPL
        }
        STZ ZP.TOPH
    }
}
