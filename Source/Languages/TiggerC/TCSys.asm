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
        STA ZP.ACCH
        INX
        LDA 0x0100, X
        STA ZP.ACCL
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
        STA ZP.IDXH
        INX
        LDA 0x0100, X
        STA ZP.IDXL
        Free.Free();
    }
    Available()
    {
        Memory.AvailableACC();
        
        LDA ZP.ACCL
        STA ZP.TOPL
        LDA ZP.ACCH
        STA ZP.TOPH
    }
    Maximum()
    {
        Memory.MaximumACC();
        
        LDA ZP.ACCL
        STA ZP.TOPL
        LDA ZP.ACCH
        STA ZP.TOPH
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
    ReadPage()
    {
        TSX
        
        // return address
        INX
        INX
        
        // byte[] data
        INX
        LDA 0x0100, X
        STA ZP.IDXH
        INX
        LDA 0x0100, X
        STA ZP.IDXL
        
        // word address
        INX
        LDA 0x0100, X
        STA ZP.IDYH
        INX
        LDA 0x0100, X
        STA ZP.IDYL
            
        // IDY contains the source address (in EEPROM)
        // IDX contains the destination address
        SerialEEPROM.ReadPage();
    }
    WritePage()
    {
        TSX
        
        // return address
        INX
        INX
        
        // const byte[] data
        INX
        LDA 0x0100, X
        STA ZP.IDXH
        INX
        LDA 0x0100, X
        STA ZP.IDXL
        
        // word address
        INX
        LDA 0x0100, X
        STA ZP.IDYH
        INX
        LDA 0x0100, X
        STA ZP.IDYL
        
        // IDX contains the source address
        // IDY contains the destination address (in EEPROM)
        SerialEEPROM.WritePage();
    }
}
