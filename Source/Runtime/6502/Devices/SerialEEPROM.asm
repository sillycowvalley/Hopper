unit SerialEEPROM
{
#define SERIAL128BYTEPAGES  // 24AA512 (64K) and 24AA1026 (128K)
//#define SERIAL64BYTEPAGES // 24C256 (32K)

#ifdef SERIAL128BYTEPAGES
    const byte serialPageSize = 128;
#endif    
#ifdef SERIAL64BYTEPAGES
    const byte serialPageSize = 64;
#endif    
    
    copyProgramPage()
    {
#ifdef CPU_65C02S
        PHY
#else
        TYA PHA
#endif
        // BeginTx
        LDA # (I2C.SerialEEPROMAddress << 1)
        STA ZP.OutB
        I2C.Start();
        LDA ZP.IDYH
        STA ZP.OutB
        I2C.ByteOut(); // EEPROM address MSB (0)
        LDA ZP.IDYL
        STA ZP.OutB
        I2C.ByteOut(); // EEPROM address LSB (0)
        // EndTx
        I2C.Stop();
        
        // delay 5ms after Stop() for EEPROM
        LDA # 5
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        Time.DelayTOP();
        
        // read one page from EEPROM:
        LDA # serialPageSize
        STA ZP.TOPL
        LDA # I2C.SerialEEPROMAddress
        STA NEXTL
        RequestFromTOPNEXT(); // NEXTL has I2C adddress, TOPL has number of bytes to return, TOPL returns number of bytes read
        // assume success
        
        LDX # 0
        loop
        {
            LDA Address.I2CInBuffer, X
#ifdef CPU_65C02S
            STA [IDX]
#else
            LDY # 0
            STA [IDX], Y
#endif
            IncIDY();
            IncIDX();
            INX
            CPX # serialPageSize
            if (Z) { break; }
        } 
#ifdef CPU_65C02S
        PLY
#else
        PLA TAY
#endif
    }
    LoadFromEEPROM()
    {
        LDA # serialPageSize // program starts from page 1, not page 0
        STA ZP.IDYL
        LDA # 0
        STA ZP.IDYH
        // IDY contains the source address (in EEPROM)
        
        LDA #(HopperData & 0xFF)
        STA ZP.IDXL
        LDA #(HopperData >> 8)
        STA ZP.IDXH
        // IDX contains the destination address
        
        LDY ZP.PROGSIZE
        loop
        {
            // copy a 256 byte 6502 page:
            copyProgramPage();
            copyProgramPage();
#ifdef SERIAL64BYTEPAGES
            copyProgramPage();
            copyProgramPage();
#endif
            DEY
            if (Z) { break; }
        }
    }
    
    copyPageToEEPROM()
    {
        PHA
#ifdef CPU_65C02S        
        PHX
        PHY
#else
        TXA PHA
        TYA PHA
#endif     
        // initialize the delay in ms for Time.DelayTOP()
        LDA # 5
        STA ZP.TOPL
#ifdef CPU_65C02S
        STZ ZP.TOPH
#else
        LDA # 0
        STA ZP.TOPH
#endif
        
        // BeginTx
        LDA # (I2C.SerialEEPROMAddress << 1)
        STA ZP.OutB
        I2C.Start();
        LDA ZP.IDYH
        STA ZP.OutB
        I2C.ByteOut(); // EEPROM address MSB
        LDA ZP.IDYL
        STA ZP.OutB
        I2C.ByteOut(); // EEPROM address LSB
        
        LDX # serialPageSize
        loop
        {
#ifdef CPU_65C02S 
            LDA [IDX]
#else
            LDY # 0
            LDA [IDX], Y
#endif
            STA ZP.OutB
            I2C.ByteOut(); // zeros ZP.OutB
            IncIDX();
            IncIDY();
            DEX
            if (Z) { break; }
        }
        I2C.Stop();
        
        // delay 5ms after Stop() for EEPROM (TOP is already initialized with 0x0005)
        Time.DelayTOP();
              
#ifdef CPU_65C02S   
        PLY
        PLX
#else
        PLA TAY
        PLA TAX
#endif
        PLA
    }
    SaveToEEPROM()
    {
        LDA #(HopperData & 0xFF)
        STA ZP.IDXL
        LDA #(HopperData >> 8)
        STA ZP.IDXH
        // IDX contains the source address
        
        LDA # serialPageSize // program starts on page 1, not page 0
        STA ZP.IDYL
        LDA # 0
        STA ZP.IDYH
        // IDY contains the destination address (in EEPROM)
        
        LDX ZP.PROGSIZE // size of program in 256 byte pages
        loop
        {
            // copy a 256 byte 6502 page:
            copyPageToEEPROM();
            copyPageToEEPROM();
#ifdef SERIAL64BYTEPAGES
            copyPageToEEPROM();
            copyPageToEEPROM();
#endif
            DEX
            if (Z) { break; }
        }
    }
    WritePage()
    {
        // IDX contains the source address
        // IDY contains the destination address (in EEPROM)
        
        // copy a 256 byte 6502 page:
        copyPageToEEPROM();
        copyPageToEEPROM();
#ifdef SERIAL64BYTEPAGES
        copyPageToEEPROM();
        copyPageToEEPROM();
#endif
    }
    
    ReadPage()
    {
        // IDY contains the source address (in EEPROM)
        // IDX contains the destination address
        
        // copy a 256 byte 6502 page:
        copyProgramPage();
        copyProgramPage();
#ifdef SERIAL64BYTEPAGES
        copyProgramPage();
        copyProgramPage();
#endif
    }
}
