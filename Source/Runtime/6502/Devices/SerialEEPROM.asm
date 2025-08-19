unit SerialEEPROM
{
#ifdef HOPPER_BASIC
    friend EEPROM;
#endif    
    
    // API Status: Clean
    // Serial EEPROM interface for program storage and retrieval
    // All public methods preserve caller state except for documented outputs
    // No accidental side effects or register corruption
    // Uses ZP.IDX/ZP.IDY for address management and ZP.TOP for timing control

    // Hardware configuration - EEPROM chip page size selection
    // Different EEPROM chips have different page sizes for write operations
#define SERIAL128BYTEPAGES  // 24AA512 (64K) and 24AA1026 (128K)
//#define SERIAL64BYTEPAGES // 24C256 (32K)

#ifdef SERIAL128BYTEPAGES
    const byte serialPageSize = 128;  // Large page EEPROMs (24AA512, 24AA1026)
#endif    
#ifdef SERIAL64BYTEPAGES
    const byte serialPageSize = 64;   // Smaller page EEPROMs (24C256)
#endif    
    
    // Copy one EEPROM page (128 or 64 bytes) to RAM
    // Input: ZP.IDY = EEPROM source address (16-bit)
    //        ZP.IDX = RAM destination address (16-bit)
    // Output: One page copied from EEPROM to RAM
    //         ZP.IDY advanced by serialPageSize bytes
    //         ZP.IDX advanced by serialPageSize bytes
    // Modifies: ZP.OutB (I2C operations), ZP.TOP (timing delay), X (byte counter)
    //          ZP.NEXTL (I2C address), ZP.TOPL (byte count)
    // Note: Includes 5ms delay after I2C operations per EEPROM timing requirements
    copyProgramPage()
    {
#ifdef CPU_65C02S
        PHY
#else
        TYA PHA
#endif
        // BeginTx - Start I2C write transaction to set EEPROM read address
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

#if !defined(HOPPER_BASIC)
    // Load complete Hopper program from Serial EEPROM to RAM
    // Input: ZP.PROGSIZE = program size in 256-byte pages
    // Output: Program loaded from EEPROM to HopperData address
    //         ZP.IDX = final destination address (after last copied byte)
    //         ZP.IDY = final EEPROM address (after last read byte)
    // Modifies: ZP.IDX, ZP.IDY, Y (page counter), all registers used by copyProgramPage()
    // Note: Program starts from EEPROM page 1 (not page 0) to skip header/metadata
    // Strategy: For 128-byte EEPROM pages, call copyProgramPage() twice per 256-byte RAM page
    //          For 64-byte EEPROM pages, call copyProgramPage() four times per 256-byte RAM page
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
#endif
    // Copy one RAM page (serialPageSize bytes) to EEPROM
    // Input: ZP.IDX = RAM source address (16-bit)
    //        ZP.IDY = EEPROM destination address (16-bit)
    // Output: One page copied from RAM to EEPROM
    //         ZP.IDX advanced by serialPageSize bytes
    //         ZP.IDY advanced by serialPageSize bytes
    // Modifies: ZP.OutB (I2C operations), ZP.TOP (timing delay), X (byte counter)
    //          A, X, Y (preserved via stack)
    // Note: Includes 5ms delay after write per EEPROM timing requirements
    //       EEPROM write operations require page-aligned addresses for optimal performance
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
        
        // BeginTx - Start I2C write transaction with EEPROM address and data
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
#if !defined(HOPPER_BASIC)
    // Save complete Hopper program from RAM to Serial EEPROM
    // Input: ZP.PROGSIZE = program size in 256-byte pages
    // Output: Program saved from HopperData address to EEPROM
    //         ZP.IDX = final source address (after last copied byte)
    //         ZP.IDY = final EEPROM address (after last written byte)
    // Modifies: ZP.IDX, ZP.IDY, X (page counter), all registers used by copyPageToEEPROM()
    // Note: Program starts at EEPROM page 1 (not page 0) to skip header/metadata area
    // Strategy: For 128-byte EEPROM pages, call copyPageToEEPROM() twice per 256-byte RAM page
    //          For 64-byte EEPROM pages, call copyPageToEEPROM() four times per 256-byte RAM page
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
#endif

}
