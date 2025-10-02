unit EEPROM
{
    uses "I2C"
    
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
        PHY
        
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
        Shared.LoadTopByte();
        Time.Delay();
        
        // read one page from EEPROM:
        LDA # serialPageSize
        STA ZP.TOP0
        LDA # I2C.SerialEEPROMAddress
        RequestFromTOPA(); // A has I2C adddress, TOPL has number of bytes to return, TOPL returns number of bytes read
        // assume success
        
        LDX # 0
        loop
        {
            LDA Address.I2CInBuffer, X
            STA [IDX]
            IncIDY();
            IncIDX();
            INX
            CPX # serialPageSize
            if (Z) { break; }
        } 
        PLY
    }


    // Copy one RAM page (serialPageSize bytes) to EEPROM
    // Input: ZP.IDX = RAM source address (16-bit)
    //        ZP.IDY = EEPROM destination address (16-bit)
    // Output: One page copied from RAM to EEPROM
    //         ZP.IDX advanced by serialPageSize bytes
    //         ZP.IDY advanced by serialPageSize bytes
    // Modifies: ZP.OutB (I2C operations), ZP.TOP and TARGET0-3 (timing delay), X (byte counter), IDX and IDY
    //          A, X, Y (preserved via stack)
    // Note: Includes 5ms delay after write per EEPROM timing requirements
    //       EEPROM write operations require page-aligned addresses for optimal performance
    copyPageToEEPROM()
    {
        PHA
        PHX
        PHY
        // initialize the delay in ms for Time.Delay()
        LDA # 5
        Shared.LoadTopByte();
        
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
            LDA [IDX]
            STA ZP.OutB
            I2C.ByteOut(); // zeros ZP.OutB
            IncIDX();
            IncIDY();
            DEX
            if (Z) { break; }
        }
        I2C.Stop();
        
        // delay 5ms after Stop() for EEPROM (TOP is already initialized with 0x0005)
        Time.Delay();
              
        PLY
        PLX
        PLA
    }
    
    
    // I2C and EEPROM initialization for Hopper BASIC
    // Returns: ZP.FLAGS with device status bits set, C for success, NC for failure to find EEPROM
    Initialize()
    {
#ifdef UNIVERSAL
        LDA #0b11111101
        AND ZP.FLAGS
        STA ZP.FLAGS
#else
        RMB1 ZP.FLAGS // no EEPROM
#endif        
        
        // Clear I2C buffer
        LDA # (I2CInBuffer >> 8)
        Memory.ClearPage();
        
        LDA # I2C.SerialEEPROMAddress
        I2C.Scan();
        if (Z)
        {
#ifdef UNIVERSAL            
            LDA #0b00000010
            ORA ZP.FLAGS
            STA ZP.FLAGS
#else
            SMB1 ZP.FLAGS // EEPROM exists
#endif             
            SEC
        }     
        else
        {
            CLC
        }
    }
    
    // Test if EEPROM is present and responding
    // Output: C set if EEPROM detected, NC if not present
    // Preserves: All registers
    // Munts: None (internally saves/restores)
    Detect()
    {
#ifdef UNIVERSAL
        PHA
        LDA ZP.FLAGS
        AND #0b00000010
        if (NZ)
        {
            PLA
            SEC  // Set C flag (C = EEPROM present)
            return;
        }
        PLA
#else
        if (BBS1, ZP.FLAGS) // EEPROM exists
        {
            SEC  // Set C flag (C = EEPROM present)
            return;
        }
#endif        
        CLC      // Clear C flag (NC = no EEPROM)
    }
    
    // Get EEPROM size in kilobytes
    // Returns: A register contains size in K (32, 64, or 128) and C, or 0 and NC if no EEPROM
    GetSize()
    {
#ifdef UNIVERSAL        
        LDA ZP.FLAGS
        AND #0b00000010
        if (Z)
        {
            LDA #0  
            CLC // No EEPROM detected
            return;
        }
#else
        if (BBR1, ZP.FLAGS) // EEPROM exists?
        {
            LDA #0  
            CLC // No EEPROM detected
            return;
        }
#endif        
        
        // EEPROM is present, return configured size
    #ifdef SERIAL128BYTEPAGES
        // Could be either 24AA512 (64K) or 24AA1026 (128K)
        // Default to 64K unless specifically configured for 128K
        #ifdef EEPROM_128K
            LDA #128    // 24AA1026 = 128K
        #else
            LDA #64     // 24AA512 = 64K (default for 128-byte pages)
        #endif
    #endif
    #ifdef SERIAL64BYTEPAGES
        LDA #32         // 24C256 = 32K
    #endif
        SEC
    }
    
    // Write single 256-byte page from RAM to EEPROM
    // Input: ZP.IDXH = RAM source address (16-bit, page-aligned, ZP.IDXL set to 0)
    //        ZP.IDYH = EEPROM destination address (16-bit, page-aligned, ZP.IDYL set to 0)
    // Output: 256 bytes copied from RAM to EEPROM
    //         ZP.IDX advanced by 256 bytes
    //         ZP.IDY advanced by 256 bytes
    // Modifies: IDY, All registers used by copyPageToEEPROM():
    //           ZP.OutB (I2C operations), ZP.TOP and TARGET0-3 (timing delay), X (byte counter), IDX and IDY
    // Note: High-level interface that handles EEPROM page size differences automatically
    //       For 128-byte EEPROMs: calls copyPageToEEPROM() twice
    //       For 64-byte EEPROMs: calls copyPageToEEPROM() four times
    WritePage()
    {
        PHA
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        // LSB's always zero
#ifdef UNIVERSAL
        LDA #0
        STA ZP.IDYL
        STA ZP.IDXL
#else        
        STZ ZP.IDYL
        STZ ZP.IDXL
#endif
        
        // copy a 256 byte 6502 page:
        copyPageToEEPROM();
        copyPageToEEPROM();
#ifdef SERIAL64BYTEPAGES
        copyPageToEEPROM();
        copyPageToEEPROM();
#endif
        // TODO : copyPageToEEPROM already has a built in 5 ms delay (test without this delay)
        
        // Proper EEPROM write completion delay
        // EEPROM write operations need 5-10ms to complete
        LDA #10          // 10 milliseconds delay
        STA ZP.TOP0
        LDA #0
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Time.Delay(); // Proper timer-based delay
        
        PLA
        STA ZP.TOP3
        PLA
        STA ZP.TOP2
        PLA
        STA ZP.TOP1
        PLA
        STA ZP.TOP0
        PLA
    }
    
    // Read single 256-byte page from EEPROM to RAM
    // Input: ZP.IDY = EEPROM source address (16-bit, page-aligned, ZP.IDXL set to 0)
    //        ZP.IDX = RAM destination address (16-bit, page-aligned, ZP.IDYL set to 0)
    // Output: 256 bytes copied from EEPROM to RAM
    //         ZP.IDY advanced by 256 bytes
    //         ZP.IDX advanced by 256 bytes
    // Modifies: All registers used by copyProgramPage()
    // Note: High-level interface that handles EEPROM page size differences automatically
    //       For 128-byte EEPROMs: calls copyProgramPage() twice
    //       For 64-byte EEPROMs: calls copyProgramPage() four times
    ReadPage()
    {
        // IDY contains the source address (in EEPROM)
        // IDX contains the destination address
        
        // LSB's always zero
#ifdef UNIVERSAL
        LDA #0
        STA ZP.IDYL
        STA ZP.IDXL
#else                
        STZ ZP.IDYL
        STZ ZP.IDXL
#endif
        
        // copy a 256 byte 6502 page:
        copyProgramPage();
        copyProgramPage();
#ifdef SERIAL64BYTEPAGES
        copyProgramPage();
        copyProgramPage();
#endif
    }    
}
