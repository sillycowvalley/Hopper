unit EEPROM
{
    uses "/Source/Runtime/6502/I2C"
    uses "/Source/Runtime/6502/Devices/SerialEEPROM"
    
    // I2C and EEPROM initialization for Hopper BASIC
    // Returns: ZP.PLUGNPLAY with device status bits set
    Initialize()
    {
        // Clear the I2C input buffer (1 page = 256 bytes)
        LDA #0
        STA IDXL
        LDA # (I2CInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages();
        
        // Initialize device detection register
        STZ ZP.PLUGNPLAY
        
        // Scan for Serial EEPROM (address 0x50) 
        LDA # I2C.SerialEEPROMAddress
        I2C.Scan();
        if (Z) // Z flag set means device responded (ACK received)
        {
            SMB1 ZP.PLUGNPLAY  // Set bit 1: EEPROM present
        }
    }
    
    // Test if EEPROM is present and responding
    // Output: C set if EEPROM detected, NC if not present
    // Preserves: All registers
    // Munts: None (internally saves/restores)
    Detect()
    {
        if (BBS1, ZP.PLUGNPLAY) // Test bit 1
        {
            SEC  // Set C flag (C = EEPROM present)
            return
        }
        CLC      // Clear C flag (NC = no EEPROM)
    }
    
    // Get EEPROM size in kilobytes
    // Returns: A register contains size in K (32, 64, or 128) and C, or 0 and NC if no EEPROM
    GetSize()
    {
        if (BBR1, ZP.PLUGNPLAY) // Test bit 1 - if clear, no EEPROM
        {
            LDA #0  
            CLC // No EEPROM detected
            return;
        }
        
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
    // Input: ZP.IDX = RAM source address (16-bit, page-aligned recommended)
    //        ZP.IDY = EEPROM destination address (16-bit, page-aligned recommended)
    // Output: 256 bytes copied from RAM to EEPROM
    //         ZP.IDX advanced by 256 bytes
    //         ZP.IDY advanced by 256 bytes
    // Modifies: All registers used by copyPageToEEPROM()
    // Note: High-level interface that handles EEPROM page size differences automatically
    //       For 128-byte EEPROMs: calls copyPageToEEPROM() twice
    //       For 64-byte EEPROMs: calls copyPageToEEPROM() four times
    WritePage()
    {
        // IDX contains the source address
        // IDY contains the destination address (in EEPROM)
        
        // copy a 256 byte 6502 page:
        SerialEEPROM.copyPageToEEPROM();
        SerialEEPROM.copyPageToEEPROM();
#ifdef SERIAL64BYTEPAGES
        SerialEEPROM.copyPageToEEPROM();
        SerialEEPROM.copyPageToEEPROM();
#endif
    }
    
    // Read single 256-byte page from EEPROM to RAM
    // Input: ZP.IDY = EEPROM source address (16-bit, page-aligned recommended)
    //        ZP.IDX = RAM destination address (16-bit, page-aligned recommended)
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
        
        // copy a 256 byte 6502 page:
        SerialEEPROM.copyProgramPage();
        SerialEEPROM.copyProgramPage();
#ifdef SERIAL64BYTEPAGES
        SerialEEPROM.copyProgramPage();
        SerialEEPROM.copyProgramPage();
#endif
    }    
}
