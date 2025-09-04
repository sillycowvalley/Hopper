unit EEPROM
{
    uses "/Source/Runtime/6502/I2C"
    uses "/Source/Runtime/6502/Devices/SerialEEPROM"
    
    // I2C and EEPROM initialization for Hopper BASIC
    // Returns: ZP.PLUGNPLAY with device status bits set, C for success, NC for failure to find EEPROM
    Initialize()
    {
        STZ ZP.PLUGNPLAY
        
        // Clear I2C buffer
        LDA # (I2CInBuffer >> 8)
        Memory.ClearPage();
        
        STZ ZP.PLUGNPLAY
        LDA # I2C.SerialEEPROMAddress
        I2C.Scan();
        if (Z)
        {
            SMB1 ZP.PLUGNPLAY
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
        if (BBS1, ZP.PLUGNPLAY) // Test bit 1
        {
            SEC  // Set C flag (C = EEPROM present)
            return;
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
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH
        PHA
        
        // LSB's always zero
        STZ ZP.IDYL
        STZ ZP.IDXL
        
        // copy a 256 byte 6502 page:
        SerialEEPROM.copyPageToEEPROM();
        SerialEEPROM.copyPageToEEPROM();
#ifdef SERIAL64BYTEPAGES
        SerialEEPROM.copyPageToEEPROM();
        SerialEEPROM.copyPageToEEPROM();
#endif
        // TODO : copyPageToEEPROM already has a built in 5 ms delay (test without this delay)
        
        // Proper EEPROM write completion delay
        // EEPROM write operations need 5-10ms to complete
        LDA #10          // 10 milliseconds delay
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        Time.DelayTOP(); // Proper timer-based delay
        
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
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
        STZ ZP.IDYL
        STZ ZP.IDXL
        
        // copy a 256 byte 6502 page:
        SerialEEPROM.copyProgramPage();
        SerialEEPROM.copyProgramPage();
#ifdef SERIAL64BYTEPAGES
        SerialEEPROM.copyProgramPage();
        SerialEEPROM.copyProgramPage();
#endif
    }    
}
