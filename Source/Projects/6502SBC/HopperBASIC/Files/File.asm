unit File
{
    // Buffer allocation (3 x 256 bytes)
    const uint FileDataBuffer       = Address.FileSystemBuffers;        // [0-255]
    const uint WorkingSectorBuffer  = Address.FileSystemBuffers + 256;  // [256-511]  
    const uint FATBuffer            = Address.FileSystemBuffers + 512;  // [512-767]
    
    // File System Zero Page Variables (aliases to existing slots)
    const byte SectorDestL          = ZP.FDESTINATIONADDRESSL; // Destination address for sector ops
    const byte SectorDestH          = ZP.FDESTINATIONADDRESSH;
    const byte TransferLengthL      = ZP.FLENGTHL;             // Bytes to transfer (LSB)
    const byte TransferLengthH      = ZP.FLENGTHH;             // Bytes to transfer (MSB)

    // Check if character is valid for filename (alphanumeric + period)
    // Input: A = character to test
    // Output: C set if valid, NC if invalid
    // Preserves: X, Y
    // Munts: A only
    IsValidFilenameChar()
    {
        loop // single exit block
        {
            // First check if it's alphanumeric
            Char.IsAlphaNumeric();
            if (C)
            {
                SEC  // Valid alphanumeric character
                break;
            }
            
            // Not alphanumeric, check if it's a period
            CMP #'.'
            if (Z)
            {
                SEC  // Valid period character
                break;
            }
            
            // Not alphanumeric and not period
            CLC  // Invalid character
            break;
        } // single exit
    }
    
    // Validate string as filename (length 1-12, valid characters)
    // Input: ZP.STR = pointer to null-terminated uppercase string
    // Output: C set if valid filename, NC if invalid
    //         A = actual string length
    // Preserves: X, Y, ZP.STR
    // Munts: A only
    ValidateFilename()
    {
        PHY
        
        loop // single exit block
        {
            // Get string length
            LDX ZP.STRL
            LDY ZP.STRH
            String.Length();  // Returns length in A
            
            // Check length bounds (1-12 characters)
            if (Z)  // Length is 0
            {
                CLC  // Invalid - empty filename
                break;
            }
            
            CMP #13
            if (C)  // Length >= 13
            {
                CLC  // Invalid - filename too long
                break;
            }
            
            // Length is valid (1-12), now check each character
            // Save length for return value
            PHA  // Save length on stack
            
            LDY #0
            loop  // Character validation loop
            {
                LDA [ZP.STR], Y
                if (Z) { break; }  // End of string - all characters valid
                
                IsValidFilenameChar();
                if (NC)
                {
                    // Invalid character found
                    PLA  // Restore length to A
                    CLC  // Invalid filename
                    break;  // Exit both loops
                }
                
                INY
            }
            
            // If we get here, all characters were valid
            PLA  // Restore length to A
            SEC  // Valid filename
            break;
        } // single exit
        
        PLY
    }
    
    // List all files in directory
    // Output: File list printed to serial, C set if successful
    // Preserves: X, Y
    // Munts: A
    DirectoryList()
    {
        TODO(); BIT ZP.EmulatorPCL
    }
    
    // Delete a file
    // Input: ZP.STR = pointer to filename (null-terminated, uppercase)
    // Output: C set if file deleted successfully, NC if error (file not found)
    // Preserves: X, Y
    // Munts: A, file system state
    DeleteFile()
    {
        TODO(); BIT ZP.EmulatorPCL
    }
    
    // Format EEPROM and initialize empty file system
    // Output: C set if format successful, NC if error
    //         All existing files destroyed, file system reset
    // Preserves: X, Y
    // Munts: A, entire EEPROM contents
    // Note: User confirmation already handled by Console before calling
    Format()
    {
        PHX
        PHY
        loop
        {
            // Clear and initialize FAT
            clearFATBuffer();
            
            LDA #1
            STA FATBuffer + 0    // Sector 0 = end-of-chain (FAT system sector)
            STA FATBuffer + 1    // Sector 1 = end-of-chain (directory system sector)
            // FATBuffer[2..255] remain 0 = free sectors
            
            // Write FAT to EEPROM
            writeFAT();
            
            // Clear and write empty directory
            clearDirectoryBuffer();
            writeDirectory();
            
            // Success - file system initialized
            SEC
            break;
        } // single exit
        PLY
        PLX
    }
    
    //==============================================================================
    // WORKER FUNCTIONS (commonly used operations)
    //==============================================================================
    
    // Clear FAT buffer to all zeros
    clearFATBuffer()
    {
        LDA #(FATBuffer % 256)
        STA SectorDestL
        LDA #(FATBuffer / 256)
        STA SectorDestH
        LDA #0
        STA TransferLengthL
        LDA #1                    // 256 bytes (1 page)
        STA TransferLengthH
        Memory.Clear();
    }
    
    // Clear directory buffer to all zeros  
    clearDirectoryBuffer()
    {
        LDA #(WorkingSectorBuffer % 256)
        STA SectorDestL
        LDA #(WorkingSectorBuffer / 256)
        STA SectorDestH
        LDA #0
        STA TransferLengthL
        LDA #1                    // 256 bytes (1 page)
        STA TransferLengthH
        Memory.Clear();
    }
    
    // Load FAT from EEPROM sector 0 into FATBuffer
    loadFAT()
    {
        STZ ZP.IDYH              // EEPROM address MSB = sector 0 (must be page aligned)
        STZ ZP.IDYL
        
        LDA #(FATBuffer / 256)   // RAM address MSB = FATBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.ReadPage();
    }
    
    // Write FATBuffer to EEPROM sector 0
    writeFAT()
    {
        STZ ZP.IDYH              // EEPROM address MSB = sector 0 (must be page aligned)
        
        LDA #(FATBuffer / 256)   // RAM address MSB = FATBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
    }
    
    // Load directory from EEPROM sector 1 into WorkingSectorBuffer
    loadDirectory()
    {
        LDA #1                           // EEPROM address MSB = sector 1  (must be page aligned)
        STA ZP.IDYH              
        
        LDA #(WorkingSectorBuffer / 256) // RAM address MSB = WorkingSectorBuffer (must be page aligned)
        STA ZP.IDXH

        EEPROM.ReadPage();
    }
    
    // Write WorkingSectorBuffer to EEPROM sector 1
    writeDirectory()
    {
        LDA #1                           // EEPROM address MSB = sector 1  (must be page aligned)
        STA ZP.IDYH             
        
        LDA #(WorkingSectorBuffer / 256) // RAM address MSB = WorkingSectorBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
    }
    
    // Read arbitrary sector into FileDataBuffer
    // Input: A = sector number
    readSector()
    {
        STA ZP.IDYH                 // EEPROM address MSB = sector number (must be page aligned)
        
        LDA #(FileDataBuffer / 256) // RAM address MSB = FileDataBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.ReadPage();
    }
    
    // Write FileDataBuffer to arbitrary sector
    // Input: A = sector number
    writeSector()
    {
        STA ZP.IDYH                 // EEPROM address MSB = sector number (must be page aligned)
        
        LDA #(FileDataBuffer / 256) // RAM address MSB = FileDataBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
    }

//#ifdef DEBUG
    // Diagnostic dump of drive state
    // Output: Drive state printed to serial, C set if successful
    // Preserves: X, Y
    // Munts: A
    DumpDriveState()
    {
        PHX
        PHY
        loop
        {
      
            
            // Load current FAT and directory
            loadFAT();
            
            loadDirectory();
            
            // Print header
            LDA #(dumpHeader % 256)
            STA ZP.STRL
            LDA #(dumpHeader / 256)
            STA ZP.STRH
            Print.String();
            Print.NewLine();
            
            // Dump directory entries
            dumpDirectoryEntries();
            
            // Dump FAT allocation map
            dumpFATMap();
            
            // Dump sector statistics
            dumpSectorStats();
            
            SEC
            break;
        } // single exit
        PLY
        PLX
    }
    
    //==============================================================================
    // DIAGNOSTIC HELPERS
    //==============================================================================
    
    const string dumpHeader  = "=== DRIVE STATE DUMP ===";
    const string dirHeader   = "Directory Entries:";
    const string fatHeader   = "FAT Map (. = free, E = end-of-chain, * = used):";
    const string statsHeader = "Sector Statistics:";
    const string freeLabel   = "Free sectors: ";
    const string usedLabel   = "Used sectors: ";
    const string endLabel    = "End-of-chain: ";
    
    // Dump directory entries (assumes directory loaded in WorkingSectorBuffer)
    dumpDirectoryEntries()
    {
        LDA #(dirHeader % 256)
        STA ZP.STRL
        LDA #(dirHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        LDY #0                   // Directory entry offset
        LDX #0                   // Entry number counter
        
        loop
        {
            // Check if entry is in use (length != 0)
            LDA WorkingSectorBuffer + 0, Y
            ORA WorkingSectorBuffer + 1, Y
            if (NZ)
            {
                 
                
                // Print entry number
                TXA
                CLC
                ADC #'0'                // Convert to ASCII digit
                Print.Char();
                LDA #':'
                Print.Char();
                LDA #' '
                Print.Char();
                
                // Print filename (scan until high bit found)
                printFilenameFromDirectory();
                
                // Print file info
                LDX #1
                Print.Spaces();      // Single space
                printFileSizeFromDirectory();
                LDA #' '
                Print.Char();
                LDA #'@'
                Print.Char();
                LDA WorkingSectorBuffer + 2, Y  // Start sector
                STA ZP.TOPL
                LDA #0
                STA ZP.TOPH
                STA ZP.TOPT
                Print.Hex();
                
                Print.NewLine();
                INX                  // Next entry number
            
            }
            TYA
            CLC
            ADC #16              // Next 16-byte entry
            TAY
            if (Z) { break; } // Continue until Y wraps to 0
        } // loop
        
        Print.NewLine();
    }
    
    // Dump FAT allocation map (assumes FAT loaded in FATBuffer)
    dumpFATMap()
    {
        LDA #(fatHeader % 256)
        STA ZP.STRL
        LDA #(fatHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        LDY #0                   // Sector counter
        LDX #0                   // Position in line counter
        
        loop
        {
            // Print sector number header every 16 sectors
            TXA
            if (Z)
            {
                TYA
                STA ZP.TOPL
                LDA #0
                STA ZP.TOPH
                STA ZP.TOPT
                Print.Hex();
                LDA #':'
                Print.Char();
                LDA #' '
                Print.Char();
            }
            
            // Print FAT value as character
            LDA FATBuffer, Y
            CMP #0
            if (Z)
            {
                LDA #'.'             // Free sector
                Print.Char();
            }
            else
            {
                CMP #1
                if (Z)
                {
                    LDA #'E'         // End of chain
                    Print.Char();
                }
                else
                {
                    LDA #'*'         // Used sector (points to another)
                    Print.Char();
                }
            }
            
            INX                      // Increment position counter
            INY                      // Next sector
            if (Z) { break; } // Y wrapped to 0 = done with all 256 sectors
            
            TXA
            CMP #16                  // End of line?
            if (Z)
            {
                Print.NewLine();
                LDX #0               // Reset position counter
            }
        }
        
        
        Print.NewLine();
        Print.NewLine();
    }
    
    // Dump sector statistics (assumes FAT loaded in FATBuffer)
    dumpSectorStats()
    {
        LDA #(statsHeader % 256)
        STA ZP.STRL
        LDA #(statsHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        // Count sector types
        LDY #0                   // Sector counter
        LDX #0                   // Free sector count
        LDA #0
        STA TransferLengthL      // Used sector count
        STA TransferLengthH      // End-of-chain count
        
        loop
        {
            LDA FATBuffer, Y
            CMP #0
            if (Z)
            {
                INX                  // Free sector
            }
            else
            {
                CMP #1
                if (Z)
                {
                    INC TransferLengthH  // End-of-chain
                }
                else
                {
                    INC TransferLengthL  // Used sector (points to another)
                }
            }
            
            INY
            if (Z) { break; }
        }
        
        // Print free sectors
        LDA #(freeLabel % 256)
        STA ZP.STRL
        LDA #(freeLabel / 256)
        STA ZP.STRH
        Print.String();
        STX ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        Print.NewLine();
        
        // Print used sectors
        LDA #(usedLabel % 256)
        STA ZP.STRL
        LDA #(usedLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA TransferLengthL
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        Print.NewLine();
        
        // Print end-of-chain sectors  
        LDA #(endLabel % 256)
        STA ZP.STRL
        LDA #(endLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA TransferLengthH
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        Print.NewLine();
    }
    
    // Print filename from current directory entry (Y = entry offset)
    printFilenameFromDirectory()
    {
        PHY
        TYA
        CLC
        ADC #3                   // Offset to filename field
        TAY
        
        loop
        {
            LDA WorkingSectorBuffer, Y
            
            PHA                      // Save character
            AND #0x7F                // Clear high bit
            Print.Char();            // Print character
            PLA                      // Restore character
            
            if (MI) { break; }       // High bit set = last character
            INY
        } // single exit
        PLY
    }
    
    // Print file size from current directory entry (Y = entry offset)
    printFileSizeFromDirectory()
    {
        LDA WorkingSectorBuffer + 0, Y  // Length LSB
        STA ZP.TOPL
        LDA WorkingSectorBuffer + 1, Y  // Length MSB
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPT
        Print.Decimal();
    }
//#endif
}
