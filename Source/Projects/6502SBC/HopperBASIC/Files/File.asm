unit File
{
    // Buffer allocation (3 x 256 bytes)
    const uint FileDataBuffer       = Address.FileSystemBuffers;        // [0-255]
    const uint WorkingSectorBuffer  = Address.FileSystemBuffers + 256;  // [256-511]  
    const uint FATBuffer            = Address.FileSystemBuffers + 512;  // [512-767]
    
    // File System Zero Page Variables (aliases to existing slots)
    const byte SectorSource         = ZP.FSOURCEADDRESS;       // for use with LDX [SectorSource], Y for example
    const byte SectorSourceL        = ZP.FSOURCEADDRESSL;      // Source address for sector ops
    const byte SectorSourceH        = ZP.FSOURCEADDRESSH;     
    
    const byte TransferLength       = ZP.FLENGTH;              // for use with LDX [TransferLength], Y for example
    const byte TransferLengthL      = ZP.FLENGTHL;             // Bytes to transfer (LSB)
    const byte TransferLengthH      = ZP.FLENGTHH;             // Bytes to transfer (MSB)
    
    const byte CurrentFileSector    = ZP.LCURRENTH;            // Current sector number in file
    const byte FileStartSector      = ZP.LNEXTH;               // First sector of current file
    const byte CurrentFileEntry     = ZP.LCURRENTL;            // Directory entry index (0-15)
    
    const byte FilePosition         = ZP.LHEADL;               // Current byte position in file (16-bit)
    const byte FilePositionL        = ZP.LHEADL;               //    "
    const byte FilePositionH        = ZP.LHEADH;               //    "
    
    const byte SectorPosition       = ZP.LHEADX;               // Byte position within current sector (0-255)
    const byte FileSystemFlags      = ZP.FSIGN;                // Operation mode and status flags
    
    const byte NextFileSector       = ZP.LNEXTL;               // Next sector in chain (from FAT)
    
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
    // Preserves: X, ZP.STR
    // Munts: A, Y only
    ValidateFilename()
    {
        String.Length();  // Returns length in Y
        
        loop
        {
            // Check length bounds
            CPY # 0
            if (Z) { CLC break; }  // Invalid - empty
            CPY # 13
            if (C){ CLC break; }   // Invalid - too long  
            
            // Check each character
            LDY #0
            loop
            {
                LDA [ZP.STR], Y
                if (Z)  { SEC break; }// Valid - reached end
                IsValidFilenameChar();
                if (NC){ CLC break; } // Invalid character
                INY
            }
            break;
        } // single exit
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
    // Munts: A, X, Y, entire EEPROM contents
    // Note: User confirmation already handled by Console before calling
    Format()
    {
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
    }
    
    
    // Create new file for writing (or overwrite existing)
    // Input: ZP.STR = pointer to filename (uppercase, null-terminated)
    // Output: C set if successful, NC if error
    // Munts: A, X, Y, file system state
    StartSave()
    {
        loop // Single exit for cleanup
        {
            // Validate filename format
            File.ValidateFilename();
            if (NC)
            {
                Error.InvalidFilename(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Load directory and FAT into buffers
            loadDirectory();
            loadFAT();
            
            // Find free directory entry (or existing file to overwrite)
            findFreeDirectoryEntry();
            if (NC)
            {
                Error.DirectoryFull(); BIT ZP.EmulatorPCL
                break;
            }
            // CurrentFileEntry now contains entry index (0-15)
            
            // Allocate first sector for file data
            allocateFirstSector();
            if (NC)
            {
                Error.EEPROMFull(); BIT ZP.EmulatorPCL
                break;
            }
            // FileStartSector and CurrentFileSector now set
            
            // Initialize file state for save operation
            initializeSaveState();
            
            // Clear file data buffer
            clearFileDataBuffer();
            
            // Success - file ready for AppendStream calls
            SEC
            break;
        }
    }
    
    
    // Find free directory entry (or existing file to overwrite)
    // Output: C set if entry found, NC if directory full
    //         CurrentFileEntry = entry index (0-15) if found
    // Munts: A, Y
    findFreeDirectoryEntry()
    {
        LDY #0                   // Directory entry index (0, 1, 2, ..., 15)
        
        loop
        {
            // Calculate byte offset: Y * 16
            TYA
            ASL ASL ASL ASL      // Y * 16 = directory entry offset
            TAX                  // X = byte offset in directory
            
            // Check if entry is free (fileLength == 0)
            LDA WorkingSectorBuffer + 0, X  // Length LSB
            ORA WorkingSectorBuffer + 1, X  // Length MSB
            if (Z)
            {
                // Found free entry
                STY CurrentFileEntry
                SEC
                return;
            }
            
            // Check if filename matches (for overwrite)
            checkFilenameMatch();
            if (C)
            {
                // Found existing file - overwrite it
                STY CurrentFileEntry
                SEC
                return;
            }
            
            INY
            CPY #16              // 16 entries maximum
            if (Z)
            {
                // Directory full
                CLC
                return;
            }
        }
    }
    
    // Check if current directory entry filename matches ZP.STR
    // Input: X = directory entry byte offset, ZP.STR = filename to match
    // Output: C set if match, NC if no match
    // Preserves: X, Y
    // Munts: A
    checkFilenameMatch()
    {
        PHA
        PHY
        
        // Point to filename field in directory entry (offset +3)
        TXA
        CLC
        ADC #3
        TAY                      // Y = filename start in WorkingSectorBuffer
        
        LDY #0                   // Index into ZP.STR filename
        LDX #3                   // Index into directory filename field
        
        loop
        {
            // Get character from input filename
            LDA [ZP.STR], Y
            if (Z)                // End of input filename
            {
                // Check if directory filename also ends here
                TXA
                CLC
                ADC CurrentFileEntry
                ASL ASL ASL ASL      // * 16 for entry offset
                CLC
                ADC #3               // + 3 for filename field
                TAX
                LDA WorkingSectorBuffer, X
                if (MI)              // High bit set = last char
                {
                    SEC              // Perfect match
                }
                else
                {
                    CLC              // Input ended but directory name continues
                }
                break;
            }
            
            // Get character from directory filename
            TXA
            CLC
            ADC CurrentFileEntry
            ASL ASL ASL ASL          // * 16 for entry offset  
            TAX
            LDA WorkingSectorBuffer, X
            AND #0x7F                // Clear high bit for comparison
            
            // Compare characters
            CMP [ZP.STR], Y
            if (NZ)
            {
                CLC                  // No match
                break;
            }
            
            // Check if this was last character in directory name
            LDA WorkingSectorBuffer, X
            if (MI)                  // High bit set = last character
            {
                // Directory name ended, check if input also ends
                INY
                LDA [ZP.STR], Y
                if (Z)
                {
                    SEC              // Perfect match
                }
                else
                {
                    CLC              // Directory ended but input continues  
                }
                break;
            }
            
            INY                      // Next character
            INX
            CPX #16                  // Max filename length (13 + 3 header bytes)
            if (Z)
            {
                CLC                  // Filename too long - no match
                break;
            }
        }
        
        PLY
        PLA
    }
    
    // Allocate first free sector from FAT
    // Output: C set if sector allocated, NC if disk full
    //         FileStartSector and CurrentFileSector = allocated sector number
    // Munts: A, Y
    allocateFirstSector()
    {
        LDY #2                   // Start from sector 2 (skip FAT and directory)
        
        loop
        {
            LDA FATBuffer, Y
            if (Z)                // Free sector found
            {
                // Mark sector as end-of-chain (initial single sector file)
                LDA #1
                STA FATBuffer, Y
                
                // Set file state
                STY FileStartSector
                STY CurrentFileSector
                
                SEC
                break;
            }
            
            INY
            if (Z)                // Y wrapped to 0 - checked all sectors
            {
                CLC               // Disk full
                break;
            }
        } // single exit
    }
    
    // Initialize file state for save operation
    // Munts: A
    initializeSaveState()
    {
        // Clear file position counters
        STZ FilePosition         // FilePositionL
        STZ FilePosition + 1     // FilePositionH
        STZ SectorPosition       // Byte position within current sector
        
        // Set operation mode flags
        LDA #0b00000011          // Bit 0: save mode, Bit 1: file open
        STA FileSystemFlags
        
        // Clear next sector (will be allocated when needed)
        STZ NextFileSector
    }
    
    // Clear file data buffer to all zeros
    clearFileDataBuffer()
    {
        LDA #(FileDataBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // Clear FAT buffer to all zeros
    clearFATBuffer()
    {
        LDA #(FATBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // Clear directory buffer to all zeros  
    clearDirectoryBuffer()
    {
        LDA #(WorkingSectorBuffer / 256) // MSB - assume page aligned
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
    // Input:  A = 1 to load from EEPROM, A = 0 to just show current RAM
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
            CMP #1
            if (Z)
            {
                loadFAT();
            }
            
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
    
    const string fileStateHeader     = "=== FILE STATE ===";
    const string currentEntryLabel   = "CurrentFileEntry: ";  
    const string startSectorLabel    = "FileStartSector: ";
    const string currentSectorLabel  = "CurrentFileSector: ";
    const string nextSectorLabel     = "NextFileSector: ";
    const string filePositionLabel   = "FilePosition: ";
    const string sectorPositionLabel = "SectorPosition: ";
    const string flagsLabel          = "FileSystemFlags: ";
    
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
    
    // Dump current file operation state (ZP variables)
    // Output: File state printed to serial, C set if successful  
    // Preserves: X, Y
    // Munts: A
    DumpFileState()
    {
        PHX
        PHY
        
        // Print header
        LDA #(fileStateHeader % 256)
        STA ZP.STRL
        LDA #(fileStateHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        // CurrentFileEntry
        LDA #(currentEntryLabel % 256)
        STA ZP.STRL
        LDA #(currentEntryLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA CurrentFileEntry
        Print.Hex();
        Print.NewLine();
        
        // FileStartSector  
        LDA #(startSectorLabel % 256)
        STA ZP.STRL
        LDA #(startSectorLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA FileStartSector
        Print.Hex();
        Print.NewLine();
        
        // CurrentFileSector
        LDA #(currentSectorLabel % 256)
        STA ZP.STRL
        LDA #(currentSectorLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA CurrentFileSector
        Print.Hex();
        Print.NewLine();
        
        // NextFileSector
        LDA #(nextSectorLabel % 256)
        STA ZP.STRL
        LDA #(nextSectorLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA NextFileSector
        Print.Hex();
        Print.NewLine();
        
        // FilePosition (16-bit)
        LDA #(filePositionLabel % 256)
        STA ZP.STRL
        LDA #(filePositionLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA FilePosition         // FilePositionL
        STA ZP.TOPL
        LDA FilePosition + 1     // FilePositionH  
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPT
        Print.Hex();
        Print.NewLine();
        
        // SectorPosition
        LDA #(sectorPositionLabel % 256)
        STA ZP.STRL
        LDA #(sectorPositionLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA SectorPosition
        Print.Hex();
        Print.NewLine();
        
        // FileSystemFlags (binary representation)
        LDA #(flagsLabel % 256)
        STA ZP.STRL
        LDA #(flagsLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA FileSystemFlags
        Print.Hex();
        LDA #' '
        Print.Char();
        LDA #'('
        Print.Char();
        
        // Print flag meanings
        LDA FileSystemFlags
        AND #0b00000001
        if (NZ)
        {
            LDA #'S'               // Save mode
            Print.Char();
        }
        else
        {
            LDA #'L'               // Load mode
            Print.Char();
        }
        
        LDA FileSystemFlags
        AND #0b00000010
        if (NZ)
        {
            LDA #'O'               // Open
            Print.Char();
        }
        else
        {
            LDA #'C'               // Closed
            Print.Char();
        }
        
        LDA FileSystemFlags
        AND #0b00000100
        if (NZ)
        {
            LDA #'E'               // EOF
            Print.Char();
        }
        else
        {
            LDA #'-'               // Not EOF
            Print.Char();
        }
        
        LDA #')'
        Print.Char();
        Print.NewLine();
        
        SEC                        // Always successful
        
        PLY
        PLX
    }
    
//#endif
}
