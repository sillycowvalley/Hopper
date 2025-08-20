unit File
{
    // Buffer allocation (3 x 256 bytes)
    const uint FileDataBuffer       = Address.FileSystemBuffers;        // [0-255]
    const uint DirectoryBuffer      = Address.FileSystemBuffers + 256;  // [256-511]  
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
    
    
    const byte NextFileSector       = ZP.LNEXTL;               // Next sector in chain (from FAT)
    
    // Additional ZP aliases needed for AppendStream
    // WARNINGL ZP.M0 - ZP.M3 are used by Time.Delay() (TARGET0-3)
    const byte BytesRemainingL      = ZP.M4;                   // 16-bit: bytes left to copy
    const byte BytesRemainingH      = ZP.M5;
    
    const byte SectorPositionL      = ZP.M6;                   // Byte position within current sector (0-255) .. with possible overflow to 256 (NO, IT IS NOT THE SAME AS ZERO IF YOU ARE IDIOTS LIKE US)
    const byte SectorPositionH      = ZP.M7;
    
    
    const string dirListHeader       = "FILES:";
    const string noFilesMsg          = "No files found.";
    const string bytesLabel          = " bytes";
    const string filesLabel          = " files, ";
    const string bytesUsedLabel      = " bytes used";
    
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
            
            // Invalid character
            CLC
            break;
        }
    }
    
    // Validate filename format (alphanumeric + period, 1-13 chars)
    // Input: ZP.STR = pointer to null-terminated filename
    // Output: C set if valid, NC if invalid  
    // Preserves: X, Y
    // Munts: A
    ValidateFilename()
    {
        PHY
        
        loop // single exit block
        {
            LDY #0
            
            // Check if filename is empty
            LDA [ZP.STR], Y
            if (Z)
            {
                CLC  // Empty filename invalid
                break;
            }
            
            // Check each character and count length
            loop
            {
                LDA [ZP.STR], Y
                if (Z) { break; }  // End of string
                
                // Check if character is valid
                IsValidFilenameChar();
                if (NC)
                {
                    CLC  // Invalid character found
                    PLY
                    return;
                }
                
                INY
                CPY #14  // Max 13 characters + null terminator
                if (Z)
                {
                    CLC  // Filename too long
                    PLY
                    return;
                }
            }
            
            // Filename is valid (1-13 chars, valid characters)
            SEC
            break;
        }
        
        PLY
    }
    
    // Format EEPROM with empty file system
    // Output: C set if successful, NC if error
    // Munts: A, X, Y, all file system buffers
    Format()
    {
        loop // Single exit for cleanup
        {
            // Clear all file system buffers
            clearFATBuffer();
            clearDirectoryBuffer();
            clearFileDataBuffer();
            
            // Set FAT system sectors as reserved
            LDA #1  // Reserved marker
            STA FATBuffer + 0   // Sector 0 (FAT)
            STA FATBuffer + 1   // Sector 1 (Directory)
            
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
            
            // Write filename to directory entry
            writeFilenameToDirectory();
            
            // Allocate first sector for file data
            allocateFirstFreeSector(); // -> Y
            if (NC)
            {
                Error.EEPROMFull(); BIT ZP.EmulatorPCL
                break;
            }
            STY FileStartSector
            STY CurrentFileSector
            
            // Update directory entry with start sector
            updateDirectoryStartSector();
            
            // Initialize file state for save operation
            initializeSaveState();
            
            // Clear file data buffer
            clearFileDataBuffer();
            
            // Success - file ready for AppendStream calls
            SEC
            break;
        }
    }
    
    // Write data chunk to current save file  
    // Input: SectorSource   = pointer to data
    //        TransferLength = number of bytes to write
    // Output: C set if successful, NC if error (disk full)
    // Preserves: X, Y
    // Munts: A, file system state  
    AppendStream()
    {
        PHX
        PHY
/*        
Debug.NL(); LDA #'<' COut(); Space();
LDY #0
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
LDA [SectorSource], Y INY HOut();
*/      
        loop // Single exit
        {
            // Copy input parameters to working variables
            LDA TransferLengthL
            STA BytesRemainingL
            LDA TransferLengthH  
            STA BytesRemainingH
            
            loop // Single exit for byte copy
            {
                // Check if done
                LDA BytesRemainingL
                ORA BytesRemainingH
                if (Z)
                { 
                    SEC break; // Set C - success
                }
                
                // Copy one byte from source to file data buffer
                LDY #0
                LDA [SectorSource], Y
                LDY SectorPositionL
                STA FileDataBuffer, Y
                
                // Update source pointer
                INC SectorSourceL
                if (Z) { INC SectorSourceH }
                
                // Update sector position (16-bit increment)
                INC SectorPositionL
                if (Z) { INC SectorPositionH }
                
                // Check if sector full (256 bytes = 0x0100)
                LDA SectorPositionH
                if (NZ) // High byte non-zero means >= 256
                {
                    flushAndAllocateNext();
                    if (NC) { Error.EEPROMFull(); BIT ZP.EmulatorPCL break; }
                }
                
                // Decrement 16-bit remaining count  
                LDA BytesRemainingL
                if (Z)
                {
                    DEC BytesRemainingH
                }
                DEC BytesRemainingL
                
                // Update FilePosition (16-bit)
                INC FilePositionL
                if (Z) { INC FilePositionH }
            }
            
            break;
        }
/*        
Debug.NL(); LDA #'>' COut(); Space();
LDA FileDataBuffer + 0 HOut();
LDA FileDataBuffer + 1 HOut();
LDA FileDataBuffer + 2 HOut();
LDA FileDataBuffer + 3 HOut();
LDA FileDataBuffer + 4 HOut();
LDA FileDataBuffer + 5 HOut();
LDA FileDataBuffer + 6 HOut();
LDA FileDataBuffer + 7 HOut();                
*/
        
        PLY
        PLX
    }
    
    // Close and finalize current save file
    // Output: C set if successful, NC if error
    // Preserves: X, Y  
    // Munts: A, file system state
    EndSave()
    {
        PHX
        PHY
        
        loop // Single exit
        {
            // Write final sector if it has data
            LDA SectorPositionL
            ORA SectorPositionH
            if (NZ)
            {
                LDA CurrentFileSector
                writeSector();
            }
            
            // Update directory entry with final file length
            // Calculate directory entry offset: CurrentFileEntry * 16
            LDA CurrentFileEntry
            ASL A ASL A ASL A ASL A      // * 16
            TAY                          // Y = directory entry offset
            
            // Set file length (FilePosition)
            LDA FilePositionL
            STA DirectoryBuffer + 0, Y
            LDA FilePositionH  
            STA DirectoryBuffer + 1, Y
            
            // Flush metadata to EEPROM
            writeFAT();
            writeDirectory();
            
            SEC // Success
            break;
        }
        
        PLY
        PLX
    }
    
    // List all files in directory with optional debug info
    // Output: Directory listing printed to serial, C set if successful
    // Preserves: X, Y
    // Munts: A, file system buffers
    DirectoryList()
    {
        PHX
        PHY
        
        loop // Single exit
        {
            // Load directory from EEPROM
            loadDirectory();
            
            // Print header
            Print.NewLine();
            LDA #(dirListHeader % 256)
            STA ZP.STRL
            LDA #(dirListHeader / 256)
            STA ZP.STRH
            Print.String();
            Print.NewLine();
            
            // Count files and calculate total bytes
            countFilesAndBytes(); // -> TransferLengthL = file count, TransferLengthH/BytesRemainingL = total bytes
            
            LDA TransferLengthL
            if (Z)
            {
                // No files found
                LDA #(noFilesMsg % 256)
                STA ZP.STRL
                LDA #(noFilesMsg / 256)
                STA ZP.STRH
                Print.String();
                Print.NewLine();
            }
            else
            {
                // Print each file entry
                printAllFileEntries();
                
                Print.NewLine();
                
                // Print summary
                printDirectorySummary();
            }
            
#ifdef DEBUG
            Print.NewLine();
            printDebugDiagnostics();
#endif
            
            SEC
            break;
        }
        
        PLY
        PLX
    }
    
    // Delete file from EEPROM file system
    // Input: ZP.STR = pointer to filename (null-terminated)
    // Output: C set if successful, NC if error
    // Preserves: X, Y
    // Munts: A, file system buffers
    DeleteFile()
    {
        PHX
        PHY
        
        loop // Single exit for cleanup
        {
            // Validate filename format
            ValidateFilename();
            if (NC)
            {
                Error.InvalidFilename(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Load directory and FAT from EEPROM
            loadDirectory();
            loadFAT();
            
            // Find the file in directory
            findFileInDirectory();
            if (NC)
            {
                Error.FileNotFound(); BIT ZP.EmulatorPCL
                break;
            }
            // CurrentFileEntry now contains the directory entry index
            
            // Get start sector from directory entry
            getFileStartSector(); // -> FileStartSector
            
            // Free all sectors used by the file
            freeFileSectorChain();
            
            // Clear the directory entry
            clearDirectoryEntry();
            
            // Write updated directory and FAT back to EEPROM
            writeDirectory();
            writeFAT();
            
            SEC // Success
            break;
        }
        
        PLY
        PLX
    }
    
    // Find file in directory by filename
    // Input: ZP.STR = filename to find
    // Output: C set if found, NC if not found
    //         CurrentFileEntry = directory entry index if found
    // Munts: A, Y
    findFileInDirectory()
    {
        LDY #0                   // Directory entry index (0-15)
        
        loop
        {
            // Calculate directory entry byte offset: Y * 16
            TYA
            ASL A ASL A ASL A ASL A // Y * 16
            TAX                     // X = byte offset in DirectoryBuffer
            
            // Check if entry is in use (fileLength != 0)
            LDA DirectoryBuffer + 0, X  // Length LSB
            ORA DirectoryBuffer + 1, X  // Length MSB
            if (NZ)
            {
                // Entry is in use, check filename match
                checkFilenameMatch(); // Uses X = dir entry offset, ZP.STR = filename
                if (C)
                {
                    // Found the file
                    STY CurrentFileEntry
                    SEC
                    return;
                }
            }
            
            INY
            CPY #16              // 16 directory entries maximum
            if (Z)
            {
                // Not found
                CLC
                return;
            }
        }
    }
    
    // Get start sector from current directory entry
    // Input: CurrentFileEntry = directory entry index
    // Output: FileStartSector = start sector number
    // Munts: A, Y
    getFileStartSector()
    {
        // Calculate directory entry offset: CurrentFileEntry * 16 + 2 (start sector field)
        LDA CurrentFileEntry
        ASL A ASL A ASL A ASL    // * 16
        CLC
        ADC #2                   // + 2 for start sector field offset
        TAY                      // Y = start sector field offset
        
        // Read start sector from directory entry
        LDA DirectoryBuffer, Y
        STA FileStartSector
    }
    
    // Free all sectors in file's FAT chain
    // Input: FileStartSector = first sector to free
    // Output: All sectors in chain marked as free (0) in FAT
    // Munts: A, CurrentFileSector, NextFileSector
    freeFileSectorChain()
    {
        LDA FileStartSector
        STA CurrentFileSector
        
        loop
        {
            // Check if we've reached end of chain
            LDA CurrentFileSector
            if (Z) { break; }        // Sanity check - should not happen
            
            // Get next sector in chain from FAT
            LDY CurrentFileSector
            LDA FATBuffer, Y
            STA NextFileSector
            
            // Mark current sector as free
            LDA #0
            STA FATBuffer, Y
            
            // Check if this was end of chain
            LDA NextFileSector
            CMP #1                   // 1 = end-of-chain marker
            if (Z) { break; }        // End of chain reached
            
            // Move to next sector
            LDA NextFileSector
            STA CurrentFileSector
        }
    }
    
    // Clear directory entry for deleted file
    // Input: CurrentFileEntry = directory entry index
    // Output: Directory entry cleared (all zeros)
    // Munts: A, X, Y
    clearDirectoryEntry()
    {
        // Calculate directory entry offset: CurrentFileEntry * 16
        LDA CurrentFileEntry
        ASL A ASL A ASL A ASL A  // * 16
        TAX                      // X = directory entry start offset
        
        // Clear 16 bytes of directory entry
        LDA #0
        LDY #16
        loop
        {
            DEY
            STA DirectoryBuffer, X
            INX
            CPY #0
            if (NZ) { continue; }
            break;
        }
    }
    
    // Count files and calculate total bytes used
    // Output: TransferLengthL = file count
    //         TransferLengthH/BytesRemainingL = total bytes (16-bit)
    // Munts: A, Y
    countFilesAndBytes()
    {
        STZ TransferLengthL      // File count
        STZ TransferLengthH      // Total bytes high
        STZ BytesRemainingL      // Total bytes low
        
        LDY #0                   // Directory entry offset
        
        loop
        {
            // Calculate byte offset: Y * 16
            TYA
            ASL ASL ASL ASL      // Y * 16 = directory entry offset
            TAX                  // X = byte offset in directory
            
            // Check if entry is in use (fileLength != 0)
            LDA DirectoryBuffer + 0, X  // Length LSB
            ORA DirectoryBuffer + 1, X  // Length MSB
            if (NZ)
            {
                INC TransferLengthL      // Increment file count
                
                // Add file size to total
                CLC
                LDA BytesRemainingL
                ADC DirectoryBuffer + 0, X  // Add length LSB
                STA BytesRemainingL
                LDA TransferLengthH
                ADC DirectoryBuffer + 1, X  // Add length MSB
                STA TransferLengthH
            }
            
            INY
            CPY #16              // 16 entries maximum
            if (Z) { break; }
        }
    }
    
    // Print all file entries with optional debug info
    // Munts: A, X, Y
    printAllFileEntries()
    {
        LDY #0                   // Directory entry offset
        
        loop
        {
            // Calculate byte offset: Y * 16
            TYA
            ASL ASL ASL ASL      // Y * 16 = directory entry offset
            TAX                  // X = byte offset in directory
            
            // Check if entry is in use (fileLength != 0)
            LDA DirectoryBuffer + 0, X  // Length LSB
            ORA DirectoryBuffer + 1, X  // Length MSB
            if (NZ)
            {
                // Print filename
                printFileEntry(); // Input: X = directory entry offset
                
#ifdef DEBUG
                // Print hex dump of first 32 bytes
                printFileHexDump(); // Input: X = directory entry offset
#endif
            }
            
            INY
            CPY #16              // 16 entries maximum
            if (Z) { break; }
        }
    }
    
    // Print single file entry: "FILENAME.EXT    1234 bytes"
    // Input: X = directory entry byte offset
    // Munts: A, Y
    printFileEntry()
    {
        PHY
        PHX
        
        TXA
        TAY
        
        printFilenameFromDirectory(); // Uses Y = filename start offset
        
        // Calculate spaces needed for alignment (aim for 16 total chars)
        // For now, use fixed spacing - could be improved with actual length calculation
        LDX #4
        Print.Spaces();
        
        // Print file size
        // Y already has directory entry offset 
        printFileSizeFromDirectory(); // Uses Y = directory entry offset
        
        LDA #(bytesLabel % 256)
        STA ZP.STRL
        LDA #(bytesLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
        
        PLX
        PLY
    }
    
    // Print filename from current directory entry 
    // Input: Y = directory entry byte offset (0, 16, 32, 48, ...)
    // Output: Filename printed to serial
    // Preserves: Y
    // Munts: A
    printFilenameFromDirectory()
    {
        PHY
        TYA
        CLC
        ADC #3                   // Offset to filename field
        TAY
        
        loop
        {
            LDA DirectoryBuffer, Y
            
            PHA                      // Save character
            AND #0x7F                // Clear high bit
            Print.Char();            // Print character
            PLA                      // Restore character
            
            if (MI) { break; }       // High bit set = last character
            INY
        } // single exit
        PLY
    }
    
    // Print file size from current directory entry
    // Input: Y = directory entry byte offset (0, 16, 32, 48, ...)  
    // Output: File size printed to serial as decimal
    // Munts: A, ZP.TOPL, ZP.TOPH, ZP.TOPT
    printFileSizeFromDirectory()
    {
        LDA DirectoryBuffer + 0, Y  // Length LSB
        STA ZP.TOPL
        LDA DirectoryBuffer + 1, Y  // Length MSB
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPT
        Print.Decimal();
    }
    
    // Print directory summary: "3 files, 2373 bytes used"
    // Input: TransferLengthL = file count, TransferLengthH/BytesRemainingL = total bytes
    // Munts: A
    printDirectorySummary()
    {
        // Print file count
        LDA TransferLengthL
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        
        // Print " files, "
        LDA #(filesLabel % 256)
        STA ZP.STRL
        LDA #(filesLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Print total bytes
        LDA BytesRemainingL
        STA ZP.TOPL
        LDA TransferLengthH
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPT
        Print.Decimal();
        
        // Print " bytes used"
        LDA #(bytesUsedLabel % 256)
        STA ZP.STRL
        LDA #(bytesUsedLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Write filename to directory entry
    // Input: CurrentFileEntry = directory entry index, ZP.STR = filename
    // Munts: A, X, Y
    writeFilenameToDirectory()
    {
        PHY
        
        // Calculate directory entry offset: CurrentFileEntry * 16 + 3 (filename field)
        LDA CurrentFileEntry
        ASL ASL ASL ASL          // * 16
        CLC
        ADC #3                   // + 3 for filename field offset
        TAX                      // X = filename field offset
        
        // Copy filename from ZP.STR to DirectoryBuffer
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) 
            { 
                // End of filename - set high bit on last character
                DEX
                LDA DirectoryBuffer, X
                ORA #0x80           // Set high bit
                STA DirectoryBuffer, X
                break; 
            }
            
            STA DirectoryBuffer, X
            INX
            INY
            CPY #13                 // Max filename length
            if (Z) 
            { 
                // Max length reached - set high bit on last character
                DEX
                LDA DirectoryBuffer, X
                ORA #0x80           // Set high bit
                STA DirectoryBuffer, X
                break; 
            }
        }
        
        PLY
    }
    
    // Update directory entry with start sector
    // Input: CurrentFileEntry = directory entry index, FileStartSector = sector number
    // Munts: A, Y
    updateDirectoryStartSector()
    {
        // Calculate directory entry offset: CurrentFileEntry * 16 + 2 (start sector field)
        LDA CurrentFileEntry
        ASL ASL ASL ASL          // * 16
        CLC
        ADC #2                   // + 2 for start sector field offset
        TAY                      // Y = start sector field offset
        
        // Write start sector to directory entry
        LDA FileStartSector
        STA DirectoryBuffer, Y
    }
    
    
    
    // Flush current sector and allocate next sector
    // Output: C set if successful, NC if disk full
    // Munts: A, Y, file system state
    flushAndAllocateNext()
    {
        PHX
        PHY
        
        loop // Single exit
        {
            // Write current FileDataBuffer to CurrentFileSector using existing writeSector()
            LDA CurrentFileSector
            writeSector();
            
            // Allocate next sector
            allocateFirstFreeSector(); // -> Y
            if (NC) 
            { 
                // Disk full
                Error.EEPROMFull(); BIT ZP.EmulatorPCL
                break; 
            }
            STY NextFileSector
            
            // Link in FAT chain: FATBuffer[CurrentFileSector] = NextFileSector
            LDY CurrentFileSector
            LDA NextFileSector
            STA FATBuffer, Y
            
            // Move to new sector
            LDA NextFileSector
            STA CurrentFileSector
            STZ SectorPositionL       // Reset to start of new sector
            STZ SectorPositionH
            
            // Clear new file data buffer using existing function
            clearFileDataBuffer();
            
            SEC // Success
            break;
        }
        
        PLY  
        PLX
    }
    
    // Allocate first free sector from FAT
    // Output: C set if sector allocated, NC if disk full
    //         Y = next sector number to allocated
    // Munts: A, Y
    allocateFirstFreeSector()
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
            LDA DirectoryBuffer + 0, X  // Length LSB
            ORA DirectoryBuffer + 1, X  // Length MSB
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
        TAY                      // Y = filename start in DirectoryBuffer
        
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
                LDA DirectoryBuffer, X
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
            LDA DirectoryBuffer, X
            AND #0x7F                // Clear high bit for comparison
            
            // Compare characters
            CMP [ZP.STR], Y
            if (NZ)
            {
                CLC                  // No match
                break;
            }
            
            // Check if this was last character in directory name
            LDA DirectoryBuffer, X
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
    
    
    
    // Initialize file state for save operation
    // Munts: A
    initializeSaveState()
    {
        // Clear file position counters
        STZ FilePosition         // FilePositionL
        STZ FilePosition + 1     // FilePositionH
        STZ SectorPositionL      // Byte position within current sector
        STZ SectorPositionH
        
        // Clear next sector (will be allocated when needed)
        STZ NextFileSector
    }
    
    // TODO : inline
    // Clear file data buffer to all zeros
    clearFileDataBuffer()
    {
        LDA #(FileDataBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // TODO : inline
    // Clear FAT buffer to all zeros
    clearFATBuffer()
    {
        LDA #(FATBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // TODO : inline
    // Clear directory buffer to all zeros  
    clearDirectoryBuffer()
    {
        LDA #(DirectoryBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // Load FAT from EEPROM sector 0 into FATBuffer
    // Input: None
    // Output: FATBuffer loaded with FAT data
    // Munts: A, EEPROM operation registers
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
    
    // Load directory from EEPROM sector 1 into DirectoryBuffer
    // Input: None
    // Output: DirectoryBuffer loaded with directory data
    // Munts: A, EEPROM operation registers
    loadDirectory()
    {
        LDA #1                           // EEPROM address MSB = sector 1  (must be page aligned)
        STA ZP.IDYH              
        
        LDA #(DirectoryBuffer / 256)     // RAM address MSB = DirectoryBuffer (must be page aligned)
        STA ZP.IDXH

        EEPROM.ReadPage();
    }
    
    // Write DirectoryBuffer to EEPROM sector 1
    writeDirectory()
    {
        LDA #1                           // EEPROM address MSB = sector 1  (must be page aligned)
        STA ZP.IDYH             
        
        LDA #(DirectoryBuffer / 256)     // RAM address MSB = DirectoryBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
    }
    
    // Read arbitrary sector into FileDataBuffer
    // Input: A = sector number (0-255)
    // Output: 256 bytes copied from EEPROM to FileDataBuffer
    // Munts: A, EEPROM operation registers
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
/*        
Debug.NL(); LDA #'W' COut(); Space(); XOut(); YOut(); Space();
LDA FileDataBuffer + 0 HOut();
LDA FileDataBuffer + 1 HOut();
LDA FileDataBuffer + 2 HOut();
LDA FileDataBuffer + 3 HOut();
LDA FileDataBuffer + 4 HOut();
LDA FileDataBuffer + 5 HOut();
LDA FileDataBuffer + 6 HOut();
LDA FileDataBuffer + 7 HOut();
*/      
        EEPROM.WritePage();
    }

#ifdef DEBUG
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
            // Print header
            PHA
            LDA #(dumpHeader % 256)
            STA ZP.STRL
            LDA #(dumpHeader / 256)
            STA ZP.STRH
            Print.String();
            PLA
            
            // Load current FAT and directory
            CMP #1
            if (Z)
            {
                LDA #(dumpHeaderLoaded % 256)
                STA ZP.STRL
                LDA #(dumpHeaderLoaded / 256)
                STA ZP.STRH
                Print.String();
                
                loadFAT();
            }
            else
            {
                LDA #(dumpHeaderRAM % 256)
                STA ZP.STRL
                LDA #(dumpHeaderRAM / 256)
                STA ZP.STRH
                Print.String();
            }
            
            loadDirectory();
            
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
    const string dumpHeaderLoaded  = " (RELOADED FROM EEPROM)";
    const string dumpHeaderRAM  = " (FROM RAM)";
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
    
    const string debugHeader         = "--- DEBUG INFO ---";
    const string dirUtilLabel        = "Dir entries used: ";
    const string fatAllocLabel       = "FAT allocation:";
    const string sector0Label        = "  Sector 00: FAT";
    const string sector1Label        = "  Sector 01: Directory";
    const string sectorLabel         = "  Sector ";
    const string sectorsLabel        = " sectors)";
    const string freeSectorsLabel    = "Free sectors: ";
    
    // Dump directory entries (assumes directory loaded in DirectoryBuffer)
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
            LDA DirectoryBuffer + 0, Y
            ORA DirectoryBuffer + 1, Y
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
                LDA DirectoryBuffer + 2, Y  // Start sector
                Print.Hex();
                
                Print.NewLine();
                INX                  // Next entry number
            }
            
            // Move to next directory entry
            TYA
            CLC
            ADC #16              // Next entry (16 bytes per entry)
            TAY
            
            CPY #240
            if (C) { break; }   // if Y >= 240, we've checked all entries
        }
        
        Print.NewLine();
    }
    
    // Dump FAT allocation map
    dumpFATMap()
    {
        LDA #(fatHeader % 256)
        STA ZP.STRL
        LDA #(fatHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        LDY #0                   // FAT entry index
        LDX #0                   // Line counter
        
        loop
        {
            // Print line header every 16 entries
            TYA
            AND #0x0F
            if (Z)
            {
                // Print two-digit hex line number
                TYA                     // Current index
                LSR A LSR A LSR A LSR A // Divide by 16 for line number
                Print.Hex();
                LDA #':'
                Print.Char();
                LDA #' '
                Print.Char();
            }
            
            // Print FAT entry status
            LDA FATBuffer, Y
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
            
            INY
            
            // Check if line is complete (16 entries)
            TYA
            AND #0x0F
            if (Z)
            {
                Print.NewLine();
            }
            
            // Check if all sectors processed
            CPY #0               // Y wrapped around to 0
            if (Z) { break; }
        }
        
        Print.NewLine();
    }
    
    // Calculate and dump sector statistics
    dumpSectorStats()
    {
        LDA #(statsHeader % 256)
        STA ZP.STRL
        LDA #(statsHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        // Count free, used, and end-of-chain sectors
        STZ TransferLengthL      // Free count
        STZ TransferLengthH      // Used count (sectors pointing to others)
        STZ TransferLength + 1   // End-of-chain count
        
        LDY #2                   // Start from sector 2 (skip system sectors)
        
        loop
        {
            LDA FATBuffer, Y
            if (Z)
            {
                INC TransferLengthL  // Free sector
            }
            else
            {
                CMP #1
                if (Z)
                {
                    INC TransferLength + 1  // End-of-chain sector
                }
                else
                {
                    INC TransferLengthH     // Used sector (part of chain)
                }
            }
            
            INY
            if (Z) { break; }    // Y wrapped to 0 - done
        }
        
        // Print free sectors
        LDA #(freeLabel % 256)
        STA ZP.STRL
        LDA #(freeLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA TransferLengthL
        STA ZP.TOPL
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
        LDA TransferLengthH
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
        LDA TransferLength + 1
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        Print.NewLine();
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
        
        // SectorPosition (16-bit)
        LDA #(sectorPositionLabel % 256)
        STA ZP.STRL
        LDA #(sectorPositionLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA SectorPositionL
        STA ZP.TOPL
        LDA SectorPositionH  
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPT
        Print.Hex();
        Print.NewLine();
        
        SEC                        // Always successful
        
        PLY
        PLX
    }
    
    // Print hex dump of first 32 bytes of file
    // Input: X = directory entry byte offset
    // Munts: A, Y
    printFileHexDump()
    {
        PHX
        PHY
        
        // Read file's first sector
        LDA DirectoryBuffer + 2, X  // Start sector at offset +2
        readSector();               // Load sector into FileDataBuffer
        
        // Print first 16 bytes
        STZ SectorPositionL     // Set to 0 for first line
        printHexDumpLine();     // Offset 0x0000
        
        // Print second 16 bytes  
        LDA #16
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();     // Offset 0x0010
        
        PLY
        PLX
    }
    
    // Print one line of hex dump (16 bytes)
    // Input: SectorPositionL = starting byte offset (0 or 16)
    // Munts: A, X, Y
    printHexDumpLine()
    {
        // Print 4-space indentation
        LDX #4
        Print.Spaces();
        
        // Print address (00: or 10:)
        LDA SectorPositionL
        Print.Hex();             // Prints 00 or 10
        LDA #':'
        Print.Char();
        LDA #' '
        Print.Char();
        
        // Print 16 hex bytes
        LDX SectorPositionL      // Starting offset
        LDY #0                   // Counter
        
        loop
        {
            LDA FileDataBuffer, X
            Print.Hex();
            LDA #' '
            Print.Char();
            
            INX
            INY
            CPY #16
            if (Z) { break; }
        }
        
        // Double space before ASCII
        LDA #' '
        Print.Char();
        Print.Char();
        
        // Print ASCII representation
        LDX SectorPositionL      // Starting offset
        LDY #0                   // Counter
        
        loop
        {
            LDA FileDataBuffer, X
            Debug.Printable();   
            
            INX
            INY
            CPY #16
            if (Z) { break; }
        }
        
        Print.NewLine();
    }
    // Print detailed diagnostic information
    // Munts: A, X, Y
    printDebugDiagnostics()
    {
        // Print debug header
        LDA #(debugHeader % 256)
        STA ZP.STRL
        LDA #(debugHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        // Directory utilization
        printDirectoryUtilization();
        
        // FAT allocation summary
        printFATAllocationSummary();
        
        // Per-file sector allocation
        printPerFileSectorAllocation();
        
        // Free space summary
        printFreeSpaceSummary();
    }
    
    // Print directory slot utilization
    printDirectoryUtilization()
    {
        LDA #(dirUtilLabel % 256)
        STA ZP.STRL
        LDA #(dirUtilLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // File count already in TransferLengthL
        LDA TransferLengthL
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        
        LDA #'/'
        Print.Char();
        
        LDA #16                  // Max directory entries
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        
        Print.NewLine();
    }
    
    // Print FAT allocation summary
    printFATAllocationSummary()
    {
        LDA #(fatAllocLabel % 256)
        STA ZP.STRL
        LDA #(fatAllocLabel / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        // Load FAT if not already loaded
        loadFAT();
        
        // Print system sectors
        LDA #(sector0Label % 256)
        STA ZP.STRL
        LDA #(sector0Label / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        LDA #(sector1Label % 256)
        STA ZP.STRL
        LDA #(sector1Label / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
    }
    
    // Print sector allocation info for all files
    // Input: None (uses DirectoryBuffer and FATBuffer)
    // Output: Per-file sector allocation printed to serial
    // Munts: A, X, Y
    // Note: Iterates through directory entries, calls printFileSectorInfo() for each used entry
    printPerFileSectorAllocation()
    {
        LDY #0                   // Directory entry offset
        
        loop
        {
            // Convert entry index to byte offset
            TYA
            ASL A ASL A ASL A ASL A  // Y * 16 = directory entry offset
            TAX                      // X = byte offset in directory
            
            // Check if entry is in use
            LDA DirectoryBuffer + 0, X  // Length LSB
            ORA DirectoryBuffer + 1, X  // Length MSB
            if (NZ)
            {
                // Print sector info for this file
                printFileSectorInfo(); // Input: X = directory entry offset
            }
            
            INY
            CPY #16
            if (Z) { break; }
        }
    }
    
    // Print sector allocation info for one file
    // Input: X = directory entry offset (0, 16, 32...)
    // Output: One line printed: "  Sector XX: FILENAME.EXT (size bytes, N sectors)"
    // Preserves: None
    // Munts: A, X, Y, ZP.TOP registers
    // Note: Converts entry index to byte offset internally for DirectoryBuffer access
    //       Calls printFilenameFromDirectory() and printFileSizeFromDirectory()
    printFileSectorInfo()
    {
        PHX
        PHY
        
        // Print "  Sector "
        LDA #(sectorLabel % 256)
        STA ZP.STRL
        LDA #(sectorLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Print start sector number
        LDA DirectoryBuffer + 2, X  // Start sector
        Print.Hex();
        
        LDA #':'
        Print.Char();
        LDA #' '
        Print.Char();
        
        // Print filename
        //TYA
        //ASL A ASL A ASL A ASL A
        TXA TAY // X -> A
        printFilenameFromDirectory(); // Expects Y = entry offset (0, 16, 32...)
        
        LDA #' '
        Print.Char();
        LDA #'('
        Print.Char();
        
        // Print file size
        printFileSizeFromDirectory(); // Expects Y = entry offset (0, 16, 32...)
        
        LDA #(bytesLabel % 256)
        STA ZP.STRL
        LDA #(bytesLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Calculate and print sector count
        LDA #','
        Print.Char();
        LDA #' '
        Print.Char();
        
        // Calculate sectors used: (filesize + 255) / 256
        LDA DirectoryBuffer + 0, X  // Size low
        CLC
        ADC #255                    // Add 255 for ceiling division
        LDA DirectoryBuffer + 1, X  // Size high  
        ADC #0                      // Add carry
        STA ZP.TOPL                 // Result is sectors used
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        
        LDA #(sectorsLabel % 256)
        STA ZP.STRL
        LDA #(sectorsLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
        
        PLY
        PLX
    }
    
    // Print free space summary
    printFreeSpaceSummary()
    {
        // Count free sectors
        LDY #2                   // Start from sector 2 (skip system)
        STZ SectorPositionL      // Free sector count
        
        loop
        {
            LDA FATBuffer, Y
            if (Z)
            {
                INC SectorPositionL  // Count free sectors
            }
            
            INY
            if (Z) { break; }    // Y wrapped to 0
        }
        
        LDA #(freeSectorsLabel % 256)
        STA ZP.STRL
        LDA #(freeSectorsLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA SectorPositionL
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        
        LDA #'/'
        Print.Char();
        
        LDA #254                 // Total usable sectors (256 - 2 system)
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        STA ZP.TOPT
        Print.Decimal();
        
        Print.NewLine();
    }
    
#endif
}
