unit File
{
    // ==============================================================================
    // HOPPER BASIC FILE SYSTEM SPECIFICATION
    // ==============================================================================
    //
    // The Hopper Basic file system is a simple FAT-based file system designed for
    // 64KB EEPROM storage (256 sectors × 256 bytes per sector).
    //
    // OVERALL STRUCTURE:
    // ------------------
    // Sector 0:    FAT (File Allocation Table)
    // Sector 1:    Root Directory (first sector, can chain to more)
    // Sectors 2-255: Data sectors (files and additional directory sectors)
    //
    // ==============================================================================
    // FAT (FILE ALLOCATION TABLE) - Sector 0
    // ==============================================================================
    // The FAT occupies sector 0 and contains 256 bytes, one per sector.
    // Each byte indicates the sector's status and linking:
    //
    // FAT[n] values:
    //   0x00:     Free sector (available for allocation)
    //   0x01:     End-of-chain marker (last sector in file or directory)
    //   0x02-0xFF: Next sector number in chain
    //
    // Reserved sectors:
    //   FAT[0] = 0x01  (FAT sector itself - reserved)
    //   FAT[1] = 0x01  (Root directory - initially end-of-chain)
    //
    // ==============================================================================
    // DIRECTORY STRUCTURE
    // ==============================================================================
    // Directories start at sector 1 and can chain to additional sectors as needed.
    // Each directory sector contains 16 entries of 16 bytes each (256 bytes total).
    //
    // Directory chaining:
    // - FAT[directory_sector] = next_directory_sector (or 0x01 for end)
    // - New directory sectors allocated as needed when all 16 slots are full
    // - Directory sectors marked with 'D' in diagnostic output
    //
    // ==============================================================================
    // DIRECTORY ENTRY FORMAT (16 bytes)
    // ==============================================================================
    // Offset  Size  Description
    // ------  ----  -----------
    // 0x00    2     File length in bytes (16-bit, little-endian)
    //               Bit 15 (MSB bit 7): File type flag
    //                 0 = Data file
    //                 1 = Executable file
    //               Bits 14-0: Actual file length (0-32767 bytes)
    //               0x0000 = empty/deleted entry
    // 0x02    1     Start sector of file (first sector in chain)
    // 0x03    13    Filename (up to 13 ASCII characters)
    //               - starts with a letter (A-Z) like an IDENTIFIER
    //               - Alphanumeric characters only (A-Z, 0-9, uppercase)
    //               - Last character has high bit (0x80) set as terminator
    //               - Unused bytes after terminator are undefined
    //
    // Example data file "HELLO.BAS" (68 bytes, starting at sector 5):
    //   00-01: 44 00        (0x0044 = 68 bytes, bit 15 clear = data file)
    //   02:    05           (start sector 5)
    //   03-0F: 48 45 4C 4C 4F 2E 42 41 D3 xx xx xx xx
    //          H  E  L  L  O  .  B  A  S(0x80 set)
    //
    // Example executable "GAME.PRG" (4096 bytes, starting at sector 8):
    //   00-01: 00 90        (0x9000 = bit 15 set + 0x1000 = executable, 4096 bytes)
    //   02:    08           (start sector 8)
    //   03-0F: 47 41 4D 45 2E 50 52 C7 xx xx xx xx xx
    //          G  A  M  E  .  P  R  G(0x80 set)
    //
    // ==============================================================================
    // FILE TYPE DETERMINATION
    // ==============================================================================
    // The high bit of the 16-bit length field indicates file type:
    // - Bit 15 = 0: Data file (text, BASIC source, etc.)
    // - Bit 15 = 1: Executable file (compiled program)
    //
    // To extract actual length: length_value & 0x7FFF
    // To check if executable: (length_high_byte & 0x80) != 0
    //
    // This limits file sizes to 32,767 bytes (sufficient for this system where
    // executables will never exceed 32KB and data files are typically small).
    //
    // ==============================================================================
    // FILE SECTOR CHAINS
    // ==============================================================================
    // Files are stored as chains of 256-byte sectors linked through the FAT:
    //
    // 1. Directory entry specifies the file's start sector
    // 2. FAT[start_sector] points to next sector (or 0x01 if single sector)
    // 3. Chain continues: FAT[sector_n] -> sector_n+1
    // 4. Last sector marked with FAT[last_sector] = 0x01
    //
    // Example file chain for 600-byte file:
    //   Directory: start_sector = 5
    //   FAT[5] = 8   (sector 5 -> sector 8)
    //   FAT[8] = 12  (sector 8 -> sector 12)
    //   FAT[12] = 1  (sector 12 is last, partial data)
    //
    // ==============================================================================
    // FILE OPERATIONS
    // ==============================================================================
    // Save operation:
    // 1. Validate filename (1-13 chars, alphanumeric)
    // 2. Find free directory entry (or overwrite existing)
    // 3. Allocate first data sector
    // 4. Write data in 256-byte chunks, allocating sectors as needed
    // 5. Update directory entry with final length
    // 6. Write FAT and directory back to EEPROM
    //
    // Load operation:
    // 1. Find file in directory by name
    // 2. Read start sector and file length (mask bit 15 to get actual length)
    // 3. Follow FAT chain to read all sectors
    // 4. Return data via NextStream() calls
    //
    // Delete operation:
    // 1. Find file in directory
    // 2. Free all sectors in FAT chain (mark as 0x00)
    // 3. Clear directory entry (set length to 0x0000)
    // 4. Compact directory by moving last entry to deleted slot
    // 5. Unlink empty directory sectors if last entry was moved
    //
    // ==============================================================================
    // LIMITS AND CONSTRAINTS
    // ==============================================================================
    // - Maximum 254 data sectors (256 - 2 system sectors)
    // - Maximum file size: 32,767 bytes (15-bit length field, bit 15 is type flag)
    // - Maximum file size: 65,535 bytes (16-bit length field)
    // - Maximum filename: 13 characters
    // - Directory can grow to multiple sectors (16 entries each)
    // - No subdirectories (flat file system)
    // - No file attributes or timestamps
    //
    // ==============================================================================
    
    
    // File System Zero Page Variables (aliases to existing slots)
    const byte SectorSource         = ZP.FS0;                  // for use with LDX [SectorSource], Y for example
    const byte SectorSourceL        = ZP.FS0;                  // Source address for sector ops
    const byte SectorSourceH        = ZP.FS1;     
    
    const byte TransferLength       = ZP.FS2;                  // for use with LDX [TransferLength], Y for example
    const byte TransferLengthL      = ZP.FS2;                  // Bytes to transfer (LSB)
    const byte TransferLengthH      = ZP.FS3;                  // Bytes to transfer (MSB)
    
    
    // Buffer allocation (3 x 256 bytes)
    const uint fatBuffer            = Address.FileSystemBuffers;        // [0-255]
    const uint directoryBuffer      = Address.FileSystemBuffers + 256;  // [256-511]  
    const uint FileDataBuffer       = Address.FileSystemBuffers + 512;  // [512-767]
    
    const byte currentFileSector    = ZP.FS4;                  // Current sector number in file
    const byte fileStartSector      = ZP.FS5;                  // First sector of current file
    const byte currentFileEntry     = ZP.FS6;                  // Directory entry index (0-15)
    
    // filePosition is used in: StartSave(initializeSaveState), AppendStream, EndSave, DumpFileState
    const byte filePosition         = ZP.FS7;                  // Current byte position in file (16-bit), for use with LDA [filePosition]
    const byte filePositionL        = ZP.FS7;                  //    "
    const byte filePositionH        = ZP.FS8;                  //    "
    
    
    const byte nextFileSector       = ZP.FS9;                  // Next sector in chain (from FAT)
    
    // Additional ZP aliases needed for AppendStream
    // WARNINGL ZP.M0 - ZP.M3 are used by Time.Delay() (TARGET0-3)
    const byte bytesRemaining       = ZP.FS10;                // for use with LDA [bytesRemaining]
    const byte bytesRemainingL      = ZP.FS10;                // 16-bit: bytes left to copy
    const byte bytesRemainingH      = ZP.FS11;
    
    const byte sectorPosition       = ZP.FS12;                // for use with LDA [sectorPosition]
    const byte sectorPositionL      = ZP.FS12;                // Byte position within current sector (0-255) .. with possible overflow to 256 (NO, IT IS NOT THE SAME AS ZERO IF YOU ARE IDIOTS LIKE US)
    const byte sectorPositionH      = ZP.FS13;
     
    const byte currentDirectorySector = ZP.FS14;              // Current directory sector being accessed
    
    // used by Delete (compaction)
    const byte lastOccupiedEntry      = ZP.FS15;              
    const byte lastOccupiedSector     = ZP.FS7;               // alias of filePositionL (see above)
    const byte previousSector         = ZP.FS8;               // alias of filePositionH (see above)
    
#if defined(CFILES)
    const byte cfilesFILE             = ZP.FS16;               // single FILE record for CFILES APIS
    // Bit 0 - open state 
    // Bit 1 - read "r" (0) or write "w" (1) 1 byte
    // Bit 2 - eof indicator
#endif    

    
    // Validate filename format (alphanumeric + period, 1-13 chars)
    // Input: ZP.STR = pointer to null-terminated filename
    // Output: C set if valid, NC if invalid  
    // Preserves: X, Y
    // Munts: A
    ValidateFilename()
    {
        PHY
        PHX
        
        loop // single exit block
        {
            LDY #0
            LDX #0
            
            // Check if filename is empty
            LDA [ZP.STR], Y
            if (Z)
            {
                Error.FilenameExpected(); // Empty filename invalid
                break;
            }
            
            // Check each character and count length
            loop
            {
                LDA [ZP.STR], Y
                if (Z) { SEC break; }  // End of string
                
                // Check if character is valid
                Char.IsAlphaNumeric();
                if (NC)
                {
                    CMP #'.'
                    if (NZ)
                    {
                        Error.IllegalFilename(); // Invalid character found
                        break;
                    }
                    else
                    {
                        INX // count the .
                    }
                }
                
                INY
                CPY #14  // Max 13 characters + null terminator
                if (Z)
                {
                    Error.FilenameTooLong(); // Filename too long
                    break;
                }
            }
            if (NC) { break; }
            
            switch (X)
            {
                case 0:
                {
                    SEC // no dots is valid
                }
                case 1:
                {
                    // one dot
                    DEY // Y -> last character
                    LDA [ZP.STR], Y
                    CMP #'.'
                    if (Z)
                    {
                        Error.IllegalFilename(); // last character is dot
                        break;
                    }
                    LDA [ZP.STR] // first character
                    CMP #'.'
                    if (Z)
                    {
                        Error.IllegalFilename(); // first character is dot
                        break;
                    }
                    SEC
                }
                default:
                {
                    Error.IllegalFilename(); // more than one dot
                    break;
                }
            }
            
            // Filename is valid (1-13 chars, valid characters)
            break;
        } // single exit
        
        PLX
        PLY
    }
    
    initializeFATandDirectory()
    {
        // Reload FAT and directory from EEPROM
        loadFAT();
        LDA #1    // Default to sector 1
        loadDirectorySector();
    }
    
    // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
    // Input:  currentFileEntry
    // Output: Y = offset in current sector
    fileEntryToDirectoryEntry()
    {
        PHA
        LDA currentFileEntry
        AND #0x0F                    // Convert global to local (0-15)
        ASL A ASL A ASL A ASL A      // * 16
        TAY                          // Y = directory entry offset
        PLA
    }
    
    // Calculate directory entry byte offset: Y * 16
    // Input:  Y - directory entry in sector
    // Output: X - offset in sector
    entryToOffset()
    {
        TYA
        ASL A ASL A ASL A ASL A // Y * 16
        TAX                     // X = byte offset in directoryBuffer
    }
    
    // Get available free space in bytes
    // Output: TOP1:TOP0 = free bytes (16-bit)
    //         C set if successful, NC if error accessing FAT
    // Preserves: X, Y  
    // Munts: A
    GetAvailable()
    {
        PHX
        PHY
        
        loop // Single exit for cleanup
        {
            initializeFATandDirectory();
            Shared.ZeroTop();

            // Count free sectors
            LDY #2                   // Start from sector 2 (skip FAT and directory)
            STZ ZP.TOP1      // Free sector count
            
            loop
            {
                LDA fatBuffer, Y
                if (Z)
                {
                    INC ZP.TOP1  // Count free sectors
                }
                INY
                if (Z) { break; }    // Y wrapped to 0 - all sectors checked
            }

            // Convert sectors to bytes: free_sectors * 256
            // Since each sector is 256 bytes, free_sectors becomes the high byte
            SEC                      // Success
            break;
        }

        PLY
        PLX
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
            STA fatBuffer + 0   // Sector 0 (FAT)
            STA fatBuffer + 1   // Sector 1 (Directory)
            
            // Write FAT to EEPROM
            writeFAT();
            
            // Clear and write empty directory
            clearDirectoryBuffer();
            LDA #1                           // Default to sector 1
            writeDirectorySector();
            
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
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            initializeFATandDirectory();
            
            // Find free directory entry (or existing file to overwrite)
            findFreeDirectoryEntry();
            if (NC)
            {
                Error.DirectoryFull(); BIT ZP.EmulatorPCL
                break;
            }
            // currentFileEntry now contains entry index (0-15)
            
            // Write filename to directory entry
            writeFilenameToDirectory();
            
            // Allocate first sector for file data
            allocateFirstFreeSector(); // -> Y
            if (NC)
            {
                Error.EEPROMFull(); BIT ZP.EmulatorPCL
                break;
            }
            STY fileStartSector
            STY currentFileSector
            
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
        
        loop // Single exit
        {
            // Copy input parameters to working variables
            LDA TransferLengthL
            STA bytesRemainingL
            LDA TransferLengthH  
            STA bytesRemainingH
            
            loop // Single exit for byte copy
            {
                // Check if done
                LDA bytesRemainingL
                ORA bytesRemainingH
                if (Z)
                { 
                    SEC break; // Set C - success
                }
                
                // Copy one byte from source to file data buffer
                LDY #0
                LDA [SectorSource], Y
                LDY sectorPositionL
                STA FileDataBuffer, Y
                
                // Update source pointer
                INC SectorSourceL
                if (Z) { INC SectorSourceH }
                
                // Update sector position (16-bit increment)
                INC sectorPositionL
                if (Z) { INC sectorPositionH }
                
                // Check if sector full (256 bytes = 0x0100)
                LDA sectorPositionH
                if (NZ) // High byte non-zero means >= 256
                {
                    flushAndAllocateNext();
                    if (NC) { Error.EEPROMFull(); BIT ZP.EmulatorPCL break; }
                }
                
                // Decrement 16-bit remaining count  
                LDA bytesRemainingL
                if (Z)
                {
                    DEC bytesRemainingH
                }
                DEC bytesRemainingL
                
                // Update filePosition (16-bit)
                INC filePositionL
                if (Z) { INC filePositionH }
            }
            
            break;
        }
        PLY
        PLX
    }
    
    // Input: A = 0x80 for executable file, A = 0x00 for data file
    // Close and finalize current save file
    // Output: C set if successful, NC if error
    // Preserves: X, Y  
    // Munts: A, file system state
    EndSave()
    {
        PHX
        PHY
        
        STA ZP.ACCH
        
        loop // Single exit
        {
            // Write final sector if it has data
            LDA sectorPositionL
            ORA sectorPositionH
            if (NZ)
            {
                LDA currentFileSector
                writeSector();
            }
            
            // Update directory entry with final file length
            // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
            fileEntryToDirectoryEntry(); // -> Y
            
            // Set file length (filePosition)
            LDA filePositionL
            STA directoryBuffer + 0, Y
            LDA filePositionH  
            ORA ZP.ACCH                    // or 0x80 if executable type
            STA directoryBuffer + 1, Y
            
            // Flush metadata to EEPROM
            writeFAT();
            
            LDA currentDirectorySector
            writeDirectorySector();
            
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
    Dir()
    {
        PHX
        PHY
        
        loop // Single exit
        {
            initializeFATandDirectory();
            
            // Count files and calculate total bytes
            countFilesAndBytes(); // -> TransferLengthL = file count, TransferLengthH/bytesRemainingL = total bytes
            
            LDA TransferLengthL
            if (NZ)
            {
                // Print header
                LDA # ErrorID.Files LDX # MessageExtras.SuffixColon Error.MessageNL();
                
                // Print each file entry
                printAllFileEntries();
            }
            // Print summary
            printDirectorySummary();
            
#ifdef FILEDEBUG
            Print.NewLine();
            printDebugDiagnostics();
            
            Print.NewLine();
            DumpDriveState();
#endif
            
            SEC
            break;
        }
        
        PLY
        PLX
    }
    
    findLastOccupiedEntry()
    {
        STZ lastOccupiedEntry
        STZ lastOccupiedSector
        STZ previousSector       // Still needed for unlinking
        
        LDA #DirWalkAction.FindLast
        STA ZP.ACCH
        walkDirectoryChain();    // Walker handles ACCL increment
        
        // Set C if we found an entry to move
        LDA lastOccupiedSector
        if (NZ) { SEC } else { CLC }
    }
    
    // Move directory entry from last occupied to deleted slot
    // Input: LastOccupiedEntry/Sector = source, currentFileEntry/currentDirectorySector = dest
    // Uses: FileDataBuffer for source read, directoryBuffer for dest write
    // Munts: A, X, Y
    moveDirectoryEntry()
    {
        // Load destination sector into directoryBuffer
        LDA currentDirectorySector
        loadDirectorySector();
        LDA #(directoryBuffer % 256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(directoryBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        
        // Check if same sector
        LDA currentDirectorySector
        CMP lastOccupiedSector
        if (Z)
        {
            // Same sector - both in directoryBuffer
            LDA ZP.FDESTINATIONADDRESSL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.FDESTINATIONADDRESSH
            STA ZP.FSOURCEADDRESSH
        }
        else
        {    
            // Load source sector into FileDataBuffer
            LDA lastOccupiedSector
            readSector();
            
            // #FileDataBuffer --> FSOURCEADDRESSL
            LDA #(FileDataBuffer % 256)
            STA ZP.FSOURCEADDRESSL
            LDA #(FileDataBuffer / 256)
            STA ZP.FSOURCEADDRESSH
        }
        
        // Get source offset
        LDA lastOccupiedEntry
        AND #0x0F
        TAY
        entryToOffset();             // Y * 16 -> X
        
        CLC
        TXA
        ADC ZP.FSOURCEADDRESSL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.FSOURCEADDRESSH
        ADC #0
        STA ZP.FSOURCEADDRESSH
        
        // Get dest offset
        fileEntryToDirectoryEntry(); // (currentFileEntry & 0x0F) * 16 -> Y
        
        CLC
        TYA
        ADC ZP.FDESTINATIONADDRESSL
        STA ZP.FDESTINATIONADDRESSL
        LDA ZP.FDESTINATIONADDRESSH
        ADC #0
        STA ZP.FDESTINATIONADDRESSH
        
        // Copy 16 bytes, and clear source entry
        LDY #15
        loop
        {
            LDA [ZP.FSOURCEADDRESS], Y
            STA [ZP.FDESTINATIONADDRESS], Y
            LDA #0
            STA [ZP.FSOURCEADDRESS], Y
            DEY
            if (MI) { break; } // Break when Y goes negative
        } // loop
        
        // Write both sectors back
        LDA currentDirectorySector
        CMP lastOccupiedSector
        if (Z)
        {
            // Same sector - only write the one we modified
            writeDirectorySector();
        }
        else
        {
            // Different sectors - write both
            writeDirectorySector();
            
            LDA lastOccupiedSector
            writeSector();
        }
    }
    
    // Delete file from EEPROM file system
    // Input: ZP.STR = pointer to filename (null-terminated)
    // Output: C set if successful, NC if error
    // Preserves: X, Y
    // Munts: A, file system buffers
    Delete()
    {
        PHX
        PHY
        
        // preserve the file name in case this is part of SAVE
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        loop // Single exit for cleanup
        {
            // Validate filename format
            File.ValidateFilename();
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            initializeFATandDirectory();
            
            // Find the file in directory
            LDA # DirWalkAction.FindFile // all files
            STA ZP.ACCH
            findFileInDirectory();
            if (NC)
            {
                Error.FileNotFound(); BIT ZP.EmulatorPCL
                break;
            }
            // currentFileEntry now contains the directory entry index
            
            // Get start sector from directory entry
            getFileStartSector(); // -> fileStartSector
            
            // Free all sectors used by the file
            freeFileSectorChain();
            
            // Clear the directory entry
            clearDirectoryEntry();
            
            // Write updated directory and FAT back to EEPROM
            LDA currentDirectorySector 
            writeDirectorySector();
            writeFAT();
            
            // Save deleted entry location for compaction
            LDA currentFileEntry
            PHA
            LDA currentDirectorySector  
            PHA
            
            findLastOccupiedEntry();
            
            // Restore deleted entry location
            PLA
            STA currentDirectorySector
            PLA
            STA currentFileEntry
            
            if (C)
            {
                // Only move if the last entry is AFTER the deleted entry
                LDA lastOccupiedEntry
                CMP currentFileEntry
                if (NC) { break; }  // LastOccupiedEntry <= currentFileEntry, no move
                if (Z)  { break; }  // LastOccupiedEntry == currentFileEntry, no move
                
                // LastOccupiedEntry > currentFileEntry
                // Move last entry to fill the deleted slot
                moveDirectoryEntry();
                
                // Check if last sector is now empty
                LDA lastOccupiedSector
                CMP #1                   // Never unlink sector 1
                if (NZ)
                {
                    LDA lastOccupiedEntry
                    AND #0x0F            // Was it slot 0?
                    if (Z)               // Yes - sector is now empty
                    {
                        // Unlink the empty sector
                        LDY previousSector
                        LDA #1               // End-of-chain marker
                        STA fatBuffer, Y
                        
                        // Free the empty sector
                        LDY lastOccupiedSector
                        LDA #0
                        STA fatBuffer, Y
                        
                        writeFAT();
                    }
                }
            }
            
            
            SEC // Success
            break;
        }
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        
        PLY
        PLX
    }
    

    // Check if file exists in directory
    // Input: ZP.STR = pointer to filename (uppercase, null-terminated), 
    //             A =  DirWalkAction.FindFile (all files) or DirWalkAction.FindExecutable (executables only)
    // Output: C set if file exists, NC if file not found
    //         currentFileEntry = directory entry index if found
    // Preserves: X, Y
    // Munts: A, file system buffers  
    // Note: Throws error if filename format is invalid
    Exists()
    {
        PHX
        PHY
        
        STA ZP.ACCH
        
        // preserve the file name
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA

        loop // Single exit for cleanup
        {
            // Validate filename format
            File.ValidateFilename();
            if (NC) 
            {
                BIT ZP.EmulatorPCL break;
            }
            initializeFATandDirectory();
            
            // Find the file in directory
            findFileInDirectory();
            // Returns C if found (with currentFileEntry set), NC if not found
            break;
        }
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        PLY
        PLX
    }
    
    // Open file for reading
    // Input: ZP.STR = pointer to filename (uppercase, null-terminated)
    //        A = DirWalkAction.FindExecutable or DirWalkAction.FindFile
    // Output: C set if successful, NC if error (file not found)
    //         File ready for reading via NextStream()
    // Preserves: X, Y
    // Munts: A, file system state
    StartLoad()
    {
        STA ZP.ACCH
        PHX
        PHY
        
        // preserve the file name in case you are just being used to verify the file exists
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        loop // Single exit for cleanup
        {
            // Check if file exists (validates filename and loads metadata)
            LDA ZP.ACCH
            File.Exists();
            if (NC)
            {
                Error.FileNotFound(); BIT ZP.EmulatorPCL
                break;
            }
            // currentFileEntry now contains the directory entry index
            
            // Get start sector from directory entry
            getFileStartSector(); // -> fileStartSector
            
            // Get file length from directory entry
            GetFileLength(); // -> bytesRemainingL/H
            
            // Initialize for first NextStream() call
            LDA fileStartSector
            STA currentFileSector
            
            // Success - file ready for NextStream() calls
            SEC
            break;
        }
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        PLY
        PLX
    }
    
    
    // Read next chunk of data from current load file
    // Prerequisites: StartLoad() must be called first to initialize file state
    //                Caller must consume all returned data before calling again
    // Output: C set if data available, NC if end of file or error
    //         Data is always in FileDataBuffer starting at offset 0
    //         TransferLength = number of valid bytes in FileDataBuffer (if C set)
    //         TransferLengthL/H = 16-bit byte count (max 256 per call)
    //         States.Success flag set appropriately
    // Preserves: X, Y
    // Munts: A, bytesRemaining, currentFileSector, nextFileSector
    // Note: - Returns up to 256 bytes (one sector) per call
    //       - Pre-reads next sector after returning data (if more exists)
    //       - FileDataBuffer will be overwritten on next call
    //       - No partial read support - caller must consume entire TransferLength
    NextStream()
    {
        PHX
        PHY
        
        loop // Single exit
        {
            // Check if any bytes remaining
            LDA bytesRemainingL
            ORA bytesRemainingH
            if (Z)
            {
                CLC  // End of file
                break;
            }
            
            // Load current sector
            LDA currentFileSector
            readSector();  // Load into FileDataBuffer
            
            // Calculate transfer: min(256, bytesRemaining)
            LDA bytesRemainingH
            if (Z)  // bytesRemaining < 256
            {
                LDA bytesRemainingL
                STA TransferLengthL
                STZ TransferLengthH
                // Don't invalidate buffer - partial read
            }
            else  // bytesRemaining >= 256
            {
                STZ TransferLengthL
                LDA #1
                STA TransferLengthH
                
                // Get next sector from FAT for next call
                LDY currentFileSector
                LDA fatBuffer, Y
                CMP #1                      // End-of-chain marker?
                if (Z)
                {
                    // Shouldn't happen if bytesRemaining is correct
                    Error.EEPROMError();
                    break;
                }
                STA currentFileSector       // Ready for next call
            }
            
            // Update bytesRemaining
            SEC
            LDA bytesRemainingL
            SBC TransferLengthL
            STA bytesRemainingL
            LDA bytesRemainingH
            SBC TransferLengthH
            STA bytesRemainingH
            
            SEC  // Success
            break;
        }
        
        PLY
        PLX
    }
          

    
    // Get file length from current directory entry
    // Input: currentFileEntry = directory entry index
    // Output: bytesRemainingL/H = file length (16-bit)
    // Munts: A, Y
    GetFileLength()
    {
        // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
        fileEntryToDirectoryEntry(); // -> Y
        
        // Read file length from directory entry (bytes 0-1)
        LDA directoryBuffer + 0, Y              // Length LSB
        STA bytesRemainingL
        LDA directoryBuffer + 1, Y              // Length MSB
        AND # 0x7F // strip bit 15 - file type
        STA bytesRemainingH
    }
        
    
    // Find file in directory by filename
    // Input: ZP.STR = filename to find
    // Output: C set if found, NC if not found
    //         currentFileEntry = directory entry index if found
    findFileInDirectory()
    {
        walkDirectoryChain();
        // NC if early exit (found), currentFileEntry already set by callback
        if (C) 
        { 
            CLC  // Not found
        }
        else
        {
            SEC  // Found
        }
    }
    
    
    // Get start sector from current directory entry
    // Input: currentFileEntry = directory entry index
    // Output: fileStartSector = start sector number
    // Munts: A, Y
    getFileStartSector()
    {
        // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
        fileEntryToDirectoryEntry(); // -> Y
        INY INY                      // + 2 (start sector field)
        
        // Read start sector from directory entry
        LDA directoryBuffer, Y
        STA fileStartSector
    }
    
    // Free all sectors in file's FAT chain
    // Input: fileStartSector = first sector to free
    // Output: All sectors in chain marked as free (0) in FAT
    // Munts: A, currentFileSector, nextFileSector
    freeFileSectorChain()
    {
        LDA fileStartSector
        STA currentFileSector
        
        loop
        {
            // Check if we've reached end of chain
            LDA currentFileSector
            if (Z) { break; }        // Sanity check - should not happen
            
            // Get next sector in chain from FAT
            LDY currentFileSector
            LDA fatBuffer, Y
            STA nextFileSector
            
            // Mark current sector as free
            LDA #0
            STA fatBuffer, Y
            
            // Check if this was end of chain
            LDA nextFileSector
            CMP #1                   // 1 = end-of-chain marker
            if (Z) { break; }        // End of chain reached
            
            // Move to next sector
            LDA nextFileSector
            STA currentFileSector
        }
    }
    
    // Clear directory entry for deleted file
    // Input: currentFileEntry = directory entry index
    // Output: Directory entry cleared (all zeros)
    // Munts: A, X, Y
    clearDirectoryEntry()
    {
        // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
        fileEntryToDirectoryEntry(); // -> Y
        
        // Clear 16 bytes of directory entry
        LDA #0
        LDX #16
        loop
        {
            STA directoryBuffer, Y
            INY
            DEX
            if (Z) { break; }
        }
    }
    
    
    // Directory walker actions
    enum DirWalkAction
    {
        Count = 0,           // Count files and total bytes
        Print = 1,           // Print directory entries
        FindFile = 2,        // Find specific file by name
        FindExecutable = 3,  // Find specific executable file by name
        FindLast = 4,        // Find last occupied entry for compaction
    }
    
    // Generic directory chain walker
    // Iterates through all directory sectors and entries, calling action-specific callbacks
    // Input:  ZP.ACCH = DirWalkAction to perform
    //         ZP.STR = filename to find (for FindFile action)
    // Output: C = completed full iteration (not found/done)
    //         NC = early exit (found target)
    //         Action-specific side effects via callbacks:
    //           Count: TransferLengthL = file count, TransferLengthH:bytesRemainingL = total bytes
    //           Print: Entries printed to console
    //           FindFile: currentFileEntry = global entry index if found
    //           FindLast: LastOccupiedEntry/Sector = last occupied position, previousSector set
    // Uses:   currentDirectorySector, directoryBuffer, ZP.ACCL (sector position counter)
    // Munts:  A, X, Y, currentDirectorySector, previousSector, ZP.ACCL
    walkDirectoryChain()
    {
        LDA #1
        STA currentDirectorySector
        STZ ZP.ACCL              // Tracks position in chain (0x00, 0x10, 0x20...)
        STZ previousSector       // Tracks previous sector for unlinking
        
        loop 
        {
            LDA currentDirectorySector
            loadDirectorySector();
            
            // Process all 16 entries in current sector
            LDY #0                       // Slot index (0-15)
            loop
            {
                entryToOffset();         // Y * 16 -> X (byte offset)
                
                // Check if entry occupied (length != 0)
                LDA directoryBuffer + 0, X
                ORA directoryBuffer + 1, X
                if (NZ)
                {
                    // Entry occupied - dispatch to action handler
                    PHY                  // Preserve slot and offset
                    PHX
                    
                    LDA ZP.ACCH
                    switch (A)
                    {
                        case DirWalkAction.Count:
                        {
                            processCountEntry();      // X = offset
                        }
                        case DirWalkAction.Print:
                        {
                            processPrintEntry();      // X = offset
                        }
                        case DirWalkAction.FindExecutable:
                        case DirWalkAction.FindFile:
                        {
                            processFindFileEntry();   // X = offset, Y = slot
                        }
                        case DirWalkAction.FindLast:
                        {
                            processFindLastEntry();   // X = offset, Y = slot
                        }
                    }
                    
                    PLX
                    PLY
                    
                    // Callback returns: C = continue scanning, NC = stop (found)
                    if (NC) { break; }
                }
                
                INY
                CPY #16
                if (Z) { break; }
            } // inner loop
            
            if (NC) { break; }  // Propagate early exit from callback
            
            // Move to next directory sector in chain
            LDA currentDirectorySector
            getNextDirectorySector();    // Returns next sector in A (1 = end-of-chain)
            CMP #1
            if (Z) 
            {
                SEC                       // Set C = completed full iteration
                break;
            }
            
            // Continue to next sector
            LDX currentDirectorySector
            STX previousSector           // Save for potential unlinking
            STA currentDirectorySector   // A still has next sector
            
            // Increment sector position counter (used by FindLast to calculate global indices)
            CLC
            LDA ZP.ACCL
            ADC #16                      // Each sector adds 16 to global entry index
            STA ZP.ACCL
        } // outer loop
    }
    
    // Callback methods
    processFindLastEntry()  // X = offset, Y = slot
    {
        // Update tracking for last occupied entry
        TYA
        ORA ZP.ACCL          // Combine with sector position
        STA lastOccupiedEntry
        
        LDA currentDirectorySector
        STA lastOccupiedSector
        
        SEC                  // Always continue scanning
    }
    
    processFindFileEntry()  // X = offset, Y = slot
    {
        PHY                 // Save slot on stack
        
        // if ZP.ACCL == DirWalkAction.FindExecutable, only find executable files
        
        checkFilenameAndTypeMatch();  // Compare ZP.STR with directoryBuffer, X
        
        PLA                 // Get slot back in A
        
        if (C)  // Match found
        {
            // Calculate global entry index
            PHA                      // Save slot again
            getCurrentSectorFirstEntry();
            PLA                      // Get slot
            CLC
            ADC currentFileEntry
            STA currentFileEntry
            
            CLC                 // Signal stop scanning
            return;
        }
        
        SEC                     // Continue scanning
    }  
    
    processCountEntry()  // X = directory offset
    {
        INC TransferLengthL          // Count file
        
        // Add file size to total
        CLC
        LDA directoryBuffer + 0, X
        ADC bytesRemainingL
        STA bytesRemainingL
        LDA directoryBuffer + 1, X
        AND # 0x7F // strip bit 15 - file type (AND does not affect C flag)
        ADC TransferLengthH
        STA TransferLengthH
        
        SEC  // Continue scanning
    }
    
    processPrintEntry()  // X = offset
    {
        // Print entry at directoryBuffer + X
        
        LDY #4
        Print.Spaces();
        
        TXA // X = directory entry byte offset
        TAY
        
        printFilenameFromDirectory(); // Uses Y = filename start offset, preserves X, Y
        
        // Print file size
        // Y already has directory entry offset 
        printFileSizeFromDirectory(); // Uses Y = directory entry offset, preserves X, Y
        
        // " BYTES"
        LDA # ErrorID.BytesLabel LDX # MessageExtras.PrefixSpace Error.MessageNL();
        
        INC ZP.ACCL         // Increment printed count  
        SEC                 // Continue scanning
    }
    
    // Count files and calculate total bytes used
    // Output: TransferLengthL = file count
    //         TransferLengthH/bytesRemainingL = total bytes (16-bit)
    countFilesAndBytes()
    {
        STZ TransferLengthL      // Initialize file count
        STZ TransferLengthH      // Initialize byte count MSB
        STZ bytesRemainingL      // Initialize byte count LSB
        
        LDA #DirWalkAction.Count
        STA ZP.ACCH
        walkDirectoryChain();
    }
    
    // Print all file entries (traverses all directory sectors)
    // Munts: A, X, Y
    printAllFileEntries()
    {
        STZ ZP.ACCL              // Entry counter
        
        LDA #DirWalkAction.Print
        STA ZP.ACCH
        
        walkDirectoryChain();
    }
    
    
    // Print filename from current directory entry 
    // Input: Y = directory entry byte offset (0, 16, 32, 48, ...)
    // Output: Filename printed to serial
    // Preserves: Y
    // Munts: A
    printFilenameFromDirectory()
    {
        PHX
        PHY
        TYA
        CLC
        ADC #3                   // Offset to filename field
        TAY
        
        PHY // store name position
        
        LDX #(13 + 2 + 1) // filename + optional " *" + at least one space
        loop
        {
            LDA directoryBuffer, Y
            
            PHA                      // Save character
            AND #0x7F                // Clear high bit
            Print.Char();            // Print character
            DEX                      // count the character (for space padding below)
            PLA                      // Restore character
            
            if (MI) { break; }       // High bit set = last character
            INY
            
        } // single exit
        
        PLY // restore name position
        
        // Check if executable and print appropriate suffix
        LDA (directoryBuffer - 2), Y  // Size high byte
        if (MI)  // Bit 7 set = executable
        {
            Space();
            LDA #'*'
            Print.Char();
            DEX DEX
        }
        
        loop
        {
            Print.Space();
            DEX
            if (Z) { break; }
        }
        
        PLY
        PLX
    }
    
    // Print file size from current directory entry
    // Input: Y = directory entry byte offset (0, 16, 32, 48, ...)  
    // Output: File size printed to serial as decimal
    // Munts: A, ZP.TOPL, ZP.TOPH, ZP.TOPT
    printFileSizeFromDirectory()
    {
        LDA directoryBuffer + 0, Y  // Length LSB
        STA ZP.TOP0
        LDA directoryBuffer + 1, Y  // Length MSB
        AND #0x7F                   // strip file type bit from size
        STA ZP.TOP1
        
        // right aligment of numbers in the 10..9999 range
        PHY
        LDY #0                       // assume 0 spaces of padding
        LDA ZP.TOP1
        CMP #4                       // Check for 1024+
        if (NC)                      // < 1024, need more analysis
        {
            CMP #3                   // Check if TOPH = 3 (768-1023)
            if (Z)                   // TOPH = 3
            {
                LDA ZP.TOP0
                CMP #232             // 1000 = 3*256 + 232
                if (NC)              // < 1000 (768-999 range)
                {        INY }       //     3-digit numbers get 1 space
                // else >= 1000 (1000-1023), keep X=0 for 4-digit alignment
            }
            else                     // TOPH = 0, 1, or 2
            {
                CMP #0
                if (NZ)              // TOPH = 1 or 2 (256-767)
                {        INY }       //     3-digit numbers get 1 space
                else                 // TOPH = 0 (0-255)
                {
                    LDA ZP.TOP0
                    CMP #100
                    if (NC)          // < 100 (10-99 range)
                    { LDY #2 }       //     2-digit numbers get 2 spaces
                    else             // >= 100 (100-255 range)
                    {    INY }       //     3-digit numbers get 1 space
                }
            }
        }
        // else >= 1024, keep Y=0 (no padding for 4+ digits)
        Print.Spaces();              // print X spaces (zero is ok)
        PLY
        
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
    }
    
    // Print directory summary: "3 files, 2373 bytes used"
    // Input: TransferLengthL = file count, TransferLengthH/bytesRemainingL = total bytes
    // Munts: A
    printDirectorySummary()
    {
        // Print file count
        LDA TransferLengthL
        Shared.LoadTopByte();
        Long.Print();
        
        // " FILES, "
        LDA # ErrorID.Files LDX # (MessageExtras.PrefixSpace|MessageExtras.SuffixComma|MessageExtras.SuffixSpace) Error.Message();
        
        // Print total bytes
        LDA bytesRemainingL
        STA ZP.TOP0
        LDA TransferLengthH
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
        
        // " BYTES USED"
        LDA # ErrorID.BytesUsedLabel LDX # MessageExtras.PrefixSpace Error.MessageNL();
    }
    
    // Write filename to directory entry
    // Input: currentFileEntry = directory entry index, ZP.STR = filename
    // Munts: A, X, Y
    writeFilenameToDirectory()
    {
        PHY
        
        // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
        fileEntryToDirectoryEntry(); // -> Y
        TYA TAX
        INX INX INX // + 3 (filename field)
        
        // Copy filename from ZP.STR to directoryBuffer
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            Char.ToUpper();
            if (Z) 
            { 
                // End of filename - set high bit on last character
                DEX
                LDA directoryBuffer, X
                ORA #0x80           // Set high bit
                STA directoryBuffer, X
                break; 
            }
            
            STA directoryBuffer, X
            INX
            INY
            CPY #13                 // Max filename length
            if (Z) 
            { 
                // Max length reached - set high bit on last character
                DEX
                LDA directoryBuffer, X
                ORA #0x80           // Set high bit
                STA directoryBuffer, X
                break; 
            }
        }
        
        PLY
    }
    
    // Update directory entry with start sector
    // Input: currentFileEntry = directory entry index, fileStartSector = sector number
    // Munts: A, Y
    updateDirectoryStartSector()
    {
        // Calculate directory entry offset: (currentFileEntry & 0x0F) * 16
        fileEntryToDirectoryEntry(); // -> Y
        INY INY                      // + 2 for start sector field offset
        
        // Write start sector to directory entry
        LDA fileStartSector
        STA directoryBuffer, Y
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
            // Write current FileDataBuffer to currentFileSector using existing writeSector()
            LDA currentFileSector
            writeSector();
            
            // Allocate next sector
            allocateFirstFreeSector(); // -> Y
            if (NC) 
            { 
                // Disk full
                Error.EEPROMFull(); BIT ZP.EmulatorPCL
                break; 
            }
            STY nextFileSector
            
            // Link in FAT chain: fatBuffer[currentFileSector] = nextFileSector
            LDY currentFileSector
            LDA nextFileSector
            STA fatBuffer, Y
            
            // Move to new sector
            LDA nextFileSector
            STA currentFileSector
            STZ sectorPositionL       // Reset to start of new sector
            STZ sectorPositionH
            
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
            LDA fatBuffer, Y
            if (Z)                // Free sector found
            {
                // Mark sector as end-of-chain (initial single sector file)
                LDA #1
                STA fatBuffer, Y
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
    
    // Check if current directory entry filename matches ZP.STR
    // Input: X = directory entry byte offset (0, 16, 32...), ZP.STR = filename to match
    //        ZP.ACCH = DirWalkAction (FindFile or FindExecutable)
    // Output: C set if match, NC if no match
    checkFilenameAndTypeMatch()
    {
        loop
        {
            // Check if we need to match executable type
            LDA ZP.ACCH
            CMP #DirWalkAction.FindExecutable
            if (Z)  // Looking for executable files only
            {
                // Check if this file is executable (bit 7 of size high byte)
                LDA (directoryBuffer + 1), X  // Size high byte at offset 1
                if (PL)  // Bit 7 not set (not executable)
                {
                    CLC  // Not a match - wrong file type
                    break;
                }
            }
            
            // Point to filename field in directory entry (offset +3)
            TXA
            CLC
            ADC #3
            TAX                      // X = filename start in directoryBuffer
    
            LDY #0                   // Index into ZP.STR filename
            loop
            {
                // Get character from input filename
                LDA [ZP.STR], Y
                if (Z)               // End of input filename?
                {
                    // Input ended - check if PREVIOUS directory character was the last
                    LDA (directoryBuffer-1), X
                    if (MI)          // High bit set = that was the last char
                    {
                        SEC          // Perfect match
                    }
                    else
                    {
                        CLC          // Input ended but directory continues
                    }
                    break;
                }
                
    
                
                // Compare with directory character (clear high bit)
                LDA directoryBuffer, X
                PHA                  // Save original with high bit
                AND #0x7F           // Clear high bit for comparison
                CMP [ZP.STR], Y
                if (NZ)
                {
                    PLA             // clean stack
                    CLC             // Characters don't match
                    break;
                }
                PLA             // Get original back
                
                // Check if this was last character in directory name
                if (MI)             // High bit set = last character
                {
                    // Directory name ended, check if input also ends
                    INY
                    LDA [ZP.STR], Y
                    if (Z)
                    {
                        SEC         // Perfect match
                    }
                    else
                    {
                        CLC         // Directory ended but input continues
                    }
                    break;
                }
                
                // Advance both pointers
                INY
                INX
            }
            break;
        } // single exit
    }   
    
    
    // Initialize file state for save operation
    // Munts: A
    initializeSaveState()
    {
        // Clear file position counters
        STZ filePosition         // filePositionL
        STZ filePosition + 1     // filePositionH
        STZ sectorPositionL      // Byte position within current sector
        STZ sectorPositionH
        
        // Clear next sector (will be allocated when needed)
        STZ nextFileSector
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
        LDA #(fatBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // TODO : inline
    // Clear directory buffer to all zeros  
    clearDirectoryBuffer()
    {
        LDA #(directoryBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
    }
    
    // Load FAT from EEPROM sector 0 into fatBuffer
    // Input: None
    // Output: fatBuffer loaded with FAT data
    // Munts: A, EEPROM operation registers
    loadFAT()
    {
        STZ ZP.IDYH              // EEPROM address MSB = sector 0 (must be page aligned)
        
        LDA #(fatBuffer / 256)   // RAM address MSB = fatBuffer (must be page aligned)
        STA ZP.IDXH
        EEPROM.ReadPage();
      
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
    }
    
    // Write fatBuffer to EEPROM sector 0
    writeFAT()
    {
        STZ ZP.IDYH              // EEPROM address MSB = sector 0 (must be page aligned)
        
        LDA #(fatBuffer / 256)   // RAM address MSB = fatBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
    }
    
    // Input: A = directory sector number to load
    // Output: directoryBuffer contains specified directory sector
    // Munts: A, EEPROM operation registers
    loadDirectorySector()
    {
        STA ZP.IDYH                      // EEPROM address MSB = sector number
        STA currentDirectorySector       // Remember which sector we loaded
        
        LDA #(directoryBuffer / 256)    // RAM address MSB = directoryBuffer
        STA ZP.IDXH
        
        EEPROM.ReadPage();
    }
    
    // New parameterized writeDirectory (takes sector in A)
    // Input: A = directory sector number to write
    // Output: directoryBuffer written to specified sector
    // Munts: A, EEPROM operation registers  
    writeDirectorySector()
    {
        STA ZP.IDYH                      // EEPROM address MSB = sector number
        
        LDA #(directoryBuffer / 256)    // RAM address MSB = directoryBuffer
        STA ZP.IDXH
        
        EEPROM.WritePage();
    }
    
    // Get next directory sector from FAT chain
    // Input: A = current directory sector
    // Output: A = next directory sector (0 if none, 1 if end-of-chain)
    // Munts: A, Y
    getNextDirectorySector()
    {
        TAY                              // Y = current sector
        LDA fatBuffer, Y                 // Get next sector from FAT
    }
    
    // Allocate new directory sector and link it to chain
    // Input: A = current last directory sector
    // Output: C set if successful, NC if disk full
    //         A = new directory sector number if successful
    // Munts: A, Y
    allocateDirectorySector()
    {
        PHA                              // Save current last sector
        
        loop // Single exit
        {
            // Find and allocate a free sector
            allocateFirstFreeSector();   // Returns Y = new sector
            if (NC) 
            { 
                PLA                      // Clean stack
                CLC                      // Disk full
                break;
            }
            
            // Link previous directory sector to new one
            PLA                          // A = previous last sector
            TAX                          // X = previous sector
            TYA                          // A = new sector
            STA fatBuffer, X             // Link previous to new
            
            // Mark new sector as end-of-chain
            TAY
            LDA #1                       // End-of-chain marker
            STA fatBuffer, Y
            
            // Clear the new directory sector
            TYA                          // A = new sector number
            PHA                          // Save for return value
            
            // Load empty data into FileDataBuffer and write to new sector
            clearFileDataBuffer();
            PLA
            PHA
            writeSector();               // Write cleared buffer to new sector
            
            PLA                          // Return new sector number in A
            SEC                          // Success
            break;
        }
    } 
    
    // currentFileEntry = (currentDirectorySector - 1) x 16
    getCurrentSectorFirstEntry()
    {
        LDA currentDirectorySector
        DEC                     // Sector 1 = entries 0-15
        ASL A ASL A ASL A ASL A // * 16
        STA currentFileEntry
    }
    
    // Find free directory entry across all directory sectors
    // Output: C set if entry found, NC if directory full
    //         currentFileEntry = global entry index (0-255) if found
    //         currentDirectorySector = sector containing the entry
    // Munts: A, X, Y
    findFreeDirectoryEntry()
    {
        
        LDA #1                           // Start with first directory sector
        STA currentDirectorySector
        STZ currentFileEntry             // Start with entry 0
        
        loop // Single exit - main search loop
        {
            // Load current directory sector
            LDA currentDirectorySector 
            loadDirectorySector(); 
            
            // Search entries in this sector
            LDY #0                       // Entry index within sector (0-15)
            
            loop // Search entries in current sector
            {
                entryToOffset(); // Y * 16 -> X, munts A
                
                // Check if entry is free (fileLength == 0)
                LDA directoryBuffer + 0, X  // Length LSB
                ORA directoryBuffer + 1, X  // Length MSB
                if (Z)
                {
                    // Found free entry!
                    // currentFileEntry = (currentDirectorySector - 1) * 16 + Y
                    getCurrentSectorFirstEntry();
                    TYA                     // Add local entry index
                    CLC
                    ADC currentFileEntry
                    STA currentFileEntry
                    SEC                    // Success - found entry
                    break;                 // Exit main loop with success
                }
                
                INY
                CPY #16
                if (Z) { CLC break; }        // Checked all entries in this sector
            }
            
            // Check if we found an entry (C will be set if we did)
            if (C) { break; }            // Exit with found entry
            
            // No free entry in this sector, check for next
            LDA currentDirectorySector
            getNextDirectorySector();
            CMP #1                       // End-of-chain?
            if (Z)
            {
                // Need to allocate new directory sector
                LDA currentDirectorySector
                allocateDirectorySector();
                if (NC)
                {
                    // Disk full - can't allocate new directory sector
                    CLC
                    break;               // Exit with failure
                }
                
                // New sector allocated in A
                STA currentDirectorySector
                loadDirectorySector();
                
                // First entry in new sector is free
                getCurrentSectorFirstEntry();
                
                // Need to write the FAT with the new link
                writeFAT();
                
                SEC                      // Success
                break;                   // Exit with new entry
            }
            
            // Move to next existing directory sector
            STA currentDirectorySector
            // Continue to next iteration of main loop
        }
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
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
    }
    
    // Write FileDataBuffer to arbitrary sector
    // Input: A = sector number
    writeSector()
    {
        STA ZP.IDYH                 // EEPROM address MSB = sector number (must be page aligned)
        
        LDA #(FileDataBuffer / 256) // RAM address MSB = FileDataBuffer (must be page aligned)
        STA ZP.IDXH
        EEPROM.WritePage();
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
    }

#ifdef FILEDEBUG
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
            
            LDA #1                           // Default to sector 1
            loadDirectorySector();
            
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
    const string currentEntryLabel   = "currentFileEntry: ";  
    const string startSectorLabel    = "fileStartSector: ";
    const string currentSectorLabel  = "currentFileSector: ";
    const string nextSectorLabel     = "nextFileSector: ";
    const string filePositionLabel   = "filePosition: ";
    const string sectorPositionLabel = "sectorPosition: ";
    
    const string debugHeader         = "--- DEBUG INFO ---";
    const string dirUtilLabel        = "Dir entries used: ";
    const string fatAllocLabel       = "FAT allocation:";
    const string sector0Label        = "  Sector 00: FAT";
    const string sector1Label        = "  Sector 01: Directory";
    const string sectorLabel         = "  Sector ";
    const string sectorsLabel        = " sectors)";
    const string freeSectorsLabel    = "Free sectors: ";
    const string dirSectorsLabel = "Directory sectors: ";
    
    // Dump directory entries (traverses all directory sectors)
    dumpDirectoryEntries()
    {
        LDA #(dirHeader % 256)
        STA ZP.STRL
        LDA #(dirHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        LDX #0                       // Global entry counter (0-255)
        LDA #1                       // Start with first directory sector
        
        loop // Traverse directory chain
        {
            PHA                      // Save current sector
            loadDirectorySector();   // Load it
            PLA
            PHA                      // Keep sector on stack
            
            LDY #0                   // Entry offset within sector
            
            loop // Process entries in current sector
            {
                // Check if entry is in use (length != 0)
                LDA directoryBuffer + 0, Y
                ORA directoryBuffer + 1, Y
                if (NZ)
                {
                    // Print entry number as #global [local]
                    TXA
                    PHA                  // Save X
                    TYA
                    PHA                  // Save Y
                    
                    // Print #global
                    LDA #'#'
                    Print.Char();
                    TXA                  // Global entry number to A
                    Shared.LoadTopByte();
                    Print.Decimal();    // Print global index
                    
                    Print.Space();
                    
                    // Print [local]
                    LDA #'['
                    Print.Char();
                    TYA                  // Local entry within sector
                    Print.Hex();        // Just hex for 0-F
                    LDA #']'
                    Print.Char();
                    
                    PLA                  // Restore Y
                    TAY
                    
                    // Print filename (Y = entry offset)
                    printFilenameFromDirectory();
                    
                    // Print file info
                    Print.Space();
                    printFileSizeFromDirectory();  // Also uses Y
                    Print.Space();
                    LDA #'@'
                    Print.Char();
                    LDA directoryBuffer + 2, Y  // Start sector
                    Print.Hex();
                    
                    Print.NewLine();
                    
                    PLA                  // Restore X
                    TAX
                }
                
                INX                      // Next global entry number
                
                // Move to next directory entry
                TYA
                CLC
                ADC #16                  // Next entry
                TAY
                if (Z) { break; }        // Y wrapped - no more entries in this sector
            }// loop
            
            // Get next directory sector
            PLA                          // Current sector
            getNextDirectorySector();
            
            CMP #1                       // End-of-chain?
            if (Z) { break; }            // No more sectors to process
        }// loop
    }   
    
    // Dump FAT allocation map
    // Output: Visual FAT map printed to serial
    // Munts: A, X, Y
    dumpFATMap()
    {
        PHX
        PHY
        
        // Print header
        LDA #(fatHeader % 256)
        STA ZP.STRL
        LDA #(fatHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        LDY #0                           // Sector counter
        
        loop // Print all sectors
        {
            // Print row header every 16 sectors
            TYA
            AND #0x0F
            if (Z)
            {
                TYA
                Print.Hex();
                LDA #':'
                Print.Char();
                Print.Space();
            }
            
            // Check if this is a directory sector by walking the directory chain
            STY ZP.M0                    // Save Y (current sector being printed)
            LDA #1                       // Start with first directory sector
            
            loop // Walk directory chain
            {
                CMP ZP.M0                // Is this our sector?
                if (Z)
                {
                    // This is a directory sector
                    LDA #'D'
                    Print.Char();
                    break;
                }
                
                // Get next in chain
                getNextDirectorySector();
                
                CMP #1                   // End-of-chain?
                if (Z)
                {
                    // Not a directory sector - check FAT value
                    LDY ZP.M0            // Restore sector number
                    LDA fatBuffer, Y
                    if (Z)
                    {
                        LDA #'.'         // Free sector
                        Print.Char();
                    }
                    else
                    {
                        CMP #1
                        if (Z)
                        {
                            LDA #'E'     // End-of-chain
                        }
                        else
                        {
                            LDA #'*'     // Used (linked)
                        }
                        Print.Char();
                    }
                    break;
                }
                // Continue walking directory chain
            }
            
            LDY ZP.M0                    // Restore Y (sector counter)
            
            // End of row?
            INY
            TYA
            AND #0x0F
            if (Z)
            {
                Print.NewLine();
            }
            
            TYA
            if (Z) { break; }            // Wrapped to 0 - done all 256 sectors
        }
        
        PLY
        PLX
    }
    
    
    
    // Dump sector usage statistics
    // Output: Sector counts printed to serial
    // Munts: A, X, Y
    dumpSectorStats()
    {
        PHX
        PHY
        
        // Print header
        LDA #(statsHeader % 256)
        STA ZP.STRL
        LDA #(statsHeader / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        // UWIDE0-3 are only used by the INT MATH (currently unused in this project)
        STZ ZP.UWIDE0 // directory sectors
        STZ ZP.UWIDE1 // free sectors
        STZ ZP.UWIDE2 // used sectors
        STZ ZP.UWIDE3 // end of chain sectors
        
        LDY #0
        loop // Count all sectors
        {
            LDA fatBuffer, Y
            if (Z)
            {
                INC ZP.UWIDE1 // free sector
            }
            else
            {
                INC ZP.UWIDE2                // not free
                CMP #1
                if (Z)
                {
                    INC ZP.UWIDE3            // End-of-chain sector
                }
            }
            INY
            if (Z) { break; }            // Wrapped to 0 - done all 256
        }
        
        // Now, walk directory sectors
        LDA #1                           // Start with first directory sector
        loop // Count directory sectors
        {
            INC ZP.UWIDE0
            getNextDirectorySector();    // Get next in chain
            CMP #1                       // End-of-chain?
            if (Z) { break; }            // Yes, stop counting
            // A already has next sector for next iteration
        }       
                 
        // Print statistics
        
        // Directory sectors
        LDA #(dirSectorsLabel % 256)
        STA ZP.STRL
        LDA #(dirSectorsLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA ZP.UWIDE0
        Shared.LoadTopByte();
        Print.Decimal();
        Print.NewLine();
        
        // Free sectors
        LDA #(freeLabel % 256)
        STA ZP.STRL
        LDA #(freeLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA ZP.UWIDE1
        Shared.LoadTopByte();
        Print.Decimal();
        Print.NewLine();
        
        // Used sectors (linked)
        LDA #(usedLabel % 256)
        STA ZP.STRL
        LDA #(usedLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA ZP.UWIDE2
        Shared.LoadTopByte();
        Print.Decimal();
        Print.NewLine();
        
        // End-of-chain sectors
        LDA #(endLabel % 256)
        STA ZP.STRL
        LDA #(endLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA ZP.UWIDE3
        Shared.LoadTopByte();
        Print.Decimal();
        Print.NewLine();
        
        PLY
        PLX
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
        
        // currentFileEntry
        LDA #(currentEntryLabel % 256)
        STA ZP.STRL
        LDA #(currentEntryLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA currentFileEntry
        Print.Hex();
        Print.NewLine();
        
        // fileStartSector  
        LDA #(startSectorLabel % 256)
        STA ZP.STRL
        LDA #(startSectorLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA fileStartSector
        Print.Hex();
        Print.NewLine();
        
        // currentFileSector
        LDA #(currentSectorLabel % 256)
        STA ZP.STRL
        LDA #(currentSectorLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA currentFileSector
        Print.Hex();
        Print.NewLine();
        
        // nextFileSector
        LDA #(nextSectorLabel % 256)
        STA ZP.STRL
        LDA #(nextSectorLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA nextFileSector
        Print.Hex();
        Print.NewLine();
        
        // filePosition (16-bit)
        LDA #(filePositionLabel % 256)
        STA ZP.STRL
        LDA #(filePositionLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA filePosition         // filePositionL
        STA ZP.TOPL
        LDA filePosition + 1     // filePositionH  
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPT
        Print.Hex();
        Print.NewLine();
        
        // sectorPosition (16-bit)
        LDA #(sectorPositionLabel % 256)
        STA ZP.STRL
        LDA #(sectorPositionLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA sectorPositionL
        STA ZP.TOPL
        LDA sectorPositionH  
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
        LDA directoryBuffer + 2, X  // Start sector at offset +2
        readSector();               // Load sector into FileDataBuffer
        
        STZ sectorPositionL     // Set to 0 for first line
        printHexDumpLine();
        
        LDA # 0x10
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x20
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x30
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x40
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x50
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x60
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x70
        STA sectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        
        
        PLY
        PLX
    }
    
    // Print one line of hex dump (16 bytes)
    // Input: sectorPositionL = starting byte offset (0 or 16)
    // Munts: A, X, Y
    printHexDumpLine()
    {
        // Print 4-space indentation
        LDY #8
        Print.Spaces();
        
        // Print address (00: or 10:)
        LDA sectorPositionL
        Print.Hex();             // Prints 00 or 10
        LDA #':'
        Print.Char();
        Print.Space();
        
        // Print 16 hex bytes
        LDX sectorPositionL      // Starting offset
        LDY #0                   // Counter
        
        loop
        {
            LDA FileDataBuffer, X
            Print.Hex();
            Print.Space();
            
            INX
            INY
            CPY #16
            if (Z) { break; }
        }
        
        // Double space before ASCII
        LDY #2 Print.Spaces();
        
        // Print ASCII representation
        LDX sectorPositionL      // Starting offset
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
        Shared.LoadTopByte();
        Print.Decimal();
        
        LDA #'/'
        Print.Char();
        
        LDA #16                  // Max directory entries
        Shared.LoadTopByte();
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
    // Input: None (uses directoryBuffer and fatBuffer)
    // Output: Per-file sector allocation printed to serial
    // Munts: A, X, Y
    // Note: Iterates through directory entries, calls printFileSectorInfo() for each used entry
    printPerFileSectorAllocation()
    {
        LDY #0                   // Directory entry offset
        
        loop
        {
            entryToOffset(); // Y * 16 -> X, munts A
            
            // Check if entry is in use
            LDA directoryBuffer + 0, X  // Length LSB
            ORA directoryBuffer + 1, X  // Length MSB
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
    // Note: Converts entry index to byte offset internally for directoryBuffer access
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
        LDA directoryBuffer + 2, X  // Start sector
        Print.Hex();
        
        LDA #':'
        Print.Char();
        Print.Space();
        
        // Print filename
        TXA TAY // X -> A
        printFilenameFromDirectory(); // Expects Y = entry offset (0, 16, 32...)
        
        Print.Space();
        LDA #'('
        Print.Char();
        
        // Print file size
        printFileSizeFromDirectory(); // Expects Y = entry offset (0, 16, 32...)
        
        // " BYTES, "
        LDA # ErrorID.BytesLabel LDX # (MessageExtras.PrefixSpace|MessageExtras.SuffixComma|MessageExtras.SuffixSpace) Error.Message();
        
        // Calculate and print sector count
        LDA #','
        Print.Char();
        Print.Space();
        
        // Calculate sectors used: (filesize + 255) / 256
        LDA directoryBuffer + 0, X  // Size low
        CLC
        ADC #255                    // Add 255 for ceiling division
        LDA directoryBuffer + 1, X  // Size high  
        ADC #0                      // Add carry
        // Result is sectors used
        Shared.LoadTopByte();
        Print.Decimal();
        
        LDA #(sectorsLabel % 256)
        STA ZP.STRL
        LDA #(sectorsLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Print FAT chain:
        Print.Space();Print.Space();
        
        // Start walking the FAT chain
        LDA directoryBuffer + 2, X  // Get start sector
        STA currentFileSector       // Use as chain walker
        STZ TransferLength          // Count actual sectors in chain
        
        loop // Walk FAT chain
        {
            // Print current sector in hex
            LDA #'0'
            Print.Char();
            LDA #'x'
            Print.Char();
            LDA currentFileSector
            Print.Hex();
            
            INC TransferLength      // Count this sector
            
            // Get next sector from FAT
            LDY currentFileSector
            LDA fatBuffer, Y
            STA nextFileSector
            
            // Check if end of chain
            CMP #1                  // 1 = end-of-chain marker
            if (Z) { break; }       // End of chain reached
            
            // Print arrow to next sector
            Print.Space();
            LDA #'-'
            Print.Char();
            LDA #'>'
            Print.Char();
            Print.Space();
            
            // Move to next sector
            LDA nextFileSector
            STA currentFileSector
        }
        
        
        Print.NewLine();
        
        PLY
        PLX
    }
    
    // Print free space summary
    printFreeSpaceSummary()
    {
        // Count free sectors
        LDY #2                   // Start from sector 2 (skip system)
        STZ sectorPositionL      // Free sector count
        
        loop
        {
            LDA fatBuffer, Y
            if (Z)
            {
                INC sectorPositionL  // Count free sectors
            }
            
            INY
            if (Z) { break; }    // Y wrapped to 0
        }
        
        LDA #(freeSectorsLabel % 256)
        STA ZP.STRL
        LDA #(freeSectorsLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA sectorPositionL
        Shared.LoadTopByte();
        Print.Decimal();
        
        LDA #'/'
        Print.Char();
        
        LDA #254                 // Total usable sectors (256 - 2 system)
        Shared.LoadTopByte();
        Print.Decimal();
        
        Print.NewLine();
    }
    
#endif

#if defined(CFILES)
    // Open file with C-style mode
    // Input:  ZP.STR = filename, ZP.NEXT = "w" or "r" string pointer
    // Output: ZP.TOP = FILE* (0x01) on success, NULL (0x00) on failure
    FOpen()
    {
        loop // single exit
        {
            // Check if file already open
            if (BBS0, cfilesFILE) { CLC break; }  // Bit 0 set = already open
            
            // Parse mode string
            LDY #0
            LDA [ZP.NEXT], Y
            CMP #'r'
            if (Z)
            {
                // Open for reading
                LDA # DirWalkAction.FindFile
                File.StartLoad();           // preserves X and Y
                if (NC) { break; }          // StartLoad failed
                File.NextStream();  // preserves X and Y
                if (NC) { break; }
                STZ sectorPositionL
                STZ sectorPositionH
                
                LDA #0b00000001             // Set open bit, clear write bit
                STA cfilesFILE
                SEC
                break;
            }
            
            CMP #'w'
            if (Z)
            {
                // Delete it if it exists
                LDA # DirWalkAction.FindFile // all files
                File.Exists();
                if (C)
                {
                    File.Delete();
                    if (NC) { break; }          // Delete failed
                }
                
                // Open for writing
                File.StartSave();           // preserves X and Y
                if (NC) { break; }          // StartSave failed
                
                LDA #0b00000011             // Set open bit and write bit
                STA cfilesFILE
                SEC
                break;
            }
            
            CLC  // Invalid mode
            break;
        } // single exit
        
        if (C)
        {
            LDA #0x01                       // Return FILE* = 1
            Shared.LoadTopByte();
            SEC
        }
        else
        {
            Shared.ZeroTop();               // Return NULL
        }
    }
    
    // Close file
    // Input:  ZP.NEXT = FILE* 
    // Output: ZP.TOP  = 0 (success) or -1 (failure)
    FClose()
    {
        loop // single exit pattern
        {
            // Check if valid FILE*
            LDA ZP.NEXT0 // let's assume it will be 0x00 or 0x01
            if (Z)  { CLC break; }   // NULL pointer
            
            // Check if file is open
            if (BBR0, cfilesFILE) { CLC break; } // Bit 0 clear = not open
            
            // If opened for write, finalize the file
            if (BBS1, cfilesFILE)  // Bit 1 set = write mode
            {
                LDA #0x00            // Data file (not executable)
                File.EndSave();      // preserves X and Y
                if (NC) { break; }          // EndSave failed?
            }
            
            // Clear file state
            STZ cfilesFILE
            
            // Return 0 for success
            Shared.ZeroTop();
            SEC
            break;
        } // single exit
        
        if (NC)
        {
            LDA #0xFF    // Return -1
            STA ZP.TOP0
            STA ZP.TOP1
            STA ZP.TOP2
            STA ZP.TOP3
        }
    }
    
    // Get character from file
    // Input:  ZP.NEXT = FILE*
    // Output: ZP.TOP  = character (0-255) or -1 for EOF/error
    FGetC()
    {
        loop // single exit
        {
            // Check if valid FILE*
            LDA ZP.NEXT0  // assume 0x00 or 0x01
            if (Z) { CLC break; }  // NULL pointer
            
            // Check if file is open for reading
            if (BBR0, cfilesFILE) { CLC break; }  // Not open
            if (BBS1, cfilesFILE) { CLC break; }  // Write mode - can't read
            if (BBS2, cfilesFILE) { CLC break; }  // EOF already reached
            
            // Check if we've consumed TransferLength bytes
            LDA sectorPositionH
            CMP TransferLengthH
            if (Z)
            {
                LDA sectorPositionL
                CMP TransferLengthL
                if (Z)  // Reached end of this buffer
                {
                    File.NextStream();  // preserves X and Y
                    if (NC)  // No more data
                    {
                        SMB2 cfilesFILE  // Set EOF bit
                        CLC
                        break;
                    }
                    STZ sectorPositionL
                    STZ sectorPositionH
                }
            }
            
            // Get character from buffer
            LDY sectorPositionL
            LDA FileDataBuffer, Y
            Shared.LoadTopByte();  // Character in ZP.TOP0, rest zeroed
            
            // Advance position
            INC sectorPositionL
            if (Z){ INC sectorPositionH }
            
            SEC  // Success
            break;
        } // single exit
        
        if (NC)
        {
            LDA #0xFF  // Return -1
            STA ZP.TOP0
            STA ZP.TOP1
            STA ZP.TOP2
            STA ZP.TOP3
        }
    }
#endif

}
