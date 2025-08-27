unit File
{
    // Buffer allocation (3 x 256 bytes)
    const uint FATBuffer            = Address.FileSystemBuffers;        // [0-255]
    const uint DirectoryBuffer      = Address.FileSystemBuffers + 256;  // [256-511]  
    const uint FileDataBuffer       = Address.FileSystemBuffers + 512;  // [512-767]
    
    
    
    // File System Zero Page Variables (aliases to existing slots)
    const byte SectorSource         = ZP.FS0;                  // for use with LDX [SectorSource], Y for example
    const byte SectorSourceL        = ZP.FS0;                  // Source address for sector ops
    const byte SectorSourceH        = ZP.FS1;     
    
    const byte TransferLength       = ZP.FS2;                  // for use with LDX [TransferLength], Y for example
    const byte TransferLengthL      = ZP.FS2;                  // Bytes to transfer (LSB)
    const byte TransferLengthH      = ZP.FS3;                  // Bytes to transfer (MSB)
    
    const byte CurrentFileSector    = ZP.FS4;                  // Current sector number in file
    const byte FileStartSector      = ZP.FS5;                  // First sector of current file
    const byte CurrentFileEntry     = ZP.FS6;                  // Directory entry index (0-15)
    
    const byte FilePosition         = ZP.FS7;                  // Current byte position in file (16-bit), for use with LDA [FilePosition]
    const byte FilePositionL        = ZP.FS7;                  //    "
    const byte FilePositionH        = ZP.FS8;                  //    "
    
    
    const byte NextFileSector       = ZP.FS9;                  // Next sector in chain (from FAT)
    
    // Additional ZP aliases needed for AppendStream
    // WARNINGL ZP.M0 - ZP.M3 are used by Time.Delay() (TARGET0-3)
    const byte BytesRemaining       = ZP.FS10;                // for use with LDA [BytesRemaining]
    const byte BytesRemainingL      = ZP.FS10;                // 16-bit: bytes left to copy
    const byte BytesRemainingH      = ZP.FS11;
    
    const byte SectorPosition       = ZP.FS12;                // for use with LDA [SectorPosition]
    const byte SectorPositionL      = ZP.FS12;                // Byte position within current sector (0-255) .. with possible overflow to 256 (NO, IT IS NOT THE SAME AS ZERO IF YOU ARE IDIOTS LIKE US)
    const byte SectorPositionH      = ZP.FS13;
    
    const byte StreamBytesAvailable = ZP.FS14;                // used only within NextStream()
    
    
#ifdef TRACEFILE
    // Trace string constants
    const string fileExistsTrace = "FileExists";
    const string validateFilenameTrace = "ValidateName";
    const string getAvailableTrace = "GetAvailable";
    const string formatTrace = "Format";
    const string startSaveTrace = "StartSave";
    const string appendStreamTrace = "AppendStream";
    const string endSaveTrace = "EndSave";
    const string dirTrace = "Dir";
    const string deleteFileTrace = "Delete";
    const string startLoadTrace = "StartLoad";
    const string nextStreamTrace = "NextStream";
    const string getFileLengthTrace = "getFileLen";
    const string advanceToNextSectorTrace = "advanceNext";
    const string updateStreamPositionTrace = "updateStream";
    const string findFileInDirectoryTrace = "findFile";
    const string compareFilenamesTrace = "compareNames";
    const string getFileStartSectorTrace = "getStartSec";
    const string freeFileSectorChainTrace = "freeSectors";
    const string clearDirectoryEntryTrace = "clearDirEnt";
    const string countFilesAndBytesTrace = "countFiles";
    const string printAllFileEntriesTrace = "printFiles";
    const string printFileEntryTrace = "printEntry";
    const string printFilenameFromDirectoryTrace = "printName";
    const string printFileSizeFromDirectoryTrace = "printSize";
    const string printDirectorySummaryTrace = "printSummary";
    const string writeFilenameToDirectoryTrace = "writeName";
    const string updateDirectoryStartSectorTrace = "updateDir";
    const string flushAndAllocateNextTrace = "flushAlloc";
    const string allocateFirstFreeSectorTrace = "allocSector";
    const string findFreeDirectoryEntryTrace = "findFreeDir";
    const string checkFilenameMatchTrace = "checkMatch";
    const string initializeSaveStateTrace = "initSave";
    const string clearFileDataBufferTrace = "clearData";
    const string clearFATBufferTrace = "clearFAT";
    const string clearDirectoryBufferTrace = "clearDir";
    const string loadFATTrace = "loadFAT";
    const string writeFATTrace = "writeFAT";
    const string loadDirectoryTrace = "loadDir";
    const string writeDirectoryTrace = "writeDir";
    const string readSectorTrace = "readSector";
    const string writeSectorTrace = "writeSector";
    const string dumpDriveStateTrace = "DumpDrive";
    const string dumpDirectoryEntriesTrace = "dumpDirEnt";
    const string dumpFATMapTrace = "dumpFAT";
    const string dumpSectorStatsTrace = "dumpStats";
    const string dumpFileStateTrace = "DumpFile";
    const string printFileHexDumpTrace = "printHex";
    const string printHexDumpLineTrace = "printHexLine";
    const string printDebugDiagnosticsTrace = "printDebug";
    const string printDirectoryUtilizationTrace = "printDirUtil";
    const string printFATAllocationSummaryTrace = "printFATSum";
    const string printPerFileSectorAllocationTrace = "printPerFile";
    const string printFileSectorInfoTrace = "printSecInfo";
    const string printFreeSpaceSummaryTrace = "printFree";
#endif
    
    // Validate filename format (alphanumeric + period, 1-13 chars)
    // Input: ZP.STR = pointer to null-terminated filename
    // Output: C set if valid, NC if invalid  
    // Preserves: X, Y
    // Munts: A
    ValidateFilename()
    {
#ifdef TRACEFILE
        LDA #(validateFilenameTrace % 256) STA ZP.TraceMessageL LDA #(validateFilenameTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
                Char.IsAlphaNumeric();
                if (NC)
                {
                    CLC  // Invalid character found
                    PLY
#ifdef TRACEFILE
                    LDA #(validateFilenameTrace % 256) STA ZP.TraceMessageL LDA #(validateFilenameTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
                    return;
                }
                
                INY
                CPY #14  // Max 13 characters + null terminator
                if (Z)
                {
                    CLC  // Filename too long
                    PLY
#ifdef TRACEFILE
                    LDA #(validateFilenameTrace % 256) STA ZP.TraceMessageL LDA #(validateFilenameTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
                    return;
                }
            }
            
            // Filename is valid (1-13 chars, valid characters)
            SEC
            break;
        }
        
        PLY
#ifdef TRACEFILE
        LDA #(validateFilenameTrace % 256) STA ZP.TraceMessageL LDA #(validateFilenameTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Get available free space in bytes
    // Output: TOPH:TOPL = free bytes (16-bit)
    //         C set if successful, NC if error accessing FAT
    // Preserves: X, Y  
    // Munts: A
    GetAvailable()
    {
#ifdef TRACEFILE
        LDA #(getAvailableTrace % 256) STA ZP.TraceMessageL LDA #(getAvailableTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        
        loop // Single exit for cleanup
        {
            // Load FAT from EEPROM
            loadFAT();
            
            // Count free sectors
            LDY #2                   // Start from sector 2 (skip FAT and directory)
            STZ ZP.TOPH      // Free sector count
            
            loop
            {
                LDA FATBuffer, Y
                if (Z)
                {
                    INC ZP.TOPH  // Count free sectors
                }
                
                INY
                if (Z) { break; }    // Y wrapped to 0 - all sectors checked
            }
            
            // Convert sectors to bytes: free_sectors * 256
            // Since each sector is 256 bytes, free_sectors becomes the high byte
            STZ ZP.TOPL
            STZ ZP.TOPT
            // Ready for Print.Decimal() ..
            
            SEC                      // Success
            break;
        }
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(getAvailableTrace % 256) STA ZP.TraceMessageL LDA #(getAvailableTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Format EEPROM with empty file system
    // Output: C set if successful, NC if error
    // Munts: A, X, Y, all file system buffers
    Format()
    {
#ifdef TRACEFILE
        LDA #(formatTrace % 256) STA ZP.TraceMessageL LDA #(formatTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(formatTrace % 256) STA ZP.TraceMessageL LDA #(formatTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Create new file for writing (or overwrite existing)
    // Input: ZP.STR = pointer to filename (uppercase, null-terminated)
    // Output: C set if successful, NC if error
    // Munts: A, X, Y, file system state
    StartSave()
    {
#ifdef TRACEFILE
        LDA #(startSaveTrace % 256) STA ZP.TraceMessageL LDA #(startSaveTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(startSaveTrace % 256) STA ZP.TraceMessageL LDA #(startSaveTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Write data chunk to current save file  
    // Input: SectorSource   = pointer to data
    //        TransferLength = number of bytes to write
    // Output: C set if successful, NC if error (disk full)
    // Preserves: X, Y
    // Munts: A, file system state  
    AppendStream()
    {
#ifdef TRACEFILE
        LDA #(appendStreamTrace % 256) STA ZP.TraceMessageL LDA #(appendStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        
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
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(appendStreamTrace % 256) STA ZP.TraceMessageL LDA #(appendStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Close and finalize current save file
    // Output: C set if successful, NC if error
    // Preserves: X, Y  
    // Munts: A, file system state
    EndSave()
    {
#ifdef TRACEFILE
        LDA #(endSaveTrace % 256) STA ZP.TraceMessageL LDA #(endSaveTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(endSaveTrace % 256) STA ZP.TraceMessageL LDA #(endSaveTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // List all files in directory with optional debug info
    // Output: Directory listing printed to serial, C set if successful
    // Preserves: X, Y
    // Munts: A, file system buffers
    Dir()
    {
#ifdef TRACEFILE
        LDA #(dirTrace % 256) STA ZP.TraceMessageL LDA #(dirTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        
        loop // Single exit
        {
            // Load directory from EEPROM
            loadDirectory();
            
            // Count files and calculate total bytes
            countFilesAndBytes(); // -> TransferLengthL = file count, TransferLengthH/BytesRemainingL = total bytes
            
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
            
#ifdef DEBUG
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
#ifdef TRACEFILE
        LDA #(dirTrace % 256) STA ZP.TraceMessageL LDA #(dirTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    
    
    
    // Delete file from EEPROM file system
    // Input: ZP.STR = pointer to filename (null-terminated)
    // Output: C set if successful, NC if error
    // Preserves: X, Y
    // Munts: A, file system buffers
    Delete()
    {
#ifdef TRACEFILE
        LDA #(deleteFileTrace % 256) STA ZP.TraceMessageL LDA #(deleteFileTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(deleteFileTrace % 256) STA ZP.TraceMessageL LDA #(deleteFileTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    

    


    // Check if file exists in directory
    // Input: ZP.STR = pointer to filename (uppercase, null-terminated)
    // Output: C set if file exists, NC if file not found
    //         CurrentFileEntry = directory entry index if found
    // Preserves: X, Y
    // Munts: A, file system buffers  
    // Note: Throws error if filename format is invalid
    Exists()
    {
    #ifdef TRACEFILE
        LDA #(fileExistsTrace % 256) STA ZP.TraceMessageL LDA #(fileExistsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        PHX
        PHY
        
        // preserve the file name
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
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
            // Returns C if found (with CurrentFileEntry set), NC if not found
            break;
        }
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        PLY
        PLX
    #ifdef TRACEFILE
        LDA #(fileExistsTrace % 256) STA ZP.TraceMessageL LDA #(fileExistsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Refactored StartLoad method:
    // Open file for reading
    // Input: ZP.STR = pointer to filename (uppercase, null-terminated)
    // Output: C set if successful, NC if error (file not found)
    //         File ready for reading via NextStream()
    // Preserves: X, Y
    // Munts: A, file system state
    StartLoad()
    {
    #ifdef TRACEFILE
        LDA #(startLoadTrace % 256) STA ZP.TraceMessageL LDA #(startLoadTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
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
            File.Exists();
            if (NC)
            {
                Error.FileNotFound(); BIT ZP.EmulatorPCL
                break;
            }
            // CurrentFileEntry now contains the directory entry index
            
            // Get start sector from directory entry
            getFileStartSector(); // -> FileStartSector
            
            // Get file length from directory entry
            getFileLength(); // -> BytesRemainingL/H
            
            // Initialize load state
            STZ SectorPositionL
            STZ SectorPositionH
            
            // Read first sector into FileDataBuffer
            LDA FileStartSector
            readSector();
            LDA FileStartSector
            STA CurrentFileSector
            
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
    #ifdef TRACEFILE
        LDA #(startLoadTrace % 256) STA ZP.TraceMessageL LDA #(startLoadTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Read next chunk of data from current load file
    // Output: C set if data available, NC if end of file
    //         SectorSource = pointer to data buffer (if C set)
    //         TransferLength = number of bytes available (if C set)
    // Preserves: X, Y
    // Munts: A, file system state
    // Note: Caller must process data before next call
    NextStream()
    {
#ifdef TRACEFILE
        LDA #(nextStreamTrace % 256) STA ZP.TraceMessageL LDA #(nextStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        
        loop // Single exit
        {
            // Check if any bytes remaining in file
            LDA BytesRemainingL
            ORA BytesRemainingH
            if (Z)
            {
                States.SetSuccess();
                CLC  // End of file
                break;
            }
            
            // If SectorPosition >= 256, advance to next sector
            LDA SectorPositionH
            if (NZ)
            {
                advanceToNextSector();
                if (NC)
                {
                    States.SetFailure();
                    Error.EEPROMError(); BIT ZP.EmulatorPCL
                    break;
                }
                continue;
            }
            
            // Calculate available bytes in sector: 256 - SectorPosition
            SEC
            LDA #0
            SBC SectorPositionL
            STA TransferLengthL
            LDA #1
            SBC #0
            STA TransferLengthH
            
            // Use minimum of (available, remaining)
            // 16-bit compare: TransferLength vs BytesRemaining
            LDA BytesRemainingH
            CMP TransferLengthH
            if (C)  // BytesRemaining >= TransferLength
            {
                if (Z)  // High bytes equal, compare low
                {
                    LDA BytesRemainingL
                    CMP TransferLengthL
                    if (C)  // BytesRemaining >= TransferLength
                    {
                        // Keep TransferLength (smaller or equal)
                    }
                    else
                    {
                        // Use BytesRemaining (smaller)
                        LDA BytesRemainingL
                        STA TransferLengthL
                        LDA BytesRemainingH
                        STA TransferLengthH
                    }
                }
                // else BytesRemaining > TransferLength, keep TransferLength
            }
            else
            {
                // BytesRemaining < TransferLength, use BytesRemaining
                LDA BytesRemainingL
                STA TransferLengthL
                LDA BytesRemainingH
                STA TransferLengthH
            }
            
            // Set pointer to data
            LDA #(FileDataBuffer % 256)
            CLC
            ADC SectorPositionL
            STA SectorSourceL
            LDA #(FileDataBuffer / 256)
            ADC #0
            STA SectorSourceH
            
            // Update counters
            updateStreamPosition();
     
            States.SetSuccess();
            SEC  // Success
            break;
        }
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(nextStreamTrace % 256) STA ZP.TraceMessageL LDA #(nextStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
              

    
    
    // Get file length from current directory entry
    // Input: CurrentFileEntry = directory entry index
    // Output: BytesRemainingL/H = file length (16-bit)
    // Munts: A, Y
    getFileLength()
    {
#ifdef TRACEFILE
        LDA #(getFileLengthTrace % 256) STA ZP.TraceMessageL LDA #(getFileLengthTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Calculate directory entry offset: CurrentFileEntry * 16
        LDA CurrentFileEntry
        ASL A ASL A ASL A ASL A                 // * 16
        TAY                                     // Y = directory entry offset
        
        // Read file length from directory entry (bytes 0-1)
        LDA DirectoryBuffer + 0, Y              // Length LSB
        STA BytesRemainingL
        LDA DirectoryBuffer + 1, Y              // Length MSB
        STA BytesRemainingH
#ifdef TRACEFILE
        PHA LDA #(getFileLengthTrace % 256) STA ZP.TraceMessageL LDA #(getFileLengthTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif
    }
    
    // Advance to next sector in FAT chain
    // Output: C set if successful, NC if end of chain or error
    // Munts: A, Y, file system state
    advanceToNextSector()
    {
#ifdef TRACEFILE
        LDA #(advanceToNextSectorTrace % 256) STA ZP.TraceMessageL LDA #(advanceToNextSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop // Single exit
        {
            // Get next sector from FAT
            LDY CurrentFileSector
            LDA FATBuffer, Y
            STA NextFileSector
            
            // Check for end of chain
            CMP #1                              // 1 = end-of-chain marker
            if (Z)
            {
                // Reached end of file chain
                CLC
                break;
            }
            
            // Update current sector and reset position
            LDA NextFileSector
            STA CurrentFileSector
            
            // Read next sector
            readSector();
            
            STZ SectorPositionL
            STZ SectorPositionH
            
            // Success
            SEC
            break;
        }
#ifdef TRACEFILE
        LDA #(advanceToNextSectorTrace % 256) STA ZP.TraceMessageL LDA #(advanceToNextSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Update position counters after NextStream() call
    // Input: TransferLength = bytes being returned to caller
    // Output: SectorPosition and BytesRemaining updated
    // Munts: A
    updateStreamPosition()
    {
#ifdef TRACEFILE
        LDA #(updateStreamPositionTrace % 256) STA ZP.TraceMessageL LDA #(updateStreamPositionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Update sector position (16-bit addition)
        CLC
        LDA SectorPositionL
        ADC TransferLengthL
        STA SectorPositionL
        LDA SectorPositionH
        ADC TransferLengthH
        STA SectorPositionH
        
        // Update file bytes remaining (16-bit subtraction)
        SEC
        LDA BytesRemainingL
        SBC TransferLengthL
        STA BytesRemainingL
        LDA BytesRemainingH
        SBC TransferLengthH
        STA BytesRemainingH
#ifdef TRACEFILE
        LDA #(updateStreamPositionTrace % 256) STA ZP.TraceMessageL LDA #(updateStreamPositionTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    // Find file in directory by filename
    // Input: ZP.STR = filename to find
    // Output: C set if found, NC if not found
    //         CurrentFileEntry = directory entry index if found
    // Munts: A, Y
    findFileInDirectory()
    {
#ifdef TRACEFILE
        LDA #(findFileInDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(findFileInDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
                compareFilenames(); // Uses X = dir entry offset, ZP.STR = filename
                if (C)
                {
                    // Found the file
                    STY CurrentFileEntry
                    SEC
#ifdef TRACEFILE
                    LDA #(findFileInDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(findFileInDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
                    return;
                }
            }
            
            INY
            CPY #16              // 16 directory entries maximum
            if (Z)
            {
                // Not found
                CLC
#ifdef TRACEFILE
                LDA #(findFileInDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(findFileInDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
                return;
            }
        }
    }
    
    // Compare filename in directory entry with input filename
    // Input: X = directory entry byte offset, ZP.STR = filename to match
    // Output: C set if match, NC if no match
    // Preserves: X, Y
    // Munts: A
    compareFilenames()
    {
#ifdef TRACEFILE
        LDA #(compareFilenamesTrace % 256) STA ZP.TraceMessageL LDA #(compareFilenamesTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        
        // Point to filename field in directory entry (offset +3)
        TXA
        CLC
        ADC #3
        TAX                      // X = filename field start in DirectoryBuffer
        
        LDY #0                   // Index into input filename
        
        loop
        {
            // Check for empty input (illegal filename)
            LDA [ZP.STR], Y
            if (Z)
            {
                CLC                  // Empty filename - immediate fail
                break;
            }
            
            // Check directory high bit first (primary branch)
            LDA DirectoryBuffer, X
            if (MI)                  // High bit set - last directory character
            {
                // Last character handler
                AND #0x7F            // Clear high bit
                CMP [ZP.STR], Y      // Compare with input character
                if (NZ)
                {
                    CLC              // Characters don't match
                    break;
                }
                
                // Characters match - check if input also ends here
                INY
                LDA [ZP.STR], Y
                if (Z)
                {
                    SEC              // Perfect match - both end together
                }
                else
                {
                    CLC              // Directory ended but input continues
                }
                break;
            }
            
            // Normal comparison path (no high bit)
            AND #0x7F                // Clear any high bit (paranoia)
            CMP [ZP.STR], Y          // Compare with input character
            if (NZ)
            {
                CLC                  // No match
                break;
            }
            
            // Characters match - advance both pointers
            INY
            INX
        }
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(compareFilenamesTrace % 256) STA ZP.TraceMessageL LDA #(compareFilenamesTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Compare filename in directory entry with input filename
    // Input: X = directory entry byte offset, ZP.STR = filename to match
    // Output: C set if match, NC if no match
    // Preserves: X, Y
    // Munts: A
    /*
    compareFilenames()
    {
        PHX
        PHY
        
        // Point to filename field in directory entry (offset +3)
        TXA
        CLC
        ADC #3
        TAX                      // X = filename field start in DirectoryBuffer
        
        LDY #0                   // Index into input filename
        
        loop
        {
            // Get character from input filename
            LDA [ZP.STR], Y
            if (Z)                // End of input filename
            {
                // Input filename ended - check if directory filename also ends here
                LDA DirectoryBuffer, X
                if (MI)              // High bit set = last char in directory
                {
                    SEC              // Perfect match
                }
                else
                {
                    CLC              // Input ended but directory continues
                }
                break;
            }
            
            // Get character from directory filename and clear high bit for comparison
            LDA DirectoryBuffer, X
            AND #0x7F                // Clear high bit for comparison
            
            // Compare characters
            CMP [ZP.STR], Y
            if (NZ)
            {
                CLC                  // No match
                break;
            }
            
            LDA DirectoryBuffer, X
            // Check if this was the last character in directory filename
            if (MI)                  // High bit set = last character
            {
                // Directory filename ended - check if input also ends
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
            
            // Move to next character
            INY
            INX
            CPY #13                  // Max filename length check
            if (Z)
            {
                CLC                  // Filename too long - no match
                break;
            }
        }
        
        PLY
        PLX
    }
    */
    
    // Get start sector from current directory entry
    // Input: CurrentFileEntry = directory entry index
    // Output: FileStartSector = start sector number
    // Munts: A, Y
    getFileStartSector()
    {
#ifdef TRACEFILE
        LDA #(getFileStartSectorTrace % 256) STA ZP.TraceMessageL LDA #(getFileStartSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Calculate directory entry offset: CurrentFileEntry * 16 + 2 (start sector field)
        LDA CurrentFileEntry
        ASL ASL ASL ASL          // * 16
        CLC
        ADC #2                   // + 2 for start sector field offset
        TAY                      // Y = start sector field offset
        
        // Read start sector from directory entry
        LDA DirectoryBuffer, Y
        STA FileStartSector
#ifdef TRACEFILE
        LDA #(getFileStartSectorTrace % 256) STA ZP.TraceMessageL LDA #(getFileStartSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Free all sectors in file's FAT chain
    // Input: FileStartSector = first sector to free
    // Output: All sectors in chain marked as free (0) in FAT
    // Munts: A, CurrentFileSector, NextFileSector
    freeFileSectorChain()
    {
#ifdef TRACEFILE
        LDA #(freeFileSectorChainTrace % 256) STA ZP.TraceMessageL LDA #(freeFileSectorChainTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(freeFileSectorChainTrace % 256) STA ZP.TraceMessageL LDA #(freeFileSectorChainTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Clear directory entry for deleted file
    // Input: CurrentFileEntry = directory entry index
    // Output: Directory entry cleared (all zeros)
    // Munts: A, X, Y
    clearDirectoryEntry()
    {
#ifdef TRACEFILE
        LDA #(clearDirectoryEntryTrace % 256) STA ZP.TraceMessageL LDA #(clearDirectoryEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Calculate directory entry offset: CurrentFileEntry * 16
        LDA CurrentFileEntry
        ASL ASL ASL ASL          // * 16
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
#ifdef TRACEFILE
        LDA #(clearDirectoryEntryTrace % 256) STA ZP.TraceMessageL LDA #(clearDirectoryEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    
    
    
    
    // Count files and calculate total bytes used
    // Output: TransferLengthL = file count
    //         TransferLengthH/BytesRemainingL = total bytes (16-bit)
    // Munts: A, Y
    countFilesAndBytes()
    {
#ifdef TRACEFILE
        LDA #(countFilesAndBytesTrace % 256) STA ZP.TraceMessageL LDA #(countFilesAndBytesTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(countFilesAndBytesTrace % 256) STA ZP.TraceMessageL LDA #(countFilesAndBytesTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print all file entries with optional debug info
    // Munts: A, X, Y
    printAllFileEntries()
    {
#ifdef TRACEFILE
        LDA #(printAllFileEntriesTrace % 256) STA ZP.TraceMessageL LDA #(printAllFileEntriesTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(printAllFileEntriesTrace % 256) STA ZP.TraceMessageL LDA #(printAllFileEntriesTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print single file entry: "FILENAME.EXT    1234 bytes"
    // Input: X = directory entry byte offset
    // Munts: A, Y
    printFileEntry()
    {
#ifdef TRACEFILE
        LDA #(printFileEntryTrace % 256) STA ZP.TraceMessageL LDA #(printFileEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHY
        PHX
        
        TXA
        TAY
        
        LDX #4
        Print.Spaces();
        
        printFilenameFromDirectory(); // Uses Y = filename start offset
        
        // Print file size
        // Y already has directory entry offset 
        printFileSizeFromDirectory(); // Uses Y = directory entry offset
        
        // " BYTES"
        LDA # ErrorID.BytesLabel LDX # MessageExtras.PrefixSpace Error.MessageNL();
        
        PLX
        PLY
#ifdef TRACEFILE
        LDA #(printFileEntryTrace % 256) STA ZP.TraceMessageL LDA #(printFileEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print filename from current directory entry 
    // Input: Y = directory entry byte offset (0, 16, 32, 48, ...)
    // Output: Filename printed to serial
    // Preserves: Y
    // Munts: A
    printFilenameFromDirectory()
    {
#ifdef TRACEFILE
        LDA #(printFilenameFromDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(printFilenameFromDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        TYA
        CLC
        ADC #3                   // Offset to filename field
        TAY
        
        LDX #16
        loop
        {
            LDA DirectoryBuffer, Y
            
            PHA                      // Save character
            AND #0x7F                // Clear high bit
            Print.Char();            // Print character
            DEX                      // count the character (for space padding below)
            PLA                      // Restore character
            
            if (MI) { break; }       // High bit set = last character
            INY
            
        } // single exit
        loop
        {
            Print.Space();
            DEX
            if (Z) { break; }
        }
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(printFilenameFromDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(printFilenameFromDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print file size from current directory entry
    // Input: Y = directory entry byte offset (0, 16, 32, 48, ...)  
    // Output: File size printed to serial as decimal
    // Munts: A, ZP.TOPL, ZP.TOPH, ZP.TOPT
    printFileSizeFromDirectory()
    {
#ifdef TRACEFILE
        LDA #(printFileSizeFromDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(printFileSizeFromDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA DirectoryBuffer + 0, Y  // Length LSB
        STA ZP.TOPL
        LDA DirectoryBuffer + 1, Y  // Length MSB
        STA ZP.TOPH
        STZ ZP.TOPT
        
        // right aligment of numbers in the 10..9999 range
        PHX
        LDX #0                       // assume 0 spaces of padding
        LDA ZP.TOPH
        CMP #4                       // Check for 1024+
        if (NC)                      // < 1024, need more analysis
        {
            CMP #3                   // Check if TOPH = 3 (768-1023)
            if (Z)                   // TOPH = 3
            {
                LDA ZP.TOPL
                CMP #232             // 1000 = 3*256 + 232
                if (NC)              // < 1000 (768-999 range)
                {        INX }       //     3-digit numbers get 1 space
                // else >= 1000 (1000-1023), keep X=0 for 4-digit alignment
            }
            else                     // TOPH = 0, 1, or 2
            {
                CMP #0
                if (NZ)              // TOPH = 1 or 2 (256-767)
                {        INX }       //     3-digit numbers get 1 space
                else                 // TOPH = 0 (0-255)
                {
                    LDA ZP.TOPL
                    CMP #100
                    if (NC)          // < 100 (10-99 range)
                    { LDX #2 }       //     2-digit numbers get 2 spaces
                    else             // >= 100 (100-255 range)
                    {    INX }       //     3-digit numbers get 1 space
                }
            }
        }
        // else >= 1024, keep X=0 (no padding for 4+ digits)
        Print.Spaces();              // print X spaces (zero is ok)
        PLX
        
        Print.Decimal();
#ifdef TRACEFILE
        LDA #(printFileSizeFromDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(printFileSizeFromDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print directory summary: "3 files, 2373 bytes used"
    // Input: TransferLengthL = file count, TransferLengthH/BytesRemainingL = total bytes
    // Munts: A
    printDirectorySummary()
    {
#ifdef TRACEFILE
        LDA #(printDirectorySummaryTrace % 256) STA ZP.TraceMessageL LDA #(printDirectorySummaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Print file count
        LDA TransferLengthL
        STA ZP.TOPL
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        // " FILES, "
        LDA # ErrorID.Files LDX # (MessageExtras.PrefixSpace|MessageExtras.SuffixComma|MessageExtras.SuffixSpace) Error.Message();
        
        // Print total bytes
        LDA BytesRemainingL
        STA ZP.TOPL
        LDA TransferLengthH
        STA ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        // " BYTES USED"
        LDA # ErrorID.BytesUsedLabel LDX # MessageExtras.PrefixSpace Error.MessageNL();
        
#ifdef TRACEFILE
        LDA #(printDirectorySummaryTrace % 256) STA ZP.TraceMessageL LDA #(printDirectorySummaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Write filename to directory entry
    // Input: CurrentFileEntry = directory entry index, ZP.STR = filename
    // Munts: A, X, Y
    writeFilenameToDirectory()
    {
#ifdef TRACEFILE
        LDA #(writeFilenameToDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(writeFilenameToDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(writeFilenameToDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(writeFilenameToDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Update directory entry with start sector
    // Input: CurrentFileEntry = directory entry index, FileStartSector = sector number
    // Munts: A, Y
    updateDirectoryStartSector()
    {
#ifdef TRACEFILE
        LDA #(updateDirectoryStartSectorTrace % 256) STA ZP.TraceMessageL LDA #(updateDirectoryStartSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Calculate directory entry offset: CurrentFileEntry * 16 + 2 (start sector field)
        LDA CurrentFileEntry
        ASL ASL ASL ASL          // * 16
        CLC
        ADC #2                   // + 2 for start sector field offset
        TAY                      // Y = start sector field offset
        
        // Write start sector to directory entry
        LDA FileStartSector
        STA DirectoryBuffer, Y
#ifdef TRACEFILE
        LDA #(updateDirectoryStartSectorTrace % 256) STA ZP.TraceMessageL LDA #(updateDirectoryStartSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    // Flush current sector and allocate next sector
    // Output: C set if successful, NC if disk full
    // Munts: A, Y, file system state
    flushAndAllocateNext()
    {
#ifdef TRACEFILE
        LDA #(flushAndAllocateNextTrace % 256) STA ZP.TraceMessageL LDA #(flushAndAllocateNextTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(flushAndAllocateNextTrace % 256) STA ZP.TraceMessageL LDA #(flushAndAllocateNextTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Allocate first free sector from FAT
    // Output: C set if sector allocated, NC if disk full
    //         Y = next sector number to allocated
    // Munts: A, Y
    allocateFirstFreeSector()
    {
#ifdef TRACEFILE
        LDA #(allocateFirstFreeSectorTrace % 256) STA ZP.TraceMessageL LDA #(allocateFirstFreeSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(allocateFirstFreeSectorTrace % 256) STA ZP.TraceMessageL LDA #(allocateFirstFreeSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    // Find free directory entry (or existing file to overwrite)
    // Output: C set if entry found, NC if directory full
    //         CurrentFileEntry = entry index (0-15) if found
    // Munts: A, Y
    findFreeDirectoryEntry()
    {
#ifdef TRACEFILE
        LDA #(findFreeDirectoryEntryTrace % 256) STA ZP.TraceMessageL LDA #(findFreeDirectoryEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
                LDA #(findFreeDirectoryEntryTrace % 256) STA ZP.TraceMessageL LDA #(findFreeDirectoryEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
                return;
            }
            
            // Check if filename matches (for overwrite)
            checkFilenameMatch();
            if (C)
            {
                // Found existing file - overwrite it
                STY CurrentFileEntry
                SEC
#ifdef TRACEFILE
                LDA #(findFreeDirectoryEntryTrace % 256) STA ZP.TraceMessageL LDA #(findFreeDirectoryEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
                return;
            }
            
            INY
            CPY #16              // 16 entries maximum
            if (Z)
            {
                // Directory full
                CLC
#ifdef TRACEFILE
                LDA #(findFreeDirectoryEntryTrace % 256) STA ZP.TraceMessageL LDA #(findFreeDirectoryEntryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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
#ifdef TRACEFILE
        LDA #(checkFilenameMatchTrace % 256) STA ZP.TraceMessageL LDA #(checkFilenameMatchTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(checkFilenameMatchTrace % 256) STA ZP.TraceMessageL LDA #(checkFilenameMatchTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    // Initialize file state for save operation
    // Munts: A
    initializeSaveState()
    {
#ifdef TRACEFILE
        LDA #(initializeSaveStateTrace % 256) STA ZP.TraceMessageL LDA #(initializeSaveStateTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Clear file position counters
        STZ FilePosition         // FilePositionL
        STZ FilePosition + 1     // FilePositionH
        STZ SectorPositionL      // Byte position within current sector
        STZ SectorPositionH
        
        // Clear next sector (will be allocated when needed)
        STZ NextFileSector
#ifdef TRACEFILE
        LDA #(initializeSaveStateTrace % 256) STA ZP.TraceMessageL LDA #(initializeSaveStateTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // TODO : inline
    // Clear file data buffer to all zeros
    clearFileDataBuffer()
    {
#ifdef TRACEFILE
        LDA #(clearFileDataBufferTrace % 256) STA ZP.TraceMessageL LDA #(clearFileDataBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #(FileDataBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
#ifdef TRACEFILE
        LDA #(clearFileDataBufferTrace % 256) STA ZP.TraceMessageL LDA #(clearFileDataBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // TODO : inline
    // Clear FAT buffer to all zeros
    clearFATBuffer()
    {
#ifdef TRACEFILE
        LDA #(clearFATBufferTrace % 256) STA ZP.TraceMessageL LDA #(clearFATBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #(FATBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
#ifdef TRACEFILE
        LDA #(clearFATBufferTrace % 256) STA ZP.TraceMessageL LDA #(clearFATBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // TODO : inline
    // Clear directory buffer to all zeros  
    clearDirectoryBuffer()
    {
#ifdef TRACEFILE
        LDA #(clearDirectoryBufferTrace % 256) STA ZP.TraceMessageL LDA #(clearDirectoryBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #(DirectoryBuffer / 256) // MSB - assume page aligned
        Memory.ClearPage();
#ifdef TRACEFILE
        LDA #(clearDirectoryBufferTrace % 256) STA ZP.TraceMessageL LDA #(clearDirectoryBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Load FAT from EEPROM sector 0 into FATBuffer
    // Input: None
    // Output: FATBuffer loaded with FAT data
    // Munts: A, EEPROM operation registers
    loadFAT()
    {
#ifdef TRACEFILE
        LDA #(loadFATTrace % 256) STA ZP.TraceMessageL LDA #(loadFATTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        STZ ZP.IDYH              // EEPROM address MSB = sector 0 (must be page aligned)
        STZ ZP.IDYL
        
        LDA #(FATBuffer / 256)   // RAM address MSB = FATBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.ReadPage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
#ifdef TRACEFILE
        LDA #(loadFATTrace % 256) STA ZP.TraceMessageL LDA #(loadFATTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Write FATBuffer to EEPROM sector 0
    writeFAT()
    {
#ifdef TRACEFILE
        LDA #(writeFATTrace % 256) STA ZP.TraceMessageL LDA #(writeFATTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        STZ ZP.IDYH              // EEPROM address MSB = sector 0 (must be page aligned)
        
        LDA #(FATBuffer / 256)   // RAM address MSB = FATBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
#ifdef TRACEFILE
        LDA #(writeFATTrace % 256) STA ZP.TraceMessageL LDA #(writeFATTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Load directory from EEPROM sector 1 into DirectoryBuffer
    // Input: None
    // Output: DirectoryBuffer loaded with directory data
    // Munts: A, EEPROM operation registers
    loadDirectory()
    {
#ifdef TRACEFILE
        LDA #(loadDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(loadDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #1                           // EEPROM address MSB = sector 1  (must be page aligned)
        STA ZP.IDYH              
        
        LDA #(DirectoryBuffer / 256)     // RAM address MSB = DirectoryBuffer (must be page aligned)
        STA ZP.IDXH

        EEPROM.ReadPage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
#ifdef TRACEFILE
        LDA #(loadDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(loadDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Write DirectoryBuffer to EEPROM sector 1
    writeDirectory()
    {
#ifdef TRACEFILE
        LDA #(writeDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(writeDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #1                           // EEPROM address MSB = sector 1  (must be page aligned)
        STA ZP.IDYH             
        
        LDA #(DirectoryBuffer / 256)     // RAM address MSB = DirectoryBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.WritePage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
#ifdef TRACEFILE
        LDA #(writeDirectoryTrace % 256) STA ZP.TraceMessageL LDA #(writeDirectoryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Read arbitrary sector into FileDataBuffer
    // Input: A = sector number (0-255)
    // Output: 256 bytes copied from EEPROM to FileDataBuffer
    // Munts: A, EEPROM operation registers
    readSector()
    {
#ifdef TRACEFILE
        PHA LDA #(readSectorTrace % 256) STA ZP.TraceMessageL LDA #(readSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        STA ZP.IDYH                 // EEPROM address MSB = sector number (must be page aligned)
        
        LDA #(FileDataBuffer / 256) // RAM address MSB = FileDataBuffer (must be page aligned)
        STA ZP.IDXH
        
        EEPROM.ReadPage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
#ifdef TRACEFILE
        LDA #(readSectorTrace % 256) STA ZP.TraceMessageL LDA #(readSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Write FileDataBuffer to arbitrary sector
    // Input: A = sector number
    writeSector()
    {
#ifdef TRACEFILE
        PHA LDA #(writeSectorTrace % 256) STA ZP.TraceMessageL LDA #(writeSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        STA ZP.IDYH                 // EEPROM address MSB = sector number (must be page aligned)
        
        LDA #(FileDataBuffer / 256) // RAM address MSB = FileDataBuffer (must be page aligned)
        STA ZP.IDXH
        EEPROM.WritePage();
        
        //BIT ZP.ACC // any instruction to defeat the tailcall optimization (JSR -> JMP) for the emulator
#ifdef TRACEFILE
        LDA #(writeSectorTrace % 256) STA ZP.TraceMessageL LDA #(writeSectorTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

#ifdef DEBUG
    // Diagnostic dump of drive state
    // Input:  A = 1 to load from EEPROM, A = 0 to just show current RAM
    // Output: Drive state printed to serial, C set if successful
    // Preserves: X, Y
    // Munts: A
    DumpDriveState()
    {
#ifdef TRACEFILE
        PHA LDA #(dumpDriveStateTrace % 256) STA ZP.TraceMessageL LDA #(dumpDriveStateTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
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
#ifdef TRACEFILE
        LDA #(dumpDriveStateTrace % 256) STA ZP.TraceMessageL LDA #(dumpDriveStateTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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
#ifdef TRACEFILE
        LDA #(dumpDirectoryEntriesTrace % 256) STA ZP.TraceMessageL LDA #(dumpDirectoryEntriesTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
                Print.Space();
                
                // Print filename (scan until high bit found)
                printFilenameFromDirectory();
                
                // Print file info
                Print.Space();
                printFileSizeFromDirectory();
                Print.Space();
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
#ifdef TRACEFILE
        LDA #(dumpDirectoryEntriesTrace % 256) STA ZP.TraceMessageL LDA #(dumpDirectoryEntriesTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Dump FAT allocation map
    dumpFATMap()
    {
#ifdef TRACEFILE
        LDA #(dumpFATMapTrace % 256) STA ZP.TraceMessageL LDA #(dumpFATMapTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
                Print.Space();
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
#ifdef TRACEFILE
        LDA #(dumpFATMapTrace % 256) STA ZP.TraceMessageL LDA #(dumpFATMapTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Calculate and dump sector statistics
    dumpSectorStats()
    {
#ifdef TRACEFILE
        LDA #(dumpSectorStatsTrace % 256) STA ZP.TraceMessageL LDA #(dumpSectorStatsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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
        STZ ZP.TOPH
        STZ ZP.TOPT
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
        STZ ZP.TOPH
        STZ ZP.TOPT
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
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        Print.NewLine();
#ifdef TRACEFILE
        LDA #(dumpSectorStatsTrace % 256) STA ZP.TraceMessageL LDA #(dumpSectorStatsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
        
    // Dump current file operation state (ZP variables)
    // Output: File state printed to serial, C set if successful  
    // Preserves: X, Y
    // Munts: A
    DumpFileState()
    {
#ifdef TRACEFILE
        LDA #(dumpFileStateTrace % 256) STA ZP.TraceMessageL LDA #(dumpFileStateTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(dumpFileStateTrace % 256) STA ZP.TraceMessageL LDA #(dumpFileStateTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print hex dump of first 32 bytes of file
    // Input: X = directory entry byte offset
    // Munts: A, Y
    printFileHexDump()
    {
#ifdef TRACEFILE
        LDA #(printFileHexDumpTrace % 256) STA ZP.TraceMessageL LDA #(printFileHexDumpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHX
        PHY
        
        // Read file's first sector
        LDA DirectoryBuffer + 2, X  // Start sector at offset +2
        readSector();               // Load sector into FileDataBuffer
        
        STZ SectorPositionL     // Set to 0 for first line
        printHexDumpLine();
        
        LDA # 0x10
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x20
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x30
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x40
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x50
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x60
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        LDA # 0x70
        STA SectorPositionL     // Use as offset counter
        printHexDumpLine();
        
        
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(printFileHexDumpTrace % 256) STA ZP.TraceMessageL LDA #(printFileHexDumpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print one line of hex dump (16 bytes)
    // Input: SectorPositionL = starting byte offset (0 or 16)
    // Munts: A, X, Y
    printHexDumpLine()
    {
#ifdef TRACEFILE
        LDA #(printHexDumpLineTrace % 256) STA ZP.TraceMessageL LDA #(printHexDumpLineTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Print 4-space indentation
        LDX #8
        Print.Spaces();
        
        // Print address (00: or 10:)
        LDA SectorPositionL
        Print.Hex();             // Prints 00 or 10
        LDA #':'
        Print.Char();
        Print.Space();
        
        // Print 16 hex bytes
        LDX SectorPositionL      // Starting offset
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
        LDX #2 Print.Spaces();
        
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
#ifdef TRACEFILE
        LDA #(printHexDumpLineTrace % 256) STA ZP.TraceMessageL LDA #(printHexDumpLineTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    // Print detailed diagnostic information
    // Munts: A, X, Y
    printDebugDiagnostics()
    {
#ifdef TRACEFILE
        LDA #(printDebugDiagnosticsTrace % 256) STA ZP.TraceMessageL LDA #(printDebugDiagnosticsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(printDebugDiagnosticsTrace % 256) STA ZP.TraceMessageL LDA #(printDebugDiagnosticsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print directory slot utilization
    printDirectoryUtilization()
    {
#ifdef TRACEFILE
        LDA #(printDirectoryUtilizationTrace % 256) STA ZP.TraceMessageL LDA #(printDirectoryUtilizationTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #(dirUtilLabel % 256)
        STA ZP.STRL
        LDA #(dirUtilLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // File count already in TransferLengthL
        LDA TransferLengthL
        STA ZP.TOPL
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        LDA #'/'
        Print.Char();
        
        LDA #16                  // Max directory entries
        STA ZP.TOPL
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        Print.NewLine();
#ifdef TRACEFILE
        LDA #(printDirectoryUtilizationTrace % 256) STA ZP.TraceMessageL LDA #(printDirectoryUtilizationTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print FAT allocation summary
    printFATAllocationSummary()
    {
#ifdef TRACEFILE
        LDA #(printFATAllocationSummaryTrace % 256) STA ZP.TraceMessageL LDA #(printFATAllocationSummaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(printFATAllocationSummaryTrace % 256) STA ZP.TraceMessageL LDA #(printFATAllocationSummaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print sector allocation info for all files
    // Input: None (uses DirectoryBuffer and FATBuffer)
    // Output: Per-file sector allocation printed to serial
    // Munts: A, X, Y
    // Note: Iterates through directory entries, calls printFileSectorInfo() for each used entry
    printPerFileSectorAllocation()
    {
#ifdef TRACEFILE
        LDA #(printPerFileSectorAllocationTrace % 256) STA ZP.TraceMessageL LDA #(printPerFileSectorAllocationTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
#ifdef TRACEFILE
        LDA #(printPerFileSectorAllocationTrace % 256) STA ZP.TraceMessageL LDA #(printPerFileSectorAllocationTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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
#ifdef TRACEFILE
        LDA #(printFileSectorInfoTrace % 256) STA ZP.TraceMessageL LDA #(printFileSectorInfoTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
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
        Print.Space();
        
        // Print filename
        //TYA
        //ASL A ASL A ASL A ASL A
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
        LDA DirectoryBuffer + 0, X  // Size low
        CLC
        ADC #255                    // Add 255 for ceiling division
        LDA DirectoryBuffer + 1, X  // Size high  
        ADC #0                      // Add carry
        STA ZP.TOPL                 // Result is sectors used
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        LDA #(sectorsLabel % 256)
        STA ZP.STRL
        LDA #(sectorsLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Print FAT chain:
        Print.Space();Print.Space();
        
        // Start walking the FAT chain
        LDA DirectoryBuffer + 2, X  // Get start sector
        STA CurrentFileSector       // Use as chain walker
        STZ TransferLength          // Count actual sectors in chain
        
        loop // Walk FAT chain
        {
            // Print current sector in hex
            LDA #'0'
            Print.Char();
            LDA #'x'
            Print.Char();
            LDA CurrentFileSector
            Print.Hex();
            
            INC TransferLength      // Count this sector
            
            // Get next sector from FAT
            LDY CurrentFileSector
            LDA FATBuffer, Y
            STA NextFileSector
            
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
            LDA NextFileSector
            STA CurrentFileSector
        }
        
        
        Print.NewLine();
        
        PLY
        PLX
#ifdef TRACEFILE
        LDA #(printFileSectorInfoTrace % 256) STA ZP.TraceMessageL LDA #(printFileSectorInfoTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Print free space summary
    printFreeSpaceSummary()
    {
#ifdef TRACEFILE
        LDA #(printFreeSpaceSummaryTrace % 256) STA ZP.TraceMessageL LDA #(printFreeSpaceSummaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
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
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        LDA #'/'
        Print.Char();
        
        LDA #254                 // Total usable sectors (256 - 2 system)
        STA ZP.TOPL
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
        Print.NewLine();
#ifdef TRACEFILE
        LDA #(printFreeSpaceSummaryTrace % 256) STA ZP.TraceMessageL LDA #(printFreeSpaceSummaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
#endif
}
