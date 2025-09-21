program BIOS
{
    #define HOPPER_BIOS
    
    //#define DEBUG     // mimimum of 874 bytes
    //#define FILEDEBUG
    #define CPU_65C02S
    
    #define RELEASE // remove all the BIT ZP.EmulatorPCL hacks (~40 bytes)
    
#ifdef DEBUG    
    #define ROM_32K
#else
    //#define ROM_16K
    #define ROM_8K
#endif
    
    
    // Optional components
    //#define HASFLOAT  // currently ~1250 bytes
    #define HASEEPROM
    #define CFILES
    
    uses "Definitions/Limits"
    uses "Definitions/MemoryMap"
    uses "Definitions/ZeroPage"
    
    uses "Debugging/Debug"
    uses "Debugging/Error"
    
    uses "Memory/Memory"

#if defined(HASEEPROM)
    uses "Devices/EEPROM"
    uses "Files/File"
#endif        
    uses "Types/Char"
    uses "Types/String"
    uses "Types/Long"
    
#if defined(HASFLOAT) 
    uses "Types/Float"
#endif
    
    uses "Serial"
    uses "Parallel"
    uses "Time"
    uses "GPIO"
    
    uses "Print"
    uses "Shared"
    
    uses "Definitions/BIOSInterface"
    uses "SysCalls"
    
    
    //uses "TestSuite/TestTime"
    //uses "TestSuite/TestHeap"
    //uses "TestSuite/TestLong"
    //uses "TestSuite/TestFloat"
    
    Run()
    {
        Tests.RunTests();
        
        // End marker
        LDA #'!'
        Serial.WriteChar();
        NewLine();
        
        loop { }
    }
    
    
    IRQ()
    {
        Serial.ISR();
        Parallel.ISR();
    }
    
    NMI()
    {
        // Hardware break - NMI -> <ctrl><C>
        SMB0 ZP.FLAGS
    }
    
    Initialize()
    {
        SEI  // Disable interrupts during initialization
        
        // Clear Zero Page
        LDX #0
        loop
        {
            CPX # ZP.ACIADATA // don't write to ACIA data register
            if (NZ) 
            {
                CPX # ZP.BIOSDISPATCHL // don't overwrite the SysCall dispatch vector
                if (NZ)
                {
                    CPX # ZP.BIOSDISPATCHH
                    if (NZ)
                    {
                        STZ 0x00, X
                    }
                }
            }
            DEX
            if (Z) { break; }
        } 
        
        Error.ClearError();
        
        // Initialize communication first
        Serial.Initialize();
        Parallel.Initialize();
        
        LDA # (Address.UserMemory >> 8)
        Memory.Initialize();
        
        STZ ZP.FLAGS

#if defined(HASEEPROM)
        EEPROM.Initialize();
#endif
        CLI  // Re-enable interrupts
    }
    vt100Escape()
    {
        LDA # Char.Escape Print.Char();
        LDA #'[' Print.Char();
        TXA Print.Char();
    }
    cmdCls()
    {
        // "ESC[H" : move cursor to (1,1)
        LDX # 'H'
        vt100Escape();
        
        // "ESC[J" : clear from cursor to end of screen
        LDX # 'J'
        vt100Escape();
    }
    cmdFormat()
    {
        // Confirm destructive action using Error system
        LDA # ErrorID.FormatWarning 
        LDX # MessageExtras.SuffixSpace
        confirmYesNo();               // Simple Y/N reader
        if (NC) { return; }           // User cancelled
        
        File.Format();                // This sets ZP.LastError on failure
    }
    
    dispatch()
    {
        JMP [ZP.BIOSDISPATCH]
    }
    
    callApplet()
    {
        LDA # (Address.UserMemory % 256)
        STA ZP.ACCL
        LDA # (Address.UserMemory / 256)
        STA ZP.ACCH
        JMP [ZP.ACC]
    }
    
    cmdMem()
    {
        // Print result using existing Print routines or Error messages
        LDA # ErrorID.MemoryMessage 
        LDX # (MessageExtras.SuffixSpace|MessageExtras.SuffixColon)
        Error.Message();
        
        // testing dispatch
        //LDX #SysCall.MemAvailable
        //dispatch();
        Memory.Available();        // Returns available bytes in ZP.ACC
        
        Shared.MoveAccToTop();
        Long.Print();              // Print ZP.TOP value
        
        LDA # ErrorID.BytesMessage 
        LDX # MessageExtras.PrefixSpace
        Error.Message();
        Print.NewLine();
#ifdef HASEEPROM
        EEPROM.Detect();
        if (C)  // Set C (detected)
        {
            LDA # ErrorID.EEPROMLabel LDX # (MessageExtras.SuffixColon|MessageExtras.SuffixSpace) Error.Message();
            
            EEPROM.GetSize(); // A -> number of K
            Shared.LoadTopByte() ;
            Long.Print();
            
            LDA #'K' Print.Char();
            LDA #',' Print.Char();
            Print.Space();
            File.GetAvailable(); // TOP -> number of B
            
            Long.Print();
            LDA # ErrorID.BytesMessage LDX # MessageExtras.PrefixSpace Error.MessageNL();
        }
#endif
    }
    
    cmdDir()
    {
        File.Dir();                   // This sets ZP.LastError on failure
    }
    
    confirmYesNo()
    {
        Error.Message();
        LDX # (MessageExtras.InParens|MessageExtras.SuffixQuest|MessageExtras.SuffixSpace)
        loop
        {
            LDA # ErrorID.YesNo 
            Error.Message();
              
            Serial.WaitForChar();
            AND #0xDF                 // Convert to uppercase
            PHA
            Serial.WriteChar();       // Echo
            Print.NewLine();
            PLA
            CMP #'Y'
            if (Z) { SEC return; }   // Yes
            CMP #'N'  
            if (Z) { CLC return; }   // No
            
            // Invalid - try again
            LDX # (MessageExtras.SuffixQuest|MessageExtras.SuffixSpace)
        }
    }
    
    cmdDel()
    {
        parseFilename();
        if (NC) 
        {
            return;
        }
        File.Delete();
    }
    
    cmdHex()
    {
        parseFilename();
        if (NC) 
        {
            return;
        }
        
        // parseFilename() left ZP.STR pointing to filename in LineBuffer
        // We need to copy it to start of LineBuffer first
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            STA Address.LineBuffer, Y
            if (Z) { break; }
            INY
        }
        
        // Address.LineBuffer + ".EXE" -> STR
        buildExecutableName();
        
        // Check if file exists
        LDA # DirWalkAction.FindFile
        File.Exists();
        if (C)
        {
            // file exits
            LDA # ErrorID.OverwriteWarning
            LDX # MessageExtras.SuffixSpace
            confirmYesNo(); // preserves ZP.STR
            if (C)
            {
                // confirmed
                File.Delete(); // preserves ZP.STR
            }
            else
            {
                // cancelled
                return;
            }
        }
        
        // Start file save
        File.StartSave();
        if (NC) 
        {
            return;
        }
        LDA # ErrorID.ReadyForHEX
        LDX # MessageExtras.SuffixColon
        Error.MessageNL();
        
        Shared.ZeroTop();
        
        // Process Intel HEX lines
        loop
        {
            processIHexLine();
            if (NC) { break; }  // EOF record found
        }
        
        // Close file
        LDA # 0x80 // high bit for executable file
        File.EndSave();
        
        Print.NewLine();
        
        LDA # ErrorID.HEXDone
        LDX # (MessageExtras.SuffixColon|MessageExtras.SuffixSpace)
        Error.Message();
        Long.Print();
        LDA # ErrorID.BytesLabel
        LDX # MessageExtras.PrefixSpace
        Error.MessageNL();
        
    }
    
    processIHexLine()  // Returns C set to continue, NC for EOF
    {
        // Set up buffer pointer for File.AppendStream()
        LDA # (Address.HexBuffer % 256)
        STA SectorSourceL
        LDA # (Address.HexBuffer / 256)
        STA SectorSourceH
        
        // Wait for ':' start character
        loop
        {
            Serial.WaitForChar();
            CMP #':'
            if (Z) { break; }
        }
        
        // Get byte count
        Serial.HexIn();
        STA TransferLengthL
        STZ TransferLengthH
        
        if (BBR7, ZP.TOP0)
        {
            LDA #'.'
            Print.Char();
        }
        
        // Skip address (4 hex chars = 2 bytes)
        Serial.HexIn();  // Address high (ignore)
        Serial.HexIn();  // Address low (ignore)
        
        // Get record type
        Serial.HexIn();
        CMP #0x01        // EOF record?
        if (Z)   
        {
            Serial.HexIn(); // chomp the FF
            Serial.IsAvailable();
            if (C)
            {
                Serial.WaitForChar(); // final newline
            }
            CLC             // Signal EOF
            return;
        }
        CMP #0x00        // Data record?
        if (NZ)          // Skip non-data records
        {
            // Skip data bytes and checksum for non-data records
            LDX TransferLengthL
            loop
            {
                CPX #0
                if (Z) { break; }
                Serial.HexIn();
                DEX
            }
            Serial.HexIn();  // Skip checksum
            SEC              // Continue
            return;
        }
        
        // Read data bytes into FileDataBuffer
        LDY #0
        LDX TransferLengthL
        loop
        {
            CPX #0
            if (Z) { break; }
            Serial.HexIn();
            STA Address.HexBuffer, Y
            //Print.Hex();
            
            INC ZP.TOP0
            if (Z) { INC ZP.TOP1 }
            
            INY
            DEX
        }
        
        // Skip checksum
        Serial.HexIn();
        
        // Write data to file if we have data
        LDA TransferLengthL
        if (NZ)
        {
            File.AppendStream();
        }
        SEC              // Continue processing
    }
    
    // Build executable filename from first word of LineBuffer
    // Extracts the command name (up to first space) and conditionally appends .EXE
    // 
    // Input:  LineBuffer contains the full command line (e.g., "TYPE SOURCE.C")
    // Output: GeneralBuffer contains executable filename
    //         - If command has no extension (no dot): appends ".EXE"
    //         - If command already has extension: keeps as-is
    //
    // Examples:
    //   "TYPE SOURCE.C"  -> "TYPE.EXE"     (command "TYPE" gets .EXE)
    //   "EDIT.TXT FILE"  -> "EDIT.TXT"     (already has extension)
    //   "PROG"           -> "PROG.EXE"     (no extension, adds .EXE)
    //   "TEST.COM"       -> "TEST.COM"     (keeps original extension)
    //
    // Preserves: LineBuffer unchanged (arguments remain available to program)
    // Munts: A, GeneralBuffer, ZP.TEMP (temporary)
    // Returns: STR pointing to GeneralBuffer with null-terminated filename (empty string if error)
    buildExecutableName()
    {
        PHY
        PHX
        
        LDY #0  // Source index (LineBuffer)
        LDX #0  // Dest index (GeneralBuffer)
        STZ ZP.TEMP  // ZP.TEMP = 1 if we found a dot (temp use)
        
        // Copy command name until null
        loop
        {
            LDA Address.LineBuffer, Y
            if (Z) { break; }      // End of line
            
            // Check for dot
            CMP #'.'
            if (Z) 
            { 
                INC ZP.TEMP     // Mark that we found a dot
            }
            
            // Store character in GeneralBuffer
            STA Address.GeneralBuffer, X
            
            INY
            INX
            CPX #60  // Leave room for .EXE and null (64 - 4 - 1 = 59, round to 60)
            if (Z) { break; }  // Prevent buffer overrun
        }
        
        // Only append .EXE if no dot was found
        LDA ZP.TEMP
        if (Z)  // No dot found
        {
            // Append .EXE (we have room since we stopped at 60)
            LDA #'.'
            STA Address.GeneralBuffer, X
            INX
            LDA #'E'
            STA Address.GeneralBuffer, X
            INX
            LDA #'X'
            STA Address.GeneralBuffer, X
            INX
            LDA #'E'
            STA Address.GeneralBuffer, X
            INX
        }
        
        // Null terminator
        LDA #0
        STA Address.GeneralBuffer, X
        
        // Point ZP.STR to the result in GeneralBuffer
        LDA # (Address.GeneralBuffer % 256)
        STA ZP.STRL
        LDA # (Address.GeneralBuffer / 256)
        STA ZP.STRH
        
        PLX
        PLY
    }
    
    parseAndExecute()
    {
        Error.FindKeyword(); // -> token in A
        if (NZ)
        {
            // Execute command based on token
            switch (A)
            {
                case ErrorWord.FORMAT: { cmdFormat();  return; }
                case ErrorWord.MEM:    { cmdMem();     return; }
                case ErrorWord.DIR:    { cmdDir();     return; }
                case ErrorWord.CLS:    { cmdCls();     return; }
                case ErrorWord.HEX:    { cmdHex();     return; }
                case ErrorWord.DEL:    { cmdDel();     return; }
                case ErrorWord.EXIT:   { SMB7 ZP.FLAGS return; }
                
                // Keyword from wrong table to cause system calls to be included in the build
                // so that the optimizer doesn't remove it.
                case ErrorWord.HEAP:   { SystemCallDispatcher(); return; } 
            }
        }
        
        // Build filename with .EXE in STR-> GeneralBuffer (not modifying LineBuffer)
        buildExecutableName();  // Creates "COMMAND.EXE" in STR-> GeneralBuffer
        
        // Input: ZP.STR = pointer to filename (uppercase, null-terminated)
        //        A = DirWalkAction.FindExecutable or DirWalkAction.FindFile
        // Output: C set if successful, NC if error (file not found)
        //         File ready for reading via NextStream()
        LDA # DirWalkAction.FindExecutable
        File.StartLoad();
        if (C) 
        {
            // load and execute
            LDA # (Address.UserMemory / 256)
            STA ZP.IDXH
            
            
            // Read next chunk of data from current load file
            // Prerequisites: StartLoad() must be called first to initialize file state
            //                Caller must consume all returned data before calling again
            // Output: C set if data available, NC if end of file or error
            //         Data is always in FileDataBuffer starting at offset 0
            //         TransferLength = number of valid bytes in FileDataBuffer (if C set)
            //         TransferLengthL/H = 16-bit byte count (max 256 per call)
            //         States.Success flag set appropriately
            // Preserves: X, Y
            // Munts: A, BytesRemaining, CurrentFileSector, NextFileSector
            // Note: - Returns up to 256 bytes (one sector) per call
            //       - Pre-reads next sector after returning data (if more exists)
            //       - FileDataBuffer will be overwritten on next call
            //       - No partial read support - caller must consume entire TransferLength
            loop
            {
                LDA ZP.IDXH PHA
                
                File.NextStream(); // munts IDX and IDY
                
                PLA STA ZP.IDXH
                
                if (NC) { break; } // done
                
                // just transfer the whole sector, even if it was partial
                LDX #0
                STZ ZP.IDXL
                loop
                {
                    LDA File.FileDataBuffer, X
                    STA [ZP.IDX]
                    IncIDX();
                    INX
                    if (Z) { break; }
                }
            }
            LDA ZP.LastError
            if (Z)
            {
                // re-initialize heap
                LDA ZP.IDXH
                Memory.Initialize();
        
                // execute
                callApplet(); // must be followed by any instruction to defeat tailcall optimization (JSR RTS -> JMP)
        
                // free the program space and re-initialize the heap        
                LDA # (Address.UserMemory >> 8)
                Memory.Initialize();
                
                Print.NewLine(); 
                return;           
            }
        }
        // "FILE NOT FOUND"
    }
    
    // Parse filename from command line after the command keyword
    // Input:  X = position in LineBuffer after command word  
    // Output: C set if successful, NC if no filename found
    //         ZP.STR points to filename in LineBuffer
    parseFilename()
    {
        // Skip any more 'spaces' before the filename
        loop
        {
            LDA Address.LineBuffer, X
            if (NZ) { break; } // not null
            
            CPX #63
            if (Z)  
            {
                // end of line, no filename found
                Error.FilenameExpected();
                CLC return;
            }
            INX
        }
        
        // Point ZP.STR to filename start in LineBuffer
        TXA
        CLC
        ADC # (Address.LineBuffer % 256)
        STA ZP.STRL  
        LDA # (Address.LineBuffer/ 256)
        ADC #0
        STA ZP.STRH
        SEC  // Success
    }
    
    processCommandLine()
    {
        readLine();                    // Fill LineBuffer
        if (Z) { return; }             // Empty line
        
        parseAndExecute();             // Combined parse + execute
        Error.CheckAndPrint();         // Handle any errors set by commands
    }
    
    readLine()
    {
        LDX #0
        loop
        {
            Serial.WaitForChar();   // Get character
            CMP # Char.EOL          // CR?
            if (Z) 
            { 
                break;
            }
            CMP # 0x0D              // LF?
            if (Z) 
            { 
                break;
            }
            CMP # Char.Backspace          // Backspace?
            if (Z)
            {
                CPX #0
                if (NZ) 
                { 
                    DEX                   Serial.WriteChar();  // Echo backspace
                    LDA #' '              Serial.WriteChar();  // Space
                    LDA # Char.Backspace  Serial.WriteChar();  // Backspace again
                }
                continue;
            }
            // Skip leading spaces
            CMP #' '
            if (Z)
            {
                CPX #0
                if (Z) { continue; }    // Ignore space at start of line
                STZ LineBuffer, X       // store null in buffer (as 'space')
            }
            else
            {
                Char.ToUpper();        // A -> uppercase A
                STA LineBuffer, X      // store character in buffer
            }
            Serial.WriteChar();        // Echo character
            
            INX
            CPX #63                    // Prevent overflow
            if (Z) { break; }
        } // loop
        
        Print.NewLine();
        STZ Address.LineBuffer, X      // null terminate
        
        TXA                            // Return length
    }
    
    printWelcome()
    {
        cmdCls();
        LDA # ErrorID.SystemReady LDX # MessageExtras.None 
        Error.MessageNL();
        cmdMem();    
    }
    
    printPrompt()
    {
        LDA #'>'
        Print.Char();
        Print.Space();
    }
    
    clearLineBuffer()
    {
        LDX # 63
        loop
        {
            STZ Address.LineBuffer, X
            DEX
            if (MI) { break; }  
            // Continue while X >= 0
        }
    }
    
    Hopper()
    {
        Initialize();  
#ifdef DEBUG        
        //Run(); // tests
#endif
        printWelcome();
        
        // Main command loop
        loop
        {
            clearLineBuffer();
            printPrompt();
            processCommandLine();
            // Errors are handled by Error.CheckAndPrint() within ProcessCommandLine
            if (BBS7, ZP.FLAGS) { break; }
        }
        
    }
}
