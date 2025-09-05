program BIOS
{
    #define HOPPER_BIOS
    
    //#define DEBUG
    #define CPU_65C02S
    
#ifdef DEBUG    
    #define ROM_32K
#else
    //#define ROM_16K
    #define ROM_8K
#endif
    
    
    // Optional components
    #define HASFLOAT
    #define HASEEPROM
    
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
    uses "TestSuite/TestFloat"
    
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
        
        // Process Intel HEX lines
        loop
        {
            processIHexLine();
            if (NC) { break; }  // EOF record found
        }
        
        // Close file
        LDA # 0x80 // high bit for executable file
        File.EndSave();
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
        
        // Skip address (4 hex chars = 2 bytes)
        Serial.HexIn();  // Address high (ignore)
        Serial.HexIn();  // Address low (ignore)
        
        // Get record type
        Serial.HexIn();
        CMP #0x01        // EOF record?
        if (Z)   
        {
            Serial.HexIn(); // chomp the FF
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
            Print.Hex();
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
        Print.NewLine();
        
        SEC              // Continue processing
    }
    
    parseAndExecute()
    {
        Error.FindKeyword(); // X already points to command start
        if (NZ)
        {
            // Execute command based on token
            switch (A)
            {
                case ErrorWord.FORMAT: { cmdFormat(); return; }
                case ErrorWord.MEM:    { cmdMem();    return; }
                case ErrorWord.DIR:    { cmdDir();    return; }
                case ErrorWord.CLS:    { cmdCls();    return; }
                case ErrorWord.HEX:    { cmdHex();    return; }
                case ErrorWord.DEL:    { cmdDel();    return; }
                
                // Keyword from wrong table to cause system calls to be included in the build
                // so that the optimizer doesn't remove it.
                case ErrorWord.HEAP:   { SystemCallDispatcher(); return; } 
            }
        }
        LDA # ErrorID.InvalidCommand
        LDX # MessageExtras.SuffixSpace
        Error.MessageNL();
    }
    
    // Parse filename from command line after the command keyword
    // Input: X = position in LineBuffer after command word  
    // Output: C set if successful, NC if no filename found
    //         ZP.STR points to filename in LineBuffer
    parseFilename()
    {
        // Skip spaces after command
        loop
        {
            LDA Address.LineBuffer, X
            CMP #' '
            if (NZ) { break; }
            INX
        }
        
        // Check if we have a filename
        if (Z)
        {
            Error.FilenameExpected();
              // No filename found
            CLC return;
        }
        
        // Point ZP.STR to filename start in LineBuffer
        TXA
        CLC
        ADC # (Address.LineBuffer % 256)
        STA ZP.STRL
        LDA # (Address.LineBuffer/ 256)
        ADC #0
        STA ZP.STRH
        
        // Find end of filename and null-terminate
        loop
        {
            LDA Address.LineBuffer, X
            if (Z) { break; }    // Already null
            CMP #' '
            if (Z) 
            { 
                STZ Address.LineBuffer, X  // Null-terminate at space
                break; 
            }
            INX
        }
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
        LDY #0
        loop
        {
            Serial.WaitForChar();         // Get character
            CMP # Char.EOL                // Enter?
            if (Z) 
            { 
                break;
            }
            CMP # Char.Backspace          // Backspace?
            if (Z)
            {
                CPY #0
                if (NZ) 
                { 
                    DEY                   Serial.WriteChar();  // Echo backspace
                    LDA #' '              Serial.WriteChar();  // Space
                    LDA # Char.Backspace  Serial.WriteChar();  // Backspace again
                }
                continue;
            }
            // Skip leading spaces
            CMP #' '
            if (Z)
            {
                CPY #0
                if (Z) { continue; }    // Ignore space at start of line
            }
            
            Serial.WriteChar();        // Echo character
            STA LineBuffer, Y
            INY
            CPY #63                    // Prevent overflow
            if (Z) { break; }
        } // loop
        Print.NewLine();
        LDA #' '
        STA Address.LineBuffer, Y      // Space terminate
        TYA                            // Return length
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
        Run(); // tests
#endif
        printWelcome();
        
        // Main command loop
        loop
        {
            clearLineBuffer();
            printPrompt();
            processCommandLine();
            // Errors are handled by Error.CheckAndPrint() within ProcessCommandLine
        }
        
    }
}
