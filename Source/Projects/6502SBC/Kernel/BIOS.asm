program BIOS
{
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
    
    uses "Print"
    uses "Shared"
    
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
                STZ 0x00, X
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
        SysCalls.Initialize();
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
    
    cmdMem()
    {
        // Print result using existing Print routines or Error messages
        LDA # ErrorID.MemoryMessage 
        LDX # (MessageExtras.SuffixSpace|MessageExtras.SuffixColon)
        Error.Message();
        
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
                
                // keyword from wrong table to cause system calls to be included in the build
                case ErrorWord.HEAP:   { SystemCallDispatcher(); return; } 
            }
        }
        LDA # ErrorID.InvalidCommand
        LDX # MessageExtras.SuffixSpace
        Error.MessageNL();
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
    
    Hopper()
    {
        Initialize();  
        
        //Run(); // tests
        
        printWelcome();
        
        // Main command loop
        loop
        {
            printPrompt();
            processCommandLine();
            // Errors are handled by Error.CheckAndPrint() within ProcessCommandLine
        }
    }
}
