program BIOS
{
    #define CPU_65C02S
    #define ROM_8K
    
    //#define DEBUG
    
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
        
        // Initialize communication first
        Serial.Initialize();
        Parallel.Initialize();
        Memory.Initialize();

#ifdef HASEEPROM
        EEPROM.Initialize();
#endif
        SysCalls.Initialize();
        CLI  // Re-enable interrupts
    }
    
    cmdFormat()
    {
        // Confirm destructive action using Error system
        LDA # ErrorID.FormatWarning 
        LDX # MessageExtras.SuffixSpace
        confirmYesNo();               // Simple Y/N reader
        if (NC) { return; }           // User cancelled
        
        File.Initialize();            // This sets ZP.LastError on failure
    }
    
    cmdMem()
    {
        Heap.Available();             // Returns available bytes in ZP.TOP
        // Print result using existing Print routines or Error messages
        LDA # ErrorID.MemoryAvailable 
        LDX # MessageExtras.SuffixSpace
        Error.Message();
        Long.Print();              // Print ZP.TOP value
        // TODO : " BYTES"
        Print.NewLine();
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
            // TODO : make uppercase
            CMP #'Y'
            if (Z) { SEC; return; }   // Yes
            CMP #'N'  
            if (Z) { CLC; return; }   // No
            
            // Invalid - try again
            LDX # (MessageExtras.SuffixQuest|MessageExtras.SuffixSpace)
        }
    }
    
    parseAndExecute()
    {
        // Skip leading spaces
        LDY #0
        loop
        {
            LDA LineBuffer, Y
            if (Z) { return; }          // Empty line
            CMP #' '
            if (NZ) { break; }          // Found non-space
            INY
        }
        
        // Point ZP.STR to command start
        TYA
        CLC
        ADC #(LineBuffer % 256)
        STA ZP.STRL
        LDA #(LineBuffer / 256)
        ADC #0
        STA ZP.STRH
        
        FindKeyword();                  // Returns token in A
        if (Z)
        {
            LDA #ErrorID.InvalidSystemCall  // Reuse existing error
            LDX #MessageExtras.SuffixSpace
            Error.Message();
            return;
        }
        
        // Execute command based on token
        switch (A)
        {
            case ErrorWord.FORMAT: { cmdFormat(); }
            case ErrorWord.MEM:    { cmdMem(); }
            case ErrorWord.DIR:    { cmdDir(); }
        }
    }
    
    Hopper()
    {
        Initialize();  
        
        //Run();
        
        LDX #SysCall.MemAvailable
        SystemCallDispatcher();
        Shared.MoveAccToTop();
        Long.Print();
        loop { }
    }
}
