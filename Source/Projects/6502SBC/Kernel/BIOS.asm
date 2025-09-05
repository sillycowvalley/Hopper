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
    /*
    Run()
    {
        Tests.RunTests();
        
        // End marker
        LDA #'!'
        Serial.WriteChar();
        NewLine();
        
        loop { }
    }
    */
    
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
    
    Hopper()
    {
        Initialize();  
        
        LDX #SysCall.MemAvailable
        SystemCallDispatcher();
        Shared.MoveAccToTop();
        Long.Print();
        loop { }
    }
}
