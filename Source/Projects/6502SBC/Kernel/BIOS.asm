program BIOS
{
    #define CPU_65C02S
    #define ROM_8K
    
    uses "Definitions/Limits"
    uses "Definitions/MemoryMap"
    uses "Definitions/ZeroPage"
    uses "Serial"
    uses "Parallel"
    
    // Interrupt handlers
    IRQ()
    {
        Serial.ISR();
        Parallel.ISR();
    }
    
    NMI()
    {
        // Hardware break - NMI -> <ctrl><C>
        SMB0 ZP.SerialFlags
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
        
        
        CLI  // Re-enable interrupts
    }
    
    Hopper()
    {
        
    }
}
