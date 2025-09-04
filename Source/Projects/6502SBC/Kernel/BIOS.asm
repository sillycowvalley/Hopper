program BIOS
{
    #define CPU_65C02S
    #define ROM_8K
    
    #define DEBUG
    
    // Optional components
    //#define HASFLOAT
    //#define HASEEPROM
    
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
        
#if defined(HASHEAP)
        Memory.Initialize();
#endif
        
        CLI  // Re-enable interrupts
    }
    
    const string Echo = "\nEcho:\n";
    
    Hopper()
    {
        Initialize();
        
        LDX # 0x00        // Initialise character offset pointer
        loop
        {
            LDA Echo, X         // Get next character to send
            if (Z) { break;    } // If it's the zero string terminator, we're done!
            Serial.WriteChar();
            INX                  // Increment character offset pointer
                                 // Loop to process next character
        }
        loop
        {
            Serial.WaitForChar();
            Serial.WriteChar();
        }
    }
}
