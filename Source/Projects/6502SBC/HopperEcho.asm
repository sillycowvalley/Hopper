program HopperEcho
{
    #define CPU_65C02S
    #define ROM_4K
    
    uses "/Source/Runtime/6502/Serial"
    
    const string Echo = "\nHopper Echo:\n";
    
    IRQ()
    {
        Serial.ISR();
    }
    NMI()
    {
    }
    
    Hopper()
    {
        SEI
        
        Serial.Initialize();
        
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
