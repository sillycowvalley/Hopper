program HelloMECB
{
    //*****************************************************************************
    //
    //	A simple 'Hello World' character output test.
    //	For 6502 based system.
    //	For ROM target (See Vector Table).
    //	eg. $F000 - $FFFF
    //
    //	Author: Greg
    //	Date:	03/06/2024
    //
    //*****************************************************************************
    
    #define CPU_65C02S
    #define ROM_4K // EntryPoint = 0xF000
    
    const uint ACIA    = 0xE008;   // MC6850 ACIA Address
    const uint ACIAtr  = (ACIA+1); // ACIA Transmit / Receive Data Register
    
    //const byte ACIA    = 0xEC;     // MC6850 ACIA Address
    //const byte ACIAtr  = (ACIA+1); // ACIA Transmit / Receive Data Register
    
    const string Hello = "\nHello 6502 World!\n";
    
    IRQ()
    {
    }
    NMI()
    {
    }
    Hopper()
    {
        // Initialise 6502
        SEI        // Disable Interrupts
        CLD        // Clear Decimal flag (Binary mode)
        LDX # 0xFF // Initialise Stack pointer ($01FF)
        TXS
        
        // Initialise ACIA
        LDA # 0x03        
        STA ACIA           // Reset ACIA
        //LDA # 0b01010001 // Receive interrupt disabled, Transmit interrupt disabled, 8 bits, 2 stop bits, no parity, /16 clock,
        LDA # 0b00010101   // Receive interrupt disabled, Transmit interrupt disabled, 8 bits, 1 stop bit, no parity, /16 clock
        STA ACIA          
        
        // Output Hello string
        LDX # 0x00        // Initialise character offset pointer
        loop
        {
            LDA #0x02            // Transmit Data Register Empty flag mask
            BIT ACIA             // Is Transmit Data Register Empty?
            if (Z) { continue; } // Loop if not empty
        
            LDA Hello, X         // Get next character to send
            if (Z) { break;    } // If it's the zero string terminator, we're done!
            STA ACIAtr           //Send the character
            INX                  // Increment character offset pointer
                                 // Loop to process next character
        }
        loop {}                  // Done, so just Loop Forever!
    }
}
