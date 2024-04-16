program WozMon
{
    //  The WOZ Monitor for the Apple 1
    //  Written by Steve Wozniak in 1976
    //
    //  Re-imagined in Hopper 6502 Assembly
    //
    // Instructions:
    // https://www.sbprojects.net/projects/apple1/wozmon.php

    // Sample program:
    // A9 20 AA 20 ED FF E8 8A C9 7F D0 F6 4C 00 FF
    
    #define CPU_6502
    //#define CPU_65C02S
    #define ROM_1K

    // Zero Page Variables

    const byte XAML            = 0x24;          //  Last "opened" location Low
    const byte XAMH            = 0x25;          //  Last "opened" location High
    const byte STL             = 0x26;          //  Store address Low
    const byte STH             = 0x27;          //  Store address High
    const byte L               = 0x28;          //  Hex value parsing Low
    const byte H               = 0x29;          //  Hex value parsing High
    const byte YSAV            = 0x2A;          //  Used to see if hex value is given
    const byte MODE            = 0x2B;          //  0x00=XAM, 0x7F=STOR, 0xAE=BLOCK XAM


    // Other Variables

    const uint IN              = 0x0200;         //  Input buffer: 0x0200..0x027F

    // Motorola 6821 PIA as per Apple I
    const uint KBD             = 0xD010;         // PIA.A keyboard input
    const uint KBDCR           = 0xD011;         // PIA.A keyboard control register
    const uint DSP             = 0xD012;         // PIA.B display output register
    const uint DSPCR           = 0xD013;         // PIA.B display control register
   
    Hopper()
    {
        CLD             // clear decimal arithmetic mode.
        CLI             // enable interrupts
                
        LDY # 0x7F      // Mask for DSP data direction register (this value is assumed in Y below, incremented to 0x80)
        
        LDA # 0xA7      // KBD and DSP control register mask.
        STA KBDCR       // Enable interrupts, set CA1, CB1, for
        STA DSPCR       //  positive edge sense/output mode.
        
NOTCR:
        CMP # (0x08 + 0x80)      // Backspace key?
        BEQ BACKSPACE   // Yes.
        CMP # (0x1B + 0x80)      // ESC?
        BEQ ESCAPE      // Yes.
        INY             // Advance text index.
        BPL NEXTCHAR    // Auto ESC if > 127.

ESCAPE:
        LDA # ('\\' + 0x80)      // "\".
        Echo();         // Output it.
GETLINE:
        LDA # (0x0A + 0x80)      // New line
        Echo();         // Output it.
        LDY # 0x01      // Initialize text index.
BACKSPACE:
        DEY             // Back up text index.
        BMI GETLINE     // Beyond start of line, reinitialize.
                
NEXTCHAR:
        loop
        {
            LDA KBDCR           // Key ready?
            if (MI) { break; }  // Loop until ready.
        }
        LDA KBD         // Load character.
        STA IN, Y       // Add to text buffer.
        Echo();         // Display character.
        CMP # (0x0A + 0x80)       // New line
        BNE NOTCR       // No.
        LDY # 0xFF      // Reset text index.
        LDA # 0x00      // For XAM mode.
        TAX             // 0 -> X.
SETSTOR:
        ASL             // Leaves 0x7B if setting STOR mode.
SETMODE:
        STA MODE        // 0x00=XAM 0x74=STOR 0xB8=BLOCK XAM
BLSKIP:
        INY             // Advance text index.
        loop
        {
            // Command Loop:

            LDA IN, Y       // Get character.
            CMP # (0x0A + 0x80)       // <enter> ?
            BEQ GETLINE     // Yes, done this line.
            CMP # ('.' + 0x80)       // "."?
            BCC BLSKIP      // Skip delimiter.
            BEQ SETMODE     // Set BLOCK XAM mode
            CMP # (':' + 0x80)       // ":"?
            BEQ SETSTOR     // Yes. Set STOR mode.
            CMP # ('R' + 0x80)       // "R"?
            BEQ RUN         // Yes. Run user program.
            STX L           // 0x00-> L.
            STX H           // and H.
            STY YSAV        // Save Y for comparison.
            
            loop
            {
                // Rotate the hex characters as values into 'HL':
                
                LDA IN,Y          // Get character for hex test.
                EOR # 0xB0        // Map digits to 0x0-9.
                CMP # 0x0A        // Digit?
                if (C)            // No.
                {
                    ADC # 0x88    // Map letter "A"-"F" to 0xFA-FF.
                    CMP # 0xFA    // Hex letter?
                    if (NC)       // No, character not hex.
                    { 
                        break; 
                    }
                }
                ASL
                ASL             // Hex digit to MSD of A.
                ASL
                ASL
                LDX # 0x04      // Shift count.
                loop
                {    
                    ASL               // Hex digit left, MSB to carry.
                    ROL L             // Rotate into LSD.
                    ROL H             // Rotate into MSD's.
                    DEX               // Done 4 shifts?
                    if (Z)
                    { 
                        break; 
                    }
                }
                INY             // Advance text index.
                                // Check next char for hex.
            }
             
            CPY YSAV        // Check if L, H empty (no hex digits).
            BEQ ESCAPE      // Yes, generate ESC sequence.
            BIT MODE        // Test MODE byte.
            BVC NOTSTOR     //  B6=0 STOR 1 for XAM & BLOCK XAM
            LDA L           // LSD's of hex data.
            STA [STL,X]     // Store at current 'store index'.
            INC STL         // Increment store index.
            if (Z)         
            { 
                INC STH     // Add carry to 'store index' high order.
            }
TONEXTITEM: 
        }    
RUN:            
        JMP [XAML]      // Run at current XAM index.
NOTSTOR:        
        BMI XAMNEXT     // B7=0 for XAM, 1 for BLOCK XAM.
        LDX # 0x02      // Byte count.

        loop
        {  
            LDA L-1,   X      // Copy hex data to
            STA STL-1, X      // 'store index'.
            STA XAML-1,X      // And to 'XAM index'.
            DEX               // Next of 2 bytes.
            if (Z) { break; } // Loop unless X=0.
        }
        
        loop
        {    
            // Data printing loop:
            
            if (Z)              // Z means first or mod 8 so address to print.
            {
                LDA # (0x0A + 0x80)  // Newline
                Echo();         // Output it.
                LDA XAMH        // 'Examine index' high-order byte.
                PrintByte();    // Output it in hex format.
                LDA XAML        // Low-order 'examine index' byte.
                PrintByte();    // Output it in hex format.
                LDA # (':' + 0x80)        // ":".
                Echo();         // Output it.
            }
             
            LDA # (' ' + 0x80)  // Blank.
            Echo();             // Output it.
            LDA [XAML,X]        // Get data byte at 'examine index'.
            PrintByte();        // Output it in hex format.
XAMNEXT:        
            STX MODE        // 0->MODE (XAM mode).
            LDA XAML
            CMP L           // Compare 'examine index' to hex data.
            LDA XAMH
            SBC H
            BCS TONEXTITEM  // Not less, so no more data to output.
            INC XAML
            if (Z)          // Increment 'examine index'.
            {
                INC XAMH
            }
            LDA XAML        // Check low-order 'examine index' byte
            AND # 0x07      // For MOD 8=0
        }
        
    } // Hopper
    
    PrintByte()
    {
        PHA             // Save A for LSD.
        LSR
        LSR
        LSR             // MSD to LSD position.
        LSR
        NOP
        PrintHex();     // Output hex digit.
        PLA             // Restore A.
        PrintHex();
    }
    PrintHex()
    {
        AND # 0x0F      // Mask LSD for hex print.
        ORA # '0'       // Add "0".
        CMP # 0x3A      // Digit?
        if (C)
        {
            ADC # 0x06      // Add offset for letter.
        }
        Echo();
    }
    Echo()
    {
        loop
        {
            BIT DSP             // DA bit (B7) cleared yet?
            if (PL) { break; }  // No, wait for display.
        }
        STA DSP         // Output character. Sets DA.          
    }
}
