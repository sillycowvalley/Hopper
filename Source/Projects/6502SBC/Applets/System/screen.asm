unit Screen
{
    uses "System/Definitions"
    uses "System/Serial"
    
    // Zero page allocation - shares block with Debug
    const byte screenSlots       = 0x67;
    const byte screenStrL        = screenSlots+0;  // Temporary string pointer low
    const byte screenStrH        = screenSlots+1;  // Temporary string pointer high
    const byte currentAttributes = screenSlots+2;
    const byte workingAttributes = screenSlots+3;
    const byte workingStatus     = screenSlots+4;
    const byte workingColour     = screenSlots+5;
    
    // ANSI/VT100 color codes
    enum Color
    {
        Black   = 0,
        Red     = 1,
        Green   = 2,
        Yellow  = 3,
        Blue    = 4,
        Magenta = 5,
        Cyan    = 6,
        White   = 7,
    }
    
    // VT100 control sequences using \x1B notation
    const string clearScreen = "\x1B[2J";
    const string home = "\x1B[H";
    const string clearEOL = "\x1B[K";
    const string clearLine = "\x1B[2K";
    const string clearToEOS = "\x1B[J";
    const string hideCursor = "\x1B[?25l";
    const string showCursor = "\x1B[?25h";
    const string saveCursor = "\x1B[s";
    const string restoreCursor = "\x1B[u";
    const string reset = "\x1B[0;32;40m"; // ensure green on black default
    const string boldOn  = "\x1B[1m";
    const string boldOff = "\x1B[22m";
    const string dim = "\x1B[2m";
    const string underline = "\x1B[4m";
    const string blink = "\x1B[5m";
    const string inverseOn = "\x1B[7m";
    const string inverseOff = "\x1B[27m";
    const string normal = "\x1B[0m";
    const string escape = "\x1B[";
    
    // Private helper to output a string without using ZP.STR
    printString()  // Input: A = low byte, Y = high byte of string address
    {
        PHY
        
        // Store string pointer in our zero page slots
        STA screenStrL
        STY screenStrH
        
        LDY #0
        loop
        {
            LDA [screenStrL], Y
            if (Z) { break; }  // Null terminator
            Serial.WriteChar();// munts X
            INY
        }
        
        PLY
    }
    
    // Clear screen and home cursor
    Clear()
    {
        PHY
        LDA #(clearScreen % 256)
        LDY #(clearScreen / 256)
        printString();
        PLY
        Home();
    }
    
    // Move cursor to home position (0,0)
    Home()
    {
        PHY
        LDA #(home % 256)
        LDY #(home / 256)
        printString();
        PLY
    }
    
    // Clear from cursor to end of line
    ClearToEOL()
    {
        PHY
        LDA #(clearEOL % 256)
        LDY #(clearEOL / 256)
        printString();
        PLY
    }
    
    // Clear entire current line
    ClearLine()
    {
        PHY
        LDA #(clearLine % 256)
        LDY #(clearLine / 256)
        printString();
        PLY
    }
    
    // Clear from cursor to end of screen
    ClearToEOS()
    {
        PHY
        LDA #(clearToEOS % 256)
        LDY #(clearToEOS / 256)
        printString();
        PLY
    }
    
    // Hide cursor
    HideCursor()
    {
        PHY
        LDA #(hideCursor % 256)
        LDY #(hideCursor / 256)
        printString();
        PLY
    }
    
    // Show cursor
    ShowCursor()
    {
        PHY
        LDA #(showCursor % 256)
        LDY #(showCursor / 256)
        printString();
        PLY
    }
    
    // Save cursor position
    SaveCursor()
    {
        PHY
        LDA #(saveCursor % 256)
        LDY #(saveCursor / 256)
        printString();
        PLY
    }
    
    // Restore saved cursor position
    RestoreCursor()
    {
        PHY
        LDA #(restoreCursor % 256)
        LDY #(restoreCursor / 256)
        printString();
        PLY
    }
    
    // Reset all attributes to normal
    Reset()
    {
        PHY
        LDA #(reset % 256)
        LDY #(reset / 256)
        printString();
        LDA # 0b00000111
        STA currentAttributes
        PLY
    }
    
    // Attributes are:
    // Bit 0-2 : foreground colour
    // Bit 3-5 : background colour
    // Bit 6   : bold
    // Bit 7   : inverse
    
    // A = packed attributes:
    //     - from  ScreenBuffer.Update() 
    //     - after a Screen.Reset()  (currentAttributes = 0b00000111)
    //
    SetAttributes()
    {
        CMP currentAttributes
        if (Z) { return; } // no change
        
        STA workingAttributes
        
        // "ESC["
        LDA #(escape % 256)
        LDY #(escape / 256)
        printString();
        
        RMB0 workingStatus
        
        // BOLD
        LDA currentAttributes
        AND # Attribute.Bold
        if (Z)
        {
            // currently not bold
            LDA workingAttributes
            AND # Attribute.Bold
            if (Z)
            {
                // still not bold
            }
            else
            {
                // switch to bold "1"
                LDA #'1'
                Serial.WriteChar();
                LDA currentAttributes
                ORA # Attribute.Bold
                STA currentAttributes
                SMB0 workingStatus
            }
        }
        else
        {
            // currently bold
            LDA workingAttributes
            AND # Attribute.Bold
            if (Z)
            {
                // switch to not bold: "22"
                LDA #'2'
                Serial.WriteChar();
                Serial.WriteChar();
                LDA currentAttributes
                EOR # Attribute.Bold
                STA currentAttributes
                SMB0 workingStatus
            }
            else
            {
                // still bold
            }
        }
        
        // INVERSE
        LDA currentAttributes
        AND # Attribute.Inverse
        if (Z)
        {
            // currently not bold
            LDA workingAttributes
            AND # Attribute.Inverse
            if (Z)
            {
                // still not inverse
            }
            else
            {
                if (BBS0, workingStatus)
                {
                    LDA #';'
                    Serial.WriteChar();
                }
                // switch to inverse "7"
                LDA #'7'
                Serial.WriteChar();
                LDA currentAttributes
                ORA # Attribute.Inverse
                STA currentAttributes
                SMB0 workingStatus
            }
        }
        else
        {
            // currently inverse
            LDA workingAttributes
            AND # Attribute.Inverse
            if (Z)
            {
                if (BBS0, workingStatus)
                {
                    LDA #';'
                    Serial.WriteChar();
                }
                // switch to not inverse: "27"
                LDA #'2'
                Serial.WriteChar();
                LDA #'7'
                Serial.WriteChar();
                LDA currentAttributes
                EOR # Attribute.Inverse
                STA currentAttributes
                SMB0 workingStatus
            }
            else
            {
                // still inverse
            }
        }
        
        // FOREGROUND
        LDA workingAttributes
        AND # 0b00000111
        STA workingColour
        
        LDA currentAttributes
        AND # 0b00000111
        CMP workingColour
        if (NZ)
        {
            // foreground has changed
            if (BBS0, workingStatus)
            {
                LDA #';'
                Serial.WriteChar();
            }
            LDA currentAttributes
            EOR # 0b00000111
            ORA workingColour
            STA currentAttributes
            LDA #'3'
            Serial.WriteChar();
            CLC
            LDA workingColour   
            ADC #'0'
            Serial.WriteChar();
            SMB0 workingStatus
        }
        
        // BACKGROUND
        LDA workingAttributes
        AND # 0b00111000
        STA workingColour
        
        LDA currentAttributes
        AND # 0b00111000
        CMP workingColour
        if (NZ)
        {
            // background has changed
            if (BBS0, workingStatus)
            {
                LDA #';'
                Serial.WriteChar();
            }
            LDA currentAttributes
            EOR # 0b00111000
            ORA workingColour
            STA currentAttributes
            LDA #'4'
            Serial.WriteChar();
                
            LDA workingColour
            LSR A LSR A LSR A
            CLC
            ADC #'0'
            Serial.WriteChar();
        }
                
        LDA #'m'
        Serial.WriteChar();
    }
    
    // Enable bold/bright text
    Bold()
    {
        PHY
        LDA #(boldOn % 256)
        LDY #(boldOn / 256)
        printString();
        PLY
    }
    
    BoldOff()
    {
        PHY
        LDA #(boldOff % 256)
        LDY #(boldOff / 256)
        printString();
        PLY
    }
    
    // Enable dim text
    Dim()
    {
        PHY
        LDA #(dim % 256)
        LDY #(dim / 256)
        printString();
        PLY
    }
    
    // Enable underline
    Underline()
    {
        PHY
        LDA #(underline % 256)
        LDY #(underline / 256)
        printString();
        PLY
    }
    
    // Enable blink
    Blink()
    {
        PHY
        LDA #(blink % 256)
        LDY #(blink / 256)
        printString();
        PLY
    }
    
    // Enable inverse video
    Inverse()
    {
        PHY
        LDA #(inverseOn % 256)
        LDY #(inverseOn / 256)
        printString();
        PLY
    }
    
    InverseOff()
    {
        PHY
        LDA #(inverseOff % 256)
        LDY #(inverseOff / 256)
        printString();
        PLY
    }
    
    // Return to normal text
    Normal()
    {
        PHY
        LDA #(normal % 256)
        LDY #(normal / 256)
        printString();
        PLY
    }
    
    // Helper: send ESC[ sequence (preserves A internally)
    sendEscape()
    {
        PHA
        LDA #0x1B
        Serial.WriteChar();// munts X
        LDA #'['
        Serial.WriteChar();// munts X
        PLA
    }
    
    // Helper: send decimal number
    sendDecimal() // Input: A = value (0-99)
    {
        PHX
        PHY
        
        LDX #'0'            // X will count tens in ASCII
        loop
        {
            CMP #10
            if (NC) { break; }  // if < 10, done
            SBC #10             // Subtract 10 (carry already set from CMP)
            INX                 // Increment tens counter
        }
        CPX #'0'               // Check if tens is zero
        if (NZ)                // Only print tens if non-zero
        {
            PHA
            TXA
            Serial.WriteChar();// munts X
            PLA
        }
        ORA #'0'
        Serial.WriteChar();// munts X
        
        PLY
        PLX
    }
    
    // Position cursor at col, row (0-based input, converts to 1-based for VT100)
    GotoXY() // Input: A = col (0-99), Y = row (0-23)
    {
        PHA
        
        sendEscape();// munts X
        
        // Send row+1 (VT100 is 1-based)
        INY
        TYA
        sendDecimal();// munts X
        
        LDA #';'
        Serial.WriteChar();// munts X
        
        // Send column+1
        PLA
        INC A
        sendDecimal();// munts X
        
        LDA #'H'
        Serial.WriteChar();// munts X
    }
    
    // Input: char in A
    Char()
    {
        Serial.WriteChar();// munts X
    }
    
    // Set foreground color
    Foreground() // Input: A = color (Color enum value 0-7)
    {
        sendEscape();// munts X
        CLC
        ADC #30  // Foreground colors are 30-37
        sendDecimal();// munts X
        LDA #'m'
        Serial.WriteChar();// munts X
    }
    
    // Set background color
    Background() // Input: A = color (Color enum value 0-7)
    {
        sendEscape();// munts X
        CLC
        ADC #40  // Background colors are 40-47
        sendDecimal();// munts X
        LDA #'m'
        Serial.WriteChar();// munts X
    }
    
    // Move cursor up
    Up() // Input: A = lines to move (0 = 1 line)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();// munts X
        sendDecimal();// munts X
        LDA #'A'
        Serial.WriteChar();// munts X
    }
    
    // Move cursor down
    Down() // Input: A = lines to move (0 = 1 line)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();// munts X
        sendDecimal();// munts X
        LDA #'B'
        Serial.WriteChar();// munts X
    }
    
    // Move cursor right
    Right() // Input: A = columns to move (0 = 1 column)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();// munts X
        sendDecimal();// munts X
        LDA #'C'
        Serial.WriteChar();// munts X
    }
    
    // Move cursor left
    Left() // Input: A = columns to move (0 = 1 column)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();// munts X
        sendDecimal();// munts X
        LDA #'D'
        Serial.WriteChar();// munts X
    }
    
    // Set both foreground and background colors
    SetColors() // Input: A = foreground, Y = background
    {
        PHA
        PHY
        
        // Set foreground
        PLA
        PHA
        Foreground();// munts X
        
        // Set background
        PLY
        PHY
        TYA
        Background();// munts X
        
        PLY
        PLA
    }
    
    // Draw a horizontal line
    DrawHLine() // Input: A = length
    {
        TAX
        if (Z) { return; }
        loop
        {
            PHX
            LDA #'-'
            Serial.WriteChar();// munts X
            PLX
            DEX
            if (Z) { break; }
        }
    }
    
    // Draw a vertical line (moves cursor)
    DrawVLine() // Input: A = height
    {
        TAX
        if (Z) { return; }
        loop
        {
            PHX
            LDA #'|'
            Serial.WriteChar();// munts X
            LDA #1
            Left(); // munts X
            LDA #1
            Down(); // munts X
            PLX
            DEX
            if (Z) { break; }
        }
    }
}
