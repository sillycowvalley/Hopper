unit Screen
{
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Serial"
    uses "System/Long"
    
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
    const string reset = "\x1B[0m";
    const string boldOn  = "\x1B[1m";
    const string boldOff = "\x1B[22m";
    const string dim = "\x1B[2m";
    const string underline = "\x1B[4m";
    const string blink = "\x1B[5m";
    const string inverseOn = "\x1B[7m";
    const string inverseOff = "\x1B[27m";
    const string normal = "\x1B[0m";
    
    // Clear screen and home cursor
    Clear()
    {
        LDA #(clearScreen % 256)
        STA ZP.STRL
        LDA #(clearScreen / 256)
        STA ZP.STRH
        Print.String();
        Home();
    }
    
    // Move cursor to home position (0,0)
    Home()
    {
        LDA #(home % 256)
        STA ZP.STRL
        LDA #(home / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Clear from cursor to end of line
    ClearToEOL()
    {
        LDA #(clearEOL % 256)
        STA ZP.STRL
        LDA #(clearEOL / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Clear entire current line
    ClearLine()
    {
        LDA #(clearLine % 256)
        STA ZP.STRL
        LDA #(clearLine / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Clear from cursor to end of screen
    ClearToEOS()
    {
        LDA #(clearToEOS % 256)
        STA ZP.STRL
        LDA #(clearToEOS / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Hide cursor
    HideCursor()
    {
        LDA #(hideCursor % 256)
        STA ZP.STRL
        LDA #(hideCursor / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Show cursor
    ShowCursor()
    {
        LDA #(showCursor % 256)
        STA ZP.STRL
        LDA #(showCursor / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Save cursor position
    SaveCursor()
    {
        LDA #(saveCursor % 256)
        STA ZP.STRL
        LDA #(saveCursor / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Restore saved cursor position
    RestoreCursor()
    {
        LDA #(restoreCursor % 256)
        STA ZP.STRL
        LDA #(restoreCursor / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Reset all attributes to normal
    Reset()
    {
        LDA #(reset % 256)
        STA ZP.STRL
        LDA #(reset / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Enable bold/bright text
    Bold()
    {
        LDA #(boldOn % 256)
        STA ZP.STRL
        LDA #(boldOn / 256)
        STA ZP.STRH
        Print.String();
    }
    BoldOff()
    {
        LDA #(boldOff % 256)
        STA ZP.STRL
        LDA #(boldOff / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Enable dim text
    Dim()
    {
        LDA #(dim % 256)
        STA ZP.STRL
        LDA #(dim / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Enable underline
    Underline()
    {
        LDA #(underline % 256)
        STA ZP.STRL
        LDA #(underline / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Enable blink
    Blink()
    {
        LDA #(blink % 256)
        STA ZP.STRL
        LDA #(blink / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Enable inverse video
    Inverse()
    {
        LDA #(inverseOn % 256)
        STA ZP.STRL
        LDA #(inverseOn / 256)
        STA ZP.STRH
        Print.String();
    }
    InverseOff()
    {
        LDA #(inverseOff % 256)
        STA ZP.STRL
        LDA #(inverseOff / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Return to normal text
    Normal()
    {
        LDA #(normal % 256)
        STA ZP.STRL
        LDA #(normal / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Helper: send ESC[ sequence (preserves A internally)
    sendEscape()
    {
        PHA
        LDA #0x1B
        Serial.WriteChar();
        LDA #'['
        Serial.WriteChar();
        PLA
    }
    
    // Helper: send decimal number
    sendDecimal() // Input: A = value (0-99)
    {
        Shared.LoadTopByte(); // A -> TOP
        Long.Print();
    }
    
    // Position cursor at col, row (0-based input, converts to 1-based for VT100)
    GotoXY() // Input: A = col (0-79), Y = row (0-23)
    {
        PHA
        
        sendEscape();
        
        // Send row+1 (VT100 is 1-based)
        INY
        TYA
        sendDecimal();
        
        LDA #';'
        Serial.WriteChar();
        
        // Send column+1
        PLA
        INC A
        sendDecimal();
        
        LDA #'H'
        Serial.WriteChar();
    }
    
    // Input: char in A
    Char()
    {
        Serial.WriteChar();
    }
    
    // Set foreground color
    Foreground() // Input: A = color (Color enum value 0-7)
    {
        sendEscape();
        CLC
        ADC #30  // Foreground colors are 30-37
        sendDecimal();
        LDA #'m'
        Serial.WriteChar();
    }
    
    // Set background color
    Background() // Input: A = color (Color enum value 0-7)
    {
        sendEscape();
        CLC
        ADC #40  // Background colors are 40-47
        sendDecimal();
        LDA #'m'
        Serial.WriteChar();
    }
    
    // Move cursor up
    Up() // Input: A = lines to move (0 = 1 line)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();
        sendDecimal();
        LDA #'A'
        Serial.WriteChar();
    }
    
    // Move cursor down
    Down() // Input: A = lines to move (0 = 1 line)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();
        sendDecimal();
        LDA #'B'
        Serial.WriteChar();
    }
    
    // Move cursor right
    Right() // Input: A = columns to move (0 = 1 column)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();
        sendDecimal();
        LDA #'C'
        Serial.WriteChar();
    }
    
    // Move cursor left
    Left() // Input: A = columns to move (0 = 1 column)
    {
        if (Z)
        {
            INC A
        }
        sendEscape();
        sendDecimal();
        LDA #'D'
        Serial.WriteChar();
    }
    
    // Set both foreground and background colors
    SetColors() // Input: A = foreground, Y = background
    {
        PHA
        PHY
        
        // Set foreground
        PLA
        PHA
        Foreground();
        
        // Set background
        PLY
        PHY
        TYA
        Background();
        
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
            LDA #'-'
            Serial.WriteChar();
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
            LDA #'|'
            Serial.WriteChar();
            LDA #1
            Left();
            LDA #1
            Down();
            DEX
            if (Z) { break; }
        }
    }
}
