program Dot
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Serial"
    uses "System/Long"
    
    // Screen dimensions
    const byte screenWidth = 80;
    const byte screenHeight = 24;
    
    // Special key codes  
    const byte keyUp = 128;
    const byte keyDown = 129;
    const byte keyRight = 130;
    const byte keyLeft = 131;
    const byte keyEsc = 0x1B;
    
    // Our own zero page variables (not used by BIOS)
    const byte curX = 0x58;      // Current X position
    const byte curY = 0x59;      // Current Y position  
    const byte escState = 0x5A;  // 0=normal, 1=got ESC, 2=got ESC[
    const byte lastKey = 0x5B;   // Store last key read
    
    // VT100 escape sequence strings - can't use \x in Hopper Assembly
    // Build them with byte arrays instead
    const byte[] clearScreenBytes = { 0x1B, '[', '2', 'J', 0x1B, '[', 'H', 0 };
    const byte[] hideCursorBytes = { 0x1B, '[', '?', '2', '5', 'l', 0 };
    const byte[] showCursorBytes = { 0x1B, '[', '?', '2', '5', 'h', 0 };
    const string goodbyeMsg = "Goodbye!\n";
    
    // Send ESC character
    sendEsc()
    {
        LDA #keyEsc
        Print.Char();
    }
    
    // Clear screen and home cursor
    clearScreen()
    {
        LDA #(clearScreenBytes % 256)
        STA ZP.STRL
        LDA #(clearScreenBytes / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Hide cursor 
    hideCursor()
    {
        LDA #(hideCursorBytes % 256)
        STA ZP.STRL
        LDA #(hideCursorBytes / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Show cursor
    showCursor()
    {
        LDA #(showCursorBytes % 256)
        STA ZP.STRL
        LDA #(showCursorBytes / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Move cursor to X,Y position (1-based for VT100)
    gotoXY()  // X in curX, Y in curY
    {
        // Send ESC[
        sendEsc();
        LDA #'['
        Print.Char();
        
        // Send Y coordinate using Long.Print
        LDA curY
        STA ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
        
        // Send semicolon
        LDA #';'
        Print.Char();
        
        // Send X coordinate using Long.Print
        LDA curX
        STA ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
        
        // Send H
        LDA #'H'
        Print.Char();
    }
    
    // Get key with VT100 escape sequence interpretation
    getKey()  // Returns key in A
    {
        loop
        {
            // Check if we're in an escape sequence
            LDA escState
            if (NZ)
            {
                // We're processing an escape sequence
                CMP #1
                if (Z)
                {
                    // Got ESC, waiting for [
                    Serial.WaitForChar();
                    CMP #'['
                    if (Z)
                    {
                        LDA #2
                        STA escState
                        continue;  // Get next char
                    }
                    else
                    {
                        // Not a bracket, reset and return ESC
                        STZ escState
                        LDA #keyEsc
                        return;
                    }
                }
                
                // escState = 2, got ESC[, waiting for direction
                Serial.WaitForChar();
                PHA  // Save the character
                
                // Reset state
                STZ escState
                
                // Check arrow key codes using switch
                PLA
                switch (A)
                {
                    case 'A':
                    {
                        LDA #keyUp
                        return;
                    }
                    case 'B':
                    {
                        LDA #keyDown
                        return;
                    }
                    case 'C':
                    {
                        LDA #keyRight
                        return;
                    }
                    case 'D':
                    {
                        LDA #keyLeft
                        return;
                    }
                    default:
                    {
                        // Unknown sequence, return the char
                        return;
                    }
                }
            }
            
            // Normal character processing
            Serial.WaitForChar();
            CMP #keyEsc
            if (Z)
            {
                // Start of escape sequence
                LDA #1
                STA escState
                continue;  // Process next char
            }
            
            // Regular character
            return;
        }
    }
    
    // Move the star to a new position
    moveStar()  // New position in curX, curY
    {
        // Draw star at current position
        gotoXY();
        LDA #'*'
        Print.Char();
    }
    
    // Clear current position
    clearPosition()
    {
        gotoXY();
        LDA #' '
        Print.Char();
    }
    
    Hopper()
    {
        // Initialize escape state
        STZ escState
        
        // Initialize position to center of screen
        LDA #40
        STA curX
        LDA #12
        STA curY
        
        // Setup screen - clear first, then hide cursor
        clearScreen();
        hideCursor();
        
        // Draw initial star
        moveStar();
        
        // Main loop
        loop
        {
            getKey();
            STA lastKey  // Store key for multiple comparisons
            
            // Check for ESC to exit
            CMP #keyEsc
            if (Z) 
            { 
                break; 
            }
            
            // Clear old position before moving
            clearPosition();
            
            // Process arrow keys using switch
            LDA lastKey  // Load key for switch
            switch (A)
            {
                case 128:
                {
                    LDA curY
                    CMP #2  // Minimum Y is 1, but check against 2 for decrement
                    if (NC)
                    {
                        DEC curY
                    }
                }
                case 129:
                {
                    LDA curY
                    CMP #screenHeight
                    if (NZ)  // Not at bottom
                    {
                        INC curY
                    }
                }
                case 131:
                {
                    LDA curX
                    CMP #2  // Minimum X is 1, but check against 2 for decrement
                    if (NC)
                    {
                        DEC curX
                    }
                }
                case 130:
                {
                    LDA curX
                    CMP #screenWidth
                    if (NZ)  // Not at right edge
                    {
                        INC curX
                    }
                }
                default:
                {
                    // Other keys - do nothing
                }
            }
            
            // Draw star at new position
            moveStar();
            
            // Check for break
            Serial.IsBreak();
            if (C) { break; }
        }
        
        // Clean up
        showCursor();
        clearScreen();
        
        LDA #(goodbyeMsg % 256)
        STA ZP.STRL
        LDA #(goodbyeMsg / 256)
        STA ZP.STRH
        Print.String();
    }
}
