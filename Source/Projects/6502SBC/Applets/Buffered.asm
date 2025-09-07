program BlueFill
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Serial"
    uses "System/Screen"
    uses "System/ScreenBuffer"
    
    const string starting = "Initializing 40x30 buffer...\n";
    const string failed = "FAIL - Could not allocate memory\n";
    const string ok = "OK\n";
    const string hello = "Hello";
    const string blank = "            ";
    
    const byte colPos   = 0x80;
    const byte rowPos   = 0x81;
    const byte escState = 0x82;  // 0=normal, 1=got ESC, 2=got ESC[
    
    // Special key codes  
    const byte keyUp = 128;
    const byte keyDown = 129;
    const byte keyRight = 130;
    const byte keyLeft = 131;
    const byte keyEsc = 0x1B;
    
    
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
    
    Hopper()
    {
        STZ escState
        
        // Clear screen from upload junk
        Screen.Clear();
    
        // Show we're starting
        LDA #(starting % 256)
        STA ZP.STRL
        LDA #(starting / 256)
        STA ZP.STRH
        Print.String();
        
        // Initialize a 40x30 buffer (2400 bytes per buffer)
        LDA #10
        LDY #7
        ScreenBuffer.Initialize();
        if (NC)
        {
            // Failed to initialize
            LDA #(failed % 256)
            STA ZP.STRL
            LDA #(failed / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        ScreenBuffer.HideCursor();
        
        // Set blue background
        LDA # Screen.Color.Blue
        ScreenBuffer.SetBackground();
        
        // Clear fills entire buffer with spaces using current background
        ScreenBuffer.Clear();
        
        LDA # Screen.Color.Yellow
        ScreenBuffer.SetForeground();
            
        LDA #4
        STA rowPos
        LDA #3
        STA colPos
        
        LDA colPos
        LDY rowPos
        ScreenBuffer.GotoXY();
        
        LDA # Screen.Color.Red
        ScreenBuffer.SetBackground();
        
        LDA #(hello % 256)
        STA ZP.STRL
        LDA #(hello / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        loop
        {
            // Wait for <esc>
            getKey();
            PHA
            
            ScreenBuffer.Suspend();
            
            LDA colPos
            LDY rowPos
            ScreenBuffer.GotoXY();
            
            LDA # Screen.Color.Blue
            ScreenBuffer.SetBackground();
            
            LDA #(blank % 256)
            STA ZP.STRL
            LDA #(blank / 256)
            STA ZP.STRH
            ScreenBuffer.String();
            
            PLA     
            switch (A)
            {
                case BlueFill.keyEsc:
                {
                    ScreenBuffer.Resume();
                    break;
                }
                /*
                case BlueFill.keyUp:
                {
                    //DEC rowPos
                    ScreenBuffer.ScrollDown();
                    ScreenBuffer.Resume();
                    continue;
                }
                */
                case BlueFill.keyDown:
                {
                    //INC rowPos
                    ScreenBuffer.ScrollUp();
                    ScreenBuffer.Resume();
                    continue;
                }
                case BlueFill.keyLeft:
                {
                    DEC colPos
                }
                case BlueFill.keyRight:
                {
                    INC colPos
                }
            }
            
            LDA colPos
            LDY rowPos
            ScreenBuffer.GotoXY();
            
            LDA # Screen.Color.Red
            ScreenBuffer.SetBackground();
            
            LDA #(hello % 256)
            STA ZP.STRL
            LDA #(hello / 256)
            STA ZP.STRH
            ScreenBuffer.String();
            
            ScreenBuffer.Resume();
        }
        
        ScreenBuffer.ShowCursor();
        
        // Clean up
        ScreenBuffer.Dispose();
        
        // Reset and Clear screen
        Screen.Reset();
        Screen.Clear();
        
        LDA #(ok % 256)
        STA ZP.STRL
        LDA #(ok / 256)
        STA ZP.STRH
        Print.String();
    }
}
