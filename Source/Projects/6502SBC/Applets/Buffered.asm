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
    const string done = "Blue fill complete. Press any key.\n";
    const string ok = "OK\n";
    
    Hopper()
    {
        // Clear screen from upload junk
        Screen.Clear();
    
        // Show we're starting
        LDA #(starting % 256)
        STA ZP.STRL
        LDA #(starting / 256)
        STA ZP.STRH
        Print.String();
        
        // Initialize a 40x30 buffer (2400 bytes per buffer)
        LDA #40
        LDY #30
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
        
        // Set blue background
        LDA #Screen.Color.Blue
        ScreenBuffer.SetBackground();
        
        // Clear fills entire buffer with spaces using current background
        ScreenBuffer.Clear();
        
        // Display it (no suspend/resume needed for single operation)
        ScreenBuffer.Update();
        
        // Show completion message
        LDA #(done % 256)
        STA ZP.STRL
        LDA #(done / 256)
        STA ZP.STRH
        Print.String();
        
        // Wait for keypress
        Serial.WaitForChar();
        
        // Clean up
        ScreenBuffer.Dispose();
        
        // Clear screen and reset
        Screen.Clear();
        Screen.Reset();
        
        LDA #(ok % 256)
        STA ZP.STRL
        LDA #(ok / 256)
        STA ZP.STRH
        Print.String();
    }
}
