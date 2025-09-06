program BufferTest
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Serial"
    uses "System/Time"
    uses "System/Screen"
    uses "System/ScreenBuffer"
    
    const string title = "ScreenBuffer Test";
    const string hello = "Hello, World!";
    const string status = "Ready";
    const string done = "Test Complete - Press any key";
    
    delay500()
    {
        LDA #(500 % 256)
        STA ZP.TOP0
        LDA #(500 / 256)
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Time.Delay();
    }
    
    Hopper()
    {
        // Initialize a small 40x15 buffer (1200 bytes per buffer)
        LDA #40
        LDY #15
        ScreenBuffer.Initialize();
        if (NC)
        {
            // Failed to initialize
            LDA #'E'
            Print.Char();
            LDA #'r'
            Print.Char();
            LDA #'r'
            Print.Char();
            Print.NewLine();
            return;
        }
        
        // Start with suspend to batch all initial drawing
        ScreenBuffer.Suspend();
        
        // Clear with blue background
        LDA #Screen.Color.Blue
        ScreenBuffer.SetBackground();
        ScreenBuffer.Clear();
        
        // Draw white title at top
        LDA #Screen.Color.White
        LDY #Screen.Color.Blue
        ScreenBuffer.SetColors();
        ScreenBuffer.SetBold();
        
        LDA #12  // Center the title
        STA ScreenBuffer.CursorX
        STZ ScreenBuffer.CursorY  // Y = 0
        
        LDA #(title % 256)
        STA ZP.STRL
        LDA #(title / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Draw a box in normal text
        ScreenBuffer.SetNormal();
        LDA #Screen.Color.Yellow
        LDY #Screen.Color.Blue
        ScreenBuffer.SetColors();
        
        LDA #5
        STA ScreenBuffer.CursorX
        LDA #3
        STA ScreenBuffer.CursorY
        
        LDA #30  // Width
        LDY #8   // Height
        ScreenBuffer.Box();
        
        // Write hello message inside box
        LDA #Screen.Color.White
        ScreenBuffer.SetForeground();
        ScreenBuffer.SetNormal();
        
        LDA #13
        STA ScreenBuffer.CursorX
        LDA #6
        STA ScreenBuffer.CursorY
        
        LDA #(hello % 256)
        STA ZP.STRL
        LDA #(hello / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Status line at bottom
        LDA #Screen.Color.Green
        LDY #Screen.Color.Black
        ScreenBuffer.SetColors();
        
        STZ ScreenBuffer.CursorX
        LDA #14
        STA ScreenBuffer.CursorY
        
        LDA #(status % 256)
        STA ZP.STRL
        LDA #(status / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        // Now render everything at once
        ScreenBuffer.Resume();
        
        // Wait a bit
        delay500();
        delay500();
        
        // Test updating just one cell - should be efficient
        ScreenBuffer.Suspend();
        
        LDA #Screen.Color.Red
        ScreenBuffer.SetForeground();
        ScreenBuffer.SetInverse();
        
        LDA #35
        STA ScreenBuffer.CursorX
        LDA #14
        STA ScreenBuffer.CursorY
        
        LDA #'!'
        ScreenBuffer.Char();
        
        ScreenBuffer.Resume();  // Should only send one character
        
        // Wait
        delay500();
        delay500();
        
        // Final message
        ScreenBuffer.Suspend();
        
        LDA #Screen.Color.White
        LDY #Screen.Color.Black
        ScreenBuffer.SetColors();
        ScreenBuffer.SetNormal();
        
        LDA #5
        STA ScreenBuffer.CursorX
        LDA #12
        STA ScreenBuffer.CursorY
        
        LDA #(done % 256)
        STA ZP.STRL
        LDA #(done / 256)
        STA ZP.STRH
        ScreenBuffer.String();
        
        ScreenBuffer.Resume();
        
        // Wait for key
        Serial.WaitForChar();
        
        // Clean up
        ScreenBuffer.Dispose();
        
        // Clear screen normally
        Screen.Clear();
        Screen.Reset();
        
        LDA #'O'
        Print.Char();
        LDA #'K'
        Print.Char();
        Print.NewLine();
    }
}
