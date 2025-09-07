program TextEditor
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Serial"
    uses "System/Screen"
    uses "System/Time"
    
    uses "Editor/Keyboard"
    uses "Editor/GapBuffer"
    uses "Editor/View"
    uses "Editor/Commands"
    
    const string initMsg = "Text Editor v1.0\nInitializing...\n";
    const string memErrorMsg = "ERROR: Could not allocate memory!\n";
    const string byeMsg = "\nGoodbye!\n";
    
    Hopper()
    {
        // Clear screen and show initialization message
        Screen.Clear();
        LDA #(initMsg % 256)
        STA ZP.STRL
        LDA #(initMsg / 256)
        STA ZP.STRH
        Print.String();
        
        // Initialize keyboard handler
        Keyboard.Initialize();
        
        // Initialize view with 80x25 screen (24 lines + 1 status line)
        LDA #80
        LDY #25
        View.Initialize();
        if (NC)
        {
            LDA #(memErrorMsg % 256)
            STA ZP.STRL
            LDA #(memErrorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Initialize gap buffer with 8KB
        LDA #(8192 % 256)
        LDY #(8192 / 256)
        GapBuffer.Initialize();
        if (NC)
        {
            View.Dispose();
            
            LDA #(memErrorMsg % 256)
            STA ZP.STRL
            LDA #(memErrorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Initialize command processor
        Commands.Initialize();
        
        // Try to load TEST file
        Commands.LoadFile();
        
        // Clear screen for editor
        Screen.Clear();
        
        // Main editor loop
        loop
        {
            // Render screen if needed
            View.Render();
            
            // Check for break key
            Serial.IsBreak();
            if (C) { break; }
            
            // Get and process key
            Keyboard.GetKey();
            Commands.ProcessKey();
            
            // Check if should exit
            Commands.ShouldExit();
            if (C) { break; }
        }
        
        // Cleanup
        GapBuffer.Dispose();
        View.Dispose();
        
        // Reset screen and show goodbye
        Screen.Reset();
        Screen.Clear();
        LDA #(byeMsg % 256)
        STA ZP.STRL
        LDA #(byeMsg / 256)
        STA ZP.STRH
        Print.String();
    }
}
