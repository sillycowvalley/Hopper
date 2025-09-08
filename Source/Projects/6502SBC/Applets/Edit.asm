program SimpleEditor
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Screen"
    uses "System/Memory"
    uses "System/File"
    uses "System/Serial"
    uses "System/Debug"
    uses "System/ScreenBuffer"
    uses "Editor/Keyboard"
    uses "Editor/GapBuffer"
    uses "Editor/View"
    
    
    // Messages
    const string loadingMsg = "Loading BIGTEST...\n";
    const string notFoundMsg = "File not found!\n";
    const string errorMsg = "Error loading file!\n";
    const string fileName = "BIGTEST";      
    
        
    // Load BIGTEST file into GapBuffer
    loadFile()
    {
        LDA #(loadingMsg % 256)
        STA ZP.STRL
        LDA #(loadingMsg / 256)
        STA ZP.STRH
        Print.String();
        
        // Set filename
        LDA #(fileName % 256)
        STA ZP.STRL
        LDA #(fileName / 256)
        STA ZP.STRH
        
        // Check if exists
        LDA #File.FileType.Any
        File.Exists();
        if (NC)
        {
            LDA #(notFoundMsg % 256)
            STA ZP.STRL
            LDA #(notFoundMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Open for reading
        LDA #(fileName % 256)
        STA ZP.STRL
        LDA #(fileName / 256)
        STA ZP.STRH
        LDA #File.FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA #(errorMsg % 256)
            STA ZP.STRL
            LDA #(errorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Read file chunks and insert into GapBuffer
        loop
        {
            File.NextStream();
            if (NC) { break; }  // End of file
            
            // Insert this chunk into GapBuffer
            LDY #0
            loop
            {
                // Check if done with chunk (Y >= TransferLength)
                // Since TransferLength is typically <= 256, high byte is usually 0
                LDA File.TransferLengthH
                if (NZ)
                {
                    // TransferLength > 255, so Y can't be >= it
                }
                else
                {
                    // TransferLength <= 255, compare Y with low byte
                    CPY File.TransferLengthL
                    if (C) { break; }  // Y >= TransferLength
                }
                
                LDA File.FileDataBuffer, Y
                PHY
                GapBuffer.InsertChar();
                PLY
                INY
                if (Z) { break; }  // Y wrapped to 0 after 255
            }
        }
    }
    
    
    Hopper()
    {
        Debug.Initialize();
        
        // Clear screen
        Screen.Clear();
        
        // Initialize keyboard
        Keyboard.Initialize();
        
        // Initialize view
        View.Initialize();
        if (NC)
        {
            return;
        }
        
        // Load the test file
        loadFile();
        View.ApplyGapBuffer();
        
        // Main loop
        loop
        {
            // Get key
            Keyboard.GetKey();
            
            // Process key
            switch (A)
            {
                case Key.Escape:
                {
                    break;  // Exit
                }
                case Key.Up:
                {
                    View.CursorUp();
                }
                case Key.Down:
                {
                    View.CursorDown();
                }
                default:
                {
                    // Ignore other keys
                }
            }
        }
        
        // Cleanup
        View.Dispose();
        Screen.Reset();
    }
}
