program WireScanDebug
{
    uses "/Source/Library/Boards/Pi"
    
    TestSingleAddress(byte address)
    {
        Print("Testing 0x");
        Print(address.ToHexString(2));
        Print(": ");
        
        Wire.BeginTx(address);
        byte result = Wire.EndTx();
        
        Print("result=");
        PrintLn(result.ToString());
        
        return result;
    }
    
    Setup()
    {
        PrintLn("Wire Scan Debug - Focus on Device Detection");
        PrintLn("==========================================");
        
        if (!Wire.Initialize(1, 2, 3, 400))
        {
            PrintLn("FAILED to initialize I2C!");
            return;
        }
        PrintLn("I2C Initialize: SUCCESS");
        
        if (!Wire.Begin(1))
        {
            PrintLn("FAILED to begin I2C!");
            return;
        }
        PrintLn("I2C Begin: SUCCESS");
        PrintLn();
    }
    
    bool InitializeSSD1306()
    {
        PrintLn("=== Initializing SSD1306 Display ===");
        
        // SSD1306 initialization sequence
        byte[15] initSequence;
        initSequence[0] = 0xAE;   // Display OFF
        initSequence[1] = 0xD5;   // Set display clock div
        initSequence[2] = 0x80;   // Suggested ratio
        initSequence[3] = 0xA8;   // Set multiplex
        initSequence[4] = 0x3F;   // 64 rows (128x64 display)
        initSequence[5] = 0xD3;   // Set display offset
        initSequence[6] = 0x00;   // No offset
        initSequence[7] = 0x40;   // Set start line
        initSequence[8] = 0x8D;   // Charge pump
        initSequence[9] = 0x14;   // Enable charge pump
        initSequence[10] = 0x20;  // Memory mode
        initSequence[11] = 0x00;  // Horizontal addressing
        initSequence[12] = 0xA1;  // Segment remap
        initSequence[13] = 0xC8;  // COM scan direction
        initSequence[14] = 0xAF;  // Display ON
        
        Wire.BeginTx(0x3C);
        Wire.Write(0x00);  // Command mode
        
        for (uint i = 0; i < 15; i++)
        {
            Wire.Write(initSequence[i]);
        }
        
        byte result = Wire.EndTx();
        Print("Initialization result: ");
        PrintLn(result.ToString());
        
        if (result == 0)
        {
            PrintLn("SSD1306 initialized successfully");
            Time.Delay(100);  // Let display settle
            return true;
        }
        PrintLn("Initialization failed");
        return false;
    }
    
    ClearDisplay()
    {
        PrintLn("=== Clearing Display ===");
        
        // Set full screen area
        Wire.BeginTx(0x3C);
        Wire.Write(0x00);  // Command mode
        Wire.Write(0x21);  // Set column address
        Wire.Write(0x00);  // Column start = 0
        Wire.Write(0x7F);  // Column end = 127
        Wire.Write(0x22);  // Set page address  
        Wire.Write(0x00);  // Page start = 0
        Wire.Write(0x07);  // Page end = 7
        byte setupResult = Wire.EndTx();
        
        if (setupResult != 0)
        {
            Print("Clear setup failed: ");
            PrintLn(setupResult.ToString());
            return;
        }
        
        // Clear all pixels (send zeros)
        for (uint page = 0; page < 8; page++)
        {
            Wire.BeginTx(0x3C);
            Wire.Write(0x40);  // Data mode
            
            // Send 16 zero bytes per transaction
            for (uint i = 0; i < 16; i++)
            {
                Wire.Write(0x00);
            }
            
            byte result = Wire.EndTx();
            if (result != 0)
            {
                Print("Clear failed at page ");
                Print(page.ToString());
                Print(": ");
                PrintLn(result.ToString());
                return;
            }
        }
        
        PrintLn("Display cleared");
    }
    
    DrawTestPattern()
    {
        PrintLn("=== Drawing Test Pattern ===");
        
        // Set area for test pattern (top-left corner)
        Wire.BeginTx(0x3C);
        Wire.Write(0x00);  // Command mode
        Wire.Write(0x21);  // Set column address
        Wire.Write(0x00);  // Column start = 0
        Wire.Write(0x1F);  // Column end = 31 (32 pixels wide)
        Wire.Write(0x22);  // Set page address  
        Wire.Write(0x00);  // Page start = 0
        Wire.Write(0x01);  // Page end = 1 (16 pixels tall)
        byte setupResult = Wire.EndTx();
        
        if (setupResult != 0)
        {
            Print("Pattern setup failed: ");
            PrintLn(setupResult.ToString());
            return;
        }
        
        // Draw pattern: alternating 0xFF and 0x00
        Wire.BeginTx(0x3C);
        Wire.Write(0x40);  // Data mode
        
        for (uint i = 0; i < 64; i++)  // 32 columns x 2 pages
        {
            if ((i % 2) == 0)
            {
                Wire.Write(0xFF);  // White pixels
            }
            else
            {
                Wire.Write(0x00);  // Black pixels
            }
        }
        
        byte result = Wire.EndTx();
        Print("Pattern draw result: ");
        PrintLn(result.ToString());
        
        if (result == 0)
        {
            PrintLn("Test pattern drawn - check top-left corner!");
        }
    }
    
    TestSSD1306Communication()
    {
        PrintLn("=== Testing SSD1306 Communication ===");
        PrintLn();
        
        // Initialize the display properly
        if (!InitializeSSD1306())
        {
            return;
        }
        
        PrintLn();
        
        // Clear the display
        ClearDisplay();
        
        PrintLn();
        
        // Draw a test pattern
        DrawTestPattern();
        
        PrintLn();
        PrintLn("Complete! You should see a striped pattern in the top-left corner.");
    }
    
    Hopper()
    {
        Setup();
        
        PrintLn("FOCUS: Testing Wire library with SSD1306");
        PrintLn();
        
        // First confirm we can still see the device
        PrintLn("=== Device Detection Test ===");
        Print("Device 0x3C present: ");
        Wire.BeginTx(0x3C);
        byte detection = Wire.EndTx();
        if (detection == 0)
        {
            PrintLn("YES");
            PrintLn();
            
            // Now test actual SSD1306 communication
            TestSSD1306Communication();
        }
        else
        {
            Print("NO - Error ");
            PrintLn(detection.ToString());
            PrintLn("Cannot proceed with SSD1306 tests");
        }
    }
}
