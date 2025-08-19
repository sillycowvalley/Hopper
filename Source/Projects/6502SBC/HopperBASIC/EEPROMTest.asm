program EEPROMTest
{
    
    #define ROM_32K
    #define CPU_65C02S
    #define HOPPER_BASIC
    
    uses "./Definitions/ZeroPage"
    uses "./Definitions/Limits"
    uses "./Definitions/MemoryMap"
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Parallel"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/I2C"
    uses "/Source/Runtime/6502/Time"
    
    uses "./Files/EEPROM"
    uses "./Utilities/Print"
    uses "./Utilities/Tools"
    
    // Test data patterns
    const byte TestPattern1 = 0x55;  // 01010101
    const byte TestPattern2 = 0xAA;  // 10101010
    const byte TestPattern3 = 0xDE;  // 11011110
    const byte TestPattern4 = 0xAD;  // 10101101
    
    // Test addresses in EEPROM
    const uint TestAddress1 = 0x0000;  // Start of EEPROM
    const uint TestAddress2 = 0x0100;  // Second page
    const uint TestAddress3 = 0x1000;  // Higher address
    
    // RAM buffer addresses
    const uint WriteBuffer = 0x3000;   // Source buffer for writes
    const uint ReadBuffer  = 0x3100;   // Destination buffer for reads
    
    // String constants for test output
    const string msgStarting        = "EEPROM Test Program Starting...\n\n";
    const string msgInitializing    = "Initializing EEPROM...\n";
    const string msgDetecting       = "Detecting EEPROM...";
    const string msgDetected        = " DETECTED\n";
    const string msgNotFound        = " NOT FOUND\n";
    const string msgErrorNoEEPROM   = "ERROR: No EEPROM detected. Test aborted.\n";
    const string msgSize            = "EEPROM Size: ";
    const string msgKBytes          = "K bytes\n\n";
    const string msgUnknown         = "Unknown\n\n";
    const string msgTestHeader      = "=== EEPROM Comprehensive Test ===\n";
    const string msgTesting         = "  Testing address ";
    const string msgWithPattern     = " with pattern ";
    const string msgDotDotDot       = "...";
    const string msgPass            = " PASS\n";
    const string msgFail            = " FAIL at offset ";
    const string msgRetesting       = "  Re-testing first page with new pattern...\n";
    const string msgComplete        = "\n=== EEPROM Test Complete ===\n";
    
    // Print null-terminated string
    // Input: ZP.STR = pointer to string
    printString()
    {
        Print.String();
    }
    
    // Print hex byte with 0x prefix
    // Input: A = byte to print
    printHexByte()
    {
        PHA
        LDA #'0'
        Serial.WriteChar();
        LDA #'x'
        Serial.WriteChar();
        PLA
        Serial.HexOut();
    }
    
    // Print hex word with 0x prefix  
    // Input: ZP.TOP = 16-bit value to print
    printHexWord()
    {
        LDA #'0'
        Serial.WriteChar();
        LDA #'x'
        Serial.WriteChar();
        LDA ZP.TOPH
        Serial.HexOut();
        LDA ZP.TOPL
        Serial.HexOut();
    }
    
    // Fill write buffer with test pattern
    // Input: A = pattern byte
    fillWriteBuffer()
    {
        PHA  // Save pattern
        
        LDA #WriteBuffer % 256
        STA ZP.IDXL
        LDA #WriteBuffer / 256
        STA ZP.IDXH
        
        PLA  // Restore pattern
        LDY #0
        loop
        {
            STA [ZP.IDX], Y
            INY
            if (Z) { break; }  // Wrapped around - filled 256 bytes
        }
    }
    
    // Clear read buffer
    clearReadBuffer()
    {
        LDA #ReadBuffer % 256
        STA ZP.IDXL
        LDA #ReadBuffer / 256
        STA ZP.IDXH
        
        LDA #0
        LDY #0
        loop
        {
            STA [ZP.IDX], Y
            INY
            if (Z) { break; }  // Wrapped around - cleared 256 bytes
        }
    }
    
    // Compare write buffer with read buffer
    // Output: C set if match, NC if mismatch, A = first mismatch offset (if NC)
    compareBuffers()
    {
        LDY #0
        loop
        {
            LDA WriteBuffer, Y
            CMP ReadBuffer, Y
            if (NZ)
            {
                TYA  // Return mismatch offset
                CLC  // Set NC (mismatch flag)
                return;
            }
            INY
            if (Z) { break; }  // Wrapped around - compared all 256 bytes
        }
        SEC  // Set C (match flag)
    }
    
    // Test single EEPROM operation
    // Input: ZP.TOP = EEPROM address, ZP.ACC = test pattern
    // Complete fixed testEEPROMOperation with proper EEPROM write delays
    testEEPROMOperation()
    {
        // Print test message
        LDA #(msgTesting % 256)
        STA ZP.STRL
        LDA #(msgTesting / 256)
        STA ZP.STRH
        printString();
        
        printHexWord();
        
        LDA #(msgWithPattern % 256)
        STA ZP.STRL
        LDA #(msgWithPattern / 256)
        STA ZP.STRH
        printString();
        
        LDA ZP.ACCL
        printHexByte();
        
        LDA #(msgDotDotDot % 256)
        STA ZP.STRL
        LDA #(msgDotDotDot / 256)
        STA ZP.STRH
        printString();
        
        // SAVE the EEPROM address before any operations that might change ZP.TOP
        LDA ZP.TOPL
        PHA              // Save EEPROM address low
        LDA ZP.TOPH
        PHA              // Save EEPROM address high
        
        // Fill write buffer with test pattern
        LDA ZP.ACCL
        fillWriteBuffer();
        
        // Clear read buffer
        clearReadBuffer();
        
        // Set up addresses for EEPROM write
        LDA #WriteBuffer % 256
        STA ZP.IDXL
        LDA #WriteBuffer / 256
        STA ZP.IDXH
        
        // Restore EEPROM address for write
        PLA
        STA ZP.IDYH      // Restore EEPROM address high
        PLA
        STA ZP.IDYL      // Restore EEPROM address low
        
        // Save EEPROM address again (WritePage may modify IDY)
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        // Write page to EEPROM
        EEPROM.WritePage();
        
        // CRITICAL FIX: Proper EEPROM write completion delay
        // EEPROM write operations need 5-10ms to complete
        // Use timer-based delay, not CPU loop
        LDA #10          // 10 milliseconds delay
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        Time.DelayTOP(); // Proper timer-based delay
        
        // Set up addresses for EEPROM read
        LDA #ReadBuffer % 256
        STA ZP.IDXL
        LDA #ReadBuffer / 256  
        STA ZP.IDXH
        
        // Restore EEPROM address for read (ZP.TOP was overwritten by DelayTOP)
        PLA
        STA ZP.IDYH      // Restore EEPROM address high
        PLA
        STA ZP.IDYL      // Restore EEPROM address low
        
        // Read page from EEPROM
        EEPROM.ReadPage();
        
        // Compare buffers
        compareBuffers();
        if (C)  // Set C (match)
        {
            LDA #(msgPass % 256)
            STA ZP.STRL
            LDA #(msgPass / 256)
            STA ZP.STRH
            printString();
        }
        else    // Set NC (mismatch)
        {
            PHA  // Save mismatch offset
            LDA #(msgFail % 256)
            STA ZP.STRL
            LDA #(msgFail / 256)
            STA ZP.STRH
            printString();
            PLA  // Restore mismatch offset
            printHexByte();
            LDA #'\n'
            Serial.WriteChar();
        }
    }
    
    // Run comprehensive EEPROM tests
    runEEPROMTests()
    {
        LDA #(msgTestHeader % 256)
        STA ZP.STRL
        LDA #(msgTestHeader / 256)
        STA ZP.STRH
        printString();
        
        // Test 1: First page with pattern 0x55
        LDA #TestAddress1 % 256
        STA ZP.TOPL
        LDA #TestAddress1 / 256
        STA ZP.TOPH
        LDA #TestPattern1
        STA ZP.ACCL
        testEEPROMOperation();
        
        // Test 2: Second page with pattern 0xAA  
        LDA #TestAddress2 % 256
        STA ZP.TOPL
        LDA #TestAddress2 / 256
        STA ZP.TOPH
        LDA #TestPattern2
        STA ZP.ACCL
        testEEPROMOperation();
        
        // Test 3: Higher address with pattern 0xDE
        LDA #TestAddress3 % 256
        STA ZP.TOPL
        LDA #TestAddress3 / 256
        STA ZP.TOPH
        LDA #TestPattern3
        STA ZP.ACCL
        testEEPROMOperation();
        
        // Test 4: Re-test first page with different pattern
        LDA #(msgRetesting % 256)
        STA ZP.STRL
        LDA #(msgRetesting / 256)
        STA ZP.STRH
        printString();
        
        LDA #TestAddress1 % 256
        STA ZP.TOPL
        LDA #TestAddress1 / 256
        STA ZP.TOPH
        LDA #TestPattern4
        STA ZP.ACCL
        testEEPROMOperation();
    }
    // Interrupt handlers
    IRQ()
    {
        Serial.ISR();
        Parallel.ISR();
    }
    
    NMI()
    {
        // Hardware break - could be used for BASIC BREAK functionality
        SMB0 ZP.SerialBreakFlag
    }
    
    Initialize()
    {
        // Clear Zero Page
        LDX #0
        loop
        {
            CPX # ZP.ACIADATA // don't write to ACIA data register
            if (NZ) 
            {
                STZ 0x00, X
            }
            DEX
            if (Z) { break; }
        } 
        
        // Clear serial buffer
        LDA # 0
        STA IDXL
        LDA # (SerialInBuffer >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages();
        
        Serial.Initialize();
        Parallel.Initialize();
        
    }
    
    // Main entry point
    Hopper()
    {
        Initialize();
        
        
        LDA #(msgStarting % 256)
        STA ZP.STRL
        LDA #(msgStarting / 256)
        STA ZP.STRH
        printString();
        
        // Initialize EEPROM system
        LDA #(msgInitializing % 256)
        STA ZP.STRL
        LDA #(msgInitializing / 256)
        STA ZP.STRH
        printString();
        
        EEPROM.Initialize();
        
        // Test EEPROM detection
        LDA #(msgDetecting % 256)
        STA ZP.STRL
        LDA #(msgDetecting / 256)
        STA ZP.STRH
        printString();
        
        EEPROM.Detect();
        if (C)  // Set C (detected)
        {
            LDA #(msgDetected % 256)
            STA ZP.STRL
            LDA #(msgDetected / 256)
            STA ZP.STRH
            printString();
        }
        else    // Set NC (not detected)
        {
            LDA #(msgNotFound % 256)
            STA ZP.STRL
            LDA #(msgNotFound / 256)
            STA ZP.STRH
            printString();
            
            LDA #(msgErrorNoEEPROM % 256)
            STA ZP.STRL
            LDA #(msgErrorNoEEPROM / 256)
            STA ZP.STRH
            printString();
            return;
        }
        
        // Get and display EEPROM size
        LDA #(msgSize % 256)
        STA ZP.STRL
        LDA #(msgSize / 256)
        STA ZP.STRH
        printString();
        
        EEPROM.GetSize();
        if (C)  // Set C (size retrieved)
        {
            Serial.HexOut();
            LDA #(msgKBytes % 256)
            STA ZP.STRL
            LDA #(msgKBytes / 256)
            STA ZP.STRH
            printString();
        }
        else    // Set NC (size unknown)
        {
            LDA #(msgUnknown % 256)
            STA ZP.STRL
            LDA #(msgUnknown / 256)
            STA ZP.STRH
            printString();
        }
        
        // Run comprehensive tests
        runEEPROMTests();
        
        LDA #(msgComplete % 256)
        STA ZP.STRL
        LDA #(msgComplete / 256)
        STA ZP.STRH
        printString();
    }
}
