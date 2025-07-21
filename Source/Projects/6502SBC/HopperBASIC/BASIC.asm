program HopperBASIC
{
    #define CPU_65C02S
    #define ROM_8K
    
    #define HOPPER_BASIC
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Tools"
    
    const string Welcome = "\nHopper BASIC v1.0\n";
    const string MemoryMsg = "Memory: ";
    const string KBytes = " bytes available\n";
    const string Ready = "READY\n> ";
    
    printAvailableMemory()
    {
        // Use the runtime's Memory.Available() function
        Memory.Available();  // Pushes available memory (UInt) to stack
        
        // Pop the typed value - Memory.Available() returns UInt type
        Stacks.PopTop();  // This properly handles the type and puts value in TOP
        
        // Convert bytes to KB (divide by 1024, approximately by using high byte)
        PrintDecimalWord();
    }
    
    initializeSystem()
    {
        // Initialize the complete runtime system
        
        // Set up program size as minimal (just our BASIC interpreter)
        LDA #0  // empty for now (not Hopper program)
        STA ZP.PROGSIZE
        
        // Initialize memory heap using runtime function
        Memory.InitializeHeapSize();
        
        // Initialize stacks
        Stacks.Initialize();
        
        // Clear flags and set up basic state
        STZ ZP.FLAGS
        SMB0 ZP.FLAGS  // Program loaded flag
    }
    
    IRQ()
    {
        Serial.ISR();
    }
    
    NMI()
    {
        // Hardware break - could be used for BASIC BREAK functionality
        INC ZP.SerialBreakFlag
    }
    
    Hopper()
    {
        SEI  // Disable interrupts during initialization
        
        // Initialize serial first
        Serial.Initialize();
        
        // Initialize the complete system using runtime functions
        initializeSystem();
        
        CLI  // Re-enable interrupts
        
        // Print welcome message
        LDA #(Welcome % 256)
        STA ZP.IDXL
        LDA #(Welcome / 256)
        STA ZP.IDXH
        PrintString();
        
        // Print memory information
        LDA #(MemoryMsg % 256)
        STA ZP.IDXL
        LDA #(MemoryMsg / 256)
        STA ZP.IDXH
        PrintString();
        
        // Print available memory using runtime function
        printAvailableMemory();
        
        LDA #(KBytes % 256)
        STA ZP.IDXL
        LDA #(KBytes / 256)
        STA ZP.IDXH
        PrintString();
        
        // Print ready prompt
        LDA #(Ready % 256)
        STA ZP.IDXL
        LDA #(Ready / 256)
        STA ZP.IDXH
        PrintString();
        
        // Main BASIC interpreter loop
        loop
        {
            // Check for break condition
            LDA ZP.SerialBreakFlag
            if (NZ)
            {
                STZ ZP.SerialBreakFlag
                LDA #'^'
                Serial.WriteChar();
                LDA #'C'
                Serial.WriteChar();
                LDA #'\n'
                Serial.WriteChar();
                
                // Print ready prompt again
                LDA #(Ready % 256)
                STA ZP.IDXL
                LDA #(Ready / 256)
                STA ZP.IDXH
                PrintString();
            }
            
            // Check if character available
            Serial.IsAvailable();
            if (NZ)
            {
                Serial.WaitForChar();
                
                // Handle basic commands
                switch (A)
                {
                    case '\r':
                    case '\n':
                    {
                        // Enter pressed - process command line
                        LDA #'\n'
                        Serial.WriteChar();
                        
                        // TODO: Parse and execute BASIC command/program line
                        
                        // For now, just show prompt again
                        LDA #(Ready % 256)
                        STA ZP.IDXL
                        LDA #(Ready / 256)
                        STA ZP.IDXH
                        PrintString();
                    }
                    case 0x08:  // Backspace
                    case 0x7F:  // Delete
                    {
                        // Handle backspace/delete
                        LDA #0x08   // Backspace
                        Serial.WriteChar();
                        LDA #' '    // Space
                        Serial.WriteChar();
                        LDA #0x08   // Backspace again
                        Serial.WriteChar();
                    }
                    case 0x03:  // Ctrl+C
                    {
                        // Break command
                        LDA #'^'
                        Serial.WriteChar();
                        LDA #'C'
                        Serial.WriteChar();
                        LDA #'\n'
                        Serial.WriteChar();
                        
                        LDA #(Ready % 256)
                        STA ZP.IDXL
                        LDA #(Ready / 256)
                        STA ZP.IDXH
                        PrintString();
                    }
                    default:
                    {
                        // Echo printable characters
                        CMP #' '
                        if (C)  // >= 32
                        {
                            CMP #0x7F
                            if (NC)  // < 127
                            {
                                Serial.WriteChar();
                                // TODO: Add to input buffer
                            }
                        }
                    }
                }
            }
            
            // TODO: Add the following BASIC interpreter components:
            // 1. Line input buffer management
            // 2. Tokenization of BASIC keywords
            // 3. Program storage in tokenized form
            // 4. Variable storage and management
            // 5. Expression evaluation
            // 6. Command processing (RUN, LIST, NEW, SAVE, LOAD, etc.)
            // 7. Statement execution (PRINT, LET, IF, FOR, etc.)
            // 8. Built-in functions (READ, WRITE, PWM, DELAY, etc.)
        }
    }
}
