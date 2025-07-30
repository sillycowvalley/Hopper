program HopperBASIC
{
    #define DEBUG
    
    #define CPU_65C02S
    #define HOPPER_BASIC
    #define ROM_32K
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Messages"
    uses "Tokenizer"
    uses "Compiler"
    uses "Executor"
    uses "Console"
    uses "Tools"
    uses "Statement"
    
    // Initialize the BASIC system
    InitializeBASIC()
    {
        Messages.ClearError();
        
        // Initialize serial communication first
        Serial.Initialize();
        
        // Initialize Hopper VM runtime components
        Memory.InitializeHeapSize();
        Stacks.Initialize();
        
        // Clear system flags and set basic state
        STZ ZP.FLAGS
        SMB0 ZP.FLAGS  // Set "program loaded" flag for BASIC
        
        // Initialize BASIC-specific components
        Console.Initialize();  // This now initializes the tokenizer too
        
        // Clear program size (we're not a traditional Hopper program)
        STZ ZP.PROGSIZE
    }
    
    
    // Print startup banner with system information
    printStartupBanner()
    {
        // Welcome message
        LDA #(Messages.Welcome % 256)
        STA ZP.ACCL
        LDA #(Messages.Welcome / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        Console.CmdMem();
    }
    
    // Main interpreter loop
    interpreterLoop()
    {
        // Show initial ready prompt
        LDA #(Messages.ReadyPrompt % 256)
        STA ZP.ACCL
        LDA #(Messages.ReadyPrompt / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        loop
        {
            // Read user input
            Console.ReadLine();
            
            // Check for empty line
            LDA ZP.BasicInputLength
            if (Z) 
            { 
                // Empty line - just show prompt, no READY
                LDA #'>'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
                continue; 
            }
            
            // Process non-empty line
            Console.ProcessLine();
            if (NC) 
            {
                CheckError();
                if (NC)
                {
                    Messages.CheckAndPrintError();
                }
                else
                {
                    break; // Exit if BYE was entered
                }
            }
            
            // Show ready prompt after successful execution
            LDA #(Messages.ReadyPrompt % 256)
            STA ZP.ACCL
            LDA #(Messages.ReadyPrompt / 256)
            STA ZP.ACCH
            Tools.PrintStringACC();
        }
    }
    
    // Interrupt handlers
    IRQ()
    {
        Serial.ISR();
    }
    
    NMI()
    {
        // Hardware break - could be used for BASIC BREAK functionality
        INC ZP.SerialBreakFlag
    }
    
    // Main entry point
    Hopper()
    {
        SEI  // Disable interrupts during initialization
        
        // Initialize the complete BASIC system
        InitializeBASIC();
        
        CLI  // Re-enable interrupts
        
        // Show startup information
        printStartupBanner();
        
        // Enter the main interpreter loop
        interpreterLoop();
    }
}
