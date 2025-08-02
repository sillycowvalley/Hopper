program HopperBASIC
{
    //#define DEBUG
    #define TRACE
    
    //#define TRACECONSOLE
    
    #define CPU_65C02S
    #define HOPPER_BASIC
    #define ROM_32K
    
#ifdef TRACECONSOLE
    #define TRACE
#endif
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Messages"
    uses "Error"
    uses "State"
    uses "Debug"
    uses "Trace"
    uses "Tools"
    
    
    uses "Tokenizer"
    uses "FunctionDeclaration.asm"
    uses "Statement"
    uses "Compiler"
    
    uses "Functions"
    
    uses "Instructions"
    uses "ComparisonInstructions"
    uses "Executor"
    
    uses "Console"
    
    
    // Initialize the BASIC system
    InitializeBASIC()
    {
        Error.ClearError();
        State.SetSuccess();    // Initialize state system
        Trace.Initialize();    // Initialize trace system (NOP in production code)
        
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
            
            // Check state after reading line
            Error.CheckErrorAndStatus();
            if (NC)
            {
                State.IsExiting();
                if (C) { break; } // Exit on Ctrl+C during input
                Error.CheckAndPrint();
                continue; // Error during input, show prompt again
            }
            
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
            
            // Check for exit first (regardless of error state)
            State.IsExiting();
            if (C) { break; } // BYE command - clean exit
            
            // Then check for errors
            Error.CheckError();
            if (NC)
            {
                Error.CheckAndPrint();
            }
            
            // Show ready prompt after successful execution
            Statement.IsCaptureModeOff();
            if (C)
            {
                LDA #(Messages.ReadyPrompt % 256)
                STA ZP.ACCL
                LDA #(Messages.ReadyPrompt / 256)
                STA ZP.ACCH
                Tools.PrintStringACC();
            }
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
        
        // Set exit state to break out of interpreter loop
        State.SetExiting();
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
