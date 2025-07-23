program HopperBASIC
{
    #define CPU_65C02S
    #define ROM_8K
    #define HOPPER_BASIC
    #define DEBUG
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Messages"
    uses "Tokenizer"
    uses "Console"
    uses "Tools"
    //uses "Variables"
    //uses "Expressions" 
    //uses "Statements"
    //uses "BASICRuntime"
    
    // Initialize the BASIC system
    initializeBASIC()
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
        //Variables.Initialize();
        //BASICRuntime.Initialize();
        
        // Clear program size (we're not a traditional Hopper program)
        STZ ZP.PROGSIZE
    }
    
    // Print available memory in decimal format
    printAvailableMemory()
    {
        // Use Hopper VM's Memory.Available() function
        Memory.Available();  // Pushes available memory (UInt) to stack
        
        // Pop the typed value and put it in TOP
        Stacks.PopTop();  
        
        // Print the decimal value using existing utility
        Tools.PrintDecimalWord();
    }
    
    // Print startup banner with system information
    printStartupBanner()
    {
        // Welcome message
        LDA #(Messages.Welcome % 256)
        STA ZP.IDXL
        LDA #(Messages.Welcome / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Memory information
        LDA #(Messages.MemoryMsg % 256)
        STA ZP.IDXL
        LDA #(Messages.MemoryMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        printAvailableMemory();
        
        LDA #(Messages.BytesMsg % 256)
        STA ZP.IDXL
        LDA #(Messages.BytesMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
    
    // Main interpreter loop
    interpreterLoop()
    {
        loop
        {
            // Show ready prompt
            LDA #(Messages.ReadyPrompt % 256)
            STA ZP.IDXL
            LDA #(Messages.ReadyPrompt / 256)
            STA ZP.IDXH
            Tools.PrintString();
            
            // Read and process user input using tokenizer
            Console.ReadLine();
            
            // Check for empty line
            LDA ZP.BasicInputLength
            if (Z) { continue; }  // Empty line, show prompt again
            
            // Parse and execute the command/statement
            Console.ProcessLine();  // Returns Z to continue, NZ to exit
            if (NZ) { break; }  // Exit if BYE was entered
            Messages.CheckAndPrintError();
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
        initializeBASIC();
        
        CLI  // Re-enable interrupts
        
        // Show startup information
        printStartupBanner();
        
        // Enter the main interpreter loop
        interpreterLoop();
        
        // If we get here, user chose to exit
        LDA #(Messages.Goodbye % 256)
        STA ZP.IDXL
        LDA #(Messages.Goodbye / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
}
