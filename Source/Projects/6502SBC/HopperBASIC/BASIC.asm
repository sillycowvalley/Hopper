program HopperBASIC
{
    //#define DEBUG
    
    #define CPU_65C02S
    #define HOPPER_BASIC
    
#if defined(DEBUG) 
    #define ROM_16K
#endif
#if !defined(ROM_8K) && !defined(ROM_8K) && !defined(ROM_16K) && !defined(ROM_32K)
    #define ROM_8K
#endif

    
    
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Messages"
    uses "Tokenizer"
    uses "Console"
    uses "Tools"
    uses "Statement"
    uses "Expression"
    //uses "Variables"
    //uses "BASICRuntime"
    
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
        //Variables.Initialize();
        //BASICRuntime.Initialize();
        
        // Clear program size (we're not a traditional Hopper program)
        STZ ZP.PROGSIZE
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
        
        Console.CmdMem();
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
            Console.ProcessLine();  // Returns C to continue, NC to exit
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