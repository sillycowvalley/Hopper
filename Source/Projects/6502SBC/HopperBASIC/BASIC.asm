program HopperBASIC
{
    // Optional Features
    #define PEEPHOLE  // include the peephole optimizer
    #define HASEEPROM
        
    #define RELEASE // remove all the BIT ZP.EmulatorPCL hacks (~450 bytes)
    //#define DEBUG
    //#define TRACE  // Compiler and Executor call tree walks
    //#define TRACEFILE  // Storage and File
    //#define TRACEPARSE // Compiler and CompilerFlow
    //#define TRACEEXE // instructions in Executor

    //#define TRACECONSOLE // trace output for Console.asm and Command.asm
    
    #define CPU_65C02S
    #define HOPPER_BASIC
    
    
#ifdef DEBUG    
    #define ROM_48K
#else
    //#define ROM_48K    
    #define ROM_32K
#endif
    
    uses "./Definitions/ZeroPage"
    uses "./Definitions/Limits"
    uses "./Definitions/MemoryMap"
    uses "./Definitions/Messages"
    uses "./Definitions/BASICTypes"
    uses "./Definitions/States"
    uses "./Definitions/Tokens"
    uses "./Debugging/Error"
    uses "./Debugging/Debug"
    uses "./Debugging/Trace"
    
    uses "/Source/Runtime/6502/Types"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/Time"
    uses "/Source/Runtime/6502/Parallel"
    
    uses "./Utilities/Print"
    uses "./Utilities/Tools"
    uses "./Utilities/BufferManager"
    
    uses "./Objects/Char"
    uses "./Objects/Long"
    uses "./Objects/String"
    
    uses "./Objects/Table"
    uses "./Objects/Objects"
    uses "./Objects/Variables"
    uses "./Objects/Locals"
    uses "./Objects/Functions"
    uses "./Objects/Array"
    
    
#ifdef HASEEPROM
    uses "./Files/EEPROM"
    uses "./Files/File"
    uses "./Files/Storage"
#endif

    uses "Tokenizer"
    uses "FunctionDeclaration.asm"
    uses "Statement"
    uses "Compiler"
    uses "Optimizer"
    
    uses "GPIO"
    
    
    uses "Instructions"
    uses "ComparisonInstructions"
    uses "Executor"
    
    uses "Console"
    
    
    // Initialize the BASIC system
    InitializeBASIC()
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
#ifdef DEBUG
        Tokens.ValidateAllKeywordTables();
#endif
        Error.ClearError();
        States.SetSuccess();    // Initialize state system
        Trace.Initialize();    // Initialize trace system (NOP in production code)
        
        // Initialize communication first
        Serial.Initialize();
        Parallel.Initialize();
        
        // Initialize Hopper VM runtime components
        Memory.InitializeHeapSize();
        Stacks.Initialize();

        // for LoadGlobals | SaveGlobals        
        STZ ZP.GVIL
        STZ ZP.GVIH
        
        // Clear system flags and set basic state
        STZ ZP.FLAGS
        SMB0 ZP.FLAGS  // Set "program loaded" flag for BASIC
#if defined(TRACE) || defined(TRACEEXE)
        RMB2 ZP.FLAGS  // TROFF by default
#else
        RMB2 ZP.FLAGS  // TROFF by default
#endif        

#ifdef HASEEPROM
        EEPROM.Initialize();
#endif
        
        // Initialize BASIC-specific components
        Console.Initialize();  // This now initializes the tokenizer too
     
    }
    
    
    // Print startup banner with system information
    printStartupBanner()
    {
        // Welcome message
        LDA #(Messages.Welcome % 256)
        STA ZP.STRL
        LDA #(Messages.Welcome / 256)
        STA ZP.STRH
        Print.String();
        
        Commands.CmdMem();
    }
    
    // Main interpreter loop
    interpreterLoop()
    {
        // Show initial ready prompt
        LDX # MessageExtras.None
        LDA # ErrorID.ReadyPrompt Error.Message();
        
        loop
        {
            BufferManager.ResetInputBuffer();
                        
            // Read user input
            Console.ReadLine();
            
            // Check state after reading line
            Error.CheckErrorAndStatus();
            if (NC)
            {
                States.IsExiting();
                if (C) 
                {
                    break; // Exit on Ctrl+C during input
                } 
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
                Print.Space();
                continue; 
            }
            
            // Clear output flag before processing statement
            RMB6 ZP.FLAGS // Bit 6 - track output was produced by REPL command
            
            // Process non-empty line
            Console.ProcessLine();
            
            // Check for exit first (regardless of error state)
            States.IsExiting();
            if (C) { break; } // BYE command - clean exit
            
            // Then check for errors
            Error.CheckError();
            if (NC)
            {
                Error.CheckAndPrint();
            }
            else
            {
                // Check if statement produced output
                if (BBR6, ZP.FLAGS)  // Bit 6 clear - no output produced by last REPL command
                {
                    Statement.IsCaptureModeOff();
                    if (C)
                    {
                        LDX # MessageExtras.None
                        LDA # ErrorID.OKPrompt Error.Message();
                    }
                }
            }
            
            // Show ready prompt after successful execution
            Statement.IsCaptureModeOff();
            if (C)
            {
                LDX # MessageExtras.None
                LDA # ErrorID.ReadyPrompt Error.Message();
            }
        }
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
