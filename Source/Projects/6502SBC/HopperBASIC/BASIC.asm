program HopperBASIC
{
    // Optional Features
    #define PEEPHOLE    // include the peephole optimizer
    #define HASEEPROM   // include EEPROM storage
    #define HASI2C      // include 6502SBC I2C support
    
    #define RELEASE // remove all the BIT ZP.EmulatorPCL hacks (~450 bytes)
    //#define DEBUG
    //#define DEBUGPEEPS
    //#define MULDIVDEBUG
    //#define VERBOSEDEBUG // debug the keyword table limits
    //#define FILEDEBUG
    
    //#define TRACE      // Compiler and Executor call tree walks
    //#define TRACEFILE  // Storage and File
    //#define TRACEPARSE // Compiler and CompilerFlow
    //#define TRACEEXE   // instructions in Executor

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
    uses "./Definitions/OpCodes"
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
    
    uses "./Objects/Table"
    uses "./Objects/Objects"
    uses "./Objects/Variables"
    uses "./Objects/Locals"
    uses "./Objects/Functions"
    uses "./Objects/Array"
    
#ifdef HASI2C    
    uses "/Source/Runtime/6502/I2C"
#endif
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
        
        LDA #(Address.TokenizerBuffer / 256)
        STA ZP.TokenizerBuffer
        LDA #(Limits.TokenizerBufferSize / 256)
        STA ZP.TokenizerBufferSize
     
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
        // Auto-execute "AUTO" file if it exists
#ifdef HASEEPROM
        LDA #(Messages.AutoexecName % 256)
        STA ZP.STRL
        LDA #(Messages.AutoexecName / 256)  
        STA ZP.STRH
        LDA # DirWalkAction.FindExecutable
        File.Exists();
        if (C) 
        { 
            Storage.LoadProgram();
            if (C) { Console.CmdRun(); }
        }
#endif        
        
        
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
            
            STZ ZP.IDCALLL // No function has been called
            STZ ZP.IDCALLH
            
            // Process non-empty line
            Console.ProcessLine();
            
            // Check for exit first (regardless of error state)
            States.IsExiting();
            if (C) { break; } // BYE command - clean exit
            
            SEC // still ok, just not exiting
            Error.CheckError(); // but check for IsFailure or ZP.LastError ..
            if (NC)
            {
#ifdef DEBUG
                STZ ZP.STRL
                STZ ZP.STRH
                LDX #1 // DASM
                Commands.CmdList();
#endif                                    
                
                
                // Runtime Error?  (IDCALL != 0, reset to zero in ExecuteOpCodes)
                LDA ZP.IDCALLL
                ORA ZP.IDCALLH
                if (NZ)
                {
                    // runtime error inside compiled function
#ifdef DEBUG                    
Debug.NL(); LDA #'R' COut();                    
#endif
                    Functions.CompileForError();
                }
                
                // compile time error or REPL runtime error
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
                        LDA # ErrorID.OKPrompt Error.MessageNL();
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
        // Hardware break - used for BASIC BREAK functionality: NMI -> <ctrl><C>
        SMB0 ZP.SerialFlags
    }
    
    // Main entry point
    Hopper()
    {
        SEI  // Disable interrupts during initialization
        
        // Initialize the complete BASIC system
        InitializeBASIC();
        
        CLI  // Re-enable interrupts
        
        // Show startup information
#ifndef VERBOSEDEBUG        
        printStartupBanner();
#endif
        
        // Enter the main interpreter loop
        interpreterLoop();
    }
}
