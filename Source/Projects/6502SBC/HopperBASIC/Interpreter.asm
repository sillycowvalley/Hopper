unit Interpreter
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "Tokenizer"
    uses "Tools"
    uses "FunctionManager"
    uses "BytecodeCompiler"
    uses "BytecodeExecutor"
    uses "GlobalManager"
    
    enum ExprTypes
    {
        INVALID = 0,
        INT     = 1,    // 16-bit signed (-32768 to 32767)
        WORD    = 2,    // 16-bit unsigned (0 to 65535) 
        BYTE    = 3,    // 8-bit unsigned (0 to 255)
        BIT     = 4,    // Boolean (0 or 1)
        STRING  = 5,    // String literals and variables
    }
    
    // Program storage - simple linked list for now
    // Each program line: [length] [line_number_lo] [line_number_hi] [tokenized_data...]
    const byte pgmListHead = ZP.PgmListHead;      // Now uses dedicated space
    const byte pgmListHeadHi = ZP.PgmListHeadHi;
    
    // Variable storage - simple array for now  
    // Each variable: [name_length] [name_chars...] [type] [value_lo] [value_hi]
    const byte varListHead = ZP.VarListHead;      // Now uses dedicated space
    const byte varListHeadHi = ZP.VarListHeadHi;
    
    // Messages
    const string msgReady = "READY\n> ";
    const string msgOK = "OK\n";
    const string msgSyntaxError = "?SYNTAX ERROR\n";
    const string msgMemoryCleared = "MEMORY CLEARED\n";
    const string msgVariablesCleared = "VARIABLES CLEARED\n";
    const string msgNoProgram = "NO PROGRAM\n";
    const string msgNoVariables = "NO VARIABLES\n";
    const string msgNoConstants = "NO CONSTANTS\n";
    const string msgGoodbye = "GOODBYE\n";
    const string msgVariablesHeader = "VARIABLES:\n";
    const string msgConstantsHeader = "CONSTANTS:\n";
    
    printMessage()
    {
        // IDX points to message string
        Tools.PrintString();
    }
    
    printReady()
    {
        LDA #(msgReady % 256)
        STA ZP.IDXL
        LDA #(msgReady / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    printOK()
    {
        LDA #(msgOK % 256)
        STA ZP.IDXL
        LDA #(msgOK / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    printSyntaxError()
    {
        LDA #(msgSyntaxError % 256)
        STA ZP.IDXL
        LDA #(msgSyntaxError / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    // Clear all program memory - just reinitialize the heap for clean start
    CmdNew()
    {
        // Nuclear option: reinitialize the entire heap
        // This wipes everything - programs, variables, functions, etc.
        Memory.InitializeHeapSize();
        
        // Initialize stacks
        Stacks.Initialize(); 
        
        // Clear flags and set up basic state
        STZ ZP.FLAGS
        SMB0 ZP.FLAGS  // Program loaded flag
        
        // Clear our list heads since everything is gone
        STZ pgmListHead
        STZ pgmListHeadHi
        STZ varListHead
        STZ varListHeadHi
        
        // Reinitialize function and global managers
        FunctionManager.Initialize();
        GlobalManager.Initialize();
        
        LDA #(msgMemoryCleared % 256)
        STA ZP.IDXL
        LDA #(msgMemoryCleared / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    // Clear variables only
    cmdClear()
    {
        GlobalManager.ClearVariables();
        
        LDA #(msgVariablesCleared % 256)
        STA ZP.IDXL
        LDA #(msgVariablesCleared / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    // List program
    cmdList()
    {
        LDA pgmListHead
        STA ZP.IDXL
        LDA pgmListHeadHi
        STA ZP.IDXH
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            LDA #(msgNoProgram % 256)
            STA ZP.IDXL
            LDA #(msgNoProgram / 256)
            STA ZP.IDXH
            printMessage();
            return;
        }
        
        // TODO: Implement program listing
        // For now, just acknowledge
        printOK();
    }
    
    // Show variables
    cmdVars()
    {
        // Walk through globals and show only variables (not constants)
        LDA ZP.VarListHead
        STA ZP.IDXL
        LDA ZP.VarListHeadHi
        STA ZP.IDXH
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            LDA #(msgNoVariables % 256)
            STA ZP.IDXL
            LDA #(msgNoVariables / 256)
            STA ZP.IDXH
            printMessage();
            return;
        }
        
        // Print header
        LDA #(msgVariablesHeader % 256)
        STA ZP.IDXL
        LDA #(msgVariablesHeader / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        STZ ZP.BasicFlags  // Clear flag for variables (was U0)
        GlobalManager.ListGlobals();
    }
    
    // Show constants
    cmdConsts()
    {
        // Walk through globals and show only constants (not variables)
        LDA ZP.VarListHead
        STA ZP.IDXL
        LDA ZP.VarListHeadHi
        STA ZP.IDXH
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            LDA #(msgNoConstants % 256)
            STA ZP.IDXL
            LDA #(msgNoConstants / 256)
            STA ZP.IDXH
            printMessage();
            return;
        }
        
        // Print header
        LDA #(msgConstantsHeader % 256)
        STA ZP.IDXL
        LDA #(msgConstantsHeader / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        LDA #1  // Set constants flag
        STA ZP.BasicFlags
        GlobalManager.ListGlobals();
    }
    
    // Run program
    cmdRun()
    {
        LDA pgmListHead
        STA ZP.IDXL
        LDA pgmListHeadHi
        STA ZP.IDXH
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            LDA #(msgNoProgram % 256)
            STA ZP.IDXL
            LDA #(msgNoProgram / 256)
            STA ZP.IDXH
            printMessage();
            return;
        }
        
        // TODO: Implement program execution
        // For now, just acknowledge
        printOK();
    }
    
    // Save/Load/Dir/Del - EEPROM operations (stub for now)
    cmdSave()
    {
        // TODO: Expect string parameter for filename
        printOK();
    }
    
    cmdLoad()
    {
        // TODO: Expect string parameter for filename
        printOK();
    }
    
    cmdDir()
    {
        // TODO: List EEPROM directory
        printOK();
    }
    
    cmdDel()
    {
        // TODO: Expect string parameter for filename
        printOK();
    }
    
    cmdFuncs()
    {
        // TODO: List defined functions
        printOK();
    }
    
    cmdBye()
    {
        // Could print farewell message
        LDA #(msgGoodbye % 256)
        STA ZP.IDXL
        LDA #(msgGoodbye / 256) 
        STA ZP.IDXH
        printMessage();
    }
    
    cmdNOP()
    {
        // Do nothing (for empty lines)
    }
    
    // All statement parsing now handled by BytecodeCompiler
    cmdStatement()
    {
        STZ ZP.TokenizerPos
        BytecodeCompiler.CompileREPLStatement();
        
        BytecodeExecutor.ExecuteREPLFunction();
        
        FunctionManager.CleanupREPLFunction();
    }
    
    cmdIdentifier()
    {
        // Look ahead to see if this is an assignment
        LDX ZP.TokenizerPos
        STX ZP.BasicTempPos  // Save current position (was U1)
        Tokenizer.nextToken();  // Get next token
        LDA ZP.CurrentToken
        LDX ZP.BasicTempPos  // Restore position
        STX ZP.TokenizerPos
        
        CMP #Tokens.EQUALS
        if (Z)
        {
            cmdStatement();  // It's an assignment
        }
        else
        {
            printSyntaxError();  // Unknown identifier
        }
    }
    
    // Process command line - all commands are immediate in structured BASIC
    processCommand()
    {
        // Reset tokenizer to start of input
        STZ ZP.TokenizerPos
        
        Tokenizer.nextToken();  // Get first token
        
        // Note: for this switch to optimize to a small jump table, the constant values of the
        //       case labels should be contiguous (no gaps)
        LDX ZP.CurrentToken
        switch (X)
        {
            case Tokens.BYE:
            {
                cmdBye();
            }
            case Tokens.NEW:
            {
                CmdNew();
            }
            case Tokens.LIST:
            {
                cmdList();
            }
            case Tokens.RUN:
            {
                cmdRun();
            }
            case Tokens.CLEAR:
            {
                cmdClear();
            }
            case Tokens.SAVE:
            {
                cmdSave();
            }
            case Tokens.LOAD:
            {
                cmdLoad();
            }
            case Tokens.DIR:
            {
                cmdDir();
            }
            case Tokens.DEL:
            {
                cmdDel();
            }
            case Tokens.VARS:
            {
                STZ ZP.BasicFlags  // Clear flag for variables
                cmdVars();
            }
            case Tokens.CONSTS:
            {
                cmdConsts();
            }
            case Tokens.FUNCS:
            {
                cmdFuncs();
            }
            case Tokens.EOL:
            {
                // Empty line - just show ready prompt
                cmdNOP(); // need method call so this switch is optimized to a jump table
            }
            case Tokens.PRINT:
            case Tokens.CONST:
            case Tokens.IntType:
            case Tokens.WordType:
            case Tokens.ByteType:
            case Tokens.BitType:
            case Tokens.StringType:
            {
                cmdStatement();
            }
            
            case Tokens.IDENTIFIER:
            {
                cmdIdentifier();
            }
            default:
            {
                // Unknown command/statement
                printSyntaxError();
            }
        }
    }
    
    // Main interpreter loop
    Run()
    {
        Tokenizer.Initialize();
        FunctionManager.Initialize();
        GlobalManager.Initialize();
        
        // Clear program and variables
        STZ pgmListHead
        STZ pgmListHeadHi
        STZ varListHead
        STZ varListHeadHi
        
        loop
        {
            printReady();
            
            Tokenizer.ReadLine();  // Read input line
            LDA ZP.BasicInputLength
            if (Z) { continue; }   // Empty line
            
            processCommand();
            
            // Check if BYE was entered
            LDA ZP.CurrentToken
            CMP #Tokens.BYE
            if (Z) { break; }      // Exit the interpreter loop
        }
    }
}
