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
    const byte pgmListHead = ZP.PgmListHead;      // was ZP.F5 - now uses dedicated space
    const byte pgmListHeadHi = ZP.PgmListHeadHi;   // was ZP.F6 - now uses dedicated space
    
    // Variable storage - simple array for now  
    // Each variable: [name_length] [name_chars...] [type] [value_lo] [value_hi]
    const byte varListHead = ZP.VarListHead;      // was ZP.F7 - now uses dedicated space
    const byte varListHeadHi = ZP.VarListHeadHi;   // was ZP.F8 - now uses dedicated space
    
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
        
        STZ ZP.U0  // Clear flag for variables
        listGlobals();  // Helper function
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
        
        LDA #1  // Show constants flag
        STA ZP.U0
        listGlobals();  // Helper function
    }
    
    // Helper function to list globals
    // If ZP.U0 = 0, show variables; if ZP.U0 = 1, show constants
    listGlobals()
    {
        // Walk the global list
        LDA ZP.VarListHead
        STA ZP.IDXL
        LDA ZP.VarListHeadHi
        STA ZP.IDXH
        
        loop
        {
            // Check for end of list
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Get type and value
            GlobalManager.GetGlobalValue();  // Returns type in FTYPE, value in TOP
            
            // Check if this matches what we want to show
            LDA ZP.FTYPE
            GlobalManager.IsConstant();  // Returns C=1 if constant
            
            LDA ZP.U0  // What are we showing? 0=vars, 1=consts
            if (Z)     // Showing variables
            {
                if (C) // This is a constant, skip it
                {
                    // Move to next global
                    LDY #GlobalManager.ghNext
                    LDA [ZP.IDX], Y
                    PHA
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    continue;
                }
            }
            else       // Showing constants
            {
                if (NC) // This is a variable, skip it
                {
                    // Move to next global
                    LDY #GlobalManager.ghNext
                    LDA [ZP.IDX], Y
                    PHA
                    INY
                    LDA [ZP.IDX], Y
                    STA ZP.IDXH
                    PLA
                    STA ZP.IDXL
                    continue;
                }
            }
            
            // Print the global: NAME = VALUE
            // Print name (8 chars, strip trailing spaces)
            LDY #GlobalManager.ghName
            LDX #0
            loop
            {
                CPX #8
                if (Z) { break; }
                
                LDA [ZP.IDX], Y
                CMP #' '
                if (Z) { break; }  // Stop at first space
                
                Serial.WriteChar();
                INY
                INX
            }
            
            // Print " = "
            LDA #' '
            Serial.WriteChar();
            LDA #'='
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            // Print value (already in TOP from GetGlobalValue)
            Tools.PrintDecimalWord();
            
            // Print newline
            LDA #'\n'
            Serial.WriteChar();
            
            // Move to next global
            LDY #GlobalManager.ghNext
            LDA [ZP.IDX], Y
            PHA
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDXH
            PLA
            STA ZP.IDXL
        }
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
                STZ ZP.U0  // Clear flag for variables
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
            case Tokens.IDENTIFIER:
            {
                // All statements handled by BytecodeCompiler
                cmdStatement();
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
