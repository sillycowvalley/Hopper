unit Interpreter
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "Tokenizer"
    uses "Tools"
    
    // Program storage - simple linked list for now
    // Each program line: [length] [line_number_lo] [line_number_hi] [tokenized_data...]
    const byte pgmLIST_HEAD = ZP.F5;   // Pointer to first program line (16-bit in F5/F6)
    const byte pgmLIST_HEADH = ZP.F6;
    
    // Variable storage - simple array for now  
    // Each variable: [name_length] [name_chars...] [type] [value_lo] [value_hi]
    const byte varLIST_HEAD = ZP.F7;   // Pointer to variable list (16-bit in F7/F8)
    const byte varLIST_HEADH = ZP.F8;
    
    // Messages
    const string msgReady = "READY\n> ";
    const string msgOK = "OK\n";
    const string msgSyntaxError = "?SYNTAX ERROR\n";
    const string msgMemoryCleared = "MEMORY CLEARED\n";
    const string msgVariablesCleared = "VARIABLES CLEARED\n";
    const string msgNoProgram = "NO PROGRAM\n";
    const string msgNoVariables = "NO VARIABLES\n";
    
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
    cmdNew()
    {
        // Nuclear option: reinitialize the entire heap
        // This wipes everything - programs, variables, functions, etc.
        Memory.InitializeHeapSize();
        
        // Clear our list heads since everything is gone
        STZ pgmLIST_HEAD
        STZ pgmLIST_HEADH
        STZ varLIST_HEAD
        STZ varLIST_HEADH
        
        LDA #(msgMemoryCleared % 256)
        STA ZP.IDXL
        LDA #(msgMemoryCleared / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    // Clear variables only
    cmdClear()
    {
        // Free all variable blocks  
        LDA varLIST_HEAD
        STA ZP.IDXL
        LDA varLIST_HEADH
        STA ZP.IDXH
        
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }  // End of list
            
            // Get next pointer before freeing
            LDY #0
            LDA [ZP.IDX], Y
            STA ZP.IDYL
            INY
            LDA [ZP.IDX], Y
            STA ZP.IDYH
            
            // Free current block
            Memory.Free();  // Frees block at IDX
            
            // Move to next
            LDA ZP.IDYL
            STA ZP.IDXL
            LDA ZP.IDYH
            STA ZP.IDXH
        }
        
        // Clear list head
        STZ varLIST_HEAD
        STZ varLIST_HEADH
        
        LDA #(msgVariablesCleared % 256)
        STA ZP.IDXL
        LDA #(msgVariablesCleared / 256)
        STA ZP.IDXH
        printMessage();
    }
    
    // List program
    cmdList()
    {
        LDA pgmLIST_HEAD
        STA ZP.IDXL
        LDA pgmLIST_HEADH
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
        LDA varLIST_HEAD
        STA ZP.IDXL
        LDA varLIST_HEADH
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
        
        // TODO: Implement variable listing
        // For now, just acknowledge
        printOK();
    }
    
    // Run program
    cmdRun()
    {
        LDA pgmLIST_HEAD
        STA ZP.IDXL
        LDA pgmLIST_HEADH
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
    
    // Process command line - all commands are immediate in structured BASIC
    processCommand()
    {
        Tokenizer.nextToken();  // Get first token
        
        LDA Tokenizer.tkCURRENT_TOK
        switch (A)
        {
            case Tokens.NEW:
            {
                cmdNew();
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
                cmdVars();
            }
            case Tokens.FUNCS:
            {
                cmdFuncs();
            }
            case Tokens.EOL:
            {
                // Empty line - just show ready prompt
                return;
            }
            default:
            {
                // Could be a program statement (LET, PRINT, etc.) or function definition
                // For now, treat as syntax error until we implement statement parsing
                printSyntaxError();
            }
        }
    }
    
    // Main interpreter loop
    Run()
    {
        Tokenizer.Initialize();
        
        // Clear program and variables
        STZ pgmLIST_HEAD
        STZ pgmLIST_HEADH
        STZ varLIST_HEAD
        STZ varLIST_HEADH
        
        loop
        {
            printReady();
            
            Tokenizer.ReadLine();  // Read input line
            LDA Tokenizer.tkINPUT_LEN
            if (Z) { continue; }   // Empty line
            
            processCommand();
        }
    }
}
