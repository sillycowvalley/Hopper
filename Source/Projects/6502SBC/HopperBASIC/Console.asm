unit Console
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "Messages"
    uses "Tools"
    uses "Tokenizer"
    uses "Statement"
    
    uses "Objects"
    
    // String constants for VARS command
    const string noVariablesMsg = "No variables defined\n";
    const string spaceOpenBracket = " (";
    const string closeBracketSpaceEqualsSpace = ") = ";
    const string varTypeMsg = "VAR";
    const string constTypeMsg = "CONST";
    const string intTypeMsg = "INT";
    const string wordTypeMsg = "WORD";
    const string bitTypeMsg = "BIT";
    
    Initialize()
    {
        // Initialize tokenizer
        Tokenizer.Initialize();
        
        // Initialize symbol tables
        Objects.Initialize();
    }
    
    // Read a line of input and tokenize it
    ReadLine()
    {
        Tokenizer.ReadLine();    // Read into BasicInputBuffer, sets ZP.BasicInputLength
        Tokenizer.TokenizeLine(); // Tokenize into BasicTokenizerBuffer
        Messages.CheckError();
        if (NC) { return; }  // Return if tokenization failed
    }
    
    // Execute MEM command
    CmdMem()
    {
        LDA #(Messages.MemoryMsg % 256)
        STA ZP.IDXL
        LDA #(Messages.MemoryMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Get available memory
        Memory.Available();  // Pushes available memory (UInt) to stack
        Stacks.PopTop();     // Pop into TOP
        Tools.PrintDecimalWord();
        
        LDA #(Messages.BytesMsg % 256)
        STA ZP.IDXL
        LDA #(Messages.BytesMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
    
    // Execute BYE command
    cmdBye()
    {
        // NOP
    }
    
    // Execute NEW command
    cmdNew()
    {
        HopperBASIC.InitializeBASIC();
        Messages.PrintOK();
    }
    
    // Execute LIST command
    cmdList()
    {
        // TODO: List program
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BRK
    }
    
    // Execute RUN command
    cmdRun()
    {
        // TODO: Run program
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BRK
    }
    
    // Execute CLEAR command
    cmdClear()
    {
        // TODO: Clear variables
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BRK
    }
    
    // Execute FUNCS command
    cmdFuncs()
    {
        // TODO: Show functions
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BRK
    }
    
    
    // Execute VARS command - display all variables and constants
    cmdVars()
    {
        PHA
        PHX
        PHY
        
        // Save ZP.IDX since we'll use it for iteration
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Save ZP.ACC for our use
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Start iteration over all variables and constants
        Variables.IterateAll();
        
        if (NC)
        {
            // No variables found
            LDA #(noVariablesMsg % 256)
            STA ZP.IDXL
            LDA #(noVariablesMsg / 256)
            STA ZP.IDXH
            Tools.PrintString();
            
            JMP varsExit
        }
        
        loop
        {
            // Get variable signature (type, value, tokens)
            Variables.GetSignature();
            
            if (NC) { break; }  // Error getting signature
            
            // Get variable name
            Variables.GetName();
            
            // Print variable name (ZP.ACC points to name string)
            LDA ZP.ACCL
            STA ZP.ACCT
            STA ZP.IDXL
            LDA ZP.ACCH
            STA ZP.IDXH
            Tools.PrintString();
            
            LDA #(spaceOpenBracket % 256)
            STA ZP.IDXL
            LDA #(spaceOpenBracket / 256)
            STA ZP.IDXH
            Tools.PrintString();
            
            // Extract and print symbol type (VARIABLE or CONSTANT)
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            CMP # (SymbolType.VARIABLE << 4)
            if (Z)
            {
                LDA #(varTypeMsg % 256)
                STA ZP.IDXL
                LDA #(varTypeMsg / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            else
            {
                LDA #(constTypeMsg % 256)
                STA ZP.IDXL
                LDA #(constTypeMsg / 256)
                STA ZP.IDXH
                Tools.PrintString();
            }
            
            LDA #':'
            Serial.WriteChar();
            
            // Extract and print data type (low nibble)
            LDA ZP.ACCT
            AND #0x0F  // Extract data type (low nibble)
            
            switch (A)
            {
                case BasicType.INT:
                {
                    LDA #(intTypeMsg % 256)
                    STA ZP.IDXL
                    LDA #(intTypeMsg / 256)
                    STA ZP.IDXH
                    Tools.PrintString();
                }
                case BasicType.WORD:
                {
                    LDA #(wordTypeMsg % 256)
                    STA ZP.IDXL
                    LDA #(wordTypeMsg / 256)
                    STA ZP.IDXH
                    Tools.PrintString();
                }
                case BasicType.BIT:
                {
                    LDA #(bitTypeMsg % 256)
                    STA ZP.IDXL
                    LDA #(bitTypeMsg / 256)
                    STA ZP.IDXH
                    Tools.PrintString();
                }
                default:
                {
                    LDA #'?'
                    Serial.WriteChar();
                }
            }
            
            LDA #(closeBracketSpaceEqualsSpace % 256)
            STA ZP.IDXL
            LDA #(closeBracketSpaceEqualsSpace / 256)
            STA ZP.IDXH
            Tools.PrintString();
            
            // Print current value (ZP.IDY contains the value from GetSignature)
            LDA ZP.IDYL
            STA ZP.TOPL
            LDA ZP.IDYH
            STA ZP.TOPH
            
            LDA ZP.ACCT
            AND #0x0F  // Extract data type
            STA ZP.TOPT
            
            Tools.PrintDecimalWord();
            
            LDA #'\n'
            Serial.WriteChar();
            
            // Move to next variable
            Variables.IterateNext();
            if (NC) { break; }  // No more variables
        }
        
    varsExit:
        // Restore ZP.ACC
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        // Restore ZP.IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }
    
    
    
    
    // Process current tokenized line
    // Returns C if should continue, NC if should exit
    ProcessLine()
    {
        // Check for tokenization errors
        Messages.CheckError();
        if (NC) { return; }  // Error during tokenization
        
        // Check for empty line (just EOL token)
        LDA ZP.TokenBufferLengthL
        CMP #1
        if (NZ) 
        { 
            // More than one token, process normally
            processTokens();
            return;
        }
        
        LDA ZP.TokenBufferLengthH
        if (NZ)
        {
            // Definitely more than one token
            processTokens();
            return;
        }
        
        // Exactly one token - check if it's EOL by getting first token
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        Tokenizer.NextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            SEC  // Continue (empty line)
            return;
        }
        
        // Single non-EOL token, reset position and process it
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        processTokens();
    }
    
    // Process the tokens in BasicTokenizerBuffer
    // Returns C to continue, NC to exit
    processTokens()
    {
        SEC  // not BYE
        
        // Get first token
        Tokenizer.NextToken();  // Returns token in A, updates ZP.CurrentToken
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
            case Tokens.VARS:
            {
                cmdVars();
            }
            case Tokens.FUNCS:
            {
                cmdFuncs();
            }
            case Tokens.MEM:
            {
                CmdMem();
            }
            case Tokens.BYE:
            {
                cmdBye();
                CLC  // Exit
                return;
            }
            case Tokens.EOL:
            case Tokens.EOF:
            {
                // Empty line - just continue
            }
            case Tokens.FORGET:
            case Tokens.SAVE:
            case Tokens.LOAD:
            case Tokens.DIR:
            case Tokens.DEL:
            {
                LDA #(Messages.NotImplemented % 256)
                STA ZP.LastErrorL
                LDA #(Messages.NotImplemented / 256)
                STA ZP.LastErrorH
                BRK
            }
            default:
            {
                // Not a console command, try to execute as a statement
                Statement.Execute();
            }
        }
    }
}
