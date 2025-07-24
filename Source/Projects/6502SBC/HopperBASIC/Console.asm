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
    
    Initialize()
    {
        // Initialize tokenizer
        Tokenizer.Initialize();
    }
    
    // Read a line of input and tokenize it
    ReadLine()
    {
        Tokenizer.ReadLine();    // Read into BasicInputBuffer, sets ZP.BasicInputLength
        Tokenizer.TokenizeLine(); // Tokenize into BasicTokenizerBuffer
        Messages.CheckError();
        if (NC) { return; }  // Return if tokenization failed
        
// Optional debug output (remove when working)
#ifdef DEBUG
        Tools.DumpBasicBuffers();
#endif
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
    
    // Execute VARS command
    cmdVars()
    {
        // TODO: Show variables
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
        
        SEC  // Continue
    }
}