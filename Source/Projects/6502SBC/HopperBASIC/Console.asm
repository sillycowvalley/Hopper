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
    
    Initialize()
    {
        // Initialize tokenizer
        Tokenizer.Initialize();
    }
    
    // Read a line of input using the tokenizer
    ReadLine()
    {
        Tokenizer.ReadLine();  // Returns length in A, sets ZP.BasicInputLength
    }
    
    // Execute MEM command
    cmdMem()
    {
        LDA #(MemoryMsg % 256)
        STA ZP.IDXL
        LDA #(MemoryMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Get available memory
        Memory.Available();  // Pushes available memory (UInt) to stack
        Stacks.PopTop();     // Pop into TOP
        Tools.PrintDecimalWord();
        
        LDA #(BytesMsg % 256)
        STA ZP.IDXL
        LDA #(BytesMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
    
    // Execute BYE command
    cmdBye()
    {
        LDA #(Goodbye % 256)
        STA ZP.IDXL
        LDA #(Goodbye / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
    
    // Execute NEW command
    cmdNew()
    {
        // TODO: Clear program and variables
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
    }
    
    // Execute LIST command
    cmdList()
    {
        // TODO: List program
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
    }
    
    // Execute RUN command
    cmdRun()
    {
        // TODO: Run program
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
    }
    
    // Execute CLEAR command
    cmdClear()
    {
        // TODO: Clear variables
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
    }
    
    // Execute VARS command
    cmdVars()
    {
        // TODO: Show variables
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
    }
    
    // Execute FUNCS command
    cmdFuncs()
    {
        // TODO: Show functions
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
    }
    
    // Process current input line using tokenizer
    // Returns Z if should continue, NZ if should exit
    ProcessLine()
    {
        // Check for empty line
        LDA ZP.BasicInputLength
        if (Z) 
        { 
            LDA #0  // Continue
            return;
        }
        
        // Get first token
        Tokenizer.NextToken();  // Returns token in A, updates ZP.CurrentToken
        CheckError();
        if (NZ)
        {
            return; // error in tokenizer
        }
        
        DumpVariables();
        DumpBasicBuffers();
        
        loop
        {
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
                    cmdMem();
                }
                case Tokens.BYE:
                {
                    cmdBye();
                    break;
                }
                case Tokens.EOL:
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
                }
                case Tokens.INT:
                case Tokens.WORD:
                case Tokens.BIT:
                case Tokens.PRINT:
                case Tokens.IF:
                case Tokens.FUNC:
                case Tokens.BEGIN:
                {
                    LDA #(Messages.NotImplemented % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.NotImplemented / 256)
                    STA ZP.LastErrorH
                }
                default:
                {
                    // Unknown token or syntax error
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                }
            }
            LDA #0  // Continue
            return;
        }
        LDA #1  // Exit
        return;
    }
}
