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
    
    // Read a line of input using the tokenizer
    ReadLine()
    {
        Tokenizer.ReadLine();  // Returns length in A, sets ZP.BasicInputLength
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
        InitializeBASIC();
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
        PHA
        Messages.CheckError();
        if (NZ)
        {
            PLA
            return; // error in tokenizer
        }
        PLA
        
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
                    CmdMem();
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
                default:
                {
                    // Not a console command, try to execute as a statement
                    Statement.Execute();
                }
            }
            LDA #0  // Continue
            return;
        }
        LDA #1  // Exit
        return;
    }
}
