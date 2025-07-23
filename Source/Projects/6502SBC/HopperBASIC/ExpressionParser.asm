unit ExpressionParser
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Tokenizer"
    uses "FunctionManager"
    uses "GlobalManager"
    
    uses "Opcodes"
    
    friend StatementCompiler, BytecodeCompiler;
    
    // Emit the current token name (from BasicWorkBuffer) to bytecode
    emitTokenName()
    {
        // Calculate string length first from workspace buffer
        LDX #0
        loop
        {
            LDA Address.BasicWorkBuffer, X
            if (Z) { break; }
            INX
        }
        
        // Emit length byte first
        TXA
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Emit each character of the token name from workspace buffer
        LDX #0
        loop
        {
            LDA Address.BasicWorkBuffer, X
            if (Z) { break; }  // Hit null terminator
            
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            
            INX
        }
    }
    
    // Unified expression parser - handles both constant and runtime expressions
    // Uses bit 1 of BasicFlags to track if expression is constant
    // On entry: current token should be start of expression
    // On exit: If constant (bit 1 set): value in TOP, no bytecode emitted
    //          If not constant (bit 1 clear): bytecode emitted for runtime evaluation
    ParseExpression()
    {
        // Set constant flag - assume expression is constant until proven otherwise
        SMB1 ZP.BasicFlags
        
        // Default value is 0
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.NUMBER:
            {
                // Numbers are always constant - flag stays set
                Tokenizer.getTokenNumber(); // Gets value in TOP
                
                // If we're emitting code (someone else cleared the flag), emit push instruction
                if (BBR1, ZP.BasicFlags)
                {
                    // Emit OpPushInt followed by the 16-bit value and type
                    LDA #OpCode.OpPushInt
                    STA ZP.NEXTL
                    LDA #0
                    STA ZP.NEXTH
                    LDA #Types.Byte
                    Stacks.PushNext();
                    FunctionManager.EmitByte();
                    
                    // Emit the constant value (16-bit)
                    LDA #Types.UInt
                    Stacks.PushTop();
                    FunctionManager.EmitWord();
                    
                    // Determine and emit type based on value range
                    LDA ZP.TOPH
                    if (Z)  // High byte is 0
                    {
                        LDA ZP.TOPL
                        if (Z)  // Value is 0
                        {
                            LDA #Types.Int
                        }
                        else
                        {
                            CMP #2
                            if (NC)  // Value <= 1
                            {
                                LDA #Types.Bool
                            }
                            else
                            {
                                // Value is 2-255, use Byte type
                                LDA #Types.Byte
                            }
                        }
                    }
                    else
                    {
                        BIT ZP.TOPH
                        if (MI)  // Negative when viewed as signed
                        {
                            LDA #Types.Int
                        }
                        else
                        {
                            LDA #Types.UInt
                        }
                    }
                    
                    STA ZP.NEXTL
                    LDA #0
                    STA ZP.NEXTH
                    LDA #Types.Byte
                    Stacks.PushNext();
                    FunctionManager.EmitByte();
                }
            }
            case Tokens.MINUS:
            {
                // Handle negative numbers: -NUMBER
                Tokenizer.nextToken();
                LDA ZP.CurrentToken
                CMP #Tokens.NUMBER
                if (Z)
                {
                    // It's a negative number - flag stays set (still constant)
                    Tokenizer.getTokenNumber();  // Gets positive value in TOP
                    
                    // Negate the value: TOP = 0 - TOP
                    SEC
                    LDA #0
                    SBC ZP.TOPL
                    STA ZP.TOPL
                    LDA #0
                    SBC ZP.TOPH
                    STA ZP.TOPH
                    
                    // If we're emitting code, emit push instruction
                    if (BBR1, ZP.BasicFlags)
                    {
                        // Emit OpPushInt followed by the 16-bit value and type
                        LDA #OpCode.OpPushInt
                        STA ZP.NEXTL
                        LDA #0
                        STA ZP.NEXTH
                        LDA #Types.Byte
                        Stacks.PushNext();
                        FunctionManager.EmitByte();
                        
                        // Emit the negative constant value (16-bit)
                        LDA #Types.UInt
                        Stacks.PushTop();
                        FunctionManager.EmitWord();
                        
                        // Negative numbers are always Int type
                        LDA #Types.Int
                        STA ZP.NEXTL
                        LDA #0
                        STA ZP.NEXTH
                        LDA #Types.Byte
                        Stacks.PushNext();
                        FunctionManager.EmitByte();
                    }
                }
                else
                {
                    // Invalid: minus without number - set error
                    LDA #(Interpreter.msgInvalidExpression % 256)
                    STA ZP.LastErrorL
                    LDA #(Interpreter.msgInvalidExpression / 256)
                    STA ZP.LastErrorH
                    return;
                }
            }
            case Tokens.IDENTIFIER:
            {
                // Variable reference - look it up
                LDA #(Address.BasicWorkBuffer & 0xFF)
                STA ZP.IDYL
                LDA #(Address.BasicWorkBuffer >> 8)
                STA ZP.IDYH
                
                GlobalManager.FindGlobal();
                if (Z)  // Found
                {
                    // Check if it's a constant
                    GlobalManager.GetGlobalValue();  // Returns value in TOP, type in FTYPE
                    LDA ZP.FTYPE
                    GlobalManager.IsConstant();
                    if (C)  // It's a constant - flag stays set
                    {
                        // Value is already in TOP, we're done for constants
                    }
                    else
                    {
                        // It's a variable - clear constant flag
                        RMB1 ZP.BasicFlags
                    }
                    
                    // If we're emitting code (flag was cleared or we started with it clear)
                    if (BBR1, ZP.BasicFlags)
                    {
                        // Emit OpLoadVar followed by the variable name
                        LDA #OpCode.OpLoadVar
                        STA ZP.NEXTL
                        LDA #0
                        STA ZP.NEXTH
                        LDA #Types.Byte
                        Stacks.PushNext();
                        FunctionManager.EmitByte();
                        
                        // Emit the variable name
                        emitTokenName();
                    }
                }
                else
                {
                    // Variable not found - set error
                    LDA #(Interpreter.msgUndefinedVariable % 256)
                    STA ZP.LastErrorL
                    LDA #(Interpreter.msgUndefinedVariable / 256)
                    STA ZP.LastErrorH
                    return;
                }
            }
            case Tokens.STRING:
            {
                // String literals not implemented - clear flag and set error
                RMB1 ZP.BasicFlags
                LDA #(Interpreter.msgInvalidExpression % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgInvalidExpression / 256)
                STA ZP.LastErrorH
                return;
            }
            case Tokens.EOL:
            {
                // Missing expression - set error
                LDA #(Interpreter.msgMissingExpression % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgMissingExpression / 256)
                STA ZP.LastErrorH
                return;
            }
            default:
            {
                // Invalid expression - set error
                LDA #(Interpreter.msgInvalidExpression % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgInvalidExpression / 256)
                STA ZP.LastErrorH
                return;
            }
        }
    }
    
    // Wrapper for constant expression parsing (variable initialization)
    // Returns value in TOP, sets error if invalid or if expression is not constant
    ParseConstantExpression()
    {
        ParseExpression();
        CheckError();
        if (NZ) { return; }  // Expression parsing failed
        
        // Check if expression was constant
        if (BBR1, ZP.BasicFlags)
        {
            // Expression was not constant - set error
            LDA #( Interpreter.msgInvalidExpression % 256)
            STA ZP.LastErrorL
            LDA #( Interpreter.msgInvalidExpression / 256)
            STA ZP.LastErrorH
            return;
        }
        // Expression was constant - value is already in TOP
    }
    
    // Wrapper for runtime expression parsing (statements like PRINT, assignment)  
    // Forces bytecode emission regardless of whether expression is constant
    ParseRuntimeExpression()
    {
        // Clear constant flag to force bytecode emission
        RMB1 ZP.BasicFlags
        ParseExpression();
        // Don't check the constant flag - we always want bytecode for runtime expressions
    }
}
