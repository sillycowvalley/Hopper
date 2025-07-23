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
    
    // Main expression parser - always generates bytecode and returns type in ZP.ExpressionType
    // On entry: current token should be start of expression
    // On exit: Bytecode generated to push result onto stack at runtime
    //          ZP.ExpressionType contains the type that will be produced
    ParseExpression()
    {
        // Default to invalid type
        STZ ZP.ExpressionType
        
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.NUMBER:
            {
                // Parse the number into TOP
                Tokenizer.getTokenNumber(); // Gets value in TOP
                
                // Determine type based on value range
                LDA ZP.TOPH
                if (Z)  // High byte is 0
                {
                    LDA ZP.TOPL
                    if (Z)  // Value is 0
                    {
                        LDA #Types.Int
                        STA ZP.ExpressionType
                    }
                    else
                    {
                        CMP #2
                        if (NC)  // Value <= 1
                        {
                            LDA #Types.Bool
                            STA ZP.ExpressionType
                        }
                        else
                        {
                            // Value is 2-255, use Byte type
                            LDA #Types.Byte
                            STA ZP.ExpressionType
                        }
                    }
                }
                else
                {
                    BIT ZP.TOPH
                    if (MI)  // Negative when viewed as signed
                    {
                        LDA #Types.Int
                        STA ZP.ExpressionType
                    }
                    else
                    {
                        LDA #Types.UInt
                        STA ZP.ExpressionType
                    }
                }
                
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
                
                // Emit the type
                LDA ZP.ExpressionType
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
            case Tokens.MINUS:
            {
                // Handle negative numbers: -NUMBER
                Tokenizer.nextToken();
                LDA ZP.CurrentToken
                CMP #Tokens.NUMBER
                if (Z)
                {
                    // Parse the positive number
                    Tokenizer.getTokenNumber();  // Gets positive value in TOP
                    
                    // Negate the value: TOP = 0 - TOP
                    SEC
                    LDA #0
                    SBC ZP.TOPL
                    STA ZP.TOPL
                    LDA #0
                    SBC ZP.TOPH
                    STA ZP.TOPH
                    
                    // Negative numbers are always Int type
                    LDA #Types.Int
                    STA ZP.ExpressionType
                    
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
                    
                    // Emit the type (Int for negative numbers)
                    LDA #Types.Int
                    STA ZP.NEXTL
                    LDA #0
                    STA ZP.NEXTH
                    LDA #Types.Byte
                    Stacks.PushNext();
                    FunctionManager.EmitByte();
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
                    // Get the variable info
                    GlobalManager.GetGlobalValue();  // Returns value in TOP, type in FTYPE
                    
                    // Convert GlobalManager type to runtime type for ExpressionType
                    LDA ZP.FTYPE
                    AND #0x7F     // Clear constant flag to get base variable type
                    switch (A)
                    {
                        case GlobalTypes.VarInt:
                        case GlobalTypes.ConstInt:
                        {
                            LDA #Types.Int
                            STA ZP.ExpressionType
                        }
                        case GlobalTypes.VarWord:
                        case GlobalTypes.ConstWord:
                        {
                            LDA #Types.UInt
                            STA ZP.ExpressionType
                        }
                        case GlobalTypes.VarByte:
                        case GlobalTypes.ConstByte:
                        {
                            LDA #Types.Byte
                            STA ZP.ExpressionType
                        }
                        case GlobalTypes.VarBit:
                        case GlobalTypes.ConstBit:
                        {
                            LDA #Types.Bool
                            STA ZP.ExpressionType
                        }
                        case GlobalTypes.VarString:
                        case GlobalTypes.ConstString:
                        {
                            LDA #Types.String
                            STA ZP.ExpressionType
                        }
                        default:
                        {
                            LDA #Types.UInt  // Default
                            STA ZP.ExpressionType
                        }
                    }
                    
                    // Check if it's a constant - if so, emit OpPushInt, otherwise OpLoadVar
                    LDA ZP.FTYPE
                    GlobalManager.IsConstant();
                    if (C)  // It's a constant
                    {
                        // Emit OpPushInt for constant value
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
                        
                        // Emit the type
                        LDA ZP.ExpressionType
                        STA ZP.NEXTL
                        LDA #0
                        STA ZP.NEXTH
                        LDA #Types.Byte
                        Stacks.PushNext();
                        FunctionManager.EmitByte();
                    }
                    else
                    {
                        // Emit OpLoadVar for variable
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
                // String literals not implemented - set error
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
    // Returns type in ZP.ExpressionType, generates bytecode
    // For constants, also validates that expression can be evaluated at compile time
    ParseConstantExpression()
    {
        ParseExpression();
        
        CheckError();
        if (NZ) { return; }  // Expression parsing failed
        
        // For now, we accept any expression as "constant" since we're generating bytecode
        // In a more sophisticated implementation, we could track whether the expression
        // contains only compile-time constants vs runtime variables
        
        // Expression type is already set in ZP.ExpressionType by ParseExpression
    }
    
    // Wrapper for runtime expression parsing (statements like PRINT, assignment)  
    // Returns type in ZP.ExpressionType, generates bytecode
    ParseRuntimeExpression()
    {
        ParseExpression();
        // Expression type is already set in ZP.ExpressionType by ParseExpression
    }
}
