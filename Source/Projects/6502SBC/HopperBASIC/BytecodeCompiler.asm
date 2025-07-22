unit BytecodeCompiler
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Tokenizer"
    uses "FunctionManager"
    uses "GlobalManager"
    
    // Bytecode opcodes
    enum Opcodes 
    {
        OpNop        = 0x00,
        OpPushInt    = 0x01,  // + 2 bytes: 16-bit value
        OpPrintInt   = 0x02,  // Print TOS as integer
        OpPrintStr   = 0x03,  // Print TOS as string literal
        OpPrintNL    = 0x04,  // Print newline
        OpReturn     = 0x05,  // Return from function
        OpLoadVar    = 0x06,  // Load variable value to stack
        OpStoreVar   = 0x07,  // Store TOS to variable
        OpHalt       = 0xFF,  // End of REPL function
    }
    
    // Emit 8-byte variable name from FSOURCEADDRESS to bytecode
    emitVariableName()
    {
        LDY #0
        loop
        {
            CPY #8
            if (Z) { break; }
            
            LDA [ZP.FSOURCEADDRESS], Y
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            
            INY
        }
    }
    
    // Compile a PRINT statement
    // Assumes current token is PRINT
    compilePrintStatement()
    {
        // Move to next token (the expression to print)
        Tokenizer.nextToken();
        
        // Compile the expression (puts result on stack)
        compileExpression();
        
        // Emit OpPrintInt to print the result
        LDA #Opcodes.OpPrintInt
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Move to next token to check for end of statement
        Tokenizer.nextToken();
        
        // If we hit EOL, add a newline print
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            LDA #Opcodes.OpPrintNL
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
        }
    }
    
    // Compile an assignment statement: A = expression
    // Assumes current token is IDENTIFIER
    compileAssignmentStatement()
    {
        // Get variable name and look it up
        LDA #ZP.BasicWorkspace0  // Use dedicated BASIC workspace instead of W0
        STA ZP.FSOURCEADDRESSL
        LDA #0
        STA ZP.FSOURCEADDRESSH
        
        Tokenizer.getTokenName();  // Save variable name
        
        // Look up the variable for validation
        GlobalManager.FindGlobal();
        if (NZ)  // Not found
        {
            // Variable doesn't exist - emit NOP and return
            LDA #Opcodes.OpNop
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            return;
        }
        
        // Check if it's a constant (can't assign to constants)
        GlobalManager.GetGlobalValue();  // Returns type in FTYPE
        LDA ZP.FTYPE
        GlobalManager.IsConstant();
        if (C)  // It's a constant
        {
            // Can't assign to constant - emit NOP and return
            LDA #Opcodes.OpNop
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            return;
        }
        
        // Move to next token - should be EQUALS
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.EQUALS
        if (NZ)
        {
            // Not an assignment - emit NOP and return
            LDA #Opcodes.OpNop
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            return;
        }
        
        // Move to expression
        Tokenizer.nextToken();
        
        // Check if we have an expression
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // Missing expression after = - emit NOP and return
            LDA #Opcodes.OpNop
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            return;
        }
        
        // Compile expression (puts result on stack)
        compileExpression();
        
        // Emit OpStoreVar followed by the 8-byte variable name
        LDA #Opcodes.OpStoreVar
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Emit the 8-byte variable name
        emitVariableName();
    }
    
    // Compile variable declaration: [CONST] TYPE NAME [= expression]
    // Current token is the type token, isConst flag indicates if CONST was seen
    compileVariableDeclaration()
    {
        // Current token is the type - convert to GlobalManager type
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.IntType:
            {
                LDA ZP.BasicFlags  // isConst flag (was U0)
                if (Z)
                {
                    LDA #GlobalManager.GlobalTypes.VarInt
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.ConstInt
                }
                STA ZP.FTYPE
            }
            case Tokens.WordType:
            {
                LDA ZP.BasicFlags  // isConst flag
                if (Z)
                {
                    LDA #GlobalManager.GlobalTypes.VarWord
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.ConstWord
                }
                STA ZP.FTYPE
            }
            case Tokens.ByteType:
            {
                LDA ZP.BasicFlags  // isConst flag
                if (Z)
                {
                    LDA #GlobalManager.GlobalTypes.VarByte
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.ConstByte
                }
                STA ZP.FTYPE
            }
            case Tokens.BitType:
            {
                LDA ZP.BasicFlags  // isConst flag
                if (Z)
                {
                    LDA #GlobalManager.GlobalTypes.VarBit
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.ConstBit
                }
                STA ZP.FTYPE
            }
            case Tokens.StringType:
            {
                LDA ZP.BasicFlags  // isConst flag
                if (Z)
                {
                    LDA #GlobalManager.GlobalTypes.VarString
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.ConstString
                }
                STA ZP.FTYPE
            }
            default:
            {
                // Invalid type - emit NOP and return
                LDA #Opcodes.OpNop
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
                return;
            }
        }
        
        // Get variable name
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.IDENTIFIER
        if (NZ)
        {
            // Invalid syntax - emit NOP and return
            LDA #Opcodes.OpNop
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            return;
        }
        
        // Set up name buffer
        LDA #ZP.BasicWorkspace0
        STA ZP.FSOURCEADDRESSL
        LDA #0
        STA ZP.FSOURCEADDRESSH
        
        Tokenizer.getTokenName();  // Returns 8-byte padded name at FSOURCEADDRESS
        
        // Default value is 0
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        // Check for optional initializer
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.EQUALS
        if (Z)
        {
            // Has initializer - compile the expression
            Tokenizer.nextToken();
            compileExpression();
            
            // The expression result is now on the runtime stack
            // We need to get it into ZP.TOP for GlobalManager.AddGlobal()
            // For now, we'll emit code to execute the expression and capture result
            // This is a bit of a hack - we're mixing compile-time and runtime evaluation
            
            // TODO: This needs proper handling
            // For now, just use a simple number if it was a number token
            LDA ZP.CurrentToken
            CMP #Tokens.NUMBER
            if (Z)
            {
                Tokenizer.getTokenNumber(); // Gets value in TOP
            }
            else
            {
                // For other expressions, default to 0 for now
                STZ ZP.TOPL
                STZ ZP.TOPH
            }
        }
        else
        {
            // Check if this is a constant without initializer (error)
            LDA ZP.BasicFlags  // isConst flag
            if (NZ)
            {
                // Constants MUST have initializer
                LDA #Opcodes.OpNop
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
                return;
            }
        }
        
        // Add the variable/constant to GlobalManager
        GlobalManager.AddGlobal();
        
        // Emit NOP as placeholder (declaration doesn't generate runtime code)
        LDA #Opcodes.OpNop
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
    }
    
    // Compile an expression (number, variable, arithmetic, etc.)
    // Leaves result on the value stack
    compileExpression()
    {
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.NUMBER:
            {
                // Load number constant
                Tokenizer.getTokenNumber();  // Gets value in TOP
                
                // Emit OpPushInt followed by the 16-bit value
                LDA #Opcodes.OpPushInt
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
            }
            case Tokens.IDENTIFIER:
            {
                // Variable reference - look it up and load its value
                LDA #ZP.BasicWorkspace1  // Use different workspace than assignment (was W2)
                STA ZP.FSOURCEADDRESSL
                LDA #0
                STA ZP.FSOURCEADDRESSH
                
                Tokenizer.getTokenName();
                
                // Look up the variable for validation
                GlobalManager.FindGlobal();
                if (Z)  // Found
                {
                    // Emit OpLoadVar followed by the 8-byte variable name
                    LDA #Opcodes.OpLoadVar
                    STA ZP.NEXTL
                    LDA #0
                    STA ZP.NEXTH
                    LDA #Types.Byte
                    Stacks.PushNext();
                    FunctionManager.EmitByte();
                    
                    // Emit the 8-byte variable name
                    emitVariableName();
                }
                else
                {
                    // Variable not found - push 0 as error value
                    STZ ZP.TOPL
                    STZ ZP.TOPH
                    
                    LDA #Opcodes.OpPushInt
                    STA ZP.NEXTL
                    LDA #0
                    STA ZP.NEXTH
                    LDA #Types.Byte
                    Stacks.PushNext();
                    FunctionManager.EmitByte();
                    
                    LDA #Types.UInt
                    Stacks.PushTop();
                    FunctionManager.EmitWord();
                }
            }
            case Tokens.STRING:
            {
                // String literal - for now just emit placeholder (push 0)
                STZ ZP.TOPL
                STZ ZP.TOPH
                
                LDA #Opcodes.OpPushInt
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
                
                LDA #Types.UInt
                Stacks.PushTop();
                FunctionManager.EmitWord();
            }
            case Tokens.EOL:
            {
                // Missing or invalid expression - push 0
                STZ ZP.TOPL
                STZ ZP.TOPH
                
                LDA #Opcodes.OpPushInt
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
                
                LDA #Types.UInt
                Stacks.PushTop();
                FunctionManager.EmitWord();
            }
            default:
            {
                // Missing or invalid expression - push 0
                STZ ZP.TOPL
                STZ ZP.TOPH
                
                LDA #Opcodes.OpPushInt
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
                
                LDA #Types.UInt
                Stacks.PushTop();
                FunctionManager.EmitWord();
            }
        }
    }
    
            
    // Compile a complete REPL statement
    CompileREPLStatement()
    {
        // Start compilation
        FunctionManager.StartREPLCompilation();
        
        // Get first token (should be statement type)
        Tokenizer.nextToken();
        
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.PRINT:
            {
                compilePrintStatement();
            }
            case Tokens.IDENTIFIER:
            {
                compileAssignmentStatement();
            }
            case Tokens.CONST:
            {
                // Constant declaration: CONST TYPE NAME = expression
                LDA #1  // Set isConst flag
                STA ZP.BasicFlags
                Tokenizer.nextToken();  // Move to type token
                compileVariableDeclaration();
            }
            case Tokens.IntType:
            case Tokens.WordType:
            case Tokens.ByteType:
            case Tokens.BitType:
            case Tokens.StringType:
            {
                // Variable declaration: TYPE NAME [= expression]
                STZ ZP.BasicFlags  // Clear isConst flag
                compileVariableDeclaration();
            }
            case Tokens.EOL:
            {
                // Empty statement - just emit NOP
                LDA #Opcodes.OpNop
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
            default:
            {
                // Unsupported statement - emit NOP for now
                LDA #Opcodes.OpNop
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
        }
        
        // Always end with HALT for REPL
        LDA #Opcodes.OpHalt
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Finish compilation
        FunctionManager.FinishREPLCompilation();
    }
}
