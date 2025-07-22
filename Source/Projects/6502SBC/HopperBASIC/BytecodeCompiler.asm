unit BytecodeCompiler
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Tokenizer"
    uses "FunctionManager"
    uses "GlobalManager"
    
    // Check if error occurred
    CheckError()
    {
        // Returns Z=0 if error occurred, Z=1 if no error
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
    }
    
    // Bytecode opcodes
    enum Opcodes 
    {
        OpNop        = 0x00,
        OpPushInt    = 0x01,  // + 2 bytes: 16-bit value
        OpPrintInt   = 0x02,  // Print TOS as integer
        OpPrintStr   = 0x03,  // Print TOS as string literal
        OpPrintNL    = 0x04,  // Print newline
        OpReturn     = 0x05,  // Return from function
        OpLoadVar    = 0x06,  // Load variable value to stack + variable name
        OpStoreVar   = 0x07,  // Store TOS to variable + variable name
        OpHalt       = 0xFF,  // End of REPL function
    }
    
    // Emit variable name from current token to bytecode
    emitTokenName()
    {
        // Emit length byte first
        LDA ZP.TokenLen
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Emit each character of the token name
        LDX ZP.TokenStart
        LDY #0
        loop
        {
            CPY ZP.TokenLen
            if (Z) { break; }
            
            LDA Address.BasicInputBuffer, X
            // Convert to uppercase
            CMP #'a'
            if (C)             // >= 'a'
            {
                CMP #('z'+1)
                if (NC)        // <= 'z'
                {
                    SBC #('a'-'A'-1)  // Convert to uppercase
                }
            }
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            
            INX
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
        
        // Check for error and return immediately
        CheckError();
        if (NZ) { return; }  // Error occurred, exit cleanly
        
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
    
    compileAssignmentStatement()
    {
        // SAVE THE VARIABLE NAME FIRST before any tokenizer advancement
        LDA ZP.TokenStart
        STA ZP.BasicWorkspace0    // Save token start
        LDA ZP.TokenLen  
        STA ZP.BasicWorkspace1    // Save token length
        
        // Look up the variable for validation - use token directly
        GlobalManager.FindGlobal();
        if (NZ)  // Not found
        {
            // Variable doesn't exist - set error
            LDA #(Interpreter.msgUndefinedVariable % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgUndefinedVariable / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Check if it's a constant (can't assign to constants)
        GlobalManager.GetGlobalValue();  // Returns type in FTYPE
        LDA ZP.FTYPE
        GlobalManager.IsConstant();
        if (C)  // It's a constant
        {
            // Can't assign to constant - set error
            LDA #(Interpreter.msgCannotAssignConstant % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgCannotAssignConstant / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Move to next token - should be EQUALS
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.EQUALS
        if (NZ)
        {
            // Not an assignment - set error
            LDA #(Interpreter.msgExpectedEquals % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgExpectedEquals / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Move to expression
        Tokenizer.nextToken();
        
        // Check if we have an expression
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // Missing expression after = - set error
            LDA #(Interpreter.msgMissingExpression % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgMissingExpression / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Compile expression (puts result on stack)
        compileExpression();
        CheckError();
        if (NZ) { return; }
        
        // Now emit OpStoreVar with the SAVED variable name
        LDA #Opcodes.OpStoreVar
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Emit the saved variable name
        emitSavedTokenName();
    }
    
    emitSavedTokenName()
    {
        // Emit length byte first
        LDA ZP.BasicWorkspace1    // Saved token length
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Emit each character of the saved token name
        LDX ZP.BasicWorkspace0    // Saved token start
        LDY #0
        loop
        {
            CPY ZP.BasicWorkspace1    // Saved token length
            if (Z) { break; }
            
            LDA Address.BasicInputBuffer, X
            // Convert to uppercase
            CMP #'a'
            if (C)             // >= 'a'
            {
                CMP #('z'+1)
                if (NC)        // <= 'z'
                {
                    SBC #('a'-'A'-1)  // Convert to uppercase
                }
            }
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
            
            INX
            INY
        }
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
                LDA ZP.BasicFlags  // isConst flag
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
                // Invalid type - set error
                LDA #(Interpreter.msgInvalidType % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgInvalidType / 256)
                STA ZP.LastErrorH
                return;
            }
        }
        
        // Get variable name
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.IDENTIFIER
        if (NZ)
        {
            // Invalid syntax - set error  
            LDA #(Interpreter.msgExpectedIdentifier % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgExpectedIdentifier / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Save the variable name token info before advancing
        LDA ZP.TokenStart
        STA ZP.BasicWorkspace2    // Save identifier token start  
        LDA ZP.TokenLen
        STA ZP.BasicWorkspace3    // Save identifier token length
        
        // Save token info for GlobalManager.AddGlobal() - token name is in TokenStart/TokenLen
        
        // Default value is 0 (ALWAYS set this first)
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        // Check for optional initializer
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.EQUALS
        if (Z)
        {
            // Has initializer - get the next token (the value)
            Tokenizer.nextToken();
            
            // Check what kind of expression we have
            LDA ZP.CurrentToken
            CMP #Tokens.NUMBER
            if (Z)
            {
                // It's a number - get the value directly
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
                // Constants MUST have initializer - set error
                LDA #(Interpreter.msgConstantNeedsValue % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgConstantNeedsValue / 256)
                STA ZP.LastErrorH
                return;
            }
        }
        
        // Restore the variable name token info for AddGlobal
        LDA ZP.BasicWorkspace2
        STA ZP.TokenStart
        LDA ZP.BasicWorkspace3  
        STA ZP.TokenLen
        
        // Add the variable/constant to GlobalManager
        // Parameters: Token name (TokenStart/TokenLen), FTYPE = type, TOP = value
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
                // Use token directly from tokenizer
                GlobalManager.FindGlobal();
                if (Z)  // Found
                {
                    // Emit OpLoadVar followed by the variable name
                    LDA #Opcodes.OpLoadVar
                    STA ZP.NEXTL
                    LDA #0
                    STA ZP.NEXTH
                    LDA #Types.Byte
                    Stacks.PushNext();
                    FunctionManager.EmitByte();
                    
                    // Emit the variable name
                    emitTokenName();
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
                // Missing or invalid expression - set error
                LDA #(Interpreter.msgInvalidExpression % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgInvalidExpression / 256)
                STA ZP.LastErrorH
                return;
            }
        }
    }
    
    // Cleanup compilation temp buffer on error
    cleanupCompilationOnError()
    {
        // Free the temp buffer if it was allocated
        LDA ZP.CompileState
        CMP #1  // Currently compiling?
        if (Z)
        {
            // Free temp buffer
            LDA ZP.TempBlockLo
            STA ZP.IDXL
            LDA ZP.TempBlockHi
            STA ZP.IDXH
            Memory.Free();
            
            // Reset compilation state
            STZ ZP.CompileState
        }
    }
            
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
            case Tokens.LET:
            {
                // LET IDENTIFIER = expression
                // Move to the identifier token
                Tokenizer.nextToken();
                LDA ZP.CurrentToken
                CMP #Tokens.IDENTIFIER
                if (NZ)
                {
                    // Expected identifier after LET
                    LDA #(Interpreter.msgExpectedIdentifier % 256)
                    STA ZP.LastErrorL
                    LDA #(Interpreter.msgExpectedIdentifier / 256)
                    STA ZP.LastErrorH
                    return;
                }
                // Now process as normal assignment
                compileAssignmentStatement();
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
                // Unsupported statement - set error
                LDA #(Interpreter.msgUnsupportedStatement % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgUnsupportedStatement / 256)
                STA ZP.LastErrorH
                return;
            }
        }
        
        CheckError();
        if (NZ) 
        { 
            // Error occurred - cleanup temp buffer before returning
            cleanupCompilationOnError();
            return; 
        }
        
        // Always end with HALT for REPL
        LDA #Opcodes.OpHalt
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Finish compilation (only if no errors)
        FunctionManager.FinishREPLCompilation();
    }
}
