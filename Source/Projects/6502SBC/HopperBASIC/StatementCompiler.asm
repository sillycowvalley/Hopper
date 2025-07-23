unit StatementCompiler
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Tokenizer"
    uses "FunctionManager"
    uses "GlobalManager"
    uses "Opcodes"
    uses "ExpressionParser"
    
    friend Interpreter;
    
    // Check if expression type and value can be assigned to target type
    // Input: expressionType in A, targetType in X, value in TOP
    // Output: C=1 if compatible, C=0 if not compatible
    isCompatibleAssignment()
    {
        // A = expressionType, X = targetType
        // Compare A with X - need to store one to compare
        STX ZP.BasicWorkspace0    // Store targetType temporarily
        CMP ZP.BasicWorkspace0    // Compare expressionType with targetType
        if (Z) 
        { 
            SEC  // Same type - always compatible
            return; 
        }
        
        // Different types - check conversion rules based on targetType
        LDX ZP.BasicWorkspace0    // Restore targetType to X
        switch (X)  // targetType
        {
            case Types.Bool:
            case Types.String:
            {
                CLC  // Strict - no conversions allowed
                return;
            }
            case Types.Byte:
            {
                // Can assign from Int or UInt if value fits in 0-255 and non-negative
                CMP #Types.Int
                if (Z) 
                { 
                    // Check if INT value fits in BYTE (0-255, no sign bit)
                    LDA ZP.TOPH
                    if (NZ)  // High byte must be 0
                    {
                        CLC
                        return;
                    }
                    BIT ZP.TOPH  // Check sign bit
                    if (MI)      // Negative
                    {
                        CLC
                        return;
                    }
                    SEC  // Value fits
                    return;
                }
                CMP #Types.UInt
                if (Z)
                {
                    // Check if WORD value fits in BYTE (= 255)
                    LDA ZP.TOPH
                    if (NZ)  // High byte must be 0
                    {
                        CLC
                        return;
                    }
                    SEC  // Value fits
                    return;
                }
                CLC  // Other types not compatible
                return;
            }
            case Types.UInt:  // WORD
            {
                CMP #Types.Byte
                if (Z) 
                { 
                    SEC  // BYTE always fits in WORD
                    return; 
                }
                CMP #Types.Int
                if (Z)
                {
                    // Check if INT value is non-negative
                    BIT ZP.TOPH
                    if (MI)  // Negative
                    {
                        CLC
                        return;
                    }
                    SEC  // Non-negative INT fits in WORD
                    return;
                }
                CLC  // Other types not compatible
                return;
            }
            case Types.Int:
            {
                CMP #Types.Byte
                if (Z) 
                { 
                    SEC  // BYTE always fits in INT
                    return; 
                }
                CMP #Types.UInt
                if (Z)
                {
                    // Check if WORD value fits in signed INT (= 32767)
                    BIT ZP.TOPH
                    if (MI)  // Value > 32767
                    {
                        CLC
                        return;
                    }
                    SEC  // Value fits in signed INT
                    return;
                }
                CLC  // Other types not compatible
                return;
            }
            default:
            {
                CLC  // Unknown target type
                return;
            }
        }
    }
    
    // Emit a variable name from a saved location in memory
    emitSavedVariableName()
    {
        // Calculate string length first
        LDX #0
        loop
        {
            LDA (Address.BasicWorkBuffer + 0x40), X
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
        
        // Emit each character of the saved variable name
        LDX #0
        loop
        {
            LDA (Address.BasicWorkBuffer + 0x40), X
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
    
    // Compile a PRINT statement
    // Assumes current token is PRINT
    CompilePrintStatement()
    {
        // Move to next token (the expression to print)
        Tokenizer.nextToken();
        
        // Compile the expression (puts result on stack)
        ExpressionParser.ParseRuntimeExpression();
        
        // Check for error and return immediately
        CheckError();
        if (NZ) { return; }  // Error occurred, exit cleanly
        
        // Emit OpPrintInt to print the result
        LDA #OpCode.OpPrintInt
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
            LDA #OpCode.OpPrintNL
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
        }
    }
    
    // Compile variable declaration: [CONST] TYPE NAME [= expression]
    // Current token is the type token, bit 0 of BasicFlags indicates if CONST was seen
    CompileVariableDeclaration()
    {
        // Current token is the type - convert to GlobalManager type
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.IntType:
            {
                if (BBS0, ZP.BasicFlags)
                {
                    LDA #GlobalManager.GlobalTypes.ConstInt
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.VarInt
                }
                STA ZP.FTYPE
            }
            case Tokens.WordType:
            {
                if (BBS0, ZP.BasicFlags)
                {
                    LDA #GlobalManager.GlobalTypes.ConstWord
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.VarWord
                }
                STA ZP.FTYPE
            }
            case Tokens.ByteType:
            {
                if (BBS0, ZP.BasicFlags)
                {
                    LDA #GlobalManager.GlobalTypes.ConstByte
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.VarByte
                }
                STA ZP.FTYPE
            }
            case Tokens.BitType:
            {
                if (BBS0, ZP.BasicFlags)
                {
                    LDA #GlobalManager.GlobalTypes.ConstBit
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.VarBit
                }
                STA ZP.FTYPE
            }
            case Tokens.StringType:
            {
                if (BBS0, ZP.BasicFlags)
                {
                    LDA #GlobalManager.GlobalTypes.ConstString
                }
                else
                {
                    LDA #GlobalManager.GlobalTypes.VarString
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
        
        // CRITICAL: Copy the variable name from BasicWorkBuffer to BasicInputBuffer 
        // (which is safe because we've already tokenized the input)
        // This preserves the name while we process the initializer
        LDX #0
        loop
        {
            LDA Address.BasicWorkBuffer, X
            STA Address.BasicInputBuffer, X  // Reuse input buffer as temp storage
            if (Z) { break; }  // Copied null terminator
            INX
        }
        
        // Default value is 0 (ALWAYS set this first)
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        // Check for optional initializer
        Tokenizer.nextToken();
        LDA ZP.CurrentToken
        CMP #Tokens.EQUALS
        if (Z)
        {
            // Has initializer - move to the expression
            Tokenizer.nextToken();
            
            // Parse the expression directly to get the value
            ExpressionParser.ParseConstantExpression();
            CheckError();
            if (NZ) { return; }
            
            // Check type compatibility for constant expressions
            // Get target variable type
            LDX ZP.FTYPE  // Save expression type
            LDA ZP.FTYPE
            AND #0x7F     // Clear constant flag to get base variable type
            switch (A)
            {
                case GlobalTypes.VarInt:
                case GlobalTypes.ConstInt:
                {
                    LDA #Types.Int
                }
                case GlobalTypes.VarWord:
                case GlobalTypes.ConstWord:
                {
                    LDA #Types.UInt
                }
                case GlobalTypes.VarByte:
                case GlobalTypes.ConstByte:
                {
                    LDA #Types.Byte
                }
                case GlobalTypes.VarBit:
                case GlobalTypes.ConstBit:
                {
                    LDA #Types.Bool
                }
                case GlobalTypes.VarString:
                case GlobalTypes.ConstString:
                {
                    LDA #Types.String
                }
                default:
                {
                    LDA #Types.Int  // Default
                }
            }
            TAX           // targetType in X
            TXA           // expressionType back in A
            PHX           // Save targetType
            
            // Get expression type from FTYPE (set by ParseConstantExpression)
            LDA ZP.FTYPE
            PLX           // Restore targetType
            
            // Check compatibility: expressionType in A, targetType in X, value in TOP
            isCompatibleAssignment();
            if (NC)  // Not compatible
            {
                // Type conversion error
                LDA #(Interpreter.msgInvalidExpression % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgInvalidExpression / 256)
                STA ZP.LastErrorH
                return;
            }
        }
        else
        {
            // Check if this is a constant without initializer (error)
            if (BBS0, ZP.BasicFlags)
            {
                // Constants MUST have initializer - set error
                LDA #(Interpreter.msgConstantNeedsValue % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgConstantNeedsValue / 256)
                STA ZP.LastErrorH
                return;
            }
        }
        
        // Set TokenPtr to point to the saved variable name in BasicInputBuffer
        LDA #(Address.BasicInputBuffer & 0xFF)
        STA ZP.TokenPtr
        LDA #(Address.BasicInputBuffer >> 8)
        STA ZP.TokenPtrHi
        
        // Add the variable/constant to GlobalManager
        // Parameters: Token name at TokenPtr (null-terminated), FTYPE = type, TOP = value
        GlobalManager.AddGlobal();
        
        // Emit NOP as placeholder (declaration doesn't generate runtime code)
        LDA #OpCode.OpNop
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
    }
    
    // Compile assignment statement: IDENTIFIER = expression
    CompileAssignmentStatement()
    {
        // Save the identifier name from BasicWorkBuffer to safe area
        LDX #0
        loop
        {
            LDA Address.BasicWorkBuffer, X
            STA (Address.BasicWorkBuffer + 0x40), X  // Store at safe offset
            if (Z) { break; }  // Copied null terminator
            INX
        }
        
        // Point IDY to the saved identifier name
        LDA #((Address.BasicWorkBuffer + 0x40) & 0xFF)
        STA ZP.IDYL
        LDA #((Address.BasicWorkBuffer + 0x40) >> 8)
        STA ZP.IDYH
        
        // Look up the variable - it MUST exist for assignment
        GlobalManager.FindGlobal();
        if (NZ)  // Variable doesn't exist - ERROR!
        {
            // Variable not found - set error
            LDA #(Interpreter.msgUndefinedVariable % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgUndefinedVariable / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Variable exists - check if it's a constant (can't assign to constants)
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
        ExpressionParser.ParseRuntimeExpression();
        CheckError();
        if (NZ) { return; }
        
        // For runtime expressions, we can't do compile-time type checking
        // The bytecode executor will handle type compatibility at runtime
        
        // Emit OpStoreVar with the variable name
        LDA #OpCode.OpStoreVar
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Emit the variable name from the saved location
        emitSavedVariableName();
    }
    
    // Cleanup compilation temp buffer on error
    CleanupCompilationOnError()
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
    
    // Main entry point for REPL statement compilation
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
                CompilePrintStatement();
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
                CompileAssignmentStatement();
            }
            case Tokens.IDENTIFIER:
            {
                CompileAssignmentStatement();
            }
            case Tokens.CONST:
            {
                // Constant declaration: CONST TYPE NAME = expression
                SMB0 ZP.BasicFlags  // Set isConst flag
                Tokenizer.nextToken();  // Move to type token
                CompileVariableDeclaration();
            }
            case Tokens.IntType:
            case Tokens.WordType:
            case Tokens.ByteType:
            case Tokens.BitType:
            case Tokens.StringType:
            {
                // Variable declaration: TYPE NAME [= expression]
                RMB0 ZP.BasicFlags  // Clear isConst flag
                CompileVariableDeclaration();
            }
            case Tokens.EOL:
            {
                // Empty statement - just emit NOP
                LDA #OpCode.OpNop
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
            CleanupCompilationOnError();
            return; 
        }
        
        // Always end with HALT for REPL
        LDA #OpCode.OpHalt
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
