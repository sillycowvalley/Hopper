unit Statement
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Tools"
    
    uses "Variables"
    uses "Instructions"
    
    // Private Statement layer storage - BasicProcessBuffer2 (32 bytes at 0x09C0-0x09DF)
    const uint stmtNamePtr     = Address.BasicProcessBuffer2;      // 0x09C0: 2 bytes - identifier name pointer
    const uint stmtValue       = Address.BasicProcessBuffer2 + 2;  // 0x09C2: 2 bytes - initial/evaluated value
    const uint stmtTokensPtr   = Address.BasicProcessBuffer2 + 4;  // 0x09C4: 2 bytes - tokens pointer  
    const uint stmtTokPos      = Address.BasicProcessBuffer2 + 8;  // 0x09C8: 2 bytes - saved tokenizer position
    const uint stmtTokLen      = Address.BasicProcessBuffer2 + 10; // 0x09CA: 2 bytes - token stream length
    const uint stmtSymbol      = Address.BasicProcessBuffer2 + 12; // 0x09CC: 1 byte  - symbol information
    const uint stmtType        = Address.BasicProcessBuffer2 + 13; // 0x09CD: 1 byte  - type information
    const uint stmtIsConstant  = Address.BasicProcessBuffer2 + 14; // 0x09CE: 1 byte  - current expression is const
    const uint stmtObjectPtr   = Address.BasicProcessBuffer2 + 15; // 0x09CF: 2 bytes - object node pointer
    
    // 15 bytes available for future statement needs (0x09D1-0x09DF)

    SetIsConstant()
    {
        STA stmtIsConstant
    }
    IsConstant()
    {
        LDA stmtIsConstant
        if (Z)
        {
            CLC // not constant (stmtISConstant == 0)
        }   
        else
        {
            SEC // constant (stmtISConstant == 1)
        }
    }
    
    
    // Typically called when ZP.CurrentToken is Tokens.IDENTIFIER, or a keyword
    // Output: symbol or function in IDX, A = IdentifierType
    resolveIdentifier()
    {
        PHX
        PHY
        
        LDY ZP.ACCT
        PHY
        
#ifdef DEBUG
        PHA
        LDA #'<'
        Tools.COut();
        LDA #'R'
        Tools.COut();
        LDA #':'
        Tools.COut();
        PLA
#endif 
        
        loop // Single exit block for clean error handling
        {
            Tokenizer.IsKeyword();
            if (C)
            {
#ifdef DEBUG
                LDA #'K' Tools.COut();
#endif
                LDA # IdentifierType.Keyword
                break; // success
            }
            
            // Get the identifier name for lookup
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            Messages.CheckError();
            if (NC) { break; }
            
            STZ ZP.SymbolIteratorFilter  // Accept any symbol type (variable or constant)
            Variables.Find(); // ZP.IDX = symbol node address
            if (C) // Symbol found
            {
                // Get symbol type and check
                Variables.GetType();
                LDA ZP.ACCT
                AND #0xF0  // Extract symbol type (high nibble)
                CMP # (SymbolType.VARIABLE << 4)
                if (Z)
                { 
#ifdef DEBUG
                    LDA # 'V' Tools.COut();
#endif
                    LDA # IdentifierType.Global
                    break; // success
                }
                CMP # (SymbolType.CONSTANT << 4)
                if (Z)
                { 
#ifdef DEBUG
                    LDA # 'C' Tools.COut();
#endif        
                    LDA # IdentifierType.Constant
                    break; // success
                }
                // what's this?
                LDA #(Messages.InternalError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InternalError / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                CLC
            }
            Messages.CheckError();
            if (NC) { break; }
            
            // function by same name exists? name pointer in TOP
            LDX #ZP.FunctionsList
            Objects.Find();
            if (C)  
            {
#ifdef DEBUG
                LDA #'F' Tools.COut();
#endif    
                LDA # IdentifierType.Function
                break; // success
            }
            Messages.CheckError();
            if (NC) { break; }
#ifdef DEBUG
            LDA #'U' Tools.COut();
#endif
            LDA #(Messages.UndefinedIdentifier % 256)
            STA ZP.LastErrorL
            LDA #(Messages.UndefinedIdentifier / 256)
            STA ZP.LastErrorH
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            LDA # IdentifierType.Undefined
            CLC  // undefined identifier
            break;
        } // end of single exit block
#ifdef DEBUG
        PHA
        LDA #'R'
        Tools.COut();
        LDA #'>'
        Tools.COut();
        PLA
#endif        
        PLY
        STY ZP.ACCT
        
        PLY
        PLX
    }
    
    // Evaluate expression using JIT compilation
    // Input: ZP.CurrentToken = first token of expression
    // Output: Expression result pushed onto value stack
    //         ZP.CurrentToken = token after expression
    // Munts: Stack, ZP.CurrentToken, all ZP variables used by parsing
    // Error: Sets ZP.LastError if syntax error or type mismatch
    EvaluateExpression()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'E'
        Tools.COut();
    #endif
        
        // NEW JIT COMPILATION PATH:
        // 1. Compile infix tokens ? postfix opcodes
        Compiler.CompileExpression();
        Messages.CheckError();
        if (NC) 
        { 
    #ifdef DEBUG
            LDA #'E'
            Tools.COut();
            LDA #'!'
            Tools.COut();
            LDA #'>'
            Tools.COut();
    #endif
            return; 
        }
        
        // 2. Execute opcodes ? result on stack
        Executor.ExecuteOpcodes();
        Messages.CheckError();
        if (NC) 
        { 
    #ifdef DEBUG
            LDA #'E'
            Tools.COut();
            LDA #'!'
            Tools.COut();
            LDA #'>'
            Tools.COut();
    #endif
            return; 
        }
        
        // Result is now on stack - identical to old behavior
        
    #ifdef DEBUG
        LDA #'E'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }
    
    
    // Execute a statement starting from current token position
    // Input: ZP.CurrentToken = first token of statement
    // Output: Statement executed, stack may contain results
    //         ZP.CurrentToken = token after statement
    // Munts: Stack, ZP.CurrentToken, all parsing and execution variables
    // Error: Sets ZP.LastError if statement execution fails
    Execute()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'S'
        Tools.COut();
        LDA ZP.CurrentToken
        Tools.HOut();
#endif
        loop
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.REM:
                case Tokens.COMMENT:
                {
                    // Comments are no-ops - just advance to next token
                    Tokenizer.NextToken();
                    SEC  // Success
                    break;
                }
                case Tokens.PRINT:
                {
                    executePrint();
                    break;
                }
                case Tokens.IF:
                {
                    executeIf();
                    break;
                }
                case Tokens.RETURN:
                {
                    executeReturn();
                    break;
                }
                case Tokens.END:
                {
                    executeEnd();
                    break;
                }
                case Tokens.IDENTIFIER:
                {
                    // Could be assignment or function call
                    executeIdentifier();
                    break;
                }
                
                case Tokens.CONST:
                {
                    executeConstantDeclaration();
                }
                case Tokens.INT:
                case Tokens.WORD:
                case Tokens.BIT:
                {
                    executeVariableDeclaration();
                    break;
                }
                
                default:
                {
                    // Unexpected token for statement
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
    
                    CLC  // Error
                    break;
                }
            } // switch
            break;
        } // loop - single exit
        
        
        if (C) // Only if statement executed successfully
        {
            LDA ZP.CurrentToken
            CMP #Tokens.REM
            if (Z)
            {
                Tokenizer.NextToken(); // Skip REM and consume comment text
            }
            else
            {
                CMP #Tokens.COMMENT  
                if (Z)
                {
                    Tokenizer.NextToken(); // Skip COMMENT and consume comment text
                }
            }
        }
        
#ifdef DEBUG
        LDA #'S'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Execute PRINT statement
    // Input: ZP.CurrentToken = PRINT token
    // Output: Expression result printed to serial with newline
    //         ZP.CurrentToken = token after PRINT statement
    // Munts: Stack, ZP.CurrentToken, ZP.TOP, ZP.TOPT, all parsing variables
    // Error: Sets ZP.LastError if expression evaluation fails
    executePrint()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'P'
        Tools.COut();
#endif

        // Get next token (should be start of expression)
        Tokenizer.NextToken();
        
        Messages.CheckError();
        if (NC) { return; }
        
        // Check for end of line (PRINT with no arguments)
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // Just print a newline
            LDA #'\n'
            Serial.WriteChar();
            
#ifdef DEBUG
            LDA #'P'
            Tools.COut();
            LDA #'>'
            Tools.COut();
#endif
            
            SEC  // Success
            return;
        }
        
        // Evaluate the expression
        EvaluateExpression();
        Messages.CheckError();
        if (NC) { return; }
        
        // Top of stack now contains the result
        // For now, assume it's a number and print it
        Stacks.PopTop();  // Pop result into TOP, modifies X
        Tools.PrintDecimalWord();
        
        // Print newline
        LDA #'\n'
        Serial.WriteChar();
        
#ifdef DEBUG
        LDA #'P'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC  // Success
    }
    
    // Execute IF statement
    // Input: ZP.CurrentToken = IF token
    // Output: Conditional statement executed if condition is true
    //         ZP.CurrentToken = token after IF statement
    // Munts: Stack, ZP.CurrentToken, ZP.TOP, ZP.TOPT, all parsing variables
    // Error: Sets ZP.LastError if syntax error or expression evaluation fails
    executeIf()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'I'
        Tools.COut();
#endif
        
        // Get next token (should be start of condition expression)
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Evaluate the condition
        EvaluateExpression();
        Messages.CheckError();
        if (NC) { return; }
        
        // Check for THEN keyword
        LDA ZP.CurrentToken
        CMP #Tokens.THEN
        if (NZ)
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC  // Error
            return;
        }
        
        // Get the condition result
        Stacks.PopTop();  // Pop condition into ZP.TOP, modifies X  
        
        // Check if condition is true (non-zero)
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (Z)
        {
            // Condition is false, skip to end of line
#ifdef DEBUG
            LDA #'I'
            Tools.COut();
            LDA #'>'
            Tools.COut();
#endif
            SEC  // Success (skip)
            return;
        }
        
        // Condition is true, get next token and execute statement
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Recursively execute the statement after THEN
        Execute();
        
#ifdef DEBUG
        LDA #'I'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Execute RETURN statement (stub implementation)
    // Input: ZP.CurrentToken = RETURN token
    // Output: Error (not implemented)
    // Munts: ZP.LastError, ZP.CurrentToken, stack if expression provided
    // Error: Always sets ZP.LastError (not implemented)
    executeReturn()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'R'
        Tools.COut();
#endif
        
        // Get next token
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Check if there's an expression to return
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // No return value, push 0
            LDA #0
            STA ZP.TOPL
            STA ZP.TOPH
            Stacks.PushTop();
        }
        else
        {
            // Evaluate return expression
            EvaluateExpression();
            Messages.CheckError();
            if (NC) { return; }
        }
        
        // TODO: Actually return from function when we have function support
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        
        Messages.StorePC(); // 6502 PC -> IDY
        
#ifdef DEBUG
        LDA #'R'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        CLC  // Error
        BRK
    }
    
    // Execute END statement (stub implementation)
    // Input: ZP.CurrentToken = END token
    // Output: Error (not implemented)
    // Munts: ZP.LastError
    // Error: Always sets ZP.LastError (not implemented)
    executeEnd()
    {
        // TODO: End program execution when we have program support
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        
        Messages.StorePC(); // 6502 PC -> IDY
        
        CLC  // Error
        BRK
    }
    
    // Execute identifier statement (assignment or function call)
    // Input: A = ZP.CurrentToken = IDENTIFIER token
    // Output: C set if successful, NC if error
    // Munts: Stack, ZP.CurrentToken, symbol tables, expression evaluation
    // Error: Sets ZP.LastError if undefined variable, type mismatch, or syntax error
    executeIdentifier()
    {
    #ifdef DEBUG
        PHA
        LDA #'<'
        Tools.COut();
        LDA #'I'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        PLA
    #endif
    
        loop // Single exit block for clean error handling
        {
            // Use ResolveIdentifier to determine what we have
            resolveIdentifier(); // symbol or function in IDX, A = IdentifierType
            Messages.CheckError();
            if (NC) { break; } // Error - identifier not found or other error
            
            // Handle different identifier types
            switch (A)
            {
                case IdentifierType.Global:
                {
                    // This is a variable - save the node address in statement storage
                    LDA ZP.IDXL
                    STA (stmtObjectPtr + 0)
                    LDA ZP.IDXH
                    STA (stmtObjectPtr + 1)
                    
                    // Move to next token - should be '=' for assignment
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Check for assignment operator
                    LDA ZP.CurrentToken
                    CMP #Tokens.EQUALS
                    if (NZ)
                    {
                        // Not an assignment - this might be a function call or syntax error
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        
                        Messages.StorePC(); // 6502 PC -> IDY
                        
                        CLC
                        break;
                    }
                    
                    // Move past the '=' token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Evaluate the expression on the right side
                    EvaluateExpression();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Pop the expression result from the value stack
                    Stacks.PopTop(); // Result in ZP.TOP, type in ZP.TOPT
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Restore the variable node address from statement storage
                    LDA (stmtObjectPtr + 0)
                    STA ZP.IDXL
                    LDA (stmtObjectPtr + 1)
                    STA ZP.IDXH
                    
                    // Get the variable's declared type for type checking
                    Variables.GetType(); // Returns type in ZP.ACCT
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    LDA ZP.ACCT
                    AND #0x0F  // Extract data type (low nibble)
                    STA ZP.ACCT // Variable's declared type
                    
                    // Check type compatibility
                    LDA ZP.TOPT // Expression result type
                    CMP ZP.ACCT // Variable's declared type
                    if (NZ)
                    {
                        // Type mismatch - check for valid assignment promotion
                        CheckRHSTypeCompatibility(); // Uses ZP.TOP = RHS value, ZP.TOPT = RHS type, ZP.NEXTT = LHS type
                        Messages.CheckError();
                        if (NC) { break; } // Assignment type promotion failed
                    }
                    
                    // Assign the value to the variable
                    Variables.SetValue(); // ZP.IDX = node, ZP.TOP = value
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    SEC // Success
                    break;
                }
                
                case IdentifierType.Constant:
                {
                    // Constants cannot be assigned to
                    LDA #(Messages.IllegalAssignment % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.IllegalAssignment / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC
                    break;
                }
                
                case IdentifierType.Keyword:
                {
                    // Keywords should not appear as statements
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC
                    break;
                }
                
                default:
                {
                    // TODO: Handle function calls when functions are implemented
                    LDA #(Messages.NotImplemented % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.NotImplemented / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC
                    break;
                }
            }
            
            break; // Exit the outer loop
        } // end single exit block
        
    #ifdef DEBUG
        LDA #'I'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }
        
    // Input: ZP.CurrentToken = CONST
    executeConstantDeclaration()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'D'
        Tools.COut();
#endif        
        loop
        {
            Tokenizer.NextToken(); // consume 'CONST'
            if (NC) { break; } // error exit
            
            // we want a constant expression
            LDA #1
            SetIsConstant();
            
            LDA #(SymbolType.CONSTANT << 4)
            STA stmtSymbol
            processSingleSymbolDeclaration();
            break;
        } // single exit
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif        
    }
    
    // Execute variable declaration statement
    // Input: ZP.CurrentToken = type token (INT, WORD, BIT)
    // Output: Variable declared and added to symbol table
    //         ZP.CurrentToken = token after declaration
    // Munts: Stack, ZP.CurrentToken, symbol tables, memory allocation, 
    //        all statement buffer locations, all parsing variables
    // Error: Sets ZP.LastError if syntax error, type mismatch, name conflict, or memory allocation fails
    executeVariableDeclaration()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'V'
        Tools.COut();
        LDA #'D'
        Tools.COut();
#endif        
        LDA #(SymbolType.VARIABLE << 4)
        STA stmtSymbol
        processSingleSymbolDeclaration();
#ifdef DEBUG
        LDA #'V'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif        
    }
    
    processSingleSymbolDeclaration()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'S'
        Tools.COut();
        LDA #'D'
        Tools.COut();
#endif

        loop
        {
            LDA ZP.CurrentToken
            STA stmtType
            
            //Tools.HOut();
        
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; } // error exit
            
            // Check that we have an identifier
            LDA ZP.CurrentToken
            CMP #Tokens.IDENTIFIER
            if (NZ)
            {
                Tokenizer.IsKeyword();
                if (C)
                {
                    LDA #(Messages.IllegalVariableName  % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.IllegalVariableName  / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                }
                else
                {
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                }
                
                CLC
                break; // error exit
            }
            
            // Get the identifier string
            Tokenizer.GetTokenString();  // Returns pointer in ZP.TOP
            
            loop
            {
                // Save name pointer
                LDA ZP.TOPL
                PHA
                LDA ZP.TOPH
                PHA
                
                // function by same name exists? name pointer in TOP
                LDX #ZP.FunctionsList
                Objects.Find(); // ZP.IDX = symbol node address
                if (C)  
                {
                    LDA #(Messages.FunctionExists % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.FunctionExists / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC  // Error
                    break;
                }
                
                // if it already exists, try to delete it
                STZ ZP.SymbolIteratorFilter // constant or variable
                Variables.Find(); // ZP.IDX = symbol node address
                if (C)
                {
                    /* 
                    
                    // Should we ever want constants not to be re-definable:
                    Variables.GetType();
                    if (NC) { break; }
                    LDA ZP.ACCT
                    AND #0xF0
                    CMP #(SymbolType.CONSTANT << 4)
                    if (Z)
                    {
                        LDA #(Messages.ConstantExists % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.ConstantExists / 256)
                        STA ZP.LastErrorH
                        
                        Messages.StorePC(); // 6502 PC -> IDY
                        
                        CLC  // Error
                        break;
                    }
                    
                    */
                    
                    // delete it (name ptr in TOP)
                    Variables.Remove();
                    if (NC) { break; }
                }
                
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { break; } // error exit
                
                // Check for optional initialization
                LDX ZP.CurrentToken
                switch (X)
                {
                    case Tokens.EQUALS:
                    {
                        // Save tokenizer position before expression
                        LDA ZP.TokenizerPosL
                        PHA
                        LDA ZP.TokenizerPosH
                        PHA
                        
                        // Get next token (start of expression)
                        Tokenizer.NextToken();
                        Messages.CheckError();
                        if (C)
                        {
                            EvaluateExpression();
                            Messages.CheckError();
                        }
                        
                        // Restore tokenizer position before expression
                        PLA
                        STA ZP.FSOURCEADDRESSH
                        PLA
                        STA ZP.FSOURCEADDRESSL
                        
                        if (NC) { break; } // error exit
                        
                        // subtract current tokenizer position
                        SEC
                        LDA ZP.TokenizerPosL
                        SBC ZP.FSOURCEADDRESSL
                        STA ZP.FLENGTHL  // Length low
                        LDA ZP.TokenizerPosH
                        SBC ZP.FSOURCEADDRESSH
                        STA ZP.FLENGTHH  // Length high
                        createTokenStream();
                        if (NC) { break; } // error exit
                        
                        // Set tokens pointer to the new stream
                        LDA ZP.FDESTINATIONADDRESSL
                        STA (stmtTokensPtr + 0)
                        LDA ZP.FDESTINATIONADDRESSH
                        STA (stmtTokensPtr + 1)
                     
                        // Pop the result into NEXT
                        Stacks.PopNext();  // Result in ZP.NEXT, type in ZP.NEXTT,  modifies X
                        
                        LDA stmtSymbol
                        CMP # (SymbolType.CONSTANT << 4)
                        if (Z)
                        {
                            IsConstant();
                            if (NC)
                            {
                                LDA #(Messages.ConstantExpressionExpected % 256)
                                STA ZP.LastErrorL
                                LDA #(Messages.ConstantExpressionExpected / 256)
                                STA ZP.LastErrorH
                                
                                Messages.StorePC(); // 6502 PC -> IDY
                                
                                CLC
                                break; // error exit
                            }
                        }
                    }
                    case Tokens.EOL:
                    case Tokens.COLON:
                    {
                        LDA stmtSymbol
                        CMP # (SymbolType.CONSTANT << 4)
                        if (Z)
                        {
                            // constants must be initialized
                            LDA #(Messages.ConstantExpressionExpected % 256)
                            STA ZP.LastErrorL
                            LDA #(Messages.ConstantExpressionExpected / 256)
                            STA ZP.LastErrorH
                            
                            Messages.StorePC(); // 6502 PC -> IDY
                            
                            CLC
                            break; // error exit
                        }
                        // initial value
                        STZ ZP.NEXTL
                        STZ ZP.NEXTH
                        // no expression tokens
                        STZ ZP.IDXH
                        STZ ZP.IDXL
                    }
                    default:
                    {
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        
                        Messages.StorePC(); // 6502 PC -> IDY
                        
                        CLC
                        break; // error exit
                    }
                }
                
                SEC  // Success
                break;
            }
            // Now restore name pointer to TOP
            PLA
            STA ZP.TOPH
            PLA
            STA ZP.TOPL
            break;
        } // loop
        
        LDX stmtType
        loop
        {
            if (NC) { break; } // error exit
            switch(X)
            {
                case Tokens.WORD:
                {
                    LDX #BasicType.WORD
                }
                case Tokens.INT:
                {
                    LDX # BasicType.INT
                }
                case Tokens.BIT:
                {
                    LDX #BasicType.BIT
                }
                default:
                {
                    LDA #(Messages.TypeMismatch % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.TypeMismatch / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC // what's this?
                    break;
                }
            }
            
            PHX
            LDA ZP.TOPL
            PHA
            LDA ZP.TOPH
            PHA
            LDA ZP.NEXTL
            PHA
            LDA ZP.NEXTH
            PHA
            
            LDA ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.NEXTH
            STA ZP.TOPH
            LDA ZP.NEXTT
            STA ZP.TOPT
            
            STX ZP.NEXTT // LHS type
            
            // RHS in TOP
            // LHS type in NEXTT
            CheckRHSTypeCompatibility();
            
            PLA
            STA ZP.NEXTH
            PLA 
            STA ZP.NEXTL
            PLA
            STA ZP.TOPH
            PLA 
            STA ZP.TOPL
            PLX
            
            if (NC)
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            // Pack symbolType|dataType: VARIABLE(1) in high nibble, dataType in low nibble
            TXA  // dataType in A
            ORA stmtSymbol // high nibble is VARIABLE<<4 of CONSTANT<<4
            STA ZP.ACCT
            
            LDA (stmtTokensPtr+0)
            STA ZP.IDYL
            LDA (stmtTokensPtr+1)
            STA ZP.IDYH
            
            // Call Variables.Declare
            // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
            //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
            Variables.Declare();
            Messages.CheckError();
            if (C)
            {
                STZ (stmtTokensPtr+0)
                STZ (stmtTokensPtr+1)
            }
            
            break;
        } // loop
        
        LDA (stmtTokensPtr+0)
        ORA (stmtTokensPtr+1)
        if (NZ)
        {
            LDA (stmtTokensPtr+0)
            STA ZP.IDXL
            LDA (stmtTokensPtr+1)
            STA ZP.IDXH
            Memory.Free();
        }
        
#ifdef DEBUG
        //DumpBasicBuffers();
        //DumpHeap();
#endif
        
#ifdef DEBUG
        LDA #'S'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
      
    // Create token stream from tokenizer buffer slice
    // Input: ZP.FSOURCEADDRESS = start position in BasicTokenizerBuffer
    //        ZP.FLENGTH = length of token stream to copy
    // Output: ZP.FDESTINATIONADDRESS = pointer to allocated token stream copy
    // Munts: ZP.IDXL, ZP.IDXH, ZP.ACCL, ZP.ACCH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS
    // Error: Sets ZP.LastError if memory allocation fails
    createTokenStream()
    {
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop
        {
            // Allocate memory for token stream
            LDA ZP.FLENGTHL
            STA ZP.ACCL
            LDA ZP.FLENGTHH
            STA ZP.ACCH
            Memory.Allocate();  // Returns address in ZP.IDX
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // Allocation failed
                LDA #(Messages.OutOfMemory % 256)
                STA ZP.LastErrorL
                LDA #(Messages.OutOfMemory / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                CLC
                break;
            }
            
            // Set up copy: source = BasicTokenizerBuffer + saved position
            CLC
            LDA # ( Address.BasicTokenizerBuffer & 0xFF)
            ADC ZP.FSOURCEADDRESSL
            STA ZP.FSOURCEADDRESSL
            LDA # ( Address.BasicTokenizerBuffer >> 8)
            ADC ZP.FSOURCEADDRESSH
            STA ZP.FSOURCEADDRESSH
            
            // Destination = allocated memory
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            // Copy the token stream
            Tools.CopyBytes();
            
            // Destination = allocated memory
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            SEC
            
            break;
        } // loop    
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDXH    
        PLA
        STA ZP.IDXL
        PLA
    }
}
