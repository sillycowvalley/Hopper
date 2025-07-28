unit Statement
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Expression"
    uses "Tools"
    
    uses "Variables"
    
    // Private Statement layer storage - BasicProcessBuffer2 (32 bytes at 0x09C0-0x09DF)
    const uint stmtNamePtr     = Address.BasicProcessBuffer2;      // 0x09C0: 2 bytes - identifier name pointer
    const uint stmtValue       = Address.BasicProcessBuffer2 + 2;  // 0x09C2: 2 bytes - initial/evaluated value
    const uint stmtTokensPtr   = Address.BasicProcessBuffer2 + 4;  // 0x09C4: 2 bytes - tokens pointer  
    const uint stmtTypeInfo    = Address.BasicProcessBuffer2 + 6;  // 0x09C6: 2 bytes - type information
    const uint stmtTokPos      = Address.BasicProcessBuffer2 + 8;  // 0x09C8: 2 bytes - saved tokenizer position
    const uint stmtTokLen      = Address.BasicProcessBuffer2 + 10; // 0x09CA: 2 bytes - token stream length
    // 20 bytes available for future statement needs (0x09CC-0x09DF)

    
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
        
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.REM:
            case Tokens.COMMENT:
            {
                // Comments are no-ops - just advance to next token
                Tokenizer.NextToken();
                
        #ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
        #endif
                
                SEC  // Success
                return;
            }
            case Tokens.PRINT:
            {
                executePrint();
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                return;
            }
            case Tokens.IF:
            {
                executeIf();
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                return;
            }
            case Tokens.RETURN:
            {
                executeReturn();
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                return;
            }
            case Tokens.END:
            {
                executeEnd();
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // Could be assignment or function call
                executeIdentifier();
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                return;
            }
            
            case Tokens.INT:
            case Tokens.WORD:
            case Tokens.BIT:
            {
                executeVariableDeclaration();
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                return;
            }
            
            default:
            {
                // Unexpected token for statement
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
#ifdef DEBUG
                LDA #'S'
                Tools.COut();
                LDA #'>'
                Tools.COut();
#endif
                CLC  // Error
                return;
            }
        }
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
        Expression.Evaluate();
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
        Expression.Evaluate();
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
            Expression.Evaluate();
            Messages.CheckError();
            if (NC) { return; }
        }
        
        // TODO: Actually return from function when we have function support
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        
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
        CLC  // Error
        BRK
    }
    
    // Execute identifier statement (assignment or function call - stub implementation)
    // Input: ZP.CurrentToken = IDENTIFIER token
    // Output: Error (not implemented)
    // Munts: ZP.LastError
    // Error: Always sets ZP.LastError (not implemented)
    executeIdentifier()
    {
        // TODO: Handle variable assignment and function calls
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Error
        BRK
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

        loop
        {
            LDA ZP.CurrentToken
            PHA  // Save type token
        
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; } // error exit
            
            // Check that we have an identifier
            LDA ZP.CurrentToken
            CMP #Tokens.IDENTIFIER
            if (NZ)
            {
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
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
                Objects.Find();
                if (C)  
                {
                    LDA #(Messages.FunctionExists % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.FunctionExists / 256)
                    STA ZP.LastErrorH
                    CLC  // Error
                    break;
                }
                
                // if it already exists, try to delete it
                STZ ZP.SymbolIteratorFilter // constant or variable
                Variables.Find();
                if (C)
                {
                    Variables.GetType();
                    if (NC) { break; }
                    LDA ZP.ACCL
                    AND #0xF0
                    CMP #(SymbolType.CONSTANT << 4)
                    if (Z)
                    {
                        LDA #(Messages.ConstantExists % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.ConstantExists / 256)
                        STA ZP.LastErrorH
                        CLC  // Error
                        break;
                    }
                    // must be a variable, delete it (name ptr in TOP)
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
                            Expression.Evaluate();
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
                        STA ZP.IDYL
                        LDA ZP.FDESTINATIONADDRESSH
                        STA ZP.IDYH
                     
                        // Pop the result into NEXT
                        Stacks.PopNext();  // Result in ZP.NEXT, type in ZP.NEXTT,  modifies X
                    }
                    case Tokens.EOL:
                    {
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
        
        PLX  // Get type token
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
                break;
            }
            
            // Pack symbolType|dataType: VARIABLE(1) in high nibble, dataType in low nibble
            TXA  // dataType in A
            ORA #(SymbolType.VARIABLE << 4)
            STA ZP.ACCT
            
            // Call Variables.Declare
            // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
            //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
            Variables.Declare();
            Messages.CheckError();
            if (C)
            {
                STZ ZP.IDYL
                STZ ZP.IDYH
            }
            
            break;
        } // loop
        
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)
        {
            LDA ZP.IDYL
            STA ZP.IDXL
            LDA ZP.IDYH
            STA ZP.IDXH
            Memory.Free();
        }
        
#ifdef DEBUG
        //DumpBasicBuffers();
        //DumpHeap();
#endif
        
#ifdef DEBUG
        LDA #'V'
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
