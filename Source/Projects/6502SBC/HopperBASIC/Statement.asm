unit Statement
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Expression"
    uses "Tools"
    
    uses "Variables"
    
    // Execute a statement starting from current token position
    // Assumes ZP.CurrentToken contains the first token of the statement
    // Returns C if successful, NC if error (error stored in ZP.LastError)
    Execute()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'S'
        Serial.WriteChar();
        LDA ZP.CurrentToken
        Serial.HexOut();
#endif
        
        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Tokens.PRINT:
            {
                executePrint();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.IF:
            {
                executeIf();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.RETURN:
            {
                executeReturn();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.END:
            {
                executeEnd();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                return;
            }
            case Tokens.IDENTIFIER:
            {
                // Could be assignment or function call
                executeIdentifier();
#ifdef DEBUG
                LDA #'S'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
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
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
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
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
#endif
                CLC  // Error
                return;
            }
        }
    }
    
    // Execute PRINT statement
    // PRINT <expression>
    executePrint()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'P'
        Serial.WriteChar();
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
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
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
        Stacks.PopTop();  // Pop result into ZP.TOP
        Tools.PrintDecimalWord();
        
        // Print newline
        LDA #'\n'
        Serial.WriteChar();
        
#ifdef DEBUG
        LDA #'P'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
        
        SEC  // Success
    }
    
    // Execute IF statement
    // IF <expression> THEN <statement>
    executeIf()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'I'
        Serial.WriteChar();
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
        Stacks.PopTop();  // Pop condition into ZP.TOP
        
        // Check if condition is true (non-zero)
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (Z)
        {
            // Condition is false, skip to end of line
#ifdef DEBUG
            LDA #'I'
            Serial.WriteChar();
            LDA #'>'
            Serial.WriteChar();
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
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
    }
    
    // Execute RETURN statement
    // RETURN [<expression>]
    executeReturn()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'R'
        Serial.WriteChar();
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
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
        
        CLC  // Error
        BRK
    }
    
    // Execute END statement
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
    
    // Execute identifier (assignment or function call)
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
    
    
    // Execute variable declaration
    // INT/WORD/BIT identifier [= expression]
    executeVariableDeclaration()
    {
#ifdef DEBUG
        LDA #'<'
        Serial.WriteChar();
        LDA #'V'
        Serial.WriteChar();
        LDA #'D'
        Serial.WriteChar();
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
                        Stacks.PopNext();  // Result in ZP.NEXT, type in ZP.NEXTT
                        
                        expectEOF();
                        if (NC) { break; }
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
                    LDX #BasicType.INT
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
            STA ZP.ACCL
            STZ ZP.ACCH
    
            // Call Variables.Declare
            // Input: ZP.TOP = name pointer, ZP.ACC = symbolType|dataType (packed),
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
        Serial.WriteChar();
        LDA #'D'
        Serial.WriteChar();
        LDA #'>'
        Serial.WriteChar();
#endif
    }
    
    expectEOF()
    {
        PHA
        
        // expect no more tokens
        Tokenizer.NextToken();
        Messages.CheckError();
        if (C)
        {
            CMP # Tokens.EOF
            if (NZ)
            {
                CLC // expected EOF
            }
        }
        
        if (NC) 
        { 
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
        }
        
        PLA
    }
    
    // source in FSOURCEADDRESS, length in FLENGTH
    // resulting stream in FDESTINATIONADDRESS
    // munts FSOURCEADDRESS
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
