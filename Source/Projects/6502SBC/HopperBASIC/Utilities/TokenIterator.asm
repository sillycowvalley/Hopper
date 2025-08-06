unit TokenIterator
{
    uses "Tokenizer"
    
    // API Status: Clean
    // Simple token stream iterator for function body display
    
    
    // Memory layout for token iterator state
    const byte tokIterCurrent        = ZP.M0;  // current token value (1 byte)
    const byte tokIterBaseL          = ZP.M1;  // token stream base pointer low (1 byte)
    const byte tokIterBaseH          = ZP.M2;  // token stream base pointer high (1 byte)
    const byte tokIterIndentLevel    = ZP.M3;  // current indentation level (1 byte, in units of 4 spaces)
    

    
    
    // Initialize token iterator at start of token stream
    // Input: ZP.IDY = token stream base pointer (16-bit)
    // Output: Iterator positioned at first token, C set if stream has content, NC if empty/null
    // Modifies: ZP.LCURRENTL/H (position = 0), tokIterCurrent (first token), tokIterBaseL/H (base pointer)
    // Uses: ZP.L* scratch space (cannot call symbol table APIs after this until done iterating)
    Start()
    {
        PHA
        PHY
        
        // Save token stream base pointer
        LDA ZP.IDYL
        STA tokIterBaseL
        LDA ZP.IDYH
        STA tokIterBaseH
        
        // Check for null pointer
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            // Null stream - set current token to 0 and return NC
            STZ tokIterCurrent
            PLY
            PLA
            CLC
            return;
        }
        
        // Initialize position to 0
        STZ ZP.LCURRENTL
        STZ ZP.LCURRENTH
        
        // Load first token
        LDY #0
        LDA [ZP.IDY], Y
        STA tokIterCurrent
        
        // Check if stream is empty (first token is 0)
        if (Z)
        {
            PLY
            PLA
            CLC  // Empty stream
            return;
        }
        
        PLY
        PLA
        SEC  // Stream has content
    }
    
    // Advance to next token in stream
    // Input: Iterator must be initialized with Start()
    // Output: C set if advanced to valid token, NC if reached end of stream
    // Modifies: ZP.LCURRENTL/H (advanced), tokIterCurrent (next token value)
    // Uses: ZP.L* scratch space (cannot call symbol table APIs while iterating)
    Next()
    {
        PHA
        PHY
        
        loop // Single exit block
        {
            // Current token determines how many bytes to skip
            LDA tokIterCurrent
            switch (A)
            {
                case Token.NUMBER:
                case Token.IDENTIFIER:
                case Token.STRING:
                case Token.REM:
                case Token.COMMENT:
                {
                    // These tokens have inline string data - skip past the string
                    skipInlineString();
                    if (NC) { break; } // Error or end of stream
                }
                default:
                {
                    // Regular token - just advance by 1 byte
                    INC ZP.LCURRENTL
                    if (Z)
                    {
                        INC ZP.LCURRENTH
                    }
                }
            }
            
            // Calculate current address: base + position
            CLC
            LDA tokIterBaseL
            ADC ZP.LCURRENTL
            STA ZP.IDYL
            LDA tokIterBaseH
            ADC ZP.LCURRENTH
            STA ZP.IDYH
            
            // Load token at current position
            LDY #0
            LDA [ZP.IDY], Y
            STA tokIterCurrent
            
            // Check if we hit end of stream (token = 0)
            if (Z)
            {
                CLC  // End of stream
                break;
            }
            
            SEC  // Valid token loaded
            break;
        }
        
        PLY
        PLA
    }
    
    // Skip past null-terminated string at current position
    // Input: ZP.LCURRENTL/H = current position pointing to start of string
    // Output: ZP.LCURRENTL/H advanced past null terminator, C set if successful, NC if error
    // Modifies: ZP.LCURRENTL/H (advanced), ZP.IDY (temporary address calculation)
    skipInlineString()
    {
        PHA
        PHY
        
        loop
        {
            // Advance position by 1 byte first
            INC ZP.LCURRENTL
            if (Z)
            {
                INC ZP.LCURRENTH
            }
            
            // Calculate current address: base + position
            CLC
            LDA tokIterBaseL
            ADC ZP.LCURRENTL
            STA ZP.IDYL
            LDA tokIterBaseH
            ADC ZP.LCURRENTH
            STA ZP.IDYH
            
            // Load character at current position
            LDY #0
            LDA [ZP.IDY], Y
            if (Z) 
            { 
                // Found null terminator - advance past it
                INC ZP.LCURRENTL
                if (Z)
                {
                    INC ZP.LCURRENTH
                }
                SEC  // Success
                break; 
            }
            
            // Continue to next character
        }
        
        PLY
        PLA
    }
    
    // Get current token value
    // Input: Iterator must be initialized and positioned
    // Output: A = current token value
    // Preserves: Everything except A
    GetCurrent()
    {
        LDA tokIterCurrent
    }
    
    // Get pointer to current token's inline data (for literals)
    // Input: Iterator positioned at token with inline data (NUMBER, IDENTIFIER, STRING, REM, COMMENT)
    // Output: ZP.IDY = pointer to start of inline data (after token byte)
    // Preserves: Iterator position, all registers except output
    GetCurrentData()
    {
        PHA
        
        // Calculate address after current token: base + position + 1
        CLC
        LDA tokIterBaseL
        ADC ZP.LCURRENTL
        STA ZP.IDYL
        LDA tokIterBaseH
        ADC ZP.LCURRENTH
        STA ZP.IDYH
        
        // Advance past the token byte to point at data
        INC ZP.IDYL
        if (Z)
        {
            INC ZP.IDYH
        }
        
        PLA
    }
    
    

    // Check if token increases indentation (+4 spaces)
    // Input: A = token value
    // Output: C set if token increases indent, NC if not
    // Preserves: A, all other registers
    isIndentIncreaseToken()
    {
        switch (A)
        {
            case Token.FUNC:
            case Token.IF:
            case Token.FOR:
            case Token.WHILE:
            case Token.BEGIN:
            { SEC return; }
        }
        
        CLC // Not an indent-increasing token
    }

    // Check if token decreases indentation (-4 spaces)
    // Input: A = token value
    // Output: C set if token decreases indent, NC if not
    // Preserves: A, all other registers
    isIndentDecreaseToken()
    {
        switch (A)
        {
            case Token.ENDFUNC:
            case Token.END:
            case Token.NEXT:
            case Token.WEND:
            case Token.UNTIL:
            { SEC return; }
        }
        CLC // Not an indent-decreasing token
    }

    // Print current indentation (tokIterIndentLevel * 4 spaces)
    // Input: None (uses tokIterIndentLevel)
    // Output: Appropriate number of spaces printed to serial
    // Preserves: Everything
    printIndentation()
    {
        PHA
        PHX
        
        LDX tokIterIndentLevel
        loop
        {
            CPX #0
            if (Z) { break; }
            
            // Print 4 spaces for each indent level
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
            
            DEX
        }
        
        PLX
        PLA
    }

    // Render token stream as formatted BASIC code with indentation
    // Input: ZP.IDY = token stream pointer (16-bit)
    // Output: Token stream rendered to serial with proper formatting and 4-space indentation
    // Modifies: ZP.L* scratch space (TokenIterator state), serial output
    // Uses: TokenIterator scratch space (cannot call symbol table APIs while rendering)
    RenderTokenStream()
    {
        PHA
        PHX
        PHY
        
        // Initialize iterator at start of token stream
        Start(); // Input: ZP.IDY = token stream base pointer
        if (NC)
        {
            // Empty or null stream - already handled by caller
            PLY
            PLX
            PLA
            return;
        }
        
        // Initialize indentation level to 1 (we're inside a function/BEGIN block)
        LDA #1
        STA tokIterIndentLevel
        
        // Render each statement in the token stream
        loop
        {
            // Render one complete statement with smart indentation
            renderStatementWithIndent();
            if (NC) { break; } // End of stream
            
            // Add newline after statement
            Tools.NL();
        }
        
        PLY
        PLX
        PLA
    }

    // Render a single statement with smart indentation
    // Input: Iterator positioned at start of statement
    // Output: Statement rendered to serial with proper indentation, iterator advanced past statement
    // Modifies: Iterator position, tokIterIndentLevel, serial output
    renderStatementWithIndent()
    {
        PHA
        PHX
        PHY
        
        // Check if first token of statement decreases indentation
        GetCurrent(); // A = current token value
        isIndentDecreaseToken();
        if (C)
        {
            // Decrease indent before printing this statement
            LDA tokIterIndentLevel
            if (NZ) // Prevent underflow
            {
                DEC tokIterIndentLevel
            }
        }
        
        // Print indentation for this statement
        printIndentation();
        
        // Now render the statement
        loop
        {
            // Get current token
            GetCurrent(); // A = current token value
            
            // Check for statement terminators or end of stream
            CMP #Token.EOL
            if (Z) 
            { 
                Next(); // Skip EOL and continue
                SEC // More statements may follow
                break; 
            }
            CMP #Token.COLON
            if (Z) 
            { 
                Next(); // Skip colon and continue  
                SEC // More statements follow
                break; 
            }
            CMP #0 // Null terminator
            if (Z) 
            { 
                CLC // End of stream
                break; 
            }
            
            // Check if this token increases indentation (for next statement)
            PHA // Save token for rendering
            isIndentIncreaseToken();
            if (C)
            {
                // This token increases indent level for subsequent statements
                INC tokIterIndentLevel
            }
            PLA // Restore token for rendering
            
            // Render the current token
            renderToken(); // Input: A = token value
            
            // Advance to next token
            Next();
            if (NC) 
            { 
                CLC // End of stream
                break; 
            }
        }
        
        PLY
        PLX
        PLA
    }
    

    // Render a single statement from current iterator position
    // Input: Iterator positioned at start of statement
    // Output: Statement rendered to serial (no newline), iterator advanced past statement
    // Modifies: Iterator position, serial output
    renderStatement()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Get current token
            GetCurrent(); // A = current token value
            
            // Check for statement terminators or end of stream
            CMP #Token.EOL
            if (Z) 
            { 
                Next(); // Skip EOL and continue
                SEC // More statements may follow
                break; 
            }
            CMP #Token.COLON
            if (Z) 
            { 
                Next(); // Skip colon and continue  
                SEC // More statements follow
                break; 
            }
            CMP #0 // Null terminator
            if (Z) 
            { 
                CLC // End of stream
                break; 
            }
            
            // Render the current token
            renderToken(); // Input: A = token value
            
            // Advance to next token
            Next();
            if (NC) 
            { 
                CLC // End of stream
                break; 
            }
        }
        
        PLY
        PLX
        PLA
    }

    // Render a single token with appropriate formatting
    // Input: A = token value, iterator positioned at token with inline data
    // Output: Token rendered to serial with appropriate spacing
    renderToken()
    {
        PHA
        PHX
        PHY
        
        switch (A)
        {
            case Token.NUMBER:
            {
                GetCurrentData(); // ZP.IDY = pointer to number string
                Tools.PrintStringIDY();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.IDENTIFIER:
            {
                GetCurrentData(); // ZP.IDY = pointer to identifier string
                Tools.PrintStringIDY();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.STRING:
            case Token.STRINGLIT:
            {
                LDA #'"'
                Serial.WriteChar();
                GetCurrentData(); // ZP.IDY = pointer to string content
                Tools.PrintStringIDY();
                LDA #'"'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.REM:
            {
                LDA #Token.REM
                Tokenizer.PrintKeyword();
                LDA #' '
                Serial.WriteChar();
                GetCurrentData(); // ZP.IDY = pointer to comment text
                Tools.PrintStringIDY();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.COMMENT:
            {
                LDA #'\''
                Serial.WriteChar();
                GetCurrentData(); // ZP.IDY = pointer to comment text
                Tools.PrintStringIDY();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.TRUE:
            {
                LDA #Token.TRUE
                Tokenizer.PrintKeyword();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.FALSE:
            {
                LDA #Token.FALSE
                Tokenizer.PrintKeyword();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.EQUALS:
            {
                LDA #'='
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.PLUS:
            {
                LDA #'+'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.MINUS:
            {
                LDA #'-'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.MULTIPLY:
            {
                LDA #'*'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.DIVIDE:
            {
                LDA #'/'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.LPAREN:
            {
                LDA #'('
                Serial.WriteChar();
            }
            case Token.RPAREN:
            {
                LDA #')'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.LT:
            {
                LDA #'<'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.GT:
            {
                LDA #'>'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.LE:
            {
                LDA #'<'
                Serial.WriteChar();
                LDA #'='
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.GE:
            {
                LDA #'>'
                Serial.WriteChar();
                LDA #'='
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.NOTEQUAL:
            {
                LDA #'<'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.BITWISE_AND:
            {
                LDA #'&'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.BITWISE_OR:
            {
                LDA #'|'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.COMMA:
            {
                LDA #','
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            case Token.SEMICOLON:
            {
                LDA #';'
                Serial.WriteChar();
                LDA #' '
                Serial.WriteChar();
            }
            default:
            {
                // Check if it's a keyword we haven't handled specifically
                PHA // Save token value
                Tokenizer.IsKeyword();
                if (C)
                {
                    // Print keyword
                    PLA
                    Tokenizer.PrintKeyword();
                    LDA #' '
                    Serial.WriteChar(); // Space after keywords
                }
                else
                {
                    // Unknown token - show as hex for debugging
                    LDA #'?'
                    Serial.WriteChar();
                    PLA // Get token value for hex output
                    Serial.HexOut();
                    LDA #' '
                    Serial.WriteChar();
                }
            }
        }
        
        PLY
        PLX
        PLA
    }
    
}
