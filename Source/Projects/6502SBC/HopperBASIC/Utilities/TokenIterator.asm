unit TokenIterator // TokenIterator.asm
{
    uses "Tokenizer"
    
    // API Status: Clean
    // Simple token stream iterator for function body display
    
    // Initialize token iterator at start of token stream
    // Input: ZP.IDY = token stream base pointer (16-bit)
    // Output: Iterator positioned at first token, C set if stream has content, NC if empty/null
    // Modifies: ZP.TOKPOSL/H (position = 0), TOKCUR (first token), TOKBASEL/H (base pointer)
    // Uses: ZP.TOK* scratch space (cannot call symbol table APIs after this until done iterating)
    Start()
    {
        PHA
        PHY
        
        loop // Single exit block
        {
            // Save token stream base pointer
            LDA ZP.IDYL
            STA ZP.TOKBASEL
            LDA ZP.IDYH
            STA ZP.TOKBASEH
            
            STZ ZP.TOKPREV
            
            
            // Check for null pointer
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                // Null stream - set current token to 0 and return NC
                STZ ZP.TOKCUR
                CLC  // Empty stream
                break;
            }
            
            // Initialize position to 0
            STZ ZP.TOKPOSL
            STZ ZP.TOKPOSH
            
            
            // Load first token
            LDY #0
            LDA [ZP.IDY], Y
            STA ZP.TOKCUR
            
            // Check if stream is empty (first token is 0 or EOF)
            if (Z)
            {
                CLC  // Empty stream (null terminator)
                break;
            }
            CMP #Token.EOF
            if (Z)
            {
                CLC  // Empty stream (starts with EOF)
                break;
            }
            
            SEC  // Stream has content
            break;
        }
        
        PLY
        PLA
    }
    
    // Advance to next token in stream
    // Input: Iterator must be initialized with Start()
    // Output: C set if advanced to valid token, NC if reached end of stream
    // Modifies: ZP.TOKPOSL/H (advanced), TOKCUR (next token value)
    // Uses: ZP.TOK* scratch space (cannot call symbol table APIs while iterating)
    Next()
    {
        PHA
        PHY
        
        loop // Single exit block
        {
            LDA ZP.TOKCUR
            STA ZP.TOKPREV
            
            // Current token determines how many bytes to skip
            LDA ZP.TOKCUR
            switch (A)
            {
                case Token.NUMBER:
                case Token.IDENTIFIER:
                case Token.REM:
                case Token.COMMENT:
                case Token.STRINGLIT:
                {
                    // These tokens have inline string data - skip past the string
                    skipInlineString();
                    if (NC) { break; } // Error or end of stream
                }
                case Token.CHARLIT:
                {
                    // CHARLIT has exactly 1 byte of character data after the token
                    // Advance by 2 bytes total (token + character value)
                    INC ZP.TOKPOSL
                    if (Z) { INC ZP.TOKPOSH }
                    INC ZP.TOKPOSL  
                    if (Z) { INC ZP.TOKPOSH }
                }
                default:
                {
                    // Regular token - just advance by 1 byte
                    INC ZP.TOKPOSL
                    if (Z)
                    {
                        INC ZP.TOKPOSH
                    }
                }
            }
            
            // Calculate current address: base + position
            CLC
            LDA ZP.TOKBASEL
            ADC ZP.TOKPOSL
            STA ZP.STRL
            LDA ZP.TOKBASEH
            ADC ZP.TOKPOSH
            STA ZP.STRH
            
            // Load token at current position
            LDY #0
            LDA [ZP.STR], Y
            STA ZP.TOKCUR
            
            // Check if we hit end of stream (token = 0)
            if (Z)
            {
                CLC  // End of stream
                break;
            }
            CMP #Token.EOF
            if (Z)
            {
                CLC  // End of stream (EOF token)
                break;
            }
            
            SEC  // Valid token loaded
            break;
        }
        
        PLY
        PLA
    }
    
    // Skip past null-terminated string at current position
    // Input: ZP.TOKPOSL/H = current position pointing to start of string
    // Output: ZP.TOKPOSL/H advanced past null terminator, C set if successful, NC if error
    // Modifies: ZP.TOKPOSL/H (advanced), ZP.TOKADDR* (temporary address calculation)
    skipInlineString()
    {
        PHA
        PHY
        
        loop
        {
            // Advance position by 1 byte first
            INC ZP.TOKPOSL
            if (Z)
            {
                INC ZP.TOKPOSH
            }
            
            // Calculate current address: base + position
            CLC
            LDA ZP.TOKBASEL
            ADC ZP.TOKPOSL
            STA ZP.STRL
            LDA ZP.TOKBASEH
            ADC ZP.TOKPOSH
            STA ZP.STRH
            
            // Load character at current position
            LDY #0
            LDA [ZP.STR], Y
            if (Z) 
            { 
                // Found null terminator - advance past it
                INC ZP.TOKPOSL    
                if (Z)
                {
                    INC ZP.TOKPOSH
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
        LDA ZP.TOKCUR
    }
    
    
    // Get pointer to current token's inline data (for literals)
    // Input: Iterator positioned at token with inline data (NUMBER, IDENTIFIER, STRINGLIT, REM, COMMENT)
    // Output: ZP.STR* = pointer to start of inline data (after token byte)
    // Preserves: Iterator position, all registers except output
    GetCurrentData()
    {
        PHA
        
        // Calculate address after current token: base + position + 1
        CLC
        LDA ZP.TOKBASEL
        ADC ZP.TOKPOSL
        STA ZP.STRL
        LDA ZP.TOKBASEH
        ADC ZP.TOKPOSH
        STA ZP.STRH
        
        // Advance past the token byte to point at data
        INC ZP.STRL
        if (Z)
        {
            INC ZP.STRH
        }
        
        PLA
    }
    
    

    // Check if token increases indentation (+4 spaces)
    // Input: A = token value
    // Output: C set if token increases indent, NC if not
    // Preserves: A, all other registers
    isIndentIncreaseToken()
    {
        loop
        {
            switch (A)
            {
                case Token.FUNC:
                case Token.FOR:
                case Token.WHILE:
                case Token.BEGIN:
                {
                    SEC
                    break;
                }
                case Token.IF:
                {
                    SMB0 ZP.TOKSINGLEIF // entering IF line
                    SEC
                    break;
                }
                case Token.ELSE:
                {
                    if (BBR0, ZP.TOKSINGLEIF) // not in a single line IF
                    {
                        SEC
                        break;
                    }
                }
            }
            CLC // Not an indent-increasing token
            break;
        } // single exit
    }

    // Check if token decreases indentation (-4 spaces)
    // Input: A = token value
    // Output: C set if token decreases indent, NC if not
    // Preserves: A, all other registers
    isIndentDecreaseToken()
    {
        loop
        {
            switch (A)
            {
                case Token.EOF:
                case Token.NEXT:
                case Token.WEND:
                case Token.UNTIL:
                case Token.ENDIF:
                case Token.ELSE:
                {
                    SEC
                    break;
                }
            }
            CLC // Not an indent-decreasing token
            break;
        } // single exit
    }

    // Print current indentation (TOKINDENT * 4 spaces)
    // Input: None (uses TOKINDENT)
    // Output: Appropriate number of spaces printed to serial
    // Preserves: Everything
    printIndentation()
    {
        PHA
        PHY
        
        loop
        {
            // Check if we're continuing after a colon
            LDA ZP.TOKCOLON
            if (NZ)
            {
                STZ ZP.TOKCOLON  // Reset flag
                break; // Skip indentation
            }
            LDY ZP.TOKINDENT
            loop
            {
                CPY #0
                if (Z) { break; }
                
                // Print 4 spaces for each indent level
                LDX #4 Print.Spaces();
                
                DEY
            }
            break;
        }
        
        PLY
        PLA
    }
    
    // Check if current position has passed error position and print marker if needed
    // Input: TOKERRORL/H = error position, TOKPOSL/H = current position
    // Output: Error marker printed if we've passed the error position
    // Modifies: TOKERRORFLAG
    // Preserves: All registers
    checkAndPrintErrorMarker()
    {
        PHA
        
        loop // Single exit block
        {
            // Skip if error marker already printed
            LDA ZP.TOKERRORFLAG
            if (NZ) // Set NZ - already printed
            { 
                break; 
            }
            
            // Skip if no error position specified (TOKERRORL/H == 0)
            LDA ZP.TOKERRORL
            ORA ZP.TOKERRORH
            if (Z) // Set Z - no error position
            { 
                break; 
            }
            
            // Check if TOKPOS < TOKERROR?
            LDA ZP.TOKPOSH      // TOKPOS MSB
            CMP ZP.TOKERRORH    // TOKERROR MSB
            if (Z)
            {
                LDA ZP.TOKPOSL   // TOKPOS LSB
                CMP ZP.TOKERRORL // TOKERROR LSB
            }
            if (C) // Set C if TOKPOS >= TOKERROR
            {
                // We've reached or passed the error position
                printErrorMarker();
                break;
            }
            // Current position < error position - not there yet
            break;      
        } // single exit
        PLA
    }
    
    // Print the error marker on the current line
    // Input: None
    // Output: "         <------" printed to serial
    // Modifies: TOKERRORFLAG
    // Preserves: All registers
    printErrorMarker()
    {
        PHA
        PHX
        
        // Mark that we've printed the error marker
        LDA #1
        STA ZP.TOKERRORFLAG
        
        LDX #9
        Print.Spaces();
            
        LDA #( Messages.ErrorMarker % 256)
        STA ZP.STRL
        LDA #( Messages.ErrorMarker / 256)
        STA ZP.STRH
        Print.String();
        
        LDX #3
        Print.Spaces();
        
        // Print the error message
        LDA ZP.LastError
        LDX # MessageExtras.PrefixQuest
        Error.Message();
             
        PLX
        PLA
    }
    

    // Render token stream as formatted BASIC code with indentation
    // Input: ZP.IDY = token stream pointer (16-bit), ZP.ACCL = error position in stream if not zero
    // Output: Token stream rendered to serial with proper formatting and 4-space indentation
    // Modifies: ZP.TOK* scratch space (TokenIterator state), serial output
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
        STA ZP.TOKINDENT
        STZ ZP.TOKCOLON // not on a COLON line
        STZ ZP.TOKERRORFLAG // error marker not yet printed
        STZ ZP.TOKSINGLEIF
        
        // Render each statement in the token stream
        loop
        {
            // Render one complete statement with smart indentation
            renderStatementWithIndent();
            if (NC) { break; } // End of stream
            
            // Check if we've passed the error position on this line
            checkAndPrintErrorMarker();
            
            // Only add newline if not following COLON
            LDA ZP.TOKCOLON
            if (Z) 
            {
                Print.NewLine();
            }
        }
        
        PLY
        PLX
        PLA
    }

    // Render a single statement with smart indentation
    // Input: Iterator positioned at start of statement
    // Output: Statement rendered to serial with proper indentation, iterator advanced past statement
    // Modifies: Iterator position, TOKINDENT, serial output
    renderStatementWithIndent()
    {
        PHA
        PHX
        PHY
        
        STA ZP.TOKPREV // new statement - no preceding ' '
        
        // Check if first token of statement decreases indentation
        GetCurrent(); // A = current token value
        isIndentDecreaseToken();
        if (C)
        {
            // Decrease indent before printing this statement
            LDA ZP.TOKINDENT
            if (NZ) // Prevent underflow
            {
                DEC ZP.TOKINDENT
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
                RMB0 ZP.TOKSINGLEIF // no longer in a single like IF (if you were in one)
                
                Next(); // Skip EOL and continue
                SEC // More statements may follow
                break; 
            }
            CMP #Token.COLON
            if (Z) 
            { 
                renderToken(); // Render the colon first
                Next(); // Skip colon and continue  
                LDA #1
                STA ZP.TOKCOLON  // Set flag for next statement
                SEC // More statements follow
                break; 
            }
            CMP #0 // Null terminator
            if (Z) 
            { 
                CLC // End of stream
                break; 
            }
            CMP #Token.EOF 
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
                INC ZP.TOKINDENT
            }
            PLA // Restore token for rendering
            
            // Render the current token
            renderToken(); // Input: A = token value
            
            if (BBS0, ZP.TOKSINGLEIF) // still within IF line?
            {
                CMP #Token.ENDIF
                if (Z)
                {
                    // compensate for the IF ++ if we are still on the same line
                    LDA ZP.TOKINDENT
                    if (NZ) // Prevent underflow
                    {
                        DEC ZP.TOKINDENT
                    }
                    RMB0 ZP.TOKSINGLEIF
                }
            }
            
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
            CMP #Token.EOF
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
    
    //const char priorSpace = '_'; // for testing
    const char priorSpace = ' ';
    renderOptionalSpace()
    {
        LDA ZP.TOKPREV
        IsKeyword();
        if (C)
        {
            switch (A)
            {
                case Token.ABS:
                case Token.MILLIS:
                case Token.PEEK:
                case Token.POKE:
                case Token.RND:
                case Token.SECONDS:
                case Token.DELAY:
                case Token.ASC:
                case Token.CHR:
                case Token.LEN:
                case Token.PINMODE:
                case Token.READ:
                case Token.WRITE:
                {
                    // followed by '('
                }
                default:
                {
                    GetCurrent(); // A = current token value
                    switch (A)
                    {
                        case Token.RBRACKET:
                        case Token.RPAREN:
                        {
                        }
                        default:
                        {
                            LDA # priorSpace Serial.WriteChar();
                        }
                    }
                }
            }
        }
        else
        {
            // previous is not a keyword
            switch (A)
            {
                case 0x00: // first on new line
                {
                }
                case Token.LBRACKET:
                case Token.LPAREN:
                {
                }
                case Token.IDENTIFIER:
                {
                    GetCurrent(); // A = current token value
                    switch (A)
                    {
                        case Token.LBRACKET:
                        case Token.LPAREN:
                        {
                        }
                        default:
                        {
                            LDA # priorSpace Serial.WriteChar();
                        }
                    }
                }
                default:
                {
                    LDA # priorSpace Serial.WriteChar();
                }
            }
        }
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
                renderOptionalSpace();
                GetCurrentData(); // ZP.TOKADDR* = pointer to number string
                Print.String();
            }
            case Token.IDENTIFIER:
            {
                renderOptionalSpace();
                GetCurrentData(); // ZP.TOKADDR* = pointer to identifier string
                Print.String();
            }
            case Token.STRINGLIT:
            {
                renderOptionalSpace();
                LDA #'"'
                Serial.WriteChar();
                GetCurrentData(); // ZP.TOKADDR* = pointer to string content
                Print.String();
                LDA #'"'
                Serial.WriteChar();
            }
            case Token.CHARLIT:
            {
                renderOptionalSpace();
                LDA #'\''
                Serial.WriteChar();
                GetCurrentData(); // ZP.STR* = pointer to character value
                LDY #0
                LDA [ZP.STR], Y   // Get the character
                Serial.WriteChar();
                LDA #'\''
                Serial.WriteChar();
            }
            case Token.REM:
            {
                renderOptionalSpace();
                LDA #Token.REM
                Tokens.PrintKeyword();
                Print.Space();
                GetCurrentData(); // ZP.TOKADDR* = pointer to comment text
                Print.String();
            }
            case Token.COMMENT:
            {
                renderOptionalSpace();
                LDA #'!'
                Serial.WriteChar();
                GetCurrentData(); // ZP.TOKADDR* = pointer to comment text
                Print.String();
            }
            case Token.TRUE:
            {
                renderOptionalSpace();
                LDA #Token.TRUE
                Tokens.PrintKeyword();
            }
            case Token.FALSE:
            {
                renderOptionalSpace();
                LDA #Token.FALSE
                Tokens.PrintKeyword();
            }
            case Token.EQUALS:
            {
                renderOptionalSpace();
                LDA #'='
                Serial.WriteChar();
            }
            case Token.PLUS:
            {
                renderOptionalSpace();
                LDA #'+'
                Serial.WriteChar();
            }
            case Token.MINUS:
            {
                renderOptionalSpace();
                LDA #'-'
                Serial.WriteChar();
            }
            case Token.MULTIPLY:
            {
                renderOptionalSpace();
                LDA #'*'
                Serial.WriteChar();
            }
            case Token.DIVIDE:
            {
                renderOptionalSpace();
                LDA #'/'
                Serial.WriteChar();
            }
            case Token.LPAREN:
            {
                renderOptionalSpace();
                LDA #'('
                Serial.WriteChar();
            }
            case Token.RPAREN:
            {
                LDA #')'
                Serial.WriteChar();
            }
            case Token.LBRACKET:
            {
                LDA #'['
                Serial.WriteChar();
            }
            case Token.RBRACKET:
            {
                LDA #']'
                Serial.WriteChar();
            }
            case Token.LT:
            {
                renderOptionalSpace();
                LDA #'<'
                Serial.WriteChar();
            }
            case Token.GT:
            {
                renderOptionalSpace();
                LDA #'>'
                Serial.WriteChar();
            }
            case Token.LE:
            {
                renderOptionalSpace();
                LDA #'<'
                Serial.WriteChar();
                LDA #'='
                Serial.WriteChar();
            }
            case Token.GE:
            {
                renderOptionalSpace();
                LDA #'>'
                Serial.WriteChar();
                LDA #'='
                Serial.WriteChar();
            }
            case Token.NOTEQUAL:
            {
                renderOptionalSpace();
                LDA #'<'
                Serial.WriteChar();
                LDA #'>'
                Serial.WriteChar();
            }
            case Token.BITWISE_AND:
            {
                renderOptionalSpace();
                LDA #'&'
                Serial.WriteChar();
            }
            case Token.BITWISE_OR:
            {
                renderOptionalSpace();
                LDA #'|'
                Serial.WriteChar();
            }
            case Token.COMMA:
            {
                LDA #','
                Serial.WriteChar();
            }
            case Token.SEMICOLON:
            {
                LDA #';'
                Serial.WriteChar();
            }
            case Token.COLON:
            {
                renderOptionalSpace();
                LDA #':'
                Serial.WriteChar();
            }
            default:
            {
                // Check if it's a keyword we haven't handled specifically
                PHA // Save token value
                Tokens.IsKeyword();
                if (C)
                {
                    renderOptionalSpace();
                    // Print keyword
                    PLA
                    Tokens.PrintKeyword();
                }
                else
                {
                    // Unknown token - show as hex for debugging
                    LDA #'?'
                    Serial.WriteChar();
                    PLA // Get token value for hex output
                    Serial.HexOut();
                }
            }
        }
        
        PLY
        PLX
        PLA
    }
    
}
