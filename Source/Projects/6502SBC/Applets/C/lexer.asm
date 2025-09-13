unit Lexer
{
    uses "../System/Definitions"
    uses "../System/File"
    uses "../System/Memory"
    uses "../System/Char"
    
    uses "Errors"
    uses "Tokens"
    
    // Lexer zero page allocation
    const byte lexSlots = 0x70;
    
    const byte currentLine   = lexSlots+0;  // Line number for errors
    const byte currentLineL  = lexSlots+0;
    const byte currentLineH  = lexSlots+1;
    
    const byte currentChar   = lexSlots+2;  // Current character
    const byte peekChar      = lexSlots+3;  // Lookahead character
    
    const byte TokenBuffer   = lexSlots+4;  // Pointer to token string buffer
    const byte TokenBufferL  = lexSlots+4;
    const byte TokenBufferH  = lexSlots+5;
    
    const byte tokenLength   = lexSlots+6;  // Current token length
    const byte TokenType     = lexSlots+7;  // Current token type
    
    const byte TokenValue    = lexSlots+8;  // Token value (for numbers)
    const byte TokenValueL   = lexSlots+8;
    const byte TokenValueH   = lexSlots+9;
    
    // File reading state
    const byte bufferIndexL  = lexSlots+10; // Index into current FileDataBuffer
    const byte bufferIndexH  = lexSlots+11;
    
    const byte maxTokenLength = 64;
    
    // Initialize lexer
    Initialize()
    {
        // Allocate token buffer
        LDA #maxTokenLength
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            Errors.OutOfMemory();
            CLC
            return;
        }
        
        LDA ZP.IDXL
        STA TokenBufferL
        LDA ZP.IDXH
        STA TokenBufferH
        
        // Initialize state
        LDA #1
        STA currentLineL
        STZ currentLineH
        
        STZ tokenLength
        STZ TokenType
        
        // Prime the pump - get first chunk
        refillBuffer();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        STZ peekChar
        STZ currentChar
        
        // Get first two characters
        advance();
        advance();
        
        SEC
    }
    
    // Get current line number for error reporting
    GetLineNumber() // -> ACC
    {
        LDA currentLineL
        STA ZP.ACCL
        LDA currentLineH
        STA ZP.ACCH
    }
    
    // Refill buffer from file
    refillBuffer()
    {
        // Save IDY before File call
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        // Save IDX before File call
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        File.NextStream();
        
        // Restore IDX after File call
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        // Restore IDY after File call
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        if (NC)
        {
            CLC
            return;
        }
        
        STZ bufferIndexL
        STZ bufferIndexH
        
        SEC
    }
    
    // Get next character
    advance()
    {
        // Move peek to current
        LDA peekChar
        STA currentChar
        
        // Check for newline
        CMP # Char.EOL
        if (Z)
        {
            INC currentLineL
            if (Z)
            {
                INC currentLineH
            }
        }
        
        LDA bufferIndexH
        CMP File.TransferLengthH
        if (Z)
        {
            LDA bufferIndexL
            CMP File.TransferLengthL
            if (Z)  // Need more data
            {
                refillBuffer();
                if (NC)
                {
                    STZ peekChar  // EOF
                    return;
                }
            }
        }
        
        // Get character from buffer
        
        // Load FileDataBuffer address into zero page pointer
        LDA #(File.FileDataBuffer % 256)
        STA ZP.IDXL
        LDA #(File.FileDataBuffer / 256)
        STA ZP.IDXH
        
        // Now read via indirect indexed
        PHY
        LDY bufferIndexL // 0..255
        LDA [ZP.IDX], Y
        STA peekChar
        PLY
        
        INC bufferIndexL
        if (Z) { INC bufferIndexH }
    }
    
    // Skip whitespace and comments
    skipWhitespace()
    {
        loop
        {
            LDA currentChar
            
            CMP #' '
            if (Z) { advance(); continue; }
            
            CMP # Char.Tab
            if (Z) { advance(); continue; }
            
            CMP # Char.EOL
            if (Z) { advance(); continue; }
            
            CMP # Char.CR
            if (Z) { advance(); continue; }
            
            // Check for // comment
            CMP #'/'
            if (Z)
            {
                LDA peekChar
                CMP #'/'
                if (Z)
                {
                    // Skip to end of line
                    loop
                    {
                        advance();
                        LDA currentChar
                        if (Z) { SEC break; }  // EOF
                        CMP # Char.EOL
                        if (Z) { SEC break; }
                    }
                    continue;
                }
            }
            
            LDA currentChar 
            
            // Check for /* comment
            CMP #'/'
            if (Z)
            {
                LDA peekChar
                CMP #'*'
                if (Z)
                {
                    advance();  // Skip *
                    advance();  // Move to next
                    
                    // Skip until */
                    loop
                    {
                        LDA currentChar
                        if (Z) // EOF without */
                        { 
                            LDA #Error.UnterminatedComment
                            Errors.Show();
                            CLC
                            return; 
                        }
                        
                        CMP #'*'
                        if (Z)
                        {
                            LDA peekChar
                            CMP #'/'
                            if (Z)
                            {
                                advance();  // Skip /
                                advance();  // Move past comment
                                SEC
                                break;
                            }
                        }
                        advance();
                    } // loop
                    continue;
                }
            }
            SEC
            break;  // Not whitespace
        } // loop
    }
    
    // Collect identifier or keyword
    scanIdentifier()
    {
        PHY
        STZ tokenLength
        
        loop
        {
            LDA currentChar
            Char.IsAlphaNumeric();
            if (NC)
            {
                LDA currentChar
                CMP #'_'
                if (NZ) { SEC break; }  // End of identifier
            }
            
            // Add to token buffer
            LDY tokenLength
            CPY #(maxTokenLength-1)
            if (Z)
            {
                LDA #Error.TokenTooLong
                Errors.Show();
                CLC
                break;
            }
            
            LDA currentChar
            STA [TokenBuffer], Y
            INC tokenLength
            
            advance();
        } // loop
        if (C)
        {
            // Null terminate
            LDY tokenLength
            LDA #0
            STA [TokenBuffer], Y
        }
        PLY
        if (C)
        {
            // Check for keywords
            checkKeyword();
        }
    }
    
    // Check if identifier is a keyword
    checkKeyword()
    {
         Tokens.CheckKeywords();
    }
    
    // Scan number
    scanNumber()
    {
        STZ TokenValueL
        STZ TokenValueH
        
        loop
        {
            LDA currentChar
            Char.IsDigit();
            if (NC) { break; }
            
            // value = value * 10 + digit
            // Multiply current value by 10
            LDA TokenValueL
            STA ZP.ACCL
            LDA TokenValueH
            STA ZP.ACCH
            
            // TODO: Multiply by 10
            // For now, simple accumulation
            
            LDA currentChar
            SEC
            SBC #'0'
            CLC
            ADC TokenValueL
            STA TokenValueL
            LDA #0
            ADC TokenValueH
            STA TokenValueH
            
            advance();
        } // loop
        
        LDA #Token.IntegerLiteral
        STA TokenType
        SEC
    }
    
    scanCharLiteral()
    {
        advance();  // Skip opening quote
        
        LDA currentChar
        if (Z)  // EOF
        {
            LDA #Error.UnterminatedString  // Reuse this error
            Errors.Show();
            CLC
            PLY
            return;
        }
        
        // Handle escape sequences
        CMP #92  // Backslash ASCII
        if (Z)
        {
            advance();
            LDA currentChar
            switch (A)
            {
                case 'n':  { LDA # Char.EOL }
                case 't':  { LDA # Char.Tab }
                case '\\':   { LDA '\\' }  // Backslash
                case '\'':   { LDA '\'' }  // Single quote
                default:   { } // Use as-is
            }
        }
        
        STA TokenValueL  // Store character value
        STZ TokenValueH
        
        advance();
        LDA currentChar
        CMP #39  // Single quote
        if (NZ)  // Should be closing quote
        {
            LDA #Error.UnterminatedString  // Reuse this error
            Errors.Show();
            CLC
            return;
        }
        
        advance();  // Skip closing quote
        
        LDA #Token.CharLiteral
        STA TokenType
        SEC
    }
    
    
    
    // Scan string literal
    scanString()
    {
        PHY
        advance();  // Skip opening quote
        STZ tokenLength
        
        loop
        {
            LDA currentChar
            if (Z)  // EOF
            {
                LDA #Error.UnterminatedString
                Errors.Show();
                CLC
                break;
            }
            
            CMP #'"'
            if (Z)
            {
                advance();  // Skip closing quote
                SEC
                break;
            }
            
            // Handle escape sequences
            CMP #'\\'
            if (Z)
            {
                advance();
                LDA currentChar
                switch (A)
                {
                    case 'n':  { LDA #Char.EOL }
                    case 't':  { LDA #Char.Tab }
                    case '\\': { LDA #'\\' }
                    case '"':  { LDA #'"' }
                    default:   { } // Use as-is
                }
            }
            
            // Add to buffer
            LDY tokenLength
            CPY #(maxTokenLength-1)
            if (Z)
            {
                LDA # Error.StringTooLong
                Errors.Show();
                CLC
                break;
            }
            
            STA [TokenBuffer], Y
            INC tokenLength
            
            advance();
        } // loop
        if (C)
        {
            // Null terminate
            LDY tokenLength
            LDA #0
            STA [TokenBuffer], Y
            
            LDA #Token.StringLiteral
            STA TokenType
        }
        PLY
    }
    
    // Get next token
    NextToken()  // Returns token type in A
    {
        skipWhitespace();
        if (NC)
        {
            return;
        }
        loop
        {
            LDA currentChar
            if (Z)  // EOF
            {
                LDA # Token.EndOfFile
                STA TokenType
                SEC
                break;
            }
            
            // Check for identifier/keyword
            Char.IsAlpha();
            if (C)
            {
                scanIdentifier();
                break;
            }
            
            // Check for number
            LDA currentChar
            Char.IsDigit();
            if (C)
            {
                scanNumber();
                break;
            }
            
            // Check for string
            LDA currentChar
            CMP #'"'
            if (Z)
            {
                scanString();
                break;
            }
            
            // Check for character literal
            LDA currentChar
            CMP #'\''
            if (Z)
            {
                scanCharLiteral();
                if (NC) { return; }
                break;
            }
        
            // Single and double character operators
            switch (A)
            {
                case '+': 
                { 
                    advance();
                    LDA #Token.Plus
                    STA TokenType
                }
                case '-':
                {
                    advance();
                    LDA #Token.Minus
                    STA TokenType
                }
                case '*':
                {
                    advance();
                    LDA #Token.Star
                    STA TokenType
                }
                case '/':
                {
                    advance();
                    LDA #Token.Slash
                    STA TokenType
                }
                case '%':
                {
                    advance();
                    LDA #Token.Percent
                    STA TokenType
                }
                case '(':
                {
                    advance();
                    LDA #Token.LeftParen
                    STA TokenType
                }
                case ')':
                {
                    advance();
                    LDA #Token.RightParen
                    STA TokenType
                }
                case '{':
                {
                    advance();
                    LDA #Token.LeftBrace
                    STA TokenType
                }
                case '}':
                {
                    advance();
                    LDA #Token.RightBrace
                    STA TokenType
                }
                case ';':
                {
                    advance();
                    LDA #Token.Semicolon
                    STA TokenType
                }
                case ',':
                {
                    advance();
                    LDA #Token.Comma
                    STA TokenType
                }
                case '=':
                {
                    advance();
                    LDA peekChar
                    CMP #'='
                    if (Z)
                    {
                        advance();
                        LDA #Token.Equal
                    }
                    else
                    {
                        LDA #Token.Assign
                    }
                    STA TokenType
                }
                case '<':
                {
                    advance();
                    LDA peekChar
                    CMP #'='
                    if (Z)
                    {
                        advance();
                        LDA #Token.LessEqual
                    }
                    else
                    {
                        LDA #Token.Less
                    }
                    STA TokenType
                }
                case '>':
                {
                    advance();
                    LDA peekChar
                    CMP #'='
                    if (Z)
                    {
                        advance();
                        LDA #Token.GreaterEqual
                    }
                    else
                    {
                        LDA #Token.Greater
                    }
                    STA TokenType
                }
                case '!':
                {
                    advance();
                    LDA peekChar
                    CMP #'='
                    if (Z)
                    {
                        advance();
                        LDA # Token.NotEqual
                        STA TokenType
                    }
                    else
                    {
                        LDA # Error.UnexpectedCharacter
                        Errors.Show();
                        CLC
                        break;
                    }
                }
                default:
                {
                    LDA # Error.UnexpectedCharacter
                    Errors.Show();
                    CLC
                    break;
                }
            } // switch
            SEC
            break;
        } // loop
        if (C)
        {
            LDA TokenType
        }
    }
    
    // Cleanup
    Dispose()
    {
        LDA TokenBufferL
        STA ZP.IDXL
        LDA TokenBufferH
        STA ZP.IDXH
        Memory.Free();
    }
}
