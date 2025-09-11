unit Lexer
{
    friend CC;
    
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
    
    const byte tokenBuffer   = lexSlots+4;  // Pointer to token string buffer
    const byte tokenBufferL  = lexSlots+4;
    const byte tokenBufferH  = lexSlots+5;
    
    const byte tokenLength   = lexSlots+6;  // Current token length
    const byte TokenType     = lexSlots+7;  // Current token type
    
    const byte tokenValue    = lexSlots+8;  // Token value (for numbers)
    const byte tokenValueL   = lexSlots+8;
    const byte tokenValueH   = lexSlots+9;
    
    // File reading state
    const byte bufferIndex   = lexSlots+10; // Index into current FileDataBuffer
    const byte bufferEnd     = lexSlots+11; // End of valid data in buffer
    const byte hasMoreData   = lexSlots+12; // Flag: more file data available
    
    const byte maxTokenLength = 32;
    
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
            LDA #Error.OutOfMemory
            Errors.ShowError();
            CLC
            return;
        }
        
        LDA ZP.IDXL
        STA tokenBufferL
        LDA ZP.IDXH
        STA tokenBufferH
        
        // Initialize state
        LDA #1
        STA currentLineL
        STZ currentLineH
        
        STZ tokenLength
        STZ TokenType
        
        LDA #1
        STA hasMoreData  // Assume more data initially
        STZ bufferIndex
        STZ bufferEnd
        
        // Prime the pump - get first chunk
        refillBuffer();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        // Get first two characters
        advance();
        advance();
        
        SEC
    }
    
    // Refill buffer from file
    refillBuffer()
    {
        File.NextStream();
        if (NC)
        {
            STZ hasMoreData
            CLC
            return;
        }
        
        STZ bufferIndex
        LDA File.TransferLengthL
        STA bufferEnd
        
        SEC
    }
    
    // Get next character
    advance()
    {
        // Move peek to current
        LDA peekChar
        STA currentChar
        
        // Check for newline
        CMP #Char.EOL
        if (Z)
        {
            INC currentLineL
            if (Z)
            {
                INC currentLineH
            }
        }
        
        // Get next character
        LDA bufferIndex
        CMP bufferEnd
        if (Z)  // Need more data
        {
            LDA hasMoreData
            if (Z)
            {
                STZ peekChar  // EOF
                return;
            }
            
            refillBuffer();
            if (NC)
            {
                STZ peekChar  // EOF
                return;
            }
        }
        
        // Get character from buffer
        PHY
        LDY bufferIndex
        LDA File.FileDataBuffer, Y
        STA peekChar
        PLY
        
        INC bufferIndex
    }
    
    // Skip whitespace and comments
    skipWhitespace()
    {
        loop
        {
            LDA currentChar
            
            CMP #' '
            if (Z) { advance(); continue; }
            
            CMP #Char.Tab
            if (Z) { advance(); continue; }
            
            CMP #Char.EOL
            if (Z) { advance(); continue; }
            
            CMP #13  // CR
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
                        if (Z) { break; }  // EOF
                        CMP #Char.EOL
                        if (Z) { break; }
                    }
                    continue;
                }
            }
            
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
                        if (Z) 
                        { 
                            LDA #Error.UnterminatedComment
                            Errors.ShowError();
                            break; 
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
                                break;
                            }
                        }
                        advance();
                    }
                    continue;
                }
            }
            
            break;  // Not whitespace
        }
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
                if (NZ) { break; }  // End of identifier
            }
            
            // Add to token buffer
            LDY tokenLength
            CPY #(maxTokenLength-1)
            if (Z)
            {
                LDA #Error.TokenTooLong
                Errors.ShowError();
                break;
            }
            
            LDA currentChar
            STA [tokenBuffer], Y
            INC tokenLength
            
            advance();
        }
        
        // Null terminate
        LDX tokenLength
        LDA #0
        STA [tokenBuffer], Y
        
        PLY
        
        // Check for keywords
        checkKeyword();
    }
    
    // Check if identifier is a keyword
    checkKeyword()
    {
         Tokens.CheckKeywords();
    }
    
    // Scan number
    scanNumber()
    {
        STZ tokenValueL
        STZ tokenValueH
        
        loop
        {
            LDA currentChar
            Char.IsDigit();
            if (NC) { break; }
            
            // value = value * 10 + digit
            // Multiply current value by 10
            LDA tokenValueL
            STA ZP.ACCL
            LDA tokenValueH
            STA ZP.ACCH
            
            // TODO: Multiply by 10
            // For now, simple accumulation
            
            LDA currentChar
            SEC
            SBC #'0'
            CLC
            ADC tokenValueL
            STA tokenValueL
            LDA #0
            ADC tokenValueH
            STA tokenValueH
            
            advance();
        }
        
        LDA #Token.IntegerLiteral
        STA TokenType
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
                Errors.ShowError();
                break;
            }
            
            CMP #'"'
            if (Z)
            {
                advance();  // Skip closing quote
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
                Errors.ShowError();
                break;
            }
            
            STA [tokenBuffer], Y
            INC tokenLength
            
            advance();
        }
        
        // Null terminate
        LDY tokenLength
        LDA #0
        STA [tokenBuffer], Y
        
        LDA #Token.StringLiteral
        STA TokenType
        PLY
    }
    
    // Get next token
    NextToken()  // Returns token type in A
    {
        skipWhitespace();
        
        LDA currentChar
        if (Z)  // EOF
        {
            LDA # Token.EndOfFile
            STA TokenType
            return;
        }
        
        // Check for identifier/keyword
        Char.IsAlpha();
        if (C)
        {
            scanIdentifier();
            LDA TokenType
            return;
        }
        
        // Check for number
        LDA currentChar
        Char.IsDigit();
        if (C)
        {
            scanNumber();
            LDA TokenType
            return;
        }
        
        // Check for string
        LDA currentChar
        CMP #'"'
        if (Z)
        {
            scanString();
            LDA TokenType
            return;
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
                    Errors.ShowError();
                }
            }
            default:
            {
                LDA # Error.UnexpectedCharacter
                Errors.ShowError();
            }
        }
        
        LDA TokenType
    }
    
    // Cleanup
    Dispose()
    {
        LDA tokenBufferL
        STA ZP.IDXL
        LDA tokenBufferH
        STA ZP.IDXH
        Memory.Free();
    }
}
