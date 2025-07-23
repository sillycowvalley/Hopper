unit Tokenizer
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "Limits"
    uses "Messages"
    
    // Phase 1 Token definitions (lean set)
    enum Tokens
    {
        // Console commands (1-16)
        NEW      = 0x01,
        LIST     = 0x02,
        RUN      = 0x03,
        CLEAR    = 0x04,
        VARS     = 0x05,
        FUNCS    = 0x06,
        FORGET   = 0x07,
        SAVE     = 0x08,
        LOAD     = 0x09,
        DIR      = 0x0A,
        DEL      = 0x0B,
        MEM      = 0x0C,
        BYE      = 0x0D,
        EOL      = 0x0E, // End of line/empty line
        
        // Type declarations (16-31)  
        INT      = 0x10,
        WORD     = 0x11,
        BIT      = 0x12,
        
        // Language keywords (32-47)
        PRINT    = 0x20,
        IF       = 0x21,
        THEN     = 0x22,
        FUNC     = 0x23,
        ENDFUNC  = 0x24,
        RETURN   = 0x25,
        BEGIN    = 0x26,
        END      = 0x27,
        
        // Operators (64-79)
        EQUALS   = 0x40,  // =
        PLUS     = 0x41,  // +
        MINUS    = 0x42,  // -
        LPAREN   = 0x43,  // (
        RPAREN   = 0x44,  // )
        NOTEQUAL = 0x45,  // <>
        
        // Literals and identifiers (128+)
        NUMBER     = 0x80,
        STRING     = 0x81,
        IDENTIFIER = 0x82,
        EOF        = 0x83,
    }
    
    // Compact keyword table: length, token, chars...
    const byte[] keywords = {
        3, Tokens.NEW, 'N', 'E', 'W',
        4, Tokens.LIST, 'L', 'I', 'S', 'T',
        3, Tokens.RUN, 'R', 'U', 'N',
        5, Tokens.CLEAR, 'C', 'L', 'E', 'A', 'R',
        4, Tokens.VARS, 'V', 'A', 'R', 'S',
        5, Tokens.FUNCS, 'F', 'U', 'N', 'C', 'S',
        6, Tokens.FORGET, 'F', 'O', 'R', 'G', 'E', 'T',
        4, Tokens.SAVE, 'S', 'A', 'V', 'E',
        4, Tokens.LOAD, 'L', 'O', 'A', 'D',
        3, Tokens.DIR, 'D', 'I', 'R',
        3, Tokens.DEL, 'D', 'E', 'L',
        3, Tokens.MEM, 'M', 'E', 'M',
        3, Tokens.BYE, 'B', 'Y', 'E',
        3, Tokens.INT, 'I', 'N', 'T',
        4, Tokens.WORD, 'W', 'O', 'R', 'D',
        3, Tokens.BIT, 'B', 'I', 'T',
        5, Tokens.PRINT, 'P', 'R', 'I', 'N', 'T',
        2, Tokens.IF, 'I', 'F',
        4, Tokens.THEN, 'T', 'H', 'E', 'N',
        4, Tokens.FUNC, 'F', 'U', 'N', 'C',
        7, Tokens.ENDFUNC, 'E', 'N', 'D', 'F', 'U', 'N', 'C',
        6, Tokens.RETURN, 'R', 'E', 'T', 'U', 'R', 'N',
        5, Tokens.BEGIN, 'B', 'E', 'G', 'I', 'N',
        3, Tokens.END, 'E', 'N', 'D',
        0  // End marker
    };
    
    // Initialize tokenizer state
    Initialize()
    {
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        STZ ZP.TokenBufferLengthL
        STZ ZP.TokenBufferLengthH
        STZ ZP.BasicInputLength
        STZ ZP.CurrentToken
    }
    
    // Skip whitespace in input buffer at position X
    // Updates X to point to next non-whitespace character
    skipWhitespace()
    {
        loop
        {
            CPX ZP.BasicInputLength
            if (Z) { break; }  // End of input
            
            LDA Address.BasicInputBuffer, X
            CMP #' '
            if (Z)
            {
                INX
                continue;
            }
            CMP #'\t'
            if (Z)
            {
                INX
                continue;
            }
            break;  // Non-whitespace found
        }
    }
    
    // Check character type
    // Input: A = character
    // Output: A = 0=other, 1=digit, 2=alpha
    getCharType()
    {
        CMP #'0'
        if (C)
        {
            CMP #('9'+1)
            if (NC) { LDA #1 return; }  // digit
        }
        CMP #'A'
        if (C)
        {
            CMP #('Z'+1) 
            if (NC) { LDA #2 return; }  // alpha
        }
        CMP #'a'
        if (C)
        {
            CMP #('z'+1)
            if (NC) { LDA #2 return; }  // alpha  
        }
        LDA #0  // other
    }
    
    // Append byte to token buffer
    // Input: A = byte to append
    // Uses: ZP.TokenBufferLength (16-bit)
    appendToTokenBuffer()
    {
        PHA  // Save byte to append
        PHY  // Save Y
        
        // Check if buffer is full
        LDA ZP.TokenBufferLengthH
        if (NZ)  // High byte non-zero, definitely full
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            PLY
            PLA
            return;
        }
        
        LDY ZP.TokenBufferLengthL
        CPY #(Limits.BasicTokenizerBufferLength & 0xFF)
        if (Z)  // At max capacity
        {
            LDA #(Messages.SyntaxError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.SyntaxError / 256)
            STA ZP.LastErrorH
            PLY
            PLA
            return;
        }
        
        // Append the byte
        PLA  // Restore byte to append
        STA Address.BasicTokenizerBuffer, Y
        
        // Increment length
        INC ZP.TokenBufferLengthL
        if (Z)
        {
            INC ZP.TokenBufferLengthH
        }
        
        PLY  // Restore Y
    }
    
    // Find keyword match for current identifier in working buffer
    // Working buffer starts at Address.BasicProcessBuffer1, null-terminated
    // Returns token in A if found, or 0 if not found
    findKeyword()
    {
        LDY #0  // Start at beginning of keyword table
        loop
        {
            LDA keywords, Y    // Get length of this keyword
            if (Z) { break; }  // End of table - not found
            
            STA ZP.ACCL        // Save keyword length
            INY
            LDA keywords, Y    // Get token value
            STA ZP.ACCH        // Save token value
            INY
            
            // Compare characters
            LDX #0  // Character index in our identifier
            loop
            {
                LDA Address.BasicProcessBuffer1, X  // Get char from our identifier
                if (Z)  // Hit null terminator in our identifier
                {
                    // Check if we've matched the full keyword length
                    CPX ZP.ACCL
                    if (Z)
                    {
                        LDA ZP.ACCH  // Return token value - exact match!
                        return;
                    }
                    break; // Length mismatch
                }
                
                // Check if we've exceeded keyword length
                CPX ZP.ACCL
                if (Z) { break; }  // Our identifier is longer than keyword
                
                CMP keywords, Y  // Compare with expected character
                if (NZ) { break; } // Mismatch
                
                INX
                INY
            }
            
            // Mismatch - skip to next keyword
            loop
            {
                CPX ZP.ACCL       // Have we reached the end of keyword?
                if (Z) { break; } // Yes, Y now points to start of next keyword
                INX               // Move to next character position  
                INY               // Advance Y to next character
            }
        }
        
        LDA #0  // Not found
    }
    
    // Tokenize complete line from BasicInputBuffer into BasicTokenizerBuffer
    // Returns Z if successful, NZ if error (error stored in ZP.LastError)
    TokenizeLine()
    {
        // Clear token buffer
        STZ ZP.TokenBufferLengthL
        STZ ZP.TokenBufferLengthH
        Messages.ClearError();
        
        // Check for empty line
        LDA ZP.BasicInputLength
        if (Z)
        {
            // Empty line - add EOL token
            LDA #Tokens.EOL
            appendToTokenBuffer();
            return;
        }
        
        LDX #0  // Position in input buffer
        
        loop
        {
            skipWhitespace();  // Updates X
            CPX ZP.BasicInputLength
            if (Z) { break; }  // End of input
            
            LDA Address.BasicInputBuffer, X
            
            // Check for operators and punctuation
            switch (A)
            {
                case '=':
                {
                    LDA #Tokens.EQUALS
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NZ) { return; }
                    INX
                }
                case '+':
                {
                    LDA #Tokens.PLUS
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NZ) { return; }
                    INX
                }
                case '-':
                {
                    LDA #Tokens.MINUS
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NZ) { return; }
                    INX
                }
                case '(':
                {
                    LDA #Tokens.LPAREN
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NZ) { return; }
                    INX
                }
                case ')':
                {
                    LDA #Tokens.RPAREN
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NZ) { return; }
                    INX
                }
                case '<':
                {
                    // Check for <>
                    LDY #1
                    STY ZP.ACCL  // Temp storage for increment amount
                    INX
                    CPX ZP.BasicInputLength
                    if (NZ)
                    {
                        LDA Address.BasicInputBuffer, X
                        CMP #'>'
                        if (Z)
                        {
                            LDA #Tokens.NOTEQUAL
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NZ) { return; }
                            INX  // Skip both '<' and '>'
                            continue;
                        }
                    }
                    DEX  // Back up, single < not supported in Phase 1
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    return;
                }
                case '"':
                {
                    // String literal - for Phase 1, this is an error
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    return;
                }
                default:
                {
                    // Check if it's a number
                    getCharType(); 
                    CMP #1 // isDigit?
                    if (Z)  
                    {
                        // Scan number and store inline in token buffer
                        LDA #Tokens.NUMBER
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NZ) { return; }
                        
                        // Store number digits inline
                        loop
                        {
                            CPX ZP.BasicInputLength
                            if (Z) { break; }
                            
                            LDA Address.BasicInputBuffer, X
                            getCharType();
                            CMP #1 // isDigit?
                            if (NZ) { break; }  // Not a digit
                            
                            LDA Address.BasicInputBuffer, X
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NZ) { return; }
                            INX
                        }
                        
                        // Add null terminator for number
                        LDA #0
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NZ) { return; }
                        continue;
                    }
                    
                    // Must be an identifier or keyword
                    CMP #2 // isAlpha?
                    if (NZ)
                    {
                        // Invalid character
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        return;
                    }
                    
                    // Scan alphanumeric characters into working buffer for keyword lookup
                    LDY #0  // Index into working buffer
                    loop
                    {
                        CPX ZP.BasicInputLength
                        if (Z) { break; }
                        
                        LDA Address.BasicInputBuffer, X
                        getCharType();
                        if (Z) { break; }  // Not alphanumeric
                        
                        // Convert to uppercase and store in working buffer
                        LDA Address.BasicInputBuffer, X
                        CMP #'a'
                        if (C)             // >= 'a'
                        {
                            CMP #('z'+1)
                            if (NC)        // <= 'z'
                            {
                                SEC
                                SBC #('a'-'A')  // Convert to uppercase
                            }
                        }
                        STA Address.BasicProcessBuffer1, Y
                        
                        INX
                        INY
                        CPY #Limits.BasicProcessBuffer1Length
                        if (Z) 
                        { 
                            LDA #(Messages.SyntaxError % 256)
                            STA ZP.LastErrorL
                            LDA #(Messages.SyntaxError / 256)
                            STA ZP.LastErrorH
                            return;
                        }
                    }
                    
                    // Add null terminator to working buffer
                    LDA #0
                    STA Address.BasicProcessBuffer1, Y
                    
                    // Check if it's a keyword
                    findKeyword();
                    if (NZ)  // Found keyword
                    {
                        
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NZ) { return; }
                    }
                    else
                    {
                        // It's an identifier - store token + inline string
                        LDA #Tokens.IDENTIFIER
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NZ) { return; }
                        
                        // Copy identifier from working buffer to token buffer
                        LDY #0
                        loop
                        {
                            LDA Address.BasicProcessBuffer1, Y
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NZ) { return; }
                            if (Z) { break; }  // Copied null terminator
                            INY
                        }
                    }
                }
            }
        }
        
        // Add EOL token
        LDA #Tokens.EOL
        appendToTokenBuffer();
        Messages.CheckError();
        if (NZ) { return; }
        
        // Reset tokenizer position to start of buffer
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        LDA #0  // Set Z for success
    }
    
    // Get next token from BasicTokenizerBuffer
    // Returns token type in A, updates ZP.CurrentToken
    // For literals, advances past the inline data
    NextToken()
    {
        // Check if we're at end of token buffer
        LDA ZP.TokenizerPosL
        CMP ZP.TokenBufferLengthL
        if (NZ)
        {
            // Not at end, get token
            LDY ZP.TokenizerPosL
            LDA Address.BasicTokenizerBuffer, Y
            STA ZP.CurrentToken
            
            // Advance past token
            INC ZP.TokenizerPosL
            if (Z)
            {
                INC ZP.TokenizerPosH
            }
            
            // If it's a literal token, skip past the inline data
            LDA ZP.CurrentToken
            CMP #Tokens.NUMBER
            if (Z)
            {
                skipInlineString();
                LDA ZP.CurrentToken
                return;
            }
            CMP #Tokens.IDENTIFIER
            if (Z)
            {
                skipInlineString();
                LDA ZP.CurrentToken
                return;
            }
            CMP #Tokens.STRING
            if (Z)
            {
                skipInlineString();
                LDA ZP.CurrentToken
                return;
            }
            
            LDA ZP.CurrentToken
            return;
        }
        
        LDA ZP.TokenizerPosH
        CMP ZP.TokenBufferLengthH
        if (Z)
        {
            // At end of buffer
            LDA #Tokens.EOF
            STA ZP.CurrentToken
            return;
        }
        
        // Past end (shouldn't happen)
        LDA #Tokens.EOF
        STA ZP.CurrentToken
    }
    
    // Skip past null-terminated string at current tokenizer position
    skipInlineString()
    {
        loop
        {
            // Check bounds
            LDA ZP.TokenizerPosL
            CMP ZP.TokenBufferLengthL
            if (NZ)
            {
                LDY ZP.TokenizerPosL
                LDA Address.BasicTokenizerBuffer, Y
                
                // Advance position
                INC ZP.TokenizerPosL
                if (Z)
                {
                    INC ZP.TokenizerPosH
                }
                
                if (Z) { break; }  // Found null terminator
                continue;
            }
            
            LDA ZP.TokenizerPosH  
            CMP ZP.TokenBufferLengthH
            if (Z) { break; }  // At end
            
            // Past end - shouldn't happen
            break;
        }
    }
    
    // Get current token as 16-bit number (assumes current token is NUMBER)
    // Inline number string follows the NUMBER token in buffer
    // Returns 16-bit number in ZP.TOP
    GetTokenNumber()
    {
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        // Current position should be pointing at the start of number digits
        LDY ZP.TokenizerPosL  // We've already consumed the NUMBER token
        
        loop
        {
            // Check bounds
            CPY ZP.TokenBufferLengthL
            if (Z)  // At or past end
            {
                LDA ZP.TokenizerPosH
                CMP ZP.TokenBufferLengthH
                if (Z) { break; }  // Definitely at end
            }
            
            LDA Address.BasicTokenizerBuffer, Y
            if (Z) { break; }  // Hit null terminator
            
            // TOP = TOP * 10 using shifts and adds
            // Save current value in NEXT
            LDA ZP.TOPL
            STA ZP.NEXTL
            LDA ZP.TOPH
            STA ZP.NEXTH
            
            // TOP = TOP * 2
            ASL ZP.TOPL
            ROL ZP.TOPH
            // TOP = TOP * 2 (now *4)
            ASL ZP.TOPL
            ROL ZP.TOPH
            
            // TOP = TOP + NEXT (4 + 1 = 5)
            CLC
            LDA ZP.TOPL
            ADC ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.TOPH
            ADC ZP.NEXTH
            STA ZP.TOPH
            
            // TOP = TOP * 2 (5 * 2 = 10)
            ASL ZP.TOPL
            ROL ZP.TOPH
            
            // Add current digit
            LDA Address.BasicTokenizerBuffer, Y
            SEC
            SBC #'0'
            CLC
            ADC ZP.TOPL
            STA ZP.TOPL
            if (C)
            {
                INC ZP.TOPH
            }
            
            INY
        }
    }
    
    // Read a line of input into BasicInputBuffer
    // Returns length in A, sets ZP.BasicInputLength
    ReadLine()
    {
        LDX #0  // Buffer position
        
        loop
        {
            Serial.WaitForChar();  // Returns character in A
            
            switch (A)
            {
                case '\r':
                case '\n':
                {
                    // End of line
                    LDA #'\n'
                    Serial.WriteChar();  // Echo newline
                    break;
                }
                case 0x08:  // Backspace
                case 0x7F:  // Delete
                {
                    CPX #0
                    if (Z) { continue; }  // Nothing to delete
                    
                    DEX
                    LDA #0x08   // Backspace
                    Serial.WriteChar();
                    LDA #' '    // Space
                    Serial.WriteChar();
                    LDA #0x08   // Backspace
                    Serial.WriteChar();
                    continue;
                }
                case 0x03:  // Ctrl+C
                {
                    LDA #'^'
                    Serial.WriteChar();
                    LDA #'C'
                    Serial.WriteChar();
                    LDA #'\n'
                    Serial.WriteChar();
                    LDX #0  // Clear buffer
                    break;
                }
                default:
                {
                    // Check for printable character
                    CMP #' '
                    if (C)  // >= 32
                    {
                        CMP #0x7F
                        if (NC)  // < 127
                        {
                            CPX #Limits.BasicInputLength
                            if (Z) { continue; }  // Buffer full
                            
                            STA Address.BasicInputBuffer, X
                            INX
                            Serial.WriteChar();  // Echo
                            continue;
                        }
                    }
                    continue;  // Ignore non-printable
                }
            }
            break;  // End of input
        }
        
        STX ZP.BasicInputLength
        TXA  // Return length
    }
}
