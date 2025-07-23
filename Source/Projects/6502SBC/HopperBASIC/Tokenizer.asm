unit Tokenizer
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "Limits"
    
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
        STZ ZP.TokenizerPos
        STZ ZP.BasicInputLength
        STZ ZP.CurrentToken
    }
    
    // Skip whitespace in input buffer
    skipWhitespace()
    {
        loop
        {
            LDX ZP.TokenizerPos
            CPX ZP.BasicInputLength
            if (Z) { break; }  // End of input
            
            LDA Address.BasicInputBuffer, X
            CMP #' '
            if (Z)
            {
                INC ZP.TokenizerPos
                continue;
            }
            CMP #'\t'
            if (Z)
            {
                INC ZP.TokenizerPos
                continue;
            }
            break;  // Non-whitespace found
        }
    }
    
    // Check if character is alphabetic
    isAlpha()
    {
        // A contains character to test
        // Returns Z=0 if alphabetic, Z=1 if not
        CMP #'A'
        if (C)  // >= 'A'
        {
            CMP #('Z'+1)
            if (NC)  // <= 'Z'
            {
                LDA #1  // Set NZ (is alphabetic)
                return;
            }
        }
        CMP #'a'
        if (C)  // >= 'a'
        {
            CMP #('z'+1)
            if (NC)  // <= 'z'
            {
                LDA #1  // Set NZ (is alphabetic)
                return;
            }
        }
        LDA #0  // Set Z (not alphabetic)
    }
    
    // Check if character is numeric
    isDigit()
    {
        // A contains character to test
        // Returns Z=0 if digit, Z=1 if not
        CMP #'0'
        if (C)  // >= '0'
        {
            CMP #('9'+1)
            if (NC)  // <= '9'
            {
                LDA #1  // Set NZ (is digit)
                return;
            }
        }
        LDA #0  // Set Z (not digit)
    }
    
    // Check if character is alphanumeric
    isAlphaNum()
    {
        // A contains character to test
        // Returns Z=0 if alphanumeric, Z=1 if not
        isAlpha();
        if (NZ) { return; }  // Is alphabetic
        isDigit();           // Check if digit
    }
    
    // Find keyword match
    // Token stored in BasicTokenizerBuffer, null-terminated
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
            LDX #0  // Character index in our token
            loop
            {
                LDA Address.BasicTokenizerBuffer, X  // Get char from our token
                if (Z)  // Hit null terminator in our token
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
                if (Z) { break; }  // Our token is longer than keyword
                
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
    
    // Get next token from input buffer
    // Returns token type in A, updates ZP.CurrentToken
    NextToken()
    {
        skipWhitespace();
        
        LDX ZP.TokenizerPos
        CPX ZP.BasicInputLength
        if (Z)
        {
            LDA #Tokens.EOL
            STA ZP.CurrentToken
            return;
        }
        
        LDA Address.BasicInputBuffer, X
        
        // Check for operators and punctuation
        switch (A)
        {
            case '=':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.EQUALS
                STA ZP.CurrentToken
                return;
            }
            case '+':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.PLUS
                STA ZP.CurrentToken
                return;
            }
            case '-':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.MINUS
                STA ZP.CurrentToken
                return;
            }
            case '(':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.LPAREN
                STA ZP.CurrentToken
                return;
            }
            case ')':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.RPAREN
                STA ZP.CurrentToken
                return;
            }
            case '<':
            {
                // Check for <>
                LDY ZP.TokenizerPos
                INY
                CPY ZP.BasicInputLength
                if (NZ)
                {
                    LDA Address.BasicInputBuffer, Y
                    CMP #'>'
                    if (Z)
                    {
                        INC ZP.TokenizerPos
                        INC ZP.TokenizerPos
                        LDA #Tokens.NOTEQUAL
                        STA ZP.CurrentToken
                        return;
                    }
                }
                // Single < not supported in Phase 1
                LDA #0
                STA ZP.CurrentToken
                return;
            }
            case '"':
            {
                // String literal - copy to BasicTokenizerBuffer
                INC ZP.TokenizerPos  // Skip opening quote
                
                LDX #0  // Index into BasicTokenizerBuffer
                loop
                {
                    LDY ZP.TokenizerPos
                    CPY ZP.BasicInputLength
                    if (Z)  
                    {
                        // End of input without closing quote
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        break;
                    }
                    
                    LDA Address.BasicInputBuffer, Y
                    INC ZP.TokenizerPos
                    CMP #'"'
                    if (Z)  // Found closing quote
                    {
                        break;
                    }
                    
                    STA Address.BasicTokenizerBuffer, X
                    INX
                    CPX # Limits.BasicTokenizerBufferLength
                    if (Z) 
                    {
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        break; // Buffer full
                    }
                }
                
                // Add null terminator
                LDA #0
                STA Address.BasicTokenizerBuffer, X
                
                LDA #Tokens.STRING
                STA ZP.CurrentToken
                return;
            }
        }
        
        // Check if it's a number
        isDigit();
        if (NZ)  // Is a digit
        {
            // Scan number and copy to BasicTokenizerBuffer
            LDX #0  // Index into BasicTokenizerBuffer
            loop
            {
                LDY ZP.TokenizerPos
                CPY ZP.BasicInputLength
                if (Z) { break; }
                
                LDA Address.BasicInputBuffer, Y
                isDigit();
                if (Z) { break; }  // Not a digit
                
                STA Address.BasicTokenizerBuffer, X
                INC ZP.TokenizerPos
                INX
                CPX # Limits.BasicTokenizerBufferLength
                if (Z) { break; }  // Buffer full
            }
            
            // Add null terminator
            LDA #0
            STA Address.BasicTokenizerBuffer, X
            
            LDA #Tokens.NUMBER
            STA ZP.CurrentToken
            return;
        }
        
        // Must be an identifier or keyword
        isAlpha();
        if (Z)
        {
            // Invalid character
            LDA #0
            STA ZP.CurrentToken
            return;
        }
        
        // Scan alphanumeric characters and copy to BasicTokenizerBuffer with uppercase conversion
        LDX #0  // Index into BasicTokenizerBuffer
        loop
        {
            LDY ZP.TokenizerPos
            CPY ZP.BasicInputLength
            if (Z) { break; }
            
            LDA Address.BasicInputBuffer, Y
            isAlphaNum();
            if (Z) { break; }  // Not alphanumeric
            
            // Convert to uppercase and store in BasicTokenizerBuffer
            LDA Address.BasicInputBuffer, Y
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
            STA Address.BasicTokenizerBuffer, X
            
            INC ZP.TokenizerPos
            INX
            CPX # Limits.BasicTokenizerBufferLength
            if (Z) { break; }  // Buffer full
        }
        
        // Add null terminator
        LDA #0
        STA Address.BasicTokenizerBuffer, X
        
        // Check if it's a keyword
        findKeyword();
        
        if (NZ)  // Found keyword
        {
            STA ZP.CurrentToken
            return;
        }
        
        // It's an identifier
        LDA #Tokens.IDENTIFIER
        STA ZP.CurrentToken
    }
    
    // Get current token as 16-bit number (assumes current token is NUMBER)
    // Token string is in BasicTokenizerBuffer
    // Returns 16-bit number in ZP.TOP
    GetTokenNumber()
    {
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        LDX #0
        
        loop
        {
            LDA Address.BasicTokenizerBuffer, X
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
            LDA Address.BasicTokenizerBuffer, X
            SEC
            SBC #'0'
            CLC
            ADC ZP.TOPL
            STA ZP.TOPL
            if (C)
            {
                INC ZP.TOPH
            }
            
            INX
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
        STZ ZP.TokenizerPos
        TXA  // Return length
    }
}
