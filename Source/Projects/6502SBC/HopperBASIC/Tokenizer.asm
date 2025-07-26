unit Tokenizer
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "Limits"
    uses "Messages"
    uses "BasicTypes"
    
    // Phase 1 Token definitions (extended set)
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
        
        // Logical keywords (48-55)
        AND      = 0x30,
        OR       = 0x31,
        NOT      = 0x32,
        MOD      = 0x33,
        
        // Basic operators (64-79)
        EQUALS   = 0x40,  // =
        PLUS     = 0x41,  // +
        MINUS    = 0x42,  // -
        LPAREN   = 0x43,  // (
        RPAREN   = 0x44,  // )
        NOTEQUAL = 0x45,  // <>
        
        // Additional comparison operators (80-87)
        LT       = 0x50,  // <
        GT       = 0x51,  // >
        LE       = 0x52,  // <=
        GE       = 0x53,  // >=
        
        // Arithmetic operators (88-95)
        MULTIPLY = 0x58,  // *
        DIVIDE   = 0x59,  // /
        
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
        3, Tokens.AND, 'A', 'N', 'D',
        2, Tokens.OR, 'O', 'R',
        3, Tokens.NOT, 'N', 'O', 'T',
        3, Tokens.MOD, 'M', 'O', 'D',
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
        STZ ZP.TokenLiteralPosL
        STZ ZP.TokenLiteralPosH
    }
    
    // Helper: Set IDX = BasicTokenizerBuffer + TokenizerPos
    setTokenizerPointer()
    {
        CLC
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        ADC ZP.TokenizerPosL
        STA ZP.IDXL
        LDA #(Address.BasicTokenizerBuffer >> 8)
        ADC ZP.TokenizerPosH
        STA ZP.IDXH
    }
    
    // Helper: Set IDX = BasicTokenizerBuffer + TokenBufferLength
    setTokenBufferEndPointer()
    {
        CLC
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        ADC ZP.TokenBufferLengthL
        STA ZP.IDXL
        LDA #(Address.BasicTokenizerBuffer >> 8)
        ADC ZP.TokenBufferLengthH
        STA ZP.IDXH
    }
    
    // Helper: Increment 16-bit TokenizerPos
    incrementTokenizerPos()
    {
        INC ZP.TokenizerPosL
        if (Z)
        {
            INC ZP.TokenizerPosH
        }
    }
    
    // Helper: Increment 16-bit TokenBufferLength
    incrementTokenBufferLength()
    {
        INC ZP.TokenBufferLengthL
        if (Z)
        {
            INC ZP.TokenBufferLengthH
        }
    }
    
    // Helper: Compare 16-bit values - TokenizerPos vs TokenBufferLength
    // Returns: Z set if equal, C set if TokenizerPos >= TokenBufferLength
    compareTokenizerPosToLength()
    {
        LDA ZP.TokenizerPosH
        CMP ZP.TokenBufferLengthH
        if (NZ) { return; }  // Not equal, C flag is correct
        
        // High bytes equal, compare low bytes
        LDA ZP.TokenizerPosL
        CMP ZP.TokenBufferLengthL
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
    
    // Append byte to token buffer using 16-bit addressing
    // Input: A = byte to append
    // Uses: ZP.TokenBufferLength (16-bit), ZP.IDX for addressing
    appendToTokenBuffer()
    {
        PHA  // Save byte to append
        
        // 16-bit boundary check: if (TokenBufferLength >= 512) return error
        LDA ZP.TokenBufferLengthH
        CMP #(Limits.BasicTokenizerBufferLength >> 8)  // Compare high byte (2)
        if (C)  // >= 2
        {
            if (NZ)  // > 2, definitely full
            {
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                PLA
                return;
            }
            // High byte = 2, check low byte
            LDA ZP.TokenBufferLengthL
            CMP #(Limits.BasicTokenizerBufferLength & 0xFF)  // Compare low byte (0)
            if (C)  // >= 512, buffer full
            {
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                PLA
                return;
            }
        }
        
        // Set up 16-bit pointer to end of buffer
        setTokenBufferEndPointer();
        
        // Store byte using indirect addressing
        PLA  // Get byte to store
        LDY #0
        STA [ZP.IDX], Y
        
        // Increment 16-bit length
        incrementTokenBufferLength();
    }
    
    // Find keyword match for current identifier in working buffer
    // Working buffer starts at Address.BasicProcessBuffer1, null-terminated
    // Returns token in A if found, or 0 if not found
    findKeyword()
    {
        PHX
        PHY
        
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
                        PLY
                        PLX
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
        
        PLY
        PLX
        
        LDA #0  // Not found
    }
    
    // Tokenize complete line from BasicInputBuffer into BasicTokenizerBuffer
    // Returns C if successful, NC if error (error stored in ZP.LastError)
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
            SEC  // Success
            return;
        }
        
        LDX #0  // Position in input buffer
        
        loop
        {
            skipWhitespace();  // Updates X
            CPX ZP.BasicInputLength
            if (Z) { break; }  // End of input
            
            // Check for operators and punctuation
            LDA Address.BasicInputBuffer, X
            switch (A)
            {
                case '=':
                {
                    LDA #Tokens.EQUALS
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '+':
                {
                    LDA #Tokens.PLUS
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '-':
                {
                    LDA #Tokens.MINUS
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '*':
                {
                    LDA #Tokens.MULTIPLY
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '/':
                {
                    LDA #Tokens.DIVIDE
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '(':
                {
                    LDA #Tokens.LPAREN
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case ')':
                {
                    LDA #Tokens.RPAREN
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '<':
                {
                    // Check for <= or <>
                    INX
                    CPX ZP.BasicInputLength
                    if (NZ)
                    {
                        LDA Address.BasicInputBuffer, X
                        CMP #'='
                        if (Z)
                        {
                            LDA #Tokens.LE
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) { return; }
                            INX  // Skip both '<' and '='
                            continue;
                        }
                        CMP #'>'
                        if (Z)
                        {
                            LDA #Tokens.NOTEQUAL
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) { return; }
                            INX  // Skip both '<' and '>'
                            continue;
                        }
                    }
                    // Just '<'
                    DEX  // Back up to point at '<'
                    LDA #Tokens.LT
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX  // Move past '<'
                }
                case '>':
                {
                    // Check for >=
                    INX
                    CPX ZP.BasicInputLength
                    if (NZ)
                    {
                        LDA Address.BasicInputBuffer, X
                        CMP #'='
                        if (Z)
                        {
                            LDA #Tokens.GE
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) { return; }
                            INX  // Skip both '>' and '='
                            continue;
                        }
                    }
                    // Just '>'
                    DEX  // Back up to point at '>'
                    LDA #Tokens.GT
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX  // Move past '>'
                }
                case '"':
                {
                    // String literal - for Phase 1, this is an error
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    CLC  // Error
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
                        if (NC) { return; }
                        
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
                            if (NC) { return; }
                            INX
                        }
                        
                        // Add null terminator for number
                        LDA #0
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NC) { return; }
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
                        CLC  // Error
                        return;
                    }
                    
                    // Scan alphanumeric characters into working buffer for keyword lookup
                    LDY #0  // Index into working buffer (FIXED: was using X)
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
                        STA Address.BasicProcessBuffer1, Y  // FIXED: Use Y for working buffer
                        
                        INX  // Advance input position
                        INY  // FIXED: Advance working buffer index separately
                        CPY #Limits.BasicProcessBuffer1Length
                        if (Z) 
                        { 
                            LDA #(Messages.SyntaxError % 256)
                            STA ZP.LastErrorL
                            LDA #(Messages.SyntaxError / 256)
                            STA ZP.LastErrorH
                            CLC  // Error
                            return;
                        }
                    }
                    
                    // Add null terminator to working buffer
                    LDA #0
                    STA Address.BasicProcessBuffer1, Y  // FIXED: Use Y for working buffer
                    
                    // Check if it's a keyword
                    findKeyword();
                    if (NZ)  // Found keyword
                    {
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NC) { return; }
                    }
                    else
                    {
                        // It's an identifier - store token + inline string
                        LDA #Tokens.IDENTIFIER
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NC) { return; }
                        
                        // Copy identifier from working buffer to token buffer
                        LDY #0  // Reset Y for copying
                        loop
                        {
                            LDA Address.BasicProcessBuffer1, Y
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) { return; }
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
        if (NC) { return; }
        
        // Reset tokenizer position to start of buffer
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        
        SEC  // Success
    }
    
    // Get next token from BasicTokenizerBuffer using 16-bit addressing
    // Returns token type in A, updates ZP.CurrentToken
    // For literals, advances past the inline data
    NextToken()
    {
        // 16-bit comparison: if (TokenizerPos >= TokenBufferLength)
        compareTokenizerPosToLength();
        if (C)  // TokenizerPos >= TokenBufferLength
        {
            LDA #Tokens.EOF
            STA ZP.CurrentToken
            return;
        }
        
        // Set up 16-bit pointer to current position
        setTokenizerPointer();
        
        // Get token using indirect addressing
        LDY #0
        LDA [ZP.IDX], Y
        STA ZP.CurrentToken
        
        // Advance past token
        incrementTokenizerPos();
        
        // If it's a literal token, save position and skip past the inline data
        LDA ZP.CurrentToken
        CMP #Tokens.NUMBER
        if (Z)
        {
            // Save current position as start of literal data
            LDA ZP.TokenizerPosL
            STA ZP.TokenLiteralPosL
            LDA ZP.TokenizerPosH
            STA ZP.TokenLiteralPosH
            
            skipInlineString();
            LDA ZP.CurrentToken
            return;
        }
        CMP #Tokens.IDENTIFIER
        if (Z)
        {
            // Save current position as start of literal data
            LDA ZP.TokenizerPosL
            STA ZP.TokenLiteralPosL
            LDA ZP.TokenizerPosH
            STA ZP.TokenLiteralPosH
            
            skipInlineString();
            LDA ZP.CurrentToken
            return;
        }
        CMP #Tokens.STRING
        if (Z)
        {
            // Save current position as start of literal data
            LDA ZP.TokenizerPosL
            STA ZP.TokenLiteralPosL
            LDA ZP.TokenizerPosH
            STA ZP.TokenLiteralPosH
            
            skipInlineString();
            LDA ZP.CurrentToken
            return;
        }
        
        LDA ZP.CurrentToken
    }
    
    // Helper function to check if multiplying ZP.TOP by 10 and adding a digit would overflow
    // Input: ZP.TOP contains current 16-bit value, A contains digit to add (0-9)
    // Output: C set if operation is safe, NC set if would overflow
    // Preserves: A (digit), ZP.TOP unchanged
    // Uses: ZP.NEXT for temporary calculations
    checkMultiply10PlusDigitOverflow()
    {
        PHA  // Save digit
        
        // Check if TOP > 6553 (65535 / 10 = 6553.5, so 6554+ will overflow)
        LDA ZP.TOPH
        CMP # 0x19  // 6553 = 0x1999, high byte = 0x19
        if (C)   // >= 0x19
        {
            if (NZ)  // > 0x19, definitely overflow
            {
                PLA  // Restore digit
                CLC  // Overflow
                return;
            }
            
            // High byte = 0x19, check low byte  
            LDA ZP.TOPL
            CMP # 0x99  // 6553 = 0x1999, low byte = 0x99
            if (C)    // >= 0x99
            {
                // TOP >= 6553, check if exactly 6553
                if (NZ)  // > 6553, overflow
                {
                    PLA  // Restore digit
                    CLC  // Overflow
                    return;
                }
                
                // TOP = 6553, check if digit > 5 (6553*10 + 6 = 65536)
                PLA  // Get digit back
                CMP #6
                if (C)  // digit >= 6, would overflow
                {
                    CLC  // Overflow
                    return;
                }
                
                SEC  // Safe
                return;
            }
        }
        
        // TOP < 6553, always safe regardless of digit
        PLA  // Restore digit
        SEC  // Safe
    }
    
    // Get current token as 16-bit number (assumes current token is NUMBER)
    // Inline number string follows the NUMBER token in buffer
    // Returns 16-bit number in ZP.TOP, type in ZP.TOPT
    // Sets error if number overflows 16-bit range
    GetTokenNumber()
    {
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        // Set up 16-bit pointer to saved literal position in token buffer
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        CLC
        ADC ZP.TokenLiteralPosL
        STA ZP.IDXL
        LDA #(Address.BasicTokenizerBuffer >> 8)
        ADC ZP.TokenLiteralPosH
        STA ZP.IDXH
        
        LDY #0  // Index into the number string
        
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) { break; }  // Hit null terminator
            
            // Check if character is a digit
            LDA [ZP.IDX], Y
            getCharType();
            CMP # 1
            if (NZ) // not digit
            {
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                return;
            }
            
            // Convert to digit value
            LDA [ZP.IDX], Y
            SEC
            SBC #'0'
            STA ZP.ACCL  // Store digit temporarily
            
            // Check for overflow before doing the math
            checkMultiply10PlusDigitOverflow();
            if (NC)  // Would overflow
            {
                LDA #( Messages.NumericOverflow % 256)
                STA ZP.LastErrorL
                LDA #( Messages.NumericOverflow / 256)
                STA ZP.LastErrorH
                return;
            }
            
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
            
            //TOP = TOP + NEXT (4 + 1 = 5)
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
            LDA ZP.ACCL
            CLC
            ADC ZP.TOPL
            STA ZP.TOPL
            if (C)
            {
                INC ZP.TOPH
            }
            
            INY
        }
        
        // Set the type based on the value
        BIT ZP.TOPH          // Check high bit
        if (MI)
        {
            LDA #BasicType.WORD   // Large positive (32768-65535)
            STA ZP.TOPT
        }
        else
        {
            LDA #BasicType.INT    // Medium positive (256-32767)
            STA ZP.TOPT       
        }
    }
    
    // Skip past null-terminated string at current tokenizer position
    skipInlineString()
    {
        loop
        {
            // Check if we're at or past end of token buffer
            LDA ZP.TokenizerPosL
            CMP ZP.TokenBufferLengthL
            if (NZ)
            {
                // Not at end - check the byte at current position
                LDA #(Address.BasicTokenizerBuffer & 0xFF)
                CLC
                ADC ZP.TokenizerPosL
                STA ZP.IDXL
                LDA #(Address.BasicTokenizerBuffer >> 8)
                ADC ZP.TokenizerPosH
                STA ZP.IDXH
                
                LDY #0
                LDA [ZP.IDX], Y
                PHA  // Save the character we just read
                
                // Advance position
                INC ZP.TokenizerPosL
                if (Z)
                {
                    INC ZP.TokenizerPosH
                }
                
                PLA  // Restore the character
                if (Z) { break; }  // Found null terminator
                continue;
            }
            
            // Check high byte
            LDA ZP.TokenizerPosH  
            CMP ZP.TokenBufferLengthH
            if (Z) { break; }  // At end
            
            // Past end - shouldn't happen
            break;
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
    
    // Get pointer to current token's string (for IDENTIFIER, NUMBER, STRING tokens)
    // Assumes current token is a literal token with inline data
    // Returns pointer in ZP.TOP
    GetTokenString()
    {
        // Set up pointer to saved literal position in token buffer
        CLC
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        ADC ZP.TokenLiteralPosL
        STA ZP.TOPL
        LDA #(Address.BasicTokenizerBuffer >> 8)
        ADC ZP.TokenLiteralPosH
        STA ZP.TOPH
    }
    
}
