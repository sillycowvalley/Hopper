unit Tokenizer
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "/Source/Runtime/6502/Stacks"
    
    friend Interpreter, Tokenizer, BytecodeCompiler;
    
    // Token definitions
    enum Tokens
    {
        // Immediate commands (0-15)
        NEW      = 0x01,
        LIST     = 0x02,
        RUN      = 0x03,
        CLEAR    = 0x04,
        SAVE     = 0x05,
        LOAD     = 0x06,
        DIR      = 0x07,
        DEL      = 0x08,
        VARS     = 0x09,
        CONSTS   = 0x0A,
        FUNCS    = 0x0B,
        BYE      = 0x0C,
        EOL      = 0x0D, // can be empty line in REPL
        
        // Language tokens (16+)
        LET      = 0x10,
        PRINT    = 0x11,
        IF       = 0x12,
        ELSE     = 0x13,
        ENDIF    = 0x14,
        FOR      = 0x15,
        NEXT     = 0x16,
        WHILE    = 0x17,
        ENDWHILE = 0x18,
        FUNC     = 0x19,
        ENDFUNC  = 0x1A,
        RETURN   = 0x1B,
        BEGIN    = 0x1C,
        END      = 0x1D,
        CONST    = 0x1E,
        
        // Type tokens
        IntType    = 0x20,
        WordType   = 0x21,
        ByteType   = 0x22,
        BitType    = 0x23,
        StringType = 0x24,
        
        // Special tokens
        NUMBER     = 0x80,
        STRING     = 0x81,
        IDENTIFIER = 0x82,
        EOF        = 0x84,
        
        // Operators  
        EQUALS   = 0x90,
        PLUS     = 0x91,
        MINUS    = 0x92,
        MULTIPLY = 0x93,
        DIVIDE   = 0x94,
        LPAREN   = 0x95,
        RPAREN   = 0x96,
        LBRACKET = 0x97,
        RBRACKET = 0x98,
        COMMA    = 0x99,
    }
    
    // Keyword table - each entry is: length, token_value, characters...
    // Stored as: len1, tok1, char1, char2, ..., len2, tok2, char1, char2, ...
    const byte[] keywords = {
        3, Tokens.NEW, 'N', 'E', 'W',
        4, Tokens.LIST, 'L', 'I', 'S', 'T', 
        3, Tokens.RUN, 'R', 'U', 'N',
        5, Tokens.CLEAR, 'C', 'L', 'E', 'A', 'R',
        4, Tokens.SAVE, 'S', 'A', 'V', 'E',
        4, Tokens.LOAD, 'L', 'O', 'A', 'D',
        3, Tokens.DIR, 'D', 'I', 'R',
        3, Tokens.DEL, 'D', 'E', 'L',
        4, Tokens.VARS, 'V', 'A', 'R', 'S',
        6, Tokens.CONSTS, 'C', 'O', 'N', 'S', 'T', 'S',
        5, Tokens.FUNCS, 'F', 'U', 'N', 'C', 'S',
        3, Tokens.BYE, 'B', 'Y', 'E',
        3, Tokens.LET, 'L', 'E', 'T',
        5, Tokens.PRINT, 'P', 'R', 'I', 'N', 'T',
        2, Tokens.IF, 'I', 'F',
        4, Tokens.ELSE, 'E', 'L', 'S', 'E',
        5, Tokens.ENDIF, 'E', 'N', 'D', 'I', 'F',
        3, Tokens.FOR, 'F', 'O', 'R',
        4, Tokens.NEXT, 'N', 'E', 'X', 'T',
        5, Tokens.WHILE, 'W', 'H', 'I', 'L', 'E',
        8, Tokens.ENDWHILE, 'E', 'N', 'D', 'W', 'H', 'I', 'L', 'E',
        4, Tokens.FUNC, 'F', 'U', 'N', 'C',
        7, Tokens.ENDFUNC, 'E', 'N', 'D', 'F', 'U', 'N', 'C',
        6, Tokens.RETURN, 'R', 'E', 'T', 'U', 'R', 'N',
        5, Tokens.BEGIN, 'B', 'E', 'G', 'I', 'N',
        3, Tokens.END, 'E', 'N', 'D',
        5, Tokens.CONST, 'C', 'O', 'N', 'S', 'T',
        3, Tokens.IntType, 'I', 'N', 'T',
        4, Tokens.WordType, 'W', 'O', 'R', 'D',
        4, Tokens.ByteType, 'B', 'Y', 'T', 'E',
        3, Tokens.BitType, 'B', 'I', 'T',
        6, Tokens.StringType, 'S', 'T', 'R', 'I', 'N', 'G',
        0  // End marker
    };
    
    // Convert character to uppercase (destructive)
    makeUppercase()
    {
        // A contains character
        CMP #'a'
        if (C)  // >= 'a'
        {
            CMP #('z'+1)
            if (NC)  // <= 'z'  
            {
                SBC #('a'-'A'-1)  // Convert to uppercase (carry is set)
            }
        }
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
    
    // Check if character is alphanumeric
    isAlphaNum()
    {
        // A contains character to test
        // Returns Z=0 if alphanumeric, Z=1 if not 
        CMP #'0'
        if (C)  // >= '0'
        {
            CMP #('9'+1)
            if (NC)  // <= '9'
            {
                LDA #1  // Set Z=0 (is alphanumeric)
                return;
            }
        }
        CMP #'A'
        if (C)  // >= 'A'
        {
            CMP #('Z'+1)
            if (NC)  // <= 'Z'
            {
                LDA #1  // Set Z=0 (is alphanumeric)
                return;
            }
        }
        CMP #'a'
        if (C)  // >= 'a'
        {
            CMP #('z'+1)
            if (NC)  // <= 'z'
            {
                LDA #1  // Set Z=0 (is alphanumeric)
                return;
            }
        }
        LDA #0  // Set Z=1 (not alphanumeric)
    }
    
    // Find keyword match
    // Returns token in A if found, or 0 if not found
    findKeyword()
    {
        LDY #0  // Initialize Y to start at beginning of keyword table
        loop
        {
            LDA keywords, Y    // Get length of this keyword
            if (Z) { break; }  // End of table
            
            CMP ZP.TokenLen
            if (Z)  // Length matches
            {
                // Compare characters
                INY
                LDA keywords, Y  // Get token value
                STA ZP.ACCL      // Save token value
                INY
                
                LDX #0  // Character index
                loop
                {
                    CPX ZP.TokenLen
                    if (Z)  // All characters matched
                    {
                        LDA ZP.ACCL  // Return token value
                        return;
                    }
                    
                    LDA keywords, Y  // Expected character from table
                    STA ZP.ACCH      
                    
                    // Get actual character from input buffer
                    PHY              // Save Y (keywords table index)
                    TXA              // Character index
                    CLC
                    ADC ZP.TokenStart // Add token start position  
                    TAY              // Use Y for INPUT_BUFFER index
                    LDA Address.BasicInputBuffer, Y
                    makeUppercase();
                    PLY              // Restore Y (keywords table index) FIRST
                    
                    CMP ZP.ACCH      // Compare with expected AFTER restoring Y
                    if (NZ)          // Mismatch
                    {
                        break;
                    }
                    
                    INX              // Next character
                    INY              // Next expected character  
                }
                
                // If we get here, there was a mismatch  
                // X = current character position, Y = current character in keyword table
                // Skip to end of this keyword: advance Y by (tokenLen - X - 1) positions
                loop
                {
                    CPX ZP.TokenLen       // Have we reached the end?
                    if (Z) { break; }     // Yes, Y now points to start of next keyword
                    INX                   // Move to next character position
                    INY                   // Advance Y to next character
                }            
            }
            else
            {
                // Skip this keyword (length doesn't match):
                
                // A contains the keyword length, Y points to the length byte
                INY          // Skip past length byte to token byte
                INY          // Skip past token byte to first character byte
                loop
                {
                    if (Z) { break; }    // When A reaches 0, we've skipped all character bytes
                    INY          // Skip past one character byte  
                    DEC          // Decrement remaining character count (A--)
                }
                // Y now points to the length byte of the next keyword            
            }
        }
        
        LDA #0  // Not found
    }
    
    // Get next token from input buffer
    // Returns token type in A, updates currentTok
    nextToken()
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
        
        // Mark start of token
        STX ZP.TokenStart
        STZ ZP.TokenLen
        
        LDA Address.BasicInputBuffer, X
        
        // Check for single character tokens
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
            case '*':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.MULTIPLY
                STA ZP.CurrentToken
                return;
            }
            case '/':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.DIVIDE
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
            case '[':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.LBRACKET
                STA ZP.CurrentToken
                return;
            }
            case ']':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.RBRACKET
                STA ZP.CurrentToken
                return;
            }
            case ',':
            {
                INC ZP.TokenizerPos
                LDA #Tokens.COMMA
                STA ZP.CurrentToken
                return;
            }
            case '"':
            {
                // String literal - scan to closing quote
                INC ZP.TokenizerPos  // Skip opening quote
                STX ZP.TokenStart  // Don't include quote in token
                
                loop
                {
                    LDX ZP.TokenizerPos
                    CPX ZP.BasicInputLength
                    if (Z)  // End of input without closing quote
                    {
                        LDA #Tokens.STRING
                        STA ZP.CurrentToken
                        return;
                    }
                    
                    LDA Address.BasicInputBuffer, X
                    INC ZP.TokenizerPos
                    CMP #'"'
                    if (Z)  // Found closing quote
                    {
                        LDA #Tokens.STRING
                        STA ZP.CurrentToken
                        return;
                    }
                    INC ZP.TokenLen
                }
            }
        }
        
        // Check if it's a number
        CMP #'0'
        if (C)  // >= '0'
        {
            CMP #('9'+1)
            if (NC)  // <= '9'
            {
                // Scan number
                loop
                {
                    INC ZP.TokenizerPos
                    INC ZP.TokenLen
                    
                    LDX ZP.TokenizerPos
                    CPX ZP.BasicInputLength
                    if (Z) { break; }
                    
                    LDA Address.BasicInputBuffer, X
                    CMP #'0'
                    if (C)  // >= '0'
                    {
                        CMP #('9'+1)
                        if (NC)  // <= '9'
                        {
                            continue;
                        }
                    }
                    break;  // Not a digit
                }
                
                LDA #Tokens.NUMBER
                STA ZP.CurrentToken
                return;
            }
        }
        
        // Must be an identifier or keyword
        // Scan alphanumeric characters
        loop
        {
            LDX ZP.TokenizerPos
            CPX ZP.BasicInputLength
            if (Z) { break; }
            
            LDA Address.BasicInputBuffer, X
            isAlphaNum();
            if (Z) { break; }  // Not alphanumeric
            
            INC ZP.TokenizerPos
            INC ZP.TokenLen
        }
        
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
    
    // Read a line of input into buffer
    // Returns length in A
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
                            CPX # Address.BasicInputBufferLength
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
    
    // Initialize tokenizer
    Initialize()
    {
        STZ ZP.TokenizerPos
        STZ ZP.BasicInputLength
        STZ ZP.TokenStart
        STZ ZP.TokenLen
        STZ ZP.CurrentToken
    }
    
    // Get current token as number (assumes NUMBER)
    // Returns 16-bit number in ZP.TOP
    getTokenNumber()
    {
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        LDX ZP.TokenStart
        LDY #0
        
        loop
        {
            CPY ZP.TokenLen
            if (Z) { break; }
            
            // TOP = TOP * 10
            LDA ZP.TOPL
            STA ZP.NEXTL
            LDA ZP.TOPH
            STA ZP.NEXTH
            
            // Multiply by 10 using shifts and adds
            ASL ZP.TOPL     // *2
            ROL ZP.TOPH
            ASL ZP.TOPL     // *4
            ROL ZP.TOPH
            CLC
            LDA ZP.TOPL     // *4 + *1 = *5
            ADC ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.TOPH
            ADC ZP.NEXTH
            STA ZP.TOPH
            ASL ZP.TOPL     // *10
            ROL ZP.TOPH
            
            // Add digit
            LDA Address.BasicInputBuffer, X
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
            INY
        }
    }
    
    // Get current token as padded name (8 bytes, space-padded)
    // Output: 8-byte padded name at ZP.FSOURCEADDRESS, actual length in ZP.FLENGTHL
    getTokenName()
    {
        // Store actual length
        LDA ZP.TokenLen
        STA ZP.FLENGTHL
        STZ ZP.FLENGTHH
        
        // Copy characters and pad to 8 bytes
        LDX ZP.TokenStart
        LDY #0
        
        // Copy actual characters
        loop
        {
            CPY ZP.TokenLen
            if (Z) { break; }  // Copied all characters
            CPY #8
            if (Z) { break; }  // Name buffer full
            
            LDA Address.BasicInputBuffer, X
            makeUppercase();
            STA [ZP.FSOURCEADDRESS], Y
            INX
            INY
        }
        
        // Pad with spaces to 8 bytes total
        loop
        {
            CPY #8
            if (Z) { break; }
            
            LDA #' '
            STA [ZP.FSOURCEADDRESS], Y
            INY
        }
    }
}
