unit Tokenizer
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "/Source/Runtime/6502/Stacks"
    
    friend Interpreter;
    
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
        FUNCS    = 0x0A,
        
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
        
        // Special tokens
        NUMBER   = 0x80,
        STRING   = 0x81,
        IDENT    = 0x82,
        EOL      = 0x83,
        EOF      = 0x84,
        
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
    
    // Input buffer for command line (128 bytes should be plenty)
    const uint INPUT_BUFFER = Address.CallStackLSB;  // Use part of call stack area when not running programs
    const byte MAX_INPUT_LEN = 120;
    
    // Tokenizer state
    const byte tkINPUT_POS   = ZP.D0;   // Current position in input buffer
    const byte tkINPUT_LEN   = ZP.D1;   // Length of current input
    const byte tkTOKEN_START = ZP.D2;   // Start of current token
    const byte tkTOKEN_LEN   = ZP.D3;   // Length of current token
    const byte tkCURRENT_TOK = ZP.D4;   // Current token value
    
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
        5, Tokens.FUNCS, 'F', 'U', 'N', 'C', 'S',
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
            if (C)  // <= 'z'  
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
            LDX tkINPUT_POS
            CPX tkINPUT_LEN
            if (Z) { break; }  // End of input
            
            LDA INPUT_BUFFER, X
            CMP #' '
            if (Z)
            {
                INC tkINPUT_POS
                continue;
            }
            CMP #'\t'
            if (Z)
            {
                INC tkINPUT_POS
                continue;
            }
            break;  // Non-whitespace found
        }
    }
    
    // Check if character is alphanumeric
    isAlphaNum()
    {
        // A contains character to test
        // Returns Z=1 if alphanumeric, Z=0 if not
        CMP #'0'
        if (C)  // >= '0'
        {
            CMP #('9'+1)
            if (C)  // <= '9'
            {
                LDA #1  // Set Z=0 (is alphanumeric)
                return;
            }
        }
        CMP #'A'
        if (C)  // >= 'A'
        {
            CMP #('Z'+1)
            if (C)  // <= 'Z'
            {
                LDA #1  // Set Z=0 (is alphanumeric)
                return;
            }
        }
        STZ ZP.ACCL  // Set Z=1 (not alphanumeric)
        LDA ZP.ACCL
    }
    
    // Find keyword match
    // Returns token in A if found, or 0 if not found
    findKeyword()
    {
        LDY #0  // Index into keywords table
        
        loop
        {
            LDA keywords, Y  // Get length of this keyword
            if (Z) { break; }  // End of table
            
            CMP tkTOKEN_LEN
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
                    CPX tkTOKEN_LEN
                    if (Z)  // All characters matched
                    {
                        LDA ZP.ACCL  // Return token value
                        return;
                    }
                    
                    LDA keywords, Y
                    STA ZP.ACCH      // Expected character
                    
                    TXA
                    CLC
                    ADC tkTOKEN_START
                    TAX
                    LDA INPUT_BUFFER, X  // Actual character
                    makeUppercase();
                    
                    CMP ZP.ACCH
                    if (NZ)  // Mismatch
                    {
                        break;
                    }
                    
                    TXA
                    SEC 
                    SBC tkTOKEN_START
                    TAX
                    INX
                    INY
                }
                
                // Skip rest of this keyword
                LDX tkTOKEN_LEN
                DEX  // We already advanced Y by tokenlen
                loop
                {
                    CPX #0
                    if (Z) { break; }
                    INY
                    DEX
                }
            }
            else
            {
                // Skip this keyword (length + 1 for token + length for chars)
                INY  // Skip token
                CLC
                ADC #1  // Length was in A
                TAX
                loop
                {
                    CPX #0
                    if (Z) { break; }
                    INY
                    DEX
                }
            }
        }
        
        LDA #0  // Not found
    }
    
    // Get next token from input buffer
    // Returns token type in A, updates tkCURRENT_TOK
    nextToken()
    {
        skipWhitespace();
        
        LDX tkINPUT_POS
        CPX tkINPUT_LEN
        if (Z)
        {
            LDA #Tokens.EOL
            STA tkCURRENT_TOK
            return;
        }
        
        // Mark start of token
        STX tkTOKEN_START
        STZ tkTOKEN_LEN
        
        LDA INPUT_BUFFER, X
        
        // Check for single character tokens
        switch (A)
        {
            case '=':
            {
                INC tkINPUT_POS
                LDA #Tokens.EQUALS
                STA tkCURRENT_TOK
                return;
            }
            case '+':
            {
                INC tkINPUT_POS
                LDA #Tokens.PLUS
                STA tkCURRENT_TOK
                return;
            }
            case '-':
            {
                INC tkINPUT_POS
                LDA #Tokens.MINUS
                STA tkCURRENT_TOK
                return;
            }
            case '*':
            {
                INC tkINPUT_POS
                LDA #Tokens.MULTIPLY
                STA tkCURRENT_TOK
                return;
            }
            case '/':
            {
                INC tkINPUT_POS
                LDA #Tokens.DIVIDE
                STA tkCURRENT_TOK
                return;
            }
            case '(':
            {
                INC tkINPUT_POS
                LDA #Tokens.LPAREN
                STA tkCURRENT_TOK
                return;
            }
            case ')':
            {
                INC tkINPUT_POS
                LDA #Tokens.RPAREN
                STA tkCURRENT_TOK
                return;
            }
            case '[':
            {
                INC tkINPUT_POS
                LDA #Tokens.LBRACKET
                STA tkCURRENT_TOK
                return;
            }
            case ']':
            {
                INC tkINPUT_POS
                LDA #Tokens.RBRACKET
                STA tkCURRENT_TOK
                return;
            }
            case ',':
            {
                INC tkINPUT_POS
                LDA #Tokens.COMMA
                STA tkCURRENT_TOK
                return;
            }
            case '"':
            {
                // String literal - scan to closing quote
                INC tkINPUT_POS  // Skip opening quote
                STX tkTOKEN_START  // Don't include quote in token
                
                loop
                {
                    LDX tkINPUT_POS
                    CPX tkINPUT_LEN
                    if (Z)  // End of input without closing quote
                    {
                        LDA #Tokens.STRING
                        STA tkCURRENT_TOK
                        return;
                    }
                    
                    LDA INPUT_BUFFER, X
                    INC tkINPUT_POS
                    CMP #'"'
                    if (Z)  // Found closing quote
                    {
                        LDA #Tokens.STRING
                        STA tkCURRENT_TOK
                        return;
                    }
                    INC tkTOKEN_LEN
                }
            }
        }
        
        // Check if it's a number
        CMP #'0'
        if (C)  // >= '0'
        {
            CMP #('9'+1)
            if (C)  // <= '9'
            {
                // Scan number
                loop
                {
                    INC tkINPUT_POS
                    INC tkTOKEN_LEN
                    
                    LDX tkINPUT_POS
                    CPX tkINPUT_LEN
                    if (Z) { break; }
                    
                    LDA INPUT_BUFFER, X
                    CMP #'0'
                    if (C)  // >= '0'
                    {
                        CMP #('9'+1)
                        if (C)  // <= '9'
                        {
                            continue;
                        }
                    }
                    break;  // Not a digit
                }
                
                LDA #Tokens.NUMBER
                STA tkCURRENT_TOK
                return;
            }
        }
        
        // Must be an identifier or keyword
        // Scan alphanumeric characters
        loop
        {
            LDX tkINPUT_POS
            CPX tkINPUT_LEN
            if (Z) { break; }
            
            LDA INPUT_BUFFER, X
            isAlphaNum();
            if (Z) { break; }  // Not alphanumeric
            
            INC tkINPUT_POS
            INC tkTOKEN_LEN
        }
        
        // Check if it's a keyword
        findKeyword();
        if (NZ)  // Found keyword
        {
            STA tkCURRENT_TOK
            return;
        }
        
        // It's an identifier
        LDA #Tokens.IDENT
        STA tkCURRENT_TOK
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
                            CPX #MAX_INPUT_LEN
                            if (Z) { continue; }  // Buffer full
                            
                            STA INPUT_BUFFER, X
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
        
        STX tkINPUT_LEN
        STZ tkINPUT_POS
        TXA  // Return length
    }
    
    // Initialize tokenizer
    Initialize()
    {
        STZ tkINPUT_POS
        STZ tkINPUT_LEN
        STZ tkTOKEN_START
        STZ tkTOKEN_LEN
        STZ tkCURRENT_TOK
    }
    
    // Get current token as number (assumes NUMBER)
    // Returns 16-bit number in ZP.TOP
    getTokenNumber()
    {
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        LDX tkTOKEN_START
        LDY #0
        
        loop
        {
            CPY tkTOKEN_LEN
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
            LDA INPUT_BUFFER, X
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
}
