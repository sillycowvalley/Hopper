unit Tokenizer
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "Limits"
    uses "Tools"
    uses "Messages"
    uses "BasicTypes"
    
    // Phase 1 Token definitions (extended set)
    enum Tokens
    {
        // Console commands
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
        HEAP     = 0x0D,
        BUFFERS  = 0x0E,
        DUMP     = 0x0F,
        BYE      = 0x10,
        REM      = 0x11,
        COMMENT  = 0x12,
        EOL      = 0x13,
        
        // Type declarations
        INT      = 0x14,
        WORD     = 0x15,
        BIT      = 0x16,
        CONST    = 0x17, 
        
        // Language keywords
        PRINT    = 0x20,
        IF       = 0x21,
        THEN     = 0x22,
        FUNC     = 0x23,
        ENDFUNC  = 0x24,
        RETURN   = 0x25,
        BEGIN    = 0x26,
        END      = 0x27,     
           
        // Logical keywords
        AND      = 0x30,
        OR       = 0x31,
        NOT      = 0x32,
        MOD      = 0x33,
        
        // Sentinel marking end of keywords
        lastKeyword = 0x33,
        
        // Built-in literals
        TRUE     = 0x34,  // Built-in BIT constant (1)
        FALSE    = 0x35,  // Built-in BIT constant (0)
        
        // Basic operators
        EQUALS   = 0x40,  // =
        PLUS     = 0x41,  // +
        MINUS    = 0x42,  // -
        LPAREN   = 0x43,  // (
        RPAREN   = 0x44,  // )
        NOTEQUAL = 0x45,  // <>
        
        // Additional comparison operators
        LT       = 0x50,  // 
        GT       = 0x51,  // >
        LE       = 0x52,  // <=
        GE       = 0x53,  // >=
        
        // Arithmetic operators
        MULTIPLY = 0x58,  // *
        DIVIDE   = 0x59,  // /
        
        BITWISE_AND = 0x5A,  // &
        BITWISE_OR  = 0x5B,  // |
        
        // Literals and identifiers
        NUMBER     = 0x80,
        STRING     = 0x81,
        IDENTIFIER = 0x82,
        EOF        = 0x83,
        COLON      = 0x84, 
        COMMA      = 0x85,
    }
    
    enum IdentifierType
    {
        Undefined,
        Global,
        Constant,
        Function,
        Argument,
        Local,
        Keyword
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
        4, Tokens.HEAP, 'H', 'E', 'A', 'P',
        7, Tokens.BUFFERS, 'B', 'U', 'F', 'F', 'E', 'R', 'S',
        4, Tokens.DUMP, 'D', 'U', 'M', 'P', 
        3, Tokens.BYE, 'B', 'Y', 'E',
        3, Tokens.REM, 'R', 'E', 'M',
        3, Tokens.INT, 'I', 'N', 'T',
        4, Tokens.WORD, 'W', 'O', 'R', 'D',
        3, Tokens.BIT, 'B', 'I', 'T',
        5, Tokens.CONST, 'C', 'O', 'N', 'S', 'T', 
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
        4, Tokens.TRUE, 'T', 'R', 'U', 'E',
        5, Tokens.FALSE, 'F', 'A', 'L', 'S', 'E',
        0  // End marker
    };
    
    
    // Print keyword corresponding to token value
    // Input: A = token value (e.g., Tokens.CONST, Tokens.INT, etc.)
    // Output: Keyword printed to serial
    // Modifies: A, X, Y (internal search), preserves token value concept
    // Error: If token not found in keywords table, prints nothing
    PrintKeyword()
    {
        PHA  // Save token value
        PHX
        PHY
        
        STA ZP.ACCL  // Store target token value
        
        LDY #0  // Index into keywords table
        loop
        {
            LDA keywords, Y     // Get length of this keyword
            if (Z) 
            { 
                break; 
            }   // End of table - not found
            
            STA ZP.ACCH         // Save keyword length
            INY
            LDA keywords, Y     // Get token value
            CMP ZP.ACCL         // Compare with target
            if (Z)
            {
                // Found it! Print the keyword
                INY  // Move to first character
                LDX ZP.ACCH  // X = character count
                loop
                {
                    CPX #0
                    if (Z) { break; }
                    
                    LDA keywords, Y
                    Serial.WriteChar();
                    INY
                    DEX
                }
                break;  // Done printing
            }
            
            // Skip to next keyword: advance Y by keyword length + 1 (for token byte)
            INY  // Skip the token value byte first
            LDX ZP.ACCH  // Then skip the keyword characters
            loop
            {
                CPX #0
                if (Z) { break; }
                INY
                DEX
            }
        }
        PLY
        PLX
        PLA
    }
    
    
    // Check if a token value represents a keyword
    // Input: A = token value to check
    // Output: C set if token is a keyword, NC if not a keyword  
    // Modifies: Processor flags only
    IsKeyword()
    {
        CMP #Tokens.IDENTIFIER
        if (Z)
        {
            CLC  // Not a keyword    
        }
        else
        {
            CMP #(Tokens.lastKeyword + 1)
            if (C)  // >= lastKeyword + 1
            {
                CLC  // Not a keyword
            }
            else
            {
                SEC  // Is a keyword
            }
        }
    }
    
    // Initialize tokenizer state
    // Input: None
    // Output: Tokenizer state cleared and ready for use
    // Munts: ZP.TokenizerPos, ZP.TokenBufferLength, ZP.BasicInputLength, ZP.CurrentToken, ZP.TokenLiteralPos
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
    
    // Set IDX = BasicTokenizerBuffer + TokenizerPos
    // Input: None (uses ZP.TokenizerPos)
    // Output: ZP.IDX = pointer to current position in token buffer
    // Munts: ZP.IDX
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
    
    // Set IDX = BasicTokenizerBuffer + TokenBufferLength
    // Input: None (uses ZP.TokenBufferLength)
    // Output: ZP.IDX = pointer to end of token buffer
    // Munts: ZP.IDX
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
    
    // Increment 16-bit TokenizerPos
    // Input: None (uses ZP.TokenizerPos)
    // Output: ZP.TokenizerPos incremented by 1
    // Munts: ZP.TokenizerPos
    incrementTokenizerPos()
    {
        INC ZP.TokenizerPosL
        if (Z)
        {
            INC ZP.TokenizerPosH
        }
    }
    
    // Increment 16-bit TokenBufferLength
    // Input: None (uses ZP.TokenBufferLength)
    // Output: ZP.TokenBufferLength incremented by 1
    // Munts: ZP.TokenBufferLength
    incrementTokenBufferLength()
    {
        INC ZP.TokenBufferLengthL
        if (Z)
        {
            INC ZP.TokenBufferLengthH
        }
    }
    
    // Compare 16-bit values - TokenizerPos vs TokenBufferLength
    // Input: None (uses ZP.TokenizerPos, ZP.TokenBufferLength)
    // Output: Z set if equal, C set if TokenizerPos >= TokenBufferLength
    // Preserves: Everything
    CompareTokenizerPosToLength()
    {
        LDA ZP.TokenizerPosH
        CMP ZP.TokenBufferLengthH
        if (NZ) { return; }  // Not equal, C flag is correct
        
        // High bytes equal, compare low bytes
        LDA ZP.TokenizerPosL
        CMP ZP.TokenBufferLengthL
    }
    
    // Skip whitespace in input buffer at position X
    // Input: X = position in BasicInputBuffer
    // Output: X = position of next non-whitespace character (or end of buffer)
    // Munts: X, A
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
    
    flags CharClass
    {
        Other = 0b00000000,
        Digit = 0b00000001,
        Alpha = 0b00000010,
        Hex   = 0b00000100,
        Lower = 0b00001000,
        Upper = 0b00010000, // redundant - just !Lower
    }
    
    // Check character type
    // Input: A = character
    // Output: A = bitmapped CharClass
    // Preserves: Everything except A
    getCharClass()
    {
        loop
        {
            CMP #'0'
            if (C)
            {
                CMP #('9'+1)
                if (NC) { LDA # (CharClass.Digit | CharClass.Hex) break; }
            }
            CMP #'A'
            if (C)
            {
                CMP #('F'+1) 
                if (NC) { LDA # (CharClass.Alpha | CharClass.Upper | CharClass.Hex) break; }
            }
            CMP #'G'
            if (C)
            {
                CMP #('Z'+1) 
                if (NC) { LDA # (CharClass.Alpha | CharClass.Upper) break; }
            }
            CMP #'a'
            if (C)
            {
                CMP #('f'+1)
                if (NC) { LDA # (CharClass.Alpha | CharClass.Lower | CharClass.Hex) break; }
            }
            CMP #'g'
            if (C)
            {
                CMP #('z'+1)
                if (NC) { LDA # (CharClass.Alpha | CharClass.Lower) break; }
            }
            LDA # CharClass.Other
            break;
        } // single exit
    }
    
    // Input: A = character
    // Output: C = digit, NC = not digit
    IsDigit()
    {
        PHA
        getCharClass();
        AND # CharClass.Digit
        if (NZ)
        {
            SEC // '0'..'9'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = alpha, NC = not alpha
    IsAlpha()
    {
        PHA
        getCharClass();
        AND # CharClass.Alpha
        if (NZ)
        {
            SEC // 'a'..'z' | 'A'..'Z'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = alphanumeric, NC = not alphanumeric
    IsAlphaNumeric()
    {
        PHA
        getCharClass();
        AND # (CharClass.Alpha|CharClass.Digit)
        if (NZ)
        {
            SEC // 'a'..'z' | 'A'..'Z'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = lower alpha, NC = not lower alpha
    IsLower()
    {
        PHA
        getCharClass();
        AND # CharClass.Lower
        if (NZ)
        {
            SEC // 'a'..'z'
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: A = character
    // Output: C = hex digit, NC = not hex digit
    IsHex()
    {
        PHA
        getCharClass();
        AND # CharClass.Hex
        if (NZ)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Append byte to token buffer using 16-bit addressing
    // Input: A = byte to append
    // Output: C set if successful, NC if buffer full (error set in ZP.LastError)
    // Munts: ZP.TokenBufferLength, ZP.IDX, A, Y
    // Error: Sets ZP.LastError if buffer overflow
    appendToTokenBuffer()
    {
        PHX
        PHY
        PHA  // Save byte to append
        loop
        {
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
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC
                    break;
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
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC
                    break;
                }
            }
            
            // Set up 16-bit pointer to end of buffer
            setTokenBufferEndPointer();
            SEC // all good
            break;
        }
        // Store byte using indirect addressing
        PLA  // Get byte to store
        if (C) 
        {
            LDY #0
            STA [ZP.IDX], Y
            
            // Increment 16-bit length
            incrementTokenBufferLength();
            SEC // all good
        }
        PLY
        PLX
    }
    
    // Find keyword match for current identifier in working buffer
    // Input: Working buffer at Address.BasicProcessBuffer1, null-terminated
    // Output: A = token value if found, or 0 if not found
    // Munts: A, X, Y, ZP.ACC
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
    
    scanHexNumber()
    {
        // Add NUMBER token
        LDA #Tokens.NUMBER
        appendToTokenBuffer();
        Messages.CheckError();
        if (NC) { return; }
        
        // Store "0x" prefix
        LDA #'0'
        appendToTokenBuffer();
        Messages.CheckError();
        if (NC) { return; }
        
        LDA Address.BasicInputBuffer, X  // 'x' or 'X'
        appendToTokenBuffer();
        Messages.CheckError();
        if (NC) { return; }
        INX
        
        // Scan hex digits
        loop
        {
            CPX ZP.BasicInputLength
            if (Z) { break; }
            
            LDA Address.BasicInputBuffer, X
            IsHex();
            if (NC) { break; }  // Not hex digit
            
            LDA Address.BasicInputBuffer, X
            appendToTokenBuffer();
            Messages.CheckError();
            if (NC) { return; }
            INX
        }
        
        // Add null terminator
        LDA #0
        appendToTokenBuffer();
        Messages.CheckError();
        if (NC) { return; }
    }
    
    // Parse hex number from token buffer
    // Input: ZP.IDX = pointer to "0x..." string, Y = 1 (pointing at 'x')
    // Output: ZP.TOP = 16-bit hex value, ZP.TOPT = determined type (INT or WORD)
    // Munts: ZP.TOP, ZP.TOPT, ZP.ACC, A, Y
    // Error: Sets ZP.LastError if invalid hex or overflow
    parseHexNumber()
    {
        INY  // Skip 'x' or 'X'
        
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) { break; }  // Null terminator
            
            IsHex();
            if (NC)  // Not hex digit
            {
                // Set syntax error and return
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)  
                STA ZP.LastErrorH
                Messages.StorePC();
                return;
            }
            
            // Convert hex char to value (0-15)
            LDA [ZP.IDX], Y
            IsDigit();
            if (C)
            {
                SEC
                SBC #'0'  // '0'-'9' -> 0-9
            }
            else
            {
                IsLower();
                if (C)
                {
                    SEC
                    SBC #'a'
                    CLC
                    ADC #10  // 'a'-'f' -> 10-15
                }
                else
                {
                    SEC
                    SBC #'A'  
                    CLC
                    ADC #10  // 'A'-'F' -> 10-15
                }
            }
            
            STA ZP.ACCL  // Store hex digit value (0-15)
            
            // Check overflow: TOP > 0x0FFF would overflow when shifted left 4 bits
            LDA ZP.TOPH
            CMP #0x10
            if (C)  // >= 0x10, would overflow
            {
                LDA #(Messages.NumericOverflow % 256)
                STA ZP.LastErrorL
                LDA #(Messages.NumericOverflow / 256)
                STA ZP.LastErrorH
                return;
            }
            
            // Shift TOP left 4 bits (multiply by 16)
            ASL ZP.TOPL
            ROL ZP.TOPH
            ASL ZP.TOPL
            ROL ZP.TOPH
            ASL ZP.TOPL
            ROL ZP.TOPH
            ASL ZP.TOPL
            ROL ZP.TOPH
            
            // Add hex digit
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
        
        // Set type based on value (same as decimal)
        BIT ZP.TOPH
        if (MI)
        {
            LDA #BasicType.WORD   // 32768-65535
            STA ZP.TOPT
        }
        else
        {
            LDA #BasicType.INT    // 0-32767
            STA ZP.TOPT
        }
    }
    
    // Replace existing TokenizeLine() with mode-aware version
    TokenizeLine() 
    {
        STZ ZP.OpcodeTemp  // Replace mode = 0
        TokenizeLineWithMode();
    }
    
    TokenizeAndAppendLine()
    {
        LDA #1 // Append mode = 1
        STA ZP.OpcodeTemp   
        TokenizeLineWithMode();
    }
    
    // Tokenize complete line from BasicInputBuffer into BasicTokenizerBuffer
    // Input: BasicInputBuffer contains raw input, ZP.BasicInputLength = input length, mode in A
    // Output: Tokens stored in BasicTokenizerBuffer, ZP.TokenBufferLength = total length
    //         ZP.TokenizerPos reset to 0
    // Munts: ZP.TokenBufferLength, ZP.TokenizerPos, ZP.IDX, A, X, Y
    // Error: Sets ZP.LastError if tokenization fails
    TokenizeLineWithMode()
    {
        LDA ZP.OpcodeTemp
        if (Z)
        {
            // Replace mode - clear token buffer
            STZ ZP.TokenBufferLengthL
            STZ ZP.TokenBufferLengthH
            Messages.ClearError();
        }
        
        // Check for empty line
        LDA ZP.BasicInputLength
        if (Z)
        {
            // Empty line - add EOL token
            LDA # Tokens.EOL
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
                case ':':
                {
                    LDA #Tokens.COLON
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case ',':
                {
                    LDA #Tokens.COMMA
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
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
                case '&':
                {
                    LDA #Tokens.BITWISE_AND
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '|':
                {
                    LDA #Tokens.BITWISE_OR
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
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC  // Error
                    return;
                }
                case '\'':
                {
                    // Single quote comment
                    LDA #Tokens.COMMENT
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    INX  // Skip the quote character
                    
                    // Store comment text as inline data
                    loop
                    {
                        CPX ZP.BasicInputLength
                        if (Z) { break; }  // End of input
                        
                        LDA Address.BasicInputBuffer, X
                        appendToTokenBuffer();
                        Messages.CheckError();
                        if (NC) { return; }
                        INX
                    }
                    
                    // Add null terminator
                    LDA #0
                    appendToTokenBuffer();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // X is already at end of input, continue will break out of main loop
                }
                default:
                {
                    // Check for hex number (0x prefix)
                    LDA Address.BasicInputBuffer, X
                    CMP #'0'
                    if (Z)
                    {
                        // Look ahead for 'x' or 'X'
                        INX
                        CPX ZP.BasicInputLength
                        if (NZ)
                        {
                            LDA Address.BasicInputBuffer, X
                            CMP #'x'
                            if (Z) { scanHexNumber(); continue; }
                            CMP #'X'  
                            if (Z) { scanHexNumber(); continue; }
                        }
                        DEX  // Back up, not hex
                        LDA Address.BasicInputBuffer, X
                    }
                    
                    // Check if it's a decimal number
                    IsDigit();
                    if (C)  
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
                            IsDigit();
                            if (NC) { SEC break; }  // Not a digit
                            
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
                    IsAlpha();
                    if (NC)
                    {
                        // Invalid character
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        
                        Messages.StorePC(); // 6502 PC -> IDY
                        
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
                        IsAlphaNumeric();
                        if (NC) { break; }  // Not alphanumeric
                        
                        // Convert to uppercase and store in working buffer
                        IsLower();
                        if (C)
                        {
                            SEC
                            SBC #('a'-'A')  // Convert to uppercase
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
                            
                            Messages.StorePC(); // 6502 PC -> IDY
                            
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
                        // Check if it's REM - need special processing
                        CMP #Tokens.REM
                        if (Z)
                        {
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) { return; }
                            
                            // Store comment text as inline data
                            skipWhitespace();  // Skip spaces after REM
                            
                            // Store all remaining characters as comment text
                            loop
                            {
                                CPX ZP.BasicInputLength
                                if (Z) { break; }  // End of input
                                
                                LDA Address.BasicInputBuffer, X
                                appendToTokenBuffer();
                                Messages.CheckError();
                                if (NC) { return; }
                                INX
                            }
                            
                            // Add null terminator
                            LDA #0
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) { return; }
                            
                            // X is already at end of input, continue will break out of main loop
                            continue;
                        }
                        
                        // Regular keyword processing
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
                            STA ZP.ACCL 
                            appendToTokenBuffer();
                            Messages.CheckError();
                            if (NC) 
                            { 
                                return; 
                            }
                            LDA ZP.ACCL // set Z
                            if (Z) 
                            { 
                                break;   // Copied null terminator
                            }
                            INY
                        } // loop
                    }
                }
            }
        }
        
        // Add EOL token
        LDA #Tokens.EOL
        appendToTokenBuffer();
        Messages.CheckError();
        if (NC) { return; }
        
        LDA ZP.OpcodeTemp
        if (Z)
        {
            // Reset tokenizer position only in replace mode
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
        }
        // Append mode - DON'T reset position (keep pointing to function start)
        
        SEC  // Success
    }
    
    // Get next token from BasicTokenizerBuffer using 16-bit addressing
    // Input: ZP.TokenizerPos = current position in token buffer
    // Output: A = token type, ZP.CurrentToken = token type
    //         For literals, ZP.TokenLiteralPos = position of inline data
    //         ZP.TokenizerPos advanced past token and any inline data
    // Munts: ZP.CurrentToken, ZP.TokenizerPos, ZP.TokenLiteralPos, ZP.IDX, A, Y
    NextToken()
    {
        // 16-bit comparison: if (TokenizerPos >= TokenBufferLength)
        CompareTokenizerPosToLength();
        if (C)  // TokenizerPos >= TokenBufferLength
        {
            LDA # Tokens.EOF
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
        CMP #Tokens.REM
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
        CMP #Tokens.COMMENT
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
    
    // Check if multiplying ZP.TOP by 10 and adding a digit would overflow
    // Input: ZP.TOP = current 16-bit value, A = digit to add (0-9)
    // Output: C set if operation is safe, NC set if would overflow
    // Preserves: A (digit), ZP.TOP unchanged
    // Munts: ZP.NEXT (temporarily)
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
    // Input: ZP.TokenLiteralPos = position of number string in token buffer
    // Output: ZP.TOP = 16-bit number value, ZP.TOPT = determined type (INT or WORD)
    // Munts: ZP.TOP, ZP.TOPT, ZP.IDX, ZP.NEXT, ZP.ACC, A, Y
    // Error: Sets ZP.LastError if number is invalid or overflows
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
        
        // Check for hex format (0x prefix)
        LDA [ZP.IDX], Y
        CMP #'0'
        if (Z)
        {
            INY
            LDA [ZP.IDX], Y
            CMP #'x'
            if (Z) { parseHexNumber(); return; }
            CMP #'X'
            if (Z) { parseHexNumber(); return; }
            DEY  // Back up
        }
        
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) { break; }  // Hit null terminator
            
            // Check if character is a digit
            LDA [ZP.IDX], Y
            IsDigit();
            if (NC) // not digit
            {
                /*
                LDA #(Messages.SyntaxError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.SyntaxError / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                return;
                */
                break;
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
    // Input: ZP.TokenizerPos = current position in token buffer
    // Output: ZP.TokenizerPos advanced past null terminator
    // Munts: ZP.TokenizerPos, ZP.IDX, A, Y
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
    // Input: None (reads from serial)
    // Output: A = length of input, ZP.BasicInputLength = input length
    //         Line stored in BasicInputBuffer (null-terminated not required)
    // Munts: ZP.BasicInputLength, A, X
    ReadLine()
    {
        // Show appropriate prompt
        IsCaptureMode();
        if (NC)
        {
            // Normal mode - prompt already shown by interpreterLoop
        }
        else
        {
            // Function capture mode
            LDA #'*'
            Serial.WriteChar();
            LDA #' '
            Serial.WriteChar();
        }
        
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
                    
                    // If in function capture mode, cancel it
                    IsCaptureMode();
                    if (C)
                    {
                        Console.ExitFunctionCaptureMode();
                    }
                    
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
    // Input: ZP.TokenLiteralPos = position of literal data in token buffer
    // Output: ZP.TOP = pointer to null-terminated string in token buffer
    // Munts: ZP.TOP
    GetTokenString()
    {
        PHA
        
        // Set up pointer to saved literal position in token buffer
        CLC
        LDA #(Address.BasicTokenizerBuffer & 0xFF)
        ADC ZP.TokenLiteralPosL
        STA ZP.TOPL
        LDA #(Address.BasicTokenizerBuffer >> 8)
        ADC ZP.TokenLiteralPosH
        STA ZP.TOPH
        
        PLA
    }
}
