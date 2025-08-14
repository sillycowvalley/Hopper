unit Tokenizer // Tokenizer.asm
{
    uses "./Definitions/Limits"
    uses "./Definitions/Messages"
    uses "./Debugging/Error"  // Added Error unit
    uses "./Definitions/BasicTypes"
    uses "./Definitions/Tokens"
    
    friend Console;
    
    
    
    // Initialize tokenizer state
    // Input: None
    // Output: Tokenizer state cleared and ready for use
    // Munts: ZP.TokenizerPos, ZP.TokenBufferContentSize, ZP.BasicInputLength, ZP.CurrentToken, ZP.TokenLiteralPos
    Initialize()
    {
        STZ ZP.TokenizerPosL
        STZ ZP.TokenizerPosH
        STZ ZP.TokenBufferContentSizeL
        STZ ZP.TokenBufferContentSizeH
        STZ ZP.BasicInputLength
        STZ ZP.CurrentToken
        STZ ZP.TokenLiteralPosL
        STZ ZP.TokenLiteralPosH
    }
    
    // Set IDX = TokenizerBuffer + TokenizerPos
    // Input: None (uses ZP.TokenizerPos)
    // Output: ZP.IDX = pointer to current position in token buffer
    // Munts: ZP.IDX
    setTokenizerPointer()
    {
        CLC
        LDA ZP.TokenBufferL
        ADC ZP.TokenizerPosL
        STA ZP.IDXL
        LDA ZP.TokenBufferH
        ADC ZP.TokenizerPosH
        STA ZP.IDXH
    }
    
    // Set IDX = TokenizerBuffer + TokenBufferContentSize
    // Input: None (uses ZP.TokenBufferContentSize)
    // Output: ZP.IDX = pointer to end of token buffer
    // Munts: ZP.IDX
    setTokenBufferEndPointer()
    {
        CLC
        LDA ZP.TokenBufferL
        ADC ZP.TokenBufferContentSizeL
        STA ZP.IDXL
        LDA ZP.TokenBufferH
        ADC ZP.TokenBufferContentSizeH
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
    
    // Increment 16-bit TokenBufferContentSize
    // Input: None (uses ZP.TokenBufferContentSize)
    // Output: ZP.TokenBufferContentSize incremented by 1
    // Munts: ZP.TokenBufferContentSize
    incrementTokenBufferContentSize()
    {
        INC ZP.TokenBufferContentSizeL
        if (Z)
        {
            INC ZP.TokenBufferContentSizeH
        }
    }
    
    // Compare 16-bit values - TokenizerPos vs TokenBufferContentSize
    // Input: None (uses ZP.TokenizerPos, ZP.TokenBufferContentSize)
    // Output: Z set if equal, C set if TokenizerPos >= TokenBufferContentSize
    // Preserves: Everything
    CompareTokenizerPosToLength()
    {
        LDA ZP.TokenizerPosH
        CMP ZP.TokenBufferContentSizeH
        if (NZ) { return; }  // Not equal, C flag is correct
        
        // High bytes equal, compare low bytes
        LDA ZP.TokenizerPosL
        CMP ZP.TokenBufferContentSizeL
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
    // Munts: ZP.TokenBufferContentSize, ZP.IDX, A, Y
    // Error: Sets ZP.LastError if buffer overflow
    appendToTokenBuffer()
    {
        PHX
        PHY
        PHA  // Save byte to append
        loop
        {
            // 16-bit boundary check
            LDA ZP.TokenBufferContentSizeH
            CMP #(Limits.TokenizerBufferLength >> 8)
            if (C) // ContentSizeH >= LimitH
            {
                if (NZ)  // ContentSizeH > LimitH (not equal)
                {
                    Error.BufferOverflow(); BIT ZP.EmulatorPCL
                    CLC
                    break;
                }
                // High bytes equal, must check low bytes
                LDA ZP.TokenBufferContentSizeL
                CMP #(Limits.TokenizerBufferLength & 0xFF) 
                if (C)  // ContentSizeL >= LimitL  
                {
//Debug.NL(); LDA ZP.TokenBufferContentSizeH HOut(); LDA ZP.TokenBufferContentSizeL HOut();
                    Error.BufferOverflow(); BIT ZP.EmulatorPCL
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
            incrementTokenBufferContentSize();
            SEC // all good
        }
        PLY
        PLX
    }
       
    scanHexNumber()
    {
        // Add NUMBER token
        LDA #Token.NUMBER
        appendToTokenBuffer();
        Error.CheckError();
        if (NC) { return; }
        
        // Store "0x" prefix
        LDA #'0'
        appendToTokenBuffer();
        Error.CheckError();
        if (NC) { return; }
        
        LDA Address.BasicInputBuffer, X  // 'x' or 'X'
        appendToTokenBuffer();
        Error.CheckError();
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
            Error.CheckError();
            if (NC) { return; }
            INX
        }
        
        // Add null terminator
        LDA #0
        appendToTokenBuffer();
        Error.CheckError();
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
                Error.SyntaxError(); BIT ZP.EmulatorPCL
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
                Error.NumericOverflow(); BIT ZP.EmulatorPCL
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
            LDA #BASICType.WORD   // 32768-65535
            STA ZP.TOPT
        }
        else
        {
            LDA #BASICType.INT    // 0-32767
            STA ZP.TOPT
        }
    }
    
    // Replace existing TokenizeLine() with mode-aware version
    TokenizeLine() 
    {
        STZ ZP.OpCodeTemp  // Replace mode = 0
        TokenizeLineWithMode();
    }

    TokenizeAndAppendLine()
    {
        LDA #1 // Append mode = 1
        STA ZP.OpCodeTemp   
        TokenizeLineWithMode();
    }

    // Tokenize complete line from BasicInputBuffer into BasicTokenizerBuffer
    // Input: BasicInputBuffer contains raw input, ZP.BasicInputLength = input length, mode in A
    // Output: Tokens stored in BasicTokenizerBuffer, ZP.TokenBufferContentSize = total length
    //         ZP.TokenizerPos reset to 0
    // Munts: ZP.TokenBufferContentSize, ZP.TokenizerPos, ZP.IDX, A, X, Y
    // Error: Sets ZP.LastError if tokenization fails
    TokenizeLineWithMode()
    {
        LDA ZP.OpCodeTemp
        if (Z)
        {
            // Replace mode - clear token buffer
            STZ ZP.TokenBufferContentSizeL
            STZ ZP.TokenBufferContentSizeH
            Error.ClearError();
        }
        
        // Check for empty line
        LDA ZP.BasicInputLength
        if (Z)
        {
            // Empty line handling depends on mode
            LDA ZP.OpCodeTemp
            if (Z)
            {
                // Replace mode - add EOL token for empty line
                LDA #Token.EOL
                appendToTokenBuffer();
                SEC  // Success
                return;
            }
            else
            {
                // Append mode - skip empty lines entirely (don't add EOL)
                SEC  // Success - but don't add anything to buffer
                return;
            }
        }
        
        // Check if line contains only whitespace
        LDX #0
        skipWhitespace(); // Updates X to first non-whitespace
        CPX ZP.BasicInputLength
        if (Z)
        {
            // Line is only whitespace - treat same as empty line
            LDA ZP.OpCodeTemp
            if (Z)
            {
                // Replace mode - add EOL token
                LDA #Token.EOL
                appendToTokenBuffer();
                SEC  // Success
                return;
            }
            else
            {
                // Append mode - skip whitespace-only lines
                SEC  // Success - but don't add anything to buffer
                return;
            }
        }
        
        // Line has content - process normally
        LDX #0  // Reset position in input buffer
        
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
                    LDA #Token.COLON
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case ',':
                {
                    LDA #Token.COMMA
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case ';':
                {
                    LDA #Token.SEMICOLON
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '=':
                {
                    LDA #Token.EQUALS
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '+':
                {
                    LDA #Token.PLUS
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '-':
                {
                    LDA #Token.MINUS
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '*':
                {
                    LDA #Token.MULTIPLY
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '/':
                {
                    LDA #Token.DIVIDE
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '&':
                {
                    LDA #Token.BITWISE_AND
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '|':
                {
                    LDA #Token.BITWISE_OR
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '(':
                {
                    LDA #Token.LPAREN
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case ')':
                {
                    LDA #Token.RPAREN
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case '[':
                {
                    LDA #Token.LBRACKET
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX
                }
                case ']':
                {
                    LDA #Token.RBRACKET
                    appendToTokenBuffer();
                    Error.CheckError();
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
                            LDA #Token.LE
                            appendToTokenBuffer();
                            Error.CheckError();
                            if (NC) { return; }
                            INX  // Skip both '<' and '='
                            continue;
                        }
                        CMP #'>'
                        if (Z)
                        {
                            LDA #Token.NOTEQUAL
                            appendToTokenBuffer();
                            Error.CheckError();
                            if (NC) { return; }
                            INX  // Skip both '<' and '>'
                            continue;
                        }
                    }
                    // Just '<'
                    DEX  // Back up to point at '<'
                    LDA #Token.LT
                    appendToTokenBuffer();
                    Error.CheckError();
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
                            LDA #Token.GE
                            appendToTokenBuffer();
                            Error.CheckError();
                            if (NC) { return; }
                            INX  // Skip both '>' and '='
                            continue;
                        }
                    }
                    // Just '>'
                    DEX  // Back up to point at '>'
                    LDA #Token.GT
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    INX  // Move past '>'
                }
                case '"':
                {
                    // String literal tokenization
                    LDA # Token.STRINGLIT
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    
                    // Store starting position of string content
                    LDA ZP.TokenBufferContentSizeL
                    STA ZP.TokenLiteralPosL
                    LDA ZP.TokenBufferContentSizeH
                    STA ZP.TokenLiteralPosH
                    
                    INX // Skip opening quote
                    
                    loop // Scan string content until closing quote
                    {
                        CPX ZP.BasicInputLength  // Check input buffer bounds
                        if (Z) // End of input without closing quote
                        {
                            Error.UnexpectedEOL(); BIT ZP.EmulatorPCL
                            CLC
                            return;
                        }
                        
                        CMP # 0x80
                        if (C)  // >= 128, invalid character
                        {
                            Error.IllegalCharacter(); BIT ZP.EmulatorPCL
                            CLC
                            return;
                        }
                        
                        LDA Address.BasicInputBuffer, X  // Read from input buffer
                        CMP #'"'
                        if (Z) // Found closing quote
                        {
                            // Add null terminator to string content
                            LDA #0
                            appendToTokenBuffer();
                            Error.CheckError();
                            if (NC) { return; }
                            
                            INX  // Skip closing quote in input buffer
                            break;
                        }
                        
                        // Add character to string content in token buffer
                        appendToTokenBuffer();
                        Error.CheckError();
                        if (NC) { return; }
                        
                        INX  // Advance input buffer position
                    }           
                    SEC  // Success
                }
                case '\'':
                {
                    // Character literal tokenization
                    LDA #Token.CHARLIT
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    
                    INX  // Skip opening quote
                    
                    // Check for end of input
                    CPX ZP.BasicInputLength
                    if (Z)  // End of input without character
                    {
                        Error.UnexpectedEOL(); BIT ZP.EmulatorPCL
                        CLC
                        return;
                    }
                    
                    // Get the character
                    LDA Address.BasicInputBuffer, X
                    
                    // Check for valid ASCII range (0-127)
                    CMP #0x80
                    if (C)  // >= 128, invalid character
                    {
                        Error.IllegalCharacter(); BIT ZP.EmulatorPCL
                        CLC
                        return;
                    }
                    
                    // Store the character value in token buffer
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    
                    INX  // Move past character
                    
                    // Check for closing quote
                    CPX ZP.BasicInputLength
                    if (Z)  // End of input without closing quote
                    {
                        Error.UnexpectedEOL(); BIT ZP.EmulatorPCL
                        CLC
                        return;
                    }
                    
                    LDA Address.BasicInputBuffer, X
                    CMP #'\''
                    if (NZ)  // Not a closing quote
                    {
                        Error.UnexpectedEOL(); BIT ZP.EmulatorPCL
                        CLC
                        return;
                    }
                    
                    INX  // Skip closing quote
                }
                case '!':
                {
                    // Single quote comment
                    LDA #Token.COMMENT
                    appendToTokenBuffer();
                    Error.CheckError();
                    if (NC) { return; }
                    
                    INX  // Skip the quote character
                    
                    // Store comment text as inline data
                    loop
                    {
                        CPX ZP.BasicInputLength
                        if (Z) { break; }  // End of input
                        
                        LDA Address.BasicInputBuffer, X
                        appendToTokenBuffer();
                        Error.CheckError();
                        if (NC) { return; }
                        INX
                    }
                    
                    // Add null terminator
                    LDA #0
                    appendToTokenBuffer();
                    Error.CheckError();
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
                        LDA #Token.NUMBER
                        appendToTokenBuffer();
                        Error.CheckError();
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
                            Error.CheckError();
                            if (NC) { return; }
                            INX
                        }
                        
                        // Add null terminator for number
                        LDA #0
                        appendToTokenBuffer();
                        Error.CheckError();
                        if (NC) { return; }
                        continue;
                    }
                    // Must be an identifier or keyword
                    IsAlpha();
                    if (NC)
                    {
                        // Invalid character
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
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
                        
                        CMP # 0x80
                        if (C)  // >= 128, invalid character
                        {
                            Error.IllegalCharacter(); BIT ZP.EmulatorPCL
                            CLC
                            return;
                        }
                        
                        IsAlphaNumeric();
                        if (NC) { break; }  // Not alphanumeric
                        
                        // Convert to uppercase and store in working buffer
                        IsLower();
                        if (C)
                        {
                            SEC
                            SBC #('a'-'A')  // Convert to uppercase
                        }
                        STA Address.BasicProcessBuffer, Y  // FIXED: Use Y for working buffer
                        
                        INX  // Advance input position
                        INY  // FIXED: Advance working buffer index separately
                        CPY #Limits.BasicProcessBufferLength
                        if (Z) 
                        { 
                            Error.SyntaxError(); BIT ZP.EmulatorPCL
                            CLC  // Error
                            return;
                        }
                    }
                    
                    // Add null terminator to working buffer
                    LDA #0
                    STA Address.BasicProcessBuffer, Y  // FIXED: Use Y for working buffer
                    
                    // Check if it's a keyword
                    Tokens.FindKeyword();
                    if (NZ)  // Found keyword
                    {
                        // Check if it's REM - need special processing
                        CMP #Token.REM
                        if (Z)
                        {
                            appendToTokenBuffer();
                            Error.CheckError();
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
                                Error.CheckError();
                                if (NC) { return; }
                                INX
                            }
                            
                            // Add null terminator
                            LDA #0
                            appendToTokenBuffer();
                            Error.CheckError();
                            if (NC) { return; }
                            
                            // X is already at end of input, continue will break out of main loop
                            continue;
                        }
                        
                        // Regular keyword processing
                        appendToTokenBuffer();
                        Error.CheckError();
                        if (NC) { return; }
                    }
                    else
                    {
                        // It's an identifier - store token + inline string
                        LDA #Token.IDENTIFIER
                        appendToTokenBuffer();
                        Error.CheckError();
                        if (NC) { return; }
                        
                        // Copy identifier from working buffer to token buffer
                        LDY #0  // Reset Y for copying
                        loop
                        {
                            LDA Address.BasicProcessBuffer, Y
                            STA ZP.ACCL 
                            appendToTokenBuffer();
                            Error.CheckError();
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
        
        // Add EOL token only for lines with content
        LDA #Token.EOL
        appendToTokenBuffer();
        Error.CheckError();
        if (NC) { return; }
        
        LDA ZP.OpCodeTemp
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
    // Preserves: X
    NextToken()
    {
        // 16-bit comparison: if (TokenizerPos >= TokenBufferContentSize)
        CompareTokenizerPosToLength();
        if (C)  // TokenizerPos >= TokenBufferContentSize
        {
            LDA # Token.EOF
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
        switch (A)
        {
            case Token.NUMBER:
            case Token.IDENTIFIER:
            case Token.STRINGLIT:
            case Token.REM:
            case Token.COMMENT:
            {
                // Save current position as start of literal data
                LDA ZP.TokenizerPosL
                STA ZP.TokenLiteralPosL
                LDA ZP.TokenizerPosH
                STA ZP.TokenLiteralPosH
                skipInlineString();
                LDA ZP.CurrentToken
            }
            case Token.CHARLIT:
            {
                // Save current position as start of character value
                LDA ZP.TokenizerPosL
                STA ZP.TokenLiteralPosL
                LDA ZP.TokenizerPosH
                STA ZP.TokenLiteralPosH
                
                // Skip past the single character value (1 byte)
                incrementTokenizerPos();
                LDA ZP.CurrentToken
            }
        } // switch
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
        LDA ZP.TokenBufferL
        CLC
        ADC ZP.TokenLiteralPosL
        STA ZP.IDXL
        LDA ZP.TokenBufferH
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
                Error.SyntaxError(); BIT ZP.EmulatorPCL
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
                Error.NumericOverflow(); BIT ZP.EmulatorPCL
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
        LDA ZP.TOPH
        if (NZ)
        {
            // Value > 255
            BIT ZP.TOPH          // Check high bit
            if (MI)
            {
                LDA #BASICType.WORD   // Large positive (32768-65535)
                STA ZP.TOPT
            }
            else
            {
                LDA #BASICType.INT    // Medium positive (256-32767)
                STA ZP.TOPT       
            }
        }
        else
        {
            LDA #BASICType.BYTE   // Values 2-255 are BYTE
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
            CMP ZP.TokenBufferContentSizeL
            if (NZ)
            {
                // Not at end - check the byte at current position
                LDA ZP.TokenBufferL
                CLC
                ADC ZP.TokenizerPosL
                STA ZP.IDXL
                LDA ZP.TokenBufferH
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
            CMP ZP.TokenBufferContentSizeH
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
        Statement.IsCaptureModeOff();
        if (C)
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
                    Statement.IsCaptureModeOn();
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
                            CMP # 0x80
                            if (C)  // >= 128, reject
                            {
                                continue;  // Ignore extended ASCII
                            }
                            
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
    
    // Get string literal content from token buffer
    // Input: None (uses current token which must be STRINGLIT)
    // Output: ZP.TOP = pointer to null-terminated string content
    // Preserves: Everything except ZP.TOP
    GetTokenString()
    {
        PHA
        
        // Calculate address of string content in token buffer
        // String content starts at TokenLiteralPos offset in token buffer
        LDA ZP.TokenBufferL
        CLC
        ADC ZP.TokenLiteralPosL
        STA ZP.TOPL
        
        LDA ZP.TokenBufferH
        ADC ZP.TokenLiteralPosH
        STA ZP.TOPH
        
        PLA
    }
    
    // Get string literal content from token buffer
    // Input: None (uses current token which must be STRINGLIT)
    // Output: ZP.STR = pointer to null-terminated string content
    // Preserves: Everything except ZP.STR
    GetTokenStringSTR()
    {
        PHA
        
        // Calculate address of string content in token buffer
        // String content starts at TokenLiteralPos offset in token buffer
        LDA ZP.TokenBufferL
        CLC
        ADC ZP.TokenLiteralPosL
        STA ZP.STRL
        
        LDA ZP.TokenBufferH
        ADC ZP.TokenLiteralPosH
        STA ZP.STRL
        
        PLA
    }
    
    
    Rollback()
    {
        PHA
        PHY
        
        loop
        {
            // Check if we're at the beginning (can't rollback)
            LDA ZP.TokenizerPosL
            ORA ZP.TokenizerPosH
            if (Z)
            {
                // Already at position 0, can't rollback
                Error.CannotRollback(); BIT ZP.EmulatorPCL
                PLY
                PLA
                break;
            }
            
            // Start scanning backwards
            loop
            {
                // Decrement TokenizerPos
                LDA ZP.TokenizerPosL
                if (Z)
                {
                    DEC ZP.TokenizerPosH
                }
                DEC ZP.TokenizerPosL
                
                // Check if we've reached the beginning
                LDA ZP.TokenizerPosL
                ORA ZP.TokenizerPosH
                if (Z)
                {
                    // We're at position 0 - this is the start
                    break;
                }
                
                // Read the byte at current position
                setTokenizerPointer(); // Sets ZP.IDX = BasicTokenizerBuffer + TokenizerPos
                LDY #0
                LDA [ZP.IDX], Y
                
                // Check if high bit is set (it's a token)
                if (MI) { break; } // Branch if minus (bit 7 set) - we found a token!
                // Not a token, continue scanning backwards
            }
            
            // TokenizerPos now points to the previous token
            // Clear CurrentToken to force re-read on next NextToken()
            STZ ZP.CurrentToken
            break;
        }
        
        PLY
        PLA
    }
    
}
