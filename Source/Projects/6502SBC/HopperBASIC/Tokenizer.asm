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
        BufferManager.ResetTokenizerBuffer();
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
        ADC ZP.TokenBufferContentLengthL
        STA ZP.IDXL
        LDA ZP.TokenBufferH
        ADC ZP.TokenBufferContentLengthH
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
    
    // Increment 16-bit TokenBufferContentLength
    // Input: None (uses ZP.TokenBufferContentLength)
    // Output: ZP.TokenBufferContentLength incremented by 1
    // Munts: ZP.TokenBufferContentLength
    incrementTokenBufferContentLength()
    {
        INC ZP.TokenBufferContentLengthL
        if (Z)
        {
            INC ZP.TokenBufferContentLengthH
        }
    }
    
    // Compare 16-bit values - TokenizerPos vs TokenBufferContentSize
    // Input: None (uses ZP.TokenizerPos, ZP.TokenBufferContentSize)
    // Output: Z set if equal, C set if TokenizerPos >= TokenBufferContentSize
    // Preserves: Everything
    CompareTokenizerPosToLength()
    {
        LDA ZP.TokenizerPosH
        CMP ZP.TokenBufferContentLengthH
        if (NZ) { return; }  // Not equal, C flag is correct
        
        // High bytes equal, compare low bytes
        LDA ZP.TokenizerPosL
        CMP ZP.TokenBufferContentLengthL
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
    
    
    
    // Append byte to token buffer using 16-bit addressing
    // Input: A = byte to append
    // Output: C set if successful, NC if buffer full (error set in ZP.LastError)
    // Munts: ZP.TokenBufferContentSize, ZP.IDX, A, Y
    // Error: Sets ZP.LastError if buffer overflow
    appendToTokenBuffer()
    {
        PHA  // Save byte to append
        loop
        {
            
            // 16-bit boundary check
            LDA ZP.TokenBufferContentLengthH
            CMP #(Limits.TokenizerBufferSize >> 8)
            if (C) // ContentSizeH >= LimitH
            {
                if (NZ)  // ContentSizeH > LimitH (not equal)
                {
                    Error.BufferOverflow(); BIT ZP.EmulatorPCL
                    CLC
                    break;
                }
                // High bytes equal, must check low bytes
                LDA ZP.TokenBufferContentLengthL
                CMP #(Limits.TokenizerBufferSize & 0xFF) 
                if (C)  // ContentSizeL >= LimitL  
                {
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
            STA [ZP.IDX]
            
            // Increment 16-bit length
            incrementTokenBufferContentLength();
            SEC // all good
        }
    }
       
    scanHexNumber()
    {
        // Add NUMBER token
        LDA #Token.NUMBER
        appendToTokenBuffer();
        CheckError();
        if (NC) { return; }
        
        // Store "0x" prefix
        LDA #'0'
        appendToTokenBuffer();
        CheckError();
        if (NC) { return; }
        
        LDA Address.BasicInputBuffer, X  // 'x' or 'X'
        appendToTokenBuffer();
        CheckError();
        if (NC) { return; }
        INX
        
        // Scan hex digits
        loop
        {
            CPX ZP.BasicInputLength
            if (Z) { break; }
            
            LDA Address.BasicInputBuffer, X
            Char.IsHex();
            if (NC) { break; }  // Not hex digit
            
            LDA Address.BasicInputBuffer, X
            appendToTokenBuffer();
            CheckError();
            if (NC) { return; }
            INX
        }
        
        // Add null terminator
        LDA #0
        appendToTokenBuffer();
        CheckError();
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
            
            Char.IsHex();
            if (NC)  // Not hex digit
            {
                // Set syntax error and return
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                return;
            }
            
            // Convert hex char to value (0-15)
            LDA [ZP.IDX], Y
            Char.IsDigit();
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
        }
        else
        {
            LDA #BASICType.INT    // 0-32767
        }
        STA ZP.TOPT
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
            BufferManager.ResetTokenizerBuffer();
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
        
        RMB6 ZP.CompilerFlags
        
        SEC
        loop
        {
            if (BBS6, CompilerFlags)
            {
                // common code from the switch cases below
                RMB6 ZP.CompilerFlags
                appendToTokenBuffer();
                CheckError();
                if (NC) { return; }
                INX
            }
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
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case ',':
                {
                    LDA #Token.COMMA
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case ';':
                {
                    LDA #Token.SEMICOLON
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '=':
                {
                    LDA #Token.EQUALS
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '+':
                {
                    LDA #Token.PLUS
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '-':
                {
                    LDA #Token.MINUS
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '*':
                {
                    LDA #Token.MULTIPLY
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '/':
                {
                    LDA #Token.DIVIDE
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '&':
                {
                    LDA #Token.BITWISE_AND
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '|':
                {
                    LDA #Token.BITWISE_OR
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '(':
                {
                    LDA #Token.LPAREN
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case ')':
                {
                    LDA #Token.RPAREN
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case '[':
                {
                    LDA #Token.LBRACKET
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
                }
                case ']':
                {
                    LDA #Token.RBRACKET
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX
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
                            SMB6 ZP.CompilerFlags
                            //appendToTokenBuffer();
                            //CheckError();
                            //if (NC) { return; }
                            //INX  // Skip both '<' and '='
                            continue;
                        }
                        CMP #'>'
                        if (Z)
                        {
                            LDA #Token.NOTEQUAL
                            SMB6 ZP.CompilerFlags
                            //appendToTokenBuffer();
                            //CheckError();
                            //if (NC) { return; }
                            //INX  // Skip both '<' and '>'
                            continue;
                        }
                    }
                    // Just '<'
                    DEX  // Back up to point at '<'
                    LDA #Token.LT
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX  // Move past '<'
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
                            SMB6 ZP.CompilerFlags
                            //appendToTokenBuffer();
                            //CheckError();
                            //if (NC) { return; }
                            //INX  // Skip both '>' and '='
                            continue;
                        }
                    }
                    // Just '>'
                    DEX  // Back up to point at '>'
                    LDA #Token.GT
                    SMB6 ZP.CompilerFlags
                    //appendToTokenBuffer();
                    //CheckError();
                    //if (NC) { return; }
                    //INX  // Move past '>'
                }
                case '"':
                {
                    // String literal tokenization
                    LDA # Token.STRINGLIT
                    appendToTokenBuffer();
                    CheckError();
                    if (NC) { return; }
                    
                    // Store starting position of string content
                    LDA ZP.TokenBufferContentLengthL
                    STA ZP.TokenLiteralPosL
                    LDA ZP.TokenBufferContentLengthH
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
                            CheckError();
                            if (NC) { return; }
                            
                            INX  // Skip closing quote in input buffer
                            break;
                        }
                        
                        // Add character to string content in token buffer
                        appendToTokenBuffer();
                        CheckError();
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
                    CheckError();
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
                    CheckError();
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
                    // EXCEPTION
                }
                case '!':
                {
                    // Single quote comment
                    LDA #Token.COMMENT
                    appendToTokenBuffer();
                    CheckError();
                    if (NC) { return; }
                    
                    INX  // Skip the quote character
                    
                    // Store comment text as inline data
                    loop
                    {
                        CPX ZP.BasicInputLength
                        if (Z) { break; }  // End of input
                        
                        LDA Address.BasicInputBuffer, X
                        appendToTokenBuffer();
                        CheckError();
                        if (NC) { return; }
                        INX
                    }
                    
                    // Add null terminator
                    LDA #0
                    appendToTokenBuffer();
                    CheckError();
                    if (NC) { return; }
                    
                    // X is already at end of input, continue will break out of main loop
                    // EXCEPTION
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
                            if (Z) { scanHexNumber(); continue; } // EXCEPTION
                            CMP #'X'  
                            if (Z) { scanHexNumber(); continue; } // EXCEPTION
                        }
                        DEX  // Back up, not hex
                        LDA Address.BasicInputBuffer, X
                    }
                    
                    // Check if it's a decimal number
                    Char.IsDigit();
                    if (C)  
                    {
                        // Scan number and store inline in token buffer
                        LDA #Token.NUMBER
                        appendToTokenBuffer();
                        CheckError();
                        if (NC) { return; }
                        
                        // Store number digits inline
                        loop
                        {
                            CPX ZP.BasicInputLength
                            if (Z) { break; }
                            
                            LDA Address.BasicInputBuffer, X
                            Char.IsDigit();
                            if (NC) { SEC break; }  // Not a digit
                            
                            appendToTokenBuffer();
                            CheckError();
                            if (NC) { return; }
                            INX
                        }
                        
                        // Add null terminator for number
                        LDA #0
                        appendToTokenBuffer();
                        CheckError();
                        if (NC) { return; }
                        continue;
                    }
                    // Must be an identifier or keyword
                    Char.IsAlpha();
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
                        
                        Char.IsAlphaNumeric();
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
                        CPY #Limits.BasicProcessBufferSize
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
                            CheckError();
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
                                CheckError();
                                if (NC) { return; }
                                INX
                            }
                            
                            // Add null terminator
                            LDA #0
                            appendToTokenBuffer();
                            CheckError();
                            if (NC) { return; }
                            
                            // X is already at end of input, continue will break out of main loop
                            continue; // EXCEPTION
                        }
                        
                        // Regular keyword processing
                        appendToTokenBuffer();
                        CheckError();
                        if (NC) { return; }
                    }
                    else
                    {
                        // It's an identifier - store token + inline string
                        LDA #Token.IDENTIFIER
                        appendToTokenBuffer();
                        CheckError();
                        if (NC) { return; }
                        
                        // Copy identifier from working buffer to token buffer
                        LDY #0  // Reset Y for copying
                        loop
                        {
                            LDA Address.BasicProcessBuffer, Y
                            STA ZP.ACCL 
                            appendToTokenBuffer();
                            CheckError();
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
                    // EXCEPTION
                } // default
            } // switch
        } // loop
        
        // Add EOL token only for lines with content
        LDA #Token.EOL
        appendToTokenBuffer();
        CheckError();
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
            case Token.REM:
            case Token.COMMENT:
            {
                // Don't munt previous literal data start
                skipInlineString();
                LDA ZP.CurrentToken
            }
            
        } // switch
    }
    
    // Peek at next token without advancing tokenizer position
    // Input: Current tokenizer state (ZP.TokenizerPos already points to next token)
    // Output: A = next token type (just the token ID byte)
    // Munts: ZP.IDX, A, Y
    // Preserves: All tokenizer state (ZP.TokenizerPos, ZP.CurrentToken, etc.)
    PeekToken()
    {
        // Check if we're already at end of buffer
        CompareTokenizerPosToLength();
        if (C)  // TokenizerPos >= TokenBufferContentSize
        {
            LDA #Token.EOF
            return;
        }
        
        PHY
        
        // TokenizerPos already points to the next token
        // Just read the byte at that position
        CLC
        LDA ZP.TokenBufferL
        ADC ZP.TokenizerPosL
        STA ZP.IDXL
        LDA ZP.TokenBufferH
        ADC ZP.TokenizerPosH
        STA ZP.IDXH
        
        LDY #0
        LDA [ZP.IDX], Y  // Load the next token ID
        
        PLY
        
        // Return it in A
    }    
    
    
    // Get current token as number (assumes current token is NUMBER)
    // Input: ZP.TokenLiteralPos = position of number string in token buffer
    // Output: ZP.TOP = number value, ZP.TOPT = determined type (BYTE/INT/WORD/LONG)
    //         For LONG: ZP.TOP0-3 contains full 32-bit value
    //         For others: ZP.TOP0-1 contains 16-bit value, ZP.TOP2-3 cleared
    // Munts: ZP.TOP0-3, ZP.TOPT, ZP.IDX, ZP.NEXT0-3, ZP.RESULT0-7, A, Y
    // Error: Sets ZP.LastError if number is invalid or overflows 32-bit
    GetTokenNumber()
    {
        // Initialize to zero
        STZ ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        
        // Set up 16-bit pointer to saved literal position in token buffer
        LDA ZP.TokenBufferL
        CLC
        ADC ZP.TokenLiteralPosL
        STA ZP.IDXL
        LDA ZP.TokenBufferH
        ADC ZP.TokenLiteralPosH
        STA ZP.IDXH
        
//LDA ZP.IDXL
//STA ZP.STRL        
//LDA ZP.IDXH
//STA ZP.STRH
//Debug.NL(); Print.String();
                
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
            Char.IsDigit();
            if (NC) { break; }  // Not a digit
            
            // Convert ASCII to digit value (0-9)
            LDA [ZP.IDX], Y
            SEC
            SBC #'0'
            PHA  // Save digit
            
            // Multiply current value by 10
            Long.MultiplyBy10();
            if (NC)  // 32-bit overflow
            {
                PLA  // Clean up stack
                Error.NumericOverflow(); BIT ZP.EmulatorPCL
                return;
            }
            
            // Add the digit
            PLA  // Restore digit (0-9)
            Long.AddDigit();
            if (NC)  // 32-bit overflow or invalid digit
            {
                Error.NumericOverflow(); BIT ZP.EmulatorPCL
                return;
            }
            INY
        }
        
        LDA ZP.TOP3
        ORA ZP.TOP2
        if (NZ)
        {
            // Value requires LONG (65536-4294967295)
            LDA #BASICType.LONG
            STA ZP.TOPT
        }
        else
        {
            LDA ZP.TOP1
            if (NZ)
            {
                // Value > 255
                BIT ZP.TOP1          // Check high bit
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
                LDA #BASICType.BYTE   // Values 0-255 are BYTE
                STA ZP.TOPT
            }
        }
    }    
    
    // Skip past null-terminated string at current tokenizer position
    // Input: ZP.TokenizerPos = current position in token buffer
    // Output: ZP.TokenizerPos advanced past null terminator, C set on success, NC on error
    // Munts: ZP.TokenizerPos, ZP.IDX, A, Y
    skipInlineString()
    {
        loop
        {
            // Check if we're at or past end of token buffer
            CompareTokenizerPosToLength();  // Proper 16-bit comparison
            if (C)  // TokenizerPos >= TokenBufferContentSize
            {
                // At or past end of buffer - error
                CLC  // Error flag
                break;
            }
            
            // Calculate pointer to current position
            LDA ZP.TokenBufferL
            CLC
            ADC ZP.TokenizerPosL
            STA ZP.IDXL
            LDA ZP.TokenBufferH
            ADC ZP.TokenizerPosH
            STA ZP.IDXH
            
            // Read the character at current position
            LDY #0
            LDA [ZP.IDX], Y
            PHA  // Save the character
            
            // Advance position
            INC ZP.TokenizerPosL
            if (Z)
            {
                INC ZP.TokenizerPosH
            }
            
            // Check if we found the null terminator
            PLA  // Restore the character
            if (Z) 
            { 
                SEC  // Success - found null terminator
                break;
            }
            
            // Not null, continue scanning
        }// single exit
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
            Print.Space();
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
                case 0x09:  // Tab key
                {
                    // Check if we have room for 4 spaces
                    CPX #(Limits.BasicInputSize - 4)
                    if (C) { continue; }  // Not enough space, ignore tab
                    
                    // Insert 4 spaces into buffer and echo them
                    LDY #4
                    loop
                    {
                        LDA #' '
                        STA Address.BasicInputBuffer, X
                        Serial.WriteChar();  // Echo space
                        INX
                        DEY
                        if (Z) { break; }
                    }
                    continue;  // Continue reading input
                }
                
                case 0x08:  // Backspace
                case 0x7F:  // Delete
                {
                    CPX #0
                    if (Z) { continue; }  // Nothing to delete
                    
                    DEX
                    LDA #0x08   // Backspace
                    Serial.WriteChar();
                    Print.Space();
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
                            
                            CPX #Limits.BasicInputSize
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
                Error.InternalError(); BIT ZP.EmulatorPCL
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
