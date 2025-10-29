unit Parser
{
    uses "Buffer"
    uses "Symbols"
    uses "Labels"
    uses "ForwardRefs"
    uses "OpCodes"
    uses "../System/Char"
    
    // Single base for easy relocation
    const byte parserSlots = 0x70; // 0x70..0x7F
    
    const byte parserFlags    = parserSlots+0;
    // Bit 0    - .MAIN seen
    // Bit 1    - .CONST seen
    // Bit 2    - .DATA seen
    // Bit 3    - .FUNC or .MAIN seen
    
    // Bit 7 - token pushed back
    
    
    const byte bufferIndexL = parserSlots+1;
    const byte bufferIndexH = parserSlots+2;
    
    const byte currentChar  = parserSlots+3;  // Current character
    const byte tokenType    = parserSlots+4;  // Type of current token
    const byte tokenValueL  = parserSlots+5;  // Token value/pointer
    const byte tokenValueH  = parserSlots+6;
    const byte tokenLength  = parserSlots+7;  // Length of identifier/string
    
    // Token buffer for identifiers/strings
    const byte tokenBuffer  = parserSlots+8;  // Pointer to allocated buffer
    const byte tokenBufferL = parserSlots+8;
    const byte tokenBufferH = parserSlots+9;
    
    const byte currentLineL = parserSlots+10;
    const byte currentLineH = parserSlots+11;
    
    const byte parserSection = parserSlots+12;
    
    const byte numberType    = parserSlots+13;
    
    const byte currentFunctionID = parserSlots+14;
    
    const byte constantWidth     = parserSlots+15; // 0 - auto-size, 1 = bytes, 2 = words
    
    const string errDirectiveExpected  = ".DATA, .CONST, .MAIN or .FUNC expected";
    const string errMAINRequired       = ".MAIN required";
    const string errMAINSeen           = ".MAIN already seen";
    const string errFUNCSeen           = ".DATA not allowed after functions";
    const string errIdentifierExpected = "Identifier expected";
    const string errNumberExpected     = "Number expected";
    const string errCharExpected       = "Char expected";
    const string errByteExpected       = "Byte expected";
    const string errWordExpected       = "Word expected";
    const string errIntExpected        = "Int expected";
    const string errValueExpected      = "Value expected";
    const string errSymbolAddFailed    = "Failed to create new symbol";
    const string errLabelAddFailed     = "Failed to create new label";
    const string errUndefinedSymbol    = "Undefined symbol";
    const string errOpCodeExpected     = "OpCode expected";
    const string errFunctionTooLarge   = "Function exceeds 256 byte limit";
    
    const string msgOnLine             = " on line ";
    
    const string mainName = "MAIN";
    
    // Token types
    enum TokenType
    {
        EOF        = 0,
        Directive  = 1,   // .CONST, .DATA, .MAIN, .FUNC
        Identifier = 2,   // Label or instruction
        Number     = 3,
        String     = 4,   // String literal
        Colon      = 5,   // :
        Comma      = 6,   // ,
        NewLine    = 7,   // End of line
    }
    flags NumberType
    {
        Byte   = 0b0001,  // 0..255
        Word   = 0b0011,  // 0..65535
        Char   = 0b0100,  // -128..127  
        Int    = 0b1100,  // -32768..32767
    }
    
    enum Section
    {
        None,
        Const,
        Data,
        Func
    }
    
    // Refill buffer from file
    refillBuffer()
    {
        File.NextStream();
        if (NC)
        {
            CLC
            return;
        }
        STZ bufferIndexL
        STZ bufferIndexH
        SEC
    }
    
    ErrorLineValue()
    {
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        LDA #'0'
        Print.Char();
        LDA #'x'
        Print.Char();
        LDA tokenValueH
        Print.Hex();
        LDA tokenValueL
        Print.Hex();
        
        Print.Space();
        LDA #'-'
        Print.Char();
        Print.Space();
        
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        ErrorLine();
    }                
    
    ErrorLineSTR()
    {
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        LDA #'"'
        Print.Char();
        
        LDA tokenBufferL
        STA STRL
        LDA tokenBufferH
        STA STRH
        Print.String();
        
        LDA #'"'
        Print.Char();
        Print.Space();
        LDA #'-'
        Print.Char();
        Print.Space();
        
        
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        ErrorLine();
    }
    
    ErrorLine()
    {
        Print.String();
        
        LDA # (msgOnLine % 256)
        STA ZP.STRL
        LDA # (msgOnLine / 256)
        STA ZP.STRH
        Print.String();
        
        LDA currentLineL
        STA ZP.TOP0
        LDA currentLineH
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
        Print.NewLine();
        CLC
    }
    
    ErrorFunctionTooLarge()
    {
        LDA ZP.TOP0
        Print.Hex();
        Print.Space();
        // size not ok
        LDA #(errFunctionTooLarge / 256)
        STA ZP.STRH
        LDA #(errFunctionTooLarge % 256)
        STA ZP.STRL
        ErrorLine(); // does CLC
    }
    
    // Get next character -> A, store in currentChar
    next()
    {
        PHY
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
                    LDA #0  // EOF
                    STA currentChar
                    PLY
                    return;
                }
            }
        }
        
        // Get character from buffer
        LDA #(File.FileDataBuffer % 256)
        STA ZP.IDXL
        LDA #(File.FileDataBuffer / 256)
        STA ZP.IDXH
        
        LDY bufferIndexL
        LDA [ZP.IDX], Y
        STA currentChar
        
        INC bufferIndexL
        if (Z) { INC bufferIndexH }
        PLY
    }
    
    // Peek at current character without advancing
    peek()
    {
        LDA currentChar
    }
    
    // Skip whitespace (except newline)
    skipWhitespace()
    {
        loop
        {
            peek();
            CMP # ' '
            if (Z) { next(); continue; }
            CMP # Char.Tab
            if (Z) { next(); continue; }
            CMP # Char.CR
            if (Z) { next(); continue; }  // Skip CR, keep LF
            break;
        }
    }
    
    // Skip comment to end of line
    skipComment()
    {
        loop
        {
            peek();
            if (Z) { break; }      // EOF
            CMP # Char.EOL
            if (Z) { break; }      // Don't consume newline
            next();
        }
    }
    
    // Read identifier or keyword into token buffer
    readIdentifier()
    {
        STZ tokenLength
        LDY #0
        
        loop
        {
            peek();
            Char.IsAlphaNumeric();
            if (NC)
            {
                CMP #'_'
                if (NZ)
                {
                    CMP #'.'
                    if (NZ) { break; }
                }
            }
            
            // Store character in buffer
            LDA currentChar
            STA [tokenBuffer], Y
            next();
            INY
            CPY # 63  // Max identifier length
            if (Z) { break; }
        }
        
        // Null terminate
        LDA #0
        STA [tokenBuffer], Y
        STY tokenLength
        
        // Check if it's a directive (starts with .)
        LDY #0
        LDA [tokenBuffer], Y
        CMP #'.'
        if (Z)
        {
            LDA #TokenType.Directive
            STA tokenType
        }
        else
        {
            LDA #TokenType.Identifier
            STA tokenType
        }
    }
    
    // Read string literal
    readString()
    {
        next();  // Skip opening quote
        STZ tokenLength
        LDY #0
        
        loop
        {
            peek();
            if (Z) { break; }       // EOF - error!
            CMP #'"'
            if (Z) 
            { 
                next();  // Consume closing quote
                break; 
            }
            
            // Handle escape sequences
            CMP #'\\'
            if (Z)
            {
                next();
                peek();
                switch (A)
                {
                    case 'n':  { LDA # Char.EOL }
                    case 'r':  { LDA # Char.CR  }
                    case 't':  { LDA # Char.Tab  }
                    case '\\': {   } // use \
                    case '"':  {   } // use "
                    default:   {   } // else use character as-is
                }
                
            }
            
            // Store character
            STA [tokenBuffer], Y
            next();
            INY
            CPY #255  // Max string length
            if (Z) { break; }
        }
        
        // Null terminate
        LDA #0
        STA [tokenBuffer], Y
        STY tokenLength
        
        LDA # TokenType.String
        STA tokenType
    }   
    
    
    // Read number (decimal, hex, or char literal)
    readNumber()
    {
        STZ tokenValueL
        STZ tokenValueH
        
        peek();
        CMP #'\''
        if (Z)  // Character literal
        {
            next();
            peek();
            STA tokenValueL
            next();  // Should be closing '
            next();
            
            LDA # NumberType.Char    
#ifdef UNIVERSAL            
            BIT tokenValueL
            if (PL)
            {
                LDA # (NumberType.Char | NumberType.Byte)
            }
#else
            if (BBR7, tokenValueL)
            {
                LDA # (NumberType.Char | NumberType.Byte)
            }
#endif            
            STA numberType
            LDA #TokenType.Number
            STA tokenType
            return;
        }
        
        // Check for hex prefix
        CMP #'0'
        if (Z)
        {
            next();
            peek();
            CMP #'x'
            if (Z)
            {
                next();
                readHexNumber();
                return;
            }
            // Was just 0
            LDA # (NumberType.Byte | NumberType.Char)
            STA numberType
            LDA #TokenType.Number
            STA tokenType
            return;
        }
        
        // Decimal number
        loop
        {
            peek();
            Char.IsDigit();
            if (NC) { break; }
            
            // Multiply current value by 10
            // (Simple version - doesn't handle overflow)
            ASL tokenValueL
            ROL tokenValueH  // *2
            LDA tokenValueL
            STA ZP.IDXL
            LDA tokenValueH
            STA ZP.IDXH      // Save *2
            
            ASL tokenValueL
            ROL tokenValueH  // *4
            ASL tokenValueL
            ROL tokenValueH  // *8
            
            CLC
            LDA tokenValueL
            ADC ZP.IDXL
            STA tokenValueL
            LDA tokenValueH
            ADC ZP.IDXH
            STA tokenValueH  // *10
            
            // Add digit
            LDA currentChar
            SEC
            SBC #'0'
            CLC
            ADC tokenValueL
            STA tokenValueL
            if (C) { INC tokenValueH }
            
            next();
        }
        LDA tokenValueH
        if (Z)
        {
            LDA tokenValueL
            if (MI)
            {
                LDA # NumberType.Byte // 128..255
            }
            else
            {
                LDA # (NumberType.Byte | NumberType.Char)  // 1..127
            }
        }
        else
        {
            if (MI)
            {
                LDA # NumberType.Word                    // 32768..65535
            }
            else
            {
                LDA # (NumberType.Word | NumberType.Int) // 128..32767
            }
        }
        STA numberType
        
        LDA #TokenType.Number
        STA tokenType
    }
    
    // Read hex number (after 0x)
    readHexNumber()
    {
        LDX #0
        loop
        {
            PHX
            peek();
            PLX
            Char.IsHex();
            if (NC) { break; }
            INX
            
            // Shift left 4 bits
            ASL tokenValueL
            ROL tokenValueH
            ASL tokenValueL
            ROL tokenValueH
            ASL tokenValueL
            ROL tokenValueH
            ASL tokenValueL
            ROL tokenValueH
            
            // Convert hex digit
            LDA currentChar
            CMP #'A'
            if (C)  // A-F
            {
                SEC
                SBC #('A' - 10)
            }
            else
            {
                CMP #'a'
                if (C)  // a-f
                {
                    SEC
                    SBC #('a' - 10)
                }
                else  // 0-9
                {
                    SEC
                    SBC #'0'
                }
            }
            
            // Add to value
            ORA tokenValueL
            STA tokenValueL
       
            PHX     
            next();
            PLX
        }
        CPX #3
        if (C) // >= 3 digits?
        {
            LDA #NumberType.Word 
        }
        else
        {
            LDA # (NumberType.Byte | NumberType.Char)
        }
        STA numberType
        
        LDA #TokenType.Number
        STA tokenType
    } 
    
    
    Initialize()
    {
        // Allocate token buffer
        LDA #0
        STA ZP.ACCL
        LDA #1  // 256 bytes
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        { 
            STZ tokenBufferL
            STZ tokenBufferH
            return;
        }
        
        LDA ZP.IDXL
        STA tokenBufferL
        LDA ZP.IDXH
        STA tokenBufferH
        
        STZ parserSection // no section
        
        STZ parserFlags
        
        // Get first character
        refillBuffer();
        if (C)
        {
            next();
        }
        
        LDA #1
        STA currentLineL
        STZ currentLineH
        
        Symbols.Initialize();
        Labels.Initialize();
        ForwardRefs.Initialize();
    }
    Dispose()
    {
        LDA tokenBufferL
        ORA tokenBufferH
        if (NZ)
        {
            LDA tokenBufferL
            STA ZP.IDXL
            LDA tokenBufferH
            STA ZP.IDXH
            Memory.Free();
        }
        
        Symbols.Dispose();
        Labels.Dispose();
        ForwardRefs.Dispose();
    }
    
    UngetToken()
    {
#ifdef UNIVERSAL
        PHA
        LDA #0b10000000
        ORA parserFlags
        STA parserFlags
        PLA
#else        
        SMB7 parserFlags        // Set bit 7 = reuse token next time
#endif
    }
    
    // Get next token
    GetToken()
    {
        // Check if token was ungot
#ifdef UNIVERSAL
        BIT parserFlags
        if (MI)  // Bit 7 set = reuse current token
        {
            LDA #0b01111111
            AND parserFlags
            STA parserFlags      // Clear the flag
            return;              // tokenType, tokenValue, tokenBuffer unchanged
        }
#else        
        if (BBS7, parserFlags)  // Bit 7 set = reuse current token
        {
            RMB7 parserFlags     // Clear the flag
            return;              // tokenType, tokenValue, tokenBuffer unchanged
        }
#endif
        skipWhitespace();
        
        peek();
        if (Z)  // EOF
        {
            LDA #TokenType.EOF
            STA tokenType
            return;
        }
        switch (A)
        {
            case ';': // Check for comment
            {
                skipComment();
                GetToken();  // Recurse to get next real token
            }
            case Char.EOL: // Checkfor newline
            {
                next();
                INC currentLineL if (Z) { INC currentLineH }
                LDA #TokenType.NewLine
                STA tokenType
            }
            case '"': // Check for string
            {
                readString();
            }
            case ':': // Check for colon
            {
                next();
                LDA #TokenType.Colon
                STA tokenType
            }
            case ',': // Check for comma
            {
                next();
                LDA #TokenType.Comma
                STA tokenType
            }
            default:
            {
                loop
                {
                    // Check for number
                    Char.IsDigit();
                    if (C)
                    {
                        readNumber();
                        break;
                    }
                
                    // Check for char literal or hex
                    CMP #'''
                    if (Z)
                    {
                        readNumber();
                        break;
                    }
                    
                    // Check for negative number
                    CMP #'-'
                    if (Z)
                    {
                        next();
                        readNumber();
                        // Negate the value
                        SEC
                        LDA #0
                        SBC tokenValueL
                        STA tokenValueL
                        LDA #0
                        SBC tokenValueH
                        STA tokenValueH
                        
                        LDA tokenValueH
                        CMP # 0xFF
                        if (Z)
                        {
                            LDA # NumberType.Char // -128..-1
                        }
                        else
                        {
                            LDA # NumberType.Int // -32768..-129
                        }
                        STA numberType
                        
                        LDA #TokenType.Number
                        STA tokenType
                        break;
                    }
                    
                    // Must be identifier/directive/instruction
                    readIdentifier();
                    break;
                }
        
            } // default
        } // switch
    }
    
    Parse()
    {
        
        loop
        {
            GetToken();

#ifdef DEBUG
            Print.NewLine();                        
            LDA tokenType
            Print.Hex(); Print.Space();
            LDA tokenType
            switch (A)
            {
                case TokenType.Identifier:
                case TokenType.Directive:
                case TokenType.String:
                {
                    LDA tokenBufferL
                    STA STRL
                    LDA tokenBufferH
                    STA STRH
                    Print.String();
                }
                case TokenType.Number:
                {
                    LDA tokenValueH
                    Print.Hex();
                    LDA tokenValueL
                    Print.Hex();
                }
            }
#endif

            LDA tokenType
            if (Z) { SEC break; }  // EOF
            
            CMP # TokenType.NewLine
            if (Z)
            {
                continue; // ignore newline
            }
            CMP # TokenType.Directive
            if (Z)
            {
                // switch modes
                LDY #1
                LDA [tokenBuffer], Y
                switch (A)
                {
                    case 'C': // .CONST
                    {
                        // parsing .CONST lines now  
                        LDA # Section.Const
                        STA parserSection 
                    }
                    case 'D': // .DATA
                    {
#ifdef UNIVERSAL
                        LDA parserFlags
                        AND #0b00001000 // Bit 3 - .MAIN or .FUNC seen
                        if (NZ)
                        {
                            LDA #(errFUNCSeen / 256) STA ZP.STRH LDA #(errFUNCSeen % 256) STA ZP.STRL
                            ErrorLine();    
                            break;
                        }
#else                        
                        if (BBS3, parserFlags) // Bit 3 - .MAIN or .FUNC seen
                        {
                            LDA #(errFUNCSeen / 256) STA ZP.STRH LDA #(errFUNCSeen % 256) STA ZP.STRL
                            ErrorLine();    
                            break;
                        }
#endif                        
                        
                        // parsing .DATA lines now    
                        LDA # Section.Data
                        STA parserSection
                    }
                    case 'M': // .MAIN
                    {
#ifdef UNIVERSAL
                        LDA parserFlags
                        AND #0b00001000
                        if (NZ) // we were in a function
                        {
                            LDA currentFunctionID
                            STA ZP.TOP0
                            Buffer.CaptureFunctionEnd(); if (NC) { break; }
                            ForwardRefs.Dispose(); if (NC) { break; } // Check for unresolved refs
                            LDA #0b11110111
                            AND parserFlags
                            STA parserFlags
                        }
                        LDA parserFlags
                        AND #0b00000001
                        if (NZ) // Bit 0 - .MAIN seen
                        {
                            LDA #(errMAINSeen / 256) STA ZP.STRH LDA #(errMAINSeen % 256) STA ZP.STRL
                            ErrorLine();    
                            break;
                        }
                        LDA #0b00001001  // .MAIN seen, .MAIN or .FUNC seen
                        ORA parserFlags 
                        STA parserFlags
#else                        
                        if (BBS3, parserFlags) // we were in a function
                        {
                            LDA currentFunctionID
                            STA ZP.TOP0
                            Buffer.CaptureFunctionEnd(); if (NC) { break; }
                            ForwardRefs.Dispose(); if (NC) { break; } // Check for unresolved refs
                            RMB3 parserFlags
                        }
                        if (BBS0, parserFlags) // Bit 0 - .MAIN seen
                        {
                            LDA #(errMAINSeen / 256) STA ZP.STRH LDA #(errMAINSeen % 256) STA ZP.STRL
                            ErrorLine();    
                            break;
                        }
                        SMB0 parserFlags // .MAIN seen
                        SMB3 parserFlags // .MAIN or .FUNC seen
#endif
                        
                        
                        LDA # (mainName % 256)
                        STA STRL
                        LDA # (mainName / 256)
                        STA STRH
                        
                        // always function 2
                        LDA #0x02                        
                        STA TOP0
                        STA currentFunctionID
                        STZ TOP1
                        
                        Buffer.CaptureFunctionStart();
                        
                        LDA # ( SymbolType.Function | NumberType.Byte)
                        // STR = name, TOP = ID
                        Symbols.Add(); // STR ->
                        if (NC)
                        {
                            LDA #(errSymbolAddFailed / 256) STA ZP.STRH LDA #(errSymbolAddFailed % 256) STA ZP.STRL
                            ErrorLine();    
                            break;
                        }
                        
                        Labels.Dispose(); // reset labels
                        
                        // parsing function opcodes now
                        LDA # Section.Func
                        STA parserSection
                    }
                    case 'F': // .FUNC
                    {
#ifdef UNIVERSAL         
                        LDA parserFlags            
                        AND #0b00001000
                        if (NZ) // we were in a function
                        {
                            LDA currentFunctionID
                            STA ZP.TOP0
                            Buffer.CaptureFunctionEnd(); if (NC) { break; }
                            ForwardRefs.Dispose(); if (NC) { break; } // Check for unresolved refs
                            LDA #0b11110111
                            AND parserFlags
                            STA parserFlags
                        }
#else
                        if (BBS3, parserFlags) // we were in a function
                        {
                            LDA currentFunctionID
                            STA ZP.TOP0
                            Buffer.CaptureFunctionEnd(); if (NC) { break; }
                            ForwardRefs.Dispose(); if (NC) { break; } // Check for unresolved refs
                            RMB3 parserFlags
                        }
#endif
                        GetToken();
                        
                        LDA tokenType
                        CMP # TokenType.Identifier
                        if (NZ)
                        {
                            LDA #(errIdentifierExpected / 256) STA ZP.STRH LDA #(errIdentifierExpected % 256) STA ZP.STRL
                            ErrorLine();    
                            break;
                        }
#ifdef UNIVERSAL
                        LDA #0b00001000
                        ORA parserFlags
                        STA parserFlags
#else                       
                        SMB3 parserFlags // .MAIN or .FUNC seen
#endif                        
                        Buffer.GetNextFunctionNumber(); // -> TOP
                        LDA ZP.TOP0
                        STA currentFunctionID
                        Buffer.CaptureFunctionStart();  // TOP ->
                        
                        LDA tokenBufferL
                        STA STRL
                        LDA tokenBufferH
                        STA STRH
                        
                        LDA # ( SymbolType.Function | NumberType.Byte)
                        Symbols.Add(); // STR ->
                        if (NC)
                        {
                            LDA #(errSymbolAddFailed / 256) STA ZP.STRH LDA #(errSymbolAddFailed % 256) STA ZP.STRL
                            ErrorLineSTR();    
                            break;
                        }
                        
                        Labels.Dispose(); // reset labels
                                                // parsing function opcodes now    
                        LDA # Section.Func
                        STA parserSection
                    }
                }
            }
            else
            {
                LDX parserSection
                switch (X)
                {
                    case Section.Const:
                    {
                        parseConstLine(); if (NC) { break; }
                    }
                    case Section.Data:
                    {
                        parseDataLine(); if (NC) { break; }
                    }
                    case Section.Func:
                    {
                        parseFuncLine(); if (NC) { break; }
                    }
                }
            }
        } // loop
        if (C)
        {
#ifdef UNIVERSAL
            LDA parserFlags
            AND #0b00001000
            if (NZ)
            {
                LDA currentFunctionID
                STA ZP.TOP0
                Buffer.CaptureFunctionEnd();
                ForwardRefs.Dispose(); // Check for unresolved refs
                LDA #0b11110111
                AND parserFlags
                STA parserFlags
            }
#else            
            if (BBS3, parserFlags) // we were in a function
            {
                LDA currentFunctionID
                STA ZP.TOP0
                Buffer.CaptureFunctionEnd();
                ForwardRefs.Dispose(); // Check for unresolved refs
                RMB3 parserFlags
            }
#endif
            if (C)
            {
#ifdef UNIVERSAL
                LDA parserFlags
                AND #0b00000001
                if (Z) // Bit 0 - .MAIN seen
                {
                    LDA #(errMAINRequired / 256) STA ZP.STRH LDA #(errMAINRequired % 256) STA ZP.STRL
                    ErrorLine();    
                }
#else                
                if (BBR0, parserFlags) // Bit 0 - .MAIN seen
                {
                    LDA #(errMAINRequired / 256) STA ZP.STRH LDA #(errMAINRequired % 256) STA ZP.STRL
                    ErrorLine();    
                }
#endif                
            }
        }
#ifdef DEBUG        
        PHP Symbols.Dump(); PLP
        PHP Labels.Dump(); PLP
#endif
    }
    
    parseConstLine()
    {
        LDA tokenType
        CMP # TokenType.Identifier
        if (NZ)
        {
            LDA #(errIdentifierExpected / 256) STA ZP.STRH LDA #(errIdentifierExpected % 256) STA ZP.STRL
            ErrorLine();    
            return;
        }
        
        GetToken();
        
        LDA tokenType
        CMP # TokenType.Number
        if (NZ)
        {
            LDA #(errNumberExpected / 256) STA ZP.STRH LDA #(errNumberExpected % 256) STA ZP.STRL
            ErrorLine();    
            return;
        }
        
        LDA tokenBufferL
        STA STRL
        LDA tokenBufferH
        STA STRH
        
        LDA tokenValueH
        STA ZP.TOP1
        LDA tokenValueL
        STA ZP.TOP0
        
        LDA numberType
        ORA # SymbolType.Constant
        Symbols.Add();
        if (NC)
        {
            LDA #(errSymbolAddFailed / 256) STA ZP.STRH LDA #(errSymbolAddFailed % 256) STA ZP.STRL
            ErrorLine();
            return;
        }
        SEC
    }
    parseDataLine()
    {
        // syntax :  name value, value, value
        LDA tokenType
        CMP # TokenType.Identifier
        if (NZ)
        {
            LDA #(errIdentifierExpected / 256) STA ZP.STRH LDA #(errIdentifierExpected % 256) STA ZP.STRL
            ErrorLine();    
            return;
        }
        
        // Default to auto-size
        STZ constantWidth
        
        // Check if this identifier is a size prefix ("byte" or "word")
        LDA tokenLength
        CMP #4
        if (Z)  // Both "byte" and "word" are 4 characters
        {
            LDY #0
            LDA [tokenBuffer], Y
            
            // Check for "byte"
            CMP #'b'
            if (Z)
            {
                INY
                LDA [tokenBuffer], Y
                CMP #'y'
                if (Z)
                {
                    INY
                    LDA [tokenBuffer], Y
                    CMP #'t'
                    if (Z)
                    {
                        INY
                        LDA [tokenBuffer], Y
                        CMP #'e'
                        if (Z)
                        {
                            // Found "byte" prefix
                            INC constantWidth  // constantWidth = 1
                            GetToken();
                            LDA tokenType
                            CMP # TokenType.Identifier
                            if (NZ)
                            {
                                LDA #(errIdentifierExpected / 256) STA ZP.STRH LDA #(errIdentifierExpected % 256) STA ZP.STRL
                                ErrorLine();    
                                return;
                            }
                        }
                    }
                }
            }
            
            // Check for "word" (only if constantWidth still 0)
            LDA constantWidth
            if (Z)
            {
                LDY #0
                LDA [tokenBuffer], Y
                CMP #'w'
                if (Z)
                {
                    INY
                    LDA [tokenBuffer], Y
                    CMP #'o'
                    if (Z)
                    {
                        INY
                        LDA [tokenBuffer], Y
                        CMP #'r'
                        if (Z)
                        {
                            INY
                            LDA [tokenBuffer], Y
                            CMP #'d'
                            if (Z)
                            {
                                // Found "word" prefix
                                LDA #2
                                STA constantWidth
                                GetToken();
                                LDA tokenType
                                CMP # TokenType.Identifier
                                if (NZ)
                                {
                                    LDA #(errIdentifierExpected / 256) STA ZP.STRH LDA #(errIdentifierExpected % 256) STA ZP.STRL
                                    ErrorLine();    
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // get the offset where this data element will start
        Buffer.GetDataOffset(); // -> TOP
        
        
        LDA tokenBufferL
        STA STRL
        LDA tokenBufferH
        STA STRH
        LDA ZP.TOP1
        if (Z)
        {
            LDA # (SymbolType.Data | NumberType.Byte)
        }
        else
        {
            LDA # (SymbolType.Data | NumberType.Word)
        }
        Symbols.Add();
        if (NC)
        {
            LDA #(errSymbolAddFailed / 256) STA ZP.STRH LDA #(errSymbolAddFailed % 256) STA ZP.STRL
            ErrorLine();    
            return;
        }
        loop
        {
            GetToken();
            LDA tokenType
            switch (A)
            {
                case TokenType.NewLine:
                {
                    continue;
                }
                case TokenType.String:
                {
                    LDY #0
                    loop
                    {
                        LDA [tokenBuffer], Y
                        if (Z) { break; }
                        Buffer.Emit();
                        INY
                    }
                    LDA #0
                    Buffer.Emit();
                }
                case TokenType.Number:
                {
                    LDA tokenValueL
                    Buffer.Emit();
                    
                    // Check constantWidth to determine emission mode
                    LDA constantWidth
                    if (Z)  // 0 = auto-size (current behavior)
                    {
                        LDA numberType
                        AND # (NumberType.Byte | NumberType.Char)
                        if (Z) // not Byte or Char?
                        {
                            LDA tokenValueH
                            Buffer.Emit();
                        }
                    }
                    else
                    {
                        CMP #2
                        if (Z)  // 2 = word mode (always emit high byte)
                        {
                            LDA tokenValueH
                            Buffer.Emit();
                        }
                        // else: 1 = byte mode (already emitted low byte only)
                    }
                }
                
                case TokenType.Identifier:
                {
                    LDA tokenBufferL
                    STA STRL
                    LDA tokenBufferH
                    STA STRH
                    
                    // name in STR
                    // C if found, NC if not
                    // value in TOP0..1, type in A
                    FindSymbol();
                    if (NC)
                    {
                        
                        LDA #(errUndefinedSymbol / 256) STA ZP.STRH LDA #(errUndefinedSymbol % 256) STA ZP.STRL
                        ErrorLineSTR();    
                        return;
                    }
                    STA numberType
                    AND # SymbolType.Mask
                    CMP # SymbolType.Constant
                    if (NZ)
                    {
                        
                        LDA #(errUndefinedSymbol / 256) STA ZP.STRH LDA #(errUndefinedSymbol % 256) STA ZP.STRL
                        ErrorLineSTR();    
                        return;
                    }
                    LDA ZP.TOP0
                    Buffer.Emit();
                    
                    LDA numberType
                    AND # (NumberType.Byte | NumberType.Char)
                    if (Z) // not Byte or Char?
                    {
                        LDA ZP.TOP1
                        Buffer.Emit();
                    }
                }
                default:
                {
                    LDA #(errValueExpected / 256) STA ZP.STRH LDA #(errValueExpected % 256) STA ZP.STRL
                    ErrorLine();    
                    return;
                }
            }
            
            loop
            {
                GetToken();
                LDA tokenType
                CMP # TokenType.NewLine
                if (NZ) { break; }
            }
            CMP #TokenType.Comma
            if (NZ) 
            {
                UngetToken(); 
                break; 
            }
        }
        Buffer.UpdateDataSize();
        SEC
    }
    parseFuncLine()
    {
        LDA tokenType
        CMP # TokenType.Identifier
        if (NZ)
        {
            LDA #(errOpCodeExpected / 256) STA ZP.STRH LDA #(errOpCodeExpected % 256) STA ZP.STRL
            ErrorLine();    
            return;
        }
        
        LDA tokenBufferL
        STA STRL
        LDA tokenBufferH
        STA STRH
        
        // Input: STR points to opcode name to find
        // Output:
        //   C if found, NC if not
        //   OpCode in A
        //   Arguments in Y
        OpCodes.FindOpCode();
        if (NC)
        {
            GetToken();
            CMP # TokenType.Colon
            if (NZ)
            {
                // restore identifier            
                LDA STRL
                STA tokenBufferL
                LDA STRH
                STA tokenBufferH
                
                LDA #(errOpCodeExpected / 256) STA ZP.STRH LDA #(errOpCodeExpected % 256) STA ZP.STRL
                ErrorLineSTR();
                return;
            }
            
            Buffer.GetCodeOffset();
            
            // label
            // STR = name, TOP = ID
            Labels.Add(); // STR ->
            if (NC)
            {
                LDA STRL
                STA tokenBufferL
                LDA STRH
                STA tokenBufferH
                
                LDA #(errLabelAddFailed / 256) STA ZP.STRH LDA #(errLabelAddFailed % 256) STA ZP.STRL
                ErrorLineSTR();    
                return;
            }
            // Patch any forward references to this label
            ForwardRefs.Patch(); // STR = label name, TOP = label address
            if (NC)
            {
                // Patch failed - but this shouldn't happen in normal cases
                // Could add error handling here if needed
            }
            
            
            SEC
            return;
        }
        Buffer.Emit(); // emit the opcode
       
        CPY # Arguments.None
        if (NZ)
        {
            PHY
            
            GetToken();
            LDA tokenType
            CMP # TokenType.Number
            if (NZ)
            {
                CMP # TokenType.Identifier
                if (NZ)
                {
                    PLY
                    LDA #(errNumberExpected / 256) STA ZP.STRH LDA #(errNumberExpected % 256) STA ZP.STRL
                    ErrorLineSTR();    
                    return;
                }
       
                LDA tokenBufferL
                STA STRL
                LDA tokenBufferH
                STA STRH
       
                // name in STR
                // C if found, NC if not
                // value in TOP0..1, type in A
                Symbols.FindSymbol();
                if (NC)
                {
                    Labels.FindLabel();
                    if (NC)
                    {
                        // Label not found - create forward reference
                        Buffer.GetCodeOffset(); // Current position for patch
                        ForwardRefs.Add(); // STR=label name, TOP=patch address
                        if (NC)
                        {
                            PLY
                            LDA #(errLabelAddFailed / 256) STA ZP.STRH LDA #(errLabelAddFailed % 256) STA ZP.STRL
                            ErrorLineSTR();    
                            return;
                        }
                        
                        // Emit placeholder offset byte
                        LDA #0
                        Buffer.Emit();
                        
                        PLY
                        SEC  // Continue processing successfully
                        return;
                    }
                    LDA ZP.TOP0
                    STA ZP.NEXT0
                    LDA ZP.TOP1
                    STA ZP.NEXT1
                    
                    Buffer.GetCodeOffset();
                    INC TOP0 if (Z) { INC TOP1 } // +1 so the starting address is the next instruction (opcode has already been emitted, branch offset has not)
                    
                    // branch offset is always positive
                    LDA ZP.TOP1
                    CMP ZP.NEXT1
                    if (Z)
                    {
                        LDA ZP.TOP0
                        CMP ZP.NEXT0
                    }
                    if (C) // TOP >= NEXT
                    {
                        SEC
                        LDA ZP.TOP0
                        SBC ZP.NEXT0
                        STA ZP.TOP0
                        LDA ZP.TOP1
                        SBC ZP.NEXT1
                        STA ZP.TOP1
                    }
                    else
                    {
                        SEC
                        LDA ZP.NEXT0
                        SBC ZP.TOP0
                        STA ZP.TOP0
                        LDA ZP.NEXT1
                        SBC ZP.TOP1
                        STA ZP.TOP1
                    }
                    LDA # NumberType.Byte
                } // not a symbol
                STA numberType
                
                // could be a label offset, constant, ptr to data or ptr to function
                LDA ZP.TOP0
                STA tokenValueL
                LDA ZP.TOP1
                STA tokenValueH
            } // not a number
            PLY
           
            
            switch (Y)
            {
                case Arguments.Byte:
                {
                    LDA numberType
                    AND # NumberType.Byte
                    if (Z) // not Byte?
                    {
                        LDA #(errByteExpected / 256) STA ZP.STRH LDA #(errByteExpected % 256) STA ZP.STRL
                        ErrorLineValue();    
                        return;
                    }
                    LDA tokenValueL
                    Buffer.Emit();
                }
                case Arguments.Char:
                {
                    LDA numberType
                    AND # NumberType.Char
                    if (Z) // not Char?
                    {
                        LDA #(errCharExpected / 256) STA ZP.STRH LDA #(errCharExpected % 256) STA ZP.STRL
                        ErrorLineValue();    
                        return;
                    }
                    LDA tokenValueL
                    Buffer.Emit();
                }
                case Arguments.Word:
                {
                    LDA numberType
                    AND # NumberType.Word
                    if (Z) // not Word?
                    {
                        LDA #(errWordExpected / 256) STA ZP.STRH LDA #(errWordExpected % 256) STA ZP.STRL
                        ErrorLineValue();    
                        return;
                    }
                    LDA tokenValueL
                    Buffer.Emit();
                    LDA tokenValueH
                    Buffer.Emit();
                }
                case Arguments.Int:
                {
                    LDA numberType
                    AND # NumberType.Int
                    if (Z) // not Int?
                    {
                        LDA #(errIntExpected / 256) STA ZP.STRH LDA #(errIntExpected % 256) STA ZP.STRL
                        ErrorLineValue();    
                        return;
                    }
                    LDA tokenValueL
                    Buffer.Emit();
                    LDA tokenValueH
                    Buffer.Emit();
                }
            }
        }
        
        // function ID in TOP
        LDA currentFunctionID
        STA ZP.TOP0
        Buffer.CheckFunctionSize();
    }
}
