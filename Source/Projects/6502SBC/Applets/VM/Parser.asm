unit Parser
{
    uses "Buffer"
    uses "../System/Char"
    
    // Single base for easy relocation
    const byte parserSlots = 0x70; // 0x70..0x7F
    
    const uint parserFlags    = parserSlots+0;
    // Bit 0 - .MAIN seen
    // Bit 1 - .CONST seen
    // Bit 2 - .DATA seen
    
    const uint bufferIndexL = parserSlots+1;
    const uint bufferIndexH = parserSlots+2;
    
    const byte currentChar  = parserSlots+3;  // Current character
    const byte tokenType    = parserSlots+4;  // Type of current token
    const uint tokenValueL  = parserSlots+5;  // Token value/pointer
    const uint tokenValueH  = parserSlots+6;
    const byte tokenLength  = parserSlots+7;  // Length of identifier/string
    
    // Token buffer for identifiers/strings
    const uint tokenBuffer  = parserSlots+8;  // Pointer to allocated buffer
    const byte tokenBufferL = parserSlots+8;
    const byte tokenBufferH = parserSlots+9;
    
    // Token types
    enum TokenType
    {
        EOF        = 0,
        Directive  = 1,   // .CONST, .DATA, .MAIN, .FUNC
        Identifier = 2,   // Label or instruction
        Number     = 3,   // Decimal, hex, or char literal
        String     = 4,   // String literal
        Colon      = 5,   // :
        Newline    = 6,   // End of line
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
        
        LDA #TokenType.Number
        STA tokenType
    }
    
    // Read hex number (after 0x)
    readHexNumber()
    {
        loop
        {
            peek();
            Char.IsHex();
            if (NC) { break; }
            
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
            
            next();
        }
        
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
        if (NC) { return; }
        
        LDA ZP.IDXL
        STA tokenBufferL
        LDA ZP.IDXH
        STA tokenBufferH
        
        // Get first character
        refillBuffer();
        if (C)
        {
            next();
        }
        
        SEC
    }
    Dispose()
    {
        LDA tokenBufferL
        STA ZP.IDXL
        LDA tokenBufferH
        STA ZP.IDXH
        Memory.Free();
    }
    
    // Get next token
    GetToken()
    {
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
                LDA #TokenType.Newline
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

            LDA tokenType
            if (Z) { break; }  // EOF
            
            // Process token based on type
            // (This is where parsing logic would go)
            
            // For now, just continue
        }
        
        SEC
    }
}
