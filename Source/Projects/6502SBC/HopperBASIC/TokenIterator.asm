unit TokenIterator
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "Tokenizer"
    uses "Messages"
    
    // API Status: Clean
    // Simple token stream iterator for function body display
    // Uses ZP.L* scratch space - DO NOT call Table/Objects/Variables/Functions APIs while iterating
    
    // Memory layout for token iterator state - BASIC ZP allocation (0x40-0x4F, 16 bytes available)
    const byte tokIterCurrent        = 0x40;  // 0x40: current token value (1 byte)
    const byte tokIterBaseL          = 0x41;  // 0x41: token stream base pointer low (1 byte)
    const byte tokIterBaseH          = 0x42;  // 0x42: token stream base pointer high (1 byte)
    // 13 bytes remaining for future token iterator needs (0x43-0x4F)
    
    // Initialize token iterator at start of token stream
    // Input: ZP.IDY = token stream base pointer (16-bit)
    // Output: Iterator positioned at first token, C set if stream has content, NC if empty/null
    // Modifies: ZP.LCURRENTL/H (position = 0), tokIterCurrent (first token), tokIterBaseL/H (base pointer)
    // Uses: ZP.L* scratch space (cannot call symbol table APIs after this until done iterating)
    Start()
    {
        PHA
        PHY
        
        // Save token stream base pointer
        LDA ZP.IDYL
        STA tokIterBaseL
        LDA ZP.IDYH
        STA tokIterBaseH
        
        // Check for null pointer
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            // Null stream - set current token to 0 and return NC
            STZ tokIterCurrent
            PLY
            PLA
            CLC
            return;
        }
        
        // Initialize position to 0
        STZ ZP.LCURRENTL
        STZ ZP.LCURRENTH
        
        // Load first token
        LDY #0
        LDA [ZP.IDY], Y
        STA tokIterCurrent
        
        // Check if stream is empty (first token is 0)
        if (Z)
        {
            PLY
            PLA
            CLC  // Empty stream
            return;
        }
        
        PLY
        PLA
        SEC  // Stream has content
    }
    
    // Advance to next token in stream
    // Input: Iterator must be initialized with Start()
    // Output: C set if advanced to valid token, NC if reached end of stream
    // Modifies: ZP.LCURRENTL/H (advanced), tokIterCurrent (next token value)
    // Uses: ZP.L* scratch space (cannot call symbol table APIs while iterating)
    Next()
    {
        PHA
        PHY
        
        loop // Single exit block
        {
            // Current token determines how many bytes to skip
            LDA tokIterCurrent
            switch (A)
            {
                case Tokens.NUMBER:
                case Tokens.IDENTIFIER:
                case Tokens.STRING:
                case Tokens.REM:
                case Tokens.COMMENT:
                {
                    // These tokens have inline string data - skip past the string
                    skipInlineString();
                    if (NC) { break; } // Error or end of stream
                }
                default:
                {
                    // Regular token - just advance by 1 byte
                    INC ZP.LCURRENTL
                    if (Z)
                    {
                        INC ZP.LCURRENTH
                    }
                }
            }
            
            // Calculate current address: base + position
            CLC
            LDA tokIterBaseL
            ADC ZP.LCURRENTL
            STA ZP.IDYL
            LDA tokIterBaseH
            ADC ZP.LCURRENTH
            STA ZP.IDYH
            
            // Load token at current position
            LDY #0
            LDA [ZP.IDY], Y
            STA tokIterCurrent
            
            // Check if we hit end of stream (token = 0)
            if (Z)
            {
                CLC  // End of stream
                break;
            }
            
            SEC  // Valid token loaded
            break;
        }
        
        PLY
        PLA
    }
    
    // Skip past null-terminated string at current position
    // Input: ZP.LCURRENTL/H = current position pointing to start of string
    // Output: ZP.LCURRENTL/H advanced past null terminator, C set if successful, NC if error
    // Modifies: ZP.LCURRENTL/H (advanced), ZP.IDY (temporary address calculation)
    skipInlineString()
    {
        PHA
        PHY
        
        loop
        {
            // Advance position by 1 byte first
            INC ZP.LCURRENTL
            if (Z)
            {
                INC ZP.LCURRENTH
            }
            
            // Calculate current address: base + position
            CLC
            LDA tokIterBaseL
            ADC ZP.LCURRENTL
            STA ZP.IDYL
            LDA tokIterBaseH
            ADC ZP.LCURRENTH
            STA ZP.IDYH
            
            // Load character at current position
            LDY #0
            LDA [ZP.IDY], Y
            if (Z) 
            { 
                // Found null terminator - advance past it
                INC ZP.LCURRENTL
                if (Z)
                {
                    INC ZP.LCURRENTH
                }
                SEC  // Success
                break; 
            }
            
            // Continue to next character
        }
        
        PLY
        PLA
    }
    
    // Get current token value
    // Input: Iterator must be initialized and positioned
    // Output: A = current token value
    // Preserves: Everything except A
    GetCurrent()
    {
        LDA tokIterCurrent
    }
    
    // Get pointer to current token's inline data (for literals)
    // Input: Iterator positioned at token with inline data (NUMBER, IDENTIFIER, STRING, REM, COMMENT)
    // Output: ZP.IDY = pointer to start of inline data (after token byte)
    // Preserves: Iterator position, all registers except output
    GetCurrentData()
    {
        PHA
        
        // Calculate address after current token: base + position + 1
        CLC
        LDA tokIterBaseL
        ADC ZP.LCURRENTL
        STA ZP.IDYL
        LDA tokIterBaseH
        ADC ZP.LCURRENTH
        STA ZP.IDYH
        
        // Advance past the token byte to point at data
        INC ZP.IDYL
        if (Z)
        {
            INC ZP.IDYH
        }
        
        PLA
    }
}
