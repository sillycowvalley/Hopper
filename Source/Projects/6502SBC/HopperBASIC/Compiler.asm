unit Compiler 
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "OpCodes"
    uses "Messages"
    uses "Tokenizer"
    uses "Tools"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Buffer management and opcode emission with proper bounds checking
    
    // Private Compiler layer storage - BasicProcessBuffer3 (32 bytes at 0x09E0-0x09FF)
    const uint compilerSavedTokenPosL = Address.BasicProcessBuffer3;      // 0x09E0: 1 byte - saved tokenizer pos low
    const uint compilerSavedTokenPosH = Address.BasicProcessBuffer3 + 1;  // 0x09E1: 1 byte - saved tokenizer pos high
    const uint compilerLiteralOffsetL = Address.BasicProcessBuffer3 + 2;  // 0x09E2: 1 byte - literal offset low
    const uint compilerLiteralOffsetH = Address.BasicProcessBuffer3 + 3;  // 0x09E3: 1 byte - literal offset high
    const uint compilerScratch1      = Address.BasicProcessBuffer3 + 4;   // 0x09E4: 1 byte - general scratch
    const uint compilerScratch2      = Address.BasicProcessBuffer3 + 5;   // 0x09E5: 1 byte - general scratch
    const uint compilerOperatorToken = Address.BasicProcessBuffer3 + 6;   // 0x09E6: 1 byte - saved operator token
    const uint compilerBufferAddr    = Address.BasicProcessBuffer3 + 7;   // 0x09E7: 2 bytes - calculated buffer address
    // 23 bytes available for future compiler needs (0x09E9-0x09FF)
    
    // Initialize the opcode buffer for compilation
    // Output: Opcode buffer ready for emission
    // Modifies: ZP.OpcodeBufferLengthL/H (set to 0), ZP.CompilerTokenPosL/H (set to current), ZP.CompilerFlags (cleared)
    InitOpcodeBuffer()
    {
        // Clear opcode buffer length
        STZ ZP.OpcodeBufferLengthL
        STZ ZP.OpcodeBufferLengthH
        
        // Save current tokenizer position for literal references
        LDA ZP.TokenizerPosL
        STA ZP.CompilerTokenPosL
        LDA ZP.TokenizerPosH
        STA ZP.CompilerTokenPosH
        
        // Clear compiler flags
        STZ ZP.CompilerFlags
        
        SEC // Success
    }
    
    // Check if there's space for N bytes in opcode buffer
    // Input: A = number of bytes needed
    // Output: C if space available, NC if buffer would overflow
    CheckBufferSpace()
    {
        // Save the required space in ZP.OpcodeTemp
        STA ZP.OpcodeTemp
        
        // Calculate current position + required space
        CLC
        ADC ZP.OpcodeBufferLengthL
        STA compilerScratch1 // Low byte of new position
        LDA ZP.OpcodeBufferLengthH
        ADC #0
        STA compilerScratch2 // High byte of new position
        
        // Compare against buffer size (512 bytes = 0x0200)
        LDA compilerScratch2
        CMP #0x02
        if (C) // >= 0x0200, definitely overflow
        {
            LDA #(Messages.BufferOverflow % 256)
            STA ZP.LastErrorL
            LDA #(Messages.BufferOverflow / 256)
            STA ZP.LastErrorH
            Messages.StorePC(); // 6502 PC -> IDY
            CLC // Overflow
            return;
        }
        
        if (NZ) // High byte is 0x01, check low byte
        {
            SEC // Within bounds (0x0100-0x01FF)
            return;
        }
        
        // High byte is 0x00, always within bounds
        SEC // Success
    }
    
    // Get current position in opcode buffer
    // Output: ZP.ACCL/ZP.ACCH = current buffer position
    GetOpcodeBufferPosition()
    {
        LDA ZP.OpcodeBufferLengthL
        STA ZP.ACCL
        LDA ZP.OpcodeBufferLengthH
        STA ZP.ACCH
        SEC // Success
    }
    
    // Emit a single-byte opcode (no operands)
    // Input: A = opcode value
    // Output: Opcode written to buffer
    // Modifies: ZP.OpcodeBufferLengthL/H (incremented)
    EmitOpcode()
    {
        // Save opcode in ZP.OpcodeTemp
        STA ZP.OpcodeTemp
        
        // Check space for 1 byte
        LDA #1
        CheckBufferSpace();
        if (NC) { return; } // Buffer overflow
        
        // Calculate buffer address: BasicOpcodeBuffer + current length
        CLC
        LDA #(Address.BasicOpcodeBuffer % 256)
        ADC ZP.OpcodeBufferLengthL
        STA compilerBufferAddr
        LDA #(Address.BasicOpcodeBuffer / 256) 
        ADC ZP.OpcodeBufferLengthH
        STA compilerBufferAddr + 1
        
        // Write opcode to buffer
        LDA ZP.OpcodeTemp
        LDY #0
        STA [compilerBufferAddr], Y
        
        // Increment buffer length
        INC ZP.OpcodeBufferLengthL
        if (Z)
        {
            INC ZP.OpcodeBufferLengthH
        }
        
        SEC // Success
    }
    
    // Emit opcode with one byte operand
    // Input: A = opcode value, X = operand byte
    // Output: Opcode and operand written to buffer
    // Modifies: ZP.OpcodeBufferLengthL/H (incremented by 2)
    EmitOpcodeWithByte()
    {
        // Save parameters
        STA ZP.OpcodeTemp
        STX compilerScratch1 // Temporarily store operand
        
        // Check space for 2 bytes
        LDA #2
        CheckBufferSpace();
        if (NC) 
        { 
            LDX compilerScratch1 // Restore X
            return; 
        }
        
        // Calculate buffer address: BasicOpcodeBuffer + current length
        CLC
        LDA #(Address.BasicOpcodeBuffer % 256)
        ADC ZP.OpcodeBufferLengthL
        STA compilerBufferAddr
        LDA #(Address.BasicOpcodeBuffer / 256)
        ADC ZP.OpcodeBufferLengthH
        STA compilerBufferAddr + 1
        
        // Write opcode
        LDA ZP.OpcodeTemp
        LDY #0
        STA [compilerBufferAddr], Y
        
        // Write operand
        LDA compilerScratch1 // Retrieve operand
        INY
        STA [compilerBufferAddr], Y
        
        // Restore X register
        LDX compilerScratch1
        
        // Increment buffer length by 2
        CLC
        LDA ZP.OpcodeBufferLengthL
        ADC #2
        STA ZP.OpcodeBufferLengthL
        LDA ZP.OpcodeBufferLengthH
        ADC #0
        STA ZP.OpcodeBufferLengthH
        
        SEC // Success
    }
    
    // Emit opcode with two byte operands (word value)
    // Input: A = opcode value, ZP.ACCL/ZP.ACCH = 16-bit operand (LSB/MSB)
    // Output: Opcode and operands written to buffer
    // Modifies: ZP.OpcodeBufferLengthL/H (incremented by 3)
    EmitOpcodeWithWord()
    {
        // Save opcode
        STA ZP.OpcodeTemp
        
        // Save word operand in scratch space
        LDA ZP.ACCL
        STA compilerScratch1
        LDA ZP.ACCH
        STA compilerScratch2
        
        // Check space for 3 bytes
        LDA #3
        CheckBufferSpace();
        if (NC) 
        { 
            // Restore ZP.ACCL/ZP.ACCH
            LDA compilerScratch1
            STA ZP.ACCL
            LDA compilerScratch2
            STA ZP.ACCH
            return; 
        }
        
        // Calculate buffer address: BasicOpcodeBuffer + current length
        CLC
        LDA #(Address.BasicOpcodeBuffer % 256)
        ADC ZP.OpcodeBufferLengthL
        STA compilerBufferAddr
        LDA #(Address.BasicOpcodeBuffer / 256)
        ADC ZP.OpcodeBufferLengthH
        STA compilerBufferAddr + 1
        
        // Write opcode
        LDA ZP.OpcodeTemp
        LDY #0
        STA [compilerBufferAddr], Y
        
        // Write LSB
        LDA compilerScratch1
        INY
        STA [compilerBufferAddr], Y
        
        // Write MSB  
        LDA compilerScratch2
        INY
        STA [compilerBufferAddr], Y
        
        // Restore ZP.ACCL/ZP.ACCH
        LDA compilerScratch1
        STA ZP.ACCL
        LDA compilerScratch2
        STA ZP.ACCH
        
        // Increment buffer length by 3
        CLC
        LDA ZP.OpcodeBufferLengthL
        ADC #3
        STA ZP.OpcodeBufferLengthL
        LDA ZP.OpcodeBufferLengthH
        ADC #0
        STA ZP.OpcodeBufferLengthH
        
        SEC // Success
    }
    
    // Calculate offset from compilation start to current tokenizer position
    // Output: ZP.ACCL/ZP.ACCH = offset from ZP.CompilerTokenPos to current ZP.TokenizerPos
    CalculateTokenOffset()
    {
        // Calculate current tokenizer position - compilation start position
        SEC
        LDA ZP.TokenizerPosL
        SBC ZP.CompilerTokenPosL
        STA ZP.ACCL
        LDA ZP.TokenizerPosH
        SBC ZP.CompilerTokenPosH
        STA ZP.ACCH
        
        SEC // Success
    }
    
    // Emit PUSHBIT opcode with immediate value
    // Input: A = bit value (0 or 1)
    // Output: PUSHBIT opcode emitted with value
    EmitPushBit()
    {
        // Validate bit value (must be 0 or 1)
        CMP #2
        if (C) // >= 2, invalid
        {
            LDA #(Messages.InvalidBitValue % 256)
            STA ZP.LastErrorL
            LDA #(Messages.InvalidBitValue / 256)
            STA ZP.LastErrorH
            Messages.StorePC(); // 6502 PC -> IDY
            CLC
            return;
        }
        
        // Emit PUSHBIT opcode with value as operand
        TAX // Move value to X for operand
        LDA #OpcodeType.PUSHBIT
        EmitOpcodeWithByte();
    }
    
    // Emit PUSHBYTE opcode with immediate value
    // Input: A = byte value
    // Output: PUSHBYTE opcode emitted with value
    EmitPushByte()
    {
        TAX // Move value to X for operand
        LDA #OpcodeType.PUSHBYTE
        EmitOpcodeWithByte();
    }
    
    // Emit PUSHINT or PUSHWORD opcode with word value
    // Input: ZP.ACCL/ZP.ACCH = 16-bit value, ZP.TOPT = type (determines opcode)
    // Output: Appropriate opcode emitted with value
    EmitPushWord()
    {
        // Select opcode based on type
        LDA ZP.TOPT
        CMP #Types.INT
        if (Z)
        {
            LDA #OpcodeType.PUSHINT
            EmitOpcodeWithWord();
            return;
        }
        
        CMP #Types.WORD
        if (Z)
        {
            LDA #OpcodeType.PUSHWORD
            EmitOpcodeWithWord();
            return;
        }
        
        // Invalid type for word push
        LDA #(Messages.InvalidType % 256)
        STA ZP.LastErrorL
        LDA #(Messages.InvalidType / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
        CLC
    }
    
    // Emit PUSHGLOBAL opcode to load variable
    // Input: No parameters (uses current token position for variable name offset)
    // Output: PUSHGLOBAL opcode emitted with token offset
    EmitPushGlobal()
    {
        // Calculate offset to current token (variable name)
        CalculateTokenOffset();
        if (NC) { return; }
        
        // Check if offset fits in single byte (most common case)
        LDA ZP.ACCH
        if (Z) // High byte is 0, use single-byte operand
        {
            LDX ZP.ACCL
            LDA #OpcodeType.PUSHGLOBAL
            EmitOpcodeWithByte();
            return;
        }
        
        // Offset requires word operand - this should be rare
        // For now, generate error as we expect most programs to fit in 256 byte token buffer
        LDA #(Messages.TokenBufferTooLarge % 256)
        STA ZP.LastErrorL
        LDA #(Messages.TokenBufferTooLarge / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
        CLC
    }
    
    // Emit POPGLOBAL opcode to store to variable  
    // Input: No parameters (uses current token position for variable name offset)
    // Output: POPGLOBAL opcode emitted with token offset
    EmitPopGlobal()
    {
        // Calculate offset to current token (variable name)
        CalculateTokenOffset();
        if (NC) { return; }
        
        // Check if offset fits in single byte
        LDA ZP.ACCH
        if (Z) // High byte is 0, use single-byte operand
        {
            LDX ZP.ACCL
            LDA #OpcodeType.POPGLOBAL
            EmitOpcodeWithByte();
            return;
        }
        
        // Offset too large
        LDA #(Messages.TokenBufferTooLarge % 256)
        STA ZP.LastErrorL
        LDA #(Messages.TokenBufferTooLarge / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
        CLC
    }
    
    // Emit arithmetic operation opcode
    // Input: A = operation token (Tokens.PLUS, Tokens.MINUS, etc.)
    // Output: Corresponding arithmetic opcode emitted
    EmitArithmeticOp()
    {
        switch (A)
        {
            case Tokens.PLUS:
            {
                LDA #OpcodeType.ADD
                EmitOpcode();
                return;
            }
            case Tokens.MINUS:
            {
                LDA #OpcodeType.SUB
                EmitOpcode();
                return;
            }
            case Tokens.MULTIPLY:
            {
                LDA #OpcodeType.MUL
                EmitOpcode();
                return;
            }
            case Tokens.DIVIDE:
            {
                LDA #OpcodeType.DIV
                EmitOpcode();
                return;
            }
            case Tokens.MODULO:
            {
                LDA #OpcodeType.MOD
                EmitOpcode();
                return;
            }
            default:
            {
                LDA #(Messages.InvalidOperator % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InvalidOperator / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                return;
            }
        }
    }
    
    // Emit comparison operation opcode
    // Input: A = comparison token (Tokens.EQUALS, Tokens.LESSTHAN, etc.)
    // Output: Corresponding comparison opcode emitted
    EmitComparisonOp()
    {
        switch (A)
        {
            case Tokens.EQUALS:
            {
                LDA #OpcodeType.EQ
                EmitOpcode();
                return;
            }
            case Tokens.NOTEQUAL:
            {
                LDA #OpcodeType.NE
                EmitOpcode();
                return;
            }
            case Tokens.LESSTHAN:
            {
                LDA #OpcodeType.LT
                EmitOpcode();
                return;
            }
            case Tokens.GREATERTHAN:
            {
                LDA #OpcodeType.GT
                EmitOpcode();
                return;
            }
            case Tokens.LESSEQUAL:
            {
                LDA #OpcodeType.LE
                EmitOpcode();
                return;
            }
            case Tokens.GREATEREQUAL:
            {
                LDA #OpcodeType.GE
                EmitOpcode();
                return;
            }
            default:
            {
                LDA #(Messages.InvalidOperator % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InvalidOperator / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                return;
            }
        }
    }
    
    // Emit logical operation opcode
    // Input: A = logical token (Tokens.AND, Tokens.OR, Tokens.NOT)
    // Output: Corresponding logical opcode emitted
    EmitLogicalOp()
    {
        switch (A)
        {
            case Tokens.AND:
            {
                LDA #OpcodeType.LOGICAL_AND
                EmitOpcode();
                return;
            }
            case Tokens.OR:
            {
                LDA #OpcodeType.LOGICAL_OR
                EmitOpcode();
                return;
            }
            case Tokens.NOT:
            {
                LDA #OpcodeType.LOGICAL_NOT
                EmitOpcode();
                return;
            }
            default:
            {
                LDA #(Messages.InvalidOperator % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InvalidOperator / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                return;
            }
        }
    }
    
    // Emit bitwise operation opcode
    // Input: A = bitwise token 
    // Output: Corresponding bitwise opcode emitted
    EmitBitwiseOp()
    {
        switch (A)
        {
            case Tokens.BITWISE_AND:
            {
                LDA #OpcodeType.BITWISE_AND
                EmitOpcode();
                return;
            }
            case Tokens.BITWISE_OR:
            {
                LDA #OpcodeType.BITWISE_OR
                EmitOpcode();
                return;
            }
            default:
            {
                LDA #(Messages.InvalidOperator % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InvalidOperator / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                return;
            }
        }
    }
    
    // Emit unary minus (negation) opcode
    // Output: NEG opcode emitted
    EmitUnaryMinus()
    {
        LDA #OpcodeType.NEG
        EmitOpcode();
    }
    
    // Emit system call opcode
    // Input: A = system call ID
    // Output: SYSCALL opcode emitted with ID
    EmitSysCall()
    {
        TAX // Move ID to X for operand
        LDA #OpcodeType.SYSCALL
        EmitOpcodeWithByte();
    }
    
    // Main entry point: Compile current expression to opcodes
    // Input: ZP.CurrentToken = first token of expression
    // Output: Expression compiled to opcode buffer, ZP.CurrentToken = token after expression
    // Modifies: Opcode buffer, ZP.CurrentToken, compilation state
    CompileExpression()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'E'
        Tools.COut();
#endif
        
        // Initialize opcode buffer if this is the start of compilation
        InitOpcodeBuffer();
        if (NC) { return; }
        
        // Compile the expression using same precedence as Expression.asm
        compileLogical();
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'E'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile logical OR operations (lowest precedence)
    // Input: ZP.CurrentToken = current token
    // Output: Logical opcodes emitted, ZP.CurrentToken = token after expression
    compileLogical()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'L'
        Tools.COut();
#endif
        
        // Compile left operand (higher precedence)
        compileLogicalAnd();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.OR
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile right operand
                compileLogicalAnd();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit OR opcode
                LDA #Tokens.OR
                EmitLogicalOp();
                if (NC) { return; }
                
                continue;
            }
            
            break; // No more OR operators
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'L'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC // Success
    }
    
    // Compile logical AND operations (higher precedence than OR)
    // Input: ZP.CurrentToken = current token
    // Output: Logical opcodes emitted, ZP.CurrentToken = token after expression
    compileLogicalAnd()
    {
        // Compile left operand (higher precedence)
        compileComparison();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.AND
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile right operand
                compileComparison();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit AND opcode
                LDA #Tokens.AND
                EmitLogicalOp();
                if (NC) { return; }
                
                continue;
            }
            
            break; // No more AND operators
        }
        
        SEC // Success
    }
    
    // Compile comparison operations
    // Input: ZP.CurrentToken = current token
    // Output: Comparison opcodes emitted, ZP.CurrentToken = token after expression
    compileComparison()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'C'
        Tools.COut();
#endif
        
        // Compile left operand
        compileBitwiseOr();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.EQUALS:
                case Tokens.NOTEQUAL:
                case Tokens.LESSTHAN:
                case Tokens.GREATERTHAN:
                case Tokens.LESSEQUAL:
                case Tokens.GREATEREQUAL:
                {
                    STA compilerOperatorToken // Save operator token
                    
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // Compile right operand
                    compileBitwiseOr();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // Emit comparison opcode
                    LDA compilerOperatorToken // Restore operator token
                    EmitComparisonOp();
                    if (NC) { return; }
                    
                    continue; // Continue looking for more comparison operators
                    break; // Exit switch to continue loop
                }
                default:
                {
                    break; // Exit switch and loop - no more comparison operators
                }
            }
            
            break; // No more comparison operators
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC // Success
    }
    
    // Compile bitwise OR operations
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise opcodes emitted, ZP.CurrentToken = token after expression
    compileBitwiseOr()
    {
        // Compile left operand (higher precedence)
        compileAdditive();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.BITWISE_OR
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile right operand
                compileAdditive();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit bitwise OR opcode
                LDA #Tokens.BITWISE_OR
                EmitBitwiseOp();
                if (NC) { return; }
                
                continue;
            }
            
            break; // No more bitwise OR operators
        }
        
        SEC // Success
    }
    
    // Compile additive operations (+ and -)
    // Input: ZP.CurrentToken = current token
    // Output: Additive opcodes emitted, ZP.CurrentToken = token after expression
    compileAdditive()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'A'
        Tools.COut();
#endif
        
        // Compile left operand
        compileMultiplicative();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.PLUS:
                case Tokens.MINUS:
                {
                    STA compilerOperatorToken // Save operator
                    
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // Compile right operand
                    compileMultiplicative();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // Emit arithmetic opcode
                    LDA compilerOperatorToken // Restore operator
                    EmitArithmeticOp();
                    if (NC) { return; }
                    
                    continue; // Continue looking for more additive operators
                    break; // Exit switch to continue loop
                }
                default:
                {
                    break; // Exit switch and loop - no more additive operators
                }
            }
            
            break; // No more additive operators
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'A'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC // Success
    }
    
    // Compile multiplicative operations (*, /, %)
    // Input: ZP.CurrentToken = current token
    // Output: Multiplicative opcodes emitted, ZP.CurrentToken = token after expression
    compileMultiplicative()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'M'
        Tools.COut();
#endif
        
        // Compile left operand
        compileUnary();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.MULTIPLY:
                case Tokens.DIVIDE:
                case Tokens.MODULO:
                {
                    STA compilerOperatorToken // Save operator
                    
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // Compile right operand
                    compileUnary();
                    Messages.CheckError();
                    if (NC) { return; }
                    
                    // Emit arithmetic opcode
                    LDA compilerOperatorToken // Restore operator
                    EmitArithmeticOp();
                    if (NC) { return; }
                    
                    continue; // Continue looking for more multiplicative operators
                    break; // Exit switch to continue loop
                }
                default:
                {
                    break; // Exit switch and loop - no more multiplicative operators
                }
            }
            
            break; // No more multiplicative operators
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'M'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        
        SEC // Success
    }
    
    // Compile unary operations (-, NOT)
    // Input: ZP.CurrentToken = current token
    // Output: Unary opcodes emitted, ZP.CurrentToken = token after expression
    compileUnary()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'U'
        Tools.COut();
#endif
        
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.MINUS:
            {
                // Unary minus
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile the operand first
                compilePrimary();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit unary minus opcode
                EmitUnaryMinus();
                if (NC) { return; }
                
                break; // Exit switch - unary minus handled
            }
            case Tokens.NOT:
            {
                // Logical NOT
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile the operand
                compilePrimary();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit logical NOT opcode
                LDA #Tokens.NOT
                EmitLogicalOp();
                if (NC) { return; }
                
                break; // Exit switch - logical NOT handled
            }
            default:
            {
                // Not unary, compile primary
                compilePrimary();
                break; // Exit switch - primary handled
            }
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'U'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile primary expressions (numbers, identifiers, parentheses)
    // Input: ZP.CurrentToken = current token
    // Output: Primary opcodes emitted, ZP.CurrentToken = token after expression
    compilePrimary()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'P'
        Tools.COut();
#endif
        
        loop // Single exit pattern
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.NUMBER:
                {
                    // Get number value and type
                    Tokenizer.GetTokenNumber(); // Result in ZP.TOP, type in ZP.TOPT
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Emit appropriate push opcode based on type and value
                    LDA ZP.TOPT
                    CMP #Types.BIT
                    if (Z)
                    {
                        LDA ZP.TOP // BIT values are single byte
                        EmitPushBit();
                        if (NC) { break; }
                    }
                    else // INT or WORD
                    {
                        CMP #Types.BYTE
                        if (Z)
                        {
                            LDA ZP.TOP
                            EmitPushByte();
                            if (NC) { break; }
                        }
                        else // 16-bit value
                        {
                            // Move value to ZP.ACCL/ZP.ACCH for word emission
                            LDA ZP.TOPL
                            STA ZP.ACCL
                            LDA ZP.TOPH
                            STA ZP.ACCH
                            
                            EmitPushWord();
                            if (NC) { break; }
                        }
                    }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break; // Exit switch - number handled
                }
                case Tokens.IDENTIFIER:
                {
                    // Emit PUSHGLOBAL with current token position
                    EmitPushGlobal();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break; // Exit switch - identifier handled
                }
                case Tokens.LPAREN:
                {
                    // Get next token (start of sub-expression)
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Compile sub-expression
                    compileLogical(); // Start from lowest precedence
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Expect closing parenthesis
                    LDA ZP.CurrentToken
                    CMP #Tokens.RPAREN
                    if (NZ)
                    {
                        LDA #(Messages.ExpectedRightParen % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.ExpectedRightParen / 256)
                        STA ZP.LastErrorH
                        Messages.StorePC(); // 6502 PC -> IDY
                        CLC
                        break;
                    }
                    
                    // Get next token after closing paren
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break; // Exit switch - parenthesized expression handled
                }
                default:
                {
                    // Unexpected token in primary expression
                    LDA #(Messages.ExpectedExpression % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.ExpectedExpression / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                    CLC
                    break; // Exit switch - error case handled
                }
            }
            
            break; // Exit loop
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'P'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
}
