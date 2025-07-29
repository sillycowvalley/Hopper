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
    const uint compilerOpCode         = Address.BasicProcessBuffer3 + 4;  // 0x09E4: 1 byte - opcode to emit
    const uint compilerOperand1       = Address.BasicProcessBuffer3 + 5;  // 0x09E5: 1 byte - first operand
    const uint compilerOperand2       = Address.BasicProcessBuffer3 + 6;  // 0x09E6: 1 byte - second operand
    // 25 bytes available for future compiler needs (0x09E7-0x09FF)
    
    // Initialize the opcode buffer for compilation
    // Output: Opcode buffer ready for emission
    // Modifies: ZP.OpcodeBufferLengthL/H (set to 0), ZP.CompilerTokenPosL/H (set to current), ZP.CompilerFlags (cleared), ZP.PC (set to buffer start)
    InitOpcodeBuffer()
    {
        // Clear opcode buffer length
        STZ ZP.OpcodeBufferLengthL
        STZ ZP.OpcodeBufferLengthH
        
        // Initialize PC to start of opcode buffer
        LDA #(Address.BasicOpcodeBuffer % 256)
        STA ZP.PCL
        LDA #(Address.BasicOpcodeBuffer / 256)
        STA ZP.PCH
        
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
    // Strategy: Increment buffer length first, then check bounds
    CheckBufferSpace()
    {
        // Add required bytes to current buffer length
        CLC
        ADC ZP.OpcodeBufferLengthL
        STA ZP.OpcodeBufferLengthL
        LDA ZP.OpcodeBufferLengthH
        ADC #0
        STA ZP.OpcodeBufferLengthH
        
        // Compare against buffer size (512 bytes = 0x0200)
        LDA ZP.OpcodeBufferLengthH
        CMP #0x02
        if (C) // >= 0x0200, overflow
        {
            LDA #(Messages.BufferOverflow % 256)
            STA ZP.LastErrorL
            LDA #(Messages.BufferOverflow / 256)
            STA ZP.LastErrorH
            Messages.StorePC(); // 6502 PC -> IDY
            CLC // Overflow
            return;
        }
        
        SEC // Success - space available
    }
    
    // Emit a single-byte opcode (no operands)
    // Input: compilerOpCode = opcode value
    // Output: Opcode written to buffer
    // Modifies: ZP.OpcodeBufferLengthL/H (incremented), ZP.PC (incremented)
    EmitOpcode()
    {
#ifdef DEBUG       
        Tools.NL(); LDA #'>' Tools.COut();
        LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
        LDA #' ' Tools.COut(); LDA compilerOpCode Tools.HOut(); LDA #' ' Tools.COut();
#endif        
        // Check space for 1 byte
        LDA #1
        CheckBufferSpace();
        if (NC) { return; } // Buffer overflow
        
        // Write opcode to buffer
        LDA compilerOpCode
        STA [ZP.PC]
        
        // Increment PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        SEC // Success
    }
    
    // Emit opcode with one byte operand
    // Input: compilerOpCode = opcode value, compilerOperand1 = operand byte
    // Output: Opcode and operand written to buffer
    // Modifies: ZP.OpcodeBufferLengthL/H (incremented by 2), ZP.PC (incremented by 2)
    EmitOpcodeWithByte()
    {
#ifdef DEBUG       
        Tools.NL(); LDA #'>' Tools.COut();
        LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
        LDA #' ' Tools.COut(); LDA compilerOpCode Tools.HOut(); LDA #' ' Tools.COut(); 
                               LDA compilerOperand1 Tools.HOut(); LDA #' ' Tools.COut();
#endif
        // Check space for 2 bytes
        LDA #2
        CheckBufferSpace();
        if (NC) { return; } // Buffer overflow
        
        // Write opcode
        LDA compilerOpCode
        STA [ZP.PC]
        
        // Increment PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        // Write operand
        LDA compilerOperand1
        STA [ZP.PC]
        
        // Increment PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        SEC // Success
    }
    
    // Emit opcode with two byte operands (word value)
    // Input: compilerOpCode = opcode value, compilerOperand1 = LSB, compilerOperand2 = MSB
    // Output: Opcode and operands written to buffer
    // Modifies: ZP.OpcodeBufferLengthL/H (incremented by 3), ZP.PC (incremented by 3)
    EmitOpcodeWithWord()
    {
#ifdef DEBUG       
        Tools.NL(); LDA #'>' Tools.COut();
        LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
        LDA #' ' Tools.COut(); LDA compilerOpCode Tools.HOut(); LDA #' ' Tools.COut(); 
                               LDA compilerOperand1 Tools.HOut(); LDA #' ' Tools.COut();
                               LDA compilerOperand2 Tools.HOut(); LDA #' ' Tools.COut();
#endif        
        // Check space for 3 bytes
        LDA #3
        CheckBufferSpace();
        if (NC) { return; } // Buffer overflow
        
        // Write opcode
        LDA compilerOpCode
        STA [ZP.PC]
        
        // Increment PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        // Write LSB
        LDA compilerOperand1
        STA [ZP.PC]
        
        // Increment PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        // Write MSB  
        LDA compilerOperand2
        STA [ZP.PC]
        
        // Increment PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
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
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpcodeWithByte()
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
        
        // Set up parameters for emission
        STA compilerOperand1          // Store value as operand
        LDA #OpcodeType.PUSHBIT
        STA compilerOpCode
        
        EmitOpcodeWithByte();
    }
    
    // Emit PUSHBYTE opcode with immediate value
    // Input: A = byte value
    // Output: PUSHBYTE opcode emitted with value
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpcodeWithByte()
    EmitPushByte()
    {
        // Set up parameters for emission
        STA compilerOperand1          // Store value as operand
        LDA #OpcodeType.PUSHBYTE
        STA compilerOpCode
        
        EmitOpcodeWithByte();
    }
    
    // Emit PUSHINT or PUSHWORD opcode with word value
    // Input: ZP.TOPT = type (determines opcode), compilerOperand1 = LSB, compilerOperand2 = MSB
    // Output: Appropriate opcode emitted with value
    // Modifies: compilerOpCode, buffer state via EmitOpcodeWithWord()
    EmitPushWord()
    {
        // Select opcode based on type
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (Z)
        {
            LDA #OpcodeType.PUSHINT
            STA compilerOpCode
            EmitOpcodeWithWord();
            return;
        }
        
        CMP #BasicType.WORD
        if (Z)
        {
            LDA #OpcodeType.PUSHWORD
            STA compilerOpCode
            EmitOpcodeWithWord();
            return;
        }
        
        // Invalid type for word push
        LDA #(Messages.TypeMismatch % 256)
        STA ZP.LastErrorL
        LDA #(Messages.TypeMismatch / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
        CLC
    }
    
    // Emit PUSHGLOBAL opcode to load variable
    // Input: No parameters (uses current token position for variable name offset)
    // Output: PUSHGLOBAL opcode emitted with token offset
    // Modifies: compilerOpCode, compilerOperand1, ZP.ACC (via CalculateTokenOffset), buffer state
    EmitPushGlobal()
    {
        // Calculate offset to current token (variable name)
        CalculateTokenOffset();
        if (NC) { return; }
        
        // Check if offset fits in single byte (most common case)
        LDA ZP.ACCH
        if (Z) // High byte is 0, use single-byte operand
        {
            LDA ZP.ACCL
            STA compilerOperand1
            LDA #OpcodeType.PUSHGLOBAL
            STA compilerOpCode
            EmitOpcodeWithByte();
            return;
        }
        
        // Offset requires word operand - this should be rare
        // For now, generate error as we expect most programs to fit in 256 byte token buffer
        LDA #(Messages.BufferOverflow % 256)
        STA ZP.LastErrorL
        LDA #(Messages.BufferOverflow / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
        CLC
    }
    
    // Emit POPGLOBAL opcode to store to variable  
    // Input: No parameters (uses current token position for variable name offset)
    // Output: POPGLOBAL opcode emitted with token offset
    // Modifies: compilerOpCode, compilerOperand1, ZP.ACC (via CalculateTokenOffset), buffer state
    EmitPopGlobal()
    {
        // Calculate offset to current token (variable name)
        CalculateTokenOffset();
        if (NC) { return; }
        
        // Check if offset fits in single byte
        LDA ZP.ACCH
        if (Z) // High byte is 0, use single-byte operand
        {
            LDA ZP.ACCL
            STA compilerOperand1
            LDA #OpcodeType.POPGLOBAL
            STA compilerOpCode
            EmitOpcodeWithByte();
            return;
        }
        
        // Offset too large
        LDA #(Messages.BufferOverflow % 256)
        STA ZP.LastErrorL
        LDA #(Messages.BufferOverflow / 256)
        STA ZP.LastErrorH
        Messages.StorePC(); // 6502 PC -> IDY
        CLC
    }
    
    // Emit arithmetic operation opcode
    // Input: A = operation token (Tokens.PLUS, Tokens.MINUS, etc.)
    // Output: Corresponding arithmetic opcode emitted
    // Modifies: compilerOpCode, buffer state via EmitOpcode(), A/X/Y registers
    EmitArithmeticOp()
    {
        switch (A)
        {
            case Tokens.PLUS:
            {
                LDA #OpcodeType.ADD
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.MINUS:
            {
                LDA #OpcodeType.SUB
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.MULTIPLY:
            {
                LDA #OpcodeType.MUL
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.DIVIDE:
            {
                LDA #OpcodeType.DIV
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.MOD:
            {
                LDA #OpcodeType.MOD
                STA compilerOpCode
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
    // Modifies: compilerOpCode, buffer state via EmitOpcode(), A/X/Y registers
    EmitComparisonOp()
    {
        switch (A)
        {
            case Tokens.EQUALS:
            {
                LDA #OpcodeType.EQ
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.NOTEQUAL:
            {
                LDA #OpcodeType.NE
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.LT:
            {
                LDA #OpcodeType.LT
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.GT:
            {
                LDA #OpcodeType.GT
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.LE:
            {
                LDA #OpcodeType.LE
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.GE:
            {
                LDA #OpcodeType.GE
                STA compilerOpCode
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
    // Modifies: compilerOpCode, buffer state via EmitOpcode(), A/X/Y registers
    EmitLogicalOp()
    {
        switch (A)
        {
            case Tokens.AND:
            {
                LDA #OpcodeType.LOGICAL_AND
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.OR:
            {
                LDA #OpcodeType.LOGICAL_OR
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.NOT:
            {
                LDA #OpcodeType.LOGICAL_NOT
                STA compilerOpCode
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
    // Modifies: compilerOpCode, buffer state via EmitOpcode(), A/X/Y registers
    EmitBitwiseOp()
    {
        switch (A)
        {
            case Tokens.BITWISE_AND:
            {
                LDA #OpcodeType.BITWISE_AND
                STA compilerOpCode
                EmitOpcode();
                return;
            }
            case Tokens.BITWISE_OR:
            {
                LDA #OpcodeType.BITWISE_OR
                STA compilerOpCode
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
    // Modifies: compilerOpCode, buffer state via EmitOpcode()
    EmitUnaryMinus()
    {
        LDA #OpcodeType.NEG
        STA compilerOpCode
        EmitOpcode();
    }
    
    // Emit system call opcode
    // Input: A = system call ID
    // Output: SYSCALL opcode emitted with ID
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpcodeWithByte()
    EmitSysCall()
    {
        STA compilerOperand1      // Store ID as operand
        LDA #OpcodeType.SYSCALL
        STA compilerOpCode
        EmitOpcodeWithByte();
    }
    
    // Main entry point: Compile current expression to opcodes
    // Input: ZP.CurrentToken = first token of expression
    // Output: Expression compiled to opcode buffer, ZP.CurrentToken = token after expression
    // Modifies: Opcode buffer, ZP.CurrentToken, compilation state, ZP.TokenizerPos (via Tokenizer calls)
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
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
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
            if (NZ) { break; }
            
            // Get next token for right operand
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { return; }
            
            // Compile right operand
            compileLogicalAnd();
            Messages.CheckError();
            if (NC) { return; }
            
            // Emit logical OR opcode
            LDA #Tokens.OR
            EmitLogicalOp();
            if (NC) { return; }
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'L'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile logical AND operations
    // Input: ZP.CurrentToken = current token
    // Output: Logical AND opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    compileLogicalAnd()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'A'
        Tools.COut();
#endif
        
        // Compile left operand (higher precedence)
        compileComparison();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.AND
            if (NZ) { break; }
            
            // Get next token for right operand
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { return; }
            
            // Compile right operand
            compileComparison();
            Messages.CheckError();
            if (NC) { return; }
            
            // Emit logical AND opcode
            LDA #Tokens.AND
            EmitLogicalOp();
            if (NC) { return; }
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'A'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile comparison operations (=, <>, <, >, <=, >=)
    // Input: ZP.CurrentToken = current token
    // Output: Comparison opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
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
        
        // Compile left operand (higher precedence)
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
                case Tokens.LT:
                case Tokens.GT:
                case Tokens.LE:
                case Tokens.GE:
                {
                    PHA // Save operator on stack
                    
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) 
                    { 
                        PLA // Clean up stack
                        return; 
                    }
                    
                    // Compile right operand
                    compileBitwiseOr();
                    Messages.CheckError();
                    if (NC) 
                    { 
                        PLA // Clean up stack
                        return; 
                    }
                    
                    // Emit comparison opcode
                    PLA // Retrieve operator
                    EmitComparisonOp();
                    if (NC) { return; }
                    
                    continue; // Check for more comparisons
                }
                default:
                {
                    break; // Not a comparison operator
                }
            }
            break;
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile bitwise OR operations
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise OR opcodes emitted, ZP.CurrentToken = token after expression  
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    compileBitwiseOr()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'O'
        Tools.COut();
#endif
        
        // Compile left operand (higher precedence)
        compileAdditive();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.BITWISE_OR
            if (NZ) { break; }
            
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
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'O'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile additive operations (+, -)
    // Input: ZP.CurrentToken = current token
    // Output: Additive opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    compileAdditive()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'+'
        Tools.COut();
#endif
        
        // Compile left operand (higher precedence)
        compileMultiplicative();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.PLUS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile right operand
                compileMultiplicative();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit addition opcode
                LDA #Tokens.PLUS
                EmitArithmeticOp();
                if (NC) { return; }
                
                continue;
            }
            
            CMP #Tokens.MINUS
            if (Z)
            {
                // Get next token for right operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile right operand
                compileMultiplicative();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit subtraction opcode
                LDA #Tokens.MINUS
                EmitArithmeticOp();
                if (NC) { return; }
                
                continue;
            }
            
            break; // Not an additive operator
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'+'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile multiplicative operations (*, /, MOD)
    // Input: ZP.CurrentToken = current token
    // Output: Multiplicative opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    compileMultiplicative()
    {
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'*'
        Tools.COut();
#endif
        
        // Compile left operand (higher precedence)
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
                case Tokens.MOD:
                {
                    PHA // Save operator on stack
                    
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) 
                    { 
                        PLA // Clean up stack
                        return; 
                    }
                    
                    // Compile right operand
                    compileUnary();
                    Messages.CheckError();
                    if (NC) 
                    { 
                        PLA // Clean up stack
                        return; 
                    }
                    
                    // Emit arithmetic opcode
                    PLA // Retrieve operator
                    EmitArithmeticOp();
                    if (NC) { return; }
                    
                    continue; // Check for more multiplicative operations
                }
                default:
                {
                    break; // Not a multiplicative operator
                }
            }
            break;
        }
        
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'*'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
    }
    
    // Compile unary operations (-, NOT)
    // Input: ZP.CurrentToken = current token
    // Output: Unary opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
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
                // Get next token for operand
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { return; }
                
                // Compile the operand
                compilePrimary();
                Messages.CheckError();
                if (NC) { return; }
                
                // Emit unary minus opcode
                EmitUnaryMinus();
                if (NC) { return; }
            }
            case Tokens.NOT:
            {
                // Get next token for operand
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
            }
            default:
            {
                // Not unary, compile primary
                compilePrimary();
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
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), ZP.TOP/TOPT (via GetTokenNumber), ZP.ACC, buffer state, A/X/Y registers
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
                    CMP #BasicType.BIT
                    if (Z)
                    {
                        LDA ZP.TOPL // BIT values are single byte
                        EmitPushBit();
                        if (NC) { break; }
                    }
                    else
                    {
                        CMP #BasicType.BYTE
                        if (Z)
                        {
                            LDA ZP.TOPL
                            EmitPushByte();
                            if (NC) { break; }
                        }
                        else // 16-bit value (INT or WORD)
                        {
                            // Set up operands for word emission
                            LDA ZP.TOPL
                            STA compilerOperand1  // LSB
                            LDA ZP.TOPH
                            STA compilerOperand2  // MSB
                            
                            EmitPushWord();
                            if (NC) { break; }
                        }
                    }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                case Tokens.IDENTIFIER:
                {
                    // Emit push global variable opcode
                    EmitPushGlobal();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                case Tokens.LPAREN:
                {
                    // Get next token (start of sub-expression)
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Parse the sub-expression
                    compileComparison();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Expect closing parenthesis
                    LDA ZP.CurrentToken
                    CMP #Tokens.RPAREN
                    if (NZ)
                    {
                        LDA #(Messages.SyntaxError % 256)
                        STA ZP.LastErrorL
                        LDA #(Messages.SyntaxError / 256)
                        STA ZP.LastErrorH
                        
                        Messages.StorePC(); // 6502 PC -> IDY
                        
                        CLC  // Error
                        break;
                    }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                default:
                {
                    // Unexpected token
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    
                    Messages.StorePC(); // 6502 PC -> IDY
                    
                    CLC  // Error
                    break;
                }
            }
            
            CLC  // Error (we should never get here)
            break;
        } // Single exit point
        
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
