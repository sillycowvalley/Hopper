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
    
    // Private Compiler layer storage - BasicCompilerWorkspace (32 bytes)
    const uint compilerSavedTokenPosL = Address.BasicCompilerWorkspace;      // 1 byte - saved tokenizer pos low
    const uint compilerSavedTokenPosH = Address.BasicCompilerWorkspace + 1;  // 1 byte - saved tokenizer pos high
    const uint compilerLiteralOffsetL = Address.BasicCompilerWorkspace + 2;  // 1 byte - literal offset low
    const uint compilerLiteralOffsetH = Address.BasicCompilerWorkspace + 3;  // 1 byte - literal offset high
    const uint compilerOpCode         = Address.BasicCompilerWorkspace + 4;  // 1 byte - opcode to emit
    const uint compilerOperand1       = Address.BasicCompilerWorkspace + 5;  // 1 byte - first operand
    const uint compilerOperand2       = Address.BasicCompilerWorkspace + 6;  // 1 byte - second operand
    const uint compilerLastOpcode     = Address.BasicCompilerWorkspace + 7;  // 1 byte - last opcode emitted
    const uint compilerFuncArgs       = Address.BasicCompilerWorkspace + 8;  // 1 byte - number of arguments for current FUNC being compiled
    const uint compilerFuncLocals     = Address.BasicCompilerWorkspace + 9;  // 1 byte - number of locals for current FUNC being compiled
    const uint compilerLiteralBaseL   = Address.BasicCompilerWorkspace + 10; // 1 byte - literal base address low
    const uint compilerLiteralBaseH   = Address.BasicCompilerWorkspace + 11; // 1 byte - literal base address high

    
    
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
        
        LDA # OpcodeType.INVALID
        STA compilerLastOpcode
        
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
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
        STA compilerLastOpcode
        
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
                               LDA compilerOperand1 Tools.HOut(); LDA #' ' Tools.COut();
#endif
        // Check space for 2 bytes
        LDA #2
        CheckBufferSpace();
        if (NC) { return; } // Buffer overflow
        
        // Write opcode
        LDA compilerOpCode
        STA [ZP.PC]
        STA compilerLastOpcode
        
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
                               LDA compilerOperand1 Tools.HOut(); LDA #' ' Tools.COut();
                               LDA compilerOperand2 Tools.HOut(); LDA #' ' Tools.COut();
#endif        
        // Check space for 3 bytes
        LDA #3
        CheckBufferSpace();
        if (NC) { return; } // Buffer overflow
        
        // Write opcode
        LDA compilerOpCode
        STA [ZP.PC]
        STA compilerLastOpcode
        
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
    
    // Emit PUSHCSTRING opcode with word operand
    // Input: compilerOperand1 = string pointer LSB, compilerOperand2 = string pointer MSB  
    // Output: PUSHCSTRING opcode emitted with operands, C set if successful
    // Modifies: A, ZP.OpcodeBufferLengthL/H, buffer state
    EmitPushCString()
    {
        PHA
        
        // Set up opcode
        LDA #OpcodeType.PUSHCSTRING
        STA compilerOpCode
        
        // Emit opcode with word operand (uses compilerOperand1/2)
        EmitOpcodeWithWord();
        
        PLA
    }
    
    // Emit PUSHGLOBAL opcode for identifier
    // Input: Current token is IDENTIFIER
    // Output: PUSHGLOBAL opcode with node address emitted, C set if successful
    // Modifies: A, X, Y, ZP.TOP, ZP.IDX, compilerOperand1/2
    EmitPushGlobal()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit
        {
            // Get the identifier name from the tokenizer
            Tokenizer.GetTokenString(); // Result in ZP.TOP (name pointer)
            Messages.CheckError();
            if (NC) { break; }
            
            // Find the variable/constant by name
            STZ ZP.SymbolIteratorFilter  // Accept both variables and constants
            Variables.Find();  // Input: ZP.TOP = name, Output: ZP.IDX = node address
            if (NC)
            {
                // Variable not found
                LDA #(Messages.UndefinedIdentifier % 256)
                STA ZP.LastErrorL
                LDA #(Messages.UndefinedIdentifier / 256)
                STA ZP.LastErrorH
                
                BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                
                CLC
                break;
            }
            
            // Store node address as operands
            LDA ZP.IDXL
            STA compilerOperand1  // LSB
            LDA ZP.IDXH
            STA compilerOperand2  // MSB
            
            // Emit PUSHGLOBAL with word operand
            LDA # OpcodeType.PUSHGLOBAL
            STA compilerOpCode
            EmitOpcodeWithWord();
            break;
        }
        
        PLY
        PLX
        PLA
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
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
                InvalidOperatorError(); BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
                BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
                InvalidOperatorError(); BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
                BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
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
    
    // Emit ENTER opcode for function entry (stack frame setup)
    // Output: ENTER opcode with argument count emitted
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpcodeWithByte()
    EmitEnter()
    {
        LDA #OpcodeType.ENTER
        STA compilerOpCode
        EmitOpcode();
    }
    
    
    // Emit RETURN opcode for function exit (no return value)
    // Input: A = total stack slots to clean up (arguments + locals)
    // Output: RETURN opcode with cleanup count emitted
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpcodeWithByte()
    EmitReturn()
    {
        STA compilerOperand1          // Store cleanup count as operand
        LDA #OpcodeType.RETURN
        STA compilerOpCode
        EmitOpcodeWithByte();
    }
    
    // Emit RETURNVAL opcode for function exit with return value
    // Input: A = total stack slots to clean up (arguments + locals)
    // Output: RETURNVAL opcode with cleanup count emitted (expects return value on stack)
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpcodeWithByte()
    EmitReturnVal()
    {
        STA compilerOperand1          // Store cleanup count as operand
        LDA #OpcodeType.RETURNVAL
        STA compilerOpCode
        EmitOpcodeWithByte();
    }  
    
    // Emit CALL opcode for unresolved function call
    // Input: Current token is IDENTIFIER (function name), tokenizer positioned at function name
    // Output: CALL opcode with absolute name address emitted, C set if successful
    // Modifies: compilerOpCode, compilerOperand1/2, buffer state
    EmitCall()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit
        {
#ifdef DEBUG
            Tools.NL();
            LDA ZP.TokenLiteralPosH Tools.HOut();
            LDA ZP.TokenLiteralPosL Tools.HOut();
#endif            
            // Calculate absolute address of function name in token buffer
            // The tokenizer's TokenLiteralPos points to the start of the identifier string
            CLC
            LDA #(Address.BasicTokenizerBuffer % 256)
            ADC ZP.TokenLiteralPosL    // TokenLiteralPos points to identifier string
            STA compilerOperand1       // Absolute address LSB
            LDA #(Address.BasicTokenizerBuffer / 256)
            ADC ZP.TokenLiteralPosH
            STA compilerOperand2       // Absolute address MSB

#ifdef DEBUG
            LDA #'-' Tools.COut(); LDA #'>' Tools.COut();
            LDA compilerOperand2 Tools.HOut();
            LDA compilerOperand1 Tools.HOut();
#endif                        
                                   
            // Emit CALL with absolute address (not offset!)
            LDA # OpcodeType.CALL
            STA compilerOpCode
            EmitOpcodeWithWord();
            break;
        }
        
        PLY
        PLX
        PLA
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
    
    // Compile bitwise AND operations (&)
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise AND opcodes emitted, ZP.CurrentToken = token after expression  
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    compileBitwiseAnd()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'&'
        Tools.COut();
    #endif
        
        // Compile left operand (higher precedence)
        compileAdditive();
        Messages.CheckError();
        if (NC) { return; }
        
        loop
        {
            LDA ZP.CurrentToken
            CMP #Tokens.BITWISE_AND
            if (NZ) { break; }
            
            // Get next token for right operand
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { return; }
            
            // Compile right operand
            compileAdditive();
            Messages.CheckError();
            if (NC) { return; }
            
            // Emit bitwise AND opcode
            LDA #Tokens.BITWISE_AND
            EmitBitwiseOp();
            if (NC) { return; }
        }
        
    #ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'&'
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
        compileBitwiseAnd();
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
            compileBitwiseAnd();
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
    
    // Parse and compile function argument list
    // Input: ZP.CurrentToken = LPAREN (opening parenthesis)
    // Output: Arguments compiled and pushed to stack in correct order, ZP.CurrentToken = RPAREN
    // Modifies: ZP.CurrentToken, buffer state, compilation state
    compileArgumentList()
    {
        PHA
        PHX
        PHY
#ifdef DEBUG
        LDA #'[' Tools.COut();
#endif
        loop // Single exit
        {
            // Get token after opening parenthesis
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            // Check for empty argument list
            LDA ZP.CurrentToken
            CMP #Tokens.RPAREN
            if (Z)
            {
                SEC // Success - empty argument list
                break;
            }
            
            // Compile arguments separated by commas
            loop
            {
                // Compile argument expression
                compileComparison(); // Use full expression compilation
                Messages.CheckError();
                if (NC) { break; }
                
                // Check what comes next
                LDA ZP.CurrentToken
                CMP #Tokens.RPAREN
                if (Z)
                {
                    SEC // Success - end of argument list
                    break;
                }
                
                // Expect comma for more arguments
                CMP #Tokens.COMMA
                if (NZ)
                {
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC;
                    CLC
                    break;
                }
                
                // Get token after comma
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { break; }
                
                // Continue with next argument
            }
            
            break; // Exit outer loop
        }
#ifdef DEBUG
        LDA #']' Tools.COut(); // Exit from argument list  
#endif
        PLY
        PLX
        PLA
    }
    
    // Update compilePrimary() to handle function calls
    compileFunctionCallOrVariable()
    {
        PHA
        PHX
        PHY
#ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'V'
        Tools.COut();
#endif
        loop // Single exit
        {
            // Save current token position for potential function name resolution
            LDA ZP.TokenizerPosL
            STA (compilerSavedTokenPosL + 0)
            LDA ZP.TokenizerPosH
            STA (compilerSavedTokenPosH + 0)
            
            // Look ahead to see if this is a function call (identifier followed by '(')
            Tokenizer.NextToken(); // Get token after identifier
            Messages.CheckError();
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Tokens.LPAREN
            if (Z)
            {
#ifdef DEBUG
        LDA #'(' Tools.COut();
#endif
                // This is a function call - restore tokenizer to identifier and emit call
                LDA (compilerSavedTokenPosL + 0)
                STA ZP.TokenizerPosL
                LDA (compilerSavedTokenPosH + 0)
                STA ZP.TokenizerPosH
                
                // Get the identifier token back
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { break; }
                
                // Emit function call opcode
                EmitCall();
                if (NC) { break; }
                
                // Expect opening parenthesis
                LDA ZP.CurrentToken
                CMP #Tokens.LPAREN
                if (NZ)
                {
                    LDA #(Messages.ExpectedLeftParen % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.ExpectedLeftParen / 256)
                    STA ZP.LastErrorH
                    BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC;
                    CLC
                    break;
                }
                
                // Parse function arguments
                compileArgumentList();
                if (NC) { break; }
                
                // Expect closing parenthesis (should be current token after argument parsing)
                LDA ZP.CurrentToken
                CMP #Tokens.RPAREN
                if (NZ)
                {
                    LDA #(Messages.ExpectedRightParen % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.ExpectedRightParen / 256)
                    STA ZP.LastErrorH
                    BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC;
                    CLC
                    break;
                }
                
                // Get next token after closing parenthesis
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { break; }
#ifdef DEBUG
        LDA #')' Tools.COut();
#endif

            }
            else
            {
#ifdef DEBUG
        LDA #'V' Tools.COut();
#endif
                // Not a function call - restore position and emit variable push
                LDA (compilerSavedTokenPosL + 0)
                STA ZP.TokenizerPosL
                LDA (compilerSavedTokenPosH + 0)
                STA ZP.TokenizerPosH
                
                // Get the identifier token back
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { break; }
                
                // Emit push global variable opcode (existing functionality)
                EmitPushGlobal();
                if (NC) { break; }
            }
            
            SEC // Success
            break;
        }
#ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'V'
        Tools.COut();
        LDA #'>'
        Tools.COut();
#endif
        PLY
        PLX
        PLA
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
                case Tokens.TRUE:
                {
                    // Emit PUSHBIT with value 1
                    LDA #1
                    EmitPushBit();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                case Tokens.FALSE:
                {
                    // Emit PUSHBIT with value 0
                    LDA #0
                    EmitPushBit();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
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
                case Tokens.STRINGLIT:
                {
                    // Get string content pointer
                    Tokenizer.GetTokenString(); // Result in ZP.TOP
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // OFFSET : compiling STRINGLIT
                    // Emit PUSHCSTRING with pointer to string content
                    LDA ZP.TOPL
                    STA compilerOperand1  // LSB
                    LDA ZP.TOPH
                    STA compilerOperand2  // MSB
                    
                    EmitPushCString();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                
                case Tokens.IDENTIFIER:
                {
#ifdef DEBUG
                    LDA #'{' Tools.COut();
#endif
                    compileFunctionCallOrVariable();
#ifdef DEBUG
                    LDA #'}' Tools.COut();
#endif
                    break;
                }
                case Tokens.LPAREN:
                {
                    // Get next token (start of sub-expression)
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { break; }
                    
                    // Parse the sub-expression
                    compileLogical();
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
                        
                        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                        
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
                    
                    BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                    
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
    
    
    // Compile function body from tokens to opcodes  
    // Input: Function tokens already copied to BasicTokenizerBuffer, ZP.TokenBufferLength set, ZP.ACCL = number of arguments for FUNC
    // Output: Function compiled to opcode buffer, C set if successful
    // Modifies: Opcode buffer, ZP.CurrentToken, ZP.TokenizerPos, compilation state
    // Error: Sets ZP.LastError if compilation fails
    CompileFunction()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'N'
        Tools.COut();
    #endif

        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        STA compilerFuncArgs
        
        loop // Single exit block
        {
            // Initialize opcode buffer
            InitOpcodeBuffer();
            if (NC) { break; }
            
            // Reset tokenizer to start of function body
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            
            // Get first token of function body
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            EmitEnter();
            if (NC) { break; }
            
            STZ compilerFuncLocals // no locals yet
            
            // Compile statements until end of function
            loop // Statement compilation loop
            {
                // Check for end of function
                LDA ZP.CurrentToken
                CMP #Tokens.ENDFUNC
                if (Z) { break; } // End of regular function
                
                CMP #Tokens.END  
                if (Z) { break; } // End of BEGIN function
                
                CMP #Tokens.EOF
                if (Z) { break; } // End of token stream
                
                CMP #Tokens.EOL
                if (Z)
                {
                    // Skip empty lines
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    if (NC) { break; }
                    continue;
                }
                
                // Compile the statement
                compileStatement();
                Messages.CheckError();
                if (NC) { break; }
            }
            
            Messages.CheckError();
            if (NC) { break; }
            
            // Check if last opcode was RETURN or RETURNVAL
            checkLastOpcodeIsReturn();
            if (NC) // Last opcode was not RETURN
            {
                // Emit RETURN with locals cleanup count
                LDA compilerFuncLocals
                EmitReturn();
                if (NC) { break; }
            }
            
            SEC // Success
            break;
        }
        
        PLY
        PLX
        PLA
        
    #ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'N'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }

    // Compile a single statement within a function
    // Input: ZP.CurrentToken = first token of statement
    // Output: Statement compiled to opcodes, ZP.CurrentToken = token after statement  
    // Modifies: Opcode buffer, ZP.CurrentToken, compilation state
    // Error: Sets ZP.LastError if statement compilation fails
    compileStatement()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'S'
        Tools.COut();
    #endif

        loop // Single exit block
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.PRINT:
                {
                    compilePrintStatement();
                    break;
                }
                case Tokens.RETURN:
                {
                    compileReturnStatement();
                    break;
                }
                case Tokens.IF:
                {
                    compileIfStatement();
                    break;
                }
                case Tokens.IDENTIFIER:
                {
                    // Could be assignment or function call
                    compileIdentifierStatement();
                    break;
                }
                case Tokens.REM:
                case Tokens.COMMENT:
                {
                    // Skip comments - advance to next token
                    Tokenizer.NextToken();
                    Messages.CheckError();
                    break;
                }
                default:
                {
                    // TODO: Add more statement types as needed
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                    CLC
                    break;
                }
            }
            break;
        }
        
    #ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'S'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }

    // Compile PRINT statement
    // Input: ZP.CurrentToken = PRINT token
    // Output: PRINT statement compiled to opcodes
    // Modifies: Opcode buffer, ZP.CurrentToken, compilation state
    compilePrintStatement()
    {
    #ifdef DEBUG
        LDA #'<'
        Tools.COut();
        LDA #'P'
        Tools.COut();
        LDA #'S'
        Tools.COut();
    #endif

        loop // Single exit block
        {
            // Get next token (should be start of expression to print)
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            // Check for PRINT with no arguments (just newline)
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                // Emit system call for print newline
                LDA #SysCallType.PrintNewLine
                EmitSysCall();
                break;
            }
            
            // Compile the expression to print
            compileLogical(); // Use full expression compilation
            Messages.CheckError();
            if (NC) { break; }
            
            // Emit system call to print the value on stack
            LDA #SysCallType.PrintValue
            EmitSysCall();
            if (NC) { break; }
            
            // Emit system call for newline
            LDA #SysCallType.PrintNewLine  
            EmitSysCall();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
        
    #ifdef DEBUG
        LDA #'P'
        Tools.COut();
        LDA #'S'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }

    // Compile RETURN statement
    // Input: ZP.CurrentToken = RETURN token
    // Output: RETURN statement compiled to opcodes
    // Modifies: Opcode buffer, ZP.CurrentToken, compilation state
    compileReturnStatement()
    {
        // Get next token
        Tokenizer.NextToken();
        Messages.CheckError();
        if (NC) { return; }
        
        // Check if there's a return expression
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            // No return value - emit RETURN
            LDA #0  // No locals to clean up for now
            EmitReturn();
            return;
        }
        
        // Compile return expression
        compileLogical();
        Messages.CheckError();
        if (NC) { return; }
        
        // Emit RETURNVAL (expects value on stack)
        LDA #0  // No locals to clean up for now
        EmitReturnVal();
    }

    // Compile IF statement (stub for now)
    // Input: ZP.CurrentToken = IF token
    // Output: Error (not implemented)
    compileIfStatement()
    {
        // TODO: Implement IF statement compilation
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }

    // Compile identifier statement (assignment or function call)
    // Input: ZP.CurrentToken = IDENTIFIER token
    // Output: Statement compiled to opcodes
    compileIdentifierStatement()
    {
         // TODO: Implement assignment and function call compilation
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }

    // Check if the last emitted opcode is RETURN or RETURNVAL
    // Input: None (uses compilerLastOpcode tracking)
    // Output: C set if last opcode is RETURN/RETURNVAL, NC if not
    // Modifies: Processor flags only
    checkLastOpcodeIsReturn()
    {
        LDA compilerLastOpcode
        
        // Check if it's RETURN or RETURNVAL
        CMP #OpcodeType.RETURN
        if (Z)
        {
            SEC // Found RETURN
            return;
        }
        
        CMP #OpcodeType.RETURNVAL
        if (Z)
        {
            SEC // Found RETURNVAL
            return;
        }
        
        CLC // Not a RETURN opcode
    }
    
    // IDY -> compilerSavedTokenPos
    SetLiteralBase()
    {
        LDA ZP.IDYL
        STA compilerLiteralBaseL
        LDA ZP.IDYH
        STA compilerLiteralBaseH
    }
    
}
