unit Compiler 
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "OpCodes"
    uses "Messages"
    uses "Error"
    uses "State"
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
    const uint compilerLastOpCode     = Address.BasicCompilerWorkspace + 7;  // 1 byte - last opcode emitted
    const uint compilerFuncArgs       = Address.BasicCompilerWorkspace + 8;  // 1 byte - number of arguments for current FUNC being compiled
    const uint compilerFuncLocals     = Address.BasicCompilerWorkspace + 9;  // 1 byte - number of locals for current FUNC being compiled
    const uint compilerLiteralBaseL   = Address.BasicCompilerWorkspace + 10; // 1 byte - literal base address low
    const uint compilerLiteralBaseH   = Address.BasicCompilerWorkspace + 11; // 1 byte - literal base address high

    
    
    // Initialize the opcode buffer for compilation
    // Output: OpCode buffer ready for emission
    // Modifies: ZP.OpCodeBufferLengthL/H (set to 0), ZP.CompilerTokenPosL/H (set to current), ZP.CompilerFlags (cleared), ZP.PC (set to buffer start)
    const string initOpCodeBufferTrace = "InitOpBuf";
    InitOpCodeBuffer()
    {
#ifdef TRACE
        LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Clear opcode buffer length
        STZ ZP.OpCodeBufferLengthL
        STZ ZP.OpCodeBufferLengthH
        
        // Initialize PC to start of opcode buffer
        LDA #(Address.BasicOpCodeBuffer % 256)
        STA ZP.PCL
        LDA #(Address.BasicOpCodeBuffer / 256)
        STA ZP.PCH
        
        // Save current tokenizer position for literal references
        LDA ZP.TokenizerPosL
        STA ZP.CompilerTokenPosL
        LDA ZP.TokenizerPosH
        STA ZP.CompilerTokenPosH
        
        // Clear compiler flags
        STZ ZP.CompilerFlags
        
        LDA # OpCodeType.INVALID
        STA compilerLastOpCode
        
        SEC // Success
        
#ifdef TRACE
        LDA #(initOpCodeBufferTrace % 256) STA ZP.TraceMessageL LDA #(initOpCodeBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Check if there's space for N bytes in opcode buffer
    // Input: A = number of bytes needed
    // Output: C if space available, NC if buffer would overflow
    // Strategy: Increment buffer length first, then check bounds
    const string checkBufferSpaceTrace = "ChkBufSpc";
    CheckBufferSpace()
    {
#ifdef TRACE
        PHA LDA #(checkBufferSpaceTrace % 256) STA ZP.TraceMessageL LDA #(checkBufferSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        loop
        {
            // Add required bytes to current buffer length
            CLC
            ADC ZP.OpCodeBufferLengthL
            STA ZP.OpCodeBufferLengthL
            LDA ZP.OpCodeBufferLengthH
            ADC #0
            STA ZP.OpCodeBufferLengthH
            
            // Compare against buffer size (512 bytes = 0x0200)
            LDA ZP.OpCodeBufferLengthH
            CMP #0x02
            if (C) // >= 0x0200, overflow
            {
                Error.BufferOverflow(); BIT ZP.EmulatorPCL
                break;
            }
            
            SEC // Success - space available
            break;
        }
        
#ifdef TRACE
        LDA #(checkBufferSpaceTrace % 256) STA ZP.TraceMessageL LDA #(checkBufferSpaceTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit a single-byte opcode (no operands)
    // Input: compilerOpCode = opcode value
    // Output: OpCode written to buffer
    // Modifies: ZP.OpCodeBufferLengthL/H (incremented), ZP.PC (incremented)
    const string emitOpCodeTrace = "EmitOp";
    EmitOpCode()
    {
#ifdef TRACE
        LDA #(emitOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
#ifdef DEBUG       
            Tools.NL(); LDA #'>' Debug.COut();
            LDA ZP.PCH Debug.HOut(); LDA ZP.PCL Debug.HOut();
            LDA #' ' Debug.COut(); LDA compilerOpCode Debug.HOut(); LDA #' ' Debug.COut();
#endif        
            // Check space for 1 byte
            LDA #1
            CheckBufferSpace();
            if (NC) 
            { 
                break; 
            } // Buffer overflow
        
            // Write opcode to buffer
            LDA compilerOpCode
            STA [ZP.PC]
            STA compilerLastOpCode
            
            // Increment PC
            INC ZP.PCL
            if (Z)
            {
                INC ZP.PCH
            }
            
            SEC // Success
            break;
        }
#ifdef TRACE
        LDA #(emitOpCodeTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit opcode with one byte operand
    // Input: compilerOpCode = opcode value, compilerOperand1 = operand byte
    // Output: OpCode and operand written to buffer
    // Modifies: ZP.OpCodeBufferLengthL/H (incremented by 2), ZP.PC (incremented by 2)
    const string emitOpCodeWithByteTrace = "EmitOpByte";
    EmitOpCodeWithByte()
    {
#ifdef TRACE
        LDA #(emitOpCodeWithByteTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithByteTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
#ifdef DEBUG       
            Tools.NL(); LDA #'>' Debug.COut();
            LDA ZP.PCH Debug.HOut(); LDA ZP.PCL Debug.HOut();
            LDA #' ' Debug.COut(); LDA compilerOpCode Debug.HOut(); LDA #' ' Debug.COut(); 
                                   LDA compilerOperand1 Debug.HOut(); LDA #' ' Debug.COut();
#endif
            // Check space for 2 bytes
            LDA #2
            CheckBufferSpace();
            if (NC) 
            { 
                break; 
            } // Buffer overflow
        
            // Write opcode
            LDA compilerOpCode
            STA [ZP.PC]
            STA compilerLastOpCode
            
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
            break;
        }
        
#ifdef TRACE
        LDA #(emitOpCodeWithByteTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit opcode with two byte operands (word value)
    // Input: compilerOpCode = opcode value, compilerOperand1 = LSB, compilerOperand2 = MSB
    // Output: OpCode and operands written to buffer
    // Modifies: ZP.OpCodeBufferLengthL/H (incremented by 3), ZP.PC (incremented by 3)
    const string emitOpCodeWithWordTrace = "EmitOpWord";
    EmitOpCodeWithWord()
    {
#ifdef TRACE
        LDA #(emitOpCodeWithWordTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithWordTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
#ifdef DEBUG       
            Tools.NL(); LDA #'>' Debug.COut();
            LDA ZP.PCH Debug.HOut(); LDA ZP.PCL Debug.HOut();
            LDA #' ' Debug.COut(); LDA compilerOpCode Debug.HOut(); LDA #' ' Debug.COut(); 
                                   LDA compilerOperand1 Debug.HOut(); LDA #' ' Debug.COut();
                                   LDA compilerOperand2 Debug.HOut(); LDA #' ' Debug.COut();
#endif        
            // Check space for 3 bytes
            LDA #3
            CheckBufferSpace();
            if (NC) 
            { 
                break; 
            } // Buffer overflow
            
            // Write opcode
            LDA compilerOpCode
            STA [ZP.PC]
            STA compilerLastOpCode
            
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
            break;
        }
#ifdef TRACE
        LDA #(emitOpCodeWithWordTrace % 256) STA ZP.TraceMessageL LDA #(emitOpCodeWithWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Calculate offset from compilation start to current tokenizer position
    // Output: ZP.ACCL/ZP.ACCH = offset from ZP.CompilerTokenPos to current ZP.TokenizerPos
    const string calculateTokenOffsetTrace = "CalcTokOff";
    CalculateTokenOffset()
    {
#ifdef TRACE
        LDA #(calculateTokenOffsetTrace % 256) STA ZP.TraceMessageL LDA #(calculateTokenOffsetTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        // Calculate current tokenizer position - compilation start position
        SEC
        LDA ZP.TokenizerPosL
        SBC ZP.CompilerTokenPosL
        STA ZP.ACCL
        LDA ZP.TokenizerPosH
        SBC ZP.CompilerTokenPosH
        STA ZP.ACCH
        
        SEC // Success
        
#ifdef TRACE
        LDA #(calculateTokenOffsetTrace % 256) STA ZP.TraceMessageL LDA #(calculateTokenOffsetTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit PUSHCSTRING opcode with word operand
    // Input: compilerOperand1 = string pointer LSB, compilerOperand2 = string pointer MSB  
    // Output: PUSHCSTRING opcode emitted with operands, C set if successful
    // Modifies: A, ZP.OpCodeBufferLengthL/H, buffer state
    const string emitPushCStringTrace = "EmitPUSHCSTRING";
    EmitPushCString()
    {
        PHA
        
#ifdef TRACE
        LDA #(emitPushCStringTrace % 256) STA ZP.TraceMessageL LDA #(emitPushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
            // Set up opcode
            LDA #OpCodeType.PUSHCSTRING
            STA compilerOpCode
            
            // Emit opcode with word operand (uses compilerOperand1/2)
            EmitOpCodeWithWord();
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitPushCStringTrace % 256) STA ZP.TraceMessageL LDA #(emitPushCStringTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLA
    }
    
    // Emit PUSHGLOBAL opcode for identifier
    // Input: Current token is IDENTIFIER
    // Output: PUSHGLOBAL opcode with node address emitted, C set if successful
    // Modifies: A, X, Y, ZP.TOP, ZP.IDX, compilerOperand1/2
    const string emitPushGlobalTrace = "Emit PUSHGLOBAL";
    EmitPushGlobal()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(emitPushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop // Single exit
        {
            // Get the identifier name from the tokenizer
            Tokenizer.GetTokenString(); // Result in ZP.TOP (name pointer)
            Error.CheckError();
            if (NC) { break; }
            
            // Find the variable/constant by name
            STZ ZP.SymbolIteratorFilter  // Accept both variables and constants
            Variables.Find();  // Input: ZP.TOP = name, Output: ZP.IDX = node address
            if (NC)
            {
                // Variable not found
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Store node address as operands
            LDA ZP.IDXL
            STA compilerOperand1  // LSB
            LDA ZP.IDXH
            STA compilerOperand2  // MSB
            
            // Emit PUSHGLOBAL with word operand
            LDA # OpCodeType.PUSHGLOBAL
            STA compilerOpCode
            EmitOpCodeWithWord();
            break;
        }
        
#ifdef TRACE
        LDA #(emitPushGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPushGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }
    
    
    // Emit PUSHBIT opcode with immediate value
    // Input: A = bit value (0 or 1)
    // Output: PUSHBIT opcode emitted with value
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpCodeWithByte()
    const string emitPushBitTrace = "Emit PUSHBIT";
    EmitPushBit()
    {
#ifdef TRACE
        PHA LDA #(emitPushBitTrace % 256) STA ZP.TraceMessageL LDA #(emitPushBitTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        loop
        {
            // Validate bit value (must be 0 or 1)
            CMP #2
            if (C) // >= 2, invalid
            {
                Error.InvalidBitValue(); BIT ZP.EmulatorPCL
                break;
            }
        
            // Set up parameters for emission
            STA compilerOperand1          // Store value as operand
            LDA #OpCodeType.PUSHBIT
            STA compilerOpCode
            
            EmitOpCodeWithByte();
            break;
        }
#ifdef TRACE
        LDA #(emitPushBitTrace % 256) STA ZP.TraceMessageL LDA #(emitPushBitTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit PUSHBYTE opcode with immediate value
    // Input: A = byte value
    // Output: PUSHBYTE opcode emitted with value
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpCodeWithByte()
    const string emitPushByteTrace = "Emit PUSHBYTE";
    EmitPushByte()
    {
#ifdef TRACE
        PHA LDA #(emitPushByteTrace % 256) STA ZP.TraceMessageL LDA #(emitPushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        // Set up parameters for emission
        STA compilerOperand1          // Store value as operand
        LDA #OpCodeType.PUSHBYTE
        STA compilerOpCode
        
        EmitOpCodeWithByte();
        
#ifdef TRACE
        LDA #(emitPushByteTrace % 256) STA ZP.TraceMessageL LDA #(emitPushByteTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit PUSHINT or PUSHWORD opcode with word value
    // Input: ZP.TOPT = type (determines opcode), compilerOperand1 = LSB, compilerOperand2 = MSB
    // Output: Appropriate opcode emitted with value
    // Modifies: compilerOpCode, buffer state via EmitOpCodeWithWord()
    const string emitPushWordTrace = "Emit PUSHWORD";
    EmitPushWord()
    {
#ifdef TRACE
        LDA #(emitPushWordTrace % 256) STA ZP.TraceMessageL LDA #(emitPushWordTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
            // Select opcode based on type
            LDA ZP.TOPT
            CMP #BasicType.INT
            if (Z)
            {
                LDA #OpCodeType.PUSHINT
                STA compilerOpCode
                EmitOpCodeWithWord();
                break;
            }
            
            CMP #BasicType.WORD
            if (Z)
            {
                LDA #OpCodeType.PUSHWORD
                STA compilerOpCode
                EmitOpCodeWithWord();
                break;
            }
            
            // Invalid type for word push
            Error.TypeMismatch(); BIT ZP.EmulatorPCL
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitPushWordTrace % 256) STA ZP.TraceMessageL LDA #(emitPushWordTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit POPGLOBAL opcode to store to variable  
    // Input: No parameters (uses current token position for variable name offset)
    // Output: POPGLOBAL opcode emitted with token offset
    // Modifies: compilerOpCode, compilerOperand1, ZP.ACC (via CalculateTokenOffset), buffer state
    const string emitPopGlobalTrace = "Emit POPGLOBAL";
    EmitPopGlobal()
    {
#ifdef TRACE
        LDA #(emitPopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
            // Calculate offset to current token (variable name)
            CalculateTokenOffset();
            if (NC) { break; }
            
            // Check if offset fits in single byte
            LDA ZP.ACCH
            if (Z) // High byte is 0, use single-byte operand
            {
                LDA ZP.ACCL
                STA compilerOperand1
                LDA #OpCodeType.POPGLOBAL
                STA compilerOpCode
                EmitOpCodeWithByte();
                break;
            }
            
            // Offset too large
            Error.BufferOverflow(); BIT ZP.EmulatorPCL
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitPopGlobalTrace % 256) STA ZP.TraceMessageL LDA #(emitPopGlobalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Emit arithmetic operation opcode
    // Input: A = operation token (Tokens.PLUS, Tokens.MINUS, etc.)
    // Output: Corresponding arithmetic opcode emitted
    // Modifies: compilerOpCode, buffer state via EmitOpCode(), A/X/Y registers
    const string emitArithmeticOpTrace = "Emit ADD";
    EmitArithmeticOp()
    {
#ifdef TRACE
        PHA LDA #(emitArithmeticOpTrace % 256) STA ZP.TraceMessageL LDA #(emitArithmeticOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        loop
        {
            switch (A)
            {
                case Tokens.PLUS:
                {
                    LDA #OpCodeType.ADD
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.MINUS:
                {
                    LDA #OpCodeType.SUB
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.MULTIPLY:
                {
                    LDA #OpCodeType.MUL
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.DIVIDE:
                {
                    LDA #OpCodeType.DIV
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.MOD:
                {
                    LDA #OpCodeType.MOD
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitArithmeticOpTrace % 256) STA ZP.TraceMessageL LDA #(emitArithmeticOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit comparison operation opcode
    // Input: A = comparison token (Tokens.EQUALS, Tokens.LESSTHAN, etc.)
    // Output: Corresponding comparison opcode emitted
    // Modifies: compilerOpCode, buffer state via EmitOpCode(), A/X/Y registers
    const string emitComparisonOpTrace = "Emit EQ";
    EmitComparisonOp()
    {
#ifdef TRACE
        PHA LDA #(emitComparisonOpTrace % 256) STA ZP.TraceMessageL LDA #(emitComparisonOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        loop
        {
            switch (A)
            {
                case Tokens.EQUALS:
                {
                    LDA #OpCodeType.EQ
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.NOTEQUAL:
                {
                    LDA #OpCodeType.NE
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.LT:
                {
                    LDA #OpCodeType.LT
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.GT:
                {
                    LDA #OpCodeType.GT
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.LE:
                {
                    LDA #OpCodeType.LE
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.GE:
                {
                    LDA #OpCodeType.GE
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitComparisonOpTrace % 256) STA ZP.TraceMessageL LDA #(emitComparisonOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit logical operation opcode
    // Input: A = logical token (Tokens.AND, Tokens.OR, Tokens.NOT)
    // Output: Corresponding logical opcode emitted
    // Modifies: compilerOpCode, buffer state via EmitOpCode(), A/X/Y registers
    const string emitLogicalOpTrace = "Emit LOGICAL_AND";
    EmitLogicalOp()
    {
#ifdef TRACE
        PHA LDA #(emitLogicalOpTrace % 256) STA ZP.TraceMessageL LDA #(emitLogicalOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        loop
        {
            switch (A)
            {
                case Tokens.AND:
                {
                    LDA #OpCodeType.LOGICAL_AND
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.OR:
                {
                    LDA #OpCodeType.LOGICAL_OR
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.NOT:
                {
                    LDA #OpCodeType.LOGICAL_NOT
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitLogicalOpTrace % 256) STA ZP.TraceMessageL LDA #(emitLogicalOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit bitwise operation opcode
    // Input: A = bitwise token 
    // Output: Corresponding bitwise opcode emitted
    // Modifies: compilerOpCode, buffer state via EmitOpCode(), A/X/Y registers
    const string emitBitwiseOpTrace = "Emit BITWISE_AND";
    EmitBitwiseOp()
    {
#ifdef TRACE
        PHA LDA #(emitBitwiseOpTrace % 256) STA ZP.TraceMessageL LDA #(emitBitwiseOpTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        loop
        {
            switch (A)
            {
                case Tokens.BITWISE_AND:
                {
                    LDA #OpCodeType.BITWISE_AND
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                case Tokens.BITWISE_OR:
                {
                    LDA #OpCodeType.BITWISE_OR
                    STA compilerOpCode
                    EmitOpCode();
                    break;
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            break;
        } // loop
        
#ifdef TRACE
        LDA #(emitBitwiseOpTrace % 256) STA ZP.TraceMessageL LDA #(emitBitwiseOpTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit unary minus (negation) opcode
    // Output: NEG opcode emitted
    // Modifies: compilerOpCode, buffer state via EmitOpCode()
    const string emitUnaryMinusTrace = "Emit NEG";
    EmitUnaryMinus()
    {
#ifdef TRACE
        LDA #(emitUnaryMinusTrace % 256) STA ZP.TraceMessageL LDA #(emitUnaryMinusTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        LDA #OpCodeType.NEG
        STA compilerOpCode
        EmitOpCode();
        
#ifdef TRACE
        LDA #(emitUnaryMinusTrace % 256) STA ZP.TraceMessageL LDA #(emitUnaryMinusTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit system call opcode
    // Input: A = system call ID
    // Output: SYSCALL opcode emitted with ID
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpCodeWithByte()
    const string emitSysCallTrace = "Emit SYSCALL";
    EmitSysCall()
    {
#ifdef TRACE
        PHA LDA #(emitSysCallTrace % 256) STA ZP.TraceMessageL LDA #(emitSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        STA compilerOperand1      // Store ID as operand
        LDA #OpCodeType.SYSCALL
        STA compilerOpCode
        EmitOpCodeWithByte();
        
#ifdef TRACE
        LDA #(emitSysCallTrace % 256) STA ZP.TraceMessageL LDA #(emitSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit ENTER opcode for function entry (stack frame setup)
    // Output: ENTER opcode with argument count emitted
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpCodeWithByte()
    const string emitEnterTrace = "Emit ENTER";
    EmitEnter()
    {
#ifdef TRACE
        LDA #(emitEnterTrace % 256) STA ZP.TraceMessageL LDA #(emitEnterTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        LDA #OpCodeType.ENTER
        STA compilerOpCode
        EmitOpCode();
        
#ifdef TRACE
        LDA #(emitEnterTrace % 256) STA ZP.TraceMessageL LDA #(emitEnterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Emit RETURN opcode for function exit (no return value)
    // Input: A = total stack slots to clean up (arguments + locals)
    // Output: RETURN opcode with cleanup count emitted
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpCodeWithByte()
    const string emitReturnTrace = "Emit RETURN";
    EmitReturn()
    {
#ifdef TRACE
        PHA LDA #(emitReturnTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        STA compilerOperand1          // Store cleanup count as operand
        LDA #OpCodeType.RETURN
        STA compilerOpCode
        EmitOpCodeWithByte();
        
#ifdef TRACE
        LDA #(emitReturnTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Emit RETURNVAL opcode for function exit with return value
    // Input: A = total stack slots to clean up (arguments + locals)
    // Output: RETURNVAL opcode with cleanup count emitted (expects return value on stack)
    // Modifies: compilerOpCode, compilerOperand1, buffer state via EmitOpCodeWithByte()
    const string emitReturnValTrace = "Emit RETURNVAL";
    EmitReturnVal()
    {
#ifdef TRACE
        PHA LDA #(emitReturnValTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnValTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        STA compilerOperand1          // Store cleanup count as operand
        LDA #OpCodeType.RETURNVAL
        STA compilerOpCode
        EmitOpCodeWithByte();
        
#ifdef TRACE
        LDA #(emitReturnValTrace % 256) STA ZP.TraceMessageL LDA #(emitReturnValTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }  
    
    // Emit CALL opcode for unresolved function call
    // Input: Current token is IDENTIFIER (function name), tokenizer positioned at function name
    // Output: CALL opcode with absolute name address emitted, C set if successful
    // Modifies: compilerOpCode, compilerOperand1/2, buffer state
    const string emitCallTrace = "Emit CALL";
    EmitCall()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(emitCallTrace % 256) STA ZP.TraceMessageL LDA #(emitCallTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop // Single exit
        {
#ifdef DEBUG
            Tools.NL();
            LDA ZP.TokenLiteralPosH Debug.HOut();
            LDA ZP.TokenLiteralPosL Debug.HOut();
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
            LDA #'-' Debug.COut(); LDA #'>' Debug.COut();
            LDA compilerOperand2 Debug.HOut();
            LDA compilerOperand1 Debug.HOut();
#endif                        
                                   
            // Emit CALL with absolute address (not offset!)
            LDA # OpCodeType.CALL
            STA compilerOpCode
            EmitOpCodeWithWord();
            break;
        }
        
#ifdef TRACE
        LDA #(emitCallTrace % 256) STA ZP.TraceMessageL LDA #(emitCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }
    
    
    
    // Compile logical OR operations (lowest precedence)
    // Input: ZP.CurrentToken = current token
    // Output: Logical opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileLogicalTrace = "CompLog // OR";
    compileLogical()
    {
#ifdef TRACE
        LDA #(compileLogicalTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'L'
        Debug.COut();
#endif
        loop
        {
            // Compile left operand (higher precedence)
            compileLogicalAnd();
            Error.CheckError();
            if (NC) { break; }
            
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.OR
                if (NZ) { break; }
                
                // Get next token for right operand
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Compile right operand
                compileLogicalAnd();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit logical OR opcode
                LDA #Tokens.OR
                EmitLogicalOp();
                Error.CheckError();
                if (NC) { break; }
            } // loop
            break;
        } // loop
            
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'L'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileLogicalTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile logical AND operations
    // Input: ZP.CurrentToken = current token
    // Output: Logical AND opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileLogicalAndTrace = "CompAnd // AND";
    compileLogicalAnd()
    {
#ifdef TRACE
        LDA #(compileLogicalAndTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalAndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'A'
        Debug.COut();
#endif
        
        loop
        {
            // Compile left operand (higher precedence)
            compileComparison();
            Error.CheckError();
            if (NC) { break; }
            
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.AND
                if (NZ) { break; }
                
                // Get next token for right operand
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Compile right operand
                compileComparison();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit logical AND opcode
                LDA #Tokens.AND
                EmitLogicalOp();
                Error.CheckError();
                if (NC) { break; }
            } // loop
            break;
        } // loop
        
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'A'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileLogicalAndTrace % 256) STA ZP.TraceMessageL LDA #(compileLogicalAndTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile comparison operations (=, <>, <, >, <=, >=)
    // Input: ZP.CurrentToken = current token
    // Output: Comparison opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileComparisonTrace = "CompCmp // = | <> | < > <= >=";
    compileComparison()
    {
#ifdef TRACE
        LDA #(compileComparisonTrace % 256) STA ZP.TraceMessageL LDA #(compileComparisonTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'C'
        Debug.COut();
#endif
        
        loop
        {
            // Compile left operand (higher precedence)
            compileBitwiseOr();
            Error.CheckError();
            if (NC) { break; }
            
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
                        Error.CheckError();
                        if (NC) 
                        { 
                            PLA // Clean up stack
                            break; 
                        }
                        
                        // Compile right operand
                        compileBitwiseOr();
                        Error.CheckError();
                        if (NC) 
                        { 
                            PLA // Clean up stack
                            break; 
                        }
                        
                        // Emit comparison opcode
                        PLA // Retrieve operator
                        EmitComparisonOp();
                        Error.CheckError();
                        if (NC) { break; }
                        
                        continue; // Check for more comparisons
                    }
                    default:
                    {
                        break; // Not a comparison operator
                    }
                }
                break;
            } // loop
            break;
        } // loop
        
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileComparisonTrace % 256) STA ZP.TraceMessageL LDA #(compileComparisonTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile bitwise AND operations (&)
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise AND opcodes emitted, ZP.CurrentToken = token after expression  
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileBitwiseAndTrace = "CompBitAnd // &";
    compileBitwiseAnd()
    {
#ifdef TRACE
        LDA #(compileBitwiseAndTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseAndTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'&'
        Debug.COut();
    #endif
        
        loop
        {
            // Compile left operand (higher precedence)
            compileAdditive();
            Error.CheckError();
            if (NC) { break; }
            
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.BITWISE_AND
                if (NZ) { break; }
                
                // Get next token for right operand
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Compile right operand
                compileAdditive();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit bitwise AND opcode
                LDA #Tokens.BITWISE_AND
                EmitBitwiseOp();
                Error.CheckError();
                if (NC) { break; }
            } // loop
            break;
        } // loop
        
    #ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'&'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif

#ifdef TRACE
        LDA #(compileBitwiseAndTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseAndTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile bitwise OR operations
    // Input: ZP.CurrentToken = current token
    // Output: Bitwise OR opcodes emitted, ZP.CurrentToken = token after expression  
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileBitwiseOrTrace = "CompBitOr // |";
    compileBitwiseOr()
    {
#ifdef TRACE
        LDA #(compileBitwiseOrTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseOrTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'O'
        Debug.COut();
#endif
        
        loop
        {
            // Compile left operand (higher precedence)
            compileBitwiseAnd();
            Error.CheckError();
            if (NC) { break; }
            
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.BITWISE_OR
                if (NZ) { break; }
                
                // Get next token for right operand
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Compile right operand
                compileBitwiseAnd();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit bitwise OR opcode
                LDA #Tokens.BITWISE_OR
                EmitBitwiseOp();
                Error.CheckError();
                if (NC) { break; }
            } // loop
            break;
        } // loop
        
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'O'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileBitwiseOrTrace % 256) STA ZP.TraceMessageL LDA #(compileBitwiseOrTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile additive operations (+, -)
    // Input: ZP.CurrentToken = current token
    // Output: Additive opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileAdditiveTrace = "CompAdd // +";
    compileAdditive()
    {
#ifdef TRACE
        LDA #(compileAdditiveTrace % 256) STA ZP.TraceMessageL LDA #(compileAdditiveTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'+'
        Debug.COut();
#endif
        
        loop
        {
            // Compile left operand (higher precedence)
            compileMultiplicative();
            Error.CheckError();
            if (NC) { break; }
            
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.PLUS
                if (Z)
                {
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Compile right operand
                    compileMultiplicative();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Emit addition opcode
                    LDA #Tokens.PLUS
                    EmitArithmeticOp();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    continue;
                }
                
                CMP #Tokens.MINUS
                if (Z)
                {
                    // Get next token for right operand
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Compile right operand
                    compileMultiplicative();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Emit subtraction opcode
                    LDA #Tokens.MINUS
                    EmitArithmeticOp();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    continue;
                }
                
                break; // Not an additive operator
            } // loop
            break;
        } // loop
        
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'+'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileAdditiveTrace % 256) STA ZP.TraceMessageL LDA #(compileAdditiveTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile multiplicative operations (*, /, MOD)
    // Input: ZP.CurrentToken = current token
    // Output: Multiplicative opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileMultiplicativeTrace = "CompMult // *";
    compileMultiplicative()
    {
#ifdef TRACE
        LDA #(compileMultiplicativeTrace % 256) STA ZP.TraceMessageL LDA #(compileMultiplicativeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'*'
        Debug.COut();
#endif
        
        loop
        {
            // Compile left operand (higher precedence)
            compileUnary();
            Error.CheckError();
            if (NC) { break; }
            
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
                        Error.CheckError();
                        if (NC) 
                        { 
                            PLA // Clean up stack
                            break; 
                        }
                        
                        // Compile right operand
                        compileUnary();
                        Error.CheckError();
                        if (NC) 
                        { 
                            PLA // Clean up stack
                            break; 
                        }
                        
                        // Emit arithmetic opcode
                        PLA // Retrieve operator
                        EmitArithmeticOp();
                        Error.CheckError();
                        if (NC) { break; }
                        
                        continue; // Check for more multiplicative operations
                    }
                    default:
                    {
                        break; // Not a multiplicative operator
                    }
                }
                break;
            } // loop
            break;
        } // loop
        
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'*'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileMultiplicativeTrace % 256) STA ZP.TraceMessageL LDA #(compileMultiplicativeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Compile unary operations (-, NOT)
    // Input: ZP.CurrentToken = current token
    // Output: Unary opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), buffer state, A/X/Y registers
    const string compileUnaryTrace = "CompUnary // -";
    compileUnary()
    {
#ifdef TRACE
        LDA #(compileUnaryTrace % 256) STA ZP.TraceMessageL LDA #(compileUnaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'U'
        Debug.COut();
#endif
        
        loop
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.MINUS:
                {
                    // Get next token for operand
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Compile the operand
                    compilePrimary();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Emit unary minus opcode
                    EmitUnaryMinus();
                    Error.CheckError();
                    if (NC) { break; }
                }
                case Tokens.NOT:
                {
                    // Get next token for operand
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Compile the operand
                    compilePrimary();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Emit logical NOT opcode
                    LDA #Tokens.NOT
                    EmitLogicalOp();
                    Error.CheckError();
                    if (NC) { break; }
                }
                default:
                {
                    // Not unary, compile primary
                    compilePrimary();
                    Error.CheckError();
                    if (NC) { break; }
                }
            }
            break;
        } // loop
        
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'U'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileUnaryTrace % 256) STA ZP.TraceMessageL LDA #(compileUnaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Parse and compile function argument list
    // Input: ZP.CurrentToken = LPAREN (opening parenthesis)
    // Output: Arguments compiled and pushed to stack in correct order, ZP.CurrentToken = RPAREN
    // Modifies: ZP.CurrentToken, buffer state, compilation state
    const string compileArgumentListTrace = "CompArgs // (...)";
    compileArgumentList()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(compileArgumentListTrace % 256) STA ZP.TraceMessageL LDA #(compileArgumentListTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'[' Debug.COut();
#endif
        loop // Single exit
        {
            // Get token after opening parenthesis
            Tokenizer.NextToken();
            Error.CheckError();
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
                Error.CheckError();
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
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get token after comma
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Continue with next argument
            }
            
            break; // Exit outer loop
        }
#ifdef DEBUG
        LDA #']' Debug.COut(); // Exit from argument list  
#endif

#ifdef TRACE
        LDA #(compileArgumentListTrace % 256) STA ZP.TraceMessageL LDA #(compileArgumentListTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }
    
    // Update compilePrimary() to handle function calls
    const string compileFunctionCallOrVariableTrace = "CompFuncVar // <identifier>";
    compileFunctionCallOrVariable()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
#ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'F'
        Debug.COut();
        LDA #'V'
        Debug.COut();
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
            Error.CheckError();
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Tokens.LPAREN
            if (Z)
            {
#ifdef DEBUG
        LDA #'(' Debug.COut();
#endif
                // This is a function call - restore tokenizer to identifier and emit call
                LDA (compilerSavedTokenPosL + 0)
                STA ZP.TokenizerPosL
                LDA (compilerSavedTokenPosH + 0)
                STA ZP.TokenizerPosH
                
                // Get the identifier token back
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit function call opcode
                EmitCall();
                Error.CheckError();
                if (NC) { break; }
                
                // Expect opening parenthesis
                LDA ZP.CurrentToken
                CMP #Tokens.LPAREN
                if (NZ)
                {
                    Error.ExpectedLeftParen(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Parse function arguments
                compileArgumentList();
                Error.CheckError();
                if (NC) { break; }
                
                // Expect closing parenthesis (should be current token after argument parsing)
                LDA ZP.CurrentToken
                CMP #Tokens.RPAREN
                if (NZ)
                {
                    Error.ExpectedRightParen(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get next token after closing parenthesis
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
#ifdef DEBUG
        LDA #')' Debug.COut();
#endif

            }
            else
            {
#ifdef DEBUG
        LDA #'V' Debug.COut();
#endif
                // Not a function call - restore position and emit variable push
                LDA (compilerSavedTokenPosL + 0)
                STA ZP.TokenizerPosL
                LDA (compilerSavedTokenPosH + 0)
                STA ZP.TokenizerPosH
                
                // Get the identifier token back
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Emit push global variable opcode (existing functionality)
                EmitPushGlobal();
                Error.CheckError();
                if (NC) { break; }
            }
            
            SEC // Success
            break;
        }
#ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'F'
        Debug.COut();
        LDA #'V'
        Debug.COut();
        LDA #'>'
        Debug.COut();
#endif

#ifdef TRACE
        LDA #(compileFunctionCallOrVariableTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionCallOrVariableTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }
    
    // Compile primary expressions (numbers, identifiers, parentheses)
    // Input: ZP.CurrentToken = current token
    // Output: Primary opcodes emitted, ZP.CurrentToken = token after expression
    // Modifies: ZP.CurrentToken, ZP.TokenizerPos (via Tokenizer calls), ZP.TOP/TOPT (via GetTokenNumber), ZP.ACC, buffer state, A/X/Y registers
    const string compilePrimaryTrace = "CompPrim // <literal>";
    compilePrimary()
    {
#ifdef TRACE
        LDA #(compilePrimaryTrace % 256) STA ZP.TraceMessageL LDA #(compilePrimaryTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
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
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    break;
                }
                case Tokens.FALSE:
                {
                    // Emit PUSHBIT with value 0
                    LDA #0
                    EmitPushBit();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    break;
                }
                case Tokens.NUMBER:
                {
                    // Get number value and type
                    Tokenizer.GetTokenNumber(); // Result in ZP.TOP, type in ZP.TOPT
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Emit appropriate push opcode based on type and value
                    LDA ZP.TOPT
                    CMP #BasicType.BIT
                    if (Z)
                    {
                        LDA ZP.TOPL // BIT values are single byte
                        EmitPushBit();
                        Error.CheckError();
                        if (NC) { break; }
                    }
                    else
                    {
                        CMP #BasicType.BYTE
                        if (Z)
                        {
                            LDA ZP.TOPL
                            EmitPushByte();
                            Error.CheckError();
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
                            Error.CheckError();
                            if (NC) { break; }
                        }
                    }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    break;
                }
                case Tokens.STRINGLIT:
                {
                    // Get string content pointer
                    Tokenizer.GetTokenString(); // Result in ZP.TOP
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // OFFSET : compiling STRINGLIT
                    // Emit PUSHCSTRING with pointer to string content
                    LDA ZP.TOPL
                    STA compilerOperand1  // LSB
                    LDA ZP.TOPH
                    STA compilerOperand2  // MSB
                    
                    EmitPushCString();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    break;
                }
                
                case Tokens.IDENTIFIER:
                {
#ifdef DEBUG
                    LDA #'{' Debug.COut();
#endif
                    compileFunctionCallOrVariable();
                    Error.CheckError();
#ifdef DEBUG
                    LDA #'}' Debug.COut();
#endif
                    break;
                }
                case Tokens.LPAREN:
                {
                    // Get next token (start of sub-expression)
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Parse the sub-expression
                    compileLogical();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    // Expect closing parenthesis
                    LDA ZP.CurrentToken
                    CMP #Tokens.RPAREN
                    if (NZ)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        break;
                    }
                    
                    // Get next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    break;
                }
                default:
                {
                    // Unexpected token
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            
            break; // Normal exit point
        } // Single exit point

#ifdef TRACE
        LDA #(compilePrimaryTrace % 256) STA ZP.TraceMessageL LDA #(compilePrimaryTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Main entry point: Compile current expression to opcodes
    // Input: ZP.CurrentToken = first token of expression
    // Output: Expression compiled to opcode buffer, ZP.CurrentToken = token after expression
    // Modifies: OpCode buffer, ZP.CurrentToken, compilation state, ZP.TokenizerPos (via Tokenizer calls)
    const string strCompileExpression = "CompExpr";
    CompileExpression()
    {
#ifdef TRACE
        LDA #(strCompileExpression % 256) STA ZP.TraceMessageL LDA #(strCompileExpression / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        // Initialize opcode buffer if this is the start of compilation
        InitOpCodeBuffer();
        Error.CheckError();
        if (NC) { State.SetFailure(); return; }
        
        // Compile the expression using same precedence as Expression.asm
        compileLogical();
        Error.CheckError();
        if (NC) { State.SetFailure(); return; }
        
        State.SetSuccess();

#ifdef TRACE
        LDA #(strCompileExpression % 256) STA ZP.TraceMessageL LDA #(strCompileExpression / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Compile function body from tokens to opcodes  
    // Input: Function tokens already copied to BasicTokenizerBuffer, ZP.TokenBufferLength set, ZP.ACCL = number of arguments for FUNC
    // Output: Function compiled to opcode buffer, SystemState set
    // Modifies: OpCode buffer, ZP.CurrentToken, ZP.TokenizerPos, compilation state
    // Error: Sets ZP.LastError if compilation fails
    const string compileFunctionTrace = "CompFunc // FUNC";
    CompileFunction()
    {
        PHA
        PHX
        PHY
        
#ifdef TRACE
        LDA #(compileFunctionTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'F'
        Debug.COut();
        LDA #'N'
        Debug.COut();
    #endif

        LDA ZP.ACCL
        STA compilerFuncArgs
        
        loop // Single exit block
        {
            // Initialize opcode buffer
            InitOpCodeBuffer();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Reset tokenizer to start of function body
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            
            // Get first token of function body
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            EmitEnter();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
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
                    Error.CheckError();
                    if (NC) { State.SetFailure(); break; }
                    continue;
                }
                
                // Compile the statement
                compileStatement();
                Error.CheckError();
                if (NC) { State.SetFailure(); break; }
            }
            
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Check if last opcode was RETURN or RETURNVAL
            checkLastOpCodeIsReturn();
            if (NC) // Last opcode was not RETURN
            {
                // Emit RETURN with locals cleanup count
                LDA compilerFuncLocals
                EmitReturn();
                Error.CheckError();
                if (NC) { State.SetFailure(); break; }
            }
            
            State.SetSuccess(); // Success
            break;
        }
        
    #ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'F'
        Debug.COut();
        LDA #'N'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif

#ifdef TRACE
        LDA #(compileFunctionTrace % 256) STA ZP.TraceMessageL LDA #(compileFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLY
        PLX
        PLA
    }

    // Compile a single statement within a function
    // Input: ZP.CurrentToken = first token of statement
    // Output: Statement compiled to opcodes, ZP.CurrentToken = token after statement  
    // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
    // Error: Sets ZP.LastError if statement compilation fails
    const string compileStatementTrace = "CompStmt // <statement>";
    compileStatement()
    {
#ifdef TRACE
        LDA #(compileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'S'
        Debug.COut();
    #endif

        loop // Single exit block
        {
            LDA ZP.CurrentToken
            switch (A)
            {
                case Tokens.PRINT:
                {
                    compilePrintStatement();
                    Error.CheckError();
                    if (NC) { State.SetFailure(); break; }
                }
                case Tokens.RETURN:
                {
                    compileReturnStatement();
                    Error.CheckError();
                    if (NC) { State.SetFailure(); break; }
                }
                case Tokens.IF:
                {
                    compileIfStatement();
                    Error.CheckError();
                    if (NC) { State.SetFailure(); break; }
                }
                case Tokens.IDENTIFIER:
                {
                    // Could be assignment or function call
                    compileIdentifierStatement();
                    Error.CheckError();
                    if (NC) { State.SetFailure(); break; }
                }
                case Tokens.REM:
                case Tokens.COMMENT:
                {
                    // Skip comments - advance to next token
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { State.SetFailure(); break; }
                }
                default:
                {
                    // TODO: Add more statement types as needed
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    State.SetFailure();
                    break;
                }
            }
            
            State.SetSuccess();
            break;
        } // loop
        
    #ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'S'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif

#ifdef TRACE
        LDA #(compileStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Compile PRINT statement
    // Input: ZP.CurrentToken = PRINT token
    // Output: PRINT statement compiled to opcodes
    // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
    const string compilePrintStatementTrace = "CompPrint // PRINT";
    compilePrintStatement()
    {
#ifdef TRACE
        LDA #(compilePrintStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePrintStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'P'
        Debug.COut();
        LDA #'S'
        Debug.COut();
    #endif

        loop // Single exit block
        {
            // Get next token (should be start of expression to print)
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Check for PRINT with no arguments (just newline)
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                // Emit system call for print newline
                LDA #SysCallType.PrintNewLine
                EmitSysCall();
                Error.CheckError();
                if (NC) { State.SetFailure(); break; }
                State.SetSuccess();
                break;
            }
            
            // Compile the expression to print
            compileLogical(); // Use full expression compilation
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Emit system call to print the value on stack
            LDA #SysCallType.PrintValue
            EmitSysCall();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Emit system call for newline
            LDA #SysCallType.PrintNewLine  
            EmitSysCall();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            State.SetSuccess(); // Success
            break;
        } // loop
        
    #ifdef DEBUG
        LDA #'P'
        Debug.COut();
        LDA #'S'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif

#ifdef TRACE
        LDA #(compilePrintStatementTrace % 256) STA ZP.TraceMessageL LDA #(compilePrintStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Compile RETURN statement
    // Input: ZP.CurrentToken = RETURN token
    // Output: RETURN statement compiled to opcodes
    // Modifies: OpCode buffer, ZP.CurrentToken, compilation state
    const string compileReturnStatementTrace = "CompReturn // RETURN";
    compileReturnStatement()
    {
#ifdef TRACE
        LDA #(compileReturnStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileReturnStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
            // Get next token
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Check if there's a return expression
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                // No return value - emit RETURN
                LDA #0  // No locals to clean up for now
                EmitReturn();
                Error.CheckError();
                if (NC) { State.SetFailure(); break; }
                State.SetSuccess();
                break;
            }
            
            // Compile return expression
            compileLogical();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            // Emit RETURNVAL (expects value on stack)
            LDA #0  // No locals to clean up for now
            EmitReturnVal();
            Error.CheckError();
            if (NC) { State.SetFailure(); break; }
            
            State.SetSuccess();
            break;
        } // loop

#ifdef TRACE
        LDA #(compileReturnStatementTrace % 256) STA ZP.TraceMessageL LDA #(compileReturnStatementTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }

    // Compile IF statement (stub for now)
    // Input: ZP.CurrentToken = IF token
    // Output: Error (not implemented)
    compileIfStatement()
    {
        // TODO: Implement IF statement compilation
        Error.NotImplemented(); BIT ZP.EmulatorPCL
        State.SetFailure();
    }

    // Compile identifier statement (assignment or function call)
    // Input: ZP.CurrentToken = IDENTIFIER token
    // Output: Statement compiled to opcodes
    compileIdentifierStatement()
    {
         // TODO: Implement assignment and function call compilation
        Error.NotImplemented(); BIT ZP.EmulatorPCL
        State.SetFailure();
    }

    // Check if the last emitted opcode is RETURN or RETURNVAL
    // Input: None (uses compilerLastOpCode tracking)
    // Output: C set if last opcode is RETURN/RETURNVAL, NC if not
    // Modifies: Processor flags only
    const string checkLastOpCodeIsReturnTrace = "ChkReturn";
    checkLastOpCodeIsReturn()
    {
#ifdef TRACE
        LDA #(checkLastOpCodeIsReturnTrace % 256) STA ZP.TraceMessageL LDA #(checkLastOpCodeIsReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            LDA compilerLastOpCode
            
            // Check if it's RETURN or RETURNVAL
            CMP #OpCodeType.RETURN
            if (Z)
            {
                SEC // Found RETURN
                break;
            }
            
            CMP #OpCodeType.RETURNVAL
            if (Z)
            {
                SEC // Found RETURNVAL
                break;
            }
            
            CLC // Not a RETURN opcode
            break;
        }
        
#ifdef TRACE
        LDA #(checkLastOpCodeIsReturnTrace % 256) STA ZP.TraceMessageL LDA #(checkLastOpCodeIsReturnTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // IDY -> compilerSavedTokenPos
    const string setLiteralBaseTrace = "SetLitBase";
    SetLiteralBase()
    {
#ifdef TRACE
        LDA #(setLiteralBaseTrace % 256) STA ZP.TraceMessageL LDA #(setLiteralBaseTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        LDA ZP.IDYL
        STA compilerLiteralBaseL
        LDA ZP.IDYH
        STA compilerLiteralBaseH
        
#ifdef TRACE
        LDA #(setLiteralBaseTrace % 256) STA ZP.TraceMessageL LDA #(setLiteralBaseTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
}