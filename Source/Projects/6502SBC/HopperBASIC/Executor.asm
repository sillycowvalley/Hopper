unit Executor 
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "OpCodes"
    uses "Variables"
    uses "Messages"
    uses "BasicTypes"
    uses "Instructions"
    uses "ComparisonInstructions"
    
    friend Functions;
    
    // Memory layout for executor state - BasicProcessBuffer3 (0x09E9-0x09FF, 23 bytes)
    const uint executorStartAddrL    = Address.BasicProcessBuffer3 + 9;   // 0x09E9: opcode buffer start low
    const uint executorStartAddrH    = Address.BasicProcessBuffer3 + 10;  // 0x09EA: opcode buffer start high
    const uint executorEndAddrL      = Address.BasicProcessBuffer3 + 11;  // 0x09EB: opcode buffer end low
    const uint executorEndAddrH      = Address.BasicProcessBuffer3 + 12;  // 0x09EC: opcode buffer end high
    const uint executorOperandL      = Address.BasicProcessBuffer3 + 13;  // 0x09ED: current operand low
    const uint executorOperandH      = Address.BasicProcessBuffer3 + 14;  // 0x09EE: current operand high
    const uint executorTokenAddrL    = Address.BasicProcessBuffer3 + 15;  // 0x09EF: token fetch addr low
    const uint executorTokenAddrH    = Address.BasicProcessBuffer3 + 16;  // 0x09F0: token fetch addr high
    // 15 bytes remaining for future executor needs (0x09F1-0x09FF)
    
    
    // Main entry point - Execute compiled opcodes
    // Input: ZP.OpcodeBufferLengthL/H contains opcode buffer length
    // Output: C set if successful, NC if error occurred
    // Uses: BasicOpcodeBuffer at Address.BasicOpcodeBuffer (0x0C00)
    ExecuteOpcodes()
    {
        PHA
        PHX
        PHY
        
        // CRITICAL: Reset stacks before execution to ensure clean state
        // Previous expression errors or interruptions could leave stacks inconsistent
        STZ ZP.SP    // Reset value/type stack pointer to 0
        STZ ZP.BP
        
        loop // Single exit block
        {
            // Initialize executor state
            InitExecutor();
            if (NC) 
            { 
                CLC // Error already set by InitExecutor
                break; 
            }
            
            // Main execution loop
            loop
            {
                // Check if we've reached end of opcodes
                LDA ZP.PCL
                CMP executorEndAddrL
                if (NZ) { /* continue */ }
                else
                {
                    LDA ZP.PCH
                    CMP executorEndAddrH
                    if (Z) 
                    { 
                        SEC // Success - reached end
                        break; 
                    }
                }
                
                // Fetch and execute next opcode
                FetchOpcode();
                if (NC) 
                { 
                    break; // Error fetching opcode
                }
                
                // Dispatch opcode (A contains opcode value)
                DispatchOpcode();
                if (NC) 
                { 
                    break; // Error executing opcode
                }
            }
            break;
        } // Single exit block
        
        PLY
        PLX
        PLA
    }
    
    // Initialize executor state from opcode buffer
    // Input: ZP.OpcodeBufferLengthL/H contains opcode buffer length
    // Output: C set if successful, NC if error
    InitExecutor()
    {
        // Set start address to BasicOpcodeBuffer
        LDA #(Address.BasicOpcodeBuffer % 256)
        STA executorStartAddrL
        STA ZP.PCL // Start execution at beginning
        LDA #(Address.BasicOpcodeBuffer / 256)
        STA executorStartAddrH
        STA ZP.PCH
        
        // Calculate end address = start + length
        CLC
        LDA executorStartAddrL
        ADC ZP.OpcodeBufferLengthL
        STA executorEndAddrL
        LDA executorStartAddrH
        ADC ZP.OpcodeBufferLengthH
        STA executorEndAddrH
        
        // Validate buffer length is not zero
        LDA ZP.OpcodeBufferLengthL
        ORA ZP.OpcodeBufferLengthH
        if (Z)
        {
            LDA #(Messages.InternalError % 256)
            STA ZP.LastErrorL
            LDA #(Messages.InternalError / 256)
            STA ZP.LastErrorH
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            CLC
            return;
        }
        
        SEC // Success
    }
    
    // Fetch next opcode from buffer
    // Input: ZP.PC points to current position
    // Output: A contains opcode, ZP.PC advanced, C set if success, NC if bounds error
    FetchOpcode()
    {
        // Check bounds
        LDA ZP.PCL
        CMP executorEndAddrL
        if (NZ) { /* continue */ }
        else
        {
            LDA ZP.PCH
            CMP executorEndAddrH
            if (Z) 
            { 
                // At end of buffer
                LDA #(Messages.InternalError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InternalError / 256)
                STA ZP.LastErrorH
                BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                CLC
                return; 
            }
        }
        
        // Fetch opcode using indirect addressing
        LDY #0
        LDA [ZP.PC], Y
        PHA // Save opcode
        
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        PLA // Restore opcode to A
        SEC // Success
    }
    
    // Fetch single byte operand from buffer
    // Input: ZP.PC points to operand position
    // Output: A contains operand byte, ZP.PC advanced, C set if success
    FetchOperandByte()
    {
        // Check bounds
        LDA ZP.PCL
        CMP executorEndAddrL
        if (NZ) { /* continue */ }
        else
        {
            LDA ZP.PCH
            CMP executorEndAddrH
            if (Z) 
            { 
                LDA #(Messages.InternalError % 256)
                STA ZP.LastErrorL
                LDA #(Messages.InternalError / 256)
                STA ZP.LastErrorH
                BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                CLC
                return; 
            }
        }
        
        // Fetch operand
        LDY #0
        LDA [ZP.PC], Y
        PHA // Save operand
        
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        PLA // Restore operand to A
        SEC // Success
    }
    
    // Fetch word operand from buffer (little-endian)
    // Input: ZP.PC points to operand position
    // Output: executorOperandL/H contains word, ZP.PC advanced by 2, C set if success
    FetchOperandWord()
    {
        // Fetch low byte
        FetchOperandByte();
        if (NC) { return; }
        STA executorOperandL
        
        // Fetch high byte
        FetchOperandByte();
        if (NC) { return; }
        STA executorOperandH
        
        SEC // Success
    }
    
    // Dispatch opcode to appropriate handler
    // Input: A contains opcode value
    // Output: Execution continues (errors detected via Messages.CheckError())
    DispatchOpcode()
    {
        TAY // for jump table optimization
        
#ifdef DEBUG       
        PHA
        Tools.NL(); LDA #']' Tools.COut();
        LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
        LDA #' ' Tools.COut(); PLA Tools.HOut(); LDA #' ' Tools.COut();
#endif
        
        // Use switch statement for opcode dispatch
        // Register Y contains the opcode value
        switch (Y)
        {
            // === NO OPERAND OPCODES (0x00-0x3F) ===
            
            // Arithmetic operations
            case OpcodeType.ADD:
            {
                Instructions.Addition();
            }
            case OpcodeType.SUB:
            {
                Instructions.Subtraction();
            }
            case OpcodeType.MUL:
            {
                Instructions.Multiply();
            }
            case OpcodeType.DIV:
            {
                Instructions.Divide();
            }
            case OpcodeType.MOD:
            {
                Instructions.Modulo();
            }
            case OpcodeType.NEG:
            {
                Instructions.UnaryMinus();
            }
            
            // Bitwise operations
            case OpcodeType.BITWISE_AND:
            {
                Instructions.BitwiseAnd();
            }
            case OpcodeType.BITWISE_OR:
            {
                Instructions.BitwiseOr();
            }
            
            // Logical operations (BIT operands only)
            case OpcodeType.LOGICAL_AND:
            {
                Instructions.LogicalAnd();
            }
            case OpcodeType.LOGICAL_OR:
            {
                Instructions.LogicalOr();
            }
            case OpcodeType.LOGICAL_NOT:
            {
                Instructions.LogicalNot();
            }
            
            // Comparison operations (all return BIT)
            case OpcodeType.EQ:
            {
                ComparisonInstructions.Equal();
            }
            case OpcodeType.NE:
            {
                ComparisonInstructions.NotEqual();
            }
            case OpcodeType.LT:
            {
                ComparisonInstructions.LessThan();
            }
            case OpcodeType.GT:
            {
                ComparisonInstructions.GreaterThan();
            }
            case OpcodeType.LE:
            {
                ComparisonInstructions.LessEqual();
            }
            case OpcodeType.GE:
            {
                ComparisonInstructions.GreaterEqual();
            }
            
            // Function operations
            case OpcodeType.ENTER:
            {
                executeEnter();
            }
            case OpcodeType.RETURN:
            {
                executeReturn();
            }
            case OpcodeType.RETURNVAL:
            {
                executeReturnVal();
            }
            
            // Stack manipulation
            case OpcodeType.DECSP:
            {
                executeDecSp();
            }
            case OpcodeType.DUP:
            {
                executeDup();
            }
            case OpcodeType.NOP:
            {
                executeNop();
            }
            
            // === ONE BYTE OPERAND OPCODES (0x40-0x7F) ===
            
            // Literal pushes (8-bit)
            case OpcodeType.PUSHBIT:
            {
                executePushBit();
            }
            case OpcodeType.PUSHBYTE:
            {
                executePushByte();
            }
            case OpcodeType.PUSHCSTRING:
            {
                executePushCString();
            }
            
            // Variable operations
            case OpcodeType.PUSHGLOBAL:
            {
                executePushGlobal();
            }
            case OpcodeType.PUSHLOCAL:
            {
                executePushLocal();
            }
            case OpcodeType.POPGLOBAL:
            {
                executePopGlobal();
            }
            case OpcodeType.POPLOCAL:
            {
                executePopLocal();
            }
            
            // Control flow (short jumps)
            case OpcodeType.JUMPB:
            {
                executeJumpB();
            }
            case OpcodeType.JUMPZB:
            {
                executeJumpZB();
            }
            case OpcodeType.JUMPNZB:
            {
                executeJumpNZB();
            }
            
            // Function and system calls
            case OpcodeType.CALL:
            {
                executeCall();
            }
            case OpcodeType.CALLF:
            {
                executeCallF();
            }
            case OpcodeType.SYSCALL:
            {
                executeSysCall();
            }
            
            // === TWO BYTE OPERAND OPCODES (0x80-0xBF) ===
            
            // Literal pushes (16-bit)
            case OpcodeType.PUSHINT:
            {
                executePushInt();
            }
            case OpcodeType.PUSHWORD:
            {
                executePushWord();
            }
            
            // Control flow (long jumps)
            case OpcodeType.JUMPW:
            {
                executeJumpW();
            }
            case OpcodeType.JUMPZW:
            {
                executeJumpZW();
            }
            case OpcodeType.JUMPNZW:
            {
                executeJumpNZW();
            }
            default:
            {
                executeNotImplemented();
            }
        }
    }
    
    executeNotImplemented()
    {
#ifdef DEBUG
        LDA #'?' Tools.COut();
        TYA Tools.HOut();
        Tools.DumpBasicBuffers();
#endif                
        // Unknown opcode
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    // === CONTROL FLOW AND STACK MANIPULATION HANDLERS ===
    
    executeReturn()
    {
        // TODO: Implement function return (no return value)
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executeReturnVal()
    {
        // TODO: Implement function return with value (pop return value from stack)
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executeEnter()
    {
        Stacks.PushBP();
        LDA ZP.BP
        STA ZP.SP
        SEC
    }
    
    executeDecSp()
    {
        // Decrement stack pointer (discard top value)
        DEC ZP.SP
        SEC // Success
    }
    
    executeDup()
    {
        // Duplicate top stack value
        Stacks.PopTop(); // Get top value in ZP.TOP and ZP.TOPT
        LDA ZP.TOPT
        Stacks.PushTop(); // Push it back
        LDA ZP.TOPT  
        Stacks.PushTop(); // Push duplicate
        SEC // Success
    }
    
    executeNop()
    {
        // No operation - do nothing
        SEC // Success
    }
    
    // === LITERAL PUSH HANDLERS (ONE BYTE OPERAND) ===
    
    executePushBit()
    {
        // Fetch operand byte
        FetchOperandByte();
        if (NC) { return; }
        
        // Store in ZP.TOP as BIT value (0 or 1)
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        LDA # BasicType.BIT
        Stacks.PushTop();
        
        SEC // Success
    }
    
    executePushByte()
    {
        // Fetch operand byte
        FetchOperandByte();
        if (NC) { return; }
        
        // Store in ZP.TOP as BYTE value
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        LDA # BasicType.BYTE
        Stacks.PushTop();
        
        SEC // Success
    }
    
    // Execute PUSHCSTRING opcode - push string pointer to stack
    // Input: PC points to operand bytes (string pointer LSB, MSB)
    // Output: String pointer pushed to stack as STRING type, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.TOP, ZP.TOPT, stack
    executePushCString()
    {
        // Fetch string pointer (little-endian)
        FetchOperandWord(); // Result in executorOperandL/H
        Messages.CheckError();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        // Store pointer in ZP.TOP as STRING value
        LDA executorOperandL
        STA ZP.TOPL
        LDA executorOperandH
        STA ZP.TOPH
        LDA #BasicType.STRING
        STA ZP.TOPT
        
        // Push to stack with STRING type
        Stacks.PushTop();
        Messages.CheckError();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        SEC // Success
    }
    
    // === VARIABLE OPERATION HANDLERS (ONE BYTE OPERAND) ===
    
    // Execute PUSHGLOBAL opcode - push global variable value
    // Input: PC points to operand bytes (node address LSB, MSB)
    // Output: Variable value pushed to stack, PC advanced by 2
    // Modifies: A, X, Y, ZP.PC, ZP.IDX, ZP.TOP, ZP.TOPT, stack
    executePushGlobal()
    {
        // Fetch node address (little-endian)
        FetchOperandWord(); // Result in executorOperandL/H
        Messages.CheckError();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        // Transfer node address to ZP.IDX
        LDA executorOperandL
        STA ZP.IDXL
        LDA executorOperandH
        STA ZP.IDXH
        
        // Get the variable's value and type
        Variables.GetValue(); // Input: ZP.IDX, Output: ZP.TOP = value, ZP.TOPT = type
        Messages.CheckError();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        // Push value to stack with type
        LDA ZP.TOPT
        Stacks.PushTop(); // Push value and type to stack
        Messages.CheckError();
        if (NC) 
        { 
            CLC
            return; 
        }
        
        SEC // Success
    }    
    executePushLocal()
    {
        // TODO: Fetch local variable by BP offset and push value
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executePopGlobal()
    {
        // TODO: Pop value and store in global variable by index
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executePopLocal()
    {
        // TODO: Pop value and store in local variable by BP offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    // === CONTROL FLOW HANDLERS (ONE BYTE OPERAND) ===
    
    executeJumpB()
    {
        // TODO: Unconditional jump with signed byte offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executeJumpZB()
    {
        // TODO: Jump if zero with signed byte offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executeJumpNZB()
    {
        // TODO: Jump if non-zero with signed byte offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    // === FUNCTION AND SYSTEM CALL HANDLERS (ONE BYTE OPERAND) ===
    
    executeCall()
    {
    #ifdef DEBUG
        LDA #'C' Tools.COut(); LDA #'A' Tools.COut(); LDA #'L' Tools.COut(); LDA #'L' Tools.COut();
    #endif
        
        loop
        {
            FetchOperandWord();
            if (NC) { break; }
    
    #ifdef DEBUG
            LDA #'@' Tools.COut(); 
            LDA executorOperandH Tools.HOut(); 
            LDA executorOperandL Tools.HOut();
    #endif
        
            LDA executorOperandL
            STA ZP.TOPL
            LDA executorOperandH
            STA ZP.TOPH
            
            // 1. resolve Function <index> to function call <address>
            Functions.Find(); // Input: ZP.TOP = name
            if (NC)
            {
    #ifdef DEBUG
                LDA #'?' Tools.COut(); LDA #'F' Tools.COut();
    #endif
                LDA #(Messages.UndefinedIdentifier % 256)
                STA ZP.LastErrorL
                LDA #(Messages.UndefinedIdentifier / 256)
                STA ZP.LastErrorH
                BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
                CLC
                break;
            }
            // ZP.IDX = function node address
    
    #ifdef DEBUG
            LDA #'F' Tools.COut(); LDA #'=' Tools.COut();
            LDA ZP.IDXH Tools.HOut(); LDA ZP.IDXL Tools.HOut();
    #endif
            
            Functions.IsCompiled();
            if (NC)
            {
    #ifdef DEBUG
                LDA #'J' Tools.COut(); LDA #'I' Tools.COut(); LDA #'T' Tools.COut();
    #endif
                // JIT
                Functions.Compile();
                if (NC)
                {
                    break; // compilation failed
                }
            }
            
    #ifdef DEBUG
            LDA #'P' Tools.COut(); LDA #'A' Tools.COut(); LDA #'T' Tools.COut(); LDA #'C' Tools.COut(); LDA #'H' Tools.COut();
            LDA #'@' Tools.COut();
            LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
    #endif
            
            // 2. replace own opcode CALL -> CALLF, <index> by <address>
            // 3, PC -= 3 (so CALLF happens instead)
            //
            // <address> MSB
            // PC--
            LDA ZP.PCL
            if (Z)
            {
                DEC ZP.PCH
            }
            DEC ZP.PCL
            LDA ZP.IDXH 
            STA [ZP.PC]
            
    #ifdef DEBUG
            LDA #'M' Tools.COut(); LDA #'=' Tools.COut();
            LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
            LDA #':' Tools.COut(); LDA ZP.IDXH Tools.HOut();
    #endif
            
            // <address> LSB
            // PC--
            LDA ZP.PCL
            if (Z)
            {
                DEC ZP.PCH
            }
            DEC ZP.PCL
            LDA ZP.IDXL
            STA [ZP.PC]
            
    #ifdef DEBUG
            LDA #'L' Tools.COut(); LDA #'=' Tools.COut();
            LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
            LDA #':' Tools.COut(); LDA ZP.IDXL Tools.HOut();
    #endif
            
            // CALLF
            // PC--
            LDA ZP.PCL
            if (Z)
            {
                DEC ZP.PCH
            }
            DEC ZP.PCL
            LDA # OpcodeType.CALLF
            STA [ZP.PC]
            
    #ifdef DEBUG
            LDA #'R' Tools.COut(); LDA #'=' Tools.COut();
            LDA ZP.PCH Tools.HOut(); LDA ZP.PCL Tools.HOut();
            LDA #':' Tools.COut(); LDA # OpcodeType.CALLF Tools.HOut();
    #endif
            
            SEC
            break;
        } // loop
    }
    
    executeCallF()
    {
    #ifdef DEBUG
        LDA #'C' Tools.COut(); LDA #'A' Tools.COut(); LDA #'L' Tools.COut(); LDA #'L' Tools.COut(); LDA #'F' Tools.COut();
    #endif
        
        // Function call by <address>
        // PUSH PC
        // PUSH BP
        // PC = <address>
        Functions.JumpToOpCodes();
    }
    
    executeSysCall()
    {
        // TODO: System call (0x01=PRINT, 0x02=PRINTLN, etc.)
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    // === LITERAL PUSH HANDLERS (TWO BYTE OPERANDS) ===
    
    executePushInt()
    {
        // Fetch 16-bit operand
        FetchOperandWord();
        if (NC) { return; }
        
        // Store in ZP.TOP as INT value
        LDA executorOperandL
        STA ZP.TOPL
        LDA executorOperandH
        STA ZP.TOPH
        LDA # BasicType.INT
        Stacks.PushTop();
        
        SEC // Success
    }
    
    executePushWord()
    {
        // Fetch 16-bit operand
        FetchOperandWord();
        if (NC) { return; }
        
        // Store in ZP.TOP as WORD value
        LDA executorOperandL
        STA ZP.TOPL
        LDA executorOperandH
        STA ZP.TOPH
        LDA # BasicType.WORD
        Stacks.PushTop();
        
        SEC // Success
    }
    
    // === CONTROL FLOW HANDLERS (TWO BYTE OPERANDS) ===
    
    executeJumpW()
    {
        // TODO: Unconditional jump with signed word offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executeJumpZW()
    {
        // TODO: Jump if zero with signed word offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
    
    executeJumpNZW()
    {
        // TODO: Jump if non-zero with signed word offset
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
        CLC
    }
}
