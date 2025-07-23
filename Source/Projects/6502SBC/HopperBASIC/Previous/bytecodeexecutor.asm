unit BytecodeExecutor
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Stacks"
    uses "OpCodes"
    uses "BytecodeCompiler"
    uses "FunctionManager" 
    uses "Tools"
    uses "GlobalManager"
    
    // Fetch next byte from bytecode and advance PC
    executorFetchByte()
    {
        // Calculate address: CODESTART + PC
        CLC
        LDA ZP.CODESTARTL
        ADC ZP.PCL
        STA ZP.IDXL
        LDA ZP.CODESTARTH
        ADC ZP.PCH
        STA ZP.IDXH
        
        // Fetch the byte
        LDY #0
        LDA [ZP.IDX], Y
        
        // Advance PC
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
        
        // Return value in A
    }
    
    // Fetch next 16-bit word (little-endian) and advance PC
    executorFetchWord()
    {
        // Fetch LSB
        executorFetchByte();
        STA ZP.TOPL
        
        // Fetch MSB
        executorFetchByte();
        STA ZP.TOPH
        
        // Return value in TOP
    }
    
    executorFetchVariableName()
    {
        // Fetch name length
        executorFetchByte();
        STA ZP.BasicWorkspace7    // Temporarily store length
        
        // Build null-terminated string in BasicWorkBuffer
        LDX #0
        loop
        {
            CPX ZP.BasicWorkspace7    // Compare with stored length
            if (Z) { break; }
            
            executorFetchByte();
            STA Address.BasicWorkBuffer, X
            INX
        }
        
        // Add null terminator
        LDA #0
        STA Address.BasicWorkBuffer, X
        
        // Set IDY to point to the null-terminated name
        LDA #(Address.BasicWorkBuffer & 0xFF)
        STA ZP.IDYL
        LDA #(Address.BasicWorkBuffer >> 8)
        STA ZP.IDYH
    }
    
    // Opcode handlers
    handleNop()
    {
        // Do nothing
    }
    
    handlePushInt()
{
    // Load 16-bit constant and its type onto value stack
    executorFetchWord();  // Gets constant into TOP
    
    // Fetch the type byte
    executorFetchByte();  // Gets type into A
    
    Stacks.PushTop();  // Push value with type from A
}
    
    handlePrintInt()
{
    // Pop integer from stack and print it
    Stacks.PopTop();  // This gets both value and type
    
    // Check the type to determine if we should print as signed or unsigned
    LDA ZP.TOPT
    CMP #Types.Int
    if (Z)  // Signed integer
    {
        // Check if this is a negative number (bit 15 set)
        BIT ZP.TOPH
        if (MI)  // Negative number
        {
            // Print minus sign
            LDA #'-'
            Serial.WriteChar();
            
            // Negate the number: TOP = 0 - TOP
            SEC
            LDA #0
            SBC ZP.TOPL
            STA ZP.TOPL
            LDA #0
            SBC ZP.TOPH
            STA ZP.TOPH
        }
    }
    // For UInt, Byte, Bool - just print as unsigned
    
    // Print the (now positive if it was negative) number
    Tools.PrintDecimalWord();
}
    
    handlePrintStr()
    {
        // SILENT FAILURE #1: String printing not implemented
        BRK // String printing not implemented
        LDA #'S'
        Serial.WriteChar();
        LDA #'T'
        Serial.WriteChar();
        LDA #'R'
        Serial.WriteChar();
    }
    
    handlePrintNL()
    {
        // Print newline
        LDA #'\n'
        Serial.WriteChar();
    }
    
    handleLoadVar()
{
    // Fetch variable name from bytecode
    executorFetchVariableName();
    
    // Look up the variable using the fetched name
    GlobalManager.FindGlobal();
    if (Z)  // Found
    {
        // Get the variable's value and push it
        GlobalManager.GetGlobalValue();  // Returns value in TOP, type in FTYPE
        
        // Convert GlobalManager type to runtime type
        LDA ZP.FTYPE
        AND #0x7F  // Clear constant flag to get base type
        switch (A)
        {
            case GlobalTypes.VarInt:
            {
                LDA #Types.Int
            }
            case GlobalTypes.VarWord:
            {
                LDA #Types.UInt
            }
            case GlobalTypes.VarByte:
            {
                LDA #Types.Byte
            }
            case GlobalTypes.VarBit:
            {
                LDA #Types.Bool
            }
            default:
            {
                LDA #Types.UInt  // Default
            }
        }
        Stacks.PushTop();
    }
    else
    {
        STZ ZP.TOPL
        STZ ZP.TOPH
        LDA #Types.UInt
        Stacks.PushTop();
        // Variable not found
        LDA #(Interpreter.msgUndefinedVariable % 256)
        STA ZP.LastErrorL
        LDA #(Interpreter.msgUndefinedVariable / 256)
        STA ZP.LastErrorH
    }
}
    
    handleStoreVar()
    {
        // Fetch variable name from bytecode
        executorFetchVariableName();
        
        // Look up the variable using the fetched name
        GlobalManager.FindGlobal();
        if (Z)  // Found
        {
            // Check if it's a constant (can't assign to constants)
            GlobalManager.GetGlobalValue();  // Returns type in FTYPE, IDX unchanged
            LDA ZP.FTYPE
            GlobalManager.IsConstant();
            if (NC)  // It's a variable, not a constant
            {
                // Pop value from stack
                Stacks.PopTop();
                
                // Find the value offset (skip past null-terminated name)
                LDY # GlobalManager.ghName
                loop
                {
                    LDA [ZP.IDX], Y
                    if (Z) 
                    { 
                        INY  // Move past null terminator
                        break; 
                    }
                    INY
                }
                // Y now points to type byte, skip to value
                INY
                
                // Store the new value
                LDA ZP.TOPL
                STA [ZP.IDX], Y
                INY
                LDA ZP.TOPH
                STA [ZP.IDX], Y
            }
            else
            {
                Stacks.PopTop();
                // Can't assign to constant - set error
                LDA #(Interpreter.msgCannotAssignConstant % 256)
                STA ZP.LastErrorL
                LDA #(Interpreter.msgCannotAssignConstant / 256)
                STA ZP.LastErrorH
                return;
            }
        }
        else
        {
            // Variable not found
            Stacks.PopTop();
            LDA #(Interpreter.msgUndefinedVariable % 256)
            STA ZP.LastErrorL
            LDA #(Interpreter.msgUndefinedVariable / 256)
            STA ZP.LastErrorH
            return;
        }
    }
    
    handleReturn()
    {
        // For REPL, this is same as HALT
        BRK // TODO: Implement proper function returns later
        handleHalt();
    }
    
    handleHalt()
    {
        // Return to interpreter - set PC to special value
        LDA #0xFF
        STA ZP.PCL
        STA ZP.PCH
    }
    
    // Main execution loop - Public method
    ExecuteREPLFunction()
    {
        // Get bytecode address and set up execution state
        FunctionManager.GetREPLBytecode();  // Returns address in IDX
        
        LDA ZP.IDXL
        STA ZP.CODESTARTL
        LDA ZP.IDXH
        STA ZP.CODESTARTH
        STZ ZP.PCL
        STZ ZP.PCH
        
        // Main interpreter loop
        loop
        {
            // Check for halt condition
            LDA ZP.PCL
            CMP #0xFF
            if (Z)
            {
                LDA ZP.PCH
                CMP #0xFF
                if (Z) { break; }  // Halted
            }
            
            // Fetch next opcode
            executorFetchByte();  // Returns opcode in A
            
            // Dispatch to handler (simple switch for now)
            // Later we can optimize this to a jump table
            switch (A)
            {
                case OpCode.OpNop:
                {
                    handleNop();
                }
                case OpCode.OpPushInt:
                {
                    handlePushInt();
                }
                case OpCode.OpPrintInt:
                {
                    handlePrintInt();
                }
                case OpCode.OpPrintStr:
                {
                    handlePrintStr();
                }
                case OpCode.OpPrintNL:
                {
                    handlePrintNL();
                }
                case OpCode.OpLoadVar:
                {
                    handleLoadVar();
                }
                case OpCode.OpStoreVar:
                {
                    handleStoreVar();
                }
                case OpCode.OpReturn:
                {
                    handleReturn();
                    break;  // Exit loop
                }
                case OpCode.OpHalt:
                {
                    handleHalt();
                    break;  // Exit loop
                }
                default:
                {
                    // Unknown opcode - halt execution
                    Serial.HexOut();
                    LDA #'?'
                    Serial.WriteChar();
                    BRK
                    handleHalt();
                    break;
                }
            }
        }
    }
}
