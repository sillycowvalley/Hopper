unit BytecodeExecutor
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Stacks"
    uses "BytecodeCompiler"
    uses "FunctionManager" 
    uses "Tools"
    
    // Fetch next byte from bytecode and advance PC
    fetchByte()
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
    fetchWord()
    {
        // Fetch low byte
        fetchByte();
        STA ZP.TOPL
        
        // Fetch high byte  
        fetchByte();
        STA ZP.TOPH
        
        // Return value in TOP
    }
    
    // Opcode handlers
    handleNop()
    {
        // Do nothing
    }
    
    handlePushInt()
    {
        // Load 16-bit constant onto value stack
        fetchWord();  // Gets constant into TOP

        LDA #Types.UInt
        Stacks.PushTop();
    }
    
    handlePrintInt()
    {
        // Pop integer from stack and print it
        Stacks.PopTop();
        Tools.PrintDecimalWord();
    }
    
    handlePrintStr()
    {
        // TODO: Implement string printing
        // For now, just print placeholder
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
    
    handleReturn()
    {
        // For REPL, this is same as HALT
        // TODO: Implement proper function returns later
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
            fetchByte();  // Returns opcode in A
            
            // Dispatch to handler (simple switch for now)
            // Later we can optimize this to a jump table
            switch (A)
            {
                case Opcodes.OpNop:
                {
                    handleNop();
                }
                case Opcodes.OpPushInt:
                {
                    handlePushInt();
                }
                case Opcodes.OpPrintInt:
                {
                    handlePrintInt();
                }
                case Opcodes.OpPrintStr:
                {
                    handlePrintStr();
                }
                case Opcodes.OpPrintNL:
                {
                    handlePrintNL();
                }
                case Opcodes.OpReturn:
                {
                    handleReturn();
                    break;  // Exit loop
                }
                case Opcodes.OpHalt:
                {
                    handleHalt();
                    break;  // Exit loop
                }
                default:
                {
                    // Unknown opcode - halt execution
                    LDA #'?'
                    Serial.WriteChar();
                    break;
                }
            }
        }
    }
}
