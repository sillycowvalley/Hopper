unit BytecodeExecutor
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Stacks"
    uses "BytecodeCompiler"
    uses "FunctionManager" 
    uses "Tools"
    
    friend Interpreter, HopperBASIC;
    
    // Execution state
    const byte programCounter    = 0x3C;  // Program counter (16-bit) within bytecode
    const byte programCounterHi  = 0x3D;
    const byte bytecodeBase      = 0x3E;  // Base address of current bytecode (16-bit)
    const byte bytecodeBaseHi    = 0x3F;
    
    // Fetch next byte from bytecode and advance PC
    fetchByte()
    {
        // Calculate address: bytecode + PC
        CLC
        LDA bytecodeBase
        ADC programCounter
        STA ZP.IDXL
        LDA bytecodeBaseHi
        ADC programCounterHi
        STA ZP.IDXH
        
        // Fetch the byte
        LDY #0
        LDA [ZP.IDX], Y
        
        // Advance PC
        INC programCounter
        if (Z)
        {
            INC programCounterHi
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
    
    handleLoadConst()
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
        STA programCounter
        STA programCounterHi
    }
    
    // Main execution loop
    executeREPLFunction()
    {
        // Get bytecode address
        FunctionManager.getREPLBytecode();  // Returns address in IDX
        
        // Initialize execution state
        LDA ZP.IDXL
        STA bytecodeBase
        LDA ZP.IDXH
        STA bytecodeBaseHi
        STZ programCounter
        STZ programCounterHi
        
        // Main interpreter loop
        loop
        {
            // Check for halt condition
            LDA programCounter
            CMP #0xFF
            if (Z)
            {
                LDA programCounterHi
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
                case Opcodes.OpLoadConst:
                {
                    handleLoadConst();
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
