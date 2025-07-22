unit BytecodeCompiler
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Tokenizer"
    uses "FunctionManager"
    
    // Bytecode opcodes (updated names)
    enum Opcodes 
    {
        OpNop        = 0x00,
        OpPushInt    = 0x01,  // + 2 bytes: 16-bit value (was OpLoadConst)
        OpPrintInt   = 0x02,  // Print TOS as integer
        OpPrintStr   = 0x03,  // Print TOS as string literal
        OpPrintNL    = 0x04,  // Print newline
        OpReturn     = 0x05,  // Return from function
        OpHalt       = 0xFF,  // End of REPL function
    }
    
    // Compile a PRINT statement
    // Assumes current token is PRINT
    compilePrintStatement()
    {
        // Move to next token (the expression to print)
        Tokenizer.nextToken();
        
        // For now, only support integer literals and string literals
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.NUMBER:
            {
                // Get the number value
                Tokenizer.getTokenNumber();  // Returns value in ZP.TOP
                
                // Emit OpPushInt followed by the 16-bit value
                LDA #Opcodes.OpPushInt
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
                
                // Emit the constant value (16-bit)
                LDA #Types.UInt
                Stacks.PushTop();
                FunctionManager.EmitWord();
                
                // Emit OpPrintInt
                LDA #Opcodes.OpPrintInt
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
            case Tokens.STRING:
            {
                // For now, we'll just emit a placeholder
                // TODO: Handle string literals properly
                LDA #Opcodes.OpPrintStr
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
            default:
            {
                // Unsupported expression type for now
                return;
            }
        }
        
        // Move to next token to check for end of statement
        Tokenizer.nextToken();
        
        // If we hit EOL, add a newline print
        LDA ZP.CurrentToken
        CMP #Tokens.EOL
        if (Z)
        {
            LDA #Opcodes.OpPrintNL
            STA ZP.NEXTL
            LDA #0
            STA ZP.NEXTH
            LDA #Types.Byte
            Stacks.PushNext();
            FunctionManager.EmitByte();
        }
    }
    
    // Compile a complete REPL statement
    CompileREPLStatement()
    {
        // Start compilation
        FunctionManager.StartREPLCompilation();
        
        // Get first token (should be statement type)
        Tokenizer.nextToken();
        
        LDA ZP.CurrentToken
        switch (A)
        {
            case Tokens.PRINT:
            {
                compilePrintStatement();
            }
            case Tokens.EOL:
            {
                // Empty statement - just emit NOP
                LDA #Opcodes.OpNop
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
            default:
            {
                // Unsupported statement - emit NOP for now
                LDA #Opcodes.OpNop
                STA ZP.NEXTL
                LDA #0
                STA ZP.NEXTH
                LDA #Types.Byte
                Stacks.PushNext();
                FunctionManager.EmitByte();
            }
        }
        
        // Always end with HALT for REPL
        LDA #Opcodes.OpHalt
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.EmitByte();
        
        // Finish compilation
        FunctionManager.FinishREPLCompilation();
    }
}
