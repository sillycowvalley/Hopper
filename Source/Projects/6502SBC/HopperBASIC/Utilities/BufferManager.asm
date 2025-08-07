unit BufferManager // BufferManager.asm
{
    // Use bit 7 of ZP.FLAGS for REPL mode flag
    // if (BBR7, ZP.FLAGS) = Branch if Bit 7 Reset (main mode)
    // if (BBS7, ZP.FLAGS) = Branch if Bit 7 Set (REPL mode)
    // SMB7 ZP.FLAGS = Set Memory Bit 7 (enter REPL mode)
    // RMB7 ZP.FLAGS = Reset Memory Bit 7 (exit REPL mode)
    
    UseREPLBuffers()
    {
        PHA
        
        // Set REPL mode flag
        SMB7 ZP.FLAGS
        
        // Point tokenizer to REPL buffer
        LDA #(REPLTokenizerBuffer % 256)
        STA ZP.TokenBufferL
        LDA #(REPLTokenizerBuffer / 256)
        STA ZP.TokenBufferH
        
        // Point opcode buffer to REPL buffer
        LDA #(REPLOpCodeBuffer % 256)
        STA ZP.OpCodeBufferL
        LDA #(REPLOpCodeBuffer / 256)
        STA ZP.OpCodeBufferH
        
        PLA
    }
    
    UseBASICBuffers()
    {
        PHA
        
        // Clear REPL mode flag
        RMB7 ZP.FLAGS
        
        // Point tokenizer to BASIC function buffer
        LDA #(BASICTokenizerBuffer % 256)
        STA ZP.TokenBufferL
        LDA #(BASICTokenizerBuffer / 256)
        STA ZP.TokenBufferH
        
        // Point opcode buffer to BASIC function buffer
        LDA #(BASICOpCodeBuffer % 256)
        STA ZP.OpCodeBufferL
        LDA #(BASICOpCodeBuffer / 256)
        STA ZP.OpCodeBufferH
        
        PLA
    }
    
    IsREPLMode()
    {
        // Test bit 7 of FLAGS
        // Returns: C set if REPL mode, NC if main mode
        if (BBS7, ZP.FLAGS)
        {
            SEC
        }
        else
        {
            CLC
        }
    }
    StoreREPLContext()
    {
        // Save REPL execution context
        LDA ZP.PCL
        PHA
        LDA ZP.PCH
        PHA
    }
    RestoreREPLContext()
    {
        // Restore REPL execution context
        PLA
        STA ZP.PCH
        PLA
        STA ZP.PCL
    }
}