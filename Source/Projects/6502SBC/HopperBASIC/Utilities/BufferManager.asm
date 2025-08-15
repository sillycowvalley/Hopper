unit BufferManager // BufferManager.asm
{
    UseREPLBuffers()
    {
        SMB3 ZP.FLAGS // Set REPL mode flag
        
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
    }
    
    UseBASICBuffers()
    {
        RMB3 ZP.FLAGS // Clear REPL mode flag
        
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
    }
    
    DetectBufferState()
    {
        /*
        LDA #(BASICTokenizerBuffer / 256)
        STA ZP.TokenBufferH
        if (Z)
        {
            RMB3 ZP.FLAGS // Clear REPL mode flag
        }
        else
        {
            SMB3 ZP.FLAGS // Set REPL mode flag
        }
        */
    }
    
    IsREPLMode()
    {
        // Test bit 7 of FLAGS
        // Returns: C set if REPL mode, NC if main mode
        if (BBS3, ZP.FLAGS)
        {
            SEC
        }
        else
        {
            CLC
        }
    }
    
}