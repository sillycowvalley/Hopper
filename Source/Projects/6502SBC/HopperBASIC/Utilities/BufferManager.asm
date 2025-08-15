unit BufferManager // BufferManager.asm
{
    InitializeForTokenGeneration()
    {
        // always tokenize to global token buffer
        LDA #(Address.TokenizerBuffer % 256)
        STA ZP.TokenBufferL
        LDA #(Address.TokenizerBuffer / 256)
        STA ZP.TokenBufferH
        Tokenizer.Initialize();
        BufferManager.ResetInputBuffer();
    }
    
    UseREPLOpCodeBuffer()
    {
        SMB3 ZP.FLAGS // Set REPL mode: we're using REPL buffers for compilation
        
        // Point opcode buffer to BASIC function buffer
        LDA #(Address.REPLOpCodeBuffer % 256)
        STA ZP.OpCodeBufferL
        LDA #(Address.REPLOpCodeBuffer / 256)
        STA ZP.OpCodeBufferH
        
#ifdef DEBUG
        LDA #(Address.REPLOpCodeBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        LDA #(Address.REPLOpCodeBuffer %256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(Limits.OpCodeBufferSize / 256)
        STA ZP.FLENGTHH
        LDA #(Limits.OpCodeBufferSize %256)
        STA ZP.FLENGTHL
        Tools.ZeroBytes();
#endif    

    }
    
    // Input: ZP.IDX = function node address (called before Functions.Compile() compiles a new function)
    UseFunctionBuffers()
    {
        RMB3 ZP.FLAGS // Clear REPL mode flag : using function buffers for compilation
        
        // Point opcode buffer to BASIC function buffer
        LDA #(Address.FunctionOpCodeBuffer % 256)
        STA ZP.OpCodeBufferL
        LDA #(Address.FunctionOpCodeBuffer / 256)
        STA ZP.OpCodeBufferH
        
#ifdef DEBUG
        LDA #(Address.FunctionOpCodeBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        LDA #(Address.FunctionOpCodeBuffer %256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(Limits.OpCodeBufferSize / 256)
        STA ZP.FLENGTHH
        LDA #(Limits.OpCodeBufferSize %256)
        STA ZP.FLENGTHL
        Tools.ZeroBytes();
#endif    
        
        loop
        {
            // Get function body tokens
            Functions.GetBody(); // Input: ZP.IDX = function node, Output: ZP.IDY = tokens pointer
            
            // Check if function has a body
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                Error.InternalError(); BIT ZP.EmulatorPCL
                break;
            }
            
            // switch to function token buffer
            LDA ZP.IDYL
            STA ZP.TokenBufferL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDYH
            STA ZP.TokenBufferH
            STA ZP.FDESTINATIONADDRESSH
            
            // Length of function tokens
            STZ ZP.FLENGTHL
            STZ ZP.FLENGTHH
            
            // measure length of function's token stream
            loop
            {
                IncLENGTH();
                LDA [ZP.IDY]
                CMP #Token.EOF  
                if (Z) { break; }
                IncIDY();
                IncDESTINATIONADDRESS();
            }
            
            // reset tokenizer to read copied function tokens
            LDA ZP.FLENGTHL                   // Length of function tokens
            STA ZP.TokenBufferContentLengthL
            LDA ZP.FLENGTHH
            STA ZP.TokenBufferContentLengthH
            
            STZ ZP.TokenizerPosL          // Start at beginning of function tokens
            STZ ZP.TokenizerPosH
            
            break;
        } // single exit
    }
    
    ResetTokenizerBuffer()
    {
        STZ ZP.TokenBufferContentLengthL
        STZ ZP.TokenBufferContentLengthH
        
#ifdef DEBUG
        LDA #(Address.TokenizerBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        LDA #(Address.TokenizerBuffer %256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(Limits.TokenizerBufferSize / 256)
        STA ZP.FLENGTHH
        LDA #(Limits.TokenizerBufferSize %256)
        STA ZP.FLENGTHL
        Tools.ZeroBytes();
#endif    
    }
    ResetInputBuffer()
    {
        PHP
        
        STZ ZP.BasicInputLength
#ifdef DEBUG        
        LDA #(Address.BasicInputBuffer / 256)
        STA ZP.FDESTINATIONADDRESSH
        LDA #(Address.BasicInputBuffer %256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(Limits.BasicInputSize / 256)
        STA ZP.FLENGTHH
        LDA #(Limits.BasicInputSize %256)
        STA ZP.FLENGTHL
        Tools.ZeroBytes();
#endif    
        PLP
    }
    
    IsREPLMode()
    {
        // Test bit 7 of FLAGS
        // Returns: C set if REPL compilation, NC if Function compilation
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
