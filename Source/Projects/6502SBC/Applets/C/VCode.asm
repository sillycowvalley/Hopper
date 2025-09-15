unit VCode
{
    const byte vzSlots = 0xD0;
    
    const byte vcodeBuffer     = vzSlots+0;
    const byte vcodeBufferL    = vzSlots+0;
    const byte vcodeBufferH    = vzSlots+1;
    
    const byte vcodeOffset     = vzSlots+2;
    const byte vcodeOffsetL    = vzSlots+2;
    const byte vcodeOffsetH    = vzSlots+3;
    
    const byte vcodeSize       = vzSlots+4;
    const byte vcodeSizeL      = vzSlots+4;
    const byte vcodeSizeH      = vzSlots+5;
    
    enum VOpCode
    {
        PushNEXT,
        PushTOP,
        PopNEXT,
        PopTOP,
    }
    
    Initialize()
    {
        // 256 bytes should be plenty
        LDA #0x00
        STA ZP.ACCL
        LDA #0x01
        STA ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        STA vcodeBufferL
        LDA ZP.IDXH
        STA vcodeBufferH
        
        STZ vcodeOffsetL   // Start at offset 0
        STZ vcodeOffsetH
        
        LDA #0x01
        STA vcodeSizeH
        STZ vcodeSizeL
    }
    Dispose()
    {
        // Free code buffer
        LDA vcodeBufferL
        ORA vcodeBufferH
        if (NZ)
        {
            LDA vcodeBufferL
            STA ZP.IDXL
            LDA vcodeBufferH
            STA ZP.IDXH
            Memory.Free();
            
            STZ vcodeBufferL
            STZ vcodeBufferH
        }
    }
    
    IsEmpty()
    {
        LDA vcodeSizeL
        ORA vcodeSizeH
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
    }
    
    Flush()
    {
        
    }
    
    PushNEXT()
    {
        LDY vcodeOffset
        LDA # VOpCode.PushNEXT
        STA [vcodeBuffer], Y
        INC vcodeOffset
    }
    
    
    // Generate code to push 32-bit value from ZP.NEXT onto runtime stack
    pushNEXT()
    {
        // SP -> X -> Y
        LDA #OpCode.TSX  
        EmitByte(); if (NC) { return; }
        LDA #OpCode.TXA
        EmitByte(); if (NC) { return; }
        LDA #OpCode.TAY
        EmitByte(); if (NC) { return; }
        
        // Store NEXT0 to stack via pointer
        // LDA ZP.NEXT0
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack0],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0
        EmitByte(); if (NC) { return; }
        
        // Store NEXT1 to stack via pointer
        // LDA ZP.NEXT1
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack1],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1
        EmitByte(); if (NC) { return; }
        
        // Store NEXT2 to stack via pointer
        // LDA ZP.NEXT2
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack2],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2
        EmitByte(); if (NC) { return; }
        
        // Store NEXT3 to stack via pointer
        // LDA ZP.NEXT3
        LDA #OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        // STA [runtimeStack3],Y
        LDA #OpCode.STA_IND_Y
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3
        EmitByte(); if (NC) { return; }
        
        // PHA - update stack pointer
        LDA #OpCode.PHA
        EmitByte(); if (NC) { return; }
        
        SEC
    }
    
}
