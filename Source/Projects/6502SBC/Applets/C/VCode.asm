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
        
    }
    
    Initialize()
    {
        // 1KB should be plenty
        LDA #0x00
        STA ZP.ACCL
        LDA #0x04  // 1KB = 0x0400
        STA ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        STA vcodeBufferL
        LDA ZP.IDXH
        STA vcodeBufferH
        
        STZ vcodeOffsetL   // Start at offset 0
        STZ vcodeOffsetH
        
        LDA #0x04
        STA vcodeSizeH     // 1KB size
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
}
