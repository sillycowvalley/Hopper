unit Breakpoints
{
    Check()
    {
        PHX
        LDX # 0
        loop
        {
            // checkNextBreakPoint
            LDA ZP.BRKH, X
            if (NZ)
            {
                SMB5 ZP.FLAGS // at least one breakpoint
                PLX
                RTS
            }
            LDA ZP.BRKL, X
            if (NZ)
            {
                SMB5 ZP.FLAGS // at least one breakpoint
                PLX
                RTS
            }
            INX
            CPX # 0x10
            if (Z) { break; }
        }
        RMB5 ZP.FLAGS // no breakpoints
        PLX
    }
    Clear()
    {
        PHX
        LDX #1
        loop
        {
            // clearNextBreakPoint
            STZ ZP.BRKH, X
            STZ ZP.BRKL, X
            INX
            CPX # 0x10
            if (Z) { break; }
        }
        PLX
        
        // check if breakpoint #0 is set
        Breakpoints.Check();
    }
    
    // clear the breakpoint in X
    ClearX()
    {
        STZ ZP.BRKH, X
        STZ ZP.BRKL, X
        
        // check if any breakpoint is set
        Breakpoints.Check();
    }
    
    // is current hopper PC at a a breakpoint?
    //     Z set if true, Z clear if not, breakpoint in X
    IsPCBreakpoint()
    {
        LDX # 0x10 // Z is clear
        
        BBS5 ZP.FLAGS, +3
        return; // skip code below if no breakpoints set
            
        LDX #0
        loop
        {
            // clearNextBreakPoint
            LDA ZP.BRKH, X
            CMP ZP.PCL
            if (Z)
            {
                LDA ZP.BRKL, X
                CMP ZP.PCH
                if (Z)
                {
                    return; // Z is set
                }
            }
            INX
            CPX # 0x10
            if (Z) { break; }
        }
        CPX # 0 // clear Z (since X is 0x10)
    }
}
