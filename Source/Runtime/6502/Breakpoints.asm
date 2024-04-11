unit Breakpoints
{
    check() // munts X, (only called from Clear and ClearX which preserve A)
    {
        LDX # 0
        loop
        {
            // checkNextBreakPoint
            LDA ZP.BRKH, X
            if (NZ)
            {
                LDA ZP.FLAGS
                ORA #0b00100000
                STA ZP.FLAGS // at least one breakpoint
                RTS
            }
            LDA ZP.BRKL, X
            if (NZ)
            {
                LDA ZP.FLAGS
                ORA #0b00100000
                STA ZP.FLAGS // at least one breakpoint
                RTS
            }
            INX
            CPX # 0x10
            if (Z) { break; }
        }
        LDA ZP.FLAGS
        AND #0b11011111
        STA ZP.FLAGS // no breakpoints
    }
    Clear() // munts X
    {
        PHA
        LDX #1
        loop
        {
            // clearNextBreakPoint
            LDA # 0
            STA ZP.BRKH, X
            STA ZP.BRKL, X
            INX
            CPX # 0x10
            if (Z) { break; }
        }
        
        // check if breakpoint #0 is set
        check();
        PLA
    }
    
    // clear the breakpoint in X
    ClearX() // munts X
    {
        PHA
        
        LDA #0
        STA ZP.BRKH, X
        STA ZP.BRKL, X
        
        // check if any breakpoint is set
        check();
        
        PLA
    }
    
    // is current hopper PC at a a breakpoint?
    //     Z set if true, Z clear if not, breakpoint in X
    //     munts A and X
    IsPCBreakpoint()
    {
        LDX # 0x10 // Z is clear
        
        LDA ZP.FLAGS
        AND # 0b00100000
        if (NZ) // are there breakpoints set?
        {  
            LDX #0
            loop
            {
                LDA ZP.BRKH, X
                CMP ZP.PCH
                if (Z)
                {
                    LDA ZP.BRKL, X
                    CMP ZP.PCL
                    if (Z)
                    {
                        return; // Z is set for X
                    }
                }
                INX
                CPX # 0x10
                if (Z) { break; }
            }
        }
        CPX # 0 // clear Z (since X is 0x10 for both exits above)
    }
}
