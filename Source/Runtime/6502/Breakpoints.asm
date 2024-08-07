unit Breakpoints
{
    check() // munts X (only called by Check() and CheckX() which preserve A)
    {
        LDX # 0
        loop
        {
            // checkNextBreakPoint
            LDA ZP.BRKH, X
            if (NZ)
            {
#ifdef CPU_65C02S
                SMB5 ZP.FLAGS
#else                                
                LDA ZP.FLAGS
                ORA #0b00100000
                STA ZP.FLAGS // at least one breakpoint
#endif
                return;
            }
            LDA ZP.BRKL, X
            if (NZ)
            {
#ifdef CPU_65C02S
                SMB5 ZP.FLAGS
#else
                LDA ZP.FLAGS
                ORA #0b00100000
                STA ZP.FLAGS // at least one breakpoint
#endif
                return;
            }
            INX
            CPX # 0x10
            if (Z) { break; }
        }
#ifdef CPU_65C02S
        RMB5 ZP.FLAGS
#else
        LDA ZP.FLAGS
        AND #0b11011111
        STA ZP.FLAGS // no breakpoints
#endif
    }
    Clear() // munts X
    {
        PHA
        LDX # 1
        loop
        {
            // clearNextBreakPoint
#ifdef CPU_65C02S
            STZ ZP.BRKH, X
            STZ ZP.BRKL, X
#else            
            LDA # 0
            STA ZP.BRKH, X
            STA ZP.BRKL, X
#endif
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
#ifdef CPU_65C02S        
        STZ ZP.BRKH, X
        STZ ZP.BRKL, X
#else
        LDA #0
        STA ZP.BRKH, X
        STA ZP.BRKL, X
#endif   
        // check if any breakpoint is set
        check();
    }
    
    // is current hopper PC at a a breakpoint?
    //     Z set if true, Z clear if not, breakpoint in X
    //     munts A and X
    IsPCBreakpoint()
    {
        LDX # 0x10 // Z is clear

#ifdef CPU_65C02S   
        if (BBS5, ZP.FLAGS) // are there breakpoints set?
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
#else
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
#endif
        CPX # 0 // clear Z (since X is 0x10 for both exits above)
    }
}
