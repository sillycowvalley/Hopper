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
}
