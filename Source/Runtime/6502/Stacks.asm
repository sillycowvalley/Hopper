unit Stacks
{
    Init()
    {
        STZ ZP.SP
        STZ ZP.BP
        STZ ZP.CSP
        
        // zeroes mean faster debug protocol
        STZ IDXL
        LDA # (Address.ValueStackLSB >> 8)
        STA IDXH
        LDX # 1
        LDX # 2
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized
        
        STZ IDXL
        LDA # (Address.TypeStackLSB >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized
        
        STZ IDXL
        LDA # (Address.CallStackLSB >> 8)
        STA IDXH
        LDX # 2
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized
    }
    
    PopBP()
    {
        PHY
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.BP
        PLY
    }
    PushBP()
    {
        PHY
        LDY ZP.CSP
        LDA ZP.BP
        STA Address.CallStackLSB, Y
        LDA # 0
        STA Address.CallStackMSB, Y
        INC ZP.CSP
        PLY
    }
    PopPC()
    {
        PHY
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.PCL
        LDA Address.CallStackMSB, Y
        STA ZP.PCH
        PLY
    }
    PushPC()
    {
        PHY
        LDY ZP.CSP
        LDA ZP.PCL
        STA Address.CallStackLSB, Y
        LDA ZP.PCH
        STA Address.CallStackMSB, Y
        INC ZP.CSP
        PLY
    }
    PopTop()
    {
        PHY
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.TOPL
        LDA Address.ValueStackMSB, Y
        STA ZP.TOPH
        LDA Address.TypeStackLSB, Y
        STA ZP.TOPT
        PLY
    }
    PushTop()
    {
        PHY
        LDY ZP.SP
        LDA ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA ZP.TOPH
        STA Address.ValueStackMSB, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
        PLY
    }
    PopNext()
    {
        PHY
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.NEXTL
        LDA Address.ValueStackMSB, Y
        STA ZP.NEXTH
        LDA Address.TypeStackLSB, Y
        STA ZP.NEXTT
        PLY
    }
    PushNext()
    {
        PHY
        LDY ZP.SP
        LDA ZP.NEXTL
        STA Address.ValueStackLSB, Y
        LDA ZP.NEXTH
        STA Address.ValueStackMSB, Y
        LDA ZP.NEXTT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
        PLY
    }
    PushAcc()
    {
        PHY
        LDY ZP.SP
        LDA ZP.ACCL
        STA Address.ValueStackLSB, Y
        LDA ZP.ACCH
        STA Address.ValueStackMSB, Y
        LDA ZP.ACCT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
        PLY
    }
    PushBool()
    {
        // value is in X: 0 or 1
        STX ZP.NEXTL
        STZ ZP.NEXTH
        LDA #Types.Bool
        STA ZP.NEXTT
        PushNext();
    }
    PopAcc()
    {
        PHY
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.ACCL
        LDA Address.ValueStackMSB, Y
        STA ZP.ACCH
        LDA Address.TypeStackLSB, Y
        STA ZP.ACCT
        PLY
    }
    
    PopA() // pop a byte (don't care about type or MSB)
    {
        PHY
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        PLY
    }
}
