unit Stacks
{
    Init()
    {
        STZ ZP.SP
        STZ ZP.BP
        STZ ZP.CSP
        
#ifdef CHECKED
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
#endif
    }
}
