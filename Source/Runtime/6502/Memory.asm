unit Memory
{
    InitializeHeapSize()
    {
        // Assumes that:
        // - entire program was loaded at HopperData (typically $0800)
        // - size in bytes of loaded program is in IDY
        
        CLC
        LDA # (Address.HopperData >> 8)
        ADC ZP.IDYH
        STA ZP.HEAPSTART
        INC ZP.HEAPSTART // next page after program
        
        SEC
        LDA # (Address.RamSize >> 8) // top page of RAM (typically 0x80)
        SBC HEAPSTART
        STA HEAPSIZE
    }
}
