unit Stacks
{
    Init()
    {
#ifdef CPU_65C02S        
        STZ ZP.SP
        STZ ZP.BP
        STZ ZP.CSP
        
        // zeroes mean faster debug protocol
        STZ IDXL  // setting this to 0 once is enough (ClearPages does not modify IDXL)
#else
        LDA # 0
        STA ZP.SP
        STA ZP.BP
        STA ZP.CSP
        
        // zeroes mean faster debug protocol
        STA IDXL // setting this to 0 once is enough (ClearPages does not modify IDXL)
#endif
        LDA # (Address.ValueStackLSB >> 8)
        STA IDXH
        LDX # 1
        LDX # 2
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized
        
        LDA # (Address.TypeStackLSB >> 8)
        STA IDXH
        LDX # 1
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized
        
        LDA # (Address.CallStackLSB >> 8)
        STA IDXH
        LDX # 2
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized (does not modify IDXL)
    }
    
    PopBP()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.BP
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PushBP()
    {
#ifdef CPU_65C02S      
        PHX
#else
        TXA PHA
#endif        
        LDX ZP.CSP
        LDA ZP.BP
        STA Address.CallStackLSB, X
#ifdef CPU_65C02S
        STZ Address.CallStackMSB, X
#else        
        LDA # 0
        STA Address.CallStackMSB, X
#endif
        INC ZP.CSP
#ifdef CPU_65C02S      
        PLX
#else
        PLA TAX
#endif        
    }
    PopPC()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.PCL
        LDA Address.CallStackMSB, Y
        STA ZP.PCH
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PushPC()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        LDY ZP.CSP
        LDA ZP.PCL
        STA Address.CallStackLSB, Y
        LDA ZP.PCH
        STA Address.CallStackMSB, Y
        INC ZP.CSP
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PopTop()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.TOPL
        LDA Address.ValueStackMSB, Y
        STA ZP.TOPH
        LDA Address.TypeStackLSB, Y
        STA ZP.TOPT
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PushTop()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        LDY ZP.SP
        LDA ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA ZP.TOPH
        STA Address.ValueStackMSB, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PopNext()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.NEXTL
        LDA Address.ValueStackMSB, Y
        STA ZP.NEXTH
        LDA Address.TypeStackLSB, Y
        STA ZP.NEXTT
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PopTopNext()
    {
        PopTop();
        PopNext();
    }
    PushNext()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        LDY ZP.SP
        LDA ZP.NEXTL
        STA Address.ValueStackLSB, Y
        LDA ZP.NEXTH
        STA Address.ValueStackMSB, Y
        LDA ZP.NEXTT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PushACC()
    {
#ifdef CPU_65C02S      
       PHY  
#else
       TYA PHA
#endif        
        LDY ZP.SP
        LDA ZP.ACCL
        STA Address.ValueStackLSB, Y
        LDA ZP.ACCH
        STA Address.ValueStackMSB, Y
        LDA ZP.ACCT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PushIDX()
    {
#ifdef CPU_65C02S      
       PHY  
#else
       TYA PHA
#endif        
        LDY ZP.SP
        LDA ZP.IDXL
        STA Address.ValueStackLSB, Y
        LDA ZP.IDXH
        STA Address.ValueStackMSB, Y
        LDA #Types.UInt
        STA Address.TypeStackLSB, Y
        INC ZP.SP
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PushBool()
    {
        // value is in X: 0 or 1
        STX ZP.NEXTL
#ifdef CPU_65C02S   
        STZ ZP.NEXTH     
#else
        LDA # 0
        STA ZP.NEXTH
#endif
        LDA #Types.Bool
        STA ZP.NEXTT
        PushNext();
    }
    PushA()
    {
        STA ZP.NEXTL
#ifdef CPU_65C02S
        STZ ZP.NEXTH
#else
        LDA # 0
        STA ZP.NEXTH
#endif
        LDA #Types.Byte
        STA ZP.NEXTT
        PushNext();
    }
    PopACC()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.ACCL
        LDA Address.ValueStackMSB, Y
        STA ZP.ACCH
        LDA Address.TypeStackLSB, Y
        STA ZP.ACCT
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PopIDX()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.IDXL
        LDA Address.ValueStackMSB, Y
        STA ZP.IDXH
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    PopIDY()
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.IDYL
        LDA Address.ValueStackMSB, Y
        STA ZP.IDYH
#ifdef CPU_65C02S      
        PLY  
#else
        PLA TAY
#endif        
    }
    
    PopA() // pop a byte (don't care about type or MSB)
    {
#ifdef CPU_65C02S      
        PHY  
#else
        TYA PHA
#endif        
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
#ifdef CPU_65C02S      
        PLY  
#else
        TAX PLA TAY TXA
#endif        
    }
}
