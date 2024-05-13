unit Buttons
{
    
    Initialize()
    {
        // buttons as inputs
#ifdef CPU_65C02S
        RMB2 ZP.DDRA
        RMB3 ZP.DDRA
#else
        LDA ZP.DDRA
        AND # 0b11110011
        STA ZP.DDRA
#endif        
    }
    ButtonOneDown()
    {
        LDA ZP.PORTA
        AND # 0b00000100
    }
    ButtonTwoDown()
    {
        LDA ZP.PORTA
        AND # 0b00001000
    }
}
