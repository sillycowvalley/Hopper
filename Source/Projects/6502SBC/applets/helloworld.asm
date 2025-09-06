program HelloWorld
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    
    const string helloWord = "Hello World!";
    
    Hopper()
    {
        LDA #(helloWord % 256)
        STA ZP.STRL
        LDA #(helloWord / 256)
        STA ZP.STRH
        Print.String();
    }
    
}
