program Test
{
    #define CPU_6502
    
    const string data2 = "-----";
    const string data = "abcde";
    
    Hopper()
    {
        LDY # 0
        LDA data, Y
        LDA data2, Y
    }
}
