program Test
{
    #define CPU_6502
    
    const string data2 = {45, 46, 47, 48 };
    const string data = "abcde";
    
    Hopper()
    {
        LDY # 0
        LDA data, Y
        LDA data2, Y
    }
}
