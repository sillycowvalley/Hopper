program NOPs
{
    #define CPU_65C02S
    #define ROM_4K
    
    NMI()
    {
    }
    IRQ()
    {
    }
    Function()
    {
        NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP
    }
    Hopper()
    {
        loop {
          Function();
        } // loop
    }
}
