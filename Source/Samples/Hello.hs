program Hello
{
    #define RP2040
    
    uses "/Source/System/MCU"
    
    {
        WriteLn("Hello Hopper!");
        for (uint i = 0; i < 10; i++)
        {
            LED = true;
            Delay(500);
            LED = false;
            Delay(500);
        }
    }
}
