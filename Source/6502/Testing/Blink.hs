program Blink
{
    #define TINYHOPPER
    
    uses "/Source/6502/Firmware/System"
    
    {
        loop
        {
            LED = true;
            Delay(500);
            LED = false;    
            Delay(500);
        }
    }
}
