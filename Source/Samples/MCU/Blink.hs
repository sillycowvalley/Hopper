program Blink
{
    #define RP2040_PICO
    uses "/Source/Library/MCU"
    
    {
        loop
        {
            LED = true;
            Write('+');
            Delay(500);
            LED = false;    
            Write('-');
            Delay(500);
        }
    }
}
