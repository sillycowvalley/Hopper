program Blink
{
    #define RP2040_PICO
    uses "/Source/Library/MCU"
    {
        loop
        {
            Write(LED ? '+' : '-');
            Delay(500);
            LED = !LED;
        }
    }
}

