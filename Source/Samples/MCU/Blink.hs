program Blink
{
    #define RP2040_PICOW
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

