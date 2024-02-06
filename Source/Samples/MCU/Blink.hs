program Blink
{
    #define CHALLENGER_RP2040_WIFI
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

