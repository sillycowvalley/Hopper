program Blink
{
    #define ADAFRUIT_METRO_RP2040
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

