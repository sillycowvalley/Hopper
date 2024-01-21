program Blink
{
    uses "/Source/6502/System"
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
