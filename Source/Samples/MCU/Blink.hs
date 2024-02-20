program Blink
{
    uses "/Source/Library/Boards/PiPico"
    
    {
        loop
        {
            Write(LED ? '+' : '-');
            Delay(500);
            LED = !LED;
        }
    }
}

