program Blink
{
    uses "/Source/Library/Boards/AdafruitFeather"
    
    {
        loop
        {
            Write(LED ? '+' : '-');
            Delay(500);
            LED = !LED;
        }
    }
}

