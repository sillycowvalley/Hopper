program BlinkRGB
{
    //uses "/Source/Library/Boards/PimoroniTiny2040"
    uses "/Source/Library/Boards/SeeedXIAORP2040"
    
    {
        loop
        {
            Write(LEDR ? 'R' : '-');
            Delay(500);
            LEDR = !LEDR;
            
            Write(LEDG ? 'G' : '-');
            Delay(500);
            LEDG = !LEDG;
            
            Write(LEDB ? 'B' : '-');
            Delay(500);
            LEDB = !LEDB;
        }
    }
}

