program PiBlink
{
    uses "/Source/Library/Boards/Pi"
    
    const byte pin = Board.GP21;
    
    Hopper()
    {
        PinMode(pin, PinModeOption.Output);
        loop
        {
            DigitalWrite(pin, true);
            Delay(500);
            DigitalWrite(pin, false);
            Delay(500);
            
            if (IO.IsAvailable)
            {
                break;
            }
        }
    }
}
