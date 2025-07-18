program Switch
{
    uses "/Source/Library/Boards/Pi"
    
    byte button = Board.GP20;
    byte led    = Board.GP4;
    
    Rising(byte pin, PinStatus status)
    {
        IO.Write('+');
    }
    
    Falling(byte pin, PinStatus status)
    {
        IO.Write('-');
    }
    
    Hopper()
    {
        PinMode(button, PinModeOption.Input);
        PinMode(led, PinModeOption.Output);
        DigitalWrite(led, false);
        
        if (AttachToPin(button, Rising, PinStatus.Rising))
        {
            IO.WriteLn("Rising success");
        }
        else
        {
            IO.WriteLn("Rising failed");
        }
        if (AttachToPin(button, Falling, PinStatus.Falling))
        {
            IO.WriteLn("Falling success");
        }
        else
        {
            IO.WriteLn("Falling failed");
        }
        loop
        {
            if (IO.IsAvailable)
            {
                break;
            }
        }
    }
}
