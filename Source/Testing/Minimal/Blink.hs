program Blink
{
    //uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/PD6502"
    uses "/Source/Library/Boards/MECB6502"
    
    //uses "/Source/Library/Boards/PiPico"
    
    Hopper()
    {
        
        //SampleMicros = 1000;
        PinMode(Board.GP14, PinModeOption.Input);
        PinMode(Board.GP15, PinModeOption.Input);    
        loop
        {
            LED = !LED;
            IO.Write((Time.Seconds).ToString());
            if (DigitalRead(Board.GP14))
            {
                IO.Write(", A HIGH");    
            }
            else
            {
                IO.Write(", A LOW");    
            }
            if (DigitalRead(Board.GP15))
            {
                IO.Write(", B HIGH");    
            }
            else
            {
                IO.Write(", B LOW");    
            }
            IO.WriteLn();
            Delay(500);
        }
    }
}
