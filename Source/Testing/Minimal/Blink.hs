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
        loop
        {
            LED = !LED;
            WriteLn((Time.Seconds).ToString());
            /*
            PinMode(Board.GP8, PinModeOption.Input);
            if (DigitalRead(Board.GP8))
            {
                WriteLn("A HIGH");    
            }
            else
            {
                WriteLn("A LOW");    
            }
            PinMode(Board.GP9, PinModeOption.Input);    
            if (DigitalRead(Board.GP9))
            {
                WriteLn("B HIGH");    
            }
            else
            {
                WriteLn("B LOW");    
            }
            */
            Delay(500);
        }
    }
}
