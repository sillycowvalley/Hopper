program Blink
{
    uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/PD6502"
    //uses "/Source/Library/Boards/PiPico"
    
    Hopper()
    {
        loop
        {
            LED = !LED;
            WriteLn((Time.Seconds).ToString());
            Delay(500);
        }
    }
}
