program Blink
{
    uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/6502Retro"
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
