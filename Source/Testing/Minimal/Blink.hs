program Blink
{
    //#define NO_PACKED_INSTRUCTIONS
    //#define NO_JIX_INSTRUCTIONS
    //uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/PD6502"
    uses "/Source/Library/Boards/MECB6502"
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
