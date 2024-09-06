program Blink
{
    //uses "/Source/Library/Boards/PiPico"
    //uses "/Source/Library/Boards/Challenger2040WiFi"
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/Hopper6502"
    
    {
        loop
        {
            Delay(500);
            
            InterruptsEnabled = false;
            LED = !LED;
            InterruptsEnabled = true;
            
            WriteLn((Time.Seconds).ToString() + " seconds");
        }
    }
}

