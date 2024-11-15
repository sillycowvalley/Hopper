program Blink
{
    //uses "/Source/Library/Boards/PiPico"
    //uses "/Source/Library/Boards/PiPicoW"
    //uses "/Source/Library/Boards/PiPico2"
    //uses "/Source/Library/Boards/Challenger2040WiFi"
    //uses "/Source/Library/Boards/Challenger2040SDRTC"
    //uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    //uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    //uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/PimoroniPicoPlus2"
    uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    //uses "/Source/Library/Boards/Hopper6502"
    
    {
        loop
        {
            Delay(500);
            
            LED = !LED;
            
            WriteLn((Time.Seconds).ToString() + " seconds");
        }
    }
}

