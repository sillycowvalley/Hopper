program Blink
{
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"   
    
    Hopper()
    {
        //for (byte pin = 0; pin <= 28; pin++)
        //{
        
        byte pin = 20;
        //byte pin = 23;
        
            MCU.PinMode(pin, MCU.PinModeOption.Output);
            
            IO.WriteLn("Pin: " + pin.ToString());
            for (byte i=0; i < 10; i++)
            {
                MCU.DigitalWrite(pin, i % 2 == 0);
                Delay(500);
            }
            MCU.DigitalWrite(pin, false);
            
            
        //}
    }
}

