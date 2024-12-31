program Pico433Rx
{
    //uses "/Source/Library/Boards/PiPicoW"
    //uses "/Source/Library/Boards/AdaFruitFeatherRP2350HSTX"
    //uses "/Source/Library/Boards/ChallengerNB2040WiFi"    
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    const byte ledPin = GP5;
    
    Hopper()
    {
        string captured;
        
        PinMode(ledPin, PinModeOption.Output);
        
        UART.Setup(9600);
        
        loop // reception loop
        {
            if (UART.IsAvailable)
            {
                char ch = UART.ReadChar();
                Serial.WriteChar(ch);
                captured += ch;
                if (ch == Char.EOL)
                {   
                    // return the content on Char.EOL           
                    DigitalWrite(ledPin, true);
                    UART.WriteString("Returned: " +captured);
                    captured = "";
                    Delay(250);
                    DigitalWrite(ledPin, false);
                }
                
            }    
        } // reception loop 
    }
}
