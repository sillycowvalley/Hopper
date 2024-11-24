program Pico433Rx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        string captured;
        UART.Setup(57600);
        PinMode(GP17, PinModeOption.Output);
        
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
                    DigitalWrite(GP17, true);
                    UART.WriteString("Returned: " +captured);
                    captured = "";
                    DigitalWrite(GP17, false);
                }
                
            }    
        } // reception loop 
    }
}
