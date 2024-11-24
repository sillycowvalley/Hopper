program Pico433Rx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        UART.Setup(57600);
        PinMode(GP16, PinModeOption.Input);
        PinMode(GP17, PinModeOption.Output);
        loop
        {
            if (UART.IsAvailable)
            {
                char ch = UART.ReadChar();
                Serial.WriteChar(ch);
                if (ch == Char.EOL)
                {
                    DigitalWrite(GP17, true);
                    Delay(100);
                    DigitalWrite(GP17, false);
                }
            }    
        }
        /*
        
        
        
        DigitalWrite(GP16, false);
        
        loop
        {
            bool signal = DigitalRead(GP16);
            DigitalWrite(GP17, signal);
        }
        */
    }
}
