program Pico433Tx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        UART.Setup(57600);
        PinMode(GP16, PinModeOption.Output);
        PinMode(GP17, PinModeOption.Output);
        
        loop
        {
            string time = (Millis).ToString();
            DigitalWrite(GP17, true);
            IO.WriteLn("Send: " + time);
            UART.WriteString(time + Char.EOL);
            DigitalWrite(GP17, false);
            Delay(1000);
        }
        /*
        UART.WriteChar('!');
        if (UART.IsAvailable)
        {
            _ = UART.ReadChar();
        }
       
        
        
        
        DigitalWrite(GP16, false);
        
        loop
        {
            DigitalWrite(GP16, true);
            DigitalWrite(GP17, true);
            Delay(50);
            DigitalWrite(GP16, false);
            DigitalWrite(GP17, false);
            Delay(950);
            
        }
        */
    }
}
