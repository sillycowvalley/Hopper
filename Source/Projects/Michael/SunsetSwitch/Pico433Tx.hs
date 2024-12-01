program Pico433Tx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    const byte ledPin = GP17;
    
    Hopper()
    {
        PinMode(ledPin, PinModeOption.Output);
        
        UART.Setup(9600);
        
        loop // transmission loop
        {
            string time = (Millis).ToString();
            DigitalWrite(ledPin, true);
            IO.Write("Sent: " + time);
            UART.WriteString(time + Char.EOL);
            Delay(250);
            DigitalWrite(ledPin, false);
            Delay(750);
            if (UART.IsAvailable)
            {
                string returned;
                loop // reception loop
                {
                   if (!UART.IsAvailable)
                   {
                       returned = "";
                       break;
                   }
                   char ch = UART.ReadChar();
                   if (ch == Char.EOL)
                   {
                       // finish reception on Char.EOL
                       IO.Write(", " + returned);
                       returned = "";
                       break;
                   }
                   returned += ch;
                } // reception loop
            }
            IO.WriteLn();
        } // transmission loop
    }
}
