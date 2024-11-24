program Pico433Tx
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        UART.Setup(57600);
        PinMode(GP17, PinModeOption.Output);
        
        loop // transmission loop
        {
            string time = (Millis).ToString();
            DigitalWrite(GP17, true);
            IO.WriteLn("Send: " + time);
            UART.WriteString(time + Char.EOL);
            DigitalWrite(GP17, false);
            Delay(1000);
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
                       IO.WriteLn(returned);
                       returned = "";
                       break;
                   }
                   returned += ch;
                } // reception loop
            }
        } // transmission loop
    }
}
