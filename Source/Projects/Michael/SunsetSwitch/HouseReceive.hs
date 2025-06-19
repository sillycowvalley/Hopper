program HouseRecieve
{
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    
    Hopper()
    {
        string message;
        UART.Setup(9600);
        
        long lastMessage;
        loop
        {
            Time.Delay(250);
            while (UART.IsAvailable)
            {
                char ch = UART.ReadChar();
                if (ch == Char.EOL)
                {
                    long thisMessage = Time.Millis;
                    IO.WriteLn("Message: '" + message + "' " + thisMessage.ToString() + " " + (thisMessage - lastMessage).ToString());
                    lastMessage = thisMessage;
                    message = "";
                    break;
                }
                else
                {
                    byte b = byte(ch);
                    if (b >= 32)
                    {
                        IO.Write("'" + ch + "' ");
                    }
                    IO.WriteLn((byte(ch)).ToHexString(2));
                    message += ch;
                }
            }
        }
    }

}
