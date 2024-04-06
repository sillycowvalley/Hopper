program Term
{
    uses "/Source/System/System"
    uses "/Source/System/Serial"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    uses "/Source/System/Clipboard"
    
    bool DoConnect()
    {
        bool success = false;
        <string> ports = Serial.Ports;
        uint i;
        char defCh;
        PrintLn("COM Ports:");
        foreach (var port in ports)
        {
            i++;
            string content = "  " + (i.ToString()).LeftPad(' ', 3) + ": " + port;
            if (i == ports.Count)
            {
                content = content + " (default)";
                defCh = char(48+i);
            }
            PrintLn(content);
        }
        PrintLn("Press number to select port (or <enter> for default)");
        Key key = ReadKey();
        char ch = Keyboard.ToChar(key);
        if (key == Key.Enter)
        {
            ch = defCh;
        }
        if ((ch >= '1') && (ch <= '9'))
        {
            i = uint(ch) - 49;
            if (i < ports.Count)
            {
                string port = (ports[i]).Replace("COM", "");
                uint iport;
                if (UInt.TryParse(port, ref iport))
                {
                    Serial.Connect(iport);
                    PrintLn("Connected to COM" + iport.ToString() + " (<Alt><F4> to exit)");
                    success = true;
                }
            }
        }
        
        
        return success;
    }
    string keyboardBuffer;
    
    {
        if (!DoConnect())
        {
            return;
        }
        
        Screen.ShowCursor = true;
        loop
        {
            if ((Keyboard.IsAvailable) || (keyboardBuffer.Length != 0))
            {
                char ch; 
                char maker;   
                bool exitKey;
                if (keyboardBuffer.Length != 0)
                {
                    ch = keyboardBuffer[0];
                    keyboardBuffer = keyboardBuffer.Substring(1);
                    Serial.WriteChar(ch);
                }
                else if (Keyboard.IsAvailable)
                {
                    Key key = Keyboard.ReadKey();
                    
                    if (key == Key.ControlV)
                    {
                       if (Clipboard.HasText)
                       {
                           keyboardBuffer = keyboardBuffer + Clipboard.GetText();
                           continue;
                       }
                    }
                    else if (key == (Key.Alt | Key.F4))
                    {
                        exitKey = true;
                    }
                    ch = Keyboard.ToSerial(key, ref maker);
                    if (maker != char(0))
                    {
                        Serial.WriteChar(maker);
                    }
                    Serial.WriteChar(ch);
                }
                
                Delay(1);
                if (exitKey)
                {
                    Screen.ShowCursor = false;
                    Screen.PrintLn();
                    break;
                }
            }
            if (Serial.IsAvailable)
            {
                Screen.ShowCursor = false;
                char pch;
                while (Serial.IsAvailable)
                {
                    char ch = Serial.ReadChar();
                    if ((ch == char(0x0A)) || (ch == char(0x0D)))
                    {
                        if ((pch == char(0x0D)) && (ch == char(0x0A)))
                        {
                            // convert '\r\n' to '\n'
                        }
                        else
                        {
                            Screen.PrintLn();
                        }
                    }
                    else if (ch == char(0x0C))
                    {
                        Screen.Clear();
                    }
                    else if (ch == char(0x08))
                    {
                        Screen.Print(ch);
                    }
                    else if ((byte(ch) < 32) || (byte(ch) > 127))
                    {
                    }
                    else
                    {
                        Screen.Print(ch);
                    }
                    pch = ch;
                }
                Screen.ShowCursor = true;
            }
        }
        
        Serial.Close();
    }
}
