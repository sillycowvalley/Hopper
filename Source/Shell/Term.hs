program Term
{
    uses "/Source/System/System"
    uses "/Source/System/Serial"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    uses "/Source/System/Clipboard"
    
    
    char TransformKey(Key key)
    {
        char ch = key.ToChar();
        if (key == (Key.Control | Key.ModX))
        {
            ch = char(0x18);
        }
        else if (key == (Key.ControlC))
        {
            ch = char(0x03); // for the debugger (on Windows)
        }
        else
        {
            key = (key & Keyboard.Key.Mask); // strip the modifiers
            if ((key == Key.Enter) || (key == Key.ModEnter))
            {
                ch = char(0x0D);
            }
            else if ((key == Key.Escape) || (key == Key.ModEscape))
            {
                ch = char(0x1B);
            }
            else if ((key == Key.Backspace) || (key == Key.ModBackspace))
            {
                ch = char(0x08);
            }
            else if (key == Key.ModSpace)
            {
                ch = ' ';
            }
        }
        return ch;
    }
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
            if (i == ports.Length)
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
            if (i < ports.Length)
            {
                string port = (ports[i]).Replace("COM", "");
                uint iport;
                if (UInt.TryParse(port, ref iport))
                {
                    Serial.Connect(iport);
                    PrintLn("Connected to COM" + iport.ToString() + " ('Q' to quit)");
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
        
        
        
        loop
        {
            if ((Keyboard.IsAvailable) || (keyboardBuffer.Length > 0))
            {
                char ch;    
                if (keyboardBuffer.Length > 0)
                {
                    ch = keyboardBuffer[0];
                    keyboardBuffer = keyboardBuffer.Substring(1);
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
                     ch = TransformKey(key);
                }
                Serial.WriteChar(ch);
                Delay(1);
                if (ch.ToUpper() == 'Q')
                {
                    break;
                }
            }
            if (Serial.IsAvailable)
            {
                char ch = Serial.ReadChar();
                if (ch == char(0x0A))
                {
                    PrintLn();
                }
                else
                {
                    Print(ch);
                }
            }
        }
        
        Serial.Close();
    }
}
