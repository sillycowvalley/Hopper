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
    
    string keyboardBuffer;
    
    {
        Serial.Connect();
        
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
