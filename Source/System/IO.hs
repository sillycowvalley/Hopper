unit IO
{
    uses "/Source/System/Screen"
    uses "/Source/System/Serial"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Clipboard"
    
    // Maximum width of lines on screen or serial console:
    //   uint LineMax
    //
    // Is there a <ctrl><X> waiting in the Keyboard or Serial buffer?
    // Other content is buffered for Read(..)
    //   bool IsBreak() 
    //
    // Wait for and read the next character from Serial for H6502 and from keyboard for Windows.
    // Non-printable keys are rejected except for <ctrl><C>, Enter, Backspace and Escape which
    // are transformed to their ASCII values. <ctrl><V> pastes the clipboard into the keyboard buffer
    // on the client (Windows) side:
    //   char Read()
    //
    // Helper method that transforms keys to useful ASCII for Serial: 
    //   char TransformKey(Key key)
    //
    // Output methods similar to Print(..) and PrintLn(..).
    // On Windows, they only output to Screen.
    // On H6502 generic Write outputs to Serial and
    // both=true sends output to both Serial and Screen (LCD):
    //
    //   Write(char c)      | WriteBoth(char c, bool both)
    //   Write(string s)    | WriteBoth(string s, bool both)
    //   WriteLn(string s)  | WriteLnBoth(string s, bool both)
    //   WriteLn()          | WriteLnBoth(bool both)
    //   WriteLn(int value) | WriteLnBoth(int value, bool both)
    
#ifndef TINYHOPPER    
    string keyboardBuffer; // used by Read(..)
    string integerString;  // re-used by WriteBoth(int value, bool both)
#endif
    uint LineMax
    {
        get 
        { 
#ifdef H6502
            return 80;
#else            
            return Screen.Columns-1;
#endif
        }
    }
    Clear()
    {
        Screen.Clear();
#ifdef H6502
        Serial.WriteChar(char(0x0C)); // form feed
#endif
    }
    
    Write(char c)
    {
        WriteBoth(c, false);
    }
    WriteBoth(char c, bool both)
    {
#ifdef H6502
        Serial.WriteChar(c);
        if (both)
        {
            if (c == char(0x0D))
            {
                Screen.PrintLn(); // PLATFORM
            }
            else
            {
                Screen.Print(c); // PLATFORM
            }
        }
#else
        if (c == char(0x0D))
        {
            Screen.PrintLn(); // PLATFORM
        }
        else
        {
            Screen.Print(c); // PLATFORM
        }
#endif        
    }
    Write(string s)
    {
        WriteBoth(s, false);
    }
    WriteBoth(string s, bool both)
    {
        foreach (var c in s)
        {
            WriteBoth(c, both);
        }
    }
    WriteLn(string s)
    {
        WriteLnBoth(s, false);
    }
    WriteLnBoth(string s, bool both)
    {
        WriteBoth(s, both);
        WriteLnBoth(both);
    }
    WriteLn()
    {
        WriteLnBoth(false);
    }
    WriteLnBoth(bool both)
    {
        WriteBoth(char(0x0D), both);
    }
    Write(int value)
    {
        WriteBoth(value, false);
    }
    
    WriteBoth(int value, bool both)
    {
        String.Build(ref integerString);
        
        bool negative = false;
        if (value < 0)
        {
            negative = true;
            value = 0 - value;
        }
        else if (value == 0)
        {
            String.Build(ref integerString, '0');
        }
        while (value != 0)
        {
            int digit = value % 10;
            char c = Char.ToDigit(byte(digit));
            String.BuildFront(ref integerString, c);
            value = value / 10;
        }
        if (negative)
        {
            String.BuildFront(ref integerString, '-');
        }
        WriteBoth(integerString, both);
    }
    
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
    
    char Read()
    {
        char ch;
        loop
        {
            if (keyboardBuffer.Length != 0)
            {
                ch = keyboardBuffer[0];
                keyboardBuffer = keyboardBuffer.Substring(1);
            }
            else
            {
#ifdef H6502
                ch = Serial.ReadChar();
#else                
                Key key = Keyboard.ReadKey();
                if (key == Key.ControlV)
                {
                    if (Clipboard.HasText)
                    {
                        string clipboardText = Clipboard.GetText(); // PLATFORM
                        keyboardBuffer = keyboardBuffer + clipboardText;
                        continue; // get the first ch from the keyboardBuffer above
                    }
                }
                ch = TransformKey(key);
#endif                
            }
            
            if ((ch == char(0x08)) || (ch == char(0x0D)) || (ch == char(0x1B)))
            {
                // from above : ok
            }
            else if ((ch >= ' ') && (ch <= '~')) 
            {
                // ASCII 32 to 126 : ok
            }
            else
            {
                continue;
            }
            break;
        }
        return ch;    
    }
    
    bool IsBreak() // only used by Tigger BASIC
    {
        bool result;
#ifdef H6502
        while (Serial.IsAvailable)
        {
            char ch = Serial.ReadChar();
            if (ch == char(0x18)) // <ctrl><X>?
            {
                result = true;
                break;
            }
            // buffer all the non <ctrl><X> characters seen here
            keyboardBuffer = keyboardBuffer.Append(ch);
        }
#else        
        while (Keyboard.IsAvailable)
        {
            Key key = Keyboard.ReadKey();
            if (key == (Key.Control | Key.ModX)) // <ctrl><X>?
            {
                result = true;
                break;
            } 
            // buffer all the non <ctrl><X> characters seen here
            char ch = IO.TransformKey(key);
            keyboardBuffer = keyboardBuffer.Append(ch);
        }
#endif
        return result;
    }
}
