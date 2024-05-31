unit IO
{
    // #### globals at the top of the file so we can keep track of them:
    
    bool echoToLCD;
    // #### end of globals
    
#if !defined(SERIAL_CONSOLE) && defined(MCU)
    #define SERIAL_CONSOLE
#endif
 
#ifndef MCU
    uses "/Source/System/Screen"
#else    
    uses "/Source/Library/Screen"
#endif
    uses "/Source/System/Serial"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Clipboard"

#ifndef RUNTIME    
    uses  "/Source/System/String" // for keyboard buffer
#endif

    bool EchoToLCD     { set { echoToLCD = value; } get { return echoToLCD; } }
#ifdef MCU
    bool EchoToDisplay { set { echoToLCD = value; } get { return echoToLCD; } } // for clarity
#endif    
   
    // Maximum width of lines on screen or serial console:
    //   uint LineMax
    //
    // Is there a <ctrl><X> waiting in the Keyboard or Serial buffer?
    // Other content is buffered for Read(..)
    //   bool IsBreak() 
    //
    // Wait for and read the next character from Serial for HOPPER_6502 and from keyboard for Windows.
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
    // On HOPPER_6502 generic Write outputs to Serial and
    // both=true sends output to both Serial and Screen (LCD):
    //
    //   Write(char c)      | WriteBoth(char c, bool both)
    //   Write(string s)    | WriteBoth(string s, bool both)
    //   WriteLn(string s)  | WriteLnBoth(string s, bool both)
    //   WriteLn()          | WriteLnBoth(bool both)
    //   WriteLn(int value) | WriteLnBoth(int value, bool both)
    
    
    uint LineMax
    {
        get 
        { 
#if defined(SERIAL_CONSOLE) || defined(HOPPER_6502)
            return 120;
#else            
            return Screen.Columns-1;
#endif
        }
    }
    Clear()
    {
#ifdef SERIAL_CONSOLE
        Serial.WriteChar(Char.Formfeed);
        if (echoToLCD)
        {
#ifdef RUNTIME
            HRScreen.Clear();
#else
            Screen.Clear();
#endif
        }
#else        
    #ifdef HOPPER_6502
        if (echoToLCD)
        {
            Screen.Clear();
        }
        Serial.WriteChar(Char.Formfeed);
    #else
        Screen.Clear();
    #endif
#endif
    }
    writeDigit(uint uthis)
    {
        char c;
        uint digit;
        digit = uthis % 10;
        c = HRByte.ToDigit(byte(digit));
        uthis = uthis / 10;
        if (uthis != 0)
        {
            writeDigit(uthis);
        }
        Write(c);
    }
    WriteInt(int this)
    {
        uint uthis;
        if (this < 0)
        {
            Write('-');
            this = 0 - this;
        }
        uthis = uint(this);
        writeDigit(uthis);
    }
    WriteUInt(uint this)
    {
        writeDigit(this);
    }
    
    WriteHex(byte b)
    {
        byte msn;
        byte lsn;
        msn = ((b >> 4) & 0xF);
        Write(ToHex(msn));
        lsn = b & 0xF;
        Write(ToHex(lsn));
    }
    WriteHex(uint u)
    {
        byte msb;
        byte lsb;
        msb = byte(u >> 8);
        WriteHex(msb);
        lsb = byte(u & 0xFF);
        WriteHex(lsb);
    }
#ifdef SERIAL_CONSOLE
    Write(char c)
    {
        Serial.WriteChar(c);
#ifndef RUNTIME
        if (echoToLCD)
        {
            if (Char.EOL == c)
            {
                Screen.PrintLn();
            }
            else if (Char.Formfeed == c)
            {
                Screen.Clear();
            }
            else
            {
                Screen.Print(c);
            }
        }
#endif
    }
    Write(string s)
    {
        foreach (var c in s)
        {
            Write(c);
        }
    }
    WriteLn()
    {
        Write(Char.EOL);
    }
    WriteLn(string s)
    {
        Write(s);
        WriteLn();
    }
#else

    Write(char c)
    {
        WriteBoth(c, echoToLCD);
    }

    Write(string s)
    {
        WriteBoth(s, echoToLCD);
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
        WriteLnBoth(s, echoToLCD);
    }
    WriteLnBoth(string s, bool both)
    {
        WriteBoth(s, both);
        WriteLnBoth(both);
    }

    WriteLn()
    {
        WriteLnBoth(echoToLCD);
    }
    WriteLnBoth(bool both)
    {
        WriteBoth(Char.EOL, both);
    }
    
#ifdef HOPPER_6502    
    WriteBoth(char c, bool both)
    {

        Serial.WriteChar(c);
        if (both)
        {
            if (c == Char.EOL)
            {
                Screen.PrintLn(); // PLATFORM
            }
            else if (c == Char.Formfeed)
            {
                Screen.Clear();
            }
            else
            {
                Screen.Print(c); // PLATFORM
            }
        }
    }
#else
    WriteBoth(char c, bool both)
    {
        if (c == Char.EOL)
        {
            Screen.PrintLn(); // PLATFORM
        }
        else if (c == Char.Formfeed)
        {
            Screen.Clear();
        }
        else
        {
            Screen.Print(c); // PLATFORM
        }
    }
#endif        
    
    
#endif
 
    char TransformKey(Key key)
    {
        char ch = key.ToChar();
        if (key == (Key.Control | Key.ModC))
        {
            ch = Char.Break; // for the debugger (on Windows)
        }
        else
        {
            key = (key & Keyboard.Key.Mask); // strip the modifiers
            if ((key == Key.Enter) || (key == Key.ModEnter))
            {
                ch = Char.EOL;
            }
            else if ((key == Key.Escape) || (key == Key.ModEscape))
            {
                ch = Char.Escape;
            }
            else if ((key == Key.Backspace) || (key == Key.ModBackspace))
            {
                ch = Char.Backspace;
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
            if (HaveKey())
            {
                ch = PopKey();
            }
            else
            {
#if defined(SERIAL_CONSOLE) || defined(HOPPER_6502)
                ch = Serial.ReadChar();
#else                
                Key key = Keyboard.ReadKey();
                if (key == Key.ControlV)
                {
                    if (Clipboard.HasText)
                    {
                        loop
                        {
                            char cch = Clipboard.GetChar();
                            if (cch == char(0))
                            {
                                break;
                            }
                            
                            if (cch == Char.EOL)
                            {
                                continue;
                            }
                            else
                            {
                                PushKey(cch);
                            }
                        }
                        continue; // get the first ch from the keyboardBuffer above
                    }
                }
                ch = TransformKey(key);
#endif                
            }
            byte b = byte(ch);
            if ((ch == Char.Backspace) || (ch == Char.EOL) || (ch == Char.Escape))
            {
                // from above : ok
            }
            else if (ch == Char.Break)
            {
                // <ctrl><C> from Read() ?
            }
            else if ((ch >= ' ') && (ch <= '~')) 
            {
                // ASCII 32 to 126 : ok
            }
            else if (ch == char(0xE0))
            {
                // Maker
            }
            else
            {
                continue;
            }
            break;
        }
        return ch;    
    }
     bool ReadLn(ref string str)
    {
        char ch;
        bool result;
        String.Build(ref str);
        loop
        {
            ch = Serial.ReadChar();
            if (ch == Char.EOL) 
            { 
                WriteLn();
                result = true; // good
                break; 
            }
            else if (ch == Char.Escape) 
            {
                while (str.Length > 0)
                {
                    Write(Char.Backspace);
                    Write(' ');
                    Write(Char.Backspace);
                    str = str.Substring(0, str.Length-1);   
                }
                continue;
            }
            else if (ch == Char.Backspace)
            {
                if (str.Length > 0)
                {
                    Write(Char.Backspace);
                    Write(' ');
                    Write(Char.Backspace);
                    str = str.Substring(0, str.Length-1);   
                }
                continue;
            }
            String.Build(ref str, ch);
            Write(ch);    
        }
        return result;
    }
    
    bool IsAvailable
    {
        get
        {
#if defined(SERIAL_CONSOLE) || defined(HOPPER_6502)
            return (HaveKey()) || Serial.IsAvailable;
#else
            return Keyboard.IsAvailable;
#endif
        }
    }
    bool IsBreak()
    {
#if defined(SERIAL_CONSOLE) || defined(HOPPER_6502)
        while (Serial.IsAvailable)
        {
            char ch = Serial.ReadChar();
            if (ch == Char.Break) // <ctrl><C>?
            {
                return true;
            }
            // buffer all the non <ctrl><C> characters seen here
            PushKey(ch);
        }
#else        
        while (Keyboard.IsAvailable)
        {
            Key key = Keyboard.ReadKey();
            if (key == (Key.Control | Key.ModC)) // <ctrl><C>?
            {
                return true;
            } 
            // buffer all the non <ctrl><C> characters seen here
            char ch = IO.TransformKey(key);
            PushKey(ch);
        }
#endif
        return false;
    }
    
#ifdef RUNTIME

    // top 256 bytes circular keyboard buffer
    uint keyboardBufferBase;
    uint keyboardInPointer;
    uint keyboardOutPointer;
    AssignKeyboardBuffer(uint buffer)
    {
        keyboardBufferBase = buffer;
    }
    
    PushKey(char c)
    {
        byte k = byte(c);
        Memory.WriteByte(keyboardBufferBase + keyboardInPointer, k);
        if (keyboardInPointer == 0xFF)
        {
            keyboardInPointer = 0;
        }
        else
        {
            keyboardInPointer++;
        }
    }
    char PopKey()
    {
        char c = char(Memory.ReadByte(keyboardBufferBase + keyboardOutPointer));
        if (keyboardOutPointer == 0xFF)
        {
            keyboardOutPointer = 0;
        }
        else
        {
            keyboardOutPointer++;
        }   
        return c;  
    }
    bool HaveKey()
    {
        return keyboardInPointer != keyboardOutPointer;
    }
    
    
    
#else        
    string keyboardBuffer;
    PushKey(char c)
    {
        keyboardBuffer = keyboardBuffer + c;
    }
    char PopKey()
    {
        char c = keyboardBuffer[0];
        keyboardBuffer = keyboardBuffer.Substring(1);  
        return c;  
    }
    bool HaveKey()
    {
        return keyboardBuffer.Length != 0;
    }    
#endif    
    
    
}
