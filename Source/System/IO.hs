unit IO
{
    // #### globals at the top of the file so we can keep track of them:
    
    bool echoToLCD;
    // #### end of globals
    
#ifndef MCU
    uses "/Source/System/Screen"
#else    
    uses "/Source/Library/Screen.hs"
#endif
    uses "/Source/System/Serial"
    uses "/Source/System/Keyboard"
    uses "/Source/System/Clipboard"

#ifndef RUNTIME    
    uses  "/Source/System/String" // for keyboard buffer
#endif
   
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
    
    bool EchoToLCD { set { echoToLCD = value; } get { return echoToLCD; } }

    uint LineMax
    {
        get 
        { 
#if defined(SERIALCONSOLE) || defined(H6502)
            return 120;
#else            
            return Screen.Columns-1;
#endif
        }
    }
    Clear()
    {
#ifdef SERIALCONSOLE
        Serial.WriteChar(char(0x0C)); // form feed
        if (echoToLCD)
        {
#ifdef RUNTIME
            HRScreen.Clear();
#else
            Screen.Clear();
#endif
        }
#else        
    #ifdef H6502
        if (echoToLCD)
        {
            Screen.Clear();
        }
        Serial.WriteChar(char(0x0C)); // form feed
    #else
        Screen.Clear();
    #endif
#endif
    }
    writeDigit(uint uthis)
    {
        uint digit = uthis % 10;
        char c = HRChar.ToDigit(byte(digit));
        uthis = uthis / 10;
        if (uthis != 0)
        {
            writeDigit(uthis);
        }
        Write(c);
    }
    WriteInt(int this)
    {
        if (this < 0)
        {
            Write('-');
            this = 0 - this;
        }
        uint uthis = uint(this);
        writeDigit(uthis);
    }
    WriteUInt(uint this)
    {
        writeDigit(this);
    }
    
    WriteHex(byte b)
    {
        byte msn = ((b >> 4) & 0xF);
        Write(ToHex(msn));
        byte lsn = b & 0xF;
        Write(ToHex(lsn));
    }
    WriteHex(uint u)
    {
        byte msb = byte(u >> 8);
        WriteHex(msb);
        byte lsb = byte(u & 0xFF);
        WriteHex(lsb);
    }
#ifdef SERIALCONSOLE
    Write(char c)
    {
        Serial.WriteChar(c);
        if (echoToLCD)
        {
            if (char(0x0D) == c)
            {
#ifndef RUNTIME
                Screen.PrintLn();
#endif 
            }
            else if (char(0x0C) == c)
            {
#ifndef RUNTIME
                Screen.Clear();
#endif 
            }
            else
            {
#ifndef RUNTIME
                Screen.Print(c);
#endif 
            }
        }
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
        Write(char(0x0D));
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
        WriteBoth(char(0x0D), both);
    }
    
#ifdef H6502    
    WriteBoth(char c, bool both)
    {

        Serial.WriteChar(c);
        if (both)
        {
            if (c == char(0x0D))
            {
                Screen.PrintLn(); // PLATFORM
            }
            else if (c == char(0x0C))
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
        if (c == char(0x0D))
        {
            Screen.PrintLn(); // PLATFORM
        }
        else if (c == char(0x0C))
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
            if (HaveKey())
            {
                ch = PopKey();
            }
            else
            {
#if defined(SERIALCONSOLE) || defined(H6502)
                ch = Serial.ReadChar();
#else                
                Key key = Keyboard.ReadKey();
                if (key == Key.ControlV)
                {
                    if (Clipboard.HasText)
                    {
                        char pc = char(0);
                        loop
                        {
                            char cch = Clipboard.GetChar();
                            if (cch == char(0))
                            {
                                break;
                            }
                            
                            if ((cch == char(0x0A)) && (pc == char(0x0D)))      // 0x0D 0x0A -> 0x0D
                            {
                                pc = cch;
                                continue;
                            }
                            else if ((cch == char(0x0D)) && (pc == char(0x0A))) // 0x0A 0x0D -> 0x0A
                            {
                                pc = cch;
                                continue;
                            }
                            else
                            {
                                pc = cch;
                                if (cch == char(0x0A))
                                {
                                    PushKey(char(0x0D));
                                }
                                else
                                {
                                    PushKey(cch);
                                }        
                            }
                        }
                        continue; // get the first ch from the keyboardBuffer above
                    }
                }
                ch = TransformKey(key);
#endif                
            }
            byte b = byte(ch);
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
    bool IsAvailable
    {
        get
        {
#if defined(SERIALCONSOLE) || defined(H6502)
            return (HaveKey()) || Serial.IsAvailable;
#else
            return Keyboard.IsAvailable;
#endif
        }
    }
    bool IsBreak()
    {
#if defined(SERIALCONSOLE) || defined(H6502)
        while (Serial.IsAvailable)
        {
            char ch = Serial.ReadChar();
            if (ch == char(0x03)) // <ctrl><C>?
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
    uint  keyboardInPointer;
    uint  keyboardOutPointer;
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
    
#ifndef TINYHOPPER    
    string keyboardBuffer; // used by Read(..)
#endif        
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
        return keyboardBuffer.Length > 0;
    }    
#endif    
    
    
}
