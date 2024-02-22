unit Keyboard
{
    flags Key
    {
        NoKey     = 0x0000,
        
        Alt     = 0x0100,
        Control = 0x0200,
        Shift   = 0x0400,
        Mask    = 0xF0FF,
        
        Click      = 0xE0FF, // pointer click event
        Scroll     = 0xE0FE, // pointer scroll event
        ClickRight = 0xE0FD, // pointer right click event
        
        Space     = 0x0020,
        Delete    = 0xE071,
        Backspace = 0x0008,
        Tab       = 0xE00D,
        Enter     = 0x000D,
        Escape    = 0x001B,
        Period    = 0x002E,

        ModSpace     = 0xE029,
        ModEscape    = 0xE076,
        ModInsert    = 0xE070,
        ModBackspace = 0xE066,
        ModEnter     = 0xE05A,
        ModEsc       = 0xE076,

        F1  = 0xE005,
        F2  = 0xE006,
        F3  = 0xE004,
        F4  = 0xE00C,
        F5  = 0xE003,
        F6  = 0xE00B,
        F7  = 0xE083,
        F8  = 0xE00A,
        F9  = 0xE001,
        F10 = 0xE009,
        F11 = 0xE078,
        F12 = 0xE007,

        Left  = 0xE06B,
        Right = 0xE074,

        Up   = 0xE075,
        Down = 0xE072,

        Home = 0xE06C,
        End  = 0xE069,
        
        PageDown = 0xE07A,
        PageUp   = 0xE07D,

        ModPeriod = 0xE049,

        
        Mod0 = 0xE045,
        Mod1 = 0xE016,
        Mod2 = 0xE01E,
        Mod3 = 0xE026,
        Mod4 = 0xE025,
        Mod5 = 0xE02E,
        Mod6 = 0xE036,
        Mod7 = 0xE03D,
        Mod8 = 0xE03E,
        Mod9 = 0xE046,

        ModA = 0xE01C,
        ModB = 0xE032,
        ModC = 0xE021,
        ModD = 0xE023,
        ModE = 0xE014,
        ModF = 0xE02B,
        ModG = 0xE034,
        ModH = 0xE033,
        ModI = 0xE043,
        ModJ = 0xE03B,
        ModK = 0xE042,
        ModL = 0xE04B,
        ModM = 0xE03A,
        ModN = 0xE031,
        ModO = 0xE044,
        ModP = 0xE04D,
        ModQ = 0xE015,
        ModR = 0xE02D,
        ModS = 0xE01B,
        ModT = 0xE02C,
        ModU = 0xE03C,
        ModV = 0xE02A,
        ModW = 0xE01D,
        ModX = 0xE022,
        ModY = 0xE035,
        ModZ = 0xE01A,
        
        ControlA = 0xE21C, // Control | ModA
        ControlC = 0xE221, // Control | ModC
        ControlV = 0xE22A, // Control | ModV
        
        A = 0x41,
        a = 0x61,
        B = 0x42,
        b = 0x62,
        C = 0x43,
        c = 0x63,
        D = 0x44,
        d = 0x64,
        E = 0x45,
        e = 0x65,
        F = 0x46,
        f = 0x66,
        G = 0x47,
        g = 0x67,
        H = 0x48,
        h = 0x68,
        I = 0x49,
        i = 0x69,
        J = 0x4A,
        j = 0x6A,
        K = 0x4B,
        k = 0x6B,
        L = 0x4C,
        l = 0x6C,
        M = 0x4D,
        m = 0x6D,
        N = 0x4E,
        n = 0x6E,
        O = 0x4F,
        o = 0x6F,
        P = 0x50,
        p = 0x70,
        Q = 0x51,
        q = 0x71,
        R = 0x52,
        r = 0x72,
        S = 0x53,
        s = 0x73,
        T = 0x54,
        t = 0x74,
        U = 0x55,
        u = 0x75,
        V = 0x56,
        v = 0x76,
        W = 0x57,
        w = 0x77,
        X = 0x58,
        x = 0x78,
        Y = 0x59,
        y = 0x79,
        Z = 0x5A,
        z = 0x7A,
        
        N0 = 0x30,
        N1 = 0x31,
        N2 = 0x32,
        N3 = 0x33,
        N4 = 0x34,
        N5 = 0x35,
        N6 = 0x36,
        N7 = 0x37,
        N8 = 0x38,
        N9 = 0x39,
        
    }
    
    // TODO : how to make this prettier in terms of syntax? 
    // - combination of '.' and 'this' as hints? (Modifiers.ToString)
    string ModifiersToString(Key this) 
    {
        string output;
        if (Keyboard.Key.NoKey != (this & Keyboard.Key.Alt))
        {
            output = output.Append("Alt");
        }
        if (Keyboard.Key.NoKey != (this & Keyboard.Key.Control))
        {
            if (output.Length != 0)
            {
                output = output.Append('|');
            }
            output = output.Append("Control");
        }
        if (Keyboard.Key.NoKey != (this & Keyboard.Key.Shift))
        {
            if (output.Length != 0)
            {
                output = output.Append('|');
            }
            output = output.Append("Shift");
        }
        if (output.Length == 0)
        {
            output = output.Append("None");
        }
        return output;
    }
    
    string KeyToString(Key this) 
    {
        string output;
        this = (this & Keyboard.Key.Mask); // strip the modifiers
        switch (this)
        {
            case Keyboard.Key.Tab:
            {
                output = "Tab";
            }
            case Keyboard.Key.Click:
            {
                output = "Click";
            }
            case Keyboard.Key.Scroll:
            {
                output = "Scroll";
            }
            case Keyboard.Key.Escape:
            {
                output = "Esc";
            }
            case Keyboard.Key.Enter:
            {
                output = "Enter";
            }
            case Keyboard.Key.Delete:
            {
                output = "Del";
            }
            case Keyboard.Key.Backspace:
            {
                output = "Backspace";
            }
            
            case Keyboard.Key.F1:
            {
                output = "F1";
            }
            case Keyboard.Key.F2:
            {
                output = "F2";
            }
            case Keyboard.Key.F3:
            {
                output = "F3";
            }
            case Keyboard.Key.F4:
            {
                output = "F4";
            }
            case Keyboard.Key.F5:
            {
                output = "F5";
            }
            case Keyboard.Key.F6:
            {
                output = "F6";
            }
            case Keyboard.Key.F7:
            {
                output = "F7";
            }
            case Keyboard.Key.F8:
            {
                output = "F8";
            }
            case Keyboard.Key.F9:
            {
                output = "F9";
            }
            case Keyboard.Key.F10:
            {
                output = "F10";
            }
            case Keyboard.Key.F11:
            {
                output = "F11";
            }
            case Keyboard.Key.F12:
            {
                output = "F12";
            }
            
            case Keyboard.Key.Home:
            {
                output = "Home";
            }
            case Keyboard.Key.End:
            {
                output = "End";
            }
            case Keyboard.Key.PageUp:
            {
                output = "PageUp";
            }
            case Keyboard.Key.PageDown:
            {
                output = "PageDown";
            }
            
            case Keyboard.Key.Left:
            {
                output = "Left";
            }
            case Keyboard.Key.Right:
            {
                output = "Right";
            }
            case Keyboard.Key.Up:
            {
                output = "Up";
            }
            case Keyboard.Key.Down:
            {
                output = "Down";
            }
            
            case Keyboard.Key.ModSpace:
            {
                output = " ";
            }
            case Keyboard.Key.ModEscape:
            {
                output = "Escape";
            }
            case Keyboard.Key.ModInsert:
            {
                output = "Insert";
            }
            case Keyboard.Key.ModBackspace:
            {
                output = "Backspace";
            }
            case Keyboard.Key.ModEnter:
            {
                output = "Enter";
            }
            
            case Keyboard.Key.Mod0:
            {
                output = "0";
            }
            case Keyboard.Key.Mod1:
            {
                output = "1";
            }
            case Keyboard.Key.Mod2:
            {
                output = "2";
            }
            case Keyboard.Key.Mod3:
            {
                output = "3";
            }
            case Keyboard.Key.Mod4:
            {
                output = "4";
            }
            case Keyboard.Key.Mod5:
            {
                output = "5";
            }
            case Keyboard.Key.Mod6:
            {
                output = "6";
            }
            case Keyboard.Key.Mod7:
            {
                output = "7";
            }
            case Keyboard.Key.Mod8:
            {
                output = "8";
            }
            case Keyboard.Key.Mod9:
            {
                output = "9";
            }
            
            case Keyboard.Key.ModA:
            {
                output = "A";
            }
            case Keyboard.Key.ModB:
            {
                output = "B";
            }
            case Keyboard.Key.ModC:
            {
                output = "C";
            }
            case Keyboard.Key.ModD:
            {
                output = "D";
            }
            case Keyboard.Key.ModE:
            {
                output = "E";
            }
            case Keyboard.Key.ModF:
            {
                output = "F";
            }
            case Keyboard.Key.ModG:
            {
                output = "G";
            }
            case Keyboard.Key.ModH:
            {
                output = "H";
            }
            case Keyboard.Key.ModI:
            {
                output = "I";
            }
            case Keyboard.Key.ModJ:
            {
                output = "J";
            }
            case Keyboard.Key.ModK:
            {
                output = "K";
            }
            case Keyboard.Key.ModL:
            {
                output = "L";
            }
            case Keyboard.Key.ModM:
            {
                output = "M";
            }
            case Keyboard.Key.ModN:
            {
                output = "N";
            }
            case Keyboard.Key.ModO:
            {
                output = "O";
            }
            case Keyboard.Key.ModP:
            {
                output = "P";
            }
            case Keyboard.Key.ModQ:
            {
                output = "Q";
            }
            case Keyboard.Key.ModR:
            {
                output = "R";
            }
            case Keyboard.Key.ModS:
            {
                output = "S";
            }
            case Keyboard.Key.ModT:
            {
                output = "T";
            }
            case Keyboard.Key.ModU:
            {
                output = "U";
            }
            case Keyboard.Key.ModV:
            {
                output = "V";
            }
            case Keyboard.Key.ModW:
            {
                output = "W";
            }
            case Keyboard.Key.ModX:
            {
                output = "X";
            }
            case Keyboard.Key.ModY:
            {
                output = "Y";
            }
            case Keyboard.Key.ModZ:
            {
                output = "Z";
            }
            
            default:
            {
                uint i = uint(this);
                if ((i >= 32) && (i <= 127)) // printable ASCII
                {
                    char c;
                    c = char(i);
                    output = "'" + c + "'";
                    output = output + " 0x" + i.ToHexString(2);
                }
                else
                {
                    output = "KeyToString TODO:0x" + i.ToHexString(4);
                }
            }
        }
        return output;
    }
    
    string ToClickString()
    {
        uint x = ClickX;
        uint y = ClickY;
        string content = "(" + x.ToString() + ", " + y.ToString() + ")";        
        if (ClickUp)
        {
            content = content + " Up";
        }
        else
        {
            content = content + " Down";
        }
        if (ClickDouble)
        {
            content = content + " Double";
        }
        return content;
    }

    uint ClickX      { get system; } // x position of last read click event
    uint ClickY      { get system; } // y position of last read click event
    bool ClickUp     { get system; } // was last read click event Up or Down?
    bool ClickDouble { get system; } // was last read click event a double click?
    int  ScrollDelta { get system; } // delta for last scroll wheel event
    
    bool IsAvailable { get system; }
    Key  ReadKey() system;
    Key  ToKey(char c) system;
    char ToChar(Key this)
    {
        char c = char(0);
        uint i = uint(this);
        if ((i >= 32) && (i <= 127)) // printable ASCII
        {
            c = char(i);
        }
        return c;
    }
    
}
