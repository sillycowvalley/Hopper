
namespace HopperNET
{
    public enum Key // singular
    {
        NoKey = 0x0000,
        NoModifier = NoKey,

        Alt = 0x0100,
        Control = 0x0200,
        Shift = 0x0400,
        Mask = 0xF0FF,

        Click      = 0xE0FF, // pointer click event
        Scroll     = 0xE0FE, // pointer scroll event
        ClickRight = 0xE0FD, // pointer right click event

        Space = 0x0020,
        Delete = 0xE071,
        Backspace = 0x0008,
        Tab = 0xE00D,
        Enter = 0x000D,
        Esc = 0x001B,

        ModSpace = 0xE029,
        ModEscape = 0xE076,
        ModInsert = 0xE070,
        ModBackspace = 0xE066,
        ModEnter = 0xE05A,
        ModEsc = 0xE076,

        F1 = 0xE005,
        F2 = 0xE006,
        F3 = 0xE004,
        F4 = 0xE00C,
        F5 = 0xE003,
        F6 = 0xE00B,
        F7 = 0xE083,
        F8 = 0xE00A,
        F9 = 0xE001,
        F10 = 0xE009,
        F11 = 0xE078,
        F12 = 0xE007,

        Left = 0xE06B,
        Right = 0xE074,

        Up = 0xE075,
        Down = 0xE072,

        Home = 0xE06C,
        End = 0xE069,

        PageDown = 0xE07A,
        PageUp = 0xE07D,

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

        ControlA = 0xE21C,
        ControlC = 0xE221,
        ControlV = 0xE22A,

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
    };
    
    struct ClickArgs
    {
        public ushort clickX;
        public ushort clickY;
        public short scrollDelta;
        public bool clickUp;
        public bool clickDouble;
    }
    
    public class Keyboard
    {
        HopperRuntime.TextGridView textView;
        private bool AnyOfAltControlShift(Key hopperModifiers)
        {
            const Key AnyModifier = Key.Alt | Key.Control | Key.Shift;
            return (hopperModifiers & AnyModifier) > 0;
        }

        private bool AltXorControl(Key hopperModifiers)
        {
            return (hopperModifiers & Key.Alt) == Key.Alt
                    ^(hopperModifiers & Key.Control) == Key.Control;
        }

        private bool AltOrControl(Key hopperModifiers)
        {
            const Key AnyModifierButShift = Key.Alt | Key.Control;
            return (hopperModifiers & AnyModifierButShift) > 0;
        }

        private bool NoneOfAltControlShift(Key hopperModifiers)
        {
            const Key AnyModifier = Key.Alt | Key.Control | Key.Shift;
            return 0 == (hopperModifiers & AnyModifier);
        }

        internal Key TranslateAsciiToHopperKey(char c, Key hopperModifiers)
        {
#if DIAG
            Debug.WriteLine("ASCII '{0}' 0x{1} {2} - {3} {4} {5}",
                c, ((int)c).ToString("X"), (int)c,
                (hopperModifiers & Key.Shift) == Key.Shift ? "SHIFT" : "shift",
                (hopperModifiers & Key.Control) == Key.Control ? "CTRL" : "ctrl",
                (hopperModifiers & Key.Alt) == Key.Alt ? "ALT" : "alt");
#endif
            // 
            if (c < ' ') 
                return Key.NoKey;

            if (c > '~') 
                return Key.NoKey;

            // Alt Gr key sequence contains both Ctrl & Alt 
            //if (BothControlAndAlt(hopperModifiers))
            //    return Key.NoKey;
            if (AltXorControl(hopperModifiers)) 
                return Key.NoKey;

            return (Key)c;
        }
        /*
        internal Key TranslateCtrlToHopperKey(Keys ctrlKey, Key hopperModifiers)
        {
#if DIAG
            Debug.WriteLine("CTRL '{0}' 0x{1} {2} - {3} {4} {5}",
                ctrlKey, ((int)ctrlKey).ToString("X"), (int)ctrlKey,
                (hopperModifiers & Key.Shift) == Key.Shift ? "SHIFT" : "shift",
                (hopperModifiers & Key.Control) == Key.Control ? "CTRL" : "ctrl",
                (hopperModifiers & Key.Alt) == Key.Alt ? "ALT" : "alt");
#endif
            // Space, allow any modifier
            if (ctrlKey == Keys.Space && AnyOfAltControlShift(hopperModifiers))
            {
                return Key.ModSpace | hopperModifiers;
            }

            if (Char.IsLetter((char)ctrlKey) && AltOrControl(hopperModifiers))
            {
                switch (ctrlKey)
                {
                    case Keys.A: return Key.ModA | hopperModifiers;
                    case Keys.B: return Key.ModB | hopperModifiers;
                    case Keys.C: return Key.ModC | hopperModifiers;
                    case Keys.D: return Key.ModD | hopperModifiers;
                    case Keys.E: return Key.ModE | hopperModifiers;
                    case Keys.F: return Key.ModF | hopperModifiers;
                    case Keys.G: return Key.ModG | hopperModifiers;
                    case Keys.H: return Key.ModH | hopperModifiers;
                    case Keys.I: return Key.ModI | hopperModifiers;
                    case Keys.J: return Key.ModJ | hopperModifiers;
                    case Keys.K: return Key.ModK | hopperModifiers;
                    case Keys.L: return Key.ModL | hopperModifiers;
                    case Keys.M: return Key.ModM | hopperModifiers;
                    case Keys.N: return Key.ModN | hopperModifiers;
                    case Keys.O: return Key.ModO | hopperModifiers;
                    case Keys.P: return Key.ModP | hopperModifiers;
                    case Keys.Q: return Key.ModQ | hopperModifiers;
                    case Keys.R: return Key.ModR | hopperModifiers;
                    case Keys.S: return Key.ModS | hopperModifiers;
                    case Keys.T: return Key.ModT | hopperModifiers;
                    case Keys.U: return Key.ModU | hopperModifiers;
                    case Keys.V: return Key.ModV | hopperModifiers;
                    case Keys.W: return Key.ModW | hopperModifiers;
                    case Keys.X: return Key.ModX | hopperModifiers;
                    case Keys.Y: return Key.ModY | hopperModifiers;
                    case Keys.Z: return Key.ModZ | hopperModifiers;
                }
            }

            if (Char.IsDigit((char)ctrlKey) && AltOrControl(hopperModifiers))
            {
                switch (ctrlKey)
                {
                    case Keys.D0: return Key.Mod0 | hopperModifiers;
                    case Keys.D1: return Key.Mod1 | hopperModifiers;
                    case Keys.D2: return Key.Mod2 | hopperModifiers;
                    case Keys.D3: return Key.Mod3 | hopperModifiers;
                    case Keys.D4: return Key.Mod4 | hopperModifiers;
                    case Keys.D5: return Key.Mod5 | hopperModifiers;
                    case Keys.D6: return Key.Mod6 | hopperModifiers;
                    case Keys.D7: return Key.Mod7 | hopperModifiers;
                    case Keys.D8: return Key.Mod8 | hopperModifiers;
                    case Keys.D9: return Key.Mod0 | hopperModifiers;
                }
            }

            switch (ctrlKey)
            {
                case Keys.Shift:
                case Keys.ShiftKey:
                case Keys.LShiftKey:
                case Keys.RShiftKey:
                case Keys.Control:
                case Keys.ControlKey:
                case Keys.RControlKey:
                case Keys.LControlKey:
                case Keys.Menu:
                case Keys.LMenu:
                case Keys.RMenu:
                    break;
                case Keys.Back:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Backspace : Key.ModBackspace | hopperModifiers;
                case Keys.Tab:
                    return (Key.Tab | hopperModifiers);
                case Keys.Return:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Enter : Key.ModEnter | hopperModifiers;
                case Keys.Escape:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Esc : Key.ModEsc | hopperModifiers;
                case Keys.Delete:
                    return Key.Delete | hopperModifiers;
                case Keys.Insert:
                    return Key.ModInsert | hopperModifiers;
                case Keys.End:
                    return Key.End | hopperModifiers;
                case Keys.Home:
                    return Key.Home | hopperModifiers;
                case Keys.Left:
                    return Key.Left | hopperModifiers;
                case Keys.Right:
                    return Key.Right | hopperModifiers;
                case Keys.Up:
                    return Key.Up | hopperModifiers;
                case Keys.Down:
                    return Key.Down | hopperModifiers;
                case Keys.Prior:
                    return Key.PageUp | hopperModifiers;
                case Keys.Next:
                    return Key.PageDown | hopperModifiers;
                case Keys.F1:
                    return Key.F1 | hopperModifiers;
                case Keys.F2:
                    return Key.F2 | hopperModifiers;
                case Keys.F3:
                    return Key.F3 | hopperModifiers;
                case Keys.F4:
                    return Key.F4 | hopperModifiers;
                case Keys.F5:
                    return Key.F5 | hopperModifiers;
                case Keys.F6:
                    return Key.F6 | hopperModifiers;
                case Keys.F7:
                    return Key.F7 | hopperModifiers;
                case Keys.F8:
                    return Key.F8 | hopperModifiers;
                case Keys.F9:
                    return Key.F9 | hopperModifiers;
                case Keys.F10:
                    return Key.F10 | hopperModifiers;
                case Keys.F11:
                    return Key.F11 | hopperModifiers;
                case Keys.F12:
                    return Key.F12 | hopperModifiers;
            }

            return Key.NoKey;
        }
        */

        Key ConsoleKeyToHopperKey(ConsoleKeyInfo keyInfo)
        {
            Key hopperModifiers = Key.NoKey;
            if ((keyInfo.Modifiers & ConsoleModifiers.Control) != ConsoleModifiers.None)
            {
                hopperModifiers |= Key.Control;
            }
            if ((keyInfo.Modifiers & ConsoleModifiers.Shift) != ConsoleModifiers.None)
            {
                hopperModifiers |= Key.Shift;
            }
            if ((keyInfo.Modifiers & ConsoleModifiers.Alt) != ConsoleModifiers.None)
            {
                hopperModifiers |= Key.Alt;
            }
            if ((keyInfo.Modifiers & ConsoleModifiers.Control) != ConsoleModifiers.None)
            {
                hopperModifiers |= Key.Control;
            }

            // Space, allow any modifier
            if ((keyInfo.Key == ConsoleKey.Spacebar) && AnyOfAltControlShift(hopperModifiers))
            {
                return Key.ModSpace | hopperModifiers;
            }

            if (Char.IsLetter((char)keyInfo.Key) && AltOrControl(hopperModifiers))
            {
                switch (keyInfo.Key)
                {
                    case ConsoleKey.A: return Key.ModA | hopperModifiers;
                    case ConsoleKey.B: return Key.ModB | hopperModifiers;
                    case ConsoleKey.C: return Key.ModC | hopperModifiers;
                    case ConsoleKey.D: return Key.ModD | hopperModifiers;
                    case ConsoleKey.E: return Key.ModE | hopperModifiers;
                    case ConsoleKey.F: return Key.ModF | hopperModifiers;
                    case ConsoleKey.G: return Key.ModG | hopperModifiers;
                    case ConsoleKey.H: return Key.ModH | hopperModifiers;
                    case ConsoleKey.I: return Key.ModI | hopperModifiers;
                    case ConsoleKey.J: return Key.ModJ | hopperModifiers;
                    case ConsoleKey.K: return Key.ModK | hopperModifiers;
                    case ConsoleKey.L: return Key.ModL | hopperModifiers;
                    case ConsoleKey.M: return Key.ModM | hopperModifiers;
                    case ConsoleKey.N: return Key.ModN | hopperModifiers;
                    case ConsoleKey.O: return Key.ModO | hopperModifiers;
                    case ConsoleKey.P: return Key.ModP | hopperModifiers;
                    case ConsoleKey.Q: return Key.ModQ | hopperModifiers;
                    case ConsoleKey.R: return Key.ModR | hopperModifiers;
                    case ConsoleKey.S: return Key.ModS | hopperModifiers;
                    case ConsoleKey.T: return Key.ModT | hopperModifiers;
                    case ConsoleKey.U: return Key.ModU | hopperModifiers;
                    case ConsoleKey.V: return Key.ModV | hopperModifiers;
                    case ConsoleKey.W: return Key.ModW | hopperModifiers;
                    case ConsoleKey.X: return Key.ModX | hopperModifiers;
                    case ConsoleKey.Y: return Key.ModY | hopperModifiers;
                    case ConsoleKey.Z: return Key.ModZ | hopperModifiers;
                }
            }

            if (Char.IsDigit((char)keyInfo.Key) && AltOrControl(hopperModifiers))
            {
                switch (keyInfo.Key)
                {
                    case ConsoleKey.D0: return Key.Mod0 | hopperModifiers;
                    case ConsoleKey.D1: return Key.Mod1 | hopperModifiers;
                    case ConsoleKey.D2: return Key.Mod2 | hopperModifiers;
                    case ConsoleKey.D3: return Key.Mod3 | hopperModifiers;
                    case ConsoleKey.D4: return Key.Mod4 | hopperModifiers;
                    case ConsoleKey.D5: return Key.Mod5 | hopperModifiers;
                    case ConsoleKey.D6: return Key.Mod6 | hopperModifiers;
                    case ConsoleKey.D7: return Key.Mod7 | hopperModifiers;
                    case ConsoleKey.D8: return Key.Mod8 | hopperModifiers;
                    case ConsoleKey.D9: return Key.Mod0 | hopperModifiers;
                }
            }

            switch (keyInfo.Key)
            {
                case ConsoleKey.Backspace:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Backspace : Key.ModBackspace | hopperModifiers;
                case ConsoleKey.Tab:
                    return (Key.Tab | hopperModifiers);
                case ConsoleKey.Enter:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Enter : Key.ModEnter | hopperModifiers;
                case ConsoleKey.Escape:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Esc : Key.ModEsc | hopperModifiers;
                case ConsoleKey.Delete:
                    return Key.Delete | hopperModifiers;
                case ConsoleKey.Insert:
                    return Key.ModInsert | hopperModifiers;
                case ConsoleKey.End:
                    return Key.End | hopperModifiers;
                case ConsoleKey.Home:
                    return Key.Home | hopperModifiers;
                case ConsoleKey.LeftArrow:
                    return Key.Left | hopperModifiers;
                case ConsoleKey.RightArrow:
                    return Key.Right | hopperModifiers;
                case ConsoleKey.UpArrow:
                    return Key.Up | hopperModifiers;
                case ConsoleKey.DownArrow:
                    return Key.Down | hopperModifiers;
                case ConsoleKey.PageUp:
                    return Key.PageUp | hopperModifiers;
                case ConsoleKey.PageDown:
                    return Key.PageDown | hopperModifiers;
                case ConsoleKey.F1:
                    return Key.F1 | hopperModifiers;
                case ConsoleKey.F2:
                    return Key.F2 | hopperModifiers;
                case ConsoleKey.F3:
                    return Key.F3 | hopperModifiers;
                case ConsoleKey.F4:
                    return Key.F4 | hopperModifiers;
                case ConsoleKey.F5:
                    return Key.F5 | hopperModifiers;
                case ConsoleKey.F6:
                    return Key.F6 | hopperModifiers;
                case ConsoleKey.F7:
                    return Key.F7 | hopperModifiers;
                case ConsoleKey.F8:
                    return Key.F8 | hopperModifiers;
                case ConsoleKey.F9:
                    return Key.F9 | hopperModifiers;
                case ConsoleKey.F10:
                    return Key.F10 | hopperModifiers;
                case ConsoleKey.F11:
                    return Key.F11 | hopperModifiers;
                case ConsoleKey.F12:
                    return Key.F12 | hopperModifiers;
            }

            if (keyInfo.KeyChar != (char)0)
            {
                return TranslateAsciiToHopperKey(keyInfo.KeyChar, hopperModifiers);
            }

            return Key.NoKey;
        }

        List<Key> keyboardBuffer;
        List<ClickArgs> clickBuffer;

        ClickArgs current;
        public Keyboard(HopperRuntime.TextGridView textView)
        {
            this.textView = textView;
            keyboardBuffer = new List<Key>();
            clickBuffer = new List<ClickArgs>();
        }

        public ushort ClickX
        {
            get { return current.clickX; }
        }
        public ushort ClickY
        {
            get { return current.clickY; }
        }
        public short ScrollDelta
        {
            get { return current.scrollDelta; }
        }
        public bool ClickUp
        {
            get { return current.clickUp; }
        }
        public bool ClickDouble
        {
            get 
            {
                return current.clickDouble; 
            }
        }

        internal bool PushToKeyboardBuffer(Key key)
        {
            if (key != Key.NoKey)
            {
                lock (keyboardBuffer)
                {
                    keyboardBuffer.Add(key);
                }
                return true;
            }

            return false;
        }
        /*
        internal void PushClick(MouseEventArgs e, bool down)
        {
            ClickArgs clickArgs = new ClickArgs();
            clickArgs.clickX = Console.TransformXtoC((uint)e.X);
            clickArgs.clickY = Console.TransformYtoR((uint)e.Y);
            clickArgs.clickUp = !down;
            clickArgs.clickDouble = false;
            clickArgs.scrollDelta = 0;
            if (e.Clicks > 1)
            {
                clickArgs.clickDouble = true;
                clickArgs.clickUp = true;
            }
            

            Key key = Key.Click;
            Keys currentModifierState = Hopper.ModifierKeys;
            if (e.Button == MouseButtons.Right)
            {
                key = Key.ClickRight;
            }
            if ((currentModifierState & Keys.Shift) != 0)
            {
                key |= Key.Shift;
            }
            if ((currentModifierState & Keys.Control) != 0)
            {
                key |= Key.Control;
            }
            if ((currentModifierState & Keys.Alt) != 0)
            {
                key |= Key.Alt;
            }
            lock (keyboardBuffer)
            {
                keyboardBuffer.Add(key);
                clickBuffer.Add(clickArgs);
            }
        }
        internal void PushScroll(MouseEventArgs e)
        {
            ClickArgs clickArgs = new ClickArgs();
            clickArgs.clickX = Console.TransformXtoC((uint)e.X);
            clickArgs.clickY = Console.TransformYtoR((uint)e.Y);
            clickArgs.scrollDelta = (short)(e.Delta / 120); // WHEEL_DELTA
            clickArgs.clickUp = false;
            clickArgs.clickDouble = false;

            Key key = Key.Scroll;
            Keys currentModifierState = Hopper.ModifierKeys;
            if ((currentModifierState & Keys.Shift) != 0)
            {
                key = (key | Key.Shift);
            }
            if ((currentModifierState & Keys.Control) != 0)
            {
                key = (key | Key.Control);
            }
            if ((currentModifierState & Keys.Alt) != 0)
            {
                key = (key | Key.Alt);
            }
            lock (keyboardBuffer)
            {
                keyboardBuffer.Add(key);
                clickBuffer.Add(clickArgs);
            }
        }
        */
        
        private void PumpKeys()
        {
            lock (keyboardBuffer)
            {
                if (Console.KeyAvailable)
                {
                    ConsoleKeyInfo keyInfo = Console.ReadKey(true);

                    Key key = ConsoleKeyToHopperKey(keyInfo);
                    PushToKeyboardBuffer(key);

                }
            }
        }

        internal void Free()
        {
            lock (keyboardBuffer)
            {
                keyboardBuffer.Clear();
                clickBuffer.Clear();
            }
        }

        

        public bool IsAvailable()
        {
            bool result = false;
            lock (keyboardBuffer)
            {
                if (keyboardBuffer.Count == 0)
                {
                    PumpKeys();
                }
                result =  keyboardBuffer.Count != 0;
            }
            return result;
        }

        static public Key ToKey(char c)
        {
            // really just as cast from ASCII to HopperKey
            return (Key)c;
        }

        public Key ReadKey()
        {
            Key c = Key.NoKey;

            /*
            bool localShow = false;
            if (!console.CursorVisible)
            {
                console.ShowCursor(true);
                localShow = true;
            }
            */

            

            for (; ; )
            {
                PumpKeys();

                lock (keyboardBuffer)
                {
                    // was there a key added to the buffer that needs to be processed
                    if ((keyboardBuffer != null) && (keyboardBuffer.Count > 0))
                    {
                        c = keyboardBuffer[0];
                        keyboardBuffer.RemoveAt(0);

                        //TODO DIAG Debug.WriteLine(c);

                        if ((c & Key.Mask) == Key.Click)
                        {
                            current = clickBuffer[0];
                            clickBuffer.RemoveAt(0);
                        }
                        else if ((c & Key.Mask) == Key.ClickRight)
                        {
                            current = clickBuffer[0];
                            clickBuffer.RemoveAt(0);
                        }
                        else if ((c & Key.Mask) == Key.Scroll)
                        {
                            current = clickBuffer[0];
                            clickBuffer.RemoveAt(0);
                        }
                    }
                }

                if (0 != c)
                {
                    break;
                }
            }
            /*
            if (localShow)
            {
                console.ShowCursor(false);
            }
            */
            return c;
        }
    }
}
