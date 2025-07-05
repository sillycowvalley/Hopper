using System.Diagnostics;

using Terminal.Gui.Drivers;
using Terminal.Gui.Input;

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
            // Support for Gr modifier
            if (c < ' ')
            {
                return Key.NoKey;
            }

            if (c > '~')
            {
                return Key.NoKey;
            }

            // Alt Gr key sequence contains both Ctrl & Alt 
            //if (BothControlAndAlt(hopperModifiers))
            //    return Key.NoKey;
            if (AltXorControl(hopperModifiers))
            {
                return Key.NoKey;
            }

            return (Key)c;
        }
        
        Key TerminalKeyToHopperKey(Terminal.Gui.Input.Key keyInfo)
        {
            System.Text.Rune rune = keyInfo.AsRune;
            KeyCode keyCode = keyInfo.KeyCode;

            
            Key hopperModifiers = Key.NoKey;
            if (keyInfo.IsCtrl)
            {
                hopperModifiers |= Key.Control;
                keyCode = (keyInfo.KeyCode & ~KeyCode.CtrlMask);
            }
            if (keyInfo.IsShift)
            {
                hopperModifiers |= Key.Shift;
                keyCode = (keyInfo.KeyCode & ~KeyCode.ShiftMask);
            }
            if (keyInfo.IsAlt)
            {
                hopperModifiers |= Key.Alt;
                keyCode = (keyInfo.KeyCode & ~KeyCode.AltMask);
            }
            

            if (keyCode == KeyCode.Null)
            {
                return Key.NoKey; // <ctrl>, <shift> or <alt> alone
            }

            // Space, allow any modifier
            if ((keyCode == KeyCode.Space) && AnyOfAltControlShift(hopperModifiers))
            {
                return Key.ModSpace | hopperModifiers;
            }

            if (Char.IsLetter((char)keyCode) && AltOrControl(hopperModifiers))
            {
                switch (keyCode)
                {
                    case KeyCode.A: return Key.ModA | hopperModifiers;
                    case KeyCode.B: return Key.ModB | hopperModifiers;
                    case KeyCode.C: return Key.ModC | hopperModifiers;
                    case KeyCode.D: return Key.ModD | hopperModifiers;
                    case KeyCode.E: return Key.ModE | hopperModifiers;
                    case KeyCode.F: return Key.ModF | hopperModifiers;
                    case KeyCode.G: return Key.ModG | hopperModifiers;
                    case KeyCode.H: return Key.ModH | hopperModifiers;
                    case KeyCode.I: return Key.ModI | hopperModifiers;
                    case KeyCode.J: return Key.ModJ | hopperModifiers;
                    case KeyCode.K: return Key.ModK | hopperModifiers;
                    case KeyCode.L: return Key.ModL | hopperModifiers;
                    case KeyCode.M: return Key.ModM | hopperModifiers;
                    case KeyCode.N: return Key.ModN | hopperModifiers;
                    case KeyCode.O: return Key.ModO | hopperModifiers;
                    case KeyCode.P: return Key.ModP | hopperModifiers;
                    case KeyCode.Q: return Key.ModQ | hopperModifiers;
                    case KeyCode.R: return Key.ModR | hopperModifiers;
                    case KeyCode.S: return Key.ModS | hopperModifiers;
                    case KeyCode.T: return Key.ModT | hopperModifiers;
                    case KeyCode.U: return Key.ModU | hopperModifiers;
                    case KeyCode.V: return Key.ModV | hopperModifiers;
                    case KeyCode.W: return Key.ModW | hopperModifiers;
                    case KeyCode.X: return Key.ModX | hopperModifiers;
                    case KeyCode.Y: return Key.ModY | hopperModifiers;
                    case KeyCode.Z: return Key.ModZ | hopperModifiers;
                }
            }

            if (Char.IsDigit((char)keyCode) && AltOrControl(hopperModifiers))
            {
                switch (keyCode)
                {
                    case KeyCode.D0: return Key.Mod0 | hopperModifiers;
                    case KeyCode.D1: return Key.Mod1 | hopperModifiers;
                    case KeyCode.D2: return Key.Mod2 | hopperModifiers;
                    case KeyCode.D3: return Key.Mod3 | hopperModifiers;
                    case KeyCode.D4: return Key.Mod4 | hopperModifiers;
                    case KeyCode.D5: return Key.Mod5 | hopperModifiers;
                    case KeyCode.D6: return Key.Mod6 | hopperModifiers;
                    case KeyCode.D7: return Key.Mod7 | hopperModifiers;
                    case KeyCode.D8: return Key.Mod8 | hopperModifiers;
                    case KeyCode.D9: return Key.Mod0 | hopperModifiers;
                }
            }

            switch (keyCode)
            {
                case KeyCode.Backspace:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Backspace : Key.ModBackspace | hopperModifiers;
                case KeyCode.Tab:
                    return (Key.Tab | hopperModifiers);
                case KeyCode.Enter:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Enter : Key.ModEnter | hopperModifiers;
                case KeyCode.Esc:
                    return NoneOfAltControlShift(hopperModifiers) ? Key.Esc : Key.ModEsc | hopperModifiers;
                case KeyCode.Delete:
                    return Key.Delete | hopperModifiers;
                case KeyCode.Insert:
                    return Key.ModInsert | hopperModifiers;
                case KeyCode.End:
                    return Key.End | hopperModifiers;
                case KeyCode.Home:
                    return Key.Home | hopperModifiers;
                case KeyCode.CursorLeft:
                    return Key.Left | hopperModifiers;
                case KeyCode.CursorRight:
                    return Key.Right | hopperModifiers;
                case KeyCode.CursorUp:
                    return Key.Up | hopperModifiers;
                case KeyCode.CursorDown:
                    return Key.Down | hopperModifiers;
                case KeyCode.PageUp:
                    return Key.PageUp | hopperModifiers;
                case KeyCode.PageDown:
                    return Key.PageDown | hopperModifiers;
                case KeyCode.F1:
                    return Key.F1 | hopperModifiers;
                case KeyCode.F2:
                    return Key.F2 | hopperModifiers;
                case KeyCode.F3:
                    return Key.F3 | hopperModifiers;
                case KeyCode.F4:
                    return Key.F4 | hopperModifiers;
                case KeyCode.F5:
                    return Key.F5 | hopperModifiers;
                case KeyCode.F6:
                    return Key.F6 | hopperModifiers;
                case KeyCode.F7:
                    return Key.F7 | hopperModifiers;
                case KeyCode.F8:
                    return Key.F8 | hopperModifiers;
                case KeyCode.F9:
                    return Key.F9 | hopperModifiers;
                case KeyCode.F10:
                    return Key.F10 | hopperModifiers;
                case KeyCode.F11:
                    return Key.F11 | hopperModifiers;
                case KeyCode.F12:
                    return Key.F12 | hopperModifiers;
            }

            if (keyCode != KeyCode.Null)
            {
                if (rune.IsAscii)
                {
                    return TranslateAsciiToHopperKey((char)rune.Value, hopperModifiers);
                }
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

        internal void PushToKeyboardBuffer(Terminal.Gui.Input.Key keyInfo)
        {
            if (keyInfo.KeyCode != KeyCode.Null)
            {
                lock (keyboardBuffer)
                {
                    Key key = TerminalKeyToHopperKey(keyInfo);
                    if (key != Key.NoKey)
                    {
                        //Trace.Write("\nKey: " + keyInfo.ToString());
                        keyboardBuffer.Add(key);
                    }
                }
            }
            keyInfo.Handled = true;
        }
        
        internal void PushClick(MouseEventArgs mouseArgs)
        {
            for (; ; )
            {
                if (MouseFlags.ReportMousePosition == (mouseArgs.Flags & MouseFlags.ReportMousePosition))
                {
                    break;
                }
                //Trace.Write("\nClick: " + mouseArgs.ToString());
                ClickArgs clickArgs = new ClickArgs();
                clickArgs.clickX = (ushort)mouseArgs.Position.X;
                clickArgs.clickY = (ushort)mouseArgs.Position.Y;
                clickArgs.clickUp = false;
                clickArgs.clickDouble = false;
                clickArgs.scrollDelta = 0;

                Key key = Key.NoKey;
                if (mouseArgs.Flags.HasFlag(MouseFlags.Button1Pressed) | mouseArgs.Flags.HasFlag(MouseFlags.Button1Clicked))
                {
                    clickArgs.clickUp = mouseArgs.Flags.HasFlag(MouseFlags.Button1Clicked);
                    key = Key.Click;
                }
                if (mouseArgs.Flags.HasFlag(MouseFlags.Button1DoubleClicked))
                {
                    clickArgs.clickUp = true;
                    clickArgs.clickDouble = true;
                    key = Key.Click;
                }
                if (mouseArgs.Flags.HasFlag(MouseFlags.Button3Pressed) | mouseArgs.Flags.HasFlag(MouseFlags.Button3Clicked))
                {
                    clickArgs.clickUp = mouseArgs.Flags.HasFlag(MouseFlags.Button3Clicked);
                    key = Key.ClickRight;
                }
                if (mouseArgs.Flags.HasFlag(MouseFlags.Button3DoubleClicked))
                {
                    clickArgs.clickUp = true;
                    clickArgs.clickDouble = true;
                    key = Key.ClickRight;
                }
                if (mouseArgs.Flags.HasFlag(MouseFlags.WheeledDown))
                {
                    clickArgs.scrollDelta = -1;
                    clickArgs.clickUp = false;
                    clickArgs.clickDouble = false;
                    key = Key.Scroll;
                }
                if (mouseArgs.Flags.HasFlag(MouseFlags.WheeledUp))
                {
                    clickArgs.scrollDelta = +1;
                    clickArgs.clickUp = false;
                    clickArgs.clickDouble = false;
                    key = Key.Scroll;
                }

                if (key != Key.NoKey)
                {
                    /*
                    if ((CurrentModifiers & Key.Shift) != 0)
                    {
                        key |= Key.Shift;
                    }
                    if ((CurrentModifiers & Key.Control) != 0)
                    {
                        key |= Key.Control;
                    }
                    if ((CurrentModifiers & Key.Alt) != 0)
                    {
                        key |= Key.Alt;
                    }
                    */
                    
                    lock (keyboardBuffer)
                    {
                        //Trace.Write(" -> " + key.ToString());
                        keyboardBuffer.Add(key);
                        clickBuffer.Add(clickArgs);
                    }
                }
                break;
            }
            
            mouseArgs.Handled = true;
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
                lock (keyboardBuffer)
                {
                    // was there a key added to the buffer that needs to be processed
                    if ((keyboardBuffer != null) && (keyboardBuffer.Count > 0))
                    {
                        c = keyboardBuffer[0];
                        keyboardBuffer.RemoveAt(0);

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
