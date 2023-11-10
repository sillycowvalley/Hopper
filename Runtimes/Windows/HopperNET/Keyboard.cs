using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

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
        Key translateToHopperKey(Keys ps2ScanCode, Key hopperModifiers)
        {
            Key c = Key.NoKey;
            switch (ps2ScanCode)
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
                    // not considered stand-alone keys
                    break;
                case Keys.Space:
                    if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)' ';
                    }
                    else
                    {
                        c = (Key)(Key.ModSpace | hopperModifiers);
                    }
                    break;
                case Keys.OemPeriod:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'>';
                    }
                    else if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'.';
                    }
                    else
                    {
                        c = (Key)(Key.ModPeriod | hopperModifiers);
                    }
                    break;
                case Keys.Oemplus:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'+';
                    }
                    else
                    {
                        c = (Key)'=';
                    }
                    break;
                case Keys.OemMinus:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'_';
                    }
                    else
                    {
                        c = (Key)'-';
                    }
                    break;
                case Keys.Oemcomma:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'<';
                    }
                    else
                    {
                        c = (Key)',';
                    }
                    break;
                case Keys.Oem2:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'?';
                    }
                    else
                    {
                        c = (Key)'/';
                    }
                    break;
                case Keys.Oem1:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)':';
                    }
                    else
                    {
                        c = (Key)';';
                    }
                    break;
                case Keys.Oem3:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'~';
                    }
                    else
                    {
                        c = (Key)'`';
                    }
                    break;
                case Keys.Oem4:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'{';
                    }
                    else
                    {
                        c = (Key)'[';
                    }
                    break;
                case Keys.Oem5:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'|';
                    }
                    else
                    {
                        c = (Key)'\\';
                    }
                    break;
                case Keys.Oem6:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'}';
                    }
                    else
                    {
                        c = (Key)']';
                    }
                    break;
                case Keys.Oem7:
                    if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = (Key)'"';
                    }
                    else
                    {
                        c = (Key)'\'';
                    }
                    break;
                case Keys.Back:
                    if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = Key.Backspace;
                    }
                    else
                    {
                        c = (Key)(Key.ModBackspace | hopperModifiers);
                    }
                    break;
                case Keys.Tab:
                    c = (Key)(Key.Tab | hopperModifiers);
                    break;
                case Keys.Return:
                    if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = Key.Enter;
                    }
                    else
                    {
                        c = (Key)(Key.ModEnter | hopperModifiers);
                    }
                    break;
                case Keys.Escape:
                    if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                    {
                        c = Key.Esc;
                    }
                    else
                    {
                        c = (Key)(Key.ModEsc | hopperModifiers);
                    }
                    break;
                case Keys.Delete:
                    c = (Key)(Key.Delete | hopperModifiers);
                    break;
                case Keys.Insert:
                    c = (Key)(Key.ModInsert | hopperModifiers);
                    break;

                case Keys.End:
                    c = (Key)(Key.End | hopperModifiers);
                    break;
                case Keys.Home:
                    c = (Key)(Key.Home | hopperModifiers);
                    break;
                case Keys.Left:
                    c = (Key)(Key.Left | hopperModifiers);
                    break;
                case Keys.Right:
                    c = (Key)(Key.Right | hopperModifiers);
                    break;
                case Keys.Up:
                    c = (Key)(Key.Up | hopperModifiers);
                    break;
                case Keys.Down:
                    c = (Key)(Key.Down | hopperModifiers);
                    break;
                case Keys.Prior:
                    c = (Key)(Key.PageUp | hopperModifiers);
                    break;
                case Keys.Next:
                    c = (Key)(Key.PageDown | hopperModifiers);
                    break;

                case Keys.F1:
                    c = (Key)(Key.F1 | hopperModifiers);
                    break;
                case Keys.F2:
                    c = (Key)(Key.F2 | hopperModifiers);
                    break;
                case Keys.F3:
                    c = (Key)(Key.F3 | hopperModifiers);
                    break;
                case Keys.F4:
                    c = (Key)(Key.F4 | hopperModifiers);
                    break;
                case Keys.F5:
                    c = (Key)(Key.F5 | hopperModifiers);
                    break;
                case Keys.F6:
                    c = (Key)(Key.F6 | hopperModifiers);
                    break;
                case Keys.F7:
                    c = (Key)(Key.F7 | hopperModifiers);
                    break;
                case Keys.F8:
                    c = (Key)(Key.F8 | hopperModifiers);
                    break;
                case Keys.F9:
                    c = (Key)(Key.F9 | hopperModifiers);
                    break;
                case Keys.F10:
                    c = (Key)(Key.F10 | hopperModifiers);
                    break;
                case Keys.F11:
                    c = (Key)(Key.F11 | hopperModifiers);
                    break;
                case Keys.F12:
                    c = (Key)(Key.F12 | hopperModifiers);
                    break;
                default:
                    if (((uint)ps2ScanCode >= 0x30) && ((uint)ps2ScanCode <= 0x39))
                    {
                        // digits
                        c = (Key)ps2ScanCode;
                        if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                        {
                            // leave as digits
                        }
                        else if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                        {
                            // shifted digits
                            switch ((char)c)
                            {
                                case '0':
                                    c = (Key)')';
                                    break;
                                case '1':
                                    c = (Key)'!';
                                    break;
                                case '2':
                                    c = (Key)'@';
                                    break;
                                case '3':
                                    c = (Key)'#';
                                    break;
                                case '4':
                                    c = (Key)'$';
                                    break;
                                case '5':
                                    c = (Key)'%';
                                    break;
                                case '6':
                                    c = (Key)'^';
                                    break;
                                case '7':
                                    c = (Key)'&';
                                    break;
                                case '8':
                                    c = (Key)'*';
                                    break;
                                case '9':
                                    c = (Key)'(';
                                    break;
                                default:
#if DEBUG
                                    Diagnostics.ASSERT(false, "not implemented");
#endif
                                    break;
                            }
                        }
                        else
                        {
                            // modified digits
                            switch ((char)c)
                            {
                                case '0':
                                    c = Key.Mod0;
                                    break;
                                case '1':
                                    c = Key.Mod1;
                                    break;
                                case '2':
                                    c = Key.Mod2;
                                    break;
                                case '3':
                                    c = Key.Mod3;
                                    break;
                                case '4':
                                    c = Key.Mod4;
                                    break;
                                case '5':
                                    c = Key.Mod5;
                                    break;
                                case '6':
                                    c = Key.Mod6;
                                    break;
                                case '7':
                                    c = Key.Mod7;
                                    break;
                                case '8':
                                    c = Key.Mod8;
                                    break;
                                case '9':
                                    c = Key.Mod9;
                                    break;
                                default:
#if DEBUG
                                    Diagnostics.ASSERT(false, "not implemented");
#endif
                                    break;
                            }
                            c = (Key)(c | hopperModifiers);
                        }
                    }
                    else if (((uint)ps2ScanCode >= 0x41) && ((uint)ps2ScanCode <= 0x5A))
                    {
                        // A.. Z
                        c = (Key)ps2ScanCode;
                        if (Key.Shift == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                        {
                            // leave as uppercase
                        }
                        else if (0 == (hopperModifiers & (Key.Alt | Key.Control | Key.Shift)))
                        {
                            // lowercase letters
                            c = (Key)(c + ('a' - 'A'));
                        }
                        else
                        {
                            // modified letters
                            switch ((char)c)
                            {
                                case 'A':
                                    c = Key.ModA;
                                    break;
                                case 'B':
                                    c = Key.ModB;
                                    break;
                                case 'C':
                                    c = Key.ModC;
                                    break;
                                case 'D':
                                    c = Key.ModD;
                                    break;
                                case 'E':
                                    c = Key.ModE;
                                    break;
                                case 'F':
                                    c = Key.ModF;
                                    break;
                                case 'G':
                                    c = Key.ModG;
                                    break;
                                case 'H':
                                    c = Key.ModH;
                                    break;
                                case 'I':
                                    c = Key.ModI;
                                    break;
                                case 'J':
                                    c = Key.ModJ;
                                    break;
                                case 'K':
                                    c = Key.ModK;
                                    break;
                                case 'L':
                                    c = Key.ModL;
                                    break;
                                case 'M':
                                    c = Key.ModM;
                                    break;
                                case 'N':
                                    c = Key.ModN;
                                    break;
                                case 'O':
                                    c = Key.ModO;
                                    break;
                                case 'P':
                                    c = Key.ModP;
                                    break;
                                case 'Q':
                                    c = Key.ModQ;
                                    break;
                                case 'R':
                                    c = Key.ModR;
                                    break;
                                case 'S':
                                    c = Key.ModS;
                                    break;
                                case 'T':
                                    c = Key.ModT;
                                    break;
                                case 'U':
                                    c = Key.ModU;
                                    break;
                                case 'V':
                                    c = Key.ModV;
                                    break;
                                case 'W':
                                    c = Key.ModW;
                                    break;
                                case 'X':
                                    c = Key.ModX;
                                    break;
                                case 'Y':
                                    c = Key.ModY;
                                    break;
                                case 'Z':
                                    c = Key.ModZ;
                                    break;
                                default:
#if DEBUG
                                    Diagnostics.ASSERT(false, "not implemented");
#endif
                                    break;
                            }
                            c = (Key)(c | hopperModifiers);
                        }
                    }
                    else
                    {
                        HopperSystem.Beep(); // deal with this key
                    }
                    break;
            }

            return c;
        }

        List<Key> keyboardBuffer;
        List<ClickArgs> clickBuffer;

        ClickArgs current;
        Console console;
        public Keyboard(Console console)
        {
            this.console = console;
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
        internal void PushKey(KeyEventArgs e)
        {
            Key key = Key.NoKey;
            Key modifiers = Key.NoKey;
            bool pushTheKey = false;
            if ((Hopper.ModifierKeys & Keys.Shift) != 0)
            {
                modifiers = (modifiers | Key.Shift);
            }
            if ((Hopper.ModifierKeys & Keys.Control) != 0)
            {
                modifiers = (modifiers | Key.Control);
            }
            if ((Hopper.ModifierKeys & Keys.Alt) != 0)
            {
                modifiers = (modifiers | Key.Alt);
            }

            switch (e.KeyCode)
            {
                case Keys.Menu:
                case Keys.LMenu:
                case Keys.RMenu:
                    // ignore alt key on its own
                    break;
                default:
                    key = translateToHopperKey(e.KeyCode, modifiers);
                    pushTheKey = true;
                    break;
            }
            if (pushTheKey)
            {
                // push key into keyboard buffer
                lock (keyboardBuffer)
                {
                    keyboardBuffer.Add(key);
                }
            }
        }
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
            if (e.Button == MouseButtons.Right)
            {
                key = Key.ClickRight;
            }
            if ((Hopper.ModifierKeys & Keys.Shift) != 0)
            {
                key = (key | Key.Shift);
            }
            if ((Hopper.ModifierKeys & Keys.Control) != 0)
            {
                key = (key | Key.Control);
            }
            if ((Hopper.ModifierKeys & Keys.Alt) != 0)
            {
                key = (key | Key.Alt);
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
            if ((Hopper.ModifierKeys & Keys.Shift) != 0)
            {
                key = (key | Key.Shift);
            }
            if ((Hopper.ModifierKeys & Keys.Control) != 0)
            {
                key = (key | Key.Control);
            }
            if ((Hopper.ModifierKeys & Keys.Alt) != 0)
            {
                key = (key | Key.Alt);
            }
            lock (keyboardBuffer)
            {
                keyboardBuffer.Add(key);
                clickBuffer.Add(clickArgs);
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
            lock (keyboardBuffer)
            {
                if (keyboardBuffer.Count == 0)
                {
                    //Application.DoEvents();
                }
                return keyboardBuffer.Count != 0;
            }
        }

        static public Key ToKey(char c)
        {
            // really just as cast from ASCII to HopperKey
            return (Key)c;
        }

        public Key ReadKey()
        {
            Key c = Key.NoKey;
            
            console.ShowCursor(true);

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
            console.ShowCursor(false);
            return c;
        }
    }
}
