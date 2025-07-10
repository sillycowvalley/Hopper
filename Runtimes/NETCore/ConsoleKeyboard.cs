using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Terminal.Gui.Input;

namespace HopperRuntime
{
    public class ConsoleKeyboard : IHopperKeyboard
    {
        List<Key> keyboardBuffer;

        public ConsoleKeyboard()
        {
            Console.CancelKeyPress += Console_CancelKeyPress;
            keyboardBuffer = new List<Key>();
        }
        private void Console_CancelKeyPress(object sender, ConsoleCancelEventArgs e)
        {
            // Cancel the termination (prevent immediate exit)
            e.Cancel = true;
            pushToKeyboardBuffer(Key.ControlC);
        }

        private bool anyOfAltControlShift(Key hopperModifiers)
        {
            const Key AnyModifier = Key.Alt | Key.Control | Key.Shift;
            return (hopperModifiers & AnyModifier) > 0;
        }

        private bool altXorControl(Key hopperModifiers)
        {
            return (hopperModifiers & Key.Alt) == Key.Alt
                    ^ (hopperModifiers & Key.Control) == Key.Control;
        }

        private bool altOrControl(Key hopperModifiers)
        {
            const Key AnyModifierButShift = Key.Alt | Key.Control;
            return (hopperModifiers & AnyModifierButShift) > 0;
        }

        private bool noneOfAltControlShift(Key hopperModifiers)
        {
            const Key AnyModifier = Key.Alt | Key.Control | Key.Shift;
            return 0 == (hopperModifiers & AnyModifier);
        }
        private Key translateAsciiToHopperKey(char c, Key hopperModifiers)
        {
            // 
            if (c < ' ')
                return Key.NoKey;

            if (c > '~')
                return Key.NoKey;

            // Alt Gr key sequence contains both Ctrl & Alt 
            //if (BothControlAndAlt(hopperModifiers))
            //    return Key.NoKey;
            if (altXorControl(hopperModifiers))
                return Key.NoKey;

            return (Key)c;
        }

        private Key consoleKeyToHopperKey(ConsoleKeyInfo keyInfo)
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
            if ((keyInfo.Key == ConsoleKey.Spacebar) && anyOfAltControlShift(hopperModifiers))
            {
                return Key.ModSpace | hopperModifiers;
            }

            if (Char.IsLetter((char)keyInfo.Key) && altOrControl(hopperModifiers))
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

            if (Char.IsDigit((char)keyInfo.Key) && altOrControl(hopperModifiers))
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
                    return noneOfAltControlShift(hopperModifiers) ? Key.Backspace : Key.ModBackspace | hopperModifiers;
                case ConsoleKey.Tab:
                    return (Key.Tab | hopperModifiers);
                case ConsoleKey.Enter:
                    return noneOfAltControlShift(hopperModifiers) ? Key.Enter : Key.ModEnter | hopperModifiers;
                case ConsoleKey.Escape:
                    return noneOfAltControlShift(hopperModifiers) ? Key.Esc : Key.ModEsc | hopperModifiers;
                case ConsoleKey.Delete:
                    return Key.Delete | hopperModifiers;
                case ConsoleKey.Insert:
                    return Key.ModInsert | hopperModifiers;
                case ConsoleKey.End:
                    return Key.End | hopperModifiers;
                case ConsoleKey.Home:
                    return Key.Home | hopperModifiers;
                /*
            case ConsoleKey.Left:
                return Key.Left | hopperModifiers;
            case ConsoleKey.Right:
                return Key.Right | hopperModifiers;
            case ConsoleKey.Up:
                return Key.Up | hopperModifiers;
            case ConsoleKey.Down:
                return Key.Down | hopperModifiers;
            case ConsoleKey.Prior:
                return Key.PageUp | hopperModifiers;
            case ConsoleKey.Next:
                return Key.PageDown | hopperModifiers;
                */
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
                return translateAsciiToHopperKey(keyInfo.KeyChar, hopperModifiers);
            }

            return Key.NoKey;
        }

        public ushort ClickX => throw new NotImplementedException();

        public ushort ClickY => throw new NotImplementedException();

        public bool ClickUp => throw new NotImplementedException();

        public bool ClickDouble => throw new NotImplementedException();

        public short ScrollDelta => throw new NotImplementedException();

        private void pumpKeys()
        {
            lock (keyboardBuffer)
            {
                if (Console.KeyAvailable)
                {
                    ConsoleKeyInfo keyInfo = Console.ReadKey(true);

                    Key key = consoleKeyToHopperKey(keyInfo);
                    pushToKeyboardBuffer(key);

                }
            }
        }

        public bool IsAvailable()
        {
            bool result = false;
            lock (keyboardBuffer)
            {
                if (keyboardBuffer.Count == 0)
                {
                    pumpKeys();
                }
                result = keyboardBuffer.Count != 0;
            }
            return result;
        }

        public void PushClick(MouseEventArgs mouseArgs)
        {
            throw new NotImplementedException();
        }

        public void PushToKeyboardBuffer(Terminal.Gui.Input.Key key)
        {
            throw new NotImplementedException();
        }
        private bool pushToKeyboardBuffer(Key key)
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

        public Key ReadKey()
        {
            Key c = Key.NoKey;

            for (; ; )
            {
                pumpKeys();

                lock (keyboardBuffer)
                {
                    // was there a key added to the buffer that needs to be processed
                    if ((keyboardBuffer != null) && (keyboardBuffer.Count > 0))
                    {
                        c = keyboardBuffer[0];
                        keyboardBuffer.RemoveAt(0);

                        /*
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
                        */
                    }
                }

                if (0 != c)
                {
                    break;
                }
            }
            return c;
        }
    }
}
