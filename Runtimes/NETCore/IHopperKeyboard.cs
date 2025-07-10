using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Terminal.Gui.Input;

namespace HopperRuntime
{
    public interface IHopperKeyboard
    {
        ushort ClickX { get; }
        ushort ClickY { get; }
        bool ClickUp { get; }
        bool ClickDouble { get; }
        short ScrollDelta { get; }

        bool IsAvailable();
        void PushClick(MouseEventArgs mouseArgs);
        void PushToKeyboardBuffer(Terminal.Gui.Input.Key key);
        Key ReadKey();
    }
}
