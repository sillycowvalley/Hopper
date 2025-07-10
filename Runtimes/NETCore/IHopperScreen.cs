using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HopperRuntime
{
    public interface IHopperScreen
    {
        ushort Columns { get; }
        ushort Rows    { get; }
        ushort CursorX { get; }
        ushort CursorY { get; }

        void Clear();
        void DrawChar(ushort x, ushort y, char c, ushort foreColour, ushort backColour);
        void Print(char c, ushort foreColour, ushort backColour);
        void Print(string s, ushort foreColour, ushort backColour);
        void PrintLn(string s, ushort foreColour, ushort backColour);
        void PrintLn();
        void Resize();
        void Resume(bool interactive);
        void SetCursor(ushort x, ushort y);
        void ShowCursor(bool visible);
        void Suspend();
    }
}
