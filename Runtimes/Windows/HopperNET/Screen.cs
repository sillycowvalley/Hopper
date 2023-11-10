using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HopperNET
{
    public class Screen
    {
        private Console console;

        public Screen(Console console)
        {
            this.console = console;
        }

        public Console Console { get { return console; } }

        public void Suspend()
        {
            console.EnterBuffering(false);
        }
        public void Resume(bool isInteractive)
        {
            console.ExitBuffering(isInteractive);
        }


        public void SetCursor(ushort x, ushort y)
        {
            console.SetCursor(x, y);
        }
        public void DrawChar(ushort x, ushort y, char c, ushort foreColour, ushort backColour)
        {
            console.WriteCharacter(x, y, c, foreColour, backColour);
        }

        public byte Columns
        {
            get { return (byte)(Console.Columns); }
        }
        public byte Rows
        {
            get { return (byte)(Console.Rows); }
        }
        public byte CursorX
        {
            get { return (byte)(console.CursorX); }
        }
        public byte CursorY
        {
            get { return (byte)(console.CursorY); }
        }

        public void Clear()
        {
            console.EnterBuffering(true);
            for (uint y = 0; y < Console.Rows; y++)
            {
                for (uint x = 0; x < Console.Columns; x++)
                {
                    console.WriteCharacter(x, y, ' ', 0x000, 0x000);
                }
            }
            console.SetCursor(0, 0);
            console.ExitBuffering(false);
        }



        public void Print(char character, ushort foreColor, ushort backColor)
        {
            uint x = console.CursorX;
            uint y = console.CursorY;

            console.EnterBuffering(false);

            if (character == 0x08) // backspace
            {
                if (x > 0)
                {
                    x--;
                }
            }
            else
            {
                console.WriteCharacter(x, y, character, foreColor, backColor);
                x++;
                if (x >= Console.Columns)
                {
                    x = 0;
                    y++;
                    if (y >= Console.Rows)
                    {
                        // "scroll" display one line
                        console.ScrollUp();
                        y--;
                    }
                }
            }
            console.SetCursor(x, y);

            console.ExitBuffering(false);
        }


        public void Print(string str, ushort foreColor, ushort backColor)
        {
            uint length = (uint)str.Length;
            if (length > 0)
            {
                uint x = console.CursorX;
                uint y = console.CursorY;

                console.EnterBuffering(false);

                for (int i = 0; i<length; i++)
                {
                    char c = str[i];
                    if (c == 0x08) // backspace
                    {
                        if (x > 0)
                        {
                            x--;
                            console.SetCursor(x, y);
                        }
                    }
                                else
                    {
                        console.WriteCharacter(x, y, c, foreColor, backColor);
                        x++;
                        if (x >= Console.Columns)
                        {
                            x = 0;
                            y++;
                            if (y >= Console.Rows)
                            {
                                // "scroll" display one line
                                console.ScrollUp();
                                y--;
                            }
                        }
                    }
                }
                console.SetCursor(x, y);
                console.ExitBuffering(false);
            }
        }

        public void PrintLn(string str, ushort foreColor, ushort backColor)
        {
            console.EnterBuffering(false);
            Print(str, foreColor, backColor);
            uint y = console.CursorY;
            y++;
            if (y >= Console.Rows)
            {
                // "scroll" display one line
                console.ScrollUp();
                y--;
            }
            console.SetCursor(0, y);
            console.ExitBuffering(false);
        }

        public void PrintLn()
        {
            PrintLn("", 0x000, 0x000);
        }


    }
}
