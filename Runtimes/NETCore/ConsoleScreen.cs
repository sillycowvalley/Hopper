using HopperRuntime;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using static System.Net.Mime.MediaTypeNames;

namespace HopperRuntime
{

    public static class PowerShellAnsiEnabler
    {
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern IntPtr GetStdHandle(int nStdHandle);

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool GetConsoleMode(IntPtr hConsoleHandle, out uint lpMode);

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool SetConsoleMode(IntPtr hConsoleHandle, uint dwMode);

        private const int STD_OUTPUT_HANDLE = -11;
        private const uint ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004;

        public static bool EnableAnsiSupport()
        {
            if (!OperatingSystem.IsWindows()) return true; // Assume Unix supports ANSI

            var handle = GetStdHandle(STD_OUTPUT_HANDLE);
            if (handle == IntPtr.Zero) return false;

            if (!GetConsoleMode(handle, out uint mode)) return false;

            mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            return SetConsoleMode(handle, mode);
        }
    }

    public class ConsoleScreen : IHopperScreen
    {
        Color ToColor(uint c444)
        {
            if (c444 == 0)
            {
                return Color.Black;
            }
            byte r = (byte)(c444 >> 8);
            byte g = (byte)((c444 >> 4) & (0x0F));
            byte b = (byte)(c444 & 0x0F);
            if (r <= 7)
            {
                r = (byte)(r << 4);
            }
            else
            {
                r = (byte)((r << 4) | 0x0F);
            }
            if (g <= 7)
            {
                g = (byte)(g << 4);
            }
            else
            {
                g = (byte)((g << 4) | 0x0F);
            }
            if (b <= 7)
            {
                b = (byte)(b << 4);
            }
            else
            {
                b = (byte)((b << 4) | 0x0F);
            }
            return Color.FromArgb(r, g, b);
        }
        public static void SetForegroundRGB(int r, int g, int b)
        {
            Console.Write($"\x1b[38;2;{r};{g};{b}m");
        }
        public static void ResetColors()
        {
            Console.Write("\x1b[0m");
        }
        public static void SetBackgroundRGB(int r, int g, int b)
        {
            Console.Write($"\x1b[48;2;{r};{g};{b}m");
        }

        public ushort Columns { get { return (ushort)Console.WindowWidth; } }

        public ushort Rows { get { return (ushort)Console.WindowHeight; } }

        public ushort CursorX { get { return (ushort)Console.CursorLeft; } }

        public ushort CursorY { get { return (ushort)Console.CursorTop; } }

        public void Clear()
        {
            Console.Clear();
            Console.SetCursorPosition(0, 0);
        }

        public void DrawChar(ushort x, ushort y, char c, ushort foreColour, ushort backColour)
        {
            SetCursor(x, y);
            Print(c, foreColour, backColour);
        }

        public void Print(char c, ushort foreColour, ushort backColour)
        {
            Color rgb = ToColor(backColour);
            SetBackgroundRGB(rgb.R, rgb.G, rgb.B);
            rgb = ToColor(foreColour);
            SetForegroundRGB(rgb.R, rgb.G, rgb.B);
            Console.Write(c);
            ResetColors();
        }

        public void Print(string s, ushort foreColour, ushort backColour)
        {
            Color rgb = ToColor(backColour);
            SetBackgroundRGB(rgb.R, rgb.G, rgb.B);
            rgb = ToColor(foreColour);
            SetForegroundRGB(rgb.R, rgb.G, rgb.B);
            Console.Write(s);
            ResetColors();
        }
        public void PrintLn()
        {
            Console.WriteLine();
        }
        public void PrintLn(string s, ushort foreColour, ushort backColour)
        {
            Print(s, foreColour, backColour);
            PrintLn();
        }

        public void Resize()
        {
            
        }
        private bool contains(ushort x, ushort y)
        {
            return (x >= 0) && (y >= 0) && (x < Columns) && (y < Rows);
        }
        public void SetCursor(ushort x, ushort y)
        {
            if (contains(x, y))
            {
                Console.SetCursorPosition(x, y);
            }
        }

        public void ShowCursor(bool visible)
        {
            Console.CursorVisible = visible;
        }
        public void Resume(bool interactive)
        {
            
        }

        public void Suspend()
        {
            
        }
    }
}
