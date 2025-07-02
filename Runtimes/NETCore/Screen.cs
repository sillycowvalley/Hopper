using HopperNET;
using System.Drawing;
using System.Runtime.InteropServices;
using static System.Net.Mime.MediaTypeNames;

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

public struct ConsoleCell
{
    public bool isSet;
    public bool isChanged;
    public Color background;
    public Color foreground;
    public char character;
}

public class DeviceContext
{
    ConsoleCell[,] consoleCells;
    int width;
    int height;
    public DeviceContext()
    {
        Clear(Console.WindowWidth, Console.WindowHeight);
    }
    public void Clear(int width, int height)
    {
        this.width = width;
        this.height = height;
        consoleCells = new ConsoleCell[width, height];
    }
    public void DrawChar(int x, int y, char c, Color foreground, Color background)
    {
        consoleCells[x, y].isSet = true;
        if (consoleCells[x, y].character != c)
        {
            consoleCells[x, y].isChanged = true;
            consoleCells[x, y].character = c;
        }
        if ((consoleCells[x, y].foreground != foreground) && (c != ' '))
        {
            consoleCells[x, y].isChanged = true;
            consoleCells[x, y].foreground = foreground;
        }
        if (consoleCells[x, y].background != background)
        {
            consoleCells[x, y].isChanged = true;
            consoleCells[x, y].background = background;
        }
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

    public void Flush()
    {
        bool atLeastOne = false;
        string cells = String.Empty;
        int oldX = 0;
        int oldY = 0;
        Color? lastBackground = null;
        Color? lastForeground = null;

        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                if (consoleCells[x, y].isSet && consoleCells[x, y].isChanged)
                {
                    if (!atLeastOne)
                    {
                        oldX = Console.CursorLeft;
                        oldY = Console.CursorTop;
                        atLeastOne = true;
                    }
                    Console.SetCursorPosition(x, y);

                    Color background = consoleCells[x, y].background;
                    if (!lastBackground.HasValue || (lastBackground != background))
                    {
                        SetBackgroundRGB(background.R, background.G, background.B);
                        lastBackground = background;
                    }
                    Color foreground = consoleCells[x, y].foreground;
                    if (!lastForeground.HasValue || (lastForeground != foreground))
                    {
                        SetForegroundRGB(foreground.R, foreground.G, foreground.B);
                        lastForeground = foreground;
                    }
                    Console.Write(consoleCells[x, y].character);
                    consoleCells[x, y].isChanged = false;
                    cells += consoleCells[x, y].character;
                }
            }
        }
        if (atLeastOne)
        {
            ResetColors();
            Console.SetCursorPosition(oldX, oldY);
            string display = cells;
            if (display.Length > 80)
            {
                display = display.Substring(0, 80) + "..";
            }
            Diagnostics.OutputDebug("\nCells updated: " + cells.Length.ToString() + ", '" + display + "'");
        }
    }
}

public class Screen
{

    public uint Rows { get { return (uint)Console.WindowHeight; } }
    public uint Columns { get { return (uint)Console.WindowWidth; } }
    public uint CursorY { get { return (uint)Console.CursorTop; } }
    public uint CursorX { get { return (uint)Console.CursorLeft; } }

    DeviceContext deviceContext = new DeviceContext();

    internal static Color ToColor(uint c444)
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

    private void print(string text, uint foreColor, uint backColor)
    {
        Color rgb = ToColor(backColor);
        SetBackgroundRGB(rgb.R, rgb.G, rgb.B);
        rgb = ToColor(foreColor);
        SetForegroundRGB(rgb.R, rgb.G, rgb.B);
        Console.Write(text);
        ResetColors();
    }
    public void Print(string text, uint foreColor, uint backColor)
    {
        print(text, foreColor, backColor);
    }
    public void PrintLn(string text, uint foreColor, uint backColor)
    {
        Print(text, foreColor, backColor);
        PrintLn();
    }
    public void Print(char c, uint foreColor, uint backColor)
    {
        Print(c + String.Empty, foreColor, backColor);
    }
    public void PrintLn()
    {
        Console.WriteLine();
    }

    internal void SetCursor(ushort x, ushort y)
    {
        if ((x >= 0) && (y >= 0) && (x < Console.BufferWidth) && (y < Console.BufferHeight))
        {
            Console.SetCursorPosition(x, y);
        }
    }

    int suspendCount = 0;
    
    internal void DrawChar(ushort x, ushort y, char c, uint foreColour, uint backColour)
    {
        Suspend();

        deviceContext.DrawChar(x, y, c, ToColor(foreColour), ToColor(backColour));

        Resume(false);
    }

    internal void Suspend()
    {
        if (suspendCount == 0)
        {
            ShowCursor(false);
        }
        suspendCount++;
    }

    internal void Resume(bool isInteractive)
    {
        suspendCount--;
        if (suspendCount == 0)
        {
            deviceContext.Flush();
            ShowCursor(true);
        }
    }

    internal void Clear()
    {
        Console.Clear();
        deviceContext.Clear(Console.WindowWidth, Console.WindowHeight);
    }

    internal void ShowCursor(bool show)
    {
        Console.CursorVisible = show;
    }

}