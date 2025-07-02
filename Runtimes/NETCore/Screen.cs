using System.Drawing;
using System.Runtime.InteropServices;

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

public class Screen
{

    public uint Rows { get { return (uint)Console.WindowHeight; } }
    public uint Columns { get { return (uint)Console.WindowWidth; } }
    public uint CursorY { get { return (uint)Console.CursorTop; } }
    public uint CursorX { get { return (uint)Console.CursorLeft; } }

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

    public void Print(string text, uint foreColor, uint backColor)
    {
        Color rgb = ToColor(backColor);
        SetBackgroundRGB(rgb.R, rgb.G, rgb.B);
        rgb = ToColor(foreColor);
        SetForegroundRGB(rgb.R, rgb.G, rgb.B);
        Console.Write(text);
        ResetColors();
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

    internal void DrawChar(ushort x, ushort y, char c, uint foreColour, uint backColour)
    {
        int oldX = Console.CursorLeft;
        int oldY = Console.CursorTop;

        SetCursor(x, y);
        Print(c, foreColour, backColour);

        Console.SetCursorPosition(oldX, oldY);
    }

    internal void SetCursor(ushort x, ushort y)
    {
        if ((x >= 0) && (y >= 0) && (x < Console.BufferWidth) && (y < Console.BufferHeight))
        {
            Console.SetCursorPosition(x, y);
        }
    }

    int suspendCount = 0;
    internal void Suspend()
    {
        if (suspendCount == 0)
        {
            ShowCursor(false);
        }
        suspendCount++;
    }

    internal void Resume(bool v)
    {
        suspendCount--;
        if (suspendCount == 0)
        {
            ShowCursor(true);
        }
    }

    internal void Clear()
    {
        Console.Clear();
    }

    internal void ShowCursor(bool show)
    {
        Console.CursorVisible = show;
    }

}