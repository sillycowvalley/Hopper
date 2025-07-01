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

public class ScreenUnit
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

    public void Print(string text, uint foreColor, uint backColor)
    {
        Color rgb = ToColor(backColor);
        SetBackgroundRGB(rgb.R, rgb.G, rgb.B);
        rgb = ToColor(foreColor);
        SetForegroundRGB(rgb.R, rgb.G, rgb.B);
        Console.Write(text);
        ResetColors();
    }

    public void PrintLn()
    {
        Console.WriteLine();
    }

    /*
    // Set cursor position (0-based coordinates)
    Console.SetCursorPosition(x, y);

    // Get current cursor position
    int x = Console.CursorLeft;
    int y = Console.CursorTop;

    // Example: Write at specific position
    Console.SetCursorPosition(10, 5);
    Console.Write("Hello at (10,5)!");
    */
}