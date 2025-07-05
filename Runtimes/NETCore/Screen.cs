using HopperNET;
using HopperRuntime;
using System.Runtime.InteropServices;
using static System.Net.Mime.MediaTypeNames;

using Terminal.Gui.Drawing;
using Terminal.Gui.Views;

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
    private TextGridView textView;
    ushort x;
    ushort y;
    public Screen(TextGridView textView)
    {
        this.textView = textView;
    }

    public uint Rows 
    { 
        get 
        { 
            return (uint)textView.Rows; 
        } 
    }
    public uint Columns 
    { 
        get 
        { 
            return (uint)textView.Columns; 
        } 
    }
    public uint CursorY 
    { 
        get 
        { 
            return (uint)y; 
        } 
    }
    public uint CursorX 
    { 
        get 
        { 
            return (uint)x; 
        } 
    }

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
        return new Color(r, g, b);
    }

    private void print(char c, uint foreColor, uint backColor)
    {
        textView.DrawChar(x, y, c, ToColor(foreColor), ToColor(backColor));
        x++;
        if (x == this.Columns)
        {
            newline();
        }
        this.SetCursor(x, y);
    }
    public void newline()
    {
        x = 0;
        y++;
        if (y == this.Rows)
        {
            textView.ScrollUp();
            y--;
        }
        this.SetCursor(x, y);
    }
    public void Print(string text, uint foreColor, uint backColor)
    {
        foreach (char c in text)
        {
            print(c, foreColor, backColor);
        }
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
        newline();
    }

    internal void SetCursor(ushort x, ushort y)
    {
        if ((x >= 0) && (y >= 0) && (x < this.Columns) && (y < this.Rows))
        {
            this.x = x;
            this.y = y;
            textView.SetCursor(x, y);
        }
    }

    int suspendCount = 0;

    internal void DrawChar(ushort x, ushort y, char c, uint foreColour, uint backColour)
    {
        Suspend();
        textView.DrawChar(x, y, c, ToColor(foreColour), ToColor(backColour));
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
            this.ShowCursor(true);
        }
    }

    internal void Clear()
    {
        for (ushort row = 0; row < this.Rows; row++)
        {
            for (ushort col = 0; col < this.Columns; col++)
            {
                textView.DrawChar(col, row, ' ', Color.Black, Color.Black);
            }
        }
        this.SetCursor(0, 0);
    }

    internal void ShowCursor(bool show)
    {
        textView.CursorVisible = show;
    }

}