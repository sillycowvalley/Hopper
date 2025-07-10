using HopperNET;
using HopperRuntime;
using System.Runtime.InteropServices;
using static System.Net.Mime.MediaTypeNames;

using Terminal.Gui.Drawing;
using Terminal.Gui.Views;
using System.Diagnostics;


/*
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
*/

public class Screen : IHopperScreen
{
    private TextGridView textView;
    ushort x;
    ushort y;
    ushort width;
    ushort height;
    public Screen(TextGridView textView)
    {
        this.textView = textView;
        Resize();
    }
    public void Resize()
    {
        x = 0;
        y = 0;
        height = (ushort)textView.Rows;
        width  = (ushort)textView.Columns;
    }

    public ushort Rows 
    { 
        get 
        { 
            return height; 
        } 
    }
    public ushort Columns 
    { 
        get 
        { 
            return width; 
        } 
    }
    public ushort CursorY 
    { 
        get 
        { 
            return y; 
        } 
    }
    public ushort CursorX 
    { 
        get 
        { 
            return x; 
        } 
    }
    private bool contains(ushort x, ushort y)
    {
        return ((x >= 0) && (y >= 0) && (x < width) && (y < height));
    }

    internal static Color ToColor(ushort c444)
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

    private void print(char c, ushort foreColor, ushort backColor)
    {
        if (contains(x, y))
        {
            textView.DrawChar(x, y, c, ToColor(foreColor), ToColor(backColor));
        }
        
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
    public void Print(string text, ushort foreColor, ushort backColor)
    {
        Suspend();
        foreach (char c in text)
        {
            print(c, foreColor, backColor);
        }
        Resume(false);
    }
    public void PrintLn(string text, ushort foreColor, ushort backColor)
    {
        Suspend();
        Print(text, foreColor, backColor);
        PrintLn();
        Resume(false);
    }
    public void Print(char c, ushort foreColor, ushort backColor)
    {
        Suspend();
        Print(c + String.Empty, foreColor, backColor);
        Resume(false);
    }
    public void PrintLn()
    {
        Suspend();
        newline();
        Resume(false);
    }

    public void SetCursor(ushort x, ushort y)
    {
        if ((x >= 0) && (y >= 0) && (x < this.Columns) && (y < this.Rows))
        {
            this.x = x;
            this.y = y;
            if (contains(x, y))
            {
                textView.SetCursor(x, y);
            }
        }
    }

    int suspendCount = 0;

    public void DrawChar(ushort x, ushort y, char c, ushort foreColour, ushort backColour)
    {
        Suspend();
        if (contains(x, y))
        {
            textView.DrawChar(x, y, c, ToColor(foreColour), ToColor(backColour));
        }
        Resume(false);
    }
    public void Clear()
    {
        Suspend();
        for (ushort row = 0; row < this.Rows; row++)
        {
            for (ushort col = 0; col < this.Columns; col++)
            {
                if (contains(col, row))
                {
                    textView.DrawChar(col, row, ' ', Color.Black, Color.Black);
                }
            }
        }
        this.SetCursor(0, 0);
        Resume(false);
    }

    public void Suspend()
    {
        if (suspendCount == 0)
        {
            ShowCursor(false);
        }
        suspendCount++;
    }

    public void Resume(bool isInteractive)
    {
        suspendCount--;
        if (suspendCount == 0)
        {
            textView.UpdateTextCells();
            this.ShowCursor(true);
        }
    }



    public void ShowCursor(bool show)
    {
        textView.CursorVisible = show;
    }

}