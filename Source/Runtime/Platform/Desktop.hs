unit Desktop
{
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "HopperVM"
    
    Print(char c, uint foreColour, uint backColour)
    {
        Screen.Print(c, foreColour, backColour);
    }
    Print(uint hstr, uint foreColour, uint backColour)
    {
        string str = External.nativeStringFromHopperString(hstr);
        Screen.Print(str, foreColour, backColour);
    }
    PrintLn()
    {
        Screen.PrintLn();
    }
    Clear()
    {
        Screen.Clear();
    }
    SetCursor(uint col, uint row)
    {
        Screen.SetCursor(col, row);
    }
    byte GetColumns()
    {
        return Screen.Columns;
    }
    byte GetRows()
    {
        return Screen.Rows;
    }
    byte GetCursorX()
    {
        return Screen.CursorX;
    }
    byte GetCursorY()
    {
        return Screen.CursorY;
    }
    Suspend()
    {
        Screen.Suspend();
    }
    Resume(bool isInteractive)
    {
        Screen.Resume(isInteractive);
    }
    uint ReadKey()
    {
        return uint(Keyboard.ReadKey());
    }
    bool IsAvailable()
    {
        return Keyboard.IsAvailable;
    }
    uint ClickX()
    {
        return Keyboard.ClickX;
    }
    uint ClickY()
    {
        return Keyboard.ClickY;
    }
    bool ClickUp()
    {
        return Keyboard.ClickUp;
    }
    bool ClickDouble()
    {
        return Keyboard.ClickDouble;
    }
    int ScrollDelta()
    {
        return Keyboard.ScrollDelta;
    }
}    
