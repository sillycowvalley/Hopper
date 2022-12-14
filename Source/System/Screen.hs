unit Screen
{
    uses "/Source/System/Color"


#ifndef ZOPPER
    byte CursorX { get system; }
    byte CursorY { get system; }
    byte Columns { get system; }
    byte Rows    { get system; }

    Suspend() system;
    
    // if !isInteractive then Resume will pump messages (not needed if we are processing keystrokes)
    Resume(bool isInteractive) system;
    
#endif

    Clear() system;

#ifndef ZOPPER    
    SetCursor(uint x, uint y) system;
    //SetForeColour(uint foreColour) system;
    //SetBackColour(uint backColour) system;
#endif    
    
#ifndef ZOPPER
    DrawChar(uint x, uint y, char c, uint foreColour, uint backColour) system;
#endif    
    Print(char c,     uint foreColour, uint backColour) system;
    Print(string s,   uint foreColour, uint backColour) system;
    PrintLn() system;



    PrintLn(char c,   uint foreColour, uint backColour)
    {
        Print(c, foreColour, backColour);
        PrintLn();
    }
    PrintLn(string s, uint foreColour, uint backColour)
    {
        Print(s, foreColour, backColour);
        PrintLn();
    }

    
    // use current colours, TinyHopper ignores colours
    
    Print(char c)
    {
        Print(c, Color.MatrixGreen, Color.Black);
    }
    Print(string s)
    {
        Print(s, Color.MatrixGreen, Color.Black);
    }
    PrintLn(char c)
    {  
        Print(c, Color.MatrixGreen, Color.Black);
        PrintLn();
    }
    PrintLn(string s)
    {  
        Print(s, Color.MatrixGreen, Color.Black);
        PrintLn();
    } 

}