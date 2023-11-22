unit HRScreen
{
    Suspend()
    {
#ifndef SERIALCONSOLE        
        Screen.Suspend();
#endif
    }
    Resume(bool isInteractive)
    {
#ifndef SERIALCONSOLE        
        Screen.Resume(isInteractive);
#endif
    }
    Clear()
    {
#ifndef SERIALCONSOLE        
        Screen.Clear();
#endif
    }    
    SetPixel(uint x, uint y, uint colour)
    {
    }
#ifndef SERIALCONSOLE   
    byte Columns { get { return Screen.Columns; }  }
    byte Rows    { get { return Screen.Rows; }  }
#else
    byte Columns { get { return 0; }  }
    byte Rows    { get { return 0; }  }
#endif
    DrawChar(uint col, uint row, char c, uint foreColour, uint backColour)
    {
    }
}
