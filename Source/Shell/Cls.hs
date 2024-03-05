program Command
{
//#define SERIAL_CONSOLE
    uses "Common"
    
    string Name                 { get { return "CLS";  } }
    string Description          { get { return "clear the screen"; } }
    
    bool   SupportsSource       { get { return false; } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return false; } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return false; } } // *.*
    bool   SupportsRecursive    { get { return false; } } // -s
    bool   SupportsConfirmation { get { return false; } } // -y
    bool   RequiresArguments    { get { return false;  } }
    
    ShowArguments() {}
    bool Argument(string arg) { return false; }
    bool OnDirectory(string path) { return true; }
    bool OnFile(string path, bool first, uint maxLength) { return true; }
    
    {
        if (Common.Arguments()) 
        { 
#ifdef SERIAL_CONSOLE
            IO.Clear(); 
#else
            Screen.Clear(); 
#endif
        }
    }
}

