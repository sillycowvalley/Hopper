program Command
{
//#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"

    string Name                 { get { return "RMDIR";  } }
    string Description          { get { return "remove directory (alias RD)"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return false; } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return false; } } // *.*
    bool   SupportsRecursive    { get { return false; } } // -s
    bool   SupportsConfirmation { get { return false; } } // -y
    bool   RequiresArguments    { get { return true;  } }
    
    
    ShowArguments() {}
    bool Argument(string arg) { return false; }
    bool OnFile(ShellObject shellObject, bool first, uint maxLength) { return true; }
    bool OnDirectory(ShellObject shellObject) 
    { 
        string path = shellObject.Path;
        
        Directory.Delete(path);
        directory dir = Directory.Open(path);
        if (dir.IsValid())
        {
            PrintLn("failed to delete " + path);
            Common.WriteLn("  failed to delete '" + path + "'", Colour.MatrixRed);
        }
        return true; 
    }
    
    Hopper()
    {
        if (Common.Arguments()) { Walk(); }
    }
}
