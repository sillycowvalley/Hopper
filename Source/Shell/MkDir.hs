program Command
{
//#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"

    string Name                 { get { return "MKDIR";  } }
    string Description          { get { return "create a new directory (alias MD)"; } }
    
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
    bool OnDirectory(ShellObject shellObject) { return true; }
    
    Hopper()
    {
        if (Common.Arguments()) 
        { 
            string path = Common.StartFolder;
            Directory.Create(path);
            directory dir = Directory.Open(path);
            if (!dir.IsValid())
            {
                Common.ErrorMessage = "failed to create '" + path + "'";
                Common.ShowUsage(false);
            }       
        }
    }
}
