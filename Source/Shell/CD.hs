program Command
{
    uses "/Source/Shell/Common"
    
    string Name                 { get { return "CD";  } }
    string Description          { get { return "display the name of or change the current directory"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return false; } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return false; } } // *.*
    bool   SupportsRecursive    { get { return false; } } // -s
    bool   SupportsConfirmation { get { return false; } } // -y
    
    
    ShowArguments() {}
    bool Argument(string arg) { return false; }
    bool OnFile(string path, bool first, uint maxLength) { return true; }
    
    bool OnDirectory(string path, bool empty) 
    { 
        System.CurrentDirectory = path;
        return true; 
    }
    
    Hopper()
    {
        <string> args = Arguments;
        if (args.Count == 0)
        {
            WriteLn(" " + System.CurrentDirectory, Colour.MatrixBlue);
        }
        else
        {
            if (Common.Arguments()) { Walk(); }
        }
    }
}
