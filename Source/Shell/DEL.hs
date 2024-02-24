program Command
{
//#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"
    
    string Name                 { get { return "DEL";  } }
    string Description          { get { return "delete one or more files (alias RM)"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return true;  } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return true;  } } // *.*
    bool   SupportsRecursive    { get { return true;  } } // -s
    bool   SupportsConfirmation { get { return true;  } } // -y
    
    ShowArguments() {}
    bool Argument(string arg) { return false; }
    bool OnDirectory(ShellObject shellObject) { return true; }
    
    bool OnFile(ShellObject shellObject, bool first, uint maxLength)
    {
        string path = shellObject.Path;
        Write(path.Pad(' ', maxLength), Colour.MatrixBlue);
        File.Delete(path);
        WriteLn(" deleted", Colour.MatrixRed);
        return true;
    } 
    
    {
        if (Common.Arguments()) { Common.Walk(); }
    }
}
