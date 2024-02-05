program Command
{
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
    bool OnDirectory(string path, bool empty) { return true; }
    
    bool OnFile(string path, bool first, uint maxLength)
    {
        Write(path.Pad(' ', maxLength), Colour.MatrixBlue);
        File.Delete(path);
        WriteLn(" deleted", Colour.MatrixRed);
        return true;
    } 
    
    {
        if (Common.Arguments()) { Common.Walk(); }
    }
}
