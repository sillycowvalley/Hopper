program Command
{
//#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"
    
    string Name                 { get { return "SHOW";  } }
    string Description          { get { return "echo file content to console (alias TYPE)"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return true;  } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return true;  } } // *.*
    bool   SupportsRecursive    { get { return true;  } } // -s
    bool   SupportsConfirmation { get { return true;  } } // -y
    bool   RequiresArguments    { get { return true;  } }
    
    ShowArguments() {}
    bool Argument(string arg) { return false; }
    bool OnDirectory(ShellObject shellObject) { return true; }
    
    bool OnFile(ShellObject shellObject, bool first, uint maxLength)
    {
        string path = shellObject.Path;
        WriteLn(path.Pad(' ', maxLength) + ":", Colour.MatrixBlue);
        file textFile = File.Open(path);
        if (textFile.IsValid())
        {
            loop
            {
                string ln = textFile.ReadLine();
                if (ln.Length == 0)
                {
                    if (!textFile.IsValid())
                    {
                        break;
                    }
                }
                WriteLn(ln, Colour.MatrixBlue);
            }
        }
        return true;
    } 
    
    {
        if (Common.Arguments()) { Common.Walk(); }
    }
}

