program Command
{
#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"
    
    string Name                 { get { return "DIR";  } }
    string Description          { get { return "list files and subdirectories of a directory (alias LS)"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return true;  } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return true;  } } // *.*
    bool   SupportsRecursive    { get { return true;  } } // -s
    bool   SupportsConfirmation { get { return false; } } // -y
    
    bool doFullPaths;
    bool doTimeStamp;
    bool doCheckSum;
    
    ShowArguments()
    {
        WriteLn("  -f : show full paths for each file");
        WriteLn("  -t : include unix time stamp");
        WriteLn("  -c : include crc-16 checksums");
    }  
    bool Argument(string arg)
    {
        switch(arg)
        {
            case "-f": { doFullPaths = true; }
            case "-t": { doTimeStamp = true; }
            case "-c": { doCheckSum  = true; }
            default:   { return false;       }
        }
        return true;
    }
    bool OnFile(ShellObject shellObject, bool first, uint maxLength)
    {
        string path = shellObject.Path;
        string filename = path;
        if (!doFullPaths)
        {
            filename  = Path.GetFileName(path);
            maxLength = maxLength + filename.Length - path.Length;
            Write(" ");
        }
        Write(filename.Pad(' ', maxLength), Colour.MatrixBlue);
        if (doTimeStamp)
        {
            long ft = File.GetTime(path);
            Write("  0x"  +  ft.ToHexString(8), Colour.MatrixGreen);
        }
        if (doCheckSum)
        {
            uint crc = File.CRC16(path);
            Write("  0x"  +  crc.ToHexString(4), Colour.MatrixRed);
        }
        WriteLn();
        return true;
    } 
    bool OnDirectory(ShellObject shellObject)
    {
        if (!doFullPaths)
        {
            WriteLn(shellObject.Path, Colour.MatrixBlue);   
        }
        return true;
    }
    
    {
        if (Common.Arguments()) { Common.Walk(); }
    }
}
