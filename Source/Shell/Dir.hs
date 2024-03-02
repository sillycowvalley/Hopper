program Command
{
//#define SERIAL_CONSOLE
    uses "/Source/Shell/Common"
    
    string Name                 { get { return "DIR";  } }
    string Description          { get { return "list files and subdirectories of a directory (alias LS)"; } }
    
    bool   SupportsSource       { get { return true;  } } // directory with or without mask as source
    bool   SupportsSourceFile   { get { return true;  } } // single file as source (never confirm)
    bool   SupportsDestination  { get { return false; } }
    bool   SupportsMask         { get { return true;  } } // *.*
    bool   SupportsRecursive    { get { return true;  } } // -s
    bool   SupportsConfirmation { get { return false; } } // -y
    bool   RequiresArguments    { get { return false;  } }
    
    bool doFullPaths;
    bool doTimeStamp;
    bool doCheckSum;
    
    ShowArguments()
    {
        WriteLn("  -f : show full paths for each file");
        WriteLn("  -t : include time and date stamp");
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
            string time = File.GetTime(path);
            string date = File.GetDate(path);
            Write("  "  + date + "  " + time, Colour.MatrixGreen);
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
            if (shellObject.Skipped != 0)
            {
                string plural = (shellObject.Skipped == 1) ? "" : "s";
                WriteLn("  " + (shellObject.Skipped).ToString() + " non Hopper compliant path" + plural + " ignored under " + shellObject.Path, Colour.MatrixOrange);
            }
        }
        return true;
    }
    
    {
        if (Common.Arguments()) { Common.Walk(); }
    }
}
