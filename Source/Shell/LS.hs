program Command
{
    uses "/Source/Shell/Common"
    
    string Name { get { return "LS"; } }
    bool   SupportsRecursive { get { return true; } }
    
    bool doFullPaths;
    bool doTimeStamp;
    bool doCheckSum;
    
    ShowArguments()
    {
        WriteLn("  -f : full paths for each file");
        WriteLn("  -t : include unix time stamp");
        WriteLn("  -c : include crc-16 checksums");
    }  
    bool Argument(string arg)
    {
        switch(arg)
        {
            case "-f":
            {
                doFullPaths = true;
            }
            case "-t":
            {
                doTimeStamp = true;
            }
            case "-c":
            {
                doCheckSum  = true;
            }
            default:
            {
                return false;
            }
        }
        return true;
    }
    OnFile(string path, bool first, uint maxLength)
    {
        string filename = path;
        if (!doFullPaths)
        {
            filename  = Path.GetFileName(path);
            maxLength = maxLength + filename.Length - path.Length;
            Write(" ");
        }
        Write(filename.Pad(' ', maxLength));
        if (doTimeStamp)
        {
            long ft = File.GetTime(path);
            Write("  0x"  +  ft.ToHexString(8));
        }
        if (doCheckSum)
        {
            uint crc = File.CRC16(path);
            Write("  0x"  +  crc.ToHexString(4));
        }
        WriteLn();
    } 
    OnDirectory(string path, bool empty)
    {
        if (!doFullPaths && !empty)
        {
            WriteLn(path);   
        }
    }
    
    {
        if (Common.Arguments())
        {
            Common.Walk(Common.StartFolder);
        }
    }
}
