unit Common
{
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    bool doRecursive;
    bool DoRecursive { get { return doRecursive; } }

    string fileMask;   
    string maskStartsWith;
    string maskEndsWith;
                
    string startFolder;
    string StartFolder { get { return startFolder; } }
    
    ShowArguments()
    {
        WriteLn("Invalid arguments for " + Command.Name + ":");
        Command.ShowArguments();
        if (Command.SupportsRecursive)
        {
            WriteLn("  -s : this directory and all subdirectories");
        }
    }
    InvalidMask()
    {
        WriteLn("Invalid wildcard for " + Command.Name + ":");
        WriteLn("  '*.*' : all");
        WriteLn("  'xxx.*' : all with name 'xxx'");
        WriteLn("  '*.xxx' : all with extension 'xxx'");
        WriteLn("  'xxx*' : all with name.ext starting with 'xxx'");
        WriteLn("  '*xxx' : all with name.ext ending with 'xxx'");
    }
    Walk(string currentFolder)
    {
        directory dir = Directory.Open(currentFolder);
        if (dir.IsValid())
        {
            uint files =  dir.GetFileCount();
            uint length;
            <string> fileList;
            for (uint fi = 0; fi < files; fi++)
            {
                string filepath = dir.GetFile(fi);
                string filename = Path.GetFileName(filepath);
                string filenameLower = filename.ToLower();
                if (maskEndsWith.Length > 0)
                {
                    if (!filenameLower.EndsWith(maskEndsWith))
                    {
                        continue;
                    }
                }
                if (maskStartsWith.Length > 0)
                {
                    if (!filenameLower.StartsWith(maskStartsWith))
                    {
                        continue;
                    }
                }
                fileList.Append(filepath);
                if (filepath.Length > length)
                {
                    length = filepath.Length;
                }
            }
            Command.OnDirectory(currentFolder, fileList.Length == 0);
            bool first = true;
            foreach (var filepath in fileList)
            {
                Command.OnFile(filepath, first, length);
                first = false;
            }
            if (DoRecursive)
            {
                uint dirs = dir.GetDirectoryCount();
                for (uint di = 0; di < dirs; di++)
                {
                    string subFolder = dir.GetDirectory(di);
                    Walk(subFolder);
                }
            }
        }
    }
    
    bool ValidateMask(string mask)
    {
        bool good = true;
        
        uint iWDot;
        bool dotFound;
        uint iLastStar;
        uint iStar;
        bool starFound;
        if (mask.IndexOf('.', ref iWDot))
        {
            dotFound = true;
        }
        if (mask.IndexOf('*', ref iStar))
        {
            starFound = true;
            if (mask.LastIndexOf('*', ref iLastStar))
            {
            }
        }
        if (mask == "*.*")
        {
            mask = ""; // all
        }
        else if (starFound && (iStar != iLastStar))
        {
            good = false; // only one '*' if not '*.*'
        }
        else if (!dotFound)
        {
            if (mask.Length < 2)
            {
                good = false;
            }
            else if (mask.StartsWith('*'))
            {
                // *xxxx
                maskEndsWith = mask.Substring(1);
                maskEndsWith = maskEndsWith.ToLower();
            }
            else if (mask.EndsWith('*'))
            {
                // xxxx*
                maskStartsWith = mask.Substring(0, iStar);
                maskStartsWith = maskStartsWith.ToLower();
            }
            else
            {
                good = false; // no '.', then must start or end with '*'
            }
        }
        else
        {
            <string> parts = mask.Split('.');
            good = parts.Length == 2;
            if (good)
            {
                string fileprefix = parts[0];
                string extension  = parts[1];
                if (fileprefix == "*")
                {
                    // *.xxx
                     maskEndsWith = "." + extension;
                     maskEndsWith = maskEndsWith.ToLower();
                }
                else if (extension == "*")
                {
                    // xxx.*
                    maskStartsWith = fileprefix + ".";
                    maskStartsWith = maskStartsWith.ToLower();
                }
                else
                {
                    good = false;
                }
            }
        }
        return good;
    }
    
    bool Arguments()
    {
        bool success = true;
        <string> rawargs = System.Arguments;
        <string> args;
        foreach (var arg in rawargs)
        {
            if ((arg.Length == 2) && arg.StartsWith('-'))
            {
                switch (arg)
                {
                    case "-s": // recursive
                    {
                        if (Command.SupportsRecursive)
                        {
                            doRecursive = true;
                        }
                        else
                        {
                            success = false;
                            break;
                        }
                    }
                    default:
                    {
                        if (!Command.Argument(arg))
                        {
                            success = false;
                            break;
                        }
                    }
                }
            }
            else if (startFolder.Length != 0)
            {
                success = false;
                break;
            }
            else
            {
                startFolder = arg;
            }
        }
        if (!success)
        {
            Common.ShowArguments();
        }
        else
        {
            if (startFolder.Length == 0)
            {
                startFolder = "*.*";
            }
            if (!startFolder.StartsWith('/'))
            {
                startFolder = Path.Combine(System.CurrentDirectory, startFolder);
            }
            uint iDot; uint iStar;
            if (!startFolder.IndexOf('.', ref iDot) && !startFolder.IndexOf('*', ref iStar))
            {
                if (!startFolder.EndsWith('/'))
                {
                    startFolder += "/";
                }
                startFolder += "*.*";
            }
            uint iSlash;
            if (startFolder.LastIndexOf('/', ref iSlash))
            {
                fileMask    = startFolder.Substring(iSlash+1);
                startFolder = startFolder.Substring(0, iSlash);
                if (startFolder.Length == 0)
                {
                    startFolder = "/";
                }
                uint iStar;
                if (startFolder.IndexOf('*', ref iStar))
                {
                    success = false;
                }
            }
            if (success)
            {
                success = ValidateMask(fileMask);
            }
            /*
            WriteLn("fileMask:       " + fileMask);
            WriteLn("maskStartsWith: " + maskStartsWith);
            WriteLn("maskEndsWith:   " + maskEndsWith);
            WriteLn("startFolder:    " + startFolder);
            */
            if (!success)
            {
                InvalidMask();
            }
        }
        return success;
    }
}
