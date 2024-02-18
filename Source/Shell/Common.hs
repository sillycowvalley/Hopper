unit Common
{
    
#ifndef SERIAL_CONSOLE
    uses "/Source/System/Runtime"
#endif

    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    
#ifdef SERIAL_CONSOLE    
    bool doRecursive = true; // TODO REMOVE : MCU testing
#else
    bool doRecursive;
#endif
    bool skipConfirmation;
    
    bool cancelled;
    
    string fileMask;
    string maskStartsWith;
    string maskEndsWith;
    
    string errorMessage;
    
    string destinationFolder;                
    string startFolder;
    string singleFile;
    
    bool starDotStarAdded;
    
    record ShellObject
    {
        string Path;
        bool   IsDirectory; // false by default -> File
        
        // directory members    
        uint FileCount;
        uint DirectoryCount;
        uint Skipped;
    }
    
    int CompareShellObjects(<ShellObject> this, uint ai, uint bi)
    {
        ShellObject a = this.GetItem(ai);
        ShellObject b = this.GetItem(bi);
        return String.Compare((a.Path).ToLower(), (b.Path).ToLower());
    }
    
    string ResolveCommandPath(string command)
    {
        string extension = Path.GetExtension(command);
        extension = extension.ToLower();
        if ((extension == ".hexe") || (extension == ".cmd")) 
        {
            if (File.Exists(command)) { } // already good
            else
            {
                string binaryname = Path.Combine(CurrentDirectory, command); // current directory?
                if (File.Exists(binaryname)) { command = binaryname; }
                else
                {
                    binaryname = Path.Combine("/bin", command); // bin folder?
                    if (File.Exists(binaryname)) { command = binaryname; }
                }
            }
        }
        else if (extension == ".")
        {
            string binaryname = command; // full path?
            if (File.Exists(binaryname + HexeExtension))     { command = binaryname + HexeExtension; }
            else if (File.Exists(binaryname + ".cmd"))       { command = binaryname + ".cmd";        }
            else
            {
                binaryname = Path.Combine(CurrentDirectory, command); // current directory?
                if (File.Exists(binaryname + HexeExtension)) { command = binaryname + HexeExtension; }
                else if (File.Exists(binaryname + ".cmd"))   { command = binaryname + ".cmd";        }
                else
                {
                    binaryname = Path.Combine("/bin", command); // bin folder?
                    if (File.Exists(binaryname + HexeExtension)) { command = binaryname + HexeExtension; }
                    else if (File.Exists(binaryname + ".cmd"))   { command = binaryname + ".cmd";        }
                }
            }
        }
        return command;        
    }
    
    bool TryDirectoryFromPath(string path, ShellObject dirObject)
    {
        directory dir = Directory.Open(path);
        bool valid = dir.IsValid();
        if (valid)
        {
            uint skippedFiles;
            uint skippedDirectories;
            dirObject.Path           = path;
            dirObject.IsDirectory    = true;
            dirObject.FileCount      = dir.GetFileCount(ref skippedFiles);
            dirObject.DirectoryCount = dir.GetDirectoryCount(ref skippedDirectories);
            dirObject.Skipped        = skippedFiles + skippedDirectories;
        }
        return valid;
    }
    bool TryFileFromPath(string path, ShellObject fileObject)
    {
        file fl = File.Open(path);
        bool valid = fl.IsValid();
        if (valid)
        {
            fileObject.Path = path;
        }
        return valid;
    }
    
    string SwitchAlias(string command)
    {
        switch (command.ToLower())
        {
            case "ls":                { command = "dir";   }
            case "man":               { command = "help";  }
            case "rm":                { command = "del";   }
            case "chdir":             { command = "cd";    }
            case "rd":                { command = "rmdir"; }
            case "md":                { command = "mkdir"; }
            case "cp":                { command = "copy";  }
            case "cmd":               { command = "shell"; }
            case "clear":             { command = "cls"; }
            case "mv":   case "move": { command = "ren";   }
            case "more": case "type": { command = "show";  }
        }
        return command;
    }
    
    Write(string msg)
    {
        IO.Write(msg);
    }
    WriteLn()
    {
        IO.WriteLn();
    }
    
    WriteLn(string msg)
    {
        Common.Write(msg);
        Common.WriteLn();
    }
    Write(string msg, uint foreColour)
    {
#ifdef SERIAL_CONSOLE
        Common.WriteLn("  " + msg);
#else
        Print("  " + msg, foreColour, Colour.Black);
#endif
    }
    WriteLn(string msg, uint foreColour)
    {
        Write(msg, foreColour);
        Common.WriteLn();
    }
        
    ShowArguments()    
    {   
        IO.WriteLn("Options for " + Command.Name + ":");
        Command.ShowArguments();
        if (Command.SupportsRecursive)
        {
            IO.WriteLn("  -s : source directory and all recursive subdirectories");
        }
        if (Command.SupportsConfirmation)
        {
            IO.WriteLn("  -y : skip confirmation for wildcard operation");
        }
        IO.WriteLn("  -h : show help");
    }
    ShowUsage(bool mask)
    {
        IO.WriteLn(Command.Name + " " + Command.Description + "."); 
        IO.WriteLn("Usage for " + Command.Name + ":");    
        IO.Write  ("  " + Command.Name + " ");
        if (Command.SupportsSource)
        {
            IO.Write("<source directory>");
        }
        if (Command.SupportsMask)
        {
            IO.Write("<mask>");
        }
        if (Command.SupportsDestination)
        {
            IO.Write(" <destination directory>");
        }
        IO.WriteLn();
        if (mask)
        {
            InvalidMask();
        }
        else if (errorMessage != "")
        {
          IO.WriteLn();   
          Common.WriteLn("  " + errorMessage, Colour.MatrixRed);
        }
    }
    InvalidMask()
    {
        IO.WriteLn();
        Common.WriteLn("Invalid mask for " +     Command.Name + ":", Colour.MatrixRed);
        Common.WriteLn("  " + errorMessage + ": '" + fileMask + "'", Colour.MatrixRed);
        IO.WriteLn("    '*.*' : all");
        IO.WriteLn("    'xxx.*' : all with name 'xxx'");
        IO.WriteLn("    '*.xxx' : all with extension 'xxx'");
        IO.WriteLn("    'xxx*' : all with name.ext starting with 'xxx'");
        IO.WriteLn("    '*xxx' : all with name.ext ending with 'xxx'");
    }
    Walk(ShellObject currentDirectory)
    {
        string currentPath         = currentDirectory.Path;
        uint currentFileCount      = currentDirectory.FileCount;
        uint currentDirectoryCount = currentDirectory.DirectoryCount;
        directory dir = Directory.Open(currentPath);
        if (dir.IsValid())
        {
            uint length;
            <ShellObject> objectList;
            for (uint fi = 0; fi < currentFileCount; fi++)
            {
                string filePath = dir.GetFile(fi);
                string filename = Path.GetFileName(filePath);
                string filenameLower = filename.ToLower();
                if (maskEndsWith.Length != 0)
                {
                    if (!filenameLower.EndsWith(maskEndsWith))
                    {
                        continue;
                    }
                }
                if (maskStartsWith.Length != 0)
                {
                    if (!filenameLower.StartsWith(maskStartsWith))
                    {
                        continue;
                    }
                }
                ShellObject fileObject;
                if (Common.TryFileFromPath(filePath, fileObject))
                {
                    objectList.Append(fileObject);
                    if (filePath.Length > length)
                    {
                        length = filePath.Length;
                    }
                }
            }
            for (uint di = 0; di < currentDirectoryCount; di++)
            {
                string subFolder = dir.GetDirectory(di);
                ShellObject directoryDir;
                if (Common.TryDirectoryFromPath(subFolder, directoryDir))
                {
                    objectList.Append(directoryDir);
                }
            }
            if (!Command.OnDirectory(currentDirectory))
            {
                cancelled = true;
            }
            if (!cancelled)
            {
                ListCompareDelegate comparer = CompareShellObjects;
                objectList.Sort(comparer);
                bool first = true;
                foreach (var shellObject in objectList)
                {
                    if (shellObject.IsDirectory)
                    {
                        if (doRecursive)
                        {
                            Walk(shellObject);
                        }
                        else if (Command.Name == "DIR")
                        {
                            // oh, what a lovely hack .. 
                            // (but what other command is interested in subFolders when not recursive?)
                            shellObject.Skipped = 0;
                            cancelled = !Command.OnDirectory(shellObject);
                        }
                    }
                    else
                    {
                        if (Command.SupportsConfirmation)
                        {
                            if (!skipConfirmation)
                            {
                                bool skip;
                                loop
                                {
                                    Common.Write(Command.Name + " " + shellObject.Path + " Y/N?", Colour.MatrixBlue);
                                    char c = (IO.Read()).ToUpper();
                                    IO.WriteLn();
                                    switch (c)
                                    {
                                        case 'Y':        { break; }
                                        case 'N':        { skip = true; break; }
                                        case char(0x03): { cancelled = true; break; }
                                    }
                                }
                                if (skip)      { continue; }
                                if (cancelled) { break; }
                            }
                        }
                        if (!Command.OnFile(shellObject, first, length))
                        {
                            cancelled = true;
                        }
                        first = false;
                    }
                    if (cancelled) { break; }
                }
            }
        } // dir.IsValid()
    }
    Walk()
    {
        ShellObject shellObject;
        if (singleFile.Length != 0)
        {
            if (Common.TryFileFromPath(singleFile, shellObject))
            {
                _ = Command.OnFile(shellObject, true, singleFile.Length);
            }
        }
        else
        {
            if (Common.TryDirectoryFromPath(startFolder, shellObject))
            {
                Walk(shellObject);
            }
        }
        if (cancelled)
        {
            Common.WriteLn("* cancelled *", Colour.MatrixRed);
        }
    }
    
    bool ValidateMask(string mask)
    {
        bool good;
        
        uint iWDot;
        bool dotFound;
        uint iLastStar;
        uint iStar;
        bool starFound;
        loop
        {
            //WriteLn("'" + mask + "'", Colour.MatrixCyan);
            if (!SupportsMask && !starDotStarAdded)
            {
                errorMessage = "<mask> not supported: '" + mask + "'";
                break;
            }
            if (mask == "*")
            {
                mask = "*.*"; // all
            }
            if (mask.IndexOf('.', ref iWDot))
            {
                dotFound = true;
            }
            if (mask.IndexOf('*', ref iStar))
            {
                starFound = true;
                _ = mask.LastIndexOf('*', ref iLastStar);
            }
            if (mask == "*.*")
            {
                mask = ""; // all
            }
            else if (starFound && (iStar != iLastStar))
            {
                errorMessage = "only one '*' if not '*.*'";
                break; // only one '*' if not '*.*'
            }
            else if (!dotFound)
            {
                if (mask.Length < 2)
                {
                    errorMessage = "bad mask";
                    break;
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
                    errorMessage = "no '.', then mask must start or end with '*'";
                    break; // no '.', then must start or end with '*'
                }
            }
            else
            {
                <string> parts = mask.Split('.');
                if (parts.Count != 2)
                {
                    errorMessage = "too many parts in mask";
                    break;
                }
                
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
                    errorMessage = "bad mask";
                    break;
                }
            }
            good = true;
            break;
        } // loop
        return good;
    }
    
    bool ReplaceDotDot(ref string folder)
    {
        uint iSlash;
        if (folder.IndexOf("/..", ref iSlash))
        {
            string prefix = folder.Substring(0, iSlash);
            string suffix = folder.Substring(iSlash+3);
            if (prefix.LastIndexOf('/', ref iSlash))
            {
                prefix = prefix.Substring(0, iSlash);
                folder = prefix + suffix;
                return true;
            }
        }
        return false;
    }
    
    bool Arguments()
    {
        bool success = true;
        bool badMask;
        <string> rawargs = System.Arguments;
        <string> args;
        foreach (var arg in rawargs)
        {
            if ((arg.Length == 2) && arg.StartsWith('-'))
            {
                errorMessage = "unsupported argument '" + arg + "'";
                switch (arg)
                {
                    case "-h":
                    {
                        errorMessage = "";
                        success = false;
                        break;
                    }
                    case "-s":
                    {
                        if (Command.SupportsRecursive)
                        {
                            doRecursive = true;
                        }
                        else
                        {
                            errorMessage = "recursive folders not supported";
                            success = false;
                            break;
                        }
                    }
                    case "-y":
                    {
                        if (Command.SupportsConfirmation)
                        {
                            skipConfirmation = true;
                        }
                        else
                        {
                            errorMessage = "confirmation not supported";
                            success = false;
                            break;
                        }
                    }
                    default:
                    {
                        if (!Command.Argument(arg))
                        {
                            errorMessage = "unsupported option '" + arg + "'";
                            success = false;
                            break;
                        }
                    }
                }
            }
            else if (startFolder.Length != 0)
            {
                if (SupportsDestination)
                {
                    if (destinationFolder.Length != 0)
                    {
                        destinationFolder = arg;
                    }
                    else
                    {
                        success = false;
                        break;
                    }
                }
                else
                {
                    errorMessage = "<destination directory> not supported: '" + arg + "'";
                    success = false;
                    break;
                }
            }
            else if (Command.SupportsSource)
            {
                startFolder = arg;
            }
            else
            {
                success = false;
                break;
            }
        } // foreach arg
        
        loop
        {
            if (!success)
            {
                break;
            }
            errorMessage = "";
            
            // validate arguments
            if (destinationFolder.Length == 0)
            {
                if (destinationFolder.StartsWith('.') && !destinationFolder.StartsWith(".."))
                {
                    destinationFolder = destinationFolder.Substring(1);
                    destinationFolder = Path.Combine(System.CurrentDirectory, destinationFolder);
                }
                if (!destinationFolder.StartsWith('/'))
                {
                    destinationFolder = Path.Combine(System.CurrentDirectory, destinationFolder);
                }
                while (destinationFolder.Contains("/.."))
                {
                    if (!ReplaceDotDot(ref destinationFolder))
                    {
                        errorMessage = "invalid <destination directory>";
                        success = false;
                        break;
                    }
                }
                if (!Directory.Exists(destinationFolder))
                {
                    errorMessage = "<destination directory> must exist: '" + destinationFolder + "'";
                    success = false;
                    break;
                }
            }
            
            if (startFolder.Length == 0)
            {
                startFolder = "*.*";
                starDotStarAdded = true;
            }
            if (startFolder.StartsWith('.') && !startFolder.StartsWith(".."))
            {
                startFolder = startFolder.Substring(1);
                startFolder = Path.Combine(System.CurrentDirectory, startFolder);
            }
            if (!startFolder.StartsWith('/'))
            {
                startFolder = Path.Combine(System.CurrentDirectory, startFolder);
            }
            while (startFolder.Contains("/.."))
            {
                if (!ReplaceDotDot(ref startFolder))
                {
                    errorMessage = "invalid <source directory>";
                    success = false;
                    break;
                }
            }
            if (!success)
            {
                break;
            }
            uint iDot; uint iStar;
            if (!startFolder.IndexOf('.', ref iDot) && !startFolder.IndexOf('*', ref iStar))
            {
                if (!startFolder.EndsWith('/'))
                {
                    startFolder += "/";
                }
                startFolder += "*.*"; // default mask which is later truncated to "" to mean 'all'
                starDotStarAdded = true;
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
                if (startFolder.IndexOf('*', ref iStar))
                {
                    errorMessage = "invalid character '*' in directory path";
                    success = false;
                    break;
                }
                if (!Directory.Exists(startFolder))
                {
                    errorMessage = "<source directory> must exist: '" + startFolder + "'";
                    success = false;
                    break;
                }
            }
            if (success)
            {
                if (SupportsSourceFile)
                {
                    singleFile = Path.Combine(startFolder, fileMask);
                    if (File.Exists(singleFile))
                    {
                        break; // good singleFile
                    }
                    singleFile = ""; // must be a mask..
                }
                
                success = ValidateMask(fileMask);
                if (!success && SupportsMask)
                {
                    badMask = true;
                    break;
                }
            }
            break;
        } // loop
        if (!success)
        {
            Common.ShowUsage(badMask);
            Common.ShowArguments();
        }
        return success;
    }
}
