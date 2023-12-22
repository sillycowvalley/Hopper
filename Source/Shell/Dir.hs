program Dir
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    string wildcardStartsWith;
    string wildcardEndsWith;
    
    uint FileCRC16(string filepath)
    {
        // stackoverflow.com/users/1180620/mark-adler .. more or less .. nice and simple.
        // Not a good implementation when it comes to performance but that doesn't seem to matter for 'dir'
        uint crc = 0xFFFF;
        file f = File.Open(filepath);
        loop
        {
            byte data = f.Read();
            if (!f.IsValid()) { break; }
            
            crc = crc ^ data;
            for (byte k = 0; k < 8; k++)
            {
                crc = (crc & 1 != 0) ? ((crc >> 1) ^ 0xA001) : (crc >> 1);
            }
        }
        return crc;
    }

    DirectoryListing(<string> options, <string> arguments, bool firstCall)
    {
        uint iFirstDot;
        bool invalidArguments;
        bool first;
        uint directories;
        uint files;
        bool recursive = false;
        bool fullpaths = false; 
        bool showtime = false;
        bool showcrc = false;
        directory dir;
        string currentFolder = CurrentDirectory;
        loop
        {
            foreach (var option in options)
            {
                if (option == "-s")
                {
                    recursive = true;
                }
                else if (option == "-f")
                {
                    fullpaths = true;
                }
                else if (option == "-t")
                {
                    showtime = true;
                }
                else if (option == "-c")
                {
                    showcrc = true;
                }
                else
                {
                    invalidArguments = true;
                    break;
                }
            }
            
            if (arguments.Length > 1)
            {   
                invalidArguments = true;
            }
            if (!invalidArguments && (arguments.Length == 1))
            {
                currentFolder = arguments[0];
            }
            
            if (currentFolder.IndexOf('.', ref iFirstDot))
            {   
                uint iLastDot;
                if (currentFolder.IndexOf('.', ref iLastDot))
                {
                }
                if (!invalidArguments && (iFirstDot != iLastDot))
                {
                    invalidArguments = true; // either one dot or no dots;
                }
            }
            if (!invalidArguments && firstCall)
            {
                wildcardStartsWith = "";
                wildcardEndsWith = "";
                string wildcards;
                uint iStar;
                if (currentFolder.IndexOf('*', ref iStar))
                {
                    uint iLastSlash;
                    bool lastSlashFound;
                    if (currentFolder.LastIndexOf('/', ref iLastSlash))
                    {
                        lastSlashFound = true;
                        if (iLastSlash > iStar)
                        {
                            invalidArguments = true; // only allow wildcard in filename
                        }
                    }
                    if (!invalidArguments)
                    {
                        bool invalidWildcards = false;
                        if (!lastSlashFound)
                        {
                            wildcards = currentFolder;
                            currentFolder = "";
                        }
                        else
                        {
                            wildcards = currentFolder.Substring(iStar);
                            currentFolder = currentFolder.Substring(0,iStar);
                        }
                        uint iWDot;
                        bool dotFound;
                        uint iLastStar;
                        bool starFound;
                        if (wildcards.IndexOf('.', ref iWDot))
                        {
                            dotFound = true;
                        }
                        if (wildcards.IndexOf('*', ref iStar))
                        {
                            starFound = true;
                            if (wildcards.LastIndexOf('*', ref iLastStar))
                            {
                            }
                        }
                        if (wildcards == "*.*")
                        {
                            wildcards = ""; // all
                        }
                        else if (starFound && (iStar != iLastStar))
                        {
                            invalidWildcards = true; // only one '*' if not '*.*'
                        }
                        else if (!dotFound)
                        {
                            if (wildcards.Length < 2)
                            {
                                invalidWildcards = true;
                            }
                            else if (wildcards.StartsWith('*'))
                            {
                                // *xxxx
                                wildcardEndsWith = wildcards.Substring(1);
                                wildcardEndsWith = wildcardEndsWith.ToLower();
                            }
                            else if (wildcards.EndsWith('*'))
                            {
                                // xxxx*
                                wildcardStartsWith = wildcards.Substring(0, iStar);
                                wildcardStartsWith = wildcardStartsWith.ToLower();
                            }
                            else
                            {
                                invalidWildcards = true; // no '.', then must start or end with '*'
                            }
                        }
                        else
                        {
                            <string> parts = wildcards.Split('.');
                            invalidWildcards = parts.Length != 2;
                            if (!invalidWildcards)
                            {
                                string fileprefix = parts[0];
                                string extension  = parts[1];
                                if (fileprefix == "*")
                                {
                                    // *.xxx
                                     wildcardEndsWith = "." + extension;
                                     wildcardEndsWith = wildcardEndsWith.ToLower();
                                }
                                else if (extension == "*")
                                {
                                    // xxx.*
                                    wildcardStartsWith = fileprefix + ".";
                                    wildcardStartsWith = wildcardStartsWith.ToLower();
                                }
                                else
                                {
                                    invalidWildcards = true;
                                }
                            }
                        }
                        if (invalidWildcards)
                        {
                            PrintLn("Invalid wildcard for DIR:");
                            PrintLn("  '*.*' : all");
                            PrintLn("  'xxx.*' : all with name 'xxx'");
                            PrintLn("  '*.xxx' : all with extension 'xxx'");
                            PrintLn("  'xxx*' : all with name.ext starting with 'xxx'");
                            PrintLn("  '*xxx' : all with name.ext ending with 'xxx'");
                            break;
                        }
                    }
                }
            }
            
            if (invalidArguments)
            {
                PrintLn("Invalid arguments for DIR:");
                PrintLn("  -f : full paths for each file");
                PrintLn("  -t : include unix time stamp");
                PrintLn("  -c : include crc-16 as checksum");
                PrintLn("  -s : this directory and all subdirectories");
                break;
            }
            dir = Directory.Open(currentFolder);
            if (!dir.IsValid())
            {
                if (!firstCall)
                {
                    break;
                }
                string subFolder = Path.Combine(CurrentDirectory, currentFolder);
                dir = Directory.Open(subFolder);
                if (!dir.IsValid())
                {
                    PrintLn("Invalid directory '" + currentFolder + "'.");
                    break;
                }
                currentFolder = subFolder;
            }
            //PrintLn("'" + currentFolder + "','" + wildcardStartsWith +  "','" + wildcardEndsWith + "'");
            
            if (firstCall)
            {
                PrintLn(currentFolder, Color.MatrixBlue, Color.Black);
            }
            
            first = true;
            directories =  dir.GetDirectoryCount();
            if (!recursive && (wildcardEndsWith.Length == 0) && (wildcardStartsWith.Length == 0))
            {
                for (uint i = 0; i < directories; i++)
                {
                    string directoryname = dir.GetDirectory(i);
                    PrintLn(directoryname, Color.MatrixBlue, Color.Black);
                }
            }
            files =  dir.GetFileCount();
            uint maxLength = 0;
            if (showtime || showcrc)
            {
                for (uint i = 0; i < files; i++)
                {
                    string filepath = dir.GetFile(i);
                    string filename = Path.GetFileName(filepath);
                    if (filename.Length > maxLength)
                    {
                        maxLength = filename.Length;
                    }
                }
            }
            for (uint i = 0; i < files; i++)
            {
                string filepath = dir.GetFile(i);
                string filename = Path.GetFileName(filepath);
                string filenameLower = filename.ToLower();
                
                long ft = File.GetTime(filepath);
                
                
                if (wildcardEndsWith.Length > 0)
                {
                    if (!filenameLower.EndsWith(wildcardEndsWith))
                    {
                        continue;
                    }
                }
                if (wildcardStartsWith.Length > 0)
                {
                    if (!filenameLower.StartsWith(wildcardStartsWith))
                    {
                        continue;
                    }
                }
                if (first && !fullpaths && !firstCall)
                {
                    // only print the folder if there is at least one file listed in it
                    PrintLn(currentFolder, Color.MatrixBlue, Color.Black);
                    first = false;
                }
                if (!fullpaths)
                {
                    Print(" " + filename, Color.MatrixBlue, Color.Black);
                }
                else
                {
                    Print(filepath, Color.MatrixBlue, Color.Black);
                }
                if (showtime || showcrc)
                {
                    uint pad = maxLength - filename.Length;
                    while (pad  != 0)
                    {
                        Print(' ');
                        pad--;
                    }
                }
                if (showtime)
                {
                    Print("  0x"  +  ft.ToHexString(8), Color.MatrixGreen, Color.Black);
                }
                if (showcrc)
                {
                    uint crc = FileCRC16(filepath);
                    Print("  0x"  +  crc.ToHexString(4), Color.MatrixRed, Color.Black);
                }
                PrintLn();
            } // file loop
            
            if (recursive)
            {
                for (uint i = 0; i < directories; i++)
                {
                    string directoryname = dir.GetDirectory(i);
                    <string> recursiveargument;
                    recursiveargument.Append(directoryname);
                    DirectoryListing(options, recursiveargument, false);
                }
            }
            break;
        }
    }
    {
        <string> rawargs = System.Arguments;
        <string> options;
        <string> args;
        foreach (var arg in rawargs)
        {
            if ((arg.Length == 2) && arg.StartsWith('-'))
            {
                options.Append(arg.ToLower());
            }
            else
            {
                args.Append(arg);
            }
        }
        DirectoryListing(options, args, true);
    }
}
