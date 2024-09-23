program EEShell
{
    uses "/Source/Library/File/File"
    uses "/Source/Library/File/Directory"
    
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    uses "/Source/Library/Boards/Hopper6502"
    
    Help()
    {
        WriteLn("  Command:   Alias:");
        WriteLn("    EXIT                Terminate this program.");
        WriteLn("    CD         CHDIR    Changes the current directory.");
        WriteLn("    MD         MKDIR    Create a new directory.");
        WriteLn("    RD         RMDIR    Remove empty directory.");
        WriteLn("    RM         DEL      Delete a file.");
        WriteLn("    DIR        LS       Lists a directory. -S for recursive.");
        WriteLn("    SHOW                Echos file content to console.");
        WriteLn("    BLOCK               Hex output of a drive block (0..255).");
        WriteLn("    FORMAT              Resets the drive.");
    }
    
    Format()
    {
        FileSystem.Mount(true);
        IO.WriteLn("    Drive formatted.");    
    }
    
    doDir(string path, bool recurse, string indent)
    {
        loop
        {
            Directory dir = Directory.Open(path);
            if (!Directory.IsValid(dir))
            {
                IO.WriteLn("    Invalid directory '" + path + "'");
                break;
            }
            uint count = Directory.GetDirectoryCount(dir);
            for (uint i = 0; i < count; i++)
            {
                string folder = Directory.GetDirectory(dir, i);
                IO.WriteLn(indent + folder);
                if (recurse)
                {
                    doDir(folder, true, indent + "  ");
                }
            }
            count = Directory.GetFileCount(dir);
            for (uint i = 0; i < count; i++)
            {
                string filePath = Directory.GetFile(dir, i);
                uint size = File.GetSize(filePath);
                string fileName = Path.GetFileName(filePath);
                filePath = Path.GetDirectoryName(filePath);
                fileName = fileName.Pad(' ', 12);
                fileName = fileName + size.ToString() + " bytes";
                filePath = Path.Combine(filePath, fileName);
                IO.WriteLn(indent + filePath);
            }
            break;
        } 
    }
    Dir(string argument)
    {
        bool recurse = (argument.ToUpper() == "-S");
        doDir(System.CurrentDirectory, recurse, "  ");
    }
    Block(string blockNumber)
    {
        loop
        {
            uint iBlock;
            if (!UInt.TryParse(blockNumber, ref iBlock) || (iBlock > 255))
            {
                IO.WriteLn("    Invalid block number '" + blockNumber + "'");
                break;
            }
            byte[256] buffer;
            BlockStorage.ReadBlock(byte(iBlock), buffer);
            uint address = iBlock * 256;
            string current;
            for (uint i = 0; i < 256; i++)
            {
                if (address % 16 == 0)
                {
                    IO.Write(address.ToHexString(4) + " ");
                }
                IO.Write((buffer[i]).ToHexString(2) + " ");
                if (buffer[i] >= 32)
                {
                    current += char(buffer[i]);
                }
                else
                {
                    current += '.';
                }
                if (address % 16 == 7)
                {
                    IO.Write(" ");
                }
                if (address % 16 == 15)
                {
                    IO.WriteLn("  " + current);
                    current = "";
                }
                address++;
            }
            IO.WriteLn();
            break;
        }
    }
    ChDir(string path)
    {
        if (Directory.Exists(path))
        {
            System.CurrentDirectory = path;
        }
        else
        {
            IO.WriteLn("    Directory does not exist");
        }
    }
    MkDir(string path)
    {
        if (Directory.Exists(path))
        {
            IO.WriteLn("    Directory already exists");
        }
        else
        {
            Directory.Create(path);
            Directory dir = Directory.Open(path);
            if (!Directory.IsValid(dir))
            {
                IO.WriteLn("    Invalid directory '" + path + "'");
            }
        }
    }
    RmDir(string path)
    {
        loop
        {
            if (!Directory.Exists(path))
            {
                IO.WriteLn("    Directory does not exist");
                break;
            }
            if (path == System.CurrentDirectory)
            {
                IO.WriteLn("    Cannot remove current directory");
                break;
            }
            Directory dir = Directory.Open(path);
            if ((Directory.GetDirectoryCount(dir) != 0) || (Directory.GetFileCount(dir) != 0))
            {
                IO.WriteLn("    Can only remove empty directory");
                break;
            }
            Directory.Delete(path);
            break;
        }
    }
    Del(string path)
    {
        loop
        {
            if (!File.Exists(path))
            {
                IO.WriteLn("    File does not exist");
                break;
            }
            File.Delete(path);
            break;
        }
    }
    Show(string path)
    {
        loop
        {
            if (!File.Exists(path))
            {
                IO.WriteLn("    File does not exist");
                break;
            }
            File fl = File.Open(path);
            loop
            {
                string content = File.ReadLine(fl);
                if (!File.IsValid(fl))
                {
                    break;
                }         
                IO.WriteLn(content);   
            }
            break;
        }
    }
    Create(string path)
    {
        loop
        {
            if (File.Exists(path))
            {
                IO.WriteLn("    File already exists");
                break;
            }
            File fl = File.Create(path);
            if (!File.IsValid(fl))
            {
                IO.WriteLn("    Failed to create '" + path + "'");
            }
            loop
            {
                string line;
                if (!IO.ReadLn(ref line))
                {
                    break;
                }
                if (line.Length == 0)
                {
                    break;
                }                
                File.Append(fl, line + Char.EOL);
            }
            File.Flush(fl);
            break;
        }
    }
    bool GoodArguments(string command, uint count)
    {
        bool ok;
        switch (command)
        {
            case "HELP":
            case "FORMAT":
            case "EXIT":   { ok = (count == 0); }
            
            case "DIR":    { ok = (count == 0) || (count == 1); }
            
            case "CD":
            case "CHDIR":  
            
            case "MD":
            case "MKDIR":  
            
            case "RD":
            case "RMDIR":  
            
            case "SHOW":
            case "CREATE":
            case "RM":
            case "DEL":
            
            case "BLOCK":  { ok = (count == 1); }
        }
        return ok;
    }
    
    Hopper()
    {
        _ = Wire.Initialize();
        
        BlockStorage.Configure(0x54, SerialEEPROM.XX512);
        
        FileSystem.Mount(); // formats if it is a fresh EEPROM
        
        loop
        {
            string line;
            IO.Write(System.CurrentDirectory + ">");
            if (IO.ReadLn(ref line))
            {
                <string> parts = line.Split(' ');
                string argument;
                if (parts.Count > 0)
                {
                    if (parts.Count > 1)
                    {
                        argument = parts[1];
                    }
                    string command = (parts[0]).ToUpper();
                    if (GoodArguments(command, parts.Count-1))
                    {
                        switch (command)
                        {
                            case "EXIT":   { break; }
                            
                            case "FORMAT": { Format(); }
                            
                            case "DIR":    { Dir(argument); }
                            
                            case "CD":
                            case "CHDIR":  { ChDir(argument); }
                            
                            case "MD":
                            case "MKDIR":  { MkDir(argument); }
                            
                            case "RD":
                            case "RMDIR":  { RmDir(argument); }
                            
                            case "SHOW":   { Show(argument);    }
                            case "CREATE": { Create(argument);  }
                            
                            case "RM":
                            case "DEL":    { Del(argument);     }
                            
                            
                            case "BLOCK":  { Block(argument); }
                            
                            case "HELP":   { Help(); }
                            
                            default:
                            {
                                IO.WriteLn("    Invalid command");
                            }
                        }
                    }
                    else
                    {
                        IO.WriteLn("    Invalid arguments for '" + command + "'");
                    }
                }
            }
            else
            {
                break; // NMI or <ctrl><C>
            }
        }
        IO.WriteLn();
        IO.WriteLn("Exited");
    }
}
