program EEShell
{
    uses "/Source/Library/File/File"
    uses "/Source/Library/File/Directory"
    
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    uses "/Source/Library/Boards/Hopper6502"
    
    // Commands:
    // - EXIT  - exit the console so the debugger or monitor can break in
    // - DIR   - list files and directories, -S for recursive
    // - CHDIR - change current directory
    //
    // - MKDIR - make directory
    // - RMDIR - remove empty directory
    // - DEL   - remove file
    // - SHOW  - display the content of a text file
    // - HELP  - show these commands
    
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
                string filename = Directory.GetFile(dir, i);
                IO.WriteLn(indent + filename);
            }
            break;
        } 
    }
    Dir(string argument)
    {
        bool recurse = (argument.ToUpper() == "-S");
        doDir(System.CurrentDirectory, recurse, "  ");
    }
    ChDir(string path)
    {
        loop
        {
            if (path.Length == 0)
            {
                break;
            }
            if (Directory.Exists(path))
            {
                System.CurrentDirectory = path;
            }
            else
            {
                IO.WriteLn("    Directory does not exist");
            }
            break;
        }
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
                    switch ((parts[0]).ToUpper())
                    {
                        case "EXIT":   { break; }
                        
                        case "DIR":    { Dir(argument); }
                        
                        case "CD":
                        case "CHDIR":  { ChDir(argument); }
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
