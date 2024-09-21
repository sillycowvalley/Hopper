program EEPROM
{
    uses "/Source/Library/File/File"
    uses "/Source/Library/File/Directory"
    
    uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/Hopper6502"
    
    uses "/Source/Library/File/BlockFileSystem"
    
    
    Hopper()
    {
        _ = Wire.Initialize();
        
        BlockStorage.Configure(0x54, SerialEEPROM.XX512);
        
        FileSystem.Format();
        Directory.Create("Folder");
        Directory.Create("Folder2");
        
        
        /*
        
        
        result = ChDir("Folder");
        IO.WriteLn(GetCwd());
        result = ChDir("/Folder");
        IO.WriteLn(GetCwd());
        */
        
        Directory dir = Directory.Open("/");
        uint count = Directory.GetDirectoryCount(dir);
        for (uint i = 0; i < count; i++)
        {
            string folder = Directory.GetDirectory(dir, i);
            IO.WriteLn(folder);
        }
        
        File current = File.Create("/Folder/Test");
        if (File.IsValid(current))
        {
            File.Append(current, "Content One" + Char.EOL);
            File.Append(current, "Content Two" + Char.EOL);
            File.Flush(current);
        }
        
        current = File.Open("/Folder/Test");
        if (File.IsValid(current))
        {
            string content = File.ReadLine(current);
            content = File.ReadLine(current);
        }
        File.Delete("/Folder/Test");
        
        Directory.Delete("/");
        
        Directory.Delete("Folder");
        Directory.Delete("Folder2");
        
    }
}
