program EEPROM
{
    uses "/Source/Library/File/File"
    uses "/Source/Library/File/Directory"
    
    //uses "/Source/Library/Boards/PimoroniTiny2350"
    uses "/Source/Library/Boards/Hopper6502"
    
    Hopper()
    {
        _ = Wire.Initialize();
        BlockStorage.Configure(0x54, SerialEEPROM.XX512);
        FileSystem.Mount(); // formats if not already formatted
        
        /*
        Directory.Create("Folder");
        Directory.Create("Folder2");
        
        Directory dir = Directory.Open("/");
        uint count = Directory.GetDirectoryCount(dir);
        for (uint i = 0; i < count; i++)
        {
            string folder = Directory.GetDirectory(dir, i);
            IO.WriteLn(folder);
        }
        
        File writer = File.Create("/Folder/Test");
        if (File.IsValid(writer))
        {
            File.Append(writer, "Content One" + Char.EOL);
            File.Append(writer, "Content Two" + Char.EOL);
            File.Flush(writer);
        }
        */
        
        
        File current = File.Open("/Folder/Test");
        if (File.IsValid(current))
        {
            string content = File.ReadLine(current);
            IO.WriteLn(content);
            content = File.ReadLine(current);
            IO.WriteLn(content);
        }
        /*
        File.Delete("/Folder/Test");
        
        Directory.Delete("/");
        
        Directory.Delete("Folder");
        Directory.Delete("Folder2");
        */
        
    }
}
