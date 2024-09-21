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
        
        //FileSystem.Format();
        //int result = MkDir("Folder");
        //result = MkDir("Folder2");
        
        
        /*
        
        
        result = ChDir("Folder");
        IO.WriteLn(GetCwd());
        result = ChDir("/Folder");
        IO.WriteLn(GetCwd());
        */
        
        System.CurrentDirectory = "/Folder";
        IO.WriteLn(System.CurrentDirectory);
        
        Directory dir = Directory.Open("/");
        uint count = Directory.GetDirectoryCount(dir);
        for (uint i = 0; i < count; i++)
        {
            string folder = Directory.GetDirectory(dir, i);
            IO.WriteLn(folder);
        }
    }
}
