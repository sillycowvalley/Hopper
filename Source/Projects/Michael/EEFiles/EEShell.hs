program EEShell
{
    uses "/Source/Library/File/File"
    uses "/Source/Library/File/Directory"
    
    uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/Hopper6502"
    
    // Commands:
    // - DIR   - list files and directorys, -S for recursive
    // - MKDIR - make directory
    // - RMDIR - remove empty directory
    // - DEL   - remove file
    // - WRITE - create a text file
    // - READ  - display the content of a text file
    // - EXIT  - exit the console so the debugger or monitor can break in
    
    Hopper()
    {
        _ = Wire.Initialize();
        
        BlockStorage.Configure(0x54, SerialEEPROM.XX512);
        
        FileSystem.Format();
        
        byte[256] buffer;
        BlockStorage.ReadBlock(0, buffer);
        BlockStorage.ReadBlock(1, buffer);
        
    }
}
