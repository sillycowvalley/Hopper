program EEPROM
{
    uses "/Source/Library/File/File"
    uses "/Source/Library/File/Directory"
    
    uses "/Source/Library/Boards/PimoroniTiny2350"
    //uses "/Source/Library/Boards/Hopper6502"
    
    
    Hopper()
    {
        _ = Wire.Initialize();
        
        BlockStorage.Configure(0x54, SerialEEPROM.XX512);
        
        FileSystem.Format();
        
                  
    }
}
