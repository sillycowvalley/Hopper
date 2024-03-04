program ExploreFS
{
    uses "/Source/Library/Boards/SparkfunProMicroRP2040"
    uses "/Source/Library/Devices/Adafruit240x135ColorTFT"
    
    uses "/Source/Library/Fonts/Arduino6x8"
    
    WalkDirectory(string folderPath, uint indent)
    {
        string indentString = ("").Pad(' ', indent);
        WriteLn(indentString + folderPath);
        directory dir = Directory.Open(folderPath);
        if (dir.IsValid())
        {
            indent += 2;
            indentString = ("").Pad(' ', indent);
            
            uint count = dir.GetDirectoryCount();
            for (uint i = 0; i < count; i++)
            {
                string subFolderPath = dir.GetDirectory(i);
                WalkDirectory(subFolderPath, indent);
            }
            count = dir.GetFileCount();
            for (uint i = 0; i < count; i++)
            {
                string filePath = dir.GetFile(i);
                long   size     = File.GetSize(filePath);
                string fileName = Path.GetFileName(filePath);
                WriteLn(indentString + fileName);
            }
        }
    }
    
    {
        DeviceDriver.SDCS = Board.GP29;
        DeviceDriver.CS   = Board.SPI0SS;
        DeviceDriver.DC   = Board.GP28;
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        
        EchoToDisplay = true;
        Screen.Clear();
        WalkDirectory("/", 0);
        EchoToDisplay = false;
        SD.Eject();
    }
}
