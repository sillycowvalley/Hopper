program ExploreFS
{
    #define SERIAL_CONSOLE
    
    uses "/Source/Library/Boards/PiPicoW"
    
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
    uses "/Source/Library/SD"
    
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
                WriteLn((indentString + fileName).Pad(' ', 32) + size.ToString() + " bytes");
            }
        }
    }
    
    {
        SD.SPIController = 0;
        SD.ClkPin = Board.SPI0SCK;
        SD.TxPin  = Board.SPI0Tx;
        SD.RxPin  = Board.SPI0Rx;
        SD.CSPin  = Board.SPI0SS;
        
        if (!SD.Mount())
        {
            WriteLn("Mount Failed");
        }
        WalkDirectory("/", 0);
        SD.Eject();
        
#ifndef SERIAL_CONSOLE
        Key k = ReadKey();
#endif

    }
}
