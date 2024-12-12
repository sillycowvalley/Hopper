program ExploreFS
{
    uses "/Source/Library/Boards/PimoroniTiny2350"
    
    WalkDirectory(string folderPath, uint indent)
    {
        string indentString = ("").Pad(' ', indent);
        IO.WriteLn(indentString + folderPath);
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
                IO.WriteLn((indentString + fileName).Pad(' ', 32) + size.ToString() + " bytes");
            }
        }
    }
    
    Hopper()
    {
        MCU.PinMode(GP3, PinModeOption.InputPullup);
        
        SD.SPIController = 0;
        SD.ClkPin = Board.SPI0SCK;
        SD.TxPin  = Board.SPI0Tx;
        SD.RxPin  = Board.SPI0Rx;
        SD.CSPin  = Board.SPI0SS;
        
        bool cardMounted;
        bool cardDetected = DigitalRead(GP3);
        if (cardDetected)
        {
            cardMounted = SD.Mount();
            if (!cardMounted)
            {
                IO.WriteLn("Mount Failed");
            }
        }
        WalkDirectory("/", 0);
        if (cardMounted)
        {
            SD.Eject();
        }
    }
}
