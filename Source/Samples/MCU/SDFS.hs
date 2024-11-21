program ExploreFS
{
    uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    
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
                IO.WriteLn(indentString + fileName);
            }
        }
    }
    
    Hopper()
    {
        // Settings for Hopper SD unit:
        SD.SPIController = 0;
        SD.ClkPin = SPI0SCK;
        SD.TxPin  = SPI0Tx;
        SD.RxPin  = SPI0Rx;
        SD.CSPin  = SPI0SS; 
        
        PinMode(GP12, PinModeOption.Input);
        loop
        {
            bool cardDetected = DigitalRead(GP12);
            if (cardDetected)
            {
                if (!SD.Mount()) // let SD library initialize SPI before call to SPI.Begin() in DisplayDriver.begin()
                {
                    IO.WriteLn("Failed to initialize SD");
                }
                else
                {
                    long start = Millis;
                    
                    WalkDirectory("/", 0);
                    
                    SD.Eject();
                
                    long elapsed = Millis - start;
                    IO.WriteLn("Elapsed: " + elapsed.ToString() + "ms");
                }
            }
            else
            {
                IO.WriteLn("No card detected");
            }
            Delay(3000);
        }
    }
}
