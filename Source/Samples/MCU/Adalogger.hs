program AdaLogger
{
    uses "/Source/Library/Devices/AdafruitAdaloggerRTCSDFeatherwing"
    
    uses "/Source/System/File"
    uses "/Source/System/Directory"
    
    
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
                //string attribute = size.ToString() + " bytes";
                string attribute = File.GetTime(filePath);
                //string attribute = File.GetDate(filePath);
                WriteLn((indentString + fileName).Pad(' ', 32) + attribute);
            }
        }
    }
    
    {
        if (!RTCDevice.Begin())
        {
            WriteLn("Failed to Initialize Logger");
        }
        if (SD.Mount())
        {
            WalkDirectory("/", 0);
        }
    }
}
