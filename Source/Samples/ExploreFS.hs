program ExploreFS
{
    #define SERIAL_CONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"
    
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
                WriteLn((indentString + fileName).Pad(' ', 20) + size.ToString() + " bytes");
            }
        }
    }
    
    {
        WalkDirectory("/", 0);
#ifndef SERIAL_CONSOLE
        Key k = ReadKey();
#endif

    }
}
